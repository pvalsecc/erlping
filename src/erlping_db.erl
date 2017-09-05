-module(erlping_db).

-behaviour(gen_server).

%% API
-export([start_link/0, load_config/1, save_result/4]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    con :: pid(),
    result_lists = #{} :: #{},
    tx_id = 1 :: non_neg_integer()
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

load_config(Pid) ->
    gen_server:call(Pid, {load_config}).

-spec save_result(PingRid :: odi:rid(), Path :: list(), Response :: {}, Result :: ok|{}) -> ok.
save_result(PingRid, Path, Response, Result) ->
    gen_server:cast(?SERVER, {save_result, PingRid, Path, Response, Result}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    {ok, #{host := Host, db_name := DBName, user := User, password := Password}} =
        application:get_env(erlping, db),
    {_Clusters, Con} = odi:db_open(Host, DBName, User, Password, []),
    State = #state{con = Con},
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call({load_config}, _From, State) ->
    {reply, load_config_impl(State), State};
handle_call(Request, _From, State) ->
    lager:error("Unknown call: ~p", [Request]),
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast({save_result, PingRid, Path, Response, Result}, State) ->
    {noreply, save_result(PingRid, Path, Response, Result, State)};
handle_cast(Request, State) ->
    lager:error("Unknown cast: ~p", [Request]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(Info, State) ->
    lager:error("Unknown info: ~p", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


load_config_impl(#state{con=Con}) ->
    {ok, T} = odi_graph:begin_transaction(Con),
    Pings = odi_graph:query(T, "SELECT FROM Ping", -1,
        "[*]out_Results:-2 in_Tests:0 in_Contains:0 out_Notifies:0 [*]out:0"),
    Result = pings_configs(T, Pings, []),
    odi_graph:rollback(T),
    Result.

pings_configs(_T, [], List) ->
    List;
pings_configs(T, [{Rid, document, _Version, Class, Ping} | Others], List) ->
    Configs = ping_configs(T, Rid, Class, Ping),
    pings_configs(T, Others, Configs ++ List).

ping_configs(T, Rid, Class, #{"in_Tests" := GroupEdges} = Ping) ->
    lists:map(fun(Config) -> {Class, group_merge(T, #{"rid" => Rid}, Ping), Config} end,
        groups_configs(T, follow_edges(T, GroupEdges, "out"), [])).

groups_configs(_T, [], Configs) ->
    Configs;
groups_configs(T, [Rid | OtherGroups], Configs) ->
    groups_configs(T, OtherGroups, Configs ++ group_config(T, Rid)).


group_config(T, Rid) ->
    {_GroupRid, document, _GroupVersion, _GroupClass, Group} = odi_graph:record_load(T, Rid, default),
    case Group of
        #{"in_Contains" := []} ->
            [group_merge(T, #{}, Group)];
        #{"in_Contains" := Parents} ->
            ParentConfigs = groups_configs(T, follow_edges(T, Parents, "out"), []),
            lists:map(fun(ParentConfig) -> group_merge(T, ParentConfig, Group) end, ParentConfigs);
        _ ->
            [group_merge(T, #{}, Group)]
    end.

follow_edges(T, Edges, Direction) ->
    lists:map(fun(EdgeRid) ->
        {_EdgeRid, document, _EdgeVersion, _EdgeClass, Edge} = odi_graph:record_load(T, EdgeRid, default),
        #{Direction := GroupRid} = Edge,
        GroupRid
    end, Edges).

group_merge(T, Parent, Child) ->
    maps:fold(
        fun("out_Notifies", V, Acc) ->
            ParentNotifs = maps:get("notifies", Acc, []),
            Acc#{"notifies" => ParentNotifs ++ get_linkeds(T, follow_edges(T, V, "in"), [])};
        ("out_Validates", V, Acc) ->
            Acc#{"validates" => get_linkeds(T, follow_edges(T, V, "in"), [])};
        ("out_" ++ _, _V, Acc) ->
            Acc;
        ("in_" ++ _, _V, Acc) ->
            Acc;
        ("name", V, Acc) ->
            ParentPath = maps:get("path", Acc, []),
            Acc#{"path" => [V | ParentPath]};
        (K, V, Acc) ->
            Acc#{K => V}
        end, Parent, Child).

get_linkeds(_T, [], Acc) ->
    lists:reverse(Acc);
get_linkeds(T, [Rid | Others], Acc) ->
    {LinkedRid, document, _LinkedVersion, LinkedClass, Linked} = odi_graph:record_load(T, Rid, default),
    get_linkeds(T, Others, [{LinkedClass, group_merge(T, #{"rid" => LinkedRid}, Linked)} | Acc]).


save_result(PingRid, Path, Response, Result, #state{con=Con, tx_id=TxId, result_lists=ResultLists}=State) ->
    {ok, T} = odi_graph:begin_transaction(Con),
    ResultListRid = get_or_create_result_list_rid(T, PingRid, Path, State),
    DbEntry = create_result(Response, Result),
    lager:debug("Adding result record: ~p", [DbEntry]),
    ok = odi_graph:create_vertex(T, -4, DbEntry),
    ok = odi_graph:create_edge(T, -5, ResultListRid, -4, {"ResultedIn", #{}}),
    case odi_graph:commit(T, TxId) of
        {error, Messages} ->
            lager:error("Failed to commit result: ~p", [Messages]),
            State;
        #{-2 := ActualResultListRid} ->
            State#state{tx_id=TxId+1, result_lists = ResultLists#{{PingRid, Path} => ActualResultListRid}};
        _ ->
            State#state{tx_id=TxId+1}
    end.

get_or_create_result_list_rid(T, PingRid, Path, #state{result_lists=ResultLists}=State) ->
    Key = {PingRid, Path},
    case ResultLists of
        #{Key := Rid} ->
            Rid;
        _ ->
            get_or_create_result_list(T, PingRid, Path)
    end.

get_or_create_result_list(T, {PingClusterId, PingPos}=PingRid, Path) ->
    Query = io_lib:format("SELECT FROM ResultList WHERE IN('Results').@rid = #~p:~p AND path = [~s]",
        [PingClusterId, PingPos, format_path(Path)]),
    case odi_graph:query(T, Query, 1, "*:-2") of
        {error, Messages} ->
            lager:error("Error getting existing ResultList: ~p", [Messages]),
            error;
        [{ResultListRid, document, _Version, "ResultList", _ResultList}] ->
            lager:debug("Found existing ResultList for ~p/~p", [PingRid, Path]),
            ResultListRid;
        [] ->
            lager:debug("Have to create a ResultList for ~p/~p", [PingRid, Path]),
            ok = odi_graph:create_vertex(T, -2, {"ResultList", #{path=>Path}}),
            ok = odi_graph:create_edge(T, -3, PingRid, -2, {"Results", #{}}),
            -2
    end.


create_result({http, IpAddress, {{_Protocol, Status, _StatusDesc}, Headers, Body}}, Result) ->
    {"HttpResult", #{
        "body" => Body,
        "headers" => lists:map(fun format_header/1, Headers),
        "ip_address" => IpAddress,
        "status" => Status,
        "passed" => case Result of ok -> true; _ -> false end,
        "timestamp" => get_timestamp()
    }}.

get_timestamp() ->
    {Mega, Sec, Micro} = os:timestamp(),
    (Mega*1000000 + Sec)*1000 + round(Micro/1000).


format_path([]) ->
    "";
format_path([Cur]) ->
    io_lib:format("~p", [Cur]);
format_path([Cur | Rest]) ->
    io_lib:format("~p, ~s", [Cur, format_path(Rest)]).


format_header({Name, Value}) ->
    lists:flatten(io_lib:format("~s: ~s", [Name, Value])).


test_truc() ->
    {_Clusters, Con}=odi:db_open("localhost", "erlping", "admin", "admin", []),
    {ok,T}=odi_graph:begin_transaction(Con),
    {Mega, Sec, Micro} = os:timestamp(),
    Timestamp = (Mega*1000000 + Sec)*1000 + round(Micro/1000),
    DbEntry = {"HttpResult", #{"body" => <<"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">\r\n\r\n<html>\r\n<head>\r\n<meta name=\"Author\" content=\"Yvan Valsecchi\"/>\r\n  <meta name=\"GENERATOR\" content=\"Mozilla/4.04 [en] (X11; I; Linux 2.0.32 i686) [Netscape]\"/>\r\n\t<meta name=\"ROBOTS\" content=\"INDEX,FOLLOW\"/>\r\n<title>Page principale du site THUS.ch</title>\r\n<style>\r\n\tbody { background: #e8eeff; color: black}\r\n</style>\r\n</head>\r\n<body >\r\n<div style=\"text-align: center; font-family:  Arial, helvetica, Verdana, sans-serif; font-weight : bold; font-style : italic; font-size: 35pt ; letter-spacing :0.3em ;\">Bienvenue</div>\r\n<div style=\"text-align: center; font-family: Verdana, Arial, sans-serif; font-weight : bold; font-size: 14pt ; color :#3333ff;\">Sur WWW.THUS.CH</div>\r\n<p><div style=\"text-align: center; font-family:  helvetica, sans-serif; font-size: 10pt ;\">Anciennement ce site portait le nom de <b><i>www.dante.urbanet.ch</i></b>. Il s'agit d'un site privé sans attache commerciale. Il regroupe aujourd'hui plusieurs sites de caractère et d'intérêts différents que vous pouvez visiter en cliquant sur les liens ci-dessous.</div></p>\r\n<p><div style=\"text-align: center; font-family:  arial, sans-serif; font-weight : bold; font-style : italic; font-size: 10pt ;\">Bonne visite et revenez souvent nous trouver.</div></p>\r\n<p><center><table border=\"0\">\r\n\t<tr>\r\n\t\t<td><a href=\"/~patrick/\"><img alt=\"\"  src=\"/~patrick/petit.jpg\" width=\"100\" height=\"76\"></a></td>\r\n\t\t<td>Patrick and his Linux universe </td>\r\n\t\t<td rowspan=\"2\" valign=\"middle\">&nbsp;&nbsp;&nbsp;&nbsp;<a href=\"http://gallery.thus.ch/\"><img src=\"egypt3.jpg\" alt=\"Photo Gallery\" width=\"100\" height=\"152\"></a></td>\r\n\t\t<td rowspan=\"2\" valign=\"middle\">Photo Gallery</td>\r\n\t</tr>\r\n\t<tr>\r\n\t\t<td><a href=\"http://marketing.thus.ch/loader.php\"><img src=\"logoyv.jpg\" alt=\"Cours complet de  marketing\" width=\"100\" height=\"125\"></a></td>\r\n\t\t<td>Cours marketing</td>\r\n\t</tr>\r\n</table></center></p>\r\n</body>\r\n</html>\r\n">>,
        "headers" => ["Referer: xxx", "Server: yyyy"], "ip_address" => "10.1.2.3", "status" => 200, "passed" => true, "timestamp" => Timestamp}},
    ok = odi_graph:create_vertex(T, -2, {"ResultList", #{path=>["Thus"]}}),
    ok = odi_graph:create_edge(T, -3, {35, 0}, -2, {"Results", #{}}),
    ok = odi_graph:create_vertex(T, -4, DbEntry),
    ok = odi_graph:create_edge(T, -5, -2, -4, {"ResultedIn", #{}}),
    odi_graph:commit(T, 1).
