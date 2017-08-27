%%%-------------------------------------------------------------------
%%% @author patrick
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Aug 2017 16:57
%%%-------------------------------------------------------------------
-module(erlping_db).
-author("patrick").

-behaviour(gen_server).

%% API
-export([start_link/0, load_config/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    con::pid()
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
handle_call(_Request, _From, State) ->
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
handle_cast(_Request, State) ->
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
handle_info(_Info, State) ->
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
    pings_configs(T, Pings, []).

pings_configs(_T, [], List) ->
    List;
pings_configs(T, [{Rid, document, _Version, Class, Ping} | Others], List) ->
    Configs = ping_configs(T, Rid, Class, Ping),
    pings_configs(T, Others, Configs ++ List).

ping_configs(T, Rid, Class, #{"in_Tests" := GroupEdges} = Ping) ->
    lists:map(fun(Config) -> {Rid, Class, group_merge(T, #{}, Ping), Config} end,
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
        ("out_" ++ _, _V, Acc) ->
            Acc;
        ("in_" ++ _, _V, Acc) ->
            Acc;
        ("name", _V, Acc) ->
            Acc;
        (K, V, Acc) ->
            Acc#{K => V}
        end, Parent, Child).

get_linkeds(_T, [], Acc) ->
    lists:reverse(Acc);
get_linkeds(T, [Rid | Others], Acc) ->
    {LinkedRid, document, _LinkedVersion, LinkedClass, Linked} = odi_graph:record_load(T, Rid, default),
    get_linkeds(T, Others, [{LinkedRid, LinkedClass, group_merge(T, #{}, Linked)} | Acc]).
