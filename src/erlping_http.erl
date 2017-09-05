-module(erlping_http).

-behaviour(gen_fsm).

%% API
-export([start_link/2]).

%% gen_fsm callbacks
-export([init/1,
    ping/2,
    handle_event/3,
    handle_sync_event/4,
    handle_info/3,
    terminate/3,
    code_change/4]).

-define(SERVER, ?MODULE).

-type validation() :: {status, number()}.
-type validations() :: [validation()].

-record(state, {
    ping :: #{},
    config :: #{},
    url :: list(),
    period :: number(),
    validations :: validations(),
    start_time :: erlang:timestamp(),
    workers = [] :: list()
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Ping::#{}, Config::#{}) -> {ok, pid()} | ignore | {error, Reason :: term()}).
start_link(Ping, Config) ->
    gen_fsm:start_link(?MODULE, [Ping, Config], []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, StateName :: atom(), StateData :: #state{}} |
    {ok, StateName :: atom(), StateData :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([Ping, Config]) ->
    #{"url" := Url, "period" := Period, "validates" := Validations} = Ping,
    RealUrl = erlping_template:do(Url, Config),
    lager:md([{desc, RealUrl}]),
    RandomStart = rand:uniform(Period * 1000),  % to not have everything start at the same time
    {ok, ping, #state{ping=Ping, config=Config,
        url= RealUrl,
        period=Period * 1000, validations=Validations}, RandomStart}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @end
%%--------------------------------------------------------------------
-spec(ping(Event :: term(), State :: #state{}) ->
    {next_state, NextStateName :: atom(), NextState :: #state{}} |
    {next_state, NextStateName :: atom(), NextState :: #state{},
        timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
ping(_Event, #state{url=Url}=State) ->
    lager:info("Sending request"),
    {ok, {Scheme, _UserInfo, Host, _Port, _Path, _Query}} = http_uri:parse(Url),
%%    IpAddresses = lists:append(inet_res:lookup(Host, in, aaaa), inet_res:lookup(Host, in, a)),
    IpAddresses = inet_res:lookup(Host, in, a),
    Workers = start_workers(Scheme, IpAddresses, State, []),
    {next_state, wait_responses, State#state{start_time=erlang:timestamp(), workers=Workers}}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @end
%%--------------------------------------------------------------------
%%-spec(state_name(Event :: term(), From :: {pid(), term()},
%%    State :: #state{}) ->
%%    {next_state, NextStateName :: atom(), NextState :: #state{}} |
%%    {next_state, NextStateName :: atom(), NextState :: #state{},
%%        timeout() | hibernate} |
%%    {reply, Reply, NextStateName :: atom(), NextState :: #state{}} |
%%    {reply, Reply, NextStateName :: atom(), NextState :: #state{},
%%        timeout() | hibernate} |
%%    {stop, Reason :: normal | term(), NewState :: #state{}} |
%%    {stop, Reason :: normal | term(), Reply :: term(),
%%        NewState :: #state{}}).
%%state_name(_Event, _From, State) ->
%%    Reply = ok,
%%    {reply, Reply, state_name, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_event(Event :: term(), StateName :: atom(),
    StateData :: #state{}) ->
    {next_state, NextStateName :: atom(), NewStateData :: #state{}} |
    {next_state, NextStateName :: atom(), NewStateData :: #state{},
        timeout() | hibernate} |
    {stop, Reason :: term(), NewStateData :: #state{}}).
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_sync_event(Event :: term(), From :: {pid(), Tag :: term()},
    StateName :: atom(), StateData :: term()) ->
    {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term()} |
    {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term(),
        timeout() | hibernate} |
    {next_state, NextStateName :: atom(), NewStateData :: term()} |
    {next_state, NextStateName :: atom(), NewStateData :: term(),
        timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewStateData :: term()} |
    {stop, Reason :: term(), NewStateData :: term()}).
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: term(), StateName :: atom(),
    StateData :: term()) ->
    {next_state, NextStateName :: atom(), NewStateData :: term()} |
    {next_state, NextStateName :: atom(), NewStateData :: term(),
        timeout() | hibernate} |
    {stop, Reason :: normal | term(), NewStateData :: term()}).
handle_info({http, {Worker, Response}}, wait_responses, #state{workers = Workers, period = Period}=State) ->
    {value, {IpAddress, Worker}, RemainingWorkers} = lists:keytake(Worker, 2, Workers),
    handle_response(Response, inet_parse:ntoa(IpAddress), State),
    case RemainingWorkers of
        [] ->
            lager:info("Done"),
            {next_state, ping, State#state{workers = RemainingWorkers}, Period};
        _ ->
            {next_state, wait_responses, State#state{workers = RemainingWorkers}}
    end;
handle_info(Info, StateName, State) ->
    lager:warning("Received a unknown event ~p in state ~p", [Info, StateName]),
    {next_state, StateName, State}.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: normal | shutdown | {shutdown, term()}
| term(), StateName :: atom(), StateData :: term()) -> term()).
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, StateName :: atom(),
    StateData :: #state{}, Extra :: term()) ->
    {ok, NextStateName :: atom(), NewStateData :: #state{}}).
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_workers(https, [IpAddress | Rest], #state{url = Url} = State, Accum) ->
    {ok, Worker} = erlping_https:start_link(Url, IpAddress),
    start_workers(https, Rest, State, [{IpAddress, Worker} | Accum]);
start_workers(http, [IpAddress | Rest], #state{url = Url} = State, Accum) ->
    {ok, {Scheme, UserInfo, Host, Port, Path, Query}} = http_uri:parse(Url),
    FixedUrl = gen_uri(Scheme, UserInfo, IpAddress, Port, Path, Query),
    {ok, _} = http_uri:parse(FixedUrl),
    lager:debug("Sending request to ~p", [FixedUrl]),
    {ok, Worker} = httpc:request(get, {FixedUrl, [{"connection", "close"}, {"host", Host}]}, [], [{sync, false}]),
    start_workers(http, Rest, State, [{IpAddress, Worker} | Accum]);
start_workers(_Scheme, [], _State, Accum) ->
    Accum.

handle_response(Response={{_Proto, _Status, _StatusText}, _Headers, _Body}, IpAddress,
    #state{validations=Validations}=State) ->
    notify({http, IpAddress, Response}, apply_validations(Response, IpAddress, Validations), State);
handle_response(Response, _IpAddress, _State) ->
    lager:warning("Unexpected response: ~p", [Response]),
    false.


apply_validation({{"HTTP/1.1", Status, _Text}, _Headers, _Body}, IpAddress,
    {"HttpStatusValidation", #{"expected_status" := Status}}) ->
    lager:debug("Good status for ~s", [IpAddress]),
    true;
apply_validation({{"HTTP/1.1", _Status, _Text}, _Headers, Body}, IpAddress,
    {"RegexpValidation", #{"regexp" := Regexp}} = Validation) ->
    case re:run(Body, Regexp) of
        {match, _} ->
            lager:debug("Good content for ~s", [IpAddress]),
            true;
        nomatch ->
            lager:warning("Bad content for ~s", [IpAddress]),
            Validation
    end;
apply_validation(Response, IpAddress, Validation) ->
    lager:warning("Validation ~p failed for ~s: ~p", [Validation, IpAddress, Response]),
    Validation.

apply_validations(Response, IpAddress, [Validation | Rest]) ->
    case apply_validation(Response, IpAddress, Validation) of
        true -> apply_validations(Response, IpAddress, Rest);
        Failure -> Failure
    end;
apply_validations(_Response, _IpAddress, []) ->
    ok.


notify(Response, Result, #state{ping=Ping, config=Config}) ->
    erlping_reporter:report(Ping, Config, Response, Result).

gen_uri(Scheme, _UserInfo, IpAddress, Port, Path, Query) ->
    lists:flatten(io_lib:format("~w://~s:~p/~s~s", [Scheme, inet_parse:ntoa(IpAddress), Port, maybe_path(Path), Query])).

maybe_path("/") ->
    "";
maybe_path(Path) ->
    Path.
