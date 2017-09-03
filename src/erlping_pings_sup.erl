%%%-------------------------------------------------------------------
%% @doc erlping top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(erlping_pings_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    Configs = erlping_db:load_config(erlping_db),
    io:format("Configs=~p~n", [Configs]),
    {ok, { {one_for_one, 0, 1}, create_childs(Configs, [])} }.

create_childs([], Acc) ->
    lists:reverse(Acc);
create_childs([{Class, Ping, Config} | Rest], Acc) ->
    case Config of
        #{"disabled" := true} ->
            create_childs(Rest, Acc);
        _ ->
            Module = ping_module(Class),
            create_childs(Rest, [
                #{
                    id => erlping_reporter:get_ping_path(Ping, Config),
                    start => {Module, start_link, [Ping, Config]},
                    restart => permanent,
                    shutdown => 1000,
                    type => worker
                } | Acc
            ])
    end.

%%====================================================================
%% Internal functions
%%====================================================================

ping_module("Http") ->
    erlping_http.
