%%%-------------------------------------------------------------------
%% @doc erlping top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(erlping_sup).

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
    {ok, { {one_for_one, 0, 1}, [
        #{
            id => db,
            start => {erlping_db, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker
        },
        #{
            id => wpool_sup,
            start => {wpool, start_pool, [erlping_reporters, [
                {workers, 10}
            ]]},
            restart => permanent,
            shutdown => 5000,
            type => worker
        },
        #{
            id => pings_sup,
            start => {erlping_pings_sup, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => supervisor
        }
    ]} }.

%%====================================================================
%% Internal functions
%%====================================================================
