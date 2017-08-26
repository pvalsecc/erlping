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
    {ok, { {one_for_one, 0, 1}, [
        #{
            id => http_ping,
            start => {erlping_http, start_link, ["https://www.suissealpine.sac-cas.ch/api/c2c/health_check?max_level=100", 10*1000, [{status, 200}]]},
            restart => permanent,
            shutdown => 1000,
            type => worker
        }
    ]} }.

%%====================================================================
%% Internal functions
%%====================================================================
