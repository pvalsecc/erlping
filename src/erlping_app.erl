%%%-------------------------------------------------------------------
%% @doc erlping public API
%% @end
%%%-------------------------------------------------------------------

-module(erlping_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    ssl:start(),
    erlping_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
