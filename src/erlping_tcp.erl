%%%-------------------------------------------------------------------
%%% @author patrick
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jun 2017 20:35
%%%-------------------------------------------------------------------
-module(erlping_tcp).

%% API
-export([connect/4, send/2, controlling_process/2, setopts/2, getopts/2, peername/1, sockname/1, port/1]).

connect(Address, Port, Opts, Timeout) ->
    {FinalAddress, FinalOpts} = case lists:keytake(force_ip_address, 1, Opts) of
        {value, {force_ip_address, Address2}, Opts2} ->
            lager:debug("IP address forced to ~p", [Address2]),
            {Address2, Opts2};
        false ->
            lager:warning("No IP address specified"),
            {Address, Opts}
    end,
    gen_tcp:connect(FinalAddress, Port, FinalOpts, Timeout).

send(Socket, Packet) ->
    gen_tcp:send(Socket, Packet).

setopts(A, B) ->
    inet:setopts(A, B).

getopts(A, B) ->
    inet:getopts(A, B).

peername(A) ->
    inet:peername(A).

sockname(A) ->
    inet:sockname(A).

port(A) ->
    inet:port(A).

controlling_process(Socket, NewOwner) ->
    gen_tcp:controlling_process(Socket, NewOwner).
