-module(erlping_statsd).

%% API
-export([send_gauge/4, send_timer/4]).


send_gauge(Address, Port, Path, Value) ->
    send_udp(Address, Port, <<(format_key(Path))/binary, ":", (integer_to_binary(Value))/binary, "|g">>).

send_timer(Address, Port, Path, Value) ->
    send_udp(Address, Port, <<(format_key(Path))/binary, ":", (integer_to_binary(Value))/binary, "|ms">>).

send_udp(Address, Port, Message) ->
    {ok, S} = gen_udp:open(0, [inet, binary]),
    ok = gen_udp:send(S, Address, Port, Message),
    gen_udp:close(S).


format_key(Path) ->
    list_to_binary(
        lists:flatten(
            string:join(
                lists:map(
                    fun(Cur) -> string:replace(Cur, ":", "_", all) end,
                    lists:filter(fun(Cur) -> (Cur /= "/") and (Cur /= "*") end, Path)),
                "."))).
