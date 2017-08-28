-module(erlping_reporter).

-export([send_report/5, get_ping_path/2]).

-export([send_report_impl/6]).

send_report(PingRid, Ping, Config, Response, Result) ->
    case Config of
        #{"notifies" := Notifies} ->
            lists:foreach(fun(Notif) ->
                wpool:cast(erlping_reporters, {?MODULE, send_report_impl, [Notif, PingRid, Ping, Config, Response, Result]})
            end, Notifies)
    end.


send_report_impl({"EmailReporter", #{"email" := ToAddress}=EmailConfig}, _PingRid, Ping, Config, Response, Result) ->
    case Result of
        ok -> ok;
        _ ->
            lager:info("send email EmailConfig=~p Result=~p", [EmailConfig, Result]),
            {ok, SmtpConfig} = application:get_env(erlping, smtp),
            From = "patrick@thus.ch",
            PingPath = get_ping_path(Ping, Config),
            Header = io_lib:format(
                "Subject: [erlping] Failure on ~s\r\nFrom: ~s\r\nTo: ~s\r\n\r\n",
                [PingPath, From, ToAddress]),
            Body = io_lib:format(
                "Failed check ~s\r\nResponse:\r\n~s",
                [format_result(Result), format_response(Response)]),
            SendResult = gen_smtp_client:send_blocking(
                {From, [ToAddress], Header ++ Body},
                SmtpConfig),
            lager:info("email result=~p", [SendResult])
    end,
    ok.


get_ping_path(Ping, Config) ->
    [PingName] = maps:get("path", Ping),
    GroupPath = maps:get("path", Config, []),
    string:join(lists:reverse([PingName | GroupPath]), "/").


format_response({http, IpAddress, {{_Protocol, Status, StatusDesc}, Headers, Body}}) ->
    io_lib:format("  Server IP: ~s\r\n\r\n  HTTP status: ~p (~s)\r\n\r\n  Headers:\r\n~s\r\n  Body:\r\n~s",
        [IpAddress, Status, StatusDesc, format_headers("    ", Headers), Body]).


format_headers(_Prefix, []) ->
    "";
format_headers(Prefix, [{Name, Value} | Rest]) ->
    io_lib:format("~s~s: ~s\r\n", [Prefix, any_to_string(Name), any_to_string(Value)]) ++
    format_headers(Prefix, Rest).


any_to_string(N) when is_integer(N); is_float(N) ->
    io_lib:format("~p", [N]);
any_to_string(A) when is_atom(A) ->
    io_lib:format("~p", [A]);
any_to_string(L) when is_list(L); is_binary(L) ->
    io_lib:format("~s", [L]).


format_result({Validator, Config}) ->
    io_lib:format("~s\r\n~s", [Validator, format_headers("  ", maps:to_list(Config))]).
