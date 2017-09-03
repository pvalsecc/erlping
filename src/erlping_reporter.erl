-module(erlping_reporter).

-export([report/4, get_ping_path/2]).

-export([report_impl/5]).

report(Ping, Config, Response, Result) ->
    case Config of
        #{"notifies" := Notifies} ->
            lists:foreach(fun(Notif) ->
                wpool:cast(erlping_reporters, {?MODULE, report_impl, [Notif, Ping, Config, Response, Result]})
            end, Notifies);
        _ ->
            ok
    end.

report_impl({_NotifClass, #{"what" := NotifWhat}}=Notif, #{"rid":=PingRid}=Ping, #{"path":=Path}=Config, Response, Result) ->
    SimpleResult = simple_result(Result),
    {VeryFirst, SameStatusCount} = erlping_history:get_same_status_count(PingRid, Path, SimpleResult),
    case need_notify(VeryFirst, SameStatusCount, NotifWhat, SimpleResult) of
        true ->
            lager:info("Notif needed"),
            send_report(Notif, Ping, Config, Response, Result);
        false ->
            lager:debug("No notif needed"),
            ok
    end.

send_report({"EmailReporter", #{"email" := ToAddress}=EmailConfig}, Ping, Config, Response, Result) ->
    lager:info("send email EmailConfig=~p Result=~p", [EmailConfig, Result]),
    {ok, SmtpConfig} = application:get_env(erlping, smtp),
    From = "patrick@thus.ch",
    PingPath = get_ping_path(Ping, Config),
    case Result of
        ok ->
            Header = io_lib:format(
                    "Subject: [erlping] Success on ~s\r\nFrom: ~s\r\nTo: ~s\r\n\r\n",
                    [PingPath, From, ToAddress]),
            Body = io_lib:format(
                "Success\r\n\r\nResponse:\r\n~s",
                [format_response(Response)]);
        _ ->
            Header = io_lib:format(
                "Subject: [erlping] Failure on ~s\r\nFrom: ~s\r\nTo: ~s\r\n\r\n",
                [PingPath, From, ToAddress]),
            Body = io_lib:format(
                "Failed check ~s\r\nResponse:\r\n~s",
                [format_result(Result), format_response(Response)])
    end,
    case gen_smtp_client:send_blocking({From, [ToAddress], Header ++ Body}, SmtpConfig) of
        {error, Type, Message} ->
            lager:error("Failed sending email: ~p ~p", [Type, Message]);
        {error, Reason} ->
            lager:error("Failed sending email: ~p", [Reason]);
        SendResult ->
            lager:info("email success: ~p", [SendResult])
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
    io_lib:format("~s", [L]);
any_to_string(X) ->
    io_lib:format("~p", [X]).


format_result({Validator, Config}) ->
    io_lib:format("~s\r\n~s", [Validator, format_headers("  ", maps:to_list(Config))]).


need_notify(_VeryFirst, _SameStatusCount, [], _Result) ->
    false;
need_notify(_VeryFirst, 1, ["new_failure" | _Rest], failure) ->
    true;
need_notify(false, 1, ["new_success" | _Rest], ok) ->
    true;
need_notify(VeryFirst, SameStatusCount, [_Cur | Rest], Result) ->
    need_notify(VeryFirst, SameStatusCount, Rest, Result).

simple_result(ok) ->
    ok;
simple_result(_) ->
    failure.
