-module(erlping_reporter).

-export([send_report/5]).

-export([send_report_impl/6]).

send_report(PingRid, Ping, Config, Response, Result) ->
    case Config of
        #{"notifies" := Notifies} ->
            lists:foreach(fun(Notif) ->
                wpool:cast(erlping_reporters, {?MODULE, send_report_impl, [Notif, PingRid, Ping, Config, Response, Result]})
            end, Notifies)
    end.


send_report_impl({"EmailReporter", EmailConfig}, _PingRid, _Ping, _Config, _Response, Result) ->
    lager:info("send email EmailConfig=~p Result=~p", [EmailConfig, Result]),
    ok.
