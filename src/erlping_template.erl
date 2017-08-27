-module(erlping_template).

%% API
-export([do/2]).


-spec do(Txt::string(), Context::#{string() => string()}) -> any().
do(Txt, Context) ->
    apply_impl(Txt, Context, "").

apply_impl([], _Context, Acc) ->
    lists:flatten(lists:reverse(Acc));
apply_impl("\\$" ++ Rest, Context, Acc) ->
    apply_impl(Rest, Context, [36 | Acc]);
apply_impl("${" ++ VarNameBin, Context, Acc) ->
    {VarName, Rest} = get_var_name(VarNameBin, ""),
    #{VarName := Value} = Context,
    apply_impl(Rest, Context, [Value | Acc]);
apply_impl([C | Rest], Context, Acc) ->
    apply_impl(Rest, Context, [C | Acc]).


get_var_name([$} | Rest], Acc) ->
    {lists:reverse(Acc), Rest};
get_var_name([C | Rest], Acc) ->
    get_var_name(Rest, [C | Acc]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

apply_test() ->
    Result = do("Hello ${world} for \$${ever}", #{"world" => "here", "ever" => "never"}),
    "Hello here for $never" = Result.

-endif.
