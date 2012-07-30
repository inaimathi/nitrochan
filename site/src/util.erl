-module(util).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

highlight() -> 
    #effect {effect=highlight, speed=1000, options=[{color, "#00ff00"}]}.
highlight(Target) ->
    #effect {target=Target, effect=highlight, speed=1000, options=[{color, "#00ff00"}]}.

trip_to_string(false) -> "";
trip_to_string(Tripcode) -> bin_to_hex(Tripcode).

now_to_datetime_string(Now) ->
    {{Y, M, D}, {H, Min, _S}} = calendar:now_to_datetime(Now),
    [Ys, Ds, Hs, Ms] = lists:map(fun integer_to_list/1, [Y, D, H, Min]),
    lists:append([Ys, ", ", month_name(M), ", ", Ds, " -- ", Hs, ":", Ms]).

now_to_thread_id(Now) -> now_to_string(Now, "thread", "").
now_to_id_string(Now) -> now_to_string(Now, "", ".").
now_to_string(Now, Prefix, Join) ->
    Res = lists:map(fun erlang:integer_to_list/1, tuple_to_list(Now)),
    Prefix ++ string:join(Res, Join).

bin_to_hex(Bin) ->
    lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- binary_to_list(Bin)]).

month_name(Num) ->
    lists:nth(Num, ["Jan", "Feb", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]).

%% make_tempname() ->
%%     make_tempname(filename:nativename("/tmp")).
%% make_tempname(TargetDir) ->
%%     {A, B, C} = now(),
%%     [D, E, F] = lists:map(fun integer_to_list/1, [A, B, C]),
%%     Tempname = lists:append(["tmp.", D, ".", E, ".", F]),
%%     filename:absname_join(TargetDir, Tempname).

id_string_to_now(IdString) ->
    Split = re:split(IdString, "\\.", [{return, list}]),
    [A, B, C] = lists:map(fun (S) -> {I, []} = string:to_integer(S), I end, Split),
    {A, B, C}.

uri(ThreadId) ->
    lists:append(["/view/", atom_to_list(wf:state(board)), "/", util:now_to_id_string(ThreadId), "/"]).
