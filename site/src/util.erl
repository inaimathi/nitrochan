-module(util).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

%%%%%%%%% Generally Useful Utility
validators(Button, [{Elem, Validators} | Rest]) ->
    wf:wire(Button, Elem, #validate{ validators=Validators }),
    validators(Button, Rest);
validators(_Button, []) -> ok.

q(Elems) when is_list(Elems) -> q(Elems, []);
q(Elem) when is_atom(Elem) -> wf:q(Elem).
q([Elem | Rest], Acc) -> q(Rest, [wf:q(Elem) | Acc]);
q([], Acc) -> lists:reverse(Acc).

now_to_datetime_string(Now) ->
    {{Y, M, D}, {H, Min, _S}} = calendar:now_to_datetime(Now),
    [Ys, Ds, Hs, Ms] = lists:map(fun integer_to_list/1, [Y, D, H, Min]),
    lists:append([Ys, ", ", month_name(M), ", ", Ds, " -- ", Hs, ":", Ms]).
month_name(Num) ->
    lists:nth(Num, ["Jan", "Feb", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]).

logout() ->
    wf:clear_roles(),
    wf:clear_user(),
    wf:clear_state(),
    wf:wire("location.reload()").

temp_id() -> wf:temp_id().
temp_id(Num) -> lists:map(fun (I) -> wf:temp_id() end, lists:seq(1, Num)).

bin_to_hex(Bin) ->
    lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- binary_to_list(Bin)]).

%% make_tempname() ->
%%     make_tempname(filename:nativename("/tmp")).
%% make_tempname(TargetDir) ->
%%     {A, B, C} = now(),
%%     [D, E, F] = lists:map(fun integer_to_list/1, [A, B, C]),
%%     Tempname = lists:append(["tmp.", D, ".", E, ".", F]),
%%     filename:absname_join(TargetDir, Tempname).

%%%%%%%%%% Nitrochan-specific utility
highlight() -> 
    #effect {effect=highlight, speed=1000, options=[{color, "#00ff00"}]}.
highlight(Target) ->
    #effect {target=Target, effect=highlight, speed=1000, options=[{color, "#00ff00"}]}.

state_change(Fn, Target) ->
    Id = case Target of
	     {image, CommentId} -> CommentId;
	     _ -> Target
	 end,
    case rpc:call(?BOARD_NODE, board, Fn, [wf:state(board), Target]) of
	{Thread, New} -> wf:send_global(wf:state(board), {replace_comment, Id, New}),
			 wf:send_global(Thread, {replace_comment, Id, New});
	New -> wf:send_global(wf:state(board), {replace_thread, Id, New})
    end.

trip_to_string(Tripcode) -> 
    try
	bin_to_hex(Tripcode)
    catch
	error:_ -> ""
    end.

now_to_thread_id(Now) -> now_to_string(Now, "thread", "").
now_to_id_string(Now) -> now_to_string(Now, "", ".").
now_to_css_id(Now) -> now_to_string(Now, "", "").
now_to_string(Now, Prefix, Join) ->
    Res = lists:map(fun erlang:integer_to_list/1, tuple_to_list(Now)),
    Prefix ++ string:join(Res, Join).

id_string_to_now(IdString) ->
    Split = re:split(IdString, "\\.", [{return, list}]),
    [A, B, C] = lists:map(fun (S) -> {I, []} = string:to_integer(S), I end, Split),
    {A, B, C}.

uri(ThreadId) ->
    lists:append(["/view/", atom_to_list(wf:state(board)), "/", util:now_to_id_string(ThreadId), "/"]).
