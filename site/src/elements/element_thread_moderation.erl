-module (element_thread_moderation).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

local(Target, Type, Postback) ->
    wf:wire(Target, #event{ type=Type, postback=Postback, delegate=?MODULE}).

reflect() -> record_info(fields, thread_moderation).

render_element(#thread_moderation{thread_id=Thread, status=active}) ->
    [DelId, MovId] = util:temp_id(2),
    local(DelId, click, {delete_thread, Thread}),
    local(MovId, change, {move_thread, Thread, MovId}),
    render_block([#link{id=DelId, text="Delete Thread"},
		  #dropdown{id=MovId, 
			    options=lists:map(fun({O, _Desc}) -> #option { text=O, value=O } end,
					      [{"Move Thread", none} | rpc:call(?BOARD_NODE, board, list, [])])}]);
render_element(#thread_moderation{thread_id=Thread, status=deleted}) ->
    RevId = util:temp_id(),
    local(RevId, click, {revive_thread, Thread}),
    render_block([#link{id=RevId, text="Phoneix Down, Thread Edition"}]).

render_block(Body) ->
    #span{ show_if=util:board_permission_p(),
	   body=[#span{ class=admin_links, body=Body},
		 #br{ class=clear }] }.

event({delete_thread, ThreadId}) ->
    util:state_change(delete, ThreadId);
event({revive_thread, ThreadId}) ->
    util:state_change(revive, ThreadId);
event({move_thread, _Thread, "Move Thread"}) ->
    haha_NO;
event({move_thread, Thread, Field}) ->
    BoardStr = util:q(Field),
    Board = list_to_atom(BoardStr),
    New = rpc:call(?BOARD_NODE, board, move, [Thread, Board]),
    wf:send_global(wf:state(board), {replace_thread, Thread, {moved, BoardStr}}),
    wf:send_global(Thread, {thread_moved, BoardStr}),
    wf:send_global(Board, {thread, New}),
    wf:wire(util:highlight(".breadcrumb_trail")).
