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
    local(MovId, click, {move_thread, Thread}),
    render_block([#link{id=DelId, text="Delete Thread"},
		  #link{id=MovId, text="Move Thread"}]);
render_element(#thread_moderation{thread_id=Thread, status=deleted}) ->
    RevId = util:temp_id(),
    local(RevId, click, {revive_thread, Thread}),
    render_block([#link{id=RevId, text="Phoneix Down, Thread Edition"}]).

render_block(Body) ->
    #span{ show_if=wf:role(admin),
	   body=[#span{ class=admin_links, body=Body},
		 #br{ class=clear }] }.

event({delete_thread, ThreadId}) ->
    util:state_change(delete, ThreadId);
event({revive_thread, ThreadId}) ->
    util:state_change(revive, ThreadId);
event({delete_comment, CommentId}) ->
    util:state_change(delete, CommentId);
event({delete_image, CommentId}) ->
    util:state_change(delete, {image, CommentId});
event({revive_comment, CommentId}) ->
    util:state_change(revive, CommentId);
event({revive_image, CommentId}) ->
    util:state_change(revive, {image, CommentId}).
