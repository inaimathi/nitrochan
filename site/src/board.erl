-module (board).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("records.hrl").

main() -> #template { file="./site/templates/bare.html" }.

title() -> "Board".

body() ->
    #container_12 { 
      body=[ #grid_8 { alpha=true, prefix=2, suffix=2, omega=true, 
		       body=case re:split(wf:path_info(), "/", [{return, list}]) of
				[""] -> wf:redirect("/index");
				[Board | _] ->  inner_body(Board);
				_ -> wf:redirect("/index")
			    end}
	   ]}.

inner_body(Board) -> 
    wf:state(board, list_to_atom(Board)),
    wf:comet_global(fun () -> post_loop() end, wf:state(board)),
    Threads =  rpc:call(?BOARD_NODE, board, summarize, [wf:state(board)]),
    [ 
      #crumbs{ board=Board },
      #panel {id=messages, body=lists:map(fun element_thread_summary:from_prop/1, Threads)},
      #comment_form{}
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% Event functions %%%
start_upload_event(image) -> ok.

post(Comment) ->
    Board = wf:state(board),
    Res = rpc:call(?BOARD_NODE, board, new_thread, [Board, Comment]),
    Id = proplists:get_value(id, Res),
    wf:send_global(Board, {thread, Res}),
    wf:redirect(util:uri({thread, Id})).

finish_upload_event(_Tag, undefined, _, _) ->
    %% Comment with no image (require a comment in this case)
    case element_comment_form:collect_comment(undefined) of
	{false, Reason} -> wf:flash(Reason);
	Comment -> post(Comment)
    end;
finish_upload_event(_Tag, _OriginalFilename, LocalFile, _Node) ->
    %% Comment with image (no other fields required, but the image has to be smaller than 3MB)
    case filelib:file_size(LocalFile) < 2097152 of
	false -> wf:flash("Your file can't be larger than 2MB"), nope;
	_ -> Filename = filename:basename(LocalFile),
	     Big = filename:join(["site", "static", "images", "big", Filename]),
	     Preview = filename:join(["site", "static", "images", "preview", Filename]),
	     file:rename(LocalFile, Big),
	     os:cmd(lists:append(["convert ", Big, "[0] -resize 250x250\\> ", Preview])),
	     case element_comment_form:collect_comment(Filename) of
		 {false, Reason} -> wf:flash(Reason);
		 Comment -> post(Comment)
	     end
    end.

post_loop() ->
    receive 
        'INIT' -> ok; %% init is sent to the first client in the comet pool. We don't care in this case.
	{thread, Thread} ->
	    wf:remove(util:now_to_thread_id(proplists:get_value(id, Thread))),
	    wf:insert_top(messages, element_thread_summary:from_prop(Thread)),
	    wf:wire(util:highlight(".thread:first"));
	{thread_update, ThreadId, ThreadSummary} ->
	    wf:remove(util:now_to_thread_id(ThreadId)),
	    wf:insert_top(messages, element_thread_summary:from_prop(ThreadSummary)),
	    wf:wire(util:highlight(".thread:first"));
	{thread_moved, NewBoard} ->
	    wf:replace(breadcrumb_trail, #crumbs{ board=NewBoard, thread=wf:state(thread) }),
	    wf:wire(util:highlight(breadcrumb_trail)),
	    wf:flash(["This thread has moved to ", #link{text=NewBoard, url=util:uri({board, NewBoard})}]);
	{replace_thread, ThreadId, {moved, BoardStr}} ->
	    CssId = util:now_to_thread_id(ThreadId),
	    wf:replace(".wfid_" ++ CssId, 
		       #panel { class=[thread, moved], id=CssId,
			        body = [ #span{ class=notice, 
						body=[#link{text=util:now_to_id_string(ThreadId), 
							    url=util:uri({thread, ThreadId})},
						      " moved to ", 
						      #link{text=BoardStr, url=util:uri({board, BoardStr})}]}]});
	{replace_thread, ElemId, Elem} ->
	    wf:replace(util:now_to_thread_id(ElemId), 
		       element_thread_summary:from_prop(Elem));
	{replace_comment, ElemId, Elem} ->
	    wf:replace(util:now_to_css_id(ElemId), 
		       element_comment:from_prop(Elem))
    end,
    wf:flush(),
    post_loop().

event(_) -> ok.
