-module (thread).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("records.hrl").

main() -> #template { file="./site/templates/bare.html" }.

title() -> "Thread".

body() ->
    #container_12 { 
      body=[ #grid_8 { alpha=true, prefix=2, suffix=2, omega=true, 
		       body=case re:split(wf:path_info(), "/", [{return, list}]) of
				[""] -> 
				    wf:redirect("/index");
				[Thread] -> 
				    inner_body(Thread);
				_ -> 
				    wf:redirect("/index")
			    end}
	   ]}.

inner_body(Thread) -> 
    wf:state(thread, util:id_string_to_now(Thread)),
    wf:comet_global(fun () -> post_loop() end, wf:state(thread)),
    Comments = rpc:call(?BOARD_NODE, board, get_thread, [wf:state(thread)]),
    {Board, Stat, DefName, _, _} = rpc:call(?BOARD_NODE, board, thread_meta, [wf:state(thread)]),
    wf:state(board, Board),
    [ 
      #crumbs{ board=atom_to_list(Board), thread=Thread },
      #thread_moderation{thread_id=wf:state(thread), status=Stat},
      #panel {id=messages, body=lists:map(fun (T) -> element_comment:from_prop(DefName, T) end, Comments)},
      #comment_form{}
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% Event functions %%%
start_upload_event(image) -> ok.

post(Comment) ->
    Board = wf:state(board), Thread = wf:state(thread),
    Res = rpc:call(?BOARD_NODE, board, reply, [Board, Thread, Comment]),
    Summary = rpc:call(?BOARD_NODE, board, summarize, [{Board, Thread}]),
    wf:send_global(Board, {thread_update, Thread, Summary}),
    wf:send_global(Thread, {message, Res}),
    wf:set(txt_comment, "").

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
	{thread_moved, NewBoard} ->
	    wf:replace(".breadcrumb_trail", #crumbs{ board=NewBoard, thread=util:now_to_id_string(wf:state(thread)) }),
	    wf:wire(util:highlight(breadcrumb_trail)),
	    wf:flash(["This thread has moved to ", #link{text=NewBoard, url=util:uri({board, NewBoard})}]);
        {message, Comment} ->
            wf:insert_bottom(messages, element_comment:from_prop(Comment)),
	    wf:wire(util:highlight(".comment:last")),
	    %% scroll to the bottom if the viewer is near it
	    %%%%% (make it easy for people to follow the latest developments without
	    %%%%%  fucking over the ones still getting up to speed)
	    wf:wire("if (1000 > ($('body').height() - $('body').scrollTop())) $('body').scrollTop($('body').height());");
	{replace_comment, ElemId, Elem} ->
	    wf:replace(util:now_to_css_id(ElemId), 
		       element_comment:from_prop(Elem))
    end,
    wf:flush(),
    post_loop().

event(_) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
