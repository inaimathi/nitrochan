-module (view).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("records.hrl").

main() -> #template { file="./site/templates/bare.html" }.

title() -> "View".

body() ->
    #container_12 { 
      body=[ #grid_8 { alpha=true, prefix=2, suffix=2, omega=true, 
		       body=case re:split(wf:path_info(), "/", [{return, list}]) of
				[""] -> wf:redirect("/index");
				[Board] -> 
				    inner_body({Board});
				[Board, Thread] -> 
				    inner_body({Board, Thread});
				[Board, Thread | _] -> 
				    inner_body({Board, Thread})
			    end}
	   ]}.

inner_body({Board}) -> 
    wf:state(board, list_to_atom(Board)),
    wf:comet_global(fun () -> post_loop() end, wf:state(board)),
    Threads =  rpc:call(?BOARD_NODE, board, summarize, [wf:state(board)]),
    [ 
      #crumbs{ board=Board },
      #panel {id=messages, body= lists:map(fun element_thread_summary:from_tup/1, Threads)},
      #comment_form{}
    ];
inner_body({Board, Thread}) -> 
    wf:state(board, list_to_atom(Board)),
    wf:state(thread, util:id_string_to_now(Thread)),
    wf:comet_global(fun () -> post_loop() end, wf:state(thread)),
    Comments = rpc:call(?BOARD_NODE, board, get_thread, [wf:state(board), wf:state(thread)]),
    [ 
      #crumbs{ board=Board, thread=Thread },
      #link{show_if=wf:role(admin), text="Delete Thread", postback={delete_thread, Thread}},
      #panel {id=messages, body=lists:map(fun element_comment:from_tup/1, Comments)},
      #comment_form{}
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% Event functions %%%
start_upload_event(image) -> ok.

collect_tripcode() ->
    case {wf:user(), wf:q(txt_tripcode)} of
	{undefined, ""} -> false;
	{undefined, Trip} -> IP = lists:map(fun erlang:integer_to_list/1, tuple_to_list(wf:peer_ip())),
			     string:join([Trip | IP], ".");
	{User, _} when is_list(User) -> registered
    end.

collect_comment(LocalFileName) ->
    Body = wf:q(txt_comment), 
    Username = wf:coalesce([wf:user(), wf:q(txt_user_name)]),
    Trip = wf:coalesce([wf:q(txt_tripcode), ""]),
    case {Body, LocalFileName, length(Body) > 3000, length(Username) > 100, length(Trip) > 250} of
	{"", undefined, _, _, _} -> {false, "You need either a comment or an image"};
	{_, _, true, _, _} -> {false, "Your comment can't be longer than 3000 characters. What the fuck are you writing, a novel?"};
	{_, _, _, true, _} -> {false, "Your username can't be longer than 100 characters. And even that's excessive."};
	{_, _, _, _, true} -> {false, "Your tripcode can't be longer than 250 characters. Really, you're secure by like 132. Anything after that is wasted effort."};
	_ -> wf:session(username, Username), wf:session(tripcode, wf:q(txt_tripcode)), wf:session(tripcode, wf:q(txt_tripcode)),
	     {Username, collect_tripcode(), re:split(Body, "\n", [{return, list}]), LocalFileName}
    end.

post(Comment) ->
    Board = wf:state(board),
    case wf:state(thread) of
	undefined -> 
	    Res = {Id, _, _, _, _} = rpc:call(?BOARD_NODE, board, new_thread, [Board, Comment]),
	    wf:send_global(Board, {thread, Res}),
	    wf:redirect(util:uri(Id));
	Thread -> 
	    Res = rpc:call(?BOARD_NODE, board, reply, [Board, Thread, Comment]),
	    Summary = rpc:call(?BOARD_NODE, board, summarize, [{Board, Thread}]),
	    wf:send_global(Board, {thread_update, Thread, Summary}),
	    wf:send_global(Thread, {message, Res})
    end,
    wf:set(txt_comment, "").

finish_upload_event(_Tag, undefined, _, _) ->
    %% Comment with no image (require a comment in this case)
    case collect_comment(undefined) of
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
	     case collect_comment(Filename) of
		 {false, Reason} -> wf:flash(Reason);
		 Comment -> post(Comment)
	     end
    end.

post_loop() ->
    receive 
        'INIT' -> ok; %% init is sent to the first client in the comet pool. We don't care in this case.
	{thread, Thread} ->
	    wf:insert_top(messages, element_thread_summary:from_tup(Thread)),
	    wf:wire(util:highlight(".thread:first"));
	{thread_update, ThreadId, ThreadSummary} ->
	    wf:remove(util:now_to_thread_id(ThreadId)),
	    wf:insert_top(messages, element_thread_summary:from_tup(ThreadSummary)),
	    wf:wire(util:highlight(".thread:first"));
        {message, Comment} ->
            wf:insert_bottom(messages, element_comment:from_tup(Comment)),
	    wf:wire(util:highlight(".comment:last"))
    end,
    wf:flush(),
    post_loop().

event({delete_thread, Thread}) ->
    rpc:call(?BOARD_NODE, board, delete, [wf:state(board), Thread]);
event({delete_comment, Comment}) ->
    rpc:call(?BOARD_NODE, board, delete, [wf:state(board), Comment]);
event({delete_image, Comment}) ->
    rpc:call(?BOARD_NODE, board, delete, [wf:state(board), {image, Comment}]);
event({revive_thread, Thread}) ->
    rpc:call(?BOARD_NODE, board, revive, [wf:state(board), Thread]);
event({revive_comment, Comment}) ->
    rpc:call(?BOARD_NODE, board, revive, [wf:state(board), Comment]);
event({revive_image, Comment}) ->
    rpc:call(?BOARD_NODE, board, revive, [wf:state(board), {image, Comment}]);
event(logout) -> util:logout();
event(login) -> wf:redirect_to_login("/auth/login");
event(register) -> wf:redirect_to_login("/auth/register");
event(_) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
