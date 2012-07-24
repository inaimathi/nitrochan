%% -*- mode: nitrogen -*-
-module (view).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

-define(NODE, 'erl_chan@127.0.1.1').

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
    Threads =  rpc:call(?NODE, board, summarize, [wf:state(board)]),
    [ 
      #h1 { text=Board },
      #panel {id=messages, body= lists:map(fun summary/1, Threads)},
      post_form()
    ];
inner_body({Board, Thread}) -> 
    wf:state(board, list_to_atom(Board)),
    wf:state(thread, id_string_to_now(Thread)),
    wf:comet_global(fun () -> post_loop() end, wf:state(thread)),
    Comments = rpc:call(?NODE, board, get_thread, [wf:state(board), wf:state(thread)]),
    [ 
      #h1 { text=Thread },
      #panel {id=messages, body=lists:map(fun comment/1, Comments)},
      post_form()
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% Echo functions %%%%
post_form() ->
    [#flash{},
     #label { text="Username" }, 
     #textbox { id=txt_user_name, next=tripcode, 
		text=wf:coalesce([wf:session(username), ""]) },
     #label { text="Tripcode" }, 
     #textbox { id=txt_tripcode, next=comment,
		text=wf:coalesce([wf:session(tripcode), ""])},
     #label { text="Comment" }, #textarea { id=txt_comment },
     #label { text="Image"}, #upload { id=txt_image, tag=image, button_text="Submit" }].

highlight() -> 
    #effect {effect=highlight, speed=1000, options=[{color, "#00ff00"}]}.
highlight(Target) ->
    #effect {target=Target, effect=highlight, speed=1000, options=[{color, "#00ff00"}]}.
    
summary({ThreadId, LastUpdate, CommentCount, [FirstComment | LastComments]}) ->
    IdString = now_to_thread_id(ThreadId),
    #panel {class=thread, id=IdString,
	    body = [ #link{text="Reply", url=uri(ThreadId)}, " ::: ", 
		     #span{ class=thread_datetime, text=now_to_datetime_string(LastUpdate) } |
		     case length(LastComments) of
			 N when N < 4 ->
			     lists:map(fun comment/1, [FirstComment | LastComments]);
			 _ -> 
			     [comment(FirstComment), #span{text=wf:f("... snip [~p] comments...", [CommentCount - 4])} | lists:map(fun comment/1, LastComments)]
		     end]}.

comment({Id, User, Tripcode, Body, File}) ->
    Trip = trip_to_string(Tripcode),
    Class = ".comment ." ++ Trip,
    #span {class=comment,
	   body=[#span{ class=username, text=User }, 
		 #span{ class=[tripcode, Trip], text=Trip, 
			actions=#event{ target=Class, 
					type=mouseover, 
					actions=highlight()} },
		 #span{ class=comment_id, text=now_to_id_string(Id) },
		 #span{ class=comment_datetime, text=now_to_datetime_string(Id) },
		 #br{ class=clear },
		 case File of
		     undefined -> "";
		     _ -> #link{ body=#image{ image="/images/preview/" ++ File }, url="/images/big/" ++ File }
		 end,
		 case Body of
		     [[]] -> "";
		     _ -> lists:map(fun ([]) -> #br{};
					(P) -> #p{ text=P } 
				    end, Body)
		 end,
		 #br{ class=clear }]}.

uri(ThreadId) ->
    lists:append(["/view/", atom_to_list(wf:state(board)), "/", now_to_id_string(ThreadId), "/"]).

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

id_string_to_now(IdString) ->
    Split = re:split(IdString, "\\.", [{return, list}]),
    [A, B, C] = lists:map(fun (S) -> {I, []} = string:to_integer(S), I end, Split),
    {A, B, C}.

bin_to_hex(Bin) ->
    lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- binary_to_list(Bin)]).

month_name(Num) ->
    lists:nth(Num, ["Jan", "Feb", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% Event functions %%%
start_upload_event(image) -> ok.

collect_tripcode() ->
    case wf:q(txt_tripcode) of
	"" -> false;
	Trip -> IP = lists:map(fun erlang:integer_to_list/1, tuple_to_list(wf:peer_ip())),
		string:join([Trip | IP], ".")
    end.

collect_comment(LocalFileName) ->
    Body = wf:q(txt_comment), 
    Username = wf:q(txt_user_name),
    Trip = wf:q(txt_tripcode),
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
	    Res = {Id, _, _, _} = rpc:call(?NODE, board, new_thread, [Board, Comment]),
	    wf:send_global(Board, {thread, Res}),
	    wf:redirect(uri(Id));
	Thread -> 
	    Res = rpc:call(?NODE, board, reply, [Board, Thread, Comment]),
	    Summary = rpc:call(?NODE, board, summarize, [{Board, Thread}]),
	    wf:send_global(Board, {thread_update, Thread, Summary}),
	    wf:send_global(Thread, {message, Res})
    end,
    wf:set(txt_comment, "").

make_tempname() ->
    make_tempname(filename:nativename("/tmp")).
make_tempname(TargetDir) ->
    {A, B, C} = now(),
    [D, E, F] = lists:map(fun integer_to_list/1, [A, B, C]),
    Tempname = lists:append(["tmp.", D, ".", E, ".", F]),
    filename:absname_join(TargetDir, Tempname).

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
	    wf:insert_top(messages, summary(Thread));
	{thread_update, ThreadId, ThreadSummary} ->
	    wf:remove(now_to_thread_id(ThreadId)),
	    wf:insert_top(messages, summary(ThreadSummary)),
	    wf:insert_bottom(messages, #span {actions = highlight(".thread:first")});
        {message, Comment} ->
            wf:insert_bottom(messages, comment(Comment)),
	    wf:insert_bottom(messages, #span {actions = highlight(".comment:last")})
    end,
    wf:flush(),
    post_loop().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
