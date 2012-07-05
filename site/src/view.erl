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
    wf:state(thread, false),
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
     #label { text="Username" }, #textbox { id=txt_user_name, next=tripcode },
     #label { text="Tripcode" }, #textbox { id=txt_tripcode, next=comment },
     #label { text="Comment" }, #textarea { id=txt_comment },
     #label { text="Image"}, #upload { id=txt_image, tag=image, button_text="Submit" }].

summary({ThreadId, _LastUpdate, _CommentCount, [FirstComment | LastComments]}) ->
    #panel {class=thread,
	    body = [ #link{text="Reply", url=uri(ThreadId)} |
		     case length(LastComments) of
			 N when N < 4 ->
			     lists:map(fun comment/1, [FirstComment | LastComments]);
			 _ -> 
			     [comment(FirstComment), #span{text="..."} | lists:map(fun comment/1, LastComments)]
		     end]}.

comment({Id, User, Tripcode, Body, File}) ->
    #span {class=comment,
	   body=[#span{ class=username, text=User }, #span{ class=tripcode, text=bin_to_hex(Tripcode) },
		 #span{ class=comment_id, text=now_to_id_string(Id) },
		 #span{ class=comment_datetime, text=now_to_datetime_string(Id) },
		 #image{ image=File },
		 #span{ text=Body }]}.

uri(ThreadId) ->
    lists:append(["/view/", atom_to_list(wf:state(board)), "/", now_to_id_string(ThreadId), "/"]).

now_to_datetime_string(Now) ->
    {{Y, M, D}, {H, Min, _S}} = calendar:now_to_datetime(Now),
    [Ys, Ds, Hs, Ms] = lists:map(fun integer_to_list/1, [Y, D, H, Min]),
    lists:append([Ys, ", ", month_name(M), ", ", Ds, " -- ", Hs, ":", Ms]).

now_to_id_string(Now) ->
    Res = lists:map(fun erlang:integer_to_list/1, tuple_to_list(Now)),
    string:join(Res, ".").
    
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

collect_comment() ->
    case {wf:q(txt_comment), wf:q(txt_image)} of
	{"", ""} -> false;
	_ -> IP = string:join(lists:map(fun erlang:integer_to_list/1, tuple_to_list(wf:peer_ip())), ","),
	     {wf:q(txt_user_name), 
	      string:join([IP, wf:q(txt_tripcode)], "||"), 
	      wf:q(txt_comment), 
	      wf:q(txt_image)}
    end.

finish_upload_event(_Tag, undefined, _, _) ->
    %% Comment with no image (require a comment in this case)
    case collect_comment() of
	false -> wf:flash("You need either a comment or an image");
	Comment -> Board = wf:state(board),
		   case wf:state(thread) of
		       false -> 
			   Res = {Id, _, _, _} = rpc:call(?NODE, board, new_thread, [Board, Comment]),
			   wf:send_global(Board, {thread, Res}),
			   wf:redirect(uri(Id));
		       Thread -> 
			   Res = rpc:call(?NODE, board, reply, [Board, Thread, Comment]),
			   wf:send_global(Thread, {message, Res})
		   end,
		   wf:set(txt_comment, "")
    end;
finish_upload_event(_Tag, FileName, LocalFileData, Node) ->
    %% Comment with image (no other fields required)
    FileSize = filelib:file_size(LocalFileData),
    wf:flash(wf:f("Uploaded file: ~s (~p bytes) on node ~s.", [FileName, FileSize, Node])),
    ok.

post_loop() ->
    receive 
        'INIT' -> ok; %% init is sent to the first client in the comet pool. We don't care in this case.
	{thread, Thread} ->
	    wf:insert_top(messages, summary(Thread)),
	    wf:flush();
        {message, Comment} ->
            wf:insert_bottom(messages, comment(Comment)),
            wf:flush()
    end,
    post_loop().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
