-module (element_comment_form).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

reflect() -> record_info(fields, comment_form).

render_element(_Record = #comment_form{}) ->
    [#panel{id=comment_form,
	    body=[#flash{},
		  #login_bar{},
		  #panel{show_if=(wf:user() == undefined),
			 body=[#label{text="Username"}, 
			       #textbox{id=txt_user_name, next=tripcode, 
					text=wf:coalesce([wf:user(), wf:session(username), ""])},
			       #label{text="Tripcode"}, 
			       #textbox{id=txt_tripcode, next=comment,
					text=wf:coalesce([wf:session(tripcode), ""])}]},
		  #textarea{id=txt_comment},
		  #upload{id=txt_image, tag=image, button_text="Submit"}]}].
    
collect_tripcode() ->
    case {wf:user(), util:q(txt_tripcode)} of
	{undefined, ""} -> false;
	{undefined, Trip} -> IP = lists:map(fun erlang:integer_to_list/1, tuple_to_list(wf:peer_ip())),
			     string:join([Trip | IP], ".");
	{User, _} when is_list(User) -> registered
    end.

process_comment_body(Body) -> 
    StripFn = fun (Str, Reg) -> re:replace(Str, Reg, "", [{return, list}]) end,
    Stripped = StripFn(StripFn(Body, "^[\s\n]+"), "[\s\n]+$"),
    Split = re:split(Stripped, "\n", [{return, list}]),
    {lists:sublist(Stripped, 250), Split}.

collect_comment(LocalFileName) ->
    Body = util:q(txt_comment), 
    Username = wf:coalesce([wf:user(), util:q(txt_user_name)]),
    Trip = wf:coalesce([util:q(txt_tripcode), ""]),
    case {Body, LocalFileName, length(Body) > 3000, length(Username) > 100, length(Trip) > 250} of
	{"", undefined, _, _, _} -> {false, "You need either a comment or an image"};
	{_, _, true, _, _} -> {false, "Your comment can't be longer than 3000 characters. What the fuck are you writing, a novel?"};
	{_, _, _, true, _} -> {false, "Your username can't be longer than 100 characters. And even that's excessive."};
	{_, _, _, _, true} -> {false, "Your tripcode can't be longer than 250 characters. Really, you're secure by like 132. Anything after that is wasted effort."};
	_ -> wf:session(username, Username), wf:session(tripcode, util:q(txt_tripcode)),
 	     {Preview, FinalBody} = process_comment_body(Body),
	     {Username, element_comment_form:collect_tripcode(), FinalBody, Preview, LocalFileName}
    end.
