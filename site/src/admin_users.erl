-module(admin_users).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("records.hrl").

main() -> 
    case wf:role(admin) of
        true -> #template { file="./site/templates/bare.html" };
        _ -> wf:redirect_to_login("/auth/login")
    end.

title() -> "Admin".

body() ->
    #container_12 { body=[
        #grid_8 { alpha=true, prefix=2, suffix=2, omega=true, body=inner_body() }
    ]}.

inner_body() -> 
    Users = rpc:call(?AUTH_NODE, users, list, []),
    GroupList = rpc:call(?AUTH_NODE, groups, list, []),
    lists:map(fun insert_user/1, Users),
    [
     #h1 { text="User Administration" },
     #panel{class=group_box,
	    body=[#label{text="Users"},
		  #textbox{id=filter, postback=filter},
		  #panel{class=group_block, 
			 body=lists:map(fun ({_, Username, _}) -> 
						#draggable{body=[Username], 
							   tag=Username, class=user_draggable, 
							   clone=true, revert=false}
					end, Users)}]},
     lists:map(fun ({group, Id, Groupname, _, _, _}) ->
		       CssId = util:to_group_id(Id),
		       #panel{class=group_box,
			      body=[#label{text=Groupname},
				    #droppable{id=CssId, tag=Id, class=group_block, body=[]}]}
	       end, GroupList)
    ].

insert_to_group({GroupId, Username}) ->
    Id = wf:temp_id(),
    CssId = util:to_group_id(GroupId),
    RemLink = #link{show_if=(wf:user() /= Username), 
		    text="[ - ]", 
		    class=remove_link, 
		    postback={remove, Id, GroupId, Username}},
    wf:insert_bottom(CssId, [#span{id=Id, body=[Username, RemLink]}, #br{}]).

insert_user({_, Username, Groups}) ->
    lists:map(fun (Id) -> insert_to_group({Id, Username}) end, Groups).

drop_event(DragTag, DropTag) ->
    rpc:call(?AUTH_NODE, groups, add_to, [DropTag, {user, DragTag}]),
    insert_to_group({DropTag, DragTag}).

event({remove, Id, GroupId, Username}) ->
    rpc:call(?AUTH_NODE, groups, remove_from, [GroupId, {user, Username}]),
    wf:remove(Id);
event(filter) ->
    %% Hides non-matching user entries (god I wish I had a JS generator here)
    wf:wire(lists:append(["$('.user_draggable').each( function () { if ($(this).text().search('", util:q(filter), "')==-1) { $(this).hide(); } else { $(this).show()}});"])).
