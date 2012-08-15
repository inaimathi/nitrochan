
-module (element_crumbs).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

local(Target, Type, Postback) ->
    wf:wire(Target, #event{ type=Type, postback=Postback, delegate=?MODULE}).

reflect() -> record_info(fields, crumbs).

render_element(Rec = #crumbs{}) ->
    [LogId, RegId, LogoutId] = util:temp_id(3),
    local(LogId, click, login),
    local(RegId, click, register),
    local(LogoutId, click, logout),
    #panel{class=breadcrumb_trail,
	   body=[#link {text="Home", url="/"}, " / ", 
		 links(Rec#crumbs.board, Rec#crumbs.thread),
		 #span{show_if=(wf:user() /= undefined), class=profile_links,
		       body=[#link{text="Profile", url="/profile"},
			     #link{text="Log Out", id=LogoutId}]},
		 #span{show_if=(wf:user() == undefined), class=profile_links,
		       body=[#link{text="Log In", id=LogId},
			     #link{text="Register", id=RegId}]},
		 #span{show_if=util:admin_p(), class=profile_links,
		       body=[#link{text="Manage Users", url="/admin/users"}, 
			     #link{text="New Board", url="/admin/boards/new"}]},
		#br{class=clear}]}.

links(Board, undefined) -> Board;
links(Board, Thread) ->
    [#link {text=Board, url=util:uri({board, Board})}, " / ", Thread].

event(logout) -> wf:clear_roles(),
		 wf:clear_user(),
		 wf:clear_state(),
		 wf:clear_session(),
		 wf:wire("location.reload()");
event(login) -> wf:redirect_to_login("/auth/login");
event(register) -> wf:redirect_to_login("/auth/register").
