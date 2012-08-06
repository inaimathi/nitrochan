-module (element_login_bar).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

local(Target, Type, Postback) ->
    wf:wire(Target, #event{ type=Type, postback=Postback, delegate=?MODULE}).

reflect() -> record_info(fields, login_bar).

render_element(_Rec = #login_bar{}) ->
    case wf:user() of
	undefined -> 
	    [LogId, RegId] = util:temp_id(2),
	    local(LogId, click, login),
	    local(RegId, click, register),
	    #panel{class=auth_panel,
		   body=[#link{id=LogId, text="Login"},
			 #link{id=RegId, text="Register"}]};
	Username ->
	    LogoutId = util:temp_id(),
	    local(LogoutId, click, logout),
	    #panel{class=logged_in_panel,
		   body=[#span{class=username, text=Username},
			 #link{id=LogoutId, text="Log Out"}]}
    end.

event(logout) -> wf:clear_roles(),
		 wf:clear_user(),
		 wf:clear_state(),
		 wf:clear_session(),
		 wf:wire("location.reload()");
event(login) -> wf:redirect_to_login("/auth/login");
event(register) -> wf:redirect_to_login("/auth/register").
