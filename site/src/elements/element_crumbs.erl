
-module (element_crumbs).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

reflect() -> record_info(fields, crumbs).

render_element(Rec = #crumbs{}) ->
    #panel{class=breadcrumb_trail,
	   body=[#link {text="Home", url="/"}, " / ", 
		 links(Rec#crumbs.board, Rec#crumbs.thread),
		 #span{show_if=(wf:user() /= undefined), class=admin_links,
		       body=[#link{text="Profile", url="/profile"}]},
		 #span{show_if=util:admin_p(), class=admin_links,
		       body=[#link{text="Manage Users", url="/admin/users"}, 
			     #link{text="New Board", url="/admin/boards/new"}]},
		#br{class=clear}]}.

links(Board, undefined) -> Board;
links(Board, Thread) ->
    [#link {text=Board, url=util:uri({board, Board})}, " / ", Thread].
