-module (element_crumbs).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

reflect() -> record_info(fields, crumbs).

render_element(Rec = #crumbs{}) ->
    #panel{class=breadcrumb_trail,
	   body=[#link {text="Home", url="/"}, " / ", 
		 links(Rec#crumbs.board, Rec#crumbs.thread)]}.

links(Board, undefined) -> Board;
links(Board, Thread) ->
    [#link {text=Board, url="/view/" ++ Board}, " / ", Thread].
