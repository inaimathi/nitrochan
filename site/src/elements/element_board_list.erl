-module (element_board_list).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

reflect() -> record_info(fields, board_list).

render_element(Rec = #board_list{}) ->
    #list{class=[board_list | Rec#board_list.extra_classes],
	  body= lists:map(fun (B) -> 
				  N = atom_to_list(B),
				  #listitem{ body=[#link{ text=N, url= "/view/" ++ N }] }
			  end,
			  rpc:call(?BOARD_NODE, board, list, []))}.
