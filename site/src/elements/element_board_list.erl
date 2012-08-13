-module (element_board_list).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

reflect() -> record_info(fields, board_list).

render_element(Rec = #board_list{}) ->
    #list{class=[board_list | Rec#board_list.extra_classes],
	  body= lists:map(fun (BoardProp) -> 
				  [Board, Desc] = util:get_values([name, description], BoardProp),
				  #listitem{ body=[ #link{ text=atom_to_list(Board), url=util:uri({board, Board}) },
						    case Rec#board_list.big of
							true -> [#br{}, #span{ text=Desc }];
							_ -> ""
						    end ]}
			  end,
			  rpc:call(?BOARD_NODE, board, list, []))}.
