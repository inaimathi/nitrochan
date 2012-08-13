-module(admin_boards_new).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("records.hrl").

main() -> 
    case {wf:user(), wf:role(admin)} of 
	{undefined, _} -> wf:redirect_to_login("/auth/login");
        {_, true}  -> #template { file="./site/templates/bare.html" };
	_ -> "Friggin denied" %% get a permission denied page
    end.

title() -> "Admin".

body() ->
    #container_12 { body=[
        #grid_8 { alpha=true, prefix=2, suffix=2, omega=true, body=inner_body() }
    ]}.

inner_body() -> 
    wf:wire(btn_ok, txt_board_name, #validate {validators=[#is_required{ text="Required" }]}),
    wf:wire(btn_ok, #event{ type=click, postback=new_board}),
    [
     #crumbs{},
     #h1 { text="New Board" },
     #label{text="Board Name"},
     #textbox{ id=txt_board_name, next=txt_description },
     #label{text="Description"},
     #textarea{ id=txt_description }, #br{},
     #button{ id=btn_ok, text="Ok" }
    ].

event(new_board) ->
    [BoardName, Description] = util:q([txt_board_name, txt_description]),
    Res = rpc:call(?BOARD_NODE, board, new, [list_to_atom(BoardName), Description]),
    erlang:display({Res, util:uri({board, BoardName})}),
    case Res of
	ok -> wf:redirect(util:uri({board, BoardName}));
	_ -> false
    end;
event(_) -> 
    ok.
