-module(admin_boards).
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
     #h1 { text="Board Administration" },
     #hr{},
     #textbox{ id=txt_board_name, next=txt_description }, #br{},
     #textarea{ id=txt_description }, #br{},
     #button{ id=btn_ok, text="Ok" },
     #hr{},
     #board_list { extra_classes=[full_page]}
    ].

event(new_board) ->
    [BoardName, Description] = util:q([txt_board_name, txt_description]),
    Res = rpc:call(?BOARD_NODE, board, new, [list_to_atom(BoardName), Description]),
    case Res of
	{ok, Proc} -> wf:redirect(util:uri({board, BoardName}));
	_ -> false
    end;
event(_) -> 
    ok.
