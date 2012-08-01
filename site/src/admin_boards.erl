-module(admin_boards).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("records.hrl").

main() -> 
    case wf:user() /= undefined of 
        true  -> #template { file="./site/templates/bare.html" };
        false -> wf:redirect_to_login("/auth/login")
    end.

title() -> "Admin".

body() ->
    #container_12 { body=[
        #grid_8 { alpha=true, prefix=2, suffix=2, omega=true, body=inner_body() }
    ]}.

inner_body() -> 
    [
     #h1 { text="Board Administration" },
     #board_list { extra_classes=[full_page]}
    ].

event(click) ->
    wf:replace(button, #panel { 
        body="You clicked the button!", 
        actions=#effect { effect=highlight }
    }).
