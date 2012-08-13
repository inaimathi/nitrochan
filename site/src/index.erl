-module (index).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("records.hrl").

main() -> #template { file="./site/templates/bare.html" }.

title() -> "Welcome to ErlChan".

body() ->
    #container_12 {body=[#grid_8{alpha=true, prefix=2, suffix=2, omega=true, body=inner_body()}]}.

inner_body() -> 
    [
     #crumbs{},
     #h1 { text="Welcome to NitroChan" },
     #board_list{extra_classes=[full_page], big=true}
    ].

event(click) ->
    wf:replace(button, #panel { 
		 body="You clicked the button!", 
		 actions=#effect { effect=highlight }
		}).
