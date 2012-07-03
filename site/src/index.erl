%% -*- mode: nitrogen -*-
-module (index).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

main() -> #template { file="./site/templates/bare.html" }.

title() -> "Welcome to ErlChan".

body() ->
    #container_12 { body=[
        #grid_8 { alpha=true, prefix=2, suffix=2, omega=true, body=inner_body() }
    ]}.

inner_body() -> 
    [
     #h1 { text="Welcome to Erl-Chan" },
     #panel { body=lists:map(
		     fun (B) -> 
			     N = atom_to_list(B),
			     #p { body=[ #link{ 
					    text=N,
					    url= "/view/" ++ N
					   }]}
		     end,
		     rpc:call('erl_chan@127.0.1.1', board, list, []))}
    ].

event(click) ->
    wf:replace(button, #panel { 
        body="You clicked the button!", 
        actions=#effect { effect=highlight }
    }).
