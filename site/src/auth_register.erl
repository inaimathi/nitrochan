-module(auth_register).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("records.hrl").

main() -> #template { file="./site/templates/bare.html" }.

title() -> "Welcome to ErlChan".

body() ->
    #container_12 { body=[
        #grid_8 { alpha=true, prefix=2, suffix=2, omega=true, body=inner_body() }
    ]}.

inner_body() -> 
    wf:wire(btn_register, txt_username, 
	    #validate{ validators=[ #is_required {text="Required"},
				    #custom { text="Incorrect user name or password.", 
					      function=fun authenticate/2 } ]}),
    wf:wire(btn_register, txt_password,
	    #validate{ validators=[ #is_required {text="What, you're not even gonna guess?"}]}),
    [
     #h1 { text="Log In" },
     #label { text="Username" }, #textbox { id=txt_username, next=password },
     #label { text="Passphrase" }, #password { id=txt_passphrase, next=txt_confirm }, 
     #label { text="Confirm Passphrase" }, #password { id=txt_confirm, next=txt_pubkey },
     #label { text="Public Key" }, #textarea { id=txt_pubkey }, #br{},
     #button { id=btn_register, text="Register", postback=register }
    ].

authenticate(_Tag, Value) ->
    wf:flash( Value ++ " :: " ++ wf:q(txt_password) ),
    Value == "secret".

event(click) ->
    wf:replace(button, #panel { 
        body="You clicked the button!", 
        actions=#effect { effect=highlight }
    }).
