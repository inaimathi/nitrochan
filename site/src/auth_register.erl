-module(auth_register).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("records.hrl").

main() -> #template { file="./site/templates/bare.html" }.
title() -> "Register".
body() -> #container_12{body=[#grid_8{alpha=true, prefix=2, suffix=2, omega=true, body=inner_body()}]}.

inner_body() -> 
    Val = [{txt_username, [#is_required {text="Required"},
			   #custom {text="That username is already taken.",
				    function=fun unique_name_p/2}]},
	   {txt_passphrase, [#is_required {text="Yes, you need a passphrase. No, it doesn't need to be any good."},
			     #min_length {text="Ok, maybe better than that. Try at least 8 letters.", length=8}]},
	   {txt_confirm, [#confirm_password {text="Passwords must match", password=txt_passphrase}]}],
    util:validators(btn_register, Val),
    [
     #h1 { text="Log In" },
     #label { text="Username" }, #textbox { id=txt_username, next=txt_passphrase },
     #label { text="Passphrase" }, #password { id=txt_passphrase, next=txt_confirm }, 
     #label { text="Confirm Passphrase" }, #password { id=txt_confirm, next=txt_pubkey },
     #label { text="Public Key (optional)" }, #textarea { id=txt_pubkey }, #br{},
     #button { id=btn_register, text="Register", postback=register }
    ].

unique_name_p(_Tag, Value) ->
    case rpc:call(?AUTH_NODE, users, get, [Value]) of
	false -> true;
	_ -> false
    end.

event(register) ->
    [User, Pass] = util:q([txt_username, txt_passphrase]),
    rpc:call(?AUTH_NODE, users, register, [User, Pass]),
    wf:session(admin_groups, []),
    wf:user(User),
    wf:redirect_from_login("/").
