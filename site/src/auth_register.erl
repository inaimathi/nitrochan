-module(auth_register).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("records.hrl").

main() -> #template { file="./site/templates/bare.html" }.
title() -> "Register".
body() -> 
    case wf:user() of
	undefined -> #container_12{body=[#grid_8{alpha=true, prefix=2, suffix=2, omega=true, body=inner_body()}]};
	_ -> wf:redirect(wf:header(referer))
    end.

inner_body() -> 
    Val = [{txt_username, [#is_required {text="Required"},
			   #custom {text="That username is already taken.",
				    function=fun unique_name_p/2}]},
	   {txt_passphrase, [#is_required {text="Yes, you need a passphrase. No, it doesn't need to be any good."},
			     #min_length {text="Ok, maybe better than that. Try at least 8 letters.", length=8}]},
	   {txt_confirm, [#confirm_password {text="Passwords must match", password=txt_passphrase}]},
	   {txt_pubkey, [#custom { text="Either leave it blank, or gimme a valid RSA Public Key.",
				   function=fun valid_pubkey/2}]}],
    Pub = rpc:call(?AUTH_NODE, rsa_auth, get_key, []),
    util:validators(btn_register, Val),
    [
     #crumbs{},
     #h1 { text="Log In" },
     #label { text="Username" }, #textbox { id=txt_username, next=txt_passphrase },
     #label { text="Passphrase" }, #password { id=txt_passphrase, next=txt_confirm }, 
     #label { text="Confirm Passphrase" }, #password { id=txt_confirm, next=txt_pubkey },
     #label { text="Public Key (optional)" }, #textarea { id=txt_pubkey }, #br{},
     #button { id=btn_register, text="Register", postback=register },
     #label { text="Server Public Key" },
     #span { class=rsa_token, text=Pub }
    ].

unique_name_p(_Tag, Value) ->
    case rpc:call(?AUTH_NODE, users, get, [Value]) of
	false -> true;
	_ -> false
    end.

valid_pubkey(_Tag, "") -> true;
valid_pubkey(_Tag, Value) -> 
    rpc:call(?AUTH_NODE, rsa_auth, validate_keystring, [Value]).

event(register) ->
    [User, Pass, PubKey] = util:q([txt_username, txt_passphrase, txt_pubkey]),
    {Id, _, _} = rpc:call(?AUTH_NODE, users, register, [User, Pass]),
    case PubKey of
	"" -> no_key;
	_ -> rpc:call(?AUTH_NODE, rsa_auth, new_key, [Id, PubKey])
    end,
    wf:session(admin_groups, []),
    wf:user(User),
    wf:redirect_from_login("/").
