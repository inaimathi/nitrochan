-module(auth_login).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("records.hrl").

main() -> #template { file="./site/templates/bare.html" }.
title() -> "Login".
body() -> 
    case wf:user() of
	undefined -> #container_12{body=[#grid_8{alpha=true, prefix=2, suffix=2, omega=true, body=inner_body()}]};
	_ -> wf:redirect_from_login("/")
    end.

inner_body() -> 
    Val = [{txt_username,
	    [#is_required {text="You're definitely not ` `. I know that guy."},
	     #custom { text="Incorrect user name or passphrase.", 
		       function=fun authenticate/2}]},
	   {txt_passphrase,
	    [ #is_required {text="What, you're not even gonna guess?"}]}],
    util:validators(btn_login, Val),
    [
     #crumbs{},
     #h1 { text="Log In" },
     #link{text="Log in the secure way", url="/auth/rsa/manual"},
     #label { text="Username" }, #textbox { id=txt_username, next=txt_passphrase },
     #label { text="Passphrase" }, #password { id=txt_passphrase, next=btn_login }, #br{},
     #button { id=btn_login, text="Login", postback=login }
     
    ].

authenticate(_Tag, Value) ->
    case rpc:call(?AUTH_NODE, users, auth, [Value, util:q(txt_passphrase)]) of
	{Id, User, Groups} -> 
	    util:populate_session(Id, User, Groups),
	    true;
	_ -> false
    end.

event(login) -> wf:redirect_from_login("/").
