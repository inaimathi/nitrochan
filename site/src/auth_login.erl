-module(auth_login).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("records.hrl").

main() -> #template { file="./site/templates/bare.html" }.
title() -> "Login".
body() -> #container_12{body=[#grid_8 { alpha=true, prefix=2, suffix=2, omega=true, body=inner_body()}]}.

inner_body() -> 
    Val = [{txt_username,
	    [#is_required {text="You're definitely not ` `. I know that guy."},
	     #custom { text="Incorrect user name or passphrase.", 
		       function=fun authenticate/2}]},
	   {txt_passphrase,
	    [ #is_required {text="What, you're not even gonna guess?"}]}],
    util:validators(btn_login, Val),
    [
     #h1 { text="Log In" },
     #label { text="Username" }, #textbox { id=txt_username, next=txt_passphrase },
     #label { text="Passphrase" }, #password { id=txt_passphrase, next=btn_login }, #br{},
     #button { id=btn_login, text="Login", postback=login }
    ].

authenticate(_Tag, Value) ->
    case rpc:call(?AUTH_NODE, users, auth, [Value, wf:q(txt_passphrase)]) of
	{_Id, Username, Groups} -> 
	    wf:session(groups, Groups),
	    wf:user(Username),
	    case lists:member(nitrochan_admin, Groups) of
		true -> wf:role(admin);
		_ -> false
	    end,
	    true;
	_ -> false
    end.

event(login) -> wf:redirect_from_login("/").
