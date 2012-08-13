-module(auth_rsa_manual).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("records.hrl").

main() -> #template { file="./site/templates/bare.html" }.
title() -> "Manual RSA Login".
body() -> #container_12{body=[#grid_8 { alpha=true, prefix=2, suffix=2, omega=true, body=inner_body()}]}.

inner_body() -> 
    Val = [{txt_username, [#is_required {text="You're definitely not ` `. I know that guy."}]}],
    util:validators(btn_send, Val),
    [
     #crumbs{},
     #h1{ text="Manual RSA Log In"},
     #label {text="Username: "},
     #textbox { id=txt_username, next=btn_send },
     #button { id=btn_send, text="Request Secret", postback=send_user },

     #panel { id=auth_token },
     #link{text="Log in the dumb way", url="/auth/login"}
    ].

event(send_user) ->
    {Token, Sig} = rpc:call(?AUTH_NODE, rsa_auth, gen_secret, [util:q(txt_username), wf:peer_ip()]),
    wf:update(auth_token,
	      [#label{text="Auth Token"},
	       #span{class=rsa_token, text=Token},
	       #label{text="Token Signature"},
	       #span{class=rsa_token, text=Sig},
	       #textarea { id=txt_auth_response },
	       #button { id=send_signed, text="Send Signed", postback=send_signed }]);
event(send_signed) ->
    Args = [wf:q(txt_username), wf:peer_ip(), 
            re:replace(wf:q(txt_auth_response), "\\\\n", "\n", [global, {return, list}])],
    Res = rpc:call(?AUTH_NODE, rsa_auth, verify, Args),
    case Res of
        {_Id, User, Groups} -> wf:session(admin_groups, Groups),
			       wf:user(User),
			       case lists:member(admin, Groups) of
				   true -> wf:role(admin, true);
				   _ -> false
			       end,
			       wf:redirect_from_login("/");
        _ -> wf:update(auth_token, [ #span {text="Authentication fail. Try it again."} ])
    end.
