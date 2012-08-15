-module(profile).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("records.hrl").

main() -> #template { file="./site/templates/bare.html" }.
title() -> "Login".
body() -> 
    case wf:user() of
	undefined -> wf:redirect_to_login("/auth/login");
	_ -> #container_12{body=[#grid_8{alpha=true, prefix=2, suffix=2, omega=true, body=inner_body()}]}
    end.

inner_body() -> 
    PassVal = [{txt_passphrase, [#is_required {text="Yes, you need a passphrase. No, it doesn't need to be very good."},
			     #min_length {text="Ok, maybe better than that. Try at least 8 letters.", length=8}]},
	   {txt_confirm, [#confirm_password {text="Your passwords must match", password=txt_passphrase}]}],
    KeyVal = [{txt_pubkey, [#is_required {text="I'm not just taking a blank key from you."},
			   #custom{text="You need to give me a valid RSA Public Key.", function=fun valid_pubkey/2}]}],
    util:validators(btn_change_pass, PassVal),
    util:validators(btn_change_key, KeyVal),
    Pubkey = rpc:call(?AUTH_NODE, rsa_auth, get_key, [wf:session(user_id)]),
    erlang:display(Pubkey),
    [
     #crumbs{},
     #h1 { text="User Profile" },
     #panel { class=change_pass_form,
	      body=[#label { text="Change Passphrase" }, #password { id=txt_passphrase, next=txt_confirm }, 
		    #label { text="Confirm Passphrase" }, #password { id=txt_confirm, next=btn_change_pass }, #br{},
		    #button { id=btn_change_pass, text="Change Password", postback=change_pass }] },
     #panel {class=change_key_form, 
	     body=[#label { text=case Pubkey of
				     false -> "Add Public Key";
				     _ -> "Change Public Key"
				 end}, 
		   #textarea { id=txt_pubkey }, #br{},
		   #button { id=btn_change_key, text="Change Key", postback={change_key, Pubkey} }]}
    ].

valid_pubkey(_Tag, Value) -> 
    rpc:call(?AUTH_NODE, rsa_auth, validate_keystring, [Value]).

event(change_pass) -> 
    rpc:call(?AUTH_NODE, users, change_password, [wf:user(), util:q(txt_passphrase)]),
    wf:wire(util:highlight(".change_pass_form")),
    wf:set(txt_passphrase, ""), wf:set(txt_confirm, ""),
    erlang:display({change_pass, wf:user(), util:q(txt_passphrase)});
event({change_key, CurrentKey}) -> 
    erlang:display(case CurrentKey of
		       false -> new_key;
		       _ -> change_key
		   end),
    rpc:call(?AUTH_NODE, rsa_auth, 
	     case CurrentKey of
		 false -> new_key;
		 _ -> change_key
	     end,
	     [wf:session(user_id), util:q(txt_pubkey)]),
    wf:wire(util:highlight(".change_key_form")),
    wf:set(txt_pubkey, ""),
    erlang:display({change_key, wf:user(), CurrentKey, util:q(txt_pubkey)}).
