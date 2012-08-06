-module (element_comment_form).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

reflect() -> record_info(fields, comment_form).

render_element(_Record = #comment_form{}) ->
    [#panel{id=comment_form,
	    body=[#flash{},
		  #login_bar{},
		  #panel{show_if=(wf:user() == undefined),
			 body=[#label{text="Username"}, 
			       #textbox{id=txt_user_name, next=tripcode, 
					text=wf:coalesce([wf:user(), wf:session(username), ""])},
			       #label{text="Tripcode"}, 
			       #textbox{id=txt_tripcode, next=comment,
					text=wf:coalesce([wf:session(tripcode), ""])}]},
		  #textarea{id=txt_comment},
		  #upload{id=txt_image, tag=image, button_text="Submit"}]}].
    
