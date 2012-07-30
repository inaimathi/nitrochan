%% -*- mode: nitrogen -*-
-module (element_comment_form).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

reflect() -> record_info(fields, comment_form).

render_element(_Record = #comment_form{}) ->
    [#flash{},
     #label { text="Username" }, 
     #textbox { id=txt_user_name, next=tripcode, 
		text=wf:coalesce([wf:session(username), ""]) },
     #label { text="Tripcode" }, 
     #textbox { id=txt_tripcode, next=comment,
		text=wf:coalesce([wf:session(tripcode), ""])},
     #label { text="Comment" }, #textarea { id=txt_comment },
     #label { text="Image"}, #upload { id=txt_image, tag=image, button_text="Submit" }].
