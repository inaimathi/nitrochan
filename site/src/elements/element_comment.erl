-module (element_comment).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

reflect() -> record_info(fields, comment).

from_tup({Id, User, Tripcode, Body, File}) ->
    #comment{comment_id=Id, user=User, body=Body, file=File,
	     tripcode=Tripcode}.

render_element(Rec = #comment{}) ->
    Trip = util:trip_to_string(Rec#comment.tripcode),
    Class = ".comment ." ++ Trip,
    Id = util:now_to_id_string(Rec#comment.comment_id),
    #span {class=[comment, "comment" ++ Id],
	   body=[#span{ class= case Rec#comment.tripcode of
				   registered -> [username, registered];
				   _ -> username
			       end, 
			text=case Rec#comment.user of
				 [] -> rpc:call(?BOARD_NODE, board, default_name, [wf:state(board)]);
				 _ -> Rec#comment.user
			     end}, 
		 #span{ class=[tripcode, Trip], text=Trip, 
			actions=#event{ target=Class, 
					type=mouseover, 
					actions=util:highlight()} },
		 #span{ class=comment_id, text=Id },
		 #span{ class=comment_datetime, text=util:now_to_datetime_string(Rec#comment.comment_id) },
		 #span{ show_if=wf:role(admin),
			class=admin_links,
			body=[ #link{text="Delete Comment", postback={delete_comment, Rec#comment.comment_id}},
			       #link{show_if=(Rec#comment.file /= undefined), text="Delete Image", postback={delete_image, Rec#comment.comment_id}}]},
		 #br{ class=clear },
		 case Rec#comment.file of
		     undefined -> "";
		     deleted -> "FILE DELETED";
		     _ -> #link{ body=#image{ image="/images/preview/" ++ Rec#comment.file }, 
				 url="/images/big/" ++ Rec#comment.file }
		 end,
		 case Rec#comment.body of
		     [[]] -> "";
		     _ -> lists:map(fun ([]) -> #br{};
					(P) -> #p{ text=P } 
				    end, Rec#comment.body)
		 end,
		 #br{ class=clear }]}.
