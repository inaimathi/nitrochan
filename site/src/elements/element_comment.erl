-module (element_comment).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

reflect() -> record_info(fields, comment).

from_tup({Id, User, Tripcode, Body, File}) ->
    #comment{comment_id=Id, user=User, body=Body, file=File,
	     tripcode=util:trip_to_string(Tripcode)}.

render_element(Rec = #comment{}) ->
    Class = ".comment ." ++ Rec#comment.tripcode,
    #span {class=comment,
	   body=[#span{ class=username, 
			text=case Rec#comment.user of
				 [] -> rpc:call(?NODE, board, default_name, [wf:state(board)]);
				 _ -> Rec#comment.user
			     end}, 
		 #span{ class=[tripcode, Rec#comment.tripcode], text=Rec#comment.tripcode, 
			actions=#event{ target=Class, 
					type=mouseover, 
					actions=util:highlight()} },
		 #span{ class=comment_id, text=util:now_to_id_string(Rec#comment.comment_id) },
		 #span{ class=comment_datetime, text=util:now_to_datetime_string(Rec#comment.comment_id) },
		 #br{ class=clear },
		 case Rec#comment.file of
		     undefined -> "";
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
