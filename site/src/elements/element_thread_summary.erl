-module (element_thread_summary).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

reflect() -> record_info(fields, thread_summary).

from_prop(Proplist) -> #thread_summary{properties=Proplist}.

render_element(#thread_summary{properties=Props}) ->
    case proplists:get_value(status, Props) of
	deleted -> [Id, Latest] = util:get_values([id, last_update], Props),
		   #panel {class=[thread, deleted], id=util:now_to_thread_id(Id),
			   body = [ #span{ class=notice, text="Deleted" },
				    #span{ class=thread_datetime, text=util:now_to_datetime_string(Latest) },
				    #thread_moderation{thread_id=Id, status=deleted}]};
	Status -> Keys = [id, last_update, first_comment, last_comments, comment_count],
		  [Id, Latest, First, Last, Count] = util:get_values(Keys, Props),
		  #panel {class=thread, id=util:now_to_thread_id(Id),
			  body = [ #link{text="Reply", url=util:uri({thread, Id})}, " ::: ",
				   #span{ class=thread_datetime, text=util:now_to_datetime_string(Latest) },
				   #thread_moderation{thread_id=Id, status=Status}|
				   case Count of
				       N when N < 6 ->
					   lists:map(fun comment_summary/1, [First | Last]);
				       _ -> 
					   [element_comment:from_prop(First), 
					    #span{text=wf:f("... snip [~p] comments...", [Count - 5])} | 
					    lists:map(fun comment_summary/1, Last)]
				   end]}
    end.

comment_summary(C) -> element_comment:from_prop({summary, C}).
