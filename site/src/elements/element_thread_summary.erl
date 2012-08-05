-module (element_thread_summary).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

reflect() -> record_info(fields, thread_summary).

from_tup({ThreadId, ThreadStatus, LastUpdate, CommentCount, [FirstComment | LastComments]}) ->
    #thread_summary{thread_id=ThreadId, status=ThreadStatus, last_update=LastUpdate, comment_count=CommentCount, 
		    first_comment=FirstComment, last_comments=LastComments}.

render_element(#thread_summary{status=deleted, thread_id=Id, last_update=Latest}) ->
    #panel {class=[thread, deleted], id=util:now_to_thread_id(Id),
	    body = [ #span{ class=notice, text="Deleted" },
		     #span{ class=thread_datetime, text=util:now_to_datetime_string(Latest) },
		     #thread_moderation{thread_id=Id, status=deleted}
		   ]};
render_element(#thread_summary{thread_id=Id, last_update=Latest, status=Status, first_comment=First, last_comments=Last, comment_count=Count}) ->
    #panel {class=thread, id=util:now_to_thread_id(Id),
	    body = [ #link{text="Reply", url=util:uri(Id)}, " ::: ",
		     #span{ class=thread_datetime, text=util:now_to_datetime_string(Latest) },
		     #thread_moderation{thread_id=Id, status=Status}|
		     case Count of
			 N when N < 6 ->
			     lists:map(fun element_comment:from_tup/1, [First | Last]);
			 _ -> 
			     [element_comment:from_tup(First), 
			      #span{text=wf:f("... snip [~p] comments...", [Count - 5])} | 
			      lists:map(fun element_comment:from_tup/1, Last)]
		     end]}.
