-module (element_thread_summary).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

reflect() -> record_info(fields, thread_summary).

from_tup({ThreadId, ThreadStatus, LastUpdate, CommentCount, [FirstComment | LastComments]}) ->
    #thread_summary{thread_id=ThreadId, status=ThreadStatus, last_update=LastUpdate, comment_count=CommentCount, 
		    first_comment=FirstComment, last_comments=LastComments}.

render_element(Rec = #thread_summary{}) ->
    #panel {class=thread, id=util:now_to_thread_id(Rec#thread_summary.thread_id),
	    body = [ case Rec#thread_summary.status of
			 active -> [#link{text="Reply", url=util:uri(Rec#thread_summary.thread_id)}, " ::: "];
			 _ -> ""
		     end,
		     #span{ class=thread_datetime, text=util:now_to_datetime_string(Rec#thread_summary.last_update) } |
		     case length(Rec#thread_summary.last_comments) of
			 N when N < 4 ->
			     lists:map(fun element_comment:from_tup/1, [Rec#thread_summary.first_comment | Rec#thread_summary.last_comments]);
			 _ -> 
			     [element_comment:from_tup(Rec#thread_summary.first_comment), 
			      #span{text=wf:f("... snip [~p] comments...", [Rec#thread_summary.comment_count - 4])} | 
			      lists:map(fun element_comment:from_tup/1, Rec#thread_summary.last_comments)]
		     end]}.
