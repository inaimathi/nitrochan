-module (element_thread_summary).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

reflect() -> record_info(fields, thread_summary).

from_tup({ThreadId, ThreadStatus, LastUpdate, CommentCount, [FirstComment | LastComments]}) ->
    #thread_summary{thread_id=ThreadId, status=ThreadStatus, last_update=LastUpdate, comment_count=CommentCount, 
		    first_comment=FirstComment, last_comments=LastComments}.

render_element(Rec = #thread_summary{status=deleted, thread_id=Id}) ->
    #panel {class=[thread, deleted], id=util:now_to_thread_id(Id),
	    body = [ #span{ class=notice, text="Deleted" },
		     #span{ class=thread_datetime, text=util:now_to_datetime_string(Rec#thread_summary.last_update) },
		     #span{ show_if=wf:role(admin),
			    class=admin_links,
			    body=[ #link{show_if=wf:role(admin), text="Phoneix Down, Thread Edition", postback={revive_thread, Id}} ]},
		     #br{ show_if=wf:role(admin), class=clear }
		   ]};
render_element(Rec = #thread_summary{thread_id=Id}) ->
    #panel {class=thread, id=util:now_to_thread_id(Id),
	    body = [ #link{text="Reply", url=util:uri(Id)}, " ::: ",
		     #span{ class=thread_datetime, text=util:now_to_datetime_string(Rec#thread_summary.last_update) },
		     #span{ show_if=wf:role(admin),
			    class=admin_links,
			    body=[ #link{show_if=wf:role(admin), text="Delete Thread", postback={delete_thread, Id}} ]},
		     #br{ show_if=wf:role(admin), class=clear }|
		     case length(Rec#thread_summary.last_comments) of
			 N when N < 4 ->
			     lists:map(fun element_comment:from_tup/1, [Rec#thread_summary.first_comment | Rec#thread_summary.last_comments]);
			 _ -> 
			     [element_comment:from_tup(Rec#thread_summary.first_comment), 
			      #span{text=wf:f("... snip [~p] comments...", [Rec#thread_summary.comment_count - 4])} | 
			      lists:map(fun element_comment:from_tup/1, Rec#thread_summary.last_comments)]
		     end]}.
