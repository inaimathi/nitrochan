-record(comment_form, {?ELEMENT_BASE(element_comment_form)}).
-record(comment, {?ELEMENT_BASE(element_comment), comment_id, user, tripcode, body, file}).
-record(thread_summary, {?ELEMENT_BASE(element_thread_summary), thread_id, status, last_update, first_comment, last_comments, comment_count}).

-define(NODE, 'erl_chan@127.0.1.1').
