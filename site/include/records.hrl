%%% custom elements
-record(comment_form, {?ELEMENT_BASE(element_comment_form)}).
-record(comment, {?ELEMENT_BASE(element_comment), comment_id, status, user, tripcode, body, file}).
-record(thread_summary, {?ELEMENT_BASE(element_thread_summary), thread_id, status, last_update, first_comment, last_comments, comment_count}).

-record(board_list, {?ELEMENT_BASE(element_board_list), big, extra_classes=[]}).

-record(crumbs, {?ELEMENT_BASE(element_crumbs), board, thread}).
-record(thread_moderation, {?ELEMENT_BASE(element_thread_moderation), thread_id, status}).

%%% custom actions

%%% node macros
-define(BOARD_NODE, 'erl_chan@127.0.1.1').
-define(AUTH_NODE, 'erl_chan@127.0.1.1').
