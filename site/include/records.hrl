%%% custom elements
-record(anchor, {?ELEMENT_BASE(element_anchor), html_encode=true, text="", body="", name}).

-record(comment_form, {?ELEMENT_BASE(element_comment_form)}).
-record(comment, {?ELEMENT_BASE(element_comment), summary=false, properties}).
-record(thread_summary, {?ELEMENT_BASE(element_thread_summary), properties}).

-record(board_list, {?ELEMENT_BASE(element_board_list), big, extra_classes=[]}).

-record(crumbs, {?ELEMENT_BASE(element_crumbs), board, thread}).
-record(thread_moderation, {?ELEMENT_BASE(element_thread_moderation), thread_id, status}).
-record(login_bar, {?ELEMENT_BASE(element_login_bar)}).

%%% custom actions

%%% node macros
-define(BOARD_NODE, 'erl_chan@127.0.1.1').
-define(AUTH_NODE, 'erl_chan@127.0.1.1').
