-module(board).
-behaviour(gen_server).

-export([start/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([create/0, clear/0, recreate/0]).

-record(board, {name, created, max_threads, kill_inactive_after}).
-record(thread, {id, board, last_update, summary, comment_count}).
-record(comment, {id, user, tripcode, body, file, password}).

-export([summarize/1, new_thread/2, get_thread/2, reply/3]).

summarize(Board) -> gen_server:call(Board, summarize).
get_thread(Board, Thread) -> gen_server:call(Board, {get_thread, Thread}).

new_thread(Board, {User, Tripcode, Comment, File}) -> 
    gen_server:call(Board, {new_thread, User, Tripcode, Comment, File}).

reply(Board, Thread, {User, Tripcode, Comment, File}) -> 
    gen_server:call(Board, {reply, Thread, User, Tripcode, Comment, File}).

handle_call(summarize, _From, BoardName) -> 
    {reply, {lookup_summaries_for_board_threads, BoardName}, BoardName};
handle_call({get_thread, Thread}, _From, BoardName) -> 
    {reply, {all_comments_in_thread, Thread}, BoardName};
handle_call({new_thread, User, Tripcode, Comment, File}, _From, BoardName) -> 
    create_thread,
    add_first_commen,
    {reply, {start_new_thread_in, BoardName}, BoardName};
handle_call({reply, Thread, User, Tripcode, Comment, File}, _From, BoardName) -> 
    insert_comment,
    update_thread_summary_count_and_timestamp,
    {reply, {added_comment_to, Thread}, BoardName}.

%%%%%%%%%%%%%%%%%%%% DB-related
create() -> 
    mnesia:create_table(board, [{type, ordered_set}, {disc_copies, [node()]}, {attributes, record_info(fields, board)}]),
    mnesia:create_table(thread, [{type, ordered_set}, {attributes, record_info(fields, thread)}]),
    mnesia:create_table(comment, [{type, ordered_set}, {attributes, record_info(fields, comment)}]).
clear() -> lists:map(fun mnesia:delete_table/1, [board, thread, comment]).
recreate() -> clear(), create().

%%%%%%%%%%%%%%%%%%%% generic actions
start(BoardName) -> gen_server:start_link({local, BoardName}, ?MODULE, BoardName, []).
stop(BoardName) -> gen_server:call(BoardName, stop).

%%%%%%%%%%%%%%%%%%%% gen_server handlers
init(BoardName) -> {ok, BoardName}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
