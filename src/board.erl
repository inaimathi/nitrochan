-module(board).
-behaviour(gen_server).
-include_lib("stdlib/include/qlc.hrl").

-export([start/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([create/0, clear/0, recreate/0]).

-record(board, {name, created, max_threads=300}).
-record(thread, {id, board, last_update, first_comment, last_comments=[], comment_count}).
-record(comment, {id, thread, user, tripcode, body, file}).

-export([list/0, new/1, summarize/1, new_thread/2, get_thread/2, reply/3, last_n/3]).

list() -> do(qlc:q([X#board.name || X <- mnesia:table(board)])).

new(BoardName) ->
    {atomic, ok} = mnesia:transaction(fun () -> mnesia:write(#board{name=BoardName, created=now()}) end),
    supervisor:start_child(erl_chan_sup, erl_chan_sup:child_spec(BoardName)).

summarize(Board) -> gen_server:call(Board, summarize).
get_thread(Board, Thread) -> gen_server:call(Board, {get_thread, Thread}).

new_thread(Board, {User, Tripcode, Comment, File}) -> 
    gen_server:call(Board, {new_thread, User, Tripcode, Comment, File}).

reply(Board, Thread, {User, Tripcode, Body, File}) -> 
    gen_server:call(Board, {reply, Thread, User, Tripcode, Body, File}).

handle_call(summarize, _From, BoardName) -> 
    Res = do(qlc:q([{X#thread.last_update, X#thread.comment_count, [X#thread.first_comment | X#thread.last_comments]}
		    || X <- mnesia:table(thread), X#thread.board =:= BoardName])),
    {reply, Res, BoardName};
handle_call({get_thread, Thread}, _From, BoardName) -> 
    Res = do(qlc:q([{X#comment.id, X#comment.user, X#comment.tripcode, X#comment.body, X#comment.file} 
		    || X <- mnesia:table(comment), X#comment.thread =:= Thread])),
    {reply, Res, BoardName};
handle_call({new_thread, User, Tripcode, Body, File}, _From, BoardName) -> 
    ThreadId = now(), 
    CommentId = now(),
    Comment = #comment{id=CommentId, thread=ThreadId, user=User, tripcode=Tripcode, body=Body, file=File},
    Thread = #thread{id=ThreadId, board=BoardName, last_update=ThreadId, comment_count=1, 
		     first_comment={CommentId, User, Tripcode, Body, File}},
    {atomic, ok} = mnesia:transation(fun () -> mnesia:write(Thread), mnesia:write(Comment) end),
    {reply, {ThreadId, CommentId} , BoardName};
handle_call({reply, Thread, User, Tripcode, Body, File}, _From, BoardName) -> 
    CommentId = now(),
    Comment = #comment{id=CommentId, thread=Thread, user=User, tripcode=Tripcode, body=Body, file=File},
    Thread = do(qlc:q([X || X <- mnesia:table(thread), X#thread.id =:= Thread])),
    LastComm = last_n(Thread#thread.last_comments, {CommentId, User, Tripcode, Body, File}, 4),
    Updated = Thread#thread{last_update=CommentId, 
			    comment_count=Thread#thread.comment_count + 1,
			    last_comments=LastComm},
    {atomic, ok} = mnesia:transaction(fun () -> mnesia:write(Comment), mnesia:write(Updated) end),
    {reply, CommentId, BoardName}.

%% Given a list, a NewElem and an N,
%% returns the last N elements of List ++ [NewElem]
last_n(List, NewElem, N) ->
    Res = lists:sublist([NewElem | lists:reverse(List)], N),
    lists:reverse(Res).
	    

%%%%%%%%%%%%%%%%%%%% DB-related
do(Q) -> 
    {atomic, Val} = mnesia:transaction(fun() -> qlc:e(Q) end),
    Val.

create() -> 
    mnesia:create_table(board, [{type, ordered_set}, {disc_copies, [node()]}, {attributes, record_info(fields, board)}]),
    Rec = #board{name=board, created=now()},
    {atomic, ok} = mnesia:transaction(fun () -> mnesia:write(Rec) end),
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
