-module(board).
-behaviour(gen_server).
-include_lib("stdlib/include/qlc.hrl").

-export([start/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([create/0]).

-record(board, {name, description, created, max_threads=300, max_thread_size=500, 
		default_name="Anonymous", deleted_image=""}).
-record(thread, {id, board, status=active, last_update, first_comment, last_comments=[], comment_count}).
-record(comment, {id, thread, user, tripcode, body, file}).

-export([list/0, new/1, new/2, delete/2, summarize/1, default_name/1, new_thread/2, get_thread/2, reply/3]).

-define(AUTH_NODE, 'erl_chan@127.0.1.1').

%%%%%%%%%%%%%%%%%%%% external API
list() -> db:do(qlc:q([{X#board.name, X#board.description} || X <- mnesia:table(board)])).

exists_p(BoardName) -> 
    case db:do(qlc:q([X#board.name || X <- mnesia:table(board), X#board.name =:= BoardName])) of
	[] -> false;
	_ -> true
    end.     

new(BoardName) when is_atom(BoardName) -> new(BoardName, "").
new(BoardName, Description) when is_atom(BoardName) ->
    case exists_p(BoardName) of
	true -> already_exists;
	false -> try
		     %% in a try block because the auth node may not exist. Intentionally.
		     rpc:call(?AUTH_NODE, groups, add_special, [BoardName, atom_to_list(BoardName) ++ " Admins"])
		 catch
		     error:_ -> false
		 end,
		 db:atomic_insert(#board{name=BoardName, description=Description, created=now()}),
		 supervisor:start_child(erl_chan_sup, erl_chan_sup:child_spec(BoardName))
    end.

delete(Board, {image, CommentId}) -> 
    gen_server:call(Board, {delete_comment_image, CommentId});
delete(Board, Id) -> 
    case find_thread(Id) of
	false -> gen_server:call(Board, {delete_comment, Id});
	_ -> gen_server:call(Board, {delete_thread, Id})
    end.

summarize({Board, ThreadId}) -> 
    gen_server:call(Board, {summarize, ThreadId});
summarize(Board) -> 
    gen_server:call(Board, summarize).

default_name(Board) -> gen_server:call(Board, default_name).
get_thread(Board, Thread) -> gen_server:call(Board, {get_thread, Thread}).
new_thread(Board, {User, Tripcode, Comment, File}) -> 
    gen_server:call(Board, {new_thread, User, Tripcode, Comment, File}).
reply(Board, Thread, {User, Tripcode, Body, File}) -> 
    gen_server:call(Board, {reply, Thread, User, Tripcode, Body, File}).

%%%%%%%%%%%%%%%%%%%% internal call handling
%%%%%%%%%% read operations
handle_call(summarize, _From, BoardName) -> 
    Res = db:do(qlc:q([to_tup(X) || X <- mnesia:table(thread), X#thread.board =:= BoardName])),
    Threads = case Res of
		  [] -> [];
		  [Thread] -> [Thread];
		  _ -> lists:sort(fun sort_threads/2, Res)
	      end,
    {reply, Threads, BoardName};
handle_call({summarize, ThreadId}, _From, BoardName) -> 
    [Res] = db:do(qlc:q([to_tup(X) || X <- mnesia:table(thread), X#thread.board =:= BoardName, X#thread.id =:= ThreadId])),
    {reply, Res, BoardName};
handle_call({get_thread, Thread}, _From, BoardName) -> 
    Res = db:do(qlc:q([to_tup(X) || X <- mnesia:table(comment), X#comment.thread =:= Thread])),
    {reply, Res, BoardName};
handle_call(default_name, _From, BoardName) -> 
    [Res] = db:do(qlc:q([X#board.default_name || X <- mnesia:table(board), X#board.name =:= BoardName])),
    {reply, Res, BoardName};

%%%%%%%%%% delete operations
handle_call({delete_thread, ThreadId}, _From, BoardName) ->
    [First | Rest] = db:do(qlc:q([X || X <- mnesia:table(comment), X#comment.thread =:= ThreadId])),
    Thread = find_thread(ThreadId),
    Comm = deleted_comment(First),
    New = Thread#thread{status=deleted, first_comment=to_tup(Comm), last_comments=[], comment_count=1},
    db:transaction(fun() ->
			   lists:map(fun mnesia:delete_object/1, Rest),
			   mnesia:write(Comm),
			   mnesia:write(New)
		   end),
    {reply, collect_files([First | Rest]), BoardName};
handle_call({delete_comment, CommentId}, _From, BoardName) ->
    Comment = find_comm(CommentId),
    Thread = find_thread(Comment#comment.thread),
    New = deleted_comment(Comment),
    db:atomic_insert([replace_comment_cache(Thread, CommentId, to_tup(New)), New]),
    {reply, Comment#comment.file, BoardName};
handle_call({delete_comment_image, CommentId}, _From, BoardName) ->
    Comment = find_comm(CommentId),
    Thread = find_thread(Comment#comment.thread),
    OldFile = Comment#comment.file,
    New = Comment#comment{file=deleted},
    db:atomic_insert([replace_comment_cache(Thread, CommentId, to_tup(New)), New]),
    {reply, OldFile, BoardName};

%%%%%%%%%% non-delete write operations
handle_call({new_thread, User, Tripcode, Body, File}, _From, BoardName) -> 
    Id = now(),
    Trip = triphash(Tripcode),
    Comment = #comment{id=Id, thread=Id, user=User, tripcode=Trip, body=Body, file=File},
    Thread = #thread{id=Id, board=BoardName, last_update=Id, comment_count=1, 
		     first_comment={Id, User, Trip, Body, File}},
    {atomic, ok} = mnesia:transaction(fun () -> mnesia:write(Thread), mnesia:write(Comment) end),
    {reply, to_tup(Thread), BoardName};
handle_call({reply, ThreadId, User, Tripcode, Body, File}, _From, BoardName) -> 
    Rec = find_thread(ThreadId),
    active = Rec#thread.status,
    Id = now(),
    Trip = triphash(Tripcode),
    Comment = #comment{id=Id, thread=ThreadId, user=User, tripcode=Trip, body=Body, file=File},
    LastComm = last_n(Rec#thread.last_comments, {Id, User, Trip, Body, File}, 4),
    Updated = Rec#thread{last_update=Id,
			 comment_count=Rec#thread.comment_count + 1,
			 last_comments=LastComm},
    db:atomic_insert([Comment, Updated]),
    {reply, to_tup(Comment), BoardName}.

%%%%%%%%%%%%%%%%%%%% local utility
triphash(Tripcode) ->
    try
	erlsha2:sha256(Tripcode)
    catch
	error:_ -> Tripcode
    end.
	

deleted_comment(Comment) ->
    NewFile = case Comment#comment.file of
		  undefined -> undefined;
		  _ -> deleted
	      end,
    Comment#comment{user="DELETED", body=["DISREGARD THAT, I SUCK COCKS."], file=NewFile}.

last_n(List, NewElem, N) ->
    Res = lists:sublist([NewElem | lists:reverse(List)], N),
    lists:reverse(Res).

replace_comment_cache(Thread, CommentId, NewComment) ->
    [First | Rest] = lists:keyreplace(CommentId, 1, 
				      [Thread#thread.first_comment | Thread#thread.last_comments], 
				      NewComment),
    Thread#thread{first_comment=First, last_comments=Rest}.

sort_threads({_, _, A, _, _}, {_, _, B, _, _}) ->
    common:now_to_seconds(A) > common:now_to_seconds(B).

collect_files(Comments) -> collect_files(Comments, []).
collect_files([], Acc) -> Acc;
collect_files([Comment | Rest], Acc) ->
    case Comment#comment.file of
	deleted -> collect_files(Rest, Acc);
	undefined -> collect_files(Rest, Acc);
	Pic -> collect_files(Rest, [Pic | Acc])
    end.

to_tup(Rec) when is_record(Rec, thread) ->
    {Rec#thread.id, Rec#thread.status, Rec#thread.last_update, Rec#thread.comment_count,
     [Rec#thread.first_comment | Rec#thread.last_comments]};
to_tup(Rec) when is_record(Rec, comment) ->
    {Rec#comment.id, Rec#comment.user, Rec#comment.tripcode, 
     Rec#comment.body, Rec#comment.file}.

%%%%%%%%%%%%%%%%%%%% DB-related
create() -> 
    mnesia:create_table(board, [{type, ordered_set}, {disc_copies, [node()]}, {attributes, record_info(fields, board)}]),
    mnesia:create_table(thread, [{type, ordered_set}, {disc_copies, [node()]}, {attributes, record_info(fields, thread)}]),
    mnesia:create_table(comment, [{type, ordered_set}, {disc_copies, [node()]}, {attributes, record_info(fields, comment)}]).

find_thread(Id) -> db:find(thread, #thread.id, Id).
find_comm(Id) -> db:find(comment, #comment.id, Id).

%%%%%%%%%%%%%%%%%%%% generic actions
start(BoardName) -> gen_server:start_link({local, BoardName}, ?MODULE, BoardName, []).
stop(BoardName) -> gen_server:call(BoardName, stop).

%%%%%%%%%%%%%%%%%%%% gen_server handlers
init(BoardName) -> {ok, BoardName}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
