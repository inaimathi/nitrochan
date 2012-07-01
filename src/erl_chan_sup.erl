-module(erl_chan_sup).
-behavior(supervisor).

-export([start/0, start_for_testing/0, start_link/1, init/1]).

start() ->
    spawn(fun() -> supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = []) end).

start_for_testing() ->
    {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = []),
    unlink(Pid).

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init([]) ->
    mnesia:start(),
    {ok, {{one_for_one, 3, 10},
	  [{board, {board, start, [board]}, permanent, 5000, worker, [board]}]}}.
