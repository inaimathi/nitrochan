-module(erl_chan_sup).
-behavior(supervisor).

-export([start/0, start_link/1, init/1, child_spec/1]).

start() ->
    spawn(fun() -> supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = []) end).

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init([]) ->
    Boards = try
		 lists:map(
		   fun (B) -> 
			   child_spec(proplists:get_value(name, B)) 
		   end, board:list())
	     catch
		 error:_ -> []
	     end,
    {ok, {{one_for_one, 3, 10}, 
	  [{mod_log, {mod_log, start, []}, permanent, 5000, worker, [mod_log]}
	    | Boards]}}.

child_spec(BoardName) -> 
    {BoardName, {board, start, [BoardName]}, permanent, 5000, worker, [board]}.
