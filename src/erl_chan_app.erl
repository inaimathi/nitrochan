-module(erl_chan_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, StartArgs) -> 
    mnesia:start(),
    erl_chan_sup:start_link(StartArgs).

stop(_State) -> ok.
