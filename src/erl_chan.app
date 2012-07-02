{application, erl_chan,
 [{description, "An implementation of a 4-chan style message board in Erlang/OTP"},
  {vsn, "1.0"},
  {modules, [erl_chan_app, erl_chan_sup, board]},
  {registered, [board]},
  {applications, [kernel, stdlib, sasl]},
  {mod, {erl_chan_app, []}},
  {start_phases, []}]}.