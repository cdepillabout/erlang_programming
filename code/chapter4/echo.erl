%% Code from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(echo).
-export([go/0, loop/0]).

go() ->
  Pid = spawn(echo, loop, []),
  Pid ! {self(), hello},
  receive
    {Pid, Msg} ->
      io:format("received in main process (~w): ~w~n",[self(), Msg])
  end,
  Pid ! stop.


loop() ->
  receive
    {From, Msg} ->
      io:format("received in loop (~w) from process ~w: ~w~n",[self(), From, Msg]),
      From ! {self(), Msg},
      loop();
    stop ->
      io:format("received in loop (~w): stop~n",[self()]),
      true
  end.
