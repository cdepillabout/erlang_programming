%% Code from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(modtest2).

-export([main/0,loop/0,a/1,do/1]).
-vsn(1.5).

main() ->
  register(foo,spawn(?MODULE,loop,[])).

loop() ->
  receive
    {Sender, N} ->
       Sender ! a(N)
  end,
  io:format("hello~n"),
  modtest2:loop().

do(M) ->
  foo ! {self(),M},
  receive Y ->
    Y
  end.

a(N) -> N.
