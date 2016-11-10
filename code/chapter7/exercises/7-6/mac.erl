
-module(mac).
-export([test/0]).


-ifdef(show).
  do_show(Code, Val) ->
    io:format("~p = ~p~n", [Code, Val]),
    Val.

  -define(SHOW_EVAL(Expr), do_show(??Expr, Expr)).
-else.
  -define(SHOW_EVAL(Expr), Expr).
-endif.

test() ->
  ?SHOW_EVAL(io:format("whatwhat~n")).
