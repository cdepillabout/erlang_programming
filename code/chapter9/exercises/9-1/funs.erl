
-module(funs).
-export([concat/1, print_1_to_N/1, print_even_1_to_N/1, filter_smaller_integer/2, sum/1]).

print_all(Ns) ->
  lists:foreach(fun(X) -> io:format("~w~n", [X]) end, Ns).

print_1_to_N(N) ->
  print_all(lists:seq(1, N)).

filter_smaller_integer(N, Ns) ->
  lists:filter(fun(X) -> X =< N end, Ns).

print_even_1_to_N(N) ->
  print_all(lists:filter(fun(X) -> X rem 2 == 0 end, lists:seq(1, N))).

concat(Ls) ->
  lists:foldl(fun(Elem, Acc) -> Acc ++ Elem end, [], Ls).

sum(Ns) ->
  lists:foldl(fun(Elem, Acc) -> Acc + Elem end, 0, Ns).
