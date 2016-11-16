
-module(comp).
-export([all_ints_squared/1, div_by_3/0, intersect/2]).

div_by_3() ->
  [ X || X <- lists:seq(1, 10), X rem 3 == 0].

all_ints_squared(Xs) ->
  [ X * X || X <- Xs, is_integer(X) ].

intersect(Xs, Ys) ->
  [ X || X <- Xs, Y <- Ys, X == Y ].
