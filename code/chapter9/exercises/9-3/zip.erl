
-module(zip).
-export([zip/2, zipWith/3]).

zip(Xs, Ys) ->
  zipWith(fun (X, Y) -> {X, Y} end, Xs, Ys).

zipWith(_Fun, [], _Ys) ->
  [];
zipWith(_Fun, _Xs, []) ->
  [];
zipWith(Fun, [X|Xs], [Y|Ys]) ->
  [ Fun(X, Y) | zipWith(Fun, Xs, Ys) ].

