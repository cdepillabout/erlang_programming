-module(three_one).
-export([sum/2, sum/1]).

sum(M) -> sum(1, M).

sum(N,M) when N == M -> N;
sum(N,M) when N < M -> N + sum(N + 1, M).
