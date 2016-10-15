-module(three_two).
-export([create/1, reverse_create/1]).

create(N) -> create_helper(1,N).

create_helper(A,N) when A == N -> [N];
create_helper(A,N) when A < N -> [A|create_helper(A+1, N)].

reverse_create(1) -> [1];
reverse_create(N) when N > 1 -> [N|reverse_create(N-1)].
