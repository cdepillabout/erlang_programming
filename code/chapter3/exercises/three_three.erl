
-module(three_three).
-export([print_out_ints/1, print_out_even_ints/1]).

print_int(N) -> io:format("Number: ~p~n", [N]).

print_out_ints(1) -> print_int(1);
print_out_ints(N) when N > 1 -> print_int(N), print_out_ints(N - 1).


print_out_even_ints(1) -> ok;
print_out_even_ints(N) when ((N > 1) and ((N rem 2) == 0)) ->
  print_int(N), print_out_even_ints(N-1);
print_out_even_ints(N) when N > 1 -> print_out_even_ints(N-1).
