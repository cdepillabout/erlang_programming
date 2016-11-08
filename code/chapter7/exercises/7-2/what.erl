-module(what).

-export ([foobar/1]).

-record(person, {name :: string(),
                 age=0 :: non_neg_integer(),
                 phone :: string(),
                 address="" :: string()}).

-type person() :: #person{}.

-spec foobar(person()) -> atom().
foobar(P) when is_record(P, person) and (P#person.name == "Joe") ->
  lalala;
foobar(P) when is_record(P, person) ->
  io:format("P: ~w~n", [P]),
  io:format("P#person.name: ~w~n", [P#person.name]),
  io:format("P#person.name == \"Joe\": ~w~n", [P#person.name == "Joe"]),
  jahaja;
foobar(_) ->
  nonono.
