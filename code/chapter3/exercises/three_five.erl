
-module(three_five).
-export([filter/2, reverse/1, concat/1, flatten/1]).

filter([], _) -> [];
filter([H|T], F) ->
  case F(H) of
    true -> [H|filter(T, F)];
    false -> filter(T, F)
  end.

reverse ([]) -> [];
reverse ([H|T]) -> reverse(T) ++ [H].

concat([]) -> [];
concat([HS|TS]) -> HS ++ concat(TS).

flatten([]) -> [];
flatten([H|T]) -> flatten(H) ++ flatten(T);
flatten(Other) -> [Other].
