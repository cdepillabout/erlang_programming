
-module(three_seven).
-export([new/0, destroy/1, write/3, delete/2, read/2, match/2]).

new() -> [].

destroy(_Db) -> ok.

write(Key, Element, Db) -> lists:usort([{Key, Element}|Db]).

delete(Key, Db) -> lists:keydelete(Key, 1, Db).

read(Key, Db) ->
  case lists:keyfind(Key, 1, Db) of
    false -> {error, instance};
    Tuple -> Tuple
  end.

match(Element, Db) -> lists:filter(fun ({_, Elem}) -> Element == Elem end, Db).
