
-module(three_four).
-export([new/0, destroy/1, write/3, delete/2, read/2, match/2]).

new() -> [].

destroy(_Db) -> ok.

write(Key, Element, Db) ->
  Update = update(Key, Element, Db),
  case Update of
    {error, no_key} -> [{Key, Element}|Db];
    New_Db -> New_Db
  end.

update(_, _, []) -> {error, no_key};
update(Key, Element, [{Key, _}|Rest]) -> [{Key, Element}|Rest];
update(Key, Element, [Other|Rest]) ->
  case update(Key, Element, Rest) of
    {error, no_key} -> {error, no_key};
    Db -> [Other|Db]
  end.

delete(_, []) -> [];
delete(Key, [{Key, _}|T]) -> T;
delete(Key, [Other|T]) -> [Other|delete(Key, T)].

read(_, []) -> {error, instance};
read(Key, [{Key, Val}|_]) -> {ok, Val};
read(Key, [_|T]) -> read(Key, T).

match(_, []) -> [];
match(Element, [{Key, Element}|T]) -> [Key|match(Element, T)];
match(Element, [_|T]) -> match(Element, T).
