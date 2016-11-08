
-module(db).
-export([new/0, destroy/1, write/3, delete/2, read/2, match/2]).

-type key() :: atom().
-type elem() :: any().

-record(db_entry, {key :: key(),
                   elem :: elem()}).

-type db_entry() :: #db_entry{}.
-type db() :: [db_entry()].

-spec new() -> db().
new() -> [].

-spec destroy(db()) -> ok.
destroy(_Db) -> ok.

-spec write(key(), elem(), db()) -> db().
write(Key, Elem, Db) ->
  Update = update(Key, Elem, Db),
  case Update of
    {error, no_key} -> [#db_entry{key=Key, elem=Elem}|Db];
    New_Db -> New_Db
  end.

-spec update(key(), elem(), db()) -> db() | {error, no_key}.
update(_, _, []) -> {error, no_key};
update(Key, Elem, [#db_entry{key=Key}|Rest]) ->
  [#db_entry{key=Key, elem=Elem}|Rest];
update(Key, Elem, [Other|Rest]) ->
  case update(Key, Elem, Rest) of
    {error, no_key} -> {error, no_key};
    Db -> [Other|Db]
  end.

-spec delete(key(), db()) -> db().
delete(_, []) -> [];
delete(Key, [#db_entry{key=Key}|T]) -> T;
delete(Key, [Other|T]) -> [Other|delete(Key, T)].

-spec read(key(), db()) -> {ok, elem()} | {error, instance}.
read(_, []) -> {error, instance};
read(Key, [#db_entry{key=Key, elem=Elem}|_]) -> {ok, Elem};
read(Key, [_|T]) -> read(Key, T).

-spec match(elem(), db()) -> [key()].
match(_, []) -> [];
match(Elem, [#db_entry{key=Key, elem=Elem}|T]) -> [Key|match(Elem, T)];
match(Elem, [_|T]) -> match(Elem, T).
