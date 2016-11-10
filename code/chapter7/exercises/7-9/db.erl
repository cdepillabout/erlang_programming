
-module(db).
-export([new/0, destroy/1, write/3, delete/2, read/2, match/2]).
-export([test/0]).

-type key() :: atom().
-type elem() :: any().

-record(db_entry, {key :: key(),
                   elem :: elem()}).

-type db_entry() :: #db_entry{}.
-type db() :: [db_entry()].

-ifdef(debug).
  -define(DBG_DB(DB), io:format("db: ~p~n", [DB])).
-else.
  -define(DBG_DB(DB), ok).
-endif.

-spec new() -> db().
new() -> [].

-spec destroy(db()) -> ok.
destroy(_Db) -> ok.

-spec write(key(), elem(), db()) -> db().
write(Key, Elem, Db) ->
  ?DBG_DB(Db),
  Update = update(Key, Elem, Db),
  case Update of
    {error, no_key} -> [#db_entry{key=Key, elem=Elem}|Db];
    New_Db -> New_Db
  end.

-spec update(key(), elem(), db()) -> db() | {error, no_key}.
update(_, _, []=Db) ->
  ?DBG_DB(Db),
  {error, no_key};
update(Key, Elem, [#db_entry{key=Key}|Rest] = Db) ->
  ?DBG_DB(Db),
  [#db_entry{key=Key, elem=Elem}|Rest];
update(Key, Elem, [Other|Rest] = Db) ->
  ?DBG_DB(Db),
  case update(Key, Elem, Rest) of
    {error, no_key} -> {error, no_key};
    Db -> [Other|Db]
  end.

-spec delete(key(), db()) -> db().
delete(_, []) -> [];
delete(Key, [#db_entry{key=Key}|T] = Db) ->
  ?DBG_DB(Db),
  T;
delete(Key, [Other|T] = Db) ->
  ?DBG_DB(Db),
  [Other|delete(Key, T)].

-spec read(key(), db()) -> {ok, elem()} | {error, instance}.
read(_, [] = Db) ->
  ?DBG_DB(Db),
  {error, instance};
read(Key, [#db_entry{key=Key, elem=Elem}|_] = Db) ->
  ?DBG_DB(Db),
  {ok, Elem};
read(Key, [_|T] = Db) ->
  ?DBG_DB(Db),
  read(Key, T).

-spec match(elem(), db()) -> [key()].
match(_, [] = Db) ->
  ?DBG_DB(Db),
  [];
match(Elem, [#db_entry{key=Key, elem=Elem}|T] = Db) ->
  ?DBG_DB(Db),
  [Key|match(Elem, T)];
match(Elem, [_|T] = Db) ->
  ?DBG_DB(Db),
  match(Elem, T).

test() ->
  Db1 = new(),
  read(hello, Db1),
  Db2 = write(hello, valval, Db1),
  read(hello, Db2).
