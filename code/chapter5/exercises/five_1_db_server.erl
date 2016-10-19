
-module(five_1_db_server).
-export([start/0, stop/0]).
-export([write/2, delete/1, read/1, match/1]).
-export([init/0]).

% General db_server start/stop definitions

start() ->
  Pid = spawn(?MODULE, init,[]),
  register(db_server, Pid),
  ok.

stop() ->
  db_server ! {stop, self()},
  receive {reply, Reply} -> Reply end.

% General DB operations

write(Key, Element) ->
  call({write, Key, Element}).

delete(Key) ->
  call({delete, Key}).

read(Key) ->
  call({read, Key}).

match(Element) ->
  call({match, Element}).

call(Msg) ->
  db_server ! {request, self(), Msg},
  receive {reply, Reply} -> Reply end.

% Server loop

init() ->
  loop(db:new()).

loop(Db_State) ->
  receive
    {request, From, Msg} ->
      {Reply,NewState} = handle_msg(Msg, Db_State),
      reply(From, Reply),
      loop(NewState);
    {stop, From}  ->
      reply(From, terminate(Db_State))
  end.

% Helpers

handle_msg({write, Key, Element}, Db_State) ->
  case db:write(Key, Element, Db_State) of
    {error, Reason} -> {{error, Reason}, Db_State};
    New_Db -> {ok, New_Db}
  end;
handle_msg({delete, Key}, Db_State) ->
  {ok, db:delete(Key, Db_State)};
handle_msg({read, Key}, Db_State) ->
  {db:read(Key, Db_State), Db_State};
handle_msg({match, Element}, Db_State) ->
  {db:match(Element, Db_State), Db_State}.

terminate(_) -> ok.

reply(To, Msg) ->
  To ! {reply, Msg}.
