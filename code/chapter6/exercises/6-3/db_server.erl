
-module(db_server).
-export([start/0, stop/0]).
-export([write/2, delete/1, read/1, match/1]).
-export([init/0]).

-type key() :: any().
-type elem() :: any().

-type msg() :: {'write', key(), elem()}
             | {'delete', key()}
             | {'read', key()}
             | {'match', elem()} .

% General db_server start/stop definitions

start() ->
  io:format("db_server start~n"),
  Pid = spawn_link(?MODULE, init,[]),
  io:format("db_server start, pid is: ~w~n", [Pid]),
  register(?MODULE, Pid),
  {ok, Pid}.

stop() ->
  ?MODULE ! {stop, self()},
  receive {reply, Reply} -> Reply end.

% General DB operations

write(Key, Element) ->
  io:format("db_server write: key = ~w, elem = ~w~n", [Key, Element]),
  call({write, Key, Element}).

delete(Key) ->
  call({delete, Key}).

read(Key) ->
  call({read, Key}).

match(Element) ->
  call({match, Element}).

-spec call(msg()) -> any().
call(Msg) ->
  io:format("db_server call: msg = ~w~n", [Msg]),
  ?MODULE ! {request, self(), Msg},
  receive {reply, Reply} -> Reply end.

% Server loop

init() ->
  io:format("db_server init~n"),
  loop(db:new()).

loop(Db_State) ->
  receive
    {request, From, Msg} ->
      io:format("db_server received request from ~w: ~w~n", [From, Msg]),
      {Reply,NewState} = handle_msg(Msg, Db_State),
      reply(From, Reply),
      loop(NewState);
    {stop, From}  ->
      reply(From, terminate(Db_State))
  end.

% Helpers

handle_msg({write, Key, Element}, Db_State) ->
  case db:write(Key, Element, Db_State) of
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

