
-module(echo_server).
-export([start/0, print/1, stop/0, loop/0]).

-spec start() -> 'ok'.
start() ->
  register(?MODULE, spawn_link(?MODULE, loop, [])),
  ok.

-spec loop() -> 'ok'.
loop() ->
  receive
    {print, Msg} ->
      io:format("received message: ~w~n", [Msg]),
      loop();
    stop ->
      ok
  end.

-spec print(atom()) -> ok.
print(Msg) ->
  ?MODULE ! {print, Msg},
  ok.

-spec stop() -> none().
stop() ->
  exit(yosomereason).
