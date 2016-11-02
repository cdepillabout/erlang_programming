
-module(test).
-export([test/0]).

test() ->
  my_supervisor:start_link([{db_server, start, [], transient}]),
  timer:sleep(500),
  db_server:write(hello, foo),
  Res = db_server:read(hello),
  io:format("result: ~w~n", [Res]).
