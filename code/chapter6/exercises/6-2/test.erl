
-module(test).
-export([test/0]).

-spec test() -> none().
test() ->
  mutex:start(),
  mutex:get_state(),
  mutex:wait(),
  % mutex:get_state(),
  exit(kill).


