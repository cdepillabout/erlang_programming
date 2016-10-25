
-module(test2).
-export([test/0]).

-spec test() -> none().
test() ->
  mutex2:start(),
  mutex2:get_state(),
  mutex2:wait(),
  mutex2:get_state(),
  exit(kill).


