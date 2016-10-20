
-module(test_five_3_event_handler).
-export([test/0]).

test() ->
  five_3_event_handler:start(),
  five_3_event_handler:add_handler(log_handler, {"somefile", nothing}),
  five_3_event_handler:send_event({someaction, 1, someevent}),
  five_3_event_handler:send_event({someaction, 2, someevent2}),
  five_3_event_handler:swap_handlers(log_handler, log_handler2),
  five_3_event_handler:send_event({someaction, 2, someevent2}),
  five_3_event_handler:stop().
