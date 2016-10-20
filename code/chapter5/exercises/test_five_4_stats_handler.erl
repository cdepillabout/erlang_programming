
-module(test_five_4_stats_handler).
-export([test/0]).

test() ->
  five_3_event_handler:start(),
  five_3_event_handler:add_handler(five_4_stats_handler, []),
  five_3_event_handler:send_event({someaction, 1, someevent}),
  five_3_event_handler:send_event({someaction, 1, someevent2}),
  Data1 = five_3_event_handler:get_data(five_4_stats_handler),
  io:format("~w~n", [Data1]),
  five_3_event_handler:send_event({someaction, 1, someevent3}),
  five_3_event_handler:send_event({someaction, 2, someevent3}),
  five_3_event_handler:send_event({otheraction, 1, someevent3}),
  Data2 = five_3_event_handler:get_data(five_4_stats_handler),
  io:format("~w~n", [Data2]),
  five_3_event_handler:stop().
