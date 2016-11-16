%% Code from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http:-module(log_handler).

-module(log_handler).

-export([init/1, terminate/1, handle_event/2]).

init({File, nothing}) ->
  {ok, Fd} = file:open(File, write),
  {File, Fd}.

terminate({File, Fd}) -> file:close(Fd), {File, nothing}.

handle_event({Action, Id, Event}, {File, Fd}) ->
  {MegaSec, Sec, MicroSec} = now(),
  io:format(Fd, "~w,~w,~w,~w,~w,~p~n",
            [MegaSec, Sec, MicroSec, Action, Id, Event]),
  {File, Fd};
handle_event(_, Data) -> Data.
