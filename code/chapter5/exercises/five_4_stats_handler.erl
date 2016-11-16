%% Code from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http:-module(log_handler).

-module(five_4_stats_handler).

-export([init/0, init/1, terminate/1, handle_event/2]).

init() -> [].

init(Stats) -> Stats.

terminate(Stats) -> Stats.

handle_event({Type, Id, _Description}, Stats) ->
  case lists:keytake({Type, Id}, 1, Stats) of
    {value, {{Type, Id}, Num}, NewStats} ->
      [{{Type, Id}, Num + 1}|NewStats];
    false ->
      [{{Type, Id}, 1}|Stats]
  end ;
handle_event(_, Stats) -> Stats.
