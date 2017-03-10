%% Code from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(dist).
-export([t/1]).

t(From) ->
  io:format("This message is being printed from pid (~w) on node (~w).~n",
            [self(), node()]),
  io:format("Sending message to: ~w~n", [From]),
  From ! {hello, node()}.
