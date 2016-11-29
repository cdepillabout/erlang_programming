
-module(test).
-export([start/0, run/1]).
-export([receiver/0, sender/3]).

-spec run(integer()) -> ok.
run(N) ->
  logging:create_tables(),
  ReceiverPid = spawn(?MODULE, receiver, []),
  spawn(?MODULE, sender, [self(), ReceiverPid, N]),
  receive
    done -> ok
  end,
  % io:format("send: ~w~n", [ets:tab2list(send)]),
  % io:format("recv: ~w~n", [ets:tab2list(recv)]),
  % io:format("send_recv_index: ~w~n", [ets:tab2list(send_recv_index)]),
  io:format("received_all: ~w~n", [logging:received_all()]),
  ok.

receiver() ->
  logging:recv_mesg(true),
  receiver().

-spec sender(pid(), pid(), integer()) -> ok.
sender(MainPid, _, 0) ->
  MainPid ! done;
sender(MainPid, ReceiverPid, N) ->
  logging:send_mesg(ReceiverPid, hello),
  sender(MainPid, ReceiverPid, N - 1).

start() -> run(10000).
