% This is the strategy where each child spawns its own child process.

-module(four_2_ring).
-export([start/3, child/2 ]).


start(M_messages, N_processes, Message) ->
  Child_Pid = spawn(?MODULE, child, [N_processes - 1, self()]),
  send_messages(Child_Pid, M_messages, Message),
  Child_Pid ! stop,
  parent_listen().

send_messages(_, 0, _) -> ok;
send_messages(Pid, M_Messages, Message) ->
  Pid ! {message, Message},
  send_messages(Pid, M_Messages - 1, Message).

parent_listen() ->
  receive
    {message, Msg} ->
      io:format("parent process (pid: ~w) received msg: ~w~n", [self(), Msg]),
      parent_listen();
    stop ->
      io:format("parent process (pid: ~w) received stop~n", [self()])
  end.

child(0, Original_Process_Pid) ->
  child_listen(0, Original_Process_Pid);
child(Process_Num, Original_Process_Pid) ->
  Child_Pid = spawn(?MODULE, child, [Process_Num - 1, Original_Process_Pid]),
  child_listen(Process_Num, Child_Pid).

child_listen(0, Pid) ->
  receive
    {message, Msg} ->
      io:format("last child process (0) (pid: ~w) received msg: ~w~n", [self(), Msg]),
      Pid ! {message, Msg},
      child_listen(0, Pid);
    stop ->
      io:format("last child process (0) (pid: ~w) received stop~n", [self()]),
      Pid ! stop
  end;
child_listen(Process_Num, Pid) ->
  receive
    {message, Msg} ->
      io:format("child process (~w) (pid: ~w) received msg: ~w~n", [Process_Num, self(), Msg]),
      Pid ! {message, Msg},
      child_listen(Process_Num, Pid);
    stop ->
      io:format("child process (~w) (pid: ~w) received stop~n", [Process_Num, self()]),
      Pid ! stop
    end.
