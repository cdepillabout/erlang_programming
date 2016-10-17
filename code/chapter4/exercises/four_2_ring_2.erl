% This is the strategy where each child spawns its own child process.

-module(four_2_ring_2).
-export([start/3, start_child/1 ]).


start(_M_Messages, N_processes, _Message) when N_processes < 2 -> {error, not_enough_processes};
start(M_Messages, N_processes, Message) ->
  All_Procs = spawn_all(N_processes),
  [First_Proc|Other_Procs] = All_Procs,
  lists:zipwith(fun (Parent_Proc, Child_Proc) -> Parent_Proc ! {pid, Child_Proc} end
                , All_Procs
                , Other_Procs ++ [nothing]),
  send_messages(M_Messages, First_Proc, {message, Message}),
  send_messages(1, First_Proc, stop).

send_messages(0, _, _) -> ok;
send_messages(M_Messages, First_Proc, Message) ->
  First_Proc ! Message,
  send_messages(M_Messages - 1, First_Proc, Message).

spawn_all(N_processes) ->
  All_Ns = lists:seq(1, N_processes),
  lists:map(fun (N) -> spawn(?MODULE, start_child, [N]) end, All_Ns).

start_child(N) ->
  receive
    {pid, Grandchild_Pid} ->
      io:format("child process (~w) (pid: ~w) received grandchild's pid: ~w~n", [N, self(), Grandchild_Pid]),
      child_loop(N, Grandchild_Pid);
    stop ->
      io:format("child process (~w) (pid: ~w) received stop before starting child_loop~n", [N, self()]),
      ok
  end.

child_loop(N, Grandchild_Pid) ->
  receive
    {message, Msg} ->
      io:format("child process (~w) (pid: ~w) received message: ~w, grandchild pid is: ~w~n", [N, self(), Msg, Grandchild_Pid]),
      case Grandchild_Pid of
        nothing -> ok;
        Grandchild_Pid -> Grandchild_Pid ! {message, Msg}
      end,
      child_loop(N, Grandchild_Pid);
    stop ->
      io:format("child process (~w) (pid: ~w) received stop~n", [N, self()]),
      case Grandchild_Pid of
        nothing -> ok;
        Grandchild_Pid -> Grandchild_Pid ! stop
      end,
      ok
  end.
