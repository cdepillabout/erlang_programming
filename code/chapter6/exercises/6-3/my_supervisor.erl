%% Code from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(my_supervisor).
-export([start_link/1, stop/0]).
-export([init/1]).
-export_type([child_spec/1]).

-type restart_type() :: 'permanent' | 'transient'.

-type child_spec(InitializationData) ::
  {module(), atom(), InitializationData, restart_type()}.

-type child_spec_pid(InitializationData) :: {pid(), child_spec(InitializationData)}.

-spec start_link([child_spec(any())]) -> 'ok'.
start_link(ChildSpecList) ->
  register(?MODULE, spawn_link(my_supervisor, init, [ChildSpecList])), ok.

-spec init([child_spec(any())]) -> 'ok'.
init(ChildSpecList) ->
  process_flag(trap_exit, true),
  loop(start_children(ChildSpecList)).

-spec start_children([child_spec(any())]) -> [child_spec_pid(any())].
start_children([]) -> [];
start_children([{Module, Function, Arguments, RestartType} | ChildSpecList]) ->
  case (catch apply(Module,Function,Arguments)) of
    {ok, Pid} ->
      [{Pid, {Module, Function, Arguments, RestartType}}|start_children(ChildSpecList)];
    _ ->
      start_children(ChildSpecList)
  end.

%% The loop of the supervisor waits in a receive clause for EXIT and stop
%% messages.  If a child terminates, the supervisor receives the EXIT signal
%% and restarts the terminated child, replacing its entry in the list of
%% children stored in the ChildList variable:

-spec restart_child(pid(), 'normal' | any(), [child_spec_pid(any())]) -> [child_spec_pid(any())].
restart_child(Pid, Reason, ChildList) ->
  io:format("in my_supervisor:restart_child, Pid is: ~w~n", [Pid]),
  io:format("in my_supervisor:restart_child, Reason is: ~w~n", [Reason]),
  io:format("in my_supervisor:restart_child, ChildList is: ~w~n", [ChildList]),
  {value, {Pid, {Module, Function, Arguments, RestartType}}} =
    lists:keysearch(Pid, 1, ChildList),
  case {RestartType, Reason} of
    % Don't restart.
    {transient, normal} ->
      lists:keydelete(Pid,1,ChildList);
    _ ->
      {ok, NewPid} = apply(Module, Function, Arguments),
      [ {NewPid, {Module, Function, Arguments, RestartType}} |
        lists:keydelete(Pid,1,ChildList) ]
  end.

-spec loop([child_spec_pid(any())]) -> 'ok'.
loop(ChildList) ->
  io:format("in my_supervisor:loop, ChildList is: ~w~n", [ChildList]),
  receive
    {'EXIT', Pid, Reason} ->
      NewChildList = restart_child(Pid, Reason, ChildList),
      loop(NewChildList);
    {stop, From}  ->
      From ! {reply, terminate(ChildList)},
      ok
  end.

%% We stop the supervisor by calling the synchronous client function stop/0.
%% Upon receiving the stop message, the supervisor runs through the ChildList,
%% terminating the children one by one. Having terminated all the children,
%% the atom ok is returned to the process that initiated the stop call:

-spec stop() -> ok.
stop() ->
  ?MODULE ! {stop, self()},
  receive {reply, Reply} -> Reply end.

-spec terminate([child_spec_pid(_)]) -> 'ok'.
terminate([]) -> ok;
terminate([{Pid, _} | ChildList]) ->
  exit(Pid, kill),
  terminate(ChildList).
