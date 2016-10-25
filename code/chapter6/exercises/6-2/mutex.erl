%% Code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(mutex).
-export([start/0, stop/0]).
-export([wait/0, signal/0, get_state/0]).
-export([init/0]).

-type msg() :: 'get_state' | 'stop' | {'signal', pid()} | {'wait', pid()}.

-spec start() -> 'ok'.
start() ->
  register(?MODULE, spawn(?MODULE, init, [])),
  ok.

-spec stop() -> 'ok'.
stop() ->
  call(stop),
  ok.

-spec wait() -> 'ok'.
wait() ->
  call(wait, self()),
  receive ok -> ok end.

-spec signal() -> 'ok'.
signal() ->
  call(signal, self()),
  ok.

-spec get_state() -> 'ok'.
get_state() ->
  call(get_state),
  ok.

-spec call(msg()) -> 'ok'.
call(Msg) ->
  ?MODULE ! Msg,
  ok.

call(Type, Msg) ->
  call({Type, Msg}).

-spec init() -> 'ok'.
init() ->
  process_flag(trap_exit, true),
  free().

-spec free() -> 'ok'.
free() ->
  receive
    {wait, Pid} ->
      Pid ! ok,
      timer:sleep(1000),
      io:format("mutex: trying to link to pid ~w~n", [Pid]),
      link(Pid),
      busy(Pid);
    get_state ->
      io:format("mutex: Currently free.~n", []),
      free();
    stop ->
      terminate()
  end.

-spec busy(pid()) -> 'ok'.
busy(Pid) ->
  receive
    {signal, Pid} ->
      free();
    get_state ->
      io:format("mutex: Currently busy.~n", []),
      busy(Pid);
    {'EXIT', Pid, Reason} ->
      io:format("mutex: Releasing mutex for pid ~w because reason: ~w~n", [Pid, Reason]),
      free()
  end.

-spec terminate() -> 'ok'.
terminate() ->
  receive
    {wait, Pid} ->
      exit(Pid, kill),
      terminate()
  after
    0 -> ok
  end.

