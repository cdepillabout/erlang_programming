% Didn't implement the requirement that client is only able to allocate up to
% three frequencies.

-module(five_2_freq).
-export([start/0, stop/0, allocate/0, deallocate/1]).
-export([init/0]).

%% These are the start functions used to create and
%% initialize the server.

start() ->
  register(?MODULE, spawn(?MODULE, init, [])).

init() ->
  Frequencies = {get_frequencies(), []},
  loop(Frequencies).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%%  The client Functions

stop()           -> call(stop).
allocate()       -> call(allocate).
deallocate(Freq) -> call({deallocate, Freq}).

%% We hide all message passing and the message
%% protocol in a functional interface.

call(Message) ->
  ?MODULE ! {request, self(), Message},
  receive
    {reply, Reply} -> Reply
  end.

%% The Main Loop

loop(Frequencies) ->
  receive
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      reply(Pid, Reply),
      loop(NewFrequencies);
    {request, Pid , {deallocate, Freq}} ->
      NewFrequencies = deallocate(Frequencies, Freq, Pid),
      reply(Pid, ok),
      loop(NewFrequencies);
    {request, Pid, stop} ->
      {_, Allocated} = Frequencies,
      case length(Allocated) of
        0 -> reply(Pid, ok);
        N ->
          reply(Pid, {error, {frequencies_allocated, N}}),
          loop(Frequencies)
      end
  end.

reply(Pid, Reply) ->
  Pid ! {reply, Reply}.

%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq, Pid) ->
  case lists:any(fun (AllocVal) -> AllocVal == {Freq, Pid} end, Allocated) of
    true -> {[Freq|Free], lists:delete({Freq, Pid}, Allocated)};
    false -> {Free, Allocated}
  end.
