-module(five_3_event_handler).
-export([start/0, start/1, stop/0]).
-export([add_handler/2, delete_handler/1, get_data/1, send_event/1, swap_handlers/2]).
-export([init/1]).

start() -> start([]).

start(HandlerList) ->
  register(?MODULE, spawn(?MODULE, init, [HandlerList])), ok.

init(HandlerList) ->
  loop(initialize(HandlerList)).

initialize([]) -> [];
initialize([{Handler, InitData}|Rest]) ->
  [{Handler, Handler:init(InitData)}|initialize(Rest)].

stop() ->
  ?MODULE ! {stop, self()},
  receive {reply, Reply} -> Reply end.

terminate([]) -> [];
terminate([{Handler, Data}|Rest]) ->
  [{Handler, Handler:terminate(Data)}|terminate(Rest)].

add_handler(Handler, InitData) ->
  call({add_handler, Handler, InitData}).

delete_handler(Handler) ->
  call({delete_handler, Handler}).

get_data(Handler) ->
  call({get_data, Handler}).

send_event(Event) ->
  call({send_event, Event}).

handle_msg({add_handler, Handler, InitData}, LoopData) ->
  {ok, [{Handler, Handler:init(InitData)}|LoopData]};

handle_msg({delete_handler, Handler}, LoopData) ->
  case lists:keysearch(Handler, 1, LoopData) of
    false ->
      {{error, instance}, LoopData};
    {value, {Handler, Data}} ->
      Reply = {data, Handler:terminate(Data)},
      NewLoopData = lists:keydelete(Handler, 1, LoopData),
      {Reply, NewLoopData}
  end;

handle_msg({get_data, Handler}, LoopData) ->
  case lists:keysearch(Handler, 1, LoopData) of
    false                    -> {{error, instance}, LoopData};
    {value, {Handler, Data}} -> {{data, Data}, LoopData}
  end;

handle_msg({send_event, Event}, LoopData) ->
  {ok, event(Event, LoopData)};

handle_msg({swap_handlers, OldHandler, NewHandler}, LoopData) ->
  NewLoopData = lists:map(fun ({Handler, HandlerData}) when Handler == OldHandler ->
                              {NewHandler, NewHandler:init(OldHandler:terminate(HandlerData))};
                              (Other) -> Other
                          end,
                          LoopData),
  {ok, NewLoopData}.

event(_Event, []) -> [];
event(Event, [{Handler, Data}|Rest]) ->
  [{Handler, Handler:handle_event(Event, Data)}|event(Event, Rest)].

call(Msg) ->
  ?MODULE ! {request, self(), Msg},
  receive {reply, Reply} -> Reply end.

reply(To, Msg) ->
  To ! {reply, Msg}.

loop(State) ->
  receive
    {request, From, Msg} ->
      {Reply, NewState} = handle_msg(Msg, State),
      reply(From, Reply),
      loop(NewState);
    {stop, From}  ->
      reply(From, terminate(State))
  end.


%%%%%%%%%%%%%%%%%%%%%%%

swap_handlers(OldHandler, NewHandler) ->
  call({swap_handlers, OldHandler, NewHandler}).
