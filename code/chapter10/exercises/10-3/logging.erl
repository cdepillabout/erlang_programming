
-module(logging).
-export([send_mesg/2, recv_mesg/1]).
-export([clear_tables/0, create_tables/0, received_all/0]).

-include("msg.hrl").

-type send_msg() :: #send_msg{}.
-type recv_msg() :: #recv_msg{}.

-spec send_mesg(pid(), any()) -> ok.
send_mesg(Pid, Msg) ->
  SendTime = erlang:timestamp(),
  SendMsg = #send_msg{send_time=SendTime, msg=Msg},
  log_send_mesg(SendMsg),
  Pid ! SendMsg,
  ok.

-spec log_send_mesg(send_msg()) -> ok.
log_send_mesg(#send_msg{send_time=SendTime}) ->
  ets:insert(send, {SendTime}),
  ok.

-spec recv_mesg(boolean()) -> any().
recv_mesg(ShouldLog) ->
  receive
    #send_msg{send_time=SendTime, msg=Msg} ->
      RecvTime = erlang:timestamp(),
      if
        ShouldLog ->
          log_recv_mesg(#recv_msg{recv_time=RecvTime, send_time=SendTime});
        true ->
          ok
      end,
      Msg
  end.

-spec log_recv_mesg(recv_msg()) -> ok.
log_recv_mesg(#recv_msg{recv_time=RecvTime, send_time=SendTime}=RecvMsg) ->
  ets:insert(recv, RecvMsg),
  ets:insert(send_recv_index, {SendTime, RecvTime}),
  ok.

%%%%%%%%%%%%%%%%%%%%%%
%% Table Operations %%
%%%%%%%%%%%%%%%%%%%%%%

-spec clear_tables() -> ok.
clear_tables() ->
  ets:delete_all_objects(send),
  ets:delete_all_objects(recv),
  ets:delete_all_objects(send_recv_index),
  ok.

-spec create_tables() -> ok.
create_tables() ->
  ets:new(send, [set, named_table, public]),
  ets:new(recv, [set, named_table, public, {keypos, #recv_msg.recv_time}]),
  ets:new(send_recv_index, [bag, named_table, public]),
  ok.

-spec received_all() -> boolean().
received_all() ->
  SendList = ets:tab2list(send),
  lists:all(fun ({SendTime}) ->
                Res = ets:lookup(send_recv_index, SendTime),
                case Res of
                  [] -> false;
                  _ -> true
                end
            end,
            SendList).
