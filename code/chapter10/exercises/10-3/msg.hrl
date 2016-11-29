
-record(send_msg, {send_time :: erlang:timestamp(),
                   msg :: string()}).

-record(recv_msg, {recv_time :: erlang:timestamp(),
                   send_time :: erlang:timestamp()}).
