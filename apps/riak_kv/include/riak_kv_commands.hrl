
-record(riak_kv_put_command, {
          sender :: pid(),
          bucket :: binary(),
          key :: binary(),
          object :: tuple(),
          req_id :: non_neg_integer(),
          start_time :: tuple(),
          options :: list()}
        ).
