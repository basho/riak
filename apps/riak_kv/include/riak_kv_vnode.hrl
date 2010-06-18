-include_lib("riak_core/include/riak_core_vnode.hrl").

-record(riak_kv_put_req_v1, {
          bucket :: binary(),
          key :: binary(),
          object :: term(),
          req_id :: non_neg_integer(),
          start_time :: tuple(),
          options :: list()}).

-record(riak_kv_get_req_v1, {
          bucket :: binary(),
          key :: binary(),
          req_id :: non_neg_integer()}).

-record(riak_kv_listkeys_req_v1, {
          bucket :: binary(),
          req_id :: non_neg_integer()}).

-record(riak_kv_delete_req_v1, {
          bkey :: {binary(), binary()},
          req_id :: non_neg_integer()}).


-define(KV_PUT_REQ, #riak_kv_put_req_v1).
-define(KV_GET_REQ, #riak_kv_get_req_v1).
-define(KV_LISTKEYS_REQ, #riak_kv_listkeys_req_v1).
-define(KV_DELETE_REQ, #riak_kv_delete_req_v1).



