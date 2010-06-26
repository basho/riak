-type sender_type() :: fsm | server | raw.
-type sender() :: {sender_type(), reference(), pid()} | noreply.
-type partition() :: non_neg_integer().
-type vnode_req() :: term().

-record(riak_vnode_req_v1, {
          index :: partition(),
          sender=noreply :: sender(),
          request :: vnode_req()}).


-record(riak_core_fold_req_v1, {
          foldfun :: fun(),
          acc0 :: term()}).

-define(VNODE_REQ, #riak_vnode_req_v1).
-define(FOLD_REQ, #riak_core_fold_req_v1).

