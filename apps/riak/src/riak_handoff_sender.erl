-module(riak_handoff_sender).
-export([get_handoff_port/1]).



%% @spec get_handoff_port(Node::atom()) -> integer()
get_handoff_port(Node) when is_atom(Node) ->
    gen_server2:call({riak_handoff_listener, Node}, handoff_port).


