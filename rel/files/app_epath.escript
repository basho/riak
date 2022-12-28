-module(app_epath).
-export([main/1]).

main([AdvancedConfig]) ->
    {ok, [AA]} = file:consult(AdvancedConfig),
    [p(A) || A <- AA].

p({App, CC}) ->
    [io:format("~s ~s ~9999p\n", [App, K, V]) || {K, V} <- CC].
