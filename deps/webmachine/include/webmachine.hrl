-export([ping/2]).

-include_lib("webmachine/include/wm_reqdata.hrl").

ping(ReqData, State) ->
    {pong, ReqData, State}.


