%% @author Bryan Fink
%% @doc jiak_proxy is intended to be a simple webmachine resource
%%      for proxying Webmachine requests to Jiak.
%%
%%      Note: this is not a production-quality proxy resource.
%%      For something that will be used in a real application, it
%%      is recommended that ibrowse be used instead of the inets
%%      http client.
-module(jiak_proxy).
-export([init/1,
         service_available/2]).
-include_lib("webmachine/include/webmachine.hrl").

init(Config) -> {ok, Config}.

%% request to jiak is made in service_available, such that
%% if jiak isn't up, we return 503 Service Unavailable, as expected
service_available(RP, C={_ExternalPath, JiakPath}) ->
    %% point path at jiak server
    Path = lists:append(
             [JiakPath,
              wrq:disp_path(RP),
              case wrq:req_qs(RP) of
                  [] -> [];
                  Qs -> [$?|mochiweb_util:urlencode(Qs)]
              end]),

    %% translate webmachine details to ibrowse details
    Headers = clean_request_headers(
                mochiweb_headers:to_list(wrq:req_headers(RP))),
    Method = wm_to_http_method(wrq:method(RP)),
    Req = case wrq:req_body(RP) of
              Empty when Empty==undefined;
                         Empty==[];
                         Empty==<<>> ->
                  {Path, Headers};
              B ->
                  {Path, Headers,
                   wrq:get_req_header("content-type", RP), B}
          end,
    case http:request(Method, Req, [{autoredirect, false}], []) of
        {ok, {{_, Status, _}, JiakHeaders, RespBody}} ->
            RespHeaders = fix_location(JiakHeaders, C),

            %% stop resource processing here and return whatever
            %% jiak wanted to return
            {{halt, Status},
             wrq:set_resp_headers(RespHeaders,
                                  wrq:set_resp_body(RespBody, RP)),
             C};
        _ ->
            {false, RP, C}
    end.

%% ibrowse will recalculate Host and Content-Length headers,
%% and will muck them up if they're manually specified
clean_request_headers(Headers) ->
    [{if is_atom(K) -> atom_to_list(K);
         true -> K end,
      V} || {K,V} <- Headers, K /= 'Host', K /= 'Content-Length'].

%% webmachine expresses method as all-caps string or atom,
%% while ibrowse uses all-lowercase atom
wm_to_http_method(Method) when is_list(Method) ->
    list_to_atom(string:to_lower(Method));
wm_to_http_method(Method) when is_atom(Method) ->
    wm_to_http_method(atom_to_list(Method)).

%% jiak returns a fully-qualified URI in Location -
%% hack off the jiak host, and drop in this proxy host
fix_location([], _) -> [];
fix_location([{"Location", JiakDataPath}|Rest],
             {ExternalPath, JiakPath}) ->
    DataPath = lists:nthtail(length(JiakPath), JiakDataPath),
    [{"Location", ExternalPath++DataPath}|Rest];
fix_location([H|T], C) ->
    [H|fix_location(T, C)].
