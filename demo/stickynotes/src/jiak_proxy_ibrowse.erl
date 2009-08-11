%% @author Bryan Fink
%% @doc jiak_proxy_ibrowse is intended to be a simple webmachine
%%      resource for proxying Webmachine requests to Jiak.  In theory,
%%      it's general enough to be a simple proxy to most any other
%%      HTTP service, but I make no guarantees about assumption it
%%      makes that are Jiak-specific.
%%
%%      This resource performs the same task as jiak_proxy, but uses
%%      ibrowse instead of inets http.
%%
%%      Load this with a dispatch line like:
%%      {['*'], jiak_proxy_ibrowse, {ExternalPath, JiakPath}}.
%%      Where:
%%        ExternalPath is the base path to this resource, like
%%          "http://localhost:8000/"
%%        JiakPath is the base path to your Jiak server, like
%%          "http://localhost:8098/"
%%
%%      Another useful example might be:
%%        {["jiak", '*'], jiak_proxy_ibrowse,
%%         {"http://localhost:8000/jiak/",
%%          "http://localhost:8098/"}}
%%      Which would redirect requests from
%%        http://localhost:8000/jiak/BUCKET/KEY
%%      to
%%        http://localhost:8098/BUCKET/KEY
-module(jiak_proxy_ibrowse).
-export([init/1,
         service_available/2]).
-include_lib("webmachine/include/webmachine.hrl").

init(Config) -> {ok, Config}.

%% request to Jiak is made in service_available, such that
%% if Jiak isn't up, we return 503 Service Unavailable, as expected
service_available(RP, C={_ExternalPath, JiakPath}) ->
    %% point path at Jiak server
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
    Method = wm_to_ibrowse_method(wrq:method(RP)),
    ReqBody = case wrq:req_body(RP) of
                  undefined -> [];
                  B -> B
              end,

    case ibrowse:send_req(Path, Headers, Method, ReqBody) of
        {ok, Status, JiakHeaders, RespBody} ->
            RespHeaders = fix_location(JiakHeaders, C),

            %% stop resource processing here and return whatever
            %% Jiak wanted to return
            {{halt, list_to_integer(Status)},
             wrq:set_resp_headers(RespHeaders,
                                  wrq:set_resp_body(RespBody, RP)),
             C};
        _ ->
            {false, RP, C}
    end.

%% ibrowse will recalculate Host and Content-Length headers,
%% and will muck them up if they're manually specified
clean_request_headers(Headers) ->
    [{K,V} || {K,V} <- Headers,
              K /= 'Host', K /= 'Content-Length'].

%% webmachine expresses method as all-caps string or atom,
%% while ibrowse uses all-lowercase atom
wm_to_ibrowse_method(Method) when is_list(Method) ->
    list_to_atom(string:to_lower(Method));
wm_to_ibrowse_method(Method) when is_atom(Method) ->
    wm_to_ibrowse_method(atom_to_list(Method)).

%% Jiak returns a fully-qualified URI in Location -
%% hack off the Jiak host, and drop in this proxy host
fix_location([], _) -> [];
fix_location([{"Location", JiakDataPath}|Rest],
             {ExternalPath, JiakPath}) ->
    DataPath = lists:nthtail(length(JiakPath), JiakDataPath),
    [{"Location", ExternalPath++DataPath}|Rest];
fix_location([H|T], C) ->
    [H|fix_location(T, C)].
