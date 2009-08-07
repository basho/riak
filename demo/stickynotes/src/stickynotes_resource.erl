%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.

-module(stickynotes_resource).
-export([init/1,
         resource_exists/2,
         content_types_provided/2,
         generate_body/2]).

-include_lib("webmachine/include/webmachine.hrl").

-record(ctx, {docroot,    %% where our static files live
              fullpath}). %% path to file we'll send

init([DocRoot]) -> {ok, #ctx{docroot=DocRoot}}.

resource_exists(RD, Ctx) ->
    Rel = case wrq:disp_path(RD) of
              [] ->
                  "index.html";
              Path ->
                  filename:join([ P || P <- string:tokens(Path, "/"),
                                       P /= ".."])
          end,
    Abs = filename:join([Ctx#ctx.docroot,Rel]),
    {filelib:is_file(Abs), RD,Ctx#ctx{fullpath=Abs}}.

content_types_provided(RD, Ctx) ->
    {case wrq:disp_path(RD) of
         [] -> [{"text/html", generate_body}];
        Path ->
            [{webmachine_util:guess_mime(Path), generate_body}]
     end,
     RD, Ctx}.

generate_body(RD, Ctx) ->
    {ok, Data} = file:read_file(Ctx#ctx.fullpath),
    {Data, RD, Ctx}.
