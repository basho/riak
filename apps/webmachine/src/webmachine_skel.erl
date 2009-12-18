%% @author Justin Sheehy <justin@basho.com>
%% @author Andy Gross <andy@basho.com>
%% @copyright 2007-2008 Basho Technologies
%%
%%    Licensed under the Apache License, Version 2.0 (the "License");
%%    you may not use this file except in compliance with the License.
%%    You may obtain a copy of the License at
%%
%%        http://www.apache.org/licenses/LICENSE-2.0
%%
%%    Unless required by applicable law or agreed to in writing, software
%%    distributed under the License is distributed on an "AS IS" BASIS,
%%    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%    See the License for the specific language governing permissions and
%%    limitations under the License.

-module(webmachine_skel).
-export([skelcopy/2]).

-include_lib("kernel/include/file.hrl").

%% External API

skelcopy(DestDir, Name) ->
    ok = ensuredir(DestDir),
    LDst = case length(filename:dirname(DestDir)) of 
               1 -> %% handle case when dirname returns "/"
                   0;
               N ->
                   N + 1
           end,
    skelcopy(src(), DestDir, Name, LDst),
    ok = file:make_symlink(
	   filename:join(filename:dirname(code:which(?MODULE)), ".."),
	   filename:join([DestDir, Name, "deps", "webmachine"])).
    

%% Internal API

src() ->
    Dir = filename:dirname(code:which(?MODULE)),
    filename:join(Dir, "../priv/skel").

skel() ->
    "skel".

skelcopy(Src, DestDir, Name, LDst) ->
    Dest = re:replace(filename:basename(Src), skel(), Name,
                      [global, {return, list}]),
    case file:read_file_info(Src) of
        {ok, #file_info{type=directory, mode=Mode}} ->
            Dir = DestDir ++ "/" ++ Dest,
            EDst = lists:nthtail(LDst, Dir),
            ok = ensuredir(Dir),
            ok = file:write_file_info(Dir, #file_info{mode=Mode}),
            {ok, Files} = file:list_dir(Src),
            io:format("~s/~n", [EDst]),
            lists:foreach(fun ("." ++ _) -> ok;
                              (F) ->
                                  skelcopy(filename:join(Src, F), 
                                           Dir,
                                           Name,
                                           LDst)
                          end,
                          Files),
            ok;
        {ok, #file_info{type=regular, mode=Mode}} ->
            OutFile = filename:join(DestDir, Dest),
            {ok, B} = file:read_file(Src),
            S = re:replace(binary_to_list(B), skel(), Name,
                           [{return, list}, global]),
            ok = file:write_file(OutFile, list_to_binary(S)),
            ok = file:write_file_info(OutFile, #file_info{mode=Mode}),
            io:format("    ~s~n", [filename:basename(Src)]),
            ok;
        {ok, _} ->
            io:format("ignored source file: ~p~n", [Src]),
            ok
    end.

ensuredir(Dir) ->
    case file:make_dir(Dir) of
        ok ->
            ok;
        {error, eexist} ->
            ok;
        E ->
            E
    end.
