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

%% @doc Mochiweb interface for webmachine.
-module(webmachine_mochiweb).
-author('Justin Sheehy <justin@basho.com>').
-author('Andy Gross <andy@basho.com>').
-export([start/1, stop/0, loop/1]).

start(Options) ->
    {DispatchList, Options1} = get_option(dispatch, Options),
    {ErrorHandler0, Options2} = get_option(error_handler, Options1),
    {EnablePerfLog, Options3} = get_option(enable_perf_logger, Options2),
    ErrorHandler = 
	case ErrorHandler0 of 
	    undefined ->
		webmachine_error_handler;
	    EH -> EH
	end,
    {LogDir, Options4} = get_option(log_dir, Options3),
    webmachine_sup:start_logger(LogDir),
    case EnablePerfLog of
	true ->
	    application:set_env(webmachine, enable_perf_logger, true),
	    webmachine_sup:start_perf_logger(LogDir);
	_ ->
	    ignore
    end,
    application:set_env(webmachine, dispatch_list, DispatchList),
    application:set_env(webmachine, error_handler, ErrorHandler),
    mochiweb_http:start([{name, ?MODULE}, {loop, fun loop/1} | Options4]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(MochiReq) ->
    Req = webmachine:new_request(mochiweb, MochiReq),
    {ok, DispatchList} = application:get_env(webmachine, dispatch_list),
    case webmachine_dispatcher:dispatch(Req:path(), DispatchList) of
        {no_dispatch_match, _UnmatchedPathTokens} ->
            {ok, ErrorHandler} = application:get_env(webmachine, error_handler),
	    ErrorHTML = ErrorHandler:render_error(404, Req, {none, none, []}),
	    Req:append_to_response_body(ErrorHTML),
	    Req:send_response(404),
	    LogData = Req:log_data(),
	    LogModule = 
                case application:get_env(webmachine,webmachine_logger_module) of
		    {ok, Val} -> Val;
		    _ -> webmachine_logger
		end,
	    spawn(LogModule, log_access, [LogData]),
	    Req:stop();
        {Mod, ModOpts, PathTokens, Bindings, AppRoot, StringPath} ->
            BootstrapResource = webmachine_resource:new(x,x,x,x),
            {ok, Resource} = BootstrapResource:wrap(Mod, ModOpts),
	    Req:load_dispatch_data(Bindings,PathTokens,AppRoot,StringPath,Req),
	    Req:set_metadata('resource_module', Mod),
            webmachine_decision_core:handle_request(Req, Resource)
    end.

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

