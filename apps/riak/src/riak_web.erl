%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at

%%   http://www.apache.org/licenses/LICENSE-2.0

%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.

%% @doc Convenience functions for setting up the Jiak HTTP interface
%%      of Riak.  This module loads parameters from the application
%%      environment:
%%
%%<dl><dt>  riak_web_ip
%%</dt><dd>   IP address that the Webmachine node should listen to
%%</dd><dt> riak_web_port
%%</dt><dd>   port that the Webmachine node should listen to
%%</dd><dt> riak_web_logdir
%%</dt><dd>   directory under which the access log will be stored
%%</dd><dt> jiak_name
%%</dt><dd>   the base path under which Jiak should be exposed;
%%            defaulted to "jiak"
%%</dd><dt> raw_name
%%</dt><dd>   the base path under which the raw_http_resource
%%            should be exposed; defaulted to "raw"
%%</dd></dl>
-module(riak_web).

-export([config/0]).

%% @spec config() -> [{Key :: atom(), Value :: term()}]
%% @doc Returns the standard Jiak Webmachine configuration.
%%      pass the return of this function to
%%      webmachine_mochiweb:start/1 to start up a Webmachine
%%      resource serving Jiak out of
%%      http://{riak_web_ip}:{riak_web_port}/jiak/
config() ->
    [{ip, riak:get_app_env(riak_web_ip)},
     {port, riak:get_app_env(riak_web_port)},
     {log_dir, riak:get_app_env(riak_web_logdir, "log")},
     {backlog, 128},
     {dispatch, dispatch_table()}].

dispatch_table() ->
    JiakProps = jiak_props(),
    RawProps = raw_props(),
    MapredProps = mapred_props(),

    [{[proplists:get_value(jiak_name, JiakProps),bucket],
      jiak_resource,
      [{key_type, container}|JiakProps]},
     {[proplists:get_value(jiak_name, JiakProps),bucket,key],
      jiak_resource,
      [{key_type, item}|JiakProps]},
     {[proplists:get_value(jiak_name, JiakProps),bucket,key,'*'],
      jaywalker_resource,JiakProps},

     {[proplists:get_value(prefix, RawProps),bucket],
      raw_http_resource,RawProps},
     {[proplists:get_value(prefix, RawProps),bucket,key],
      raw_http_resource, RawProps},
     {[proplists:get_value(prefix, RawProps),bucket,key,'*'],
      raw_link_walker_resource, RawProps},

     {[proplists:get_value(prefix, MapredProps)],
      mapred_resource, MapredProps},

     {["ping"], ping_http_resource, []}].

jiak_props() ->
    [{jiak_name, riak:get_app_env(jiak_name, "jiak")},
     {riak_local, true},
     {jiak_buckets, [jiak_example]}].

raw_props() ->
    [{prefix, riak:get_app_env(raw_name, "raw")},
     {riak, local}].

mapred_props() ->
    [{prefix, riak:get_app_env(mapred_name, "mapred")}].
