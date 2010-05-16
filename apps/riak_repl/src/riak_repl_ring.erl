%% Riak EnterpriseDS
%% Copyright (c) 2007-2010 Basho Technologies, Inc.  All Rights Reserved.
-module(riak_repl_ring).
-author('Andy Gross <andy@andygross.org>').
-include("riak_repl.hrl").
-export([ensure_config/1,
         add_site/2,
         add_listener/2,
         del_site/2,
         del_listener/2,
         set_listener_meta/2,
         set_listener_meta/3,
         get_listener_meta/2,
         get_listener_meta/3,
         set_site_meta/2,
         set_site_meta/3,
         get_site_meta/2,
         get_site_meta/3]).

ensure_config(_) -> ok.
add_site(_, _) -> ok.
add_listener(_,_) -> ok.
del_site(_,_) -> ok.
del_listener(_,_) -> ok.
get_listener_meta(_,_,_) -> ok.
get_listener_meta(_,_) -> ok.
set_listener_meta(_,_) -> ok.
set_listener_meta(_,_,_) -> ok.
set_site_meta(_,_) ->  ok.
set_site_meta(_,_,_) ->  ok.
get_site_meta(_,_) ->  ok.
get_site_meta(_,_,_) ->  ok.
    
     
    

