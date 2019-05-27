%%%-------------------------------------------------------------------
%%% Part of logger_cooldown_filter Erlang App
%%% MIT License
%%% Copyright (c) 2019 Jose Maria Perez Ramos
%%%-------------------------------------------------------------------
-module(logger_cooldown_filter_app).

-behaviour(application).
-export([start/2, stop/1]).

-behaviour(supervisor).
-export([init/1]).


%%====================================================================
%% behaviour callbacks
%%====================================================================

start(_StartType, _StartArgs) ->
    supervisor:start_link(?MODULE, []).

stop(_State) ->
    ok.

init(_) ->
    {ok, {#{strategy => one_for_one},
          [#{id => worker,
             start => {logger_cooldown_filter, start_link, []}
          }]
    }}.

