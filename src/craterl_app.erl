-module(craterl_app).

-behaviour(application).

%% API
-export([]).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Pid} = craterl_sup:start_link(),
    PoolName = config_provider:get(crate_pool_name, crate),
    PoolSize = config_provider:get(crate_pool_size, 500),
    TimeOut = config_provider:get(crate_pool_timeout, 150000),
    hackney_pool:start_pool(PoolName, [{pool_size, PoolSize}, {timeout, TimeOut}]),
    {ok, Pid, PoolName}.

stop(PoolName) ->
    hackney_pool:stop_pool(PoolName),
    ok.
