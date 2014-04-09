-module(crate_erlang_app).

-behaviour(application).

%% API
-export([get_env/2]).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    PoolName = get_env(crate_pool_name, crate),
    PoolSize = get_env(crate_pool_size, 500),
    TimeOut = get_env(crate_pool_timeout, 150000),
    hackney_pool:start_pool(PoolName, [{pool_size, PoolSize}, {timeout, TimeOut}]),
    {ok, Pid} = crate_erlang_sup:start_link(),
    {ok, Pid, PoolName}.

stop(PoolName) ->
    hackney_pool:stop_pool(PoolName),
    ok.

get_env(Key, Default) when is_atom(Key) ->
  get_env(atom_to_list(Key), Default);
get_env(Key, Default) when is_list(Key) ->
  case os:getenv(string:to_upper(Key)) of
    undefined ->
      case application:get_env(Key) of
        {ok, Val} -> Val;
        undefined -> Default
      end;
    Value -> Value
  end.
