-module(crate_erlang_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Servers = crate_erlang_app:get_env(crate_servers, [<<"localhost">>, 4200]),
    {ok,
      {{one_for_one, 5, 10},
        [{crate_req_sup,
          {crate_request_handler_sup, start_link, []},
          permanent,
          5000,
          supervisor,
          [crate_request_handler_sup]},
         {crate_connection_manager,
          {connection_manager, start_link, [Servers]},
          permanent,
          5000,
          worker,
          [connection_manager]}
        ]
      }
    }.

