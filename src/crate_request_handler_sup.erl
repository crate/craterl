%%%-------------------------------------------------------------------
%%% @author mat
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Apr 2014 11:51 AM
%%%-------------------------------------------------------------------
-module(crate_request_handler_sup).
-author("mat").

-behaviour(supervisor).
-include("craterl.hrl").
%% API
-export([start_link/0, request/3]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec request(Request, ServerSpec, CallerPid) -> Result when
  Request :: #sql_request{} | #blob_request{} ,
  ServerSpec :: string() | binary(),
  CallerPid :: pid(),
  Result :: {ok, ChildPid :: pid()} | {error, Err :: term()}.
request(Request, ServerSpec, CallerPid) when is_pid(CallerPid) ->
  case supervisor:start_child(?MODULE, [Request, ServerSpec, CallerPid]) of
    {ok, ChildPid} -> {ok, ChildPid};
    {ok, ChildPid, _} -> {ok, ChildPid};
    {error, Err} -> {error, Err}
  end.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]
  }} |
  ignore |
  {error, Reason :: term()}).
init([]) ->
  RestartStrategy = simple_one_for_one,
  MaxRestarts = 2,
  MaxSecondsBetweenRestarts = 60,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = temporary,
  Shutdown = 2000,
  Type = worker,

  AChild = {crate_request_handler, {crate_request_handler, start_link, []},
    Restart, Shutdown, Type, []},

  {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
