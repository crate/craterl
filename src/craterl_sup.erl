%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, CRATE Technology GmbH
%%% Licensed to CRATE Technology GmbH ("Crate") under one or more contributor
%%% license agreements.  See the NOTICE file distributed with this work for
%%% additional information regarding copyright ownership.  Crate licenses
%%% this file to you under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.  You may
%%% obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
%%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
%%% License for the specific language governing permissions and limitations
%%% under the License.
%%%
%%% However, if you have executed another commercial license agreement
%%% with Crate these terms will supersede the license and you may use the
%%% software solely pursuant to the terms of the relevant commercial agreement.
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(craterl_sup).

-include("craterl.hrl").

-behaviour(supervisor).

%% API
-export([start_link/0, start_client/3, stop_client/1]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_client(ClientSpec::craterl_client_spec(), Servers::[craterl_server_spec()], Options::[term()]) -> atom().
start_client(ClientSpec, Servers, Options) ->
  ClientName = client_name(ClientSpec),
  case supervisor:start_child(?MODULE, {
    ClientName,
    {
      craterl_gen_server, start_link, [ClientSpec, Servers, Options]
    },
    permanent,
    5000,
    worker,
    [craterl_gen_server]
  }) of
    {ok, _ChildPid} ->
      ClientName;
    {error, {already_started, _Pid}} -> {error, {already_started, client_name(ClientSpec)}}
  end.

-spec stop_client(atom()) -> ok | {error, term()}.
stop_client(ClientName) ->
  supervisor:terminate_child(?MODULE, ClientName),
  supervisor:delete_child(?MODULE, ClientName).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok,
      {{one_for_one, 5, 10},
        [] %% initially empty
      }
    }.


%% INTERNAL

%%
%% @doc
%% return the name to be used when calling the server
%%
-spec client_name(ClientSpec:: craterl_client_spec()) -> atom().
client_name(ClientSpec) ->
  case ClientSpec of
     {local, Name} when is_atom(Name) -> Name;
     {global, Name} when is_atom(Name) -> Name;
     {via, _Module, Name} when is_atom(Name) -> Name;
     Name when is_atom(Name) -> Name
  end.
