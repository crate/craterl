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
-module(craterl_gen_server_tests).
-author("mat").

-include("craterl.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(setup(F, Start), {setup, Start, fun stop/1, F}).

craterl_gen_server_single_test_() ->
  {"test craterl gen server with a single server",
    ?setup(fun single_server_test/1, fun start_single/0)
  }.

craterl_gen_server_multiple_test_() ->
  {
    "test craterl gen server with multiple servers",
    ?setup(fun multiple_server_test/1, fun start_multiple/0)
  }.

craterl_gen_server_inactive_test_() ->
  {
    "test craterl gen server with multiple servers of which one is inactive",
    ?setup(fun inactive_server_test/1, fun start_multiple/0)
  }.

craterl_gen_server_set_server_test_() ->
  {"test setting a list of active servers",
    ?setup(fun set_server_test/1, fun start_single/0)
  }.

craterl_gen_server_active_test_() ->
  {
    "test adding active servers dynamically",
    ?setup(fun active_test/1, fun start_single/0)

  }.

%%%%%%%%%%%%%
%%% setup %%%
%%%%%%%%%%%%%
start_single() ->
  Servers = [{<<"localhost">>, 4200}],
  start(Servers).

start_multiple() ->
  Servers =  [{<<"localhost">>, 4200}, {<<"localhost">>, 4201}, {<<"localhost">>, 4202}],
  start(Servers).

start(Servers) ->
  ok = meck:new(hackney_pool),
  meck:expect(hackney_pool, start_pool, 2, ok),
  {ok, _CmPid} = craterl_gen_server:start_link({local, ?MODULE}, Servers, []),
  Servers.

%% teardown
stop(_) ->
  meck:unload(hackney_pool),
  craterl_gen_server:stop(?MODULE),
  timer:sleep(100).

single_server_test(Servers) ->
  ServerConf = #craterl_server_conf{address = lists:nth(1, Servers), config = craterl_config:apply_defaults()},
  [
    ?_assertEqual({ok, ServerConf}, craterl_gen_server:get_server(?MODULE)),
    ?_assertEqual({ok, ServerConf}, craterl_gen_server:get_server(?MODULE)),
    ?_assertEqual({ok, ServerConf}, craterl_gen_server:get_server(?MODULE))
  ].

multiple_server_test(Servers) ->
  ServersGot = lists:map(fun(_) ->
      {ok, ServerConf} = craterl_gen_server:get_server(?MODULE),
      ServerConf#craterl_server_conf.address
    end,
    lists:seq(1, 4)
  ),
  [
    ?_assertEqual(true, lists:member(Server, Servers)) || Server <- ServersGot
  ].

inactive_server_test(Servers) ->
  {ok, ServerConf} = craterl_gen_server:get_server(?MODULE),
  Server = ServerConf#craterl_server_conf.address,
  [
    ?_assertEqual(true, lists:member(Server, Servers)),
    ?_assertEqual(ok, craterl_gen_server:add_inactive(?MODULE, lists:nth(1, Servers))),
    ?_assertNotEqual({ok, lists:nth(1, Servers)}, craterl_gen_server:get_server(?MODULE)),
    ?_assertNotEqual({ok, lists:nth(1, Servers)}, craterl_gen_server:get_server(?MODULE)),
    ?_assertNotEqual({ok, lists:nth(1, Servers)}, craterl_gen_server:get_server(?MODULE)),
    ?_assertNotEqual({ok, lists:nth(1, Servers)}, craterl_gen_server:get_server(?MODULE)),
    ?_assertEqual(ok, craterl_gen_server:add_inactive(?MODULE, lists:nth(2, Servers))),
    ?_assertEqual(
      {ok, #craterl_server_conf{address = lists:nth(3, Servers), config = craterl_config:apply_defaults()}},
      craterl_gen_server:get_server(?MODULE)
    ),
    ?_assertEqual(
      {ok, #craterl_server_conf{address = lists:nth(3, Servers), config = craterl_config:apply_defaults()}},
      craterl_gen_server:get_server(?MODULE)),
    ?_assertEqual(ok, craterl_gen_server:add_inactive(?MODULE, lists:nth(3, Servers))),
    ?_assertEqual(none_active, craterl_gen_server:get_server(?MODULE))
  ].


set_server_test(_) ->
    ok = craterl_gen_server:set_servers(?MODULE, [{<<"127.0.0.1">>, 123}]),
    ServerConf = #craterl_server_conf{
      address = {<<"127.0.0.1">>, 123},
      config = craterl_config:apply_defaults()
    },
    [
     ?_assertEqual({ok, ServerConf},
       craterl_gen_server:get_server(?MODULE))
    ].

active_test(Servers) ->
  NewServer = {<<"new">>, 123},
  ok = craterl_gen_server:add_active(?MODULE, NewServer),
  AllServers = Servers ++ [NewServer],
  GetServer = fun() ->
    {ok, Server} = craterl_gen_server:get_server(?MODULE),
    Server#craterl_server_conf.address
  end,
  [
    ?_assertEqual(true, lists:member(GetServer(), AllServers)),
    ?_assertEqual(true, lists:member(GetServer(), AllServers)),
    ?_assertEqual(true, lists:member(GetServer(), AllServers)),
    ?_assertEqual(true, lists:member(GetServer(), AllServers))
  ].
