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
-module(connection_manager_tests).
-author("mat").

-include_lib("eunit/include/eunit.hrl").

-define(setup(F, Start), {setup, Start, fun stop/1, F}).
simple_test() ->
  ?assert(true).

connection_manager_single_test_() ->
  {"test connection manager with a single server",
    ?setup(fun single_server_test/1, fun start_single/0)
  }.

connection_manager_multiple_test_() ->
  {
    "test connection manager with multiple servers",
    ?setup(fun multiple_server_test/1, fun start_multiple/0)
  }.

connection_manager_inactive_test_() ->
  {
    "test connection manager with multiple servers of which one is inactive",
    ?setup(fun inactive_server_test/1, fun start_multiple/0)
  }.

connection_manager_set_server_test_() ->
  {"test setting a list of active servers",
    ?setup(fun set_server_test/1, fun start_single/0)
  }.

%%%%%%%%%%%%%
%%% setup %%%
%%%%%%%%%%%%%
start_single() ->
  os:putenv("CRATE_SERVERS", "localhost:4200"),
  start().

start_multiple() ->
  os:putenv("CRATE_SERVERS", "localhost:4200, localhost:4201, localhost:4202"),
  start().

start() ->
  {ok, _Pid} = config_provider:start_link(),
  {ok, _CmPid} = connection_manager:start_link().

%% teardown
stop(_) ->
  config_provider:stop(),
  connection_manager:stop(),
  timer:sleep(100).

single_server_test(_) ->
  [
    ?_assertEqual({ok, <<"localhost:4200">>}, connection_manager:get_server()),
    ?_assertEqual({ok, <<"localhost:4200">>}, connection_manager:get_server()),
    ?_assertEqual({ok, <<"localhost:4200">>}, connection_manager:get_server())
  ].

multiple_server_test(_) ->
  [
    ?_assertEqual({ok, <<"localhost:4200">>}, connection_manager:get_server()),
    ?_assertEqual({ok, <<"localhost:4201">>}, connection_manager:get_server()),
    ?_assertEqual({ok, <<"localhost:4202">>}, connection_manager:get_server()),
    ?_assertEqual({ok, <<"localhost:4200">>}, connection_manager:get_server()),
    ?_assertEqual({ok, <<"localhost:4201">>}, connection_manager:get_server()),
    ?_assertEqual({ok, <<"localhost:4202">>}, connection_manager:get_server())
  ].

inactive_server_test(_) ->
  {ok, Server} = connection_manager:get_server(),
  [
    ?_assertEqual(<<"localhost:4200">>, Server),
    ?_assertEqual(ok, connection_manager:add_inactive(Server)),
    ?_assertEqual({ok, <<"localhost:4201">>}, connection_manager:get_server()),
    ?_assertEqual({ok, <<"localhost:4202">>}, connection_manager:get_server()),
    ?_assertEqual({ok, <<"localhost:4201">>}, connection_manager:get_server()),
    ?_assertEqual({ok, <<"localhost:4202">>}, connection_manager:get_server()),
    ?_assertEqual(ok, connection_manager:add_inactive(<<"localhost:4201">>)),
    ?_assertEqual({ok, <<"localhost:4202">>}, connection_manager:get_server()),
    ?_assertEqual({ok, <<"localhost:4202">>}, connection_manager:get_server()),
    ?_assertEqual(ok, connection_manager:add_inactive(<<"localhost:4202">>)),
    ?_assertEqual(none_active, connection_manager:get_server())
  ].

parse_server_string_test_() ->
  [
    ?_assertEqual(
      connection_manager:parse_servers_string("localhost:123, host:456"),
      [<<"localhost:123">>, <<"host:456">>])
  ].

set_server_test(_) ->
    ok = connection_manager:set_servers([aserver]),
    [
     ?_assertEqual({ok, aserver}, connection_manager:get_server())
    ].
