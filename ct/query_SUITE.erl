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

-module(query_SUITE).

-include_lib("common_test/include/ct.hrl").

-include("craterl.hrl").

-export([all/0,
	 init_per_suite/1, end_per_suite/1,
	 init_per_testcase/2, end_per_testcase/2]).

-export([simple_query_test_binary/1, simple_query_test_str/1]).

all() ->
  [simple_query_test_str, simple_query_test_binary].

init_per_suite(Config) ->
  ok = hackney:start(),
  ok = craterl:start(),
  Config.

end_per_suite(Config) ->
  application:stop(craterl),
  Config.

init_per_testcase(_, Config) ->
  Config.

end_per_testcase(_, Config) ->
  Config.

simple_query_test_binary(_Config) ->
  {ok, Response} = craterl:sql(<<"select id, name from sys.cluster">>),
  [<<"id">>, <<"name">>] = Response#sql_response.cols.

simple_query_test_str(_Config) ->
  {ok, #sql_response{}} = craterl:sql("select * from sys.cluster").
