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

-export([all/0,
	 init_per_suite/1, end_per_suite/1,
	 init_per_testcase/2, end_per_testcase/2]).

-export([
  simple_query_test_binary/1,
  simple_query_test_str/1,
  bulk_query_test/1
]).

all() ->
  [simple_query_test_str, simple_query_test_binary, bulk_query_test].

init_per_suite(Config) ->
  ok = craterl:start(),
  ClientRef = craterl:new([{<<"localhost">>, 48200}, {<<"localhost">>, 48201}]),
  [{client, ClientRef} | Config].

end_per_suite(Config) ->
  ClientRef = ct:get_config(client),
  craterl_gen_server:stop(ClientRef),
  application:stop(craterl),
  Config.

init_per_testcase(bulk_query_test, Config) ->
  Client = ?config(client, Config),
  {ok, _SqlResponse} = craterl:sql(Client, <<"create table test (id int primary key, name string) with (number_of_replicas=0)">>, [], true),
  ct_helpers:wait_for_green_state(<<"localhost:48200">>),
  Config;
init_per_testcase(_, Config) ->
  Config.
end_per_testcase(bulk_query_test, Config) ->
  Client = ?config(client, Config),
  {ok, _SqlResponse} = craterl:sql(Client, <<"drop table test">>, [], true),
  Config;
end_per_testcase(_, Config) ->
  Config.

simple_query_test_binary(_Config) ->
  {ok, Response} = craterl:sql(<<"select 'abc', 42, id, name from sys.cluster">>),
  [<<"'abc'">>, <<"42">>, <<"id">>, <<"name">>] = craterl_resp:column_names(Response),
  1 = craterl_resp:row_count(Response),
  [[<<"abc">>|Tail]] = craterl_resp:rows(Response),
  [42|_] = Tail.

simple_query_test_str(_Config) ->
  {ok, Response} = craterl:sql("select * from sys.cluster"),
  [<<"id">>,<<"name">>,<<"master_node">>, <<"settings">>] = craterl_resp:column_names(Response).

bulk_query_test(Config) ->
  Client = ?config(client, Config),
  {ok, SqlBulkResponse} = craterl:sql_bulk(Client, <<"insert into test (id, name) values (?, ?)">>, [[1, <<"Ford">>], [2, <<"Trillian">>], [3, <<"Zaphod">>]], true),
  Results = craterl_resp:bulk_results(SqlBulkResponse),
  3 = length(Results),
  SumFun = fun (BulkResult, Acc) -> Acc + craterl_resp:row_count(BulkResult) end,
  3 = lists:foldl(SumFun, 0, Results),
  {ok, SqlBulkResponse2} = craterl:sql_bulk(Client, <<"insert into test (id, name) values (?, ?)">>, [[1, <<"Ford">>], [2, <<"Trillian">>], [3, <<"Zaphod">>]], true),
  Results2 = craterl_resp:bulk_results(SqlBulkResponse2),
  3 = length(Results2),
  [-2, -2, -2] = lists:map(fun craterl_resp:row_count/1, Results2).  % -2 means failure
