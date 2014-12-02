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
-module(craterl_sql_tests).
-author("mat").

-include("craterl.hrl").
-include_lib("eunit/include/eunit.hrl").


setup_sql() ->
  meck:new(hackney),
  meck:expect(hackney, request, 5, meck:seq([
    {ok, 200, [], make_ref()},
     {ok, 200, [], make_ref()},
    {ok, 200, [], make_ref()},
    {ok, 200, [], make_ref()},
    {ok, 400, [], make_ref()},
    {ok, 200, [], make_ref()},
    {error, timeout},
    {ok, 200, [], make_ref()}
  ])
  ),
  meck:expect(hackney, body, 1,
    meck:seq([
      {ok, <<"{}">>},
      {ok, <<"{\"cols\":[], \"col_types\":[1,2], \"results\":[], \"duration\":4}">>},
      {ok, <<"{\"cols\":[\"a\",\"b\"], \"rows\":[[true, 5.6],[false, -5.6]], \"rowcount\":2, \"duration\":12456}">>},
      {ok, <<"invalid">>},
      {ok, <<"{\"error\":{\"code\":4000, \"message\":\"something went wrong\"}}">>},
      {ok, <<"">>},
      {error, closed}
    ])
  ),
  ok.

teardown_mecks(_) ->
  meck:validate(hackney),
  meck:unload(hackney).

test_server_conf() ->
  #craterl_server_conf{
    address={<<"localhost">>, 4300},
    config = craterl_config:apply_defaults()
  }.

sql_request_test_() ->
{
  "testing if issueing sql request works correctly",
  {
    setup,
    fun setup_sql/0,
    fun teardown_mecks/1,
    fun (_) ->
      ServerConf = test_server_conf(),
      [
        ?_assertEqual(
          {ok, #sql_response{cols = [], rows = [], rowCount = 0, duration = 0}},
          craterl_sql:sql_request(#sql_request{stmt = <<"select * from t">>, args=[]}, ServerConf)
        ),
        ?_assertEqual(
          {ok, #sql_bulk_response{cols = [], colTypes = [1,2], results = [], duration = 4}},
          craterl_sql:sql_request(#sql_bulk_request{stmt = <<"select * from t">>, bulk_args=[]}, ServerConf)
        ),
        ?_assertEqual(
          {ok, #sql_response{cols = [<<"a">>, <<"b">>], rows = [[true, 5.6], [false, -5.6]], rowCount=2, duration=12456}},
          craterl_sql:sql_request(#sql_request{stmt = <<"select * from t">>, args=[]}, ServerConf)
        ),
        ?_assertEqual(
          {error, invalid_json},
          craterl_sql:sql_request(#sql_request{stmt = <<"select * from t">>, args=[]}, ServerConf)
        ),
        ?_assertEqual(
          {error, #sql_error{code = 4000, message = <<"something went wrong">>}},
          craterl_sql:sql_request(#sql_request{stmt = <<"select * from t">>, args=[]}, ServerConf)
        ),
        ?_assertEqual(
          {error, no_content},
          craterl_sql:sql_request(#sql_request{stmt = <<"select * from t">>, args=[]}, ServerConf)
        ),
        ?_assertEqual(
          {error, timeout},
          craterl_sql:sql_request(#sql_request{stmt = <<"select * from t">>, args=[]}, ServerConf)
        ),
        ?_assertEqual(
          {error, closed},
          craterl_sql:sql_request(#sql_request{stmt = <<"select * from t">>, args=[]}, ServerConf)
        )

      ]
    end
  }
}.

create_payload_test_() ->
  ?_assertEqual(<<"{\"stmt\":\"select * from craty\",\"args\":[]}">>,
    craterl_sql:create_payload(<<"select * from craty">>, [])).

build_response_test() ->
  ?assertEqual(#sql_response{rows = [[1,2,3],[4,5,6]], cols=[<<"x">>, <<"y">>, <<"z">>], rowCount = 2, duration = 4},
    craterl_sql:build_response(<<"{\"rows\":[[1,2,3],[4,5,6]], \"cols\":[\"x\",\"y\",\"z\"], \"rowcount\":2, \"duration\":4}">>)
  ),
  ?assertEqual(#sql_response{},
    craterl_sql:build_response(<<"{\"some\":\"weird\", \"stuff\":1}">>)
  ).

build_error_response_test() ->
  ?assertEqual(#sql_error{code = 1000, message = <<"DAU Exception">>},
    craterl_sql:build_error_response(<<"{\"error\":{\"message\":\"DAU Exception\",\"code\":1000}}">>)
  ),
  ?assertEqual(#sql_error{}, craterl_sql:build_error_response(<<"{}">>)),
  ?assertEqual({error,invalid_json}, craterl_sql:build_error_response(<<"">>)),
  ?assertEqual({error,invalid_json}, craterl_sql:build_error_response(<<>>)).
