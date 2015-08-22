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
%%% 
%%% @author Matthias Wahl
%%%
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(craterl_resp_tests).
-author("Matthias Wahl").

-include("../src/craterl_priv.hrl").
-include_lib("eunit/include/eunit.hrl").

column_test_() ->
  Response = #sql_response{
  rows=[[1, <<"a">>, 3.5], [2, <<"b">>, 4.6], [3, <<"c">>, 5.7]],
    rowCount = 3,
    cols = [<<"id">>, <<"name">>, <<"score">>],
    colTypes = [1, 2, 3],
    duration = 10
  },
  {
    "testing that extracting a column from a response works as expected",
    [
      ?_assertEqual(
        [1, 2, 3],
        craterl_resp:column(Response, 1)
      ),
      ?_assertEqual(
        [<<"a">>, <<"b">>, <<"c">>],
        craterl_resp:column(Response, 2)
      ),
      ?_assertEqual(
        [3.5, 4.6, 5.7],
        craterl_resp:column(Response, 3)
      ),
      ?_assertError(
        function_clause, craterl_resp:column(Response, 4)
      ),
      ?_assertEqual(
        [],
        craterl_resp:column(#sql_response{rows = []}, 1)
      )
    ]
  }.
