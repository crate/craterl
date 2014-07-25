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
-module(crate_request_handler_tests).
-author("mat").

-include("craterl.hrl").
-include_lib("eunit/include/eunit.hrl").

simple_test() ->
  ?assert(true).

server_uri_from_spec_test() ->
  ?assertEqual(
    <<"http://localhost:4200/_sql">>,
    crate_request_handler:create_server_url(<<"localhost:4200">>, <<"/_sql">>)),
  ?assertEqual(<<"http://bla:123/_sql">>, crate_request_handler:create_server_url(<<"http://bla:123">>, <<"/_sql">>)),
  ?assertEqual(<<"https://secure:4200/_sql">>, crate_request_handler:create_server_url(<<"https://secure">>, <<"/_sql">>))
.

create_payload_test() ->
  ?assertEqual(<<"{\"stmt\":\"select * from craty\",\"args\":[]}">>,
    crate_request_handler:create_payload(<<"select * from craty">>, [])).

build_response_test() ->
  ?assertEqual(#sql_response{rows = [[1,2,3],[4,5,6]], cols=[<<"x">>, <<"y">>, <<"z">>], rowCount = 2, duration = 4},
    crate_request_handler:build_response(<<"{\"rows\":[[1,2,3],[4,5,6]], \"cols\":[\"x\",\"y\",\"z\"], \"rowcount\":2, \"duration\":4}">>)
  ),
  ?assertEqual(#sql_response{},
    crate_request_handler:build_response(<<"{\"some\":\"weird\", \"stuff\":1}">>)
  ).

build_error_response_test() ->
  ?assertEqual(#sql_error{code = 1000, message = <<"DAU Exception">>},
    crate_request_handler:build_error_response(<<"{\"error\":{\"message\":\"DAU Exception\",\"code\":1000}}">>)
  ),
  ?assertEqual(#sql_error{}, crate_request_handler:build_error_response(<<"{}">>)).
