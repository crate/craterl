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

-module(craterl_url_tests).
-author("Matthias Wahl").

-include("craterl.hrl").
-include_lib("eunit/include/eunit.hrl").

server_uri_from_spec_test_() ->
  {
    "create a server url from a spec",
    [
      ?_assertEqual(
        <<"http://localhost:4200/_sql?types">>,
        craterl_url:create_server_url({<<"localhost">>, 4200}, true)
      ),
      ?_assertEqual(
        <<"http://localhost:4200/_sql">>,
        craterl_url:create_server_url({<<"localhost">>, 4200}, false)
      ),
      ?_assertEqual(
        <<"http://localhost:4200/_sql">>,
        craterl_url:create_server_url({<<"localhost">>, 4200}, <<"/_sql">>)),
      ?_assertEqual(<<"http://bla:123/_sql">>, craterl_url:create_server_url({<<"http://bla">>, 123}, <<"/_sql">>)),
      ?_assertEqual(<<"https://secure:4200/_sql">>, craterl_url:create_server_url({<<"https://secure">>,4200}, <<"/_sql">>))
    ]
  }.
