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
-module(craterl_blob_tests).
-author("mat").

-include("craterl.hrl").
-include_lib("eunit/include/eunit.hrl").

setup_blob() ->
  meck:new(hackney),
  meck:expect(hackney, request,
    fun(_Method, _Url, _Headers, _Body, _Options) ->
      {ok, 200, [], 2}
    end
  ),
  meck:expect(hackney, stream_body, 1,
    meck:seq([{ok, <<"content">>}, done])
  ),
  ok.

teardown_mecks(_) ->
  meck:validate(hackney),
  meck:unload(hackney).

setup_get_blob_to_file() ->
  setup_blob(),
  integer_to_list(erlang:phash2(make_ref())).

teardown_get_blob_to_file(FileName) ->
  teardown_mecks(nothing),
  file:delete(FileName).

setup_blob_put() ->
  meck:new(hackney),
  meck:expect(hackney, request, 5,
    meck:seq([
      {ok, make_ref()},
      {ok, make_ref()},
      {error, closed},
      {ok, make_ref()}
    ])),
  meck:expect(hackney, send_body, 2,
    meck:seq([
      ok,
      ok,
      ok
    ])),
  meck:expect(hackney, start_response, 1,
    meck:seq([
      {ok, 201, [], make_ref()},
      {ok, 409, [], make_ref()},
      {ok, 404, [], make_ref()}
    ])),
  ok.


blob_put_test_() ->
  {
    "test putting blobs to the server",
    {
      setup,
      fun setup_blob_put/0,
      fun teardown_mecks/1,
      fun (_) ->
        [
          ?_assertEqual(
              {ok, {created, <<"123456">>}},
              craterl_blob:blob_put({<<"localhost">>, 4300}, <<"myblobs">>, <<"123456">>, {data, <<"foobar">>})
          ),
          ?_assertEqual(
              {error, {already_exists, <<"123456">>}},
              craterl_blob:blob_put({<<"localhost">>, 4300}, <<"myblobs">>, <<"123456">>, {data, <<"foobar">>})
          ),
          ?_assertEqual(
            {error, closed},
            craterl_blob:blob_put({<<"localhost">>, 4300}, <<"myblobs">>, <<"1234567">>, {data, <<"foobar">>})
          ),
          ?_assertEqual(
            {error, {not_found, <<"noblobs">>}},
            craterl_blob:blob_put({<<"localhost">>, 4300}, <<"noblobs">>, <<"1234567">>, {data, <<"foobar">>})
          )
        ]
      end
    }
  }.

blob_get_to_mem_test_() ->
  {
    "test getting blob to mem",
    {
      setup,
      fun setup_blob/0,
      fun teardown_mecks/1,
      fun (_) ->
        {ok, GetDataFun} = craterl_blob:blob_get_to_mem({<<"localhost">>, 4200}, <<"myblobs">>, <<"123456">>),
        {ok, FirstResult} = GetDataFun(),
        {ok, SecondResult} = GetDataFun(),
        [
          ?_assertEqual(<<"content">>, FirstResult),
          ?_assertEqual(done, SecondResult)
        ]
      end
    }
  }.

blob_get_to_file_test_() ->
{
  "testing getting blob to file",
  {
    setup,
    fun setup_get_blob_to_file/0,
    fun teardown_get_blob_to_file/1,
    fun (FilePath) ->
      {ok, FilePath} = craterl_blob:blob_get_to_file({<<"localhost">>, 4200}, <<"myblobs">>, <<"123456">>, FilePath),
      {ok, Content} = file:read_file(FilePath),
      io:format("~p~n", [Content]),
      [
        ?_assertEqual(<<"content">>, Content)
      ]
    end
  }
}.

blob_exists_test_() ->
{
  "testing if a blob exists",
  {
    setup,
    fun setup_blob/0,
    fun teardown_mecks/1,
    fun (_) ->
      [
        ?_assertEqual({ok, exists}, craterl_blob:blob_exists({<<"localhost">>, 4200}, <<"myblobs">>, <<"123456">>))
      ]
    end
  }
}.

blob_delete_test_() ->
{
  "testing if the blob_delete function executes correctly with sane arguments",
  {
    setup,
    fun setup_blob/0,
    fun teardown_mecks/1,
    fun (_) ->
      [
        ?_assertEqual({ok, deleted}, craterl_blob:blob_delete({<<"localhost">>, 4200}, <<"myblobs">>, <<"123456">>))
      ]
    end
  }
}.

setup_blob_request() ->
  meck:new(hackney),
  meck:expect(hackney, request, 5,
    meck:seq([
      {ok, 200, [], make_ref()},
      {ok, 200, [], make_ref()},
      {ok, 200, [], make_ref()},
      {ok, make_ref()},
      {ok, make_ref()},
      {error, bla},
      {ok, 200, [], make_ref()}
    ])
  ),
  meck:expect(hackney, stream_body, 1,
    meck:seq([
      {ok, <<"content">>}, done,
      {ok, <<"content">>}, done
    ])
  ),
  meck:expect(hackney, send_body, 2, meck:seq([
    ok,
    ok,
    {error, bla}
  ])),
  meck:expect(hackney, start_response, 1, meck:seq([
    {ok, 201, [], make_ref()},
    {ok, 201, [], make_ref()}
  ])),
  ok,
  integer_to_list(erlang:phash2(make_ref())).

blob_request_test_() ->
{
  "testing if starting the generic server with a blob request works correctly",
  {
    setup,
    fun setup_blob_request/0,
    fun teardown_mecks/1,
    fun (FilePath) ->
      {ok, DataFun} = craterl_blob:blob_request(
                        #blob_request{method=get, table= <<"myblobs">>, digest= <<"123456">>},
                        {<<"localhost">>, 4200}
      ),
      {ok, CreatedPath} = craterl_blob:blob_request(
          #blob_request{method=get, table= <<"myblobs">>, digest= <<"123456">>, payload = {file, FilePath}},
          {<<"localhost">>, 4200}
      ),
      [
        ?_assertEqual({ok, <<"content">>}, DataFun()),
        ?_assertEqual({ok, done}, DataFun()),
        ?_assertEqual({ok, <<"content">>}, file:read_file(CreatedPath)),
        ?_assertEqual({ok, exists}, craterl_blob:blob_request(
          #blob_request{method=head, table= <<"myblobs">>, digest= <<"123456">>},
          {<<"localhost">>, 4200}
        )),
        ?_assertEqual({ok, {created, <<"123456">>}}, craterl_blob:blob_request(
          #blob_request{method=put, table = <<"myblobs">>, digest = <<"123456">>,
            payload = {data, <<"content">>}
          }, {<<"localhost">>, 4200})),
        ?_assertEqual({ok, {created, <<"123456">>}}, craterl_blob:blob_request(
          #blob_request{method=put, table = <<"myblobs">>, digest = <<"123456">>,
            payload = {file, FilePath}}, {<<"localhost">>, 4200})),
        ?_assertEqual({error, bla}, craterl_blob:blob_request(
          #blob_request{method=put, table = <<"myblobs">>, digest = <<"123456">>,
            payload = <<"content">>
          }, {<<"localhost">>, 9200})),
        ?_assertEqual({ok, deleted}, craterl_blob:blob_request(
          #blob_request{method=delete, table = <<"myblobs">>, digest = <<"123456">>},
          {<<"localhost">>, 9200})
        ),
        ?_assertEqual({error, unsupported},
          craterl_blob:blob_request(
            #blob_request{method=poop,table = <<"myblobs">>, digest = <<"123456">>},
            {<<"localhost">>, 1234})
        )
      ]
    end
  }
}.
