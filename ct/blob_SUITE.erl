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

-module(blob_SUITE).
-author("Matthias Wahl").

-include_lib("common_test/include/ct.hrl").

-include("craterl.hrl").

-export([all/0,
	 init_per_suite/1, end_per_suite/1,
	 init_per_testcase/2, end_per_testcase/2]).

-export([
  blob_put_small_file_roundtrip_test/1,
  blob_put_small_roundtrip_test/1,
  blob_put_big_file_roundtrip_test/1,
  blob_put_big_roundtrip_test/1
]).

all() ->
  [
    blob_put_small_file_roundtrip_test,
    blob_put_small_roundtrip_test,
    blob_put_big_file_roundtrip_test,
    blob_put_big_roundtrip_test
  ].

init_per_suite(Config) ->
  ok = craterl:start(),
  ClientRef = craterl:new([{<<"localhost">>, 48200}, {<<"localhost">>, 48201}]),
  [{client, ClientRef} | Config].

end_per_suite(Config) ->
  ClientRef = ct:get_config(client),
  craterl_gen_server:stop(ClientRef),
  application:stop(craterl),
  Config.

init_per_testcase(_, Config) ->
  PrivDir = ?config(priv_dir, Config),

  SmallBlobFile = filename:join(PrivDir, "small_blob"),
  ok = file:write_file(SmallBlobFile, <<"content">>),

  BigBlobFile = filename:join(PrivDir, "big_blob"),
  ok =  file:write_file(BigBlobFile, <<0:5194304/unit:8>>),

  BlobTable = <<"mytable">>,
  craterl:sql(list_to_binary([<<"drop blob table ">>, BlobTable])),
  craterl:sql(list_to_binary([<<"create blob table ">>, BlobTable, <<" with (number_of_replicas=0)">>])),
  ct_helpers:wait_for_green_state(<<"localhost:48200">>),
  [{big_blob, BigBlobFile},{small_blob, SmallBlobFile}, {table, BlobTable}] ++ Config.


end_per_testcase(_, Config) ->
  file:delete(?config(big_blob, Config)),
  file:delete(?config(small_blob, Config)),
  Config.

blob_put_small_file_roundtrip_test(Config) ->
  ClientRef = ?config(client, Config),
  Table = ?config(table, Config),
  SmallBlobFile = ?config(small_blob, Config),
  {ok, Hash} = craterl_hash:sha1HexFile(SmallBlobFile),
  {error, 404} = craterl:blob_exists(ClientRef, Table, Hash),
  {ok, {created, Hash}} = craterl:blob_put_file(Table, SmallBlobFile),
  ok = craterl:blob_exists(ClientRef, Table, Hash),
  {ok, DataFun} = craterl:blob_get(Table, Hash),
  {ok, <<"content">>} = DataFun(),
  {ok, done} = DataFun(),
  ok = craterl:blob_delete(Table, Hash),
  {error, 404} = craterl:blob_exists(ClientRef, Table, Hash),
  {error, 404} = craterl:blob_delete(Table, Hash).


blob_put_big_file_roundtrip_test(Config) ->
  ClientRef = ?config(client, Config),
  Table = ?config(table, Config),
  BigBlobFile = ?config(big_blob, Config),
  {ok, Hash} = craterl_hash:sha1HexFile(BigBlobFile),
  {error, 404} = craterl:blob_exists(ClientRef, Table, Hash),
  {ok, {created, Hash}} = craterl:blob_put_file(Table, BigBlobFile),
  ok = craterl:blob_exists(ClientRef, Table, Hash),
  {ok, DataFun} = craterl:blob_get(Table, Hash),
  {ok, FileContent} = file:read_file(BigBlobFile),
  {BlobContent, NumCalls} = ct_helpers:get_blob_content(DataFun),
  BlobContent = FileContent,
  NumCalls = 3558,
  ok = craterl:blob_delete(Table, Hash),
  {error, 404} = craterl:blob_exists(ClientRef, Table, Hash),
  {error, 404} = craterl:blob_delete(Table, Hash).


blob_put_small_roundtrip_test(Config) ->
  ClientRef = ?config(client, Config),
  Table = ?config(table, Config),
  BlobContent = crypto:rand_bytes(10),
  {ok, Hash} = craterl_hash:sha1Hex(BlobContent),
  {error, 404} = craterl:blob_exists(ClientRef, Table, Hash),
  {ok, {created, Hash}} = craterl:blob_put(Table, BlobContent),
  ok = craterl:blob_exists(ClientRef, Table, Hash),
  {ok, DataFun} = craterl:blob_get(Table, Hash),
  {ok, BlobContent} = DataFun(),
  {ok, done} = DataFun(),
  ok = craterl:blob_delete(Table, Hash),
  {error, 404} = craterl:blob_exists(ClientRef, Table, Hash),
  {error, 404} = craterl:blob_delete(Table, Hash).


blob_put_big_roundtrip_test(Config) ->
  ClientRef = ?config(client, Config),
  Table = ?config(table, Config),
  BlobContent = crypto:rand_bytes(5194304),
  {ok, Hash} = craterl_hash:sha1Hex(BlobContent),
  {error, 404} = craterl:blob_exists(ClientRef, Table, Hash),
  {ok, {created, Hash}} = craterl:blob_put(Table, BlobContent),
  ok = craterl:blob_exists(ClientRef, Table, Hash),
  {ok, DataFun} = craterl:blob_get(Table, Hash),
  ct_helpers:validate_blob_content(DataFun, BlobContent, 3558),
  ok = craterl:blob_delete(Table, Hash),
  {error, 404} = craterl:blob_exists(ClientRef, Table, Hash),
  {error, 404} = craterl:blob_delete(Table, Hash).

%% TODO: test craterl:blob_put_file
