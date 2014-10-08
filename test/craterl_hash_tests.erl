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

-module(craterl_hash_tests).
-author("Matthias Wahl").

-include_lib("eunit/include/eunit.hrl").

-define(SHA1_FILE_SETUP(TestFun), {setup, fun create_file/0, fun delete_file/1, TestFun}).
-define(SHA1_BIG_FILE_SETUP(TestFun), {setup, fun create_big_file/0, fun delete_file/1, TestFun}).


%%% SETUP/TEARDOWN
create_file() ->
  FileName = integer_to_list(erlang:phash2(make_ref())),
  file:write_file(FileName,<<0:8/unit:8>>),
  FileName.

create_big_file()->
  FileName = integer_to_list(erlang:phash2(make_ref())),
  file:write_file(FileName, <<0:5194304/unit:8>>),
  FileName.

delete_file(FileName) ->
  file:delete(FileName).

%%% TESTS

sha1_test_() ->
  {"test generating sha1 hex digests from binary content",
  [
    ?_assertEqual({ok, <<"da39a3ee5e6b4b0d3255bfef95601890afd80709">>}, craterl_hash:sha1Hex(<<"">>)),
    ?_assertEqual({ok, <<"1260ebb2e483a13c9d03bc9e07ea92b07ccd26df">>}, craterl_hash:sha1Hex(integer_to_binary(65536))),
    ?_assertEqual({ok, <<"238a131a3e8eb98d1fc5b27d882ca40b7618fd2a">>}, craterl_hash:sha1Hex(<<"CONTENT">>)),
    ?_assertEqual({ok, <<"2f03aeaf5bec7672e5c20319c78ef0eb9a1cbb8f">>}, craterl_hash:sha1Hex(<<0:5194304/unit:8>>))
  ]
  }.

sha1_file_test_() ->
  {"test sha1 generation from a given filepath",
    ?SHA1_FILE_SETUP(
      fun (FilePath) ->
        [
          ?_assertEqual({ok,<<"05fe405753166f125559e7c9ac558654f107c7e9">>,<<0:8/unit:8>>}, craterl_hash:sha1HexFileData(FilePath)),
          ?_assertEqual({ok,<<"05fe405753166f125559e7c9ac558654f107c7e9">>}, craterl_hash:sha1HexFile(FilePath)),
          ?_assertEqual({error, enoent}, craterl_hash:sha1HexFile("/non-existent-file")),
          ?_assertEqual({error, enoent}, craterl_hash:sha1HexFileData("/non-existing-file"))
        ]
      end
    )
  }.

sha1_big_file_test_() ->
  {"test sha1 generation from a big file",
    ?SHA1_BIG_FILE_SETUP(
      fun (FileName) ->
        [
          ?_assertEqual({ok, <<"2f03aeaf5bec7672e5c20319c78ef0eb9a1cbb8f">>}, craterl_hash:sha1HexFile(FileName)),
          ?_assertEqual({ok, <<"2f03aeaf5bec7672e5c20319c78ef0eb9a1cbb8f">>, <<0:5194304/unit:8>>}, craterl_hash:sha1HexFileData(FileName))
        ]
      end
    )
  }.
