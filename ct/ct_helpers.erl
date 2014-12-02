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

-module(ct_helpers).
-author("Matthias Wahl").

%% API
-export([wait_for_green_state/1,
  get_blob_content/1,
  validate_blob_content/3
]).

wait_for_green_state(Host) ->
  {ok, 200, _Stuff, _Ref} = hackney:request(get, <<Host/binary, "/_cluster/health?wait_for_status=green">>).

get_blob_content(DataFun) ->
  get_blob_content(DataFun, [], 0).
get_blob_content(DataFun, Acc, Calls) ->
  case DataFun() of
    {ok, done} -> {list_to_binary(Acc), Calls};
    {ok, Content} when is_binary(Content) ->
      get_blob_content(DataFun, [Content|Acc], Calls+1)
  end.
validate_blob_content(DataFun, ExpectedContent, ExpectedCalls) ->
  validate_blob_content(DataFun, ExpectedContent, ExpectedCalls, 0, 0).
validate_blob_content(DataFun, ExpectedContent, ExpectedCalls, Calls, Pos) ->
  case DataFun() of
    {ok, done} -> ok;
    {ok, Content} ->
      ByteSize = byte_size(Content),
      ExpectedPart = binary:part(ExpectedContent, Pos, ByteSize),
      if Content /= ExpectedPart ->
        ct:print("Expected: ~p~n", [ExpectedPart]),
        ct:print("Got: ~p~n", [Content]);
        true -> ok
      end,
      validate_blob_content(DataFun, ExpectedContent, ExpectedCalls, Calls+1, Pos+ByteSize)
  end.
