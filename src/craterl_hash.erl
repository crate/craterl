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
-module(craterl_hash).
-author("mat").

-ifdef(TEST).
-compile(export_all).
-endif.

%% API
-export([sha1Hex/1, sha1HexFile/1, sha1HexFileData/1]).

-define(CHUNK_SIZE, 4194304).

-spec sha1Hex(Content::binary()) -> {ok, binary()}.
sha1Hex(Content) when is_binary(Content) ->
  Ctx = crypto:hash_init(sha),
  hashContent(Ctx, <<>>, Content).

hashContent(Ctx, Content, <<>>) ->
  NewCtx = crypto:hash_update(Ctx, Content),
  Digest = crypto:hash_final(NewCtx),
  {ok, bin_to_hex:hexstring(Digest)};
hashContent(Ctx, Content, Rest) ->
  NewCtx = crypto:hash_update(Ctx, Content),
  Bytes = byte_size(Rest),
  if
    Bytes =< ?CHUNK_SIZE -> %% TODO: find optimal chunk size
      hashContent(NewCtx, Rest, <<>>);
    true ->
      <<Chunk:Bytes/binary, NewRest/binary>> = Rest,
      hashContent(NewCtx, Chunk, NewRest)
  end.

%%
%% @doc hashing the file contents of the file at FilePath
%%
-spec sha1HexFile(FilePath::binary()|string()) -> {ok, binary()}|{error, term()}.
sha1HexFile(FilePath) ->
  case file:open(FilePath, [read, raw, binary]) of
    {ok, FileHandle} ->
      Ctx = crypto:hash_init(sha),
      Result = hashOpenFile(FileHandle, Ctx),
      file:close(FileHandle),
      Result;
    {error, Reason} ->
      {error, Reason}
  end.


%%
%% @doc hashing the file contents of the file at FilePath and return the file data too
%%
-spec sha1HexFileData(FilePath::binary()|string()) -> {ok, binary(), binary()} | {error, term()}.
sha1HexFileData(FilePath) ->
  case file:open(FilePath, [read, raw, binary]) of
    {ok, FileHandle} ->
      Ctx = crypto:hash_init(sha),
      Result = hashOpenFile(FileHandle, Ctx, <<>>),
      file:close(FileHandle),
      Result;
    {error, Reason} -> {error, Reason}
  end.

hashOpenFile(FileHandle, HashCtx) ->
  case file:read(FileHandle, ?CHUNK_SIZE) of
    {ok, Data} ->
      NewHashCtx = crypto:hash_update(HashCtx, Data),
      hashOpenFile(FileHandle, NewHashCtx);
    eof ->
      Digest = crypto:hash_final(HashCtx),
      {ok, bin_to_hex:hexstring(Digest)};
    {error, Reason} ->
      {error, Reason}
  end.

hashOpenFile(FileHandle, HashCtx, DataAcc) when is_binary(DataAcc) ->
  case file:read(FileHandle, ?CHUNK_SIZE) of
    {ok, Data} ->
      NewHashCtx = crypto:hash_update(HashCtx, Data),
      hashOpenFile(FileHandle, NewHashCtx, <<DataAcc/binary, Data/binary>>);
    eof ->
      Digest = crypto:hash_final(HashCtx),
      {ok, bin_to_hex:hexstring(Digest), DataAcc};
    {error, Reason} ->
      {error, Reason}
  end.

