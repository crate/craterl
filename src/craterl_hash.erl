%%%-------------------------------------------------------------------
%%% @author mat
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Apr 2014 9:06 AM
%%%-------------------------------------------------------------------
-module(craterl_hash).
-author("mat").

%% API
-export([sha1Hex/1, sha1HexFile/1, sha1HexFileData/1]).

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
    Bytes =< 4194304 -> %% TODO: find optimal chunk size
      hashContent(NewCtx, Rest, <<>>);
    true ->
      <<Chunk:Bytes, NewRest/binary>> = Content,
      hashContent(NewCtx, Chunk, NewRest)
  end.

sha1HexFile(FilePath) ->
  case file:open(FilePath, [read, raw, binary]) of
    {ok, FileHandle} ->
      Ctx = crypto:hash_init(sha),
      Result = hashOpenFile(FileHandle, Ctx),
      file:close(FileHandle),
      Result;
    {error, Reason} -> {error, Reason}
  end.

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
  case file:read(FileHandle, 4194304) of
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
  case file:read(FileHandle, 4194304) of
    {ok, Data} ->
      NewHashCtx = crypto:hash_update(HashCtx, Data),
      hashOpenFile(FileHandle, NewHashCtx, <<DataAcc/binary, Data/binary>>);
    eof ->
      Digest = crypto:hash_final(HashCtx),
      {ok, bin_to_hex:hexstring(Digest), DataAcc};
    {error, Reason} ->
      {error, Reason}
  end.

