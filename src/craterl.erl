%%%-------------------------------------------------------------------
%%% @author Peter Sabaini <peter@sabaini.at>
%%% @copyright (C) 2014, Peter Sabaini
%%% @doc 
%%%
%%% @end
%%% Created :  9 Apr 2014 by Peter Sabaini <peter@sabaini.at>
%%%-------------------------------------------------------------------
-module(craterl).

-include("craterl.hrl").
-compile([{parse_transform, lager_transform}]).


%% API
-export([sql/1, sql/2,
  set_servers/1,
  blob_put/2, blob_put_file/2,
  start/0]).

start() ->
  application:ensure_all_started(jsx),
  application:ensure_all_started(hackney),
  application:ensure_all_started(lager),
  application:start(craterl).

set_servers(Servers) ->
    connection_manager:set_servers(Servers).

sql(Stmt) when is_binary(Stmt) ->
    sql(Stmt, []);
sql(Stmt) when is_list(Stmt) ->
    sql(list_to_binary(Stmt), []);
sql(_) -> {error, unsupported}.

sql(Stmt, Args) when is_list(Stmt) and is_list(Args) ->
    sql(list_to_binary(Stmt), Args);

sql(Stmt, Args) when is_binary(Stmt) and is_list(Args) ->
    statistics(runtime),
    statistics(wall_clock),
    case connection_manager:get_server() of
        none_active ->
            {error, "No active server"};
        {ok, Server} ->
            {ok, ChildPid} = crate_request_handler_sup:request(
                               #sql_request{stmt=Stmt, args=Args}, Server, self()),
            receive
                {ChildPid, {ok, SqlResponse}} ->
                    {ok, instrument(SqlResponse)};
                {ChildPid, {error, econnrefused}} ->
                    lager:info("sql/econnrefused: ~p~n", [Server]),
                    connection_manager:add_inactive(Server),
                    sql(Stmt, Args);
                {ChildPid, {error, OtherError}} ->
                    lager:info("sql/error other: ~p~n", [OtherError]),
                    {error, OtherError};
                Other ->
                    lager:error("sql/other: ~p~n", [Other]),
                    {error, Other}
            end
    end;
sql(_, _) -> {error, unsupported}.

instrument(#sql_response{
              cols=Cols, rows=Rows,
              rowCount=RowCnt,
              duration=Duration,
              wallclock=_, runtime=_
             }) ->   
    {_, RunTime} = statistics(runtime),
    {_, WallClock} = statistics(wall_clock),
    #sql_response{
       cols=Cols, rows=Rows, 
       rowCount=RowCnt, 
       duration=Duration,
       wallclock=WallClock,
       runtime=RunTime
      }.


blob_put(BlobTable, Content) ->
  % TODO: do not use file interface as it migth double used ram
  case ramFileGetHashAndData(Content) of
    {ok, HexDigest, FileData} ->
      send_blob(BlobTable, HexDigest, FileData);
    {error, Reason} -> {error, Reason}
  end.

blob_put_file(BlobTable, FilePath) ->
  case fileGetHashAndData(FilePath) of
    {ok, HexDigest, FileData} ->
      send_blob(BlobTable, HexDigest, FileData);
    {error, Reason} -> {error, Reason}
  end.

send_blob(BlobTable, HexDigest, FileData) ->
  case connection_manager:get_server() of
      none_active ->
          {error, "No active server"};
      {ok, Server} ->
          {ok, ChildPid} = crate_request_handler_sup:request(
                             #blob_request{
                               method=put,
                               table=BlobTable,
                               digest=HexDigest,
                               data=FileData},
                             Server, self()),
          receive
              {ChildPid, {ok, created, Digest}} ->
                  {ok, created, Digest};
              {ChildPid, {error, econnrefused}} ->
                  connection_manager:add_inactive(Server),
                  send_blob(BlobTable, HexDigest, FileData);
              {ChildPid, {error, Reason}} ->
                {error, Reason};
              {ChildPid, {error, Reason, Digest}} ->
                {error, Reason, Digest};
              {ChildPid, Other} ->
                  io:format("blob/other: ~p~n", [Other])
          end
  end.

%% hash binary Content as if it was a file
ramFileGetHashAndData(Content) ->
  case file:open(Content, [read, ram, binary]) of
    {ok, FileHandle} ->
      Ctx = crypto:hash_init(sha),
      Result = hashOpenFile(FileHandle, Ctx, <<>>),
      file:close(FileHandle),
      Result;
    {error, Reason} -> {error, Reason}
  end.

fileGetHashAndData(FilePath) ->
  case file:open(FilePath, [read, raw, binary]) of
    {ok, FileHandle} ->
      Ctx = crypto:hash_init(sha),
      Result = hashOpenFile(FileHandle, Ctx, <<>>),
      file:close(FileHandle),
      Result;
    {error, Reason} -> {error, Reason}
  end.

hashOpenFile(FileHandle, HashCtx, DataAcc) when is_binary(DataAcc)->
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

