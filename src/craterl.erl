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
  blob_get/2, blob_get_to_file/3,
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
    Request = #sql_request{stmt=Stmt, args=Args},
    SuccessFun = fun
      (SqlResponse = #sql_response{}) -> {ok, instrument(SqlResponse)};
      (Response) -> {error, invalid_response, Response}
    end,
    request(Request, SuccessFun);
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

request(Request, SuccessFun) when is_function(SuccessFun) ->
  case connection_manager:get_server() of
      none_active ->
          {error, "No active server"};
      {ok, Server} ->
        case crate_request_handler_sup:request(Request, Server, self()) of
          {ok, ChildPid} ->
            receive
                {ChildPid, {ok, Response}} ->
                    SuccessFun(Response);
                {ChildPid, {error, econnrefused}} ->
                    lager:info("request/econnrefused: ~p~n", [Server]),
                    connection_manager:add_inactive(Server),
                    request(Request, SuccessFun);
                {ChildPid, {error, OtherError}} ->
                    lager:info("request/error: ~p~n", [OtherError]),
                    {error, OtherError};
                {ChildPid, Other} ->
                    lager:error("request/other: ~p", [Other])
            end;
          {error, Reason} -> {error, Reason}
        end
  end.


blob_get(BlobTable, HexDigest) ->
  Request = #blob_request{
               method=get,
               table=BlobTable,
               digest=HexDigest},
  SuccessFun = fun
    (GetDataFun) when is_function(GetDataFun) ->
      {ok, GetDataFun};
    (Response) -> {error, invalid_response, Response}
  end,
  request(Request, SuccessFun).

blob_get_to_file(BlobTable, HexDigest, FilePath) ->
  Request = #blob_request{
               method=get,
               table=BlobTable,
               digest=HexDigest,
               payload={file, FilePath}},
  SuccessFun = fun
    (ResultFilePath) when is_binary(ResultFilePath) -> {ok, ResultFilePath};
    (Response) -> {error, invalid_response, Response}
  end,
  request(Request, SuccessFun).

blob_exists(BlobTable, HexDigest) ->
  Request = #blob_request{
               method=head,
               table=BlobTable,
               digest=HexDigest},
  SuccessFun = fun
    (ResultFilePath) when is_binary(ResultFilePath) -> {ok, ResultFilePath};
    (Response) -> {error, invalid_response, Response}
  end,
  request(Request, SuccessFun).

blob_put(BlobTable, Content) ->
  case craterl_hash:sha1Hex(Content) of
    {ok, HexDigest} ->
      send_blob(BlobTable, HexDigest, {data, Content});
    {error, Reason} -> {error, Reason}
  end.

blob_put_file(BlobTable, FilePath) ->
  case craterl_hash:sha1HexFile(FilePath) of
    {ok, HexDigest} ->
      send_blob(BlobTable, HexDigest, {file, FilePath});
    {error, Reason} -> {error, Reason}
  end.

send_blob(BlobTable, HexDigest, Payload) ->
  Request = #blob_request{
               method=put,
               table=BlobTable,
               digest=HexDigest,
               payload=Payload},
  SuccessFun = fun
    ({created, Digest}) -> {ok, created, Digest};
    (Response) -> {error, invalid_response, Response}
  end,
  request(Request, SuccessFun).

