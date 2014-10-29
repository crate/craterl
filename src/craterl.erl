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

-module(craterl).

-include("craterl.hrl").
-compile([{parse_transform, lager_transform}]).

%% API
-export([
  start/0,
  new/0, new/1, new/2, new/3,
  stop_client/1,
  sql/1, sql/2, sql/3, sql/4,
  sql_bulk/1, sql_bulk/2, sql_bulk/3, sql_bulk/4,
  blob_get/2, blob_get/3,
  blob_get_to_file/3, blob_get_to_file/4,
  blob_put/2, blob_put/3,
  blob_put_file/2, blob_put_file/3,
  blob_exists/2, blob_exists/3
  ]).


-define(DEFAULT_CLIENT_SPEC, {local, ?MODULE}).

start() ->
  start_deps(craterl, permanent).

start_deps(App, Type) ->
  case application:start(App, Type) of
    {error, {not_started, Dep}} ->
      start_deps(Dep, Type),
      start_deps(App, Type);
    {error, {already_started, _Dep}} -> ok;
    ok -> ok
  end.

-spec new() -> craterl_client_spec().
new() ->
  new(?DEFAULT_CLIENT_SPEC, [?CRATERL_DEFAULT_SERVER], []).

-spec new([craterl_server_spec()]) -> craterl_client_spec().
new(Servers) ->
  new(?DEFAULT_CLIENT_SPEC, Servers, []).

-spec new([craterl_server_spec()], [term()]) -> craterl_client_spec().
new(Servers, Options) when is_list(Options) ->
  new(?DEFAULT_CLIENT_SPEC, Servers, Options).

-spec new(ClientSpec:: craterl_client_spec(), Servers::[craterl_server_spec()], Options::[term()]) -> craterl_client_spec().
new(ClientSpec, Servers, Options) ->
  craterl_sup:start_client(ClientSpec, Servers, Options).

-spec stop_client(ClientName::atom()) -> ok | {error, term()}.
stop_client(ClientName) when is_atom(ClientName) ->
  craterl_sup:stop_client(ClientName).


%%--------------------------------------------------------------------
%% @doc
%% issue a SQL statement with optional arguments
%% or a prebuilt #sql_request{}
%%
%% @end
%%--------------------------------------------------------------------
-spec sql(binary()|string()|sql_request()) -> {ok, sql_response()}.
sql(Stmt) when is_binary(Stmt) ->
    sql(Stmt, []);
sql(Stmt) when is_list(Stmt) ->
    sql(list_to_binary(Stmt), []);
sql(Request = #sql_request{}) ->
    sql(?MODULE, Request).


%%--------------------------------------------------------------------
%% @doc
%% issue a SQL statement with optional arguments
%% or a prebuilt #sql_request{} to a specific client
%%
%% @end
%%--------------------------------------------------------------------
-spec sql(Stmt::binary()|string(), Args::list())      -> {ok, sql_response()};
         (ClientSpec::craterl_client_spec(), Request::sql_request()) -> {ok, sql_response()}.
sql(Stmt, Args) when is_list(Stmt) and is_list(Args) ->
    sql(list_to_binary(Stmt), Args, false);
sql(Stmt, Args) when is_binary(Stmt) and is_list(Args) ->
    sql(Stmt, Args, false);
sql(ClientSpec, Request = #sql_request{}) ->
   SuccessFun = fun
     (SqlResponse = #sql_response{}) -> {ok, SqlResponse};
     (Response) -> {error, {invalid_response, Response}}
   end,
   execute_request(ClientSpec, Request, SuccessFun).

%%--------------------------------------------------------------------
%% @doc
%% issue a SQL statement with optional arguments
%% and a boolean indicating whether you want to receive type information
%% for the returned columns.
%% @end
%%--------------------------------------------------------------------
-spec sql(Stmt::binary()|string(), Args::list(), IncludeTypes::boolean()) -> {ok, sql_response()}.
sql(Stmt, Args, IncludeTypes) when is_binary(Stmt) and is_list(Args) and is_boolean(IncludeTypes) ->
  sql(?MODULE, #sql_request{stmt=Stmt, args=Args, includeTypes = IncludeTypes}).

%%--------------------------------------------------------------------
%% @doc
%% issue a SQL statement with optional arguments
%% and a boolean indicating whether you want to receive type information
%% for the returned columns
%% to a specific client.
%% @end
%%--------------------------------------------------------------------
-spec sql(ClientSpec::atom(), Stmt::binary()|string(), Args::list(), IncludeTypes::boolean()) -> {ok, sql_response()}.
sql(ClientSpec, Stmt, Args, IncludeTypes) ->
   sql(ClientSpec, #sql_request{stmt=Stmt, args=Args, includeTypes = IncludeTypes}).


%%--------------------------------------------------------------------
%% @doc
%% issue a Bulk SQL statement using a #sql_bulk_request{}
%%
%% Bulk statements are only valid for INSERT/UPDATE and DELETE queries
%% @end
%%--------------------------------------------------------------------
-spec sql_bulk(BulkRequest::sql_bulk_request()) -> {ok, sql_bulk_response()} | {error, term()}.
sql_bulk(BulkRequest=#sql_bulk_request{}) ->
  sql_bulk(?MODULE, BulkRequest).

%%--------------------------------------------------------------------
%% @doc
%% issue a Bulk SQL statement using a #sql_bulk_request{}
%% to a specific client.
%% Or giving a binary or string statement and a list of bulk arguments
%% to the default client.
%%
%% @end
%%--------------------------------------------------------------------
-spec sql_bulk(ClientName::atom(), BulkRequest::sql_bulk_request()) -> {ok, sql_bulk_response()} | {error, term()};
    (Stmt::binary()|string(), [[term()]]) -> {ok, sql_bulk_response()} | {error, term()}.
sql_bulk(ClientName, BulkRequest=#sql_bulk_request{}) ->
  SuccessFun = fun
    (SqlBulkResponse = #sql_bulk_response{}) -> {ok, SqlBulkResponse};
    (Response) -> {error, {invalid_response, Response}}
  end,
  execute_request(ClientName, BulkRequest, SuccessFun);
sql_bulk(Stmt, BulkArgs) when is_list(Stmt) ->
  sql_bulk(list_to_binary(Stmt), BulkArgs);
sql_bulk(Stmt, BulkArgs) when is_binary(Stmt) ->
  sql_bulk(?MODULE, Stmt, BulkArgs, false).

%%--------------------------------------------------------------------
%% @doc
%% issue a Bulk SQL statement with bulk arguments and
%% a boolean that determines if the response should contain
%% column types or not.
%% @end
%%--------------------------------------------------------------------
sql_bulk(Stmt, BulkArgs, IncludeTypes) when is_binary(Stmt) ->
  sql_bulk(?MODULE, Stmt, BulkArgs, IncludeTypes).

%%--------------------------------------------------------------------
%% @doc
%% issue a Bulk SQL statement with bulk arguments and
%% a boolean that determines if the response should contain
%% column types or not
%% to a specifi client.
%% @end
%%--------------------------------------------------------------------
-spec sql_bulk(ClientName::atom(), Stmt::binary(), BulkArgs::[[term()]], IncludeTypes::boolean()) -> {ok, sql_bulk_response()} | {error, term()}.
sql_bulk(ClientName, Stmt, BulkArgs, IncludeTypes)->
  sql_bulk(ClientName, #sql_bulk_request{stmt = Stmt, bulk_args = BulkArgs, includeTypes = IncludeTypes}).



-spec blob_get(binary(), binary()) -> {ok, term()}.
blob_get(BlobTable, HexDigest) ->
  blob_get(?MODULE, BlobTable, HexDigest).

-spec blob_get(ClientName::atom(), binary(), binary()) -> {ok, term()}.
blob_get(ClientName, BlobTable, HexDigest) ->
  Request = #blob_request{
               method=get,
               table=BlobTable,
               digest=HexDigest},
  SuccessFun = fun
    (GetDataFun) when is_function(GetDataFun) ->
      {ok, GetDataFun};
    (Response) -> {error, {invalid_response, Response}}
  end,
  execute_request(ClientName, Request, SuccessFun).


-spec blob_get_to_file(BlobTable::binary(), HexDigest::binary(), FilePath::binary()) -> {ok, binary()}.
blob_get_to_file(BlobTable, HexDigest, FilePath) ->
  blob_get_to_file(?MODULE, BlobTable, HexDigest, FilePath).

-spec blob_get_to_file(ClientName::atom(), BlobTable::binary(), HexDigest::binary(), FilePath::binary()) -> {ok, binary()}.
blob_get_to_file(ClientName, BlobTable, HexDigest, FilePath) ->
  Request = #blob_request{
               method=get,
               table=BlobTable,
               digest=HexDigest,
               payload={file, FilePath}},
  SuccessFun = fun
    (ResultFilePath) when is_binary(ResultFilePath) -> {ok, ResultFilePath};
    (Response) -> {error, {invalid_response, Response}}
  end,
  execute_request(ClientName, Request, SuccessFun).


-spec blob_exists(BlobTable::binary(), HexDigest::binary()) -> ok.
blob_exists(BlobTable, HexDigest) ->
  blob_exists(?MODULE, BlobTable, HexDigest).
-spec blob_exists(ClientName::atom(), BlobTable::binary(), HexDigest::binary()) -> ok.
blob_exists(ClientName, BlobTable, HexDigest) ->
  Request = #blob_request{
               method=head,
               table=BlobTable,
               digest=HexDigest},
  SuccessFun = fun(ok) -> ok end,
  execute_request(ClientName, Request, SuccessFun).


-spec blob_put(BlobTable::binary(), Content::binary()) -> {ok, {created, binary()}} | {error, term()}.
blob_put(BlobTable, Content) ->
  blob_put(?MODULE, BlobTable, Content).
-spec blob_put(ClientName::atom(), BlobTable::binary(), Content::binary()) -> {ok, {created, binary()}} | {error, term()}.
blob_put(ClientName, BlobTable, Content) ->
  case craterl_hash:sha1Hex(Content) of
    {ok, HexDigest} ->
      send_blob(ClientName, BlobTable, HexDigest, {data, Content})
  end.


-spec blob_put_file(BlobTable::binary(), FilePath::binary()) -> {ok, {created, binary()}} | {error, term()}.
blob_put_file(BlobTable, FilePath) ->
  blob_put_file(?MODULE, BlobTable, FilePath).

-spec blob_put_file(ClientName::atom(), BlobTable::binary(), FilePath::binary()) -> {ok, {created, binary()}} | {error, term()}.
blob_put_file(ClientName, BlobTable, FilePath) ->
  case craterl_hash:sha1HexFile(FilePath) of
    {ok, HexDigest} ->
      send_blob(ClientName, BlobTable, HexDigest, {file, FilePath});
    {error, Reason} -> {error, Reason}
  end.



%%% INTERNAL %%%

-spec send_blob(atom(), binary(), binary(), blob_payload()) -> {ok, created, binary()} | {error, term()}.
send_blob(ClientName, BlobTable, HexDigest, Payload) ->
  Request = #blob_request{
               method=put,
               table=BlobTable,
               digest=HexDigest,
               payload=Payload},
  SuccessFun = fun
    ({created, Digest}) -> {ok, {created, Digest}};
    (Response) -> {error, {invalid_response, Response}}
  end,
  execute_request(ClientName, Request, SuccessFun).

-spec execute_request(atom(), blob_request(),     fun()) -> ok | {ok, term()} | {error, term()};
                     (atom(), sql_request(),      fun()) -> {ok, sql_response()} | {error, term()};
                     (atom(), sql_bulk_request(), fun()) -> {ok, sql_bulk_response()} | {error, term()}.
execute_request(ClientName, Request, SuccessFun) when is_function(SuccessFun) ->
  case craterl_gen_server:get_server(ClientName) of
      none_active ->
          {error, "No active server"};
      {ok, Server} ->
        case execute_request_on_server(Request, Server, SuccessFun) of
          {error, SqlError=#sql_error{}} ->
            {error, SqlError};
          {error, Reason} ->
            craterl_gen_server:add_inactive(ClientName, Server),
            {error, Reason};
          Response -> Response
        end
  end.

-spec execute_request_on_server(sql_request(), craterl_server_spec(), fun()) -> {ok, sql_response()} | {error, term()};
                               (sql_bulk_request(), craterl_server_spec(), fun()) -> {ok, sql_bulk_response()} | {error, term()};
                               (blob_request(), craterl_server_spec(), fun()) -> ok|{ok, term()} | {error, term()}.
execute_request_on_server(Request=#sql_request{}, Server, SuccessFun) ->
  case craterl_sql:sql_request(Request, Server) of
    {ok, Response} -> SuccessFun(Response);
    Other -> Other
  end;
execute_request_on_server(Request=#sql_bulk_request{}, Server, SuccessFun) ->
  case craterl_sql:sql_request(Request, Server) of
    {ok, Response} -> SuccessFun(Response);
    Other -> Other
  end;
execute_request_on_server(Request=#blob_request{}, Server, SuccessFun) ->
  case craterl_blob:blob_request(Request, Server) of
    {ok, Response} -> SuccessFun(Response);
    Other -> Other
  end.
