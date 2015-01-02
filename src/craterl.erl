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
  blob_delete/2, blob_delete/3,
  blob_get/2, blob_get/3,
  blob_get_to_file/3, blob_get_to_file/4,
  blob_put/2, blob_put/3,
  blob_put_file/2, blob_put_file/3,
  blob_exists/2, blob_exists/3
  ]).


-define(DEFAULT_CLIENT_SPEC, {local, ?MODULE}).

%%--------------------------------------------------------------------
%% @doc
%% start the craterl application and all its dependencies
%%
%% @end
%%--------------------------------------------------------------------
start() ->
  start_deps(craterl, permanent).

start_deps(App, Type) ->
  case application:start(App, Type) of
    {error, {not_started, Dep}} ->
      lager:info("~p not started~n", [Dep]),
      start_deps(Dep, Type),
      start_deps(App, Type);
    {error, {already_started, _Dep}} -> ok;
    ok -> ok
  end.

%%--------------------------------------------------------------------
%% @doc
%% create a new crate client with the default settings and
%% registration name
%%
%% @end
%%--------------------------------------------------------------------
-spec new() -> atom().
new() ->
  new(?DEFAULT_CLIENT_SPEC, [?CRATERL_DEFAULT_SERVER], []).

%%--------------------------------------------------------------------
%% @doc
%% start a new craterl client instance with the default name craterl
%% given a list of crate server instances.
%%
%% Example:
%% <pre>
%% craterl = new([{&lt;&lt;"192.168.0.1"&gt;&gt;, 4200}, {&lt;&lt;"my.hostname"&lt;&lt;, 44200}]).
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec new([craterl_server_spec()]) -> atom().
new(Servers) ->
  new(?DEFAULT_CLIENT_SPEC, Servers, []).

%%--------------------------------------------------------------------
%% @doc
%% create a new craterl client instance with the default name craterl
%% given a list of crate server instances
%% and a list of options as a proplist.
%%
%% Example:
%% <pre>
%% craterl = new([{&lt;&lt;"192.168.0.1"&gt;&gt;, 4200}], [{poolsize, 100], {timeout, 1000}).
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec new([craterl_server_spec()], [term()]) -> atom().
new(Servers, Options) when is_list(Options) ->
  new(?DEFAULT_CLIENT_SPEC, Servers, Options).

%%--------------------------------------------------------------------
%% @doc
%% create a new craterl client instance given a client specification
%% comprised of a tuple of the same kind you would use for a call to register(),
%% e.g. {local, my_client} or {global, my_other_client}.
%% The client will be registered by the process name given in the tuple.
%% It must be unique per erlang node.
%% The second argument is a list of crate servers instances and a list of options as a proplist.
%%
%% Example:
%% <pre>
%% my_client = new({local, my_client}, [{&lt;&lt;"192.168.0.1"&gt;&gt;, 4200}], [{poolsize, 100], {timeout, 1000}).
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec new(ClientSpec:: craterl_client_spec(), Servers::[craterl_server_spec()], Options::[term()]) -> atom().
new(ClientSpec, Servers, Options) ->
  NormalizedServers = lists:map(fun(Spec) -> craterl_url:server_spec(Spec) end, Servers),
  craterl_sup:start_client(ClientSpec, NormalizedServers, Options).

%%--------------------------------------------------------------------
%% @doc
%% stop a running client instance by giving a client reference,
%% the return value of new().
%%
%% @end
%%--------------------------------------------------------------------
-spec stop_client(ClientName::atom()) -> ok | {error, term()}.
stop_client(ClientName) when is_atom(ClientName) ->
  craterl_sup:stop_client(ClientName).


%%--------------------------------------------------------------------
%% @doc
%% issue a SQL statement, with optional arguments
%% or a prebuilt #sql_request{}
%% using the default client instance.
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
%% @end
%%--------------------------------------------------------------------
-spec sql(Stmt::binary()|string(), Args::list())      -> {ok, sql_response()};
         (ClientSpec::atom(), Request::sql_request()) -> {ok, sql_response()}.
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
-spec sql(Stmt::binary(), Args::list(), IncludeTypes::boolean()) -> {ok, sql_response()}.
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
-spec sql(ClientSpec::atom(), Stmt::binary(), Args::list(), IncludeTypes::boolean()) -> {ok, sql_response()}.
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
%% to a specific client.
%% @end
%%--------------------------------------------------------------------
-spec sql_bulk(ClientName::atom(), Stmt::binary(), BulkArgs::[[term()]], IncludeTypes::boolean()) -> {ok, sql_bulk_response()} | {error, term()}.
sql_bulk(ClientName, Stmt, BulkArgs, IncludeTypes)->
  sql_bulk(ClientName, #sql_bulk_request{stmt = Stmt, bulk_args = BulkArgs, includeTypes = IncludeTypes}).


%%--------------------------------------------------------------------
%% @doc
%% Get a blob by digest from a blob table.
%%
%% This method will return {ok, Fun} in case of success where Fun is
%% a function returning {ok, BinaryData} as long as there is further
%% data to fetch. When all data is fetched, it returns {ok, done}.
%% Using this pattern, it is possible to chunk the response from the server
%% and not load everything into memory.
%% @end
%%--------------------------------------------------------------------
-spec blob_get(binary(), binary()) -> {ok, term()}.
blob_get(BlobTable, HexDigest) ->
  blob_get(?MODULE, BlobTable, HexDigest).

%%--------------------------------------------------------------------
%% @doc
%% Get a blob by digest from a blob table
%% using a specific client.
%%
%% This method will return {ok, Fun} in case of success where Fun is
%% a function returning {ok, BinaryData} as long as there is further
%% data to fetch. When all data is fetched, it returns {ok, done}.
%% Using this pattern, it is possible to chunk the response from the server
%% and not load everything into memory.
%% @end
%%--------------------------------------------------------------------
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


%%--------------------------------------------------------------------
%% @doc
%% Get a blob by digest from a blob table right to a given file.
%% It will return {ok, FilePath} where FilePath is the path to the file
%% where the blob got stored
%% @end
%%--------------------------------------------------------------------
-spec blob_get_to_file(BlobTable::binary(), HexDigest::binary(), FilePath::binary()) -> {ok, binary()}.
blob_get_to_file(BlobTable, HexDigest, FilePath) ->
  blob_get_to_file(?MODULE, BlobTable, HexDigest, FilePath).

%%--------------------------------------------------------------------
%% @doc
%% Get a blob by digest from a blob table right to a given file
%% using a specific client.
%% It will return {ok, FilePath} on success where FilePath is the path to the file
%% where the blob got stored
%% @end
%%--------------------------------------------------------------------
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


%%--------------------------------------------------------------------
%% @doc
%% check if a blob exists given a digest and a blob table.
%% Will return ok on success.
%% @end
%%--------------------------------------------------------------------
-spec blob_exists(BlobTable::binary(), HexDigest::binary()) -> ok | {error, term()}.
blob_exists(BlobTable, HexDigest) ->
  blob_exists(?MODULE, BlobTable, HexDigest).

%%--------------------------------------------------------------------
%% @doc
%% check if a blob exists given a digest and a blob table
%% using a specicif client.
%%
%% Will return ok on success.
%% @end
%%--------------------------------------------------------------------
-spec blob_exists(ClientName::atom(), BlobTable::binary(), HexDigest::binary()) -> ok | {error, term()}.
blob_exists(ClientName, BlobTable, HexDigest) ->
  Request = #blob_request{
               method=head,
               table=BlobTable,
               digest=HexDigest},
  SuccessFun = fun
    (exists) -> ok;
    (Response) -> {error, {invalid_response, Response}}
  end,
  execute_request(ClientName, Request, SuccessFun).

%%--------------------------------------------------------------------
%% @doc
%% put a blob to the crate server given its content
%% and the blob table to store it into.
%%
%% this function will create the hash of the content and return it like this
%% on success: {ok, {created, HashDigest}}. Use the hash digest to refer to
%% your blob in further requests.
%%
%% @end
%%--------------------------------------------------------------------
-spec blob_put(BlobTable::binary(), Content::binary()) -> {ok, {created, binary()}} | {error, term()}.
blob_put(BlobTable, Content) ->
  blob_put(?MODULE, BlobTable, Content).

%%--------------------------------------------------------------------
%% @doc
%% put a blob to the crate server given its content
%% and the blob table to store it into
%% using a specific client.
%%
%% this function will create the hash of the content and return it like this
%% on success: {ok, {created, HashDigest}}. Use the hash digest to refer to
%% your blob in further requests.
%%
%% @end
%%--------------------------------------------------------------------

-spec blob_put(ClientName::atom(), BlobTable::binary(), Content::binary()) -> {ok, {created, binary()}} | {error, term()}.
blob_put(ClientName, BlobTable, Content) ->
  case craterl_hash:sha1Hex(Content) of
    {ok, HexDigest} ->
      send_blob(ClientName, BlobTable, HexDigest, {data, Content})
  end.

%%--------------------------------------------------------------------
%% @doc
%% put a blob to the crate server given a filename from which to fetch the content
%% and the blob table to store it into.
%%
%% this function will create the hash of the file content and return it like this
%% on success: {ok, {created, HashDigest}}. Use the hash digest to refer to
%% your blob in further requests.
%% @end
%%--------------------------------------------------------------------
-spec blob_put_file(BlobTable::binary(), FilePath::binary()) -> {ok, {created, binary()}} | {error, term()}.
blob_put_file(BlobTable, FilePath) ->
  blob_put_file(?MODULE, BlobTable, FilePath).


%%--------------------------------------------------------------------
%% @doc
%% put a blob to the crate server given a filename from which to fetch the content
%% and the blob table to store it into
%% using a specific client.
%%
%% this function will create the hash of the file content and return it like this
%% on success: {ok, {created, HashDigest}}. Use the hash digest to refer to
%% your blob in further requests.
%% @end
%%--------------------------------------------------------------------
-spec blob_put_file(ClientName::atom(), BlobTable::binary(), FilePath::binary()) -> {ok, {created, binary()}} | {error, term()}.
blob_put_file(ClientName, BlobTable, FilePath) ->
  case craterl_hash:sha1HexFile(FilePath) of
    {ok, HexDigest} ->
      send_blob(ClientName, BlobTable, HexDigest, {file, FilePath});
    {error, Reason} -> {error, Reason}
  end.


%%--------------------------------------------------------------------
%% @doc
%% delete a blob from a blob table referenced by its hashdigest.
%%
%% @end
%%--------------------------------------------------------------------
-spec blob_delete(BlobTable::binary(), HexDigest::binary()) -> ok | {error, term()}.
blob_delete(BlobTable, HexDigest) ->
  blob_delete(?MODULE, BlobTable, HexDigest).

%%--------------------------------------------------------------------
%% @doc
%% delete a blob from a blob table referenced by its hashdigest
%% using a specific client.
%%
%% @end
%%--------------------------------------------------------------------
-spec blob_delete(ClientName::atom(), BlobTable::binary(), HexDigest::binary()) -> ok | {error, term()}.
blob_delete(ClientName, BlobTable, HexDigest) ->
  Request = #blob_request{
    method = delete,
    table = BlobTable,
    digest = HexDigest
  },
  SuccessFun = fun
    (deleted) -> ok;
    (Response) -> {error, {invalid_response, Response}}
  end,
  execute_request(ClientName, Request, SuccessFun).


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
          {error, Reason = econnrefused} ->
            craterl_gen_server:add_inactive(ClientName, Server),
            {error, Reason};
          Response -> Response
        end
  end.

-spec execute_request_on_server(sql_request(), craterl_server_conf(), fun()) -> {ok, sql_response()} | {error, term()};
                               (sql_bulk_request(), craterl_server_conf(), fun()) -> {ok, sql_bulk_response()} | {error, term()};
                               (blob_request(), craterl_server_conf(), fun()) -> ok|{ok, term()} | {error, term()}.
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
