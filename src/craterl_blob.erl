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

-module(craterl_blob).
-author("Matthias Wahl").

%% API
-export([blob_request/2]).
-ifdef(TEST).
-compile(export_all).
-endif.

-include("craterl.hrl").
-compile([{parse_transform, lager_transform}]).

%%% API %%%

-spec blob_request(blob_request(), craterl_server_conf()) -> {ok, term()} | {error, term()}.
blob_request(#blob_request{method=Method, table=Table, digest=Digest, payload=Payload}, ServerConf) ->
  Response = case Method of
    get ->
      case Payload of
        undefined -> blob_get_to_mem(ServerConf, Table, Digest);
        {file, FilePath} -> blob_get_to_file(ServerConf, Table, Digest, FilePath)
      end;
    head -> blob_exists(ServerConf, Table, Digest);
    put -> blob_put(ServerConf, Table, Digest, Payload);
    delete -> blob_delete(ServerConf, Table, Digest);
    _ -> {error, unsupported}
  end,
  Response.


%%% INTERNAL %%%

-spec blob_put(ServerConf :: craterl_server_conf(), Table :: binary(), Digest :: binary(), Payload :: blob_payload()) -> {ok, {created, binary()}} | {error, term()}.
blob_put(#craterl_server_conf{config = {_Options, RequestConfig}, address = ServerUrl}, Table, Digest, Payload) when is_binary(Table) and is_binary(Digest) ->

  Url =  craterl_url:create_server_url(ServerUrl, <<"/_blobs/", Table/binary, "/", Digest/binary>>),
  lager:debug("putting blob to ~p", [Url]),
  case hackney:request(put,
    Url,
    [
      {<<"Transfer-Encoding">>, <<"chunked">>}
    ],
    stream,
    RequestConfig
  ) of
    {ok, Client} ->
      Body = case Payload of
        {file, Path} -> {file, Path};
        {data, Data} -> Data
      end,
      case hackney:send_body(Client, Body) of
        ok ->
          case hackney:start_response(Client) of
            {ok, StatusCode, _RespHeaders, _ClientRef} ->
               case StatusCode of
                 201 -> {ok, {created, Digest}};
                 400 -> {error, {bad_request, Digest}};
                 404 -> {error, {not_found, Table}};
                 409 -> {error, {already_exists, Digest}};
                 _ -> {error, StatusCode}
               end;
            {error, Reason} ->
              {error, Reason}
          end;
        {error, Reason} -> {error, Reason}
      end;
      {error, Reason} -> {error, Reason}
  end.

-spec blob_get_to_mem(ServerConf :: craterl_server_conf(), Table :: binary(), Digest :: binary()) -> {ok, fun(()-> {ok, binary() | done})}.
blob_get_to_mem(ServerConf, Table, Digest) ->
  HandleBodyFun = fun (ClientRef) ->
    GetDataFun = fun() ->
      case hackney:stream_body(ClientRef) of
        done -> {ok, done};
        Other -> Other
      end
    end,
    {ok, GetDataFun}
  end,
  execute_blob_request(ServerConf, get, Table, Digest, HandleBodyFun).

-spec blob_get_to_file(ServerConf :: craterl_server_conf(), Table :: binary(), Digest :: binary(), FilePath :: binary()) -> {ok, binary()} | {error, term()}.
blob_get_to_file(ServerConf, Table, Digest, FilePath) ->
  HandleBodyFun = fun (ClientRef) ->
      case file:open(FilePath, [write, binary, raw]) of
        {ok, FileHandle} ->
          Result = case stream_blob_to_file(ClientRef, FileHandle) of
            ok -> {ok, FilePath};
            {error, Reason} -> {error, Reason}
          end,
          file:close(FileHandle),
          Result;
        {error, Reason} -> {error, Reason}
      end
  end,
  execute_blob_request(ServerConf, get, Table, Digest, HandleBodyFun).

-spec blob_exists(ServerConf :: craterl_server_conf(), Table :: binary(), Digest :: binary()) -> {ok, exists} | {error, 404} | {error, term()}.
blob_exists(ServerConf, Table, Digest) ->
  SuccessFun = fun (_ClientRef) ->
    {ok, exists}
  end,
  execute_blob_request(ServerConf, head, Table, Digest, SuccessFun).

-spec blob_delete(ServerConf :: craterl_server_conf(), Table :: binary(), Digest :: binary()) -> {ok, deleted} | {error, term()}.
blob_delete(ServerConf, Table, Digest) ->
  SuccessFun = fun (_ClientRef) ->
    {ok, deleted}
  end,
  execute_blob_request(ServerConf, delete, Table, Digest, SuccessFun).


-spec stream_blob_to_file(hackney:client_ref(), FileHandle::term()) -> ok | {ok, binary()} | {error, term()}.
stream_blob_to_file(ClientRef, FileHandle) ->
  case hackney:stream_body(ClientRef) of
    {ok, Data} ->
      file:write(FileHandle, Data),
      stream_blob_to_file(ClientRef, FileHandle);
    done -> ok;
    {error, Reason} -> {error, Reason}
  end.

-spec execute_blob_request(craterl_server_conf(), atom(), binary(), binary(), fun((hackney:client_ref()) -> term()))  -> term().
execute_blob_request(#craterl_server_conf{config = {_Options, RequestConfig}, address = ServerUrl}, Method, Table, Digest, HandleBodyFun) when is_function(HandleBodyFun)
                                                                and is_atom(Method) ->
  Url = craterl_url:create_server_url(ServerUrl,
    <<"/_blobs/", Table/binary, "/", Digest/binary>>),
  lager:debug("blob request: ~p ~p", [Method, Url]),
  Headers = [],
  ResponseMeta = hackney:request(Method, Url, Headers, <<>>, RequestConfig),
  lager:debug("blob response: ~p", [ResponseMeta]),
  case ResponseMeta of
    {ok, StatusCode, _RespHeaders, ClientRef} ->
      case StatusCode of
          Code when Code < 400 -> HandleBodyFun(ClientRef);
          _ -> {error, StatusCode}
      end;
    {ok, StatusCode, _RespHeaders} ->
      % HEAD Request
      case StatusCode of
          Code when Code < 400 -> HandleBodyFun(undefined);
          _ -> {error, StatusCode}
      end;
    {error, Reason} -> {error, Reason}
  end.
