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

-module(craterl_sql).
-author("Matthias Wahl").

%% API
-export([sql_request/2]).

-ifdef(TEST).
-compile(export_all).
-endif.

-include("craterl.hrl").
-compile([{parse_transform, lager_transform}]).

%%% API %%%

-spec sql_request(sql_request(), craterl_server_conf()) -> {ok, sql_response()} | {error, term()};
    (sql_bulk_request(), craterl_server_conf()) -> {ok, sql_bulk_response()} | {error, term()}.
sql_request(#sql_request{stmt=Stmt, args=Args, includeTypes=IncludeTypes}, ServerConf) ->
  sql_request(Stmt, Args, IncludeTypes, ServerConf);
sql_request(#sql_bulk_request{stmt=Stmt, bulk_args=BulkArgs, includeTypes = IncludeTypes}, ServerConf) ->
  sql_request(Stmt, {bulk, BulkArgs}, IncludeTypes, ServerConf).
sql_request(Stmt, Args, IncludeTypes, #craterl_server_conf{config = {_Options, RequestConfig}, address = ServerUrl}) ->
  Url = craterl_url:create_server_url(ServerUrl, IncludeTypes),
  Payload = create_payload(Stmt, Args),
  lager:debug("sql request: ~p ~p", [Url, Payload]),
  HttpResponseMeta = hackney:request(post,
    Url,
    [{<<"Content-Type">>, <<"application/json">>}, {<<"Accept">>, <<"application/json">>}],
    Payload,
    RequestConfig
  ),
  lager:debug("sql response: ~p", [HttpResponseMeta]),
  Response = case HttpResponseMeta of
    {ok, StatusCode, _RespHeaders, ClientRef} ->
       % parse body
       case hackney:body(ClientRef) of
         {ok, <<"">>} -> {error, no_content};
         {ok, Body} ->
           case StatusCode of
             StatusCode when StatusCode < 400 ->
               case build_response(Body) of
                 {error, Reason} -> {error, Reason};
                 JsonResponse -> {ok, JsonResponse}
               end;
             _ErrorCode ->
               {error, build_error_response(Body)}
           end;
         {error, Reason} ->
           {error, Reason}
       end;
    {error, Reason} -> {error, Reason}
  end,
  Response.

%%% INTERNAL %%%

-spec create_payload(binary(), list()) -> binary();
                    (binary(), {bulk, list()}) -> binary().
create_payload(Stmt, {bulk, Args}) ->
  create_payload([
    {<<"stmt">>, Stmt},
    {<<"bulk_args">>, Args}
  ]);
create_payload(Stmt, Args) ->
  create_payload([
    {<<"stmt">>, Stmt},
    {<<"args">>, Args}
  ]).
create_payload(Payload) ->
  jsx:encode(Payload).

-spec build_response(binary()) -> sql_response() | sql_bulk_response() | {error, invalid_json}.
build_response(Body) when is_binary(Body) ->
  try case jsx:decode(Body) of
    Decoded ->
      Columns = proplists:get_value(<<"cols">>, Decoded, []),
      ColumnTypes = proplists:get_value(<<"col_types">>, Decoded, []),
      Duration = proplists:get_value(<<"duration">>, Decoded, 0),
      case proplists:get_value(<<"results">>, Decoded) of
        undefined ->  #sql_response{
            rowCount = proplists:get_value(<<"rowcount">>, Decoded, 0),
            cols = Columns,
            colTypes = ColumnTypes,
            rows = proplists:get_value(<<"rows">>, Decoded, []),
            duration = Duration
          };
        Results -> #sql_bulk_response{
            results = Results,
            cols = Columns,
            colTypes = ColumnTypes,
            duration = Duration
          }
      end
  end
  catch
    error:badarg -> {error, invalid_json}
  end.


-spec build_error_response(binary()) -> sql_error() | {error, invalid_json}.
build_error_response(Body) when is_binary(Body) ->
  try
    Decoded = jsx:decode(Body),
    ErrorInfo = proplists:get_value(<<"error">>, Decoded, []),
    #sql_error{
      code=proplists:get_value(<<"code">>, ErrorInfo, ?CRATERL_DEFAULT_ERROR_CODE),
      message=proplists:get_value(<<"message">>, ErrorInfo, ?CRATERL_DEFAULT_MESSAGE)
    }
  catch
    error:badarg -> {error, invalid_json}
  end.
