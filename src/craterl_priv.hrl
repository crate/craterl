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

-author("Matthias Wahl").

-ifndef(CRATERL_PRIV).
-define(CRATERL_PRIV, 1).

-define(CRATERL_DEFAULT_PORT, 4200).
-define(CRATERL_DEFAULT_ROWCOUNT, 0).
-define(CRATERL_DEFAULT_DURATION, 0).
-define(CRATERL_DEFAULT_MESSAGE, <<"Shit happens">>).
-define(CRATERL_DEFAULT_ERROR_CODE, 1000).
-define(CRATERL_DEFAULT_SERVER, {<<"localhost">>, 4200}).

-record(sql_request, {
        stmt :: binary(),
        args=[] :: list(),
        includeTypes=false :: boolean()
}).
-type sql_request() :: #sql_request{}.

-type craterl_client_spec() :: {local, atom()} | {global, atom()} | {via, atom(), atom()}.

-type craterl_client_ref() :: atom().

-type craterl_server_spec() :: {binary(), non_neg_integer()} | string() | binary().

-record(craterl_server_conf, {
  address :: craterl_server_spec(),
  config :: {proplists:proplist(), proplists:proplist()}
}).
-type craterl_server_conf() :: #craterl_server_conf{}.

-record(sql_response, {
        cols=[] :: [binary()],
        colTypes=[] :: [integer()],
        rows=[] :: [ [any()] ],
        rowCount=?CRATERL_DEFAULT_ROWCOUNT :: integer(),
        duration=?CRATERL_DEFAULT_DURATION :: non_neg_integer()
    }).
-type sql_response() :: #sql_response{}.

-record(sql_bulk_request, {
        stmt :: binary(),
        bulk_args=[[]] :: [ [ any() ] ],
        includeTypes=false :: boolean()
}).
-type sql_bulk_request() :: #sql_bulk_request{}.

-record(sql_bulk_response, {
        cols=[] :: [binary()],
        colTypes=[] :: [integer()],
        results :: [sql_bulk_result()],
        duration=?CRATERL_DEFAULT_DURATION :: non_neg_integer()
}).
-type sql_bulk_response() :: #sql_bulk_response{}.

-record(sql_bulk_result, {
        rowCount=?CRATERL_DEFAULT_ROWCOUNT :: non_neg_integer(),
        errorMessage :: binary()
}).
-type sql_bulk_result() :: #sql_bulk_result{}.

-record(sql_error, {
        message=?CRATERL_DEFAULT_MESSAGE :: binary(),
        code=?CRATERL_DEFAULT_ERROR_CODE :: integer()
}).
-type sql_error() :: #sql_error{}.


-type blob_payload() :: {data, binary()} | {file, binary()} | undefined.
-record(blob_request, {
    method :: atom(),
    table :: binary(),
    digest :: binary(),
    payload :: blob_payload()
}).
-type blob_request() :: #blob_request{}.

-endif.
