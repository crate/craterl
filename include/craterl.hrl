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
-author("mat").


-define(SQLPATH, <<"/_sql">>).
-define(DEFAULT_PORT, 4200).
-define(DEFAULT_ROWCOUNT, 0).
-define(DEFAULT_DURATION, 0).
-define(DEFAULT_WALLCLOCK, 0).
-define(DEFAULT_RUNTIME, 0).
-define(DEFAULT_MESSAGE, <<"Shit happens">>).
-define(DEFAULT_CODE, 1000).
-define(DEFAULT_SERVER, <<"localhost:4200">>).

-record(sql_response, {cols=[], rows=[], rowCount=?DEFAULT_ROWCOUNT, duration=?DEFAULT_DURATION, wallclock=?DEFAULT_WALLCLOCK, runtime=?DEFAULT_RUNTIME}).

-record(sql_request, {stmt, args=[]}).
-record(sql_error, {message=?DEFAULT_MESSAGE, code=?DEFAULT_CODE}).

-record(blob_request, {method, table, digest, payload=undefined}).
