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

-module(craterl_resp).
-author("Matthias Wahl").

-include("craterl_priv.hrl").

%% API
-export([
  column_names/1,
  rows/1,
  row_count/1,
  bulk_results/1,
  duration/1,
  types/1,
  to_tuple/1,
  column/2,
  error_message/1,
  error_code/1
]).

-spec column_names(sql_response()|sql_bulk_response()) -> [binary()].
column_names(#sql_response{cols = Columns}) ->
  Columns;
column_names(#sql_bulk_response{cols = Columns}) ->
  Columns.

-spec rows(sql_response()) -> [[term()]].
rows(#sql_response{rows = Rows}) ->
  Rows.

-spec bulk_results(sql_bulk_response()) -> [sql_bulk_result()].
bulk_results(#sql_bulk_response{results = Results}) ->
  Results.

-spec row_count(sql_response()|sql_bulk_result()) -> integer().
row_count(#sql_response{rowCount = RowCount}) ->
  RowCount;
row_count(#sql_bulk_result{rowCount = RowCount}) ->
  RowCount.

-spec duration(sql_response()|sql_bulk_response()) -> non_neg_integer().
duration(#sql_response{duration = Duration}) ->
  Duration;
duration(#sql_bulk_response{duration = Duration}) ->
  Duration.

-spec types(sql_response()|sql_bulk_response()) -> [integer()].
types(#sql_response{colTypes = Types}) ->
  Types;
types(#sql_bulk_response{colTypes = Types}) ->
  Types.

-spec to_tuple(sql_response()) -> {[[term]], integer(), [binary()], [integer()], non_neg_integer()};
    (sql_bulk_response()) -> {[sql_bulk_result()], [binary()], [integer()], [non_neg_integer()]}.
to_tuple(#sql_response{rows=Rows, rowCount = RowCount, cols = Columns, colTypes = ColTypes, duration = Duration}) ->
  {Rows, RowCount, Columns, ColTypes, Duration};
to_tuple(#sql_bulk_response{results = Results, cols=Columns, colTypes = ColTypes, duration = Duration}) ->
  {Results, Columns, ColTypes, Duration}.

-spec column(sql_response(), pos_integer()) -> [any()].
column(#sql_response{rows=Rows}, Index) when is_integer(Index), Index > 0 ->
  [ lists:nth(Index, Row) || Row <- Rows ].

-spec error_message(sql_bulk_result()|sql_error()) -> binary().
error_message(#sql_bulk_result{errorMessage = ErrorMessage}) ->
  ErrorMessage;
error_message(#sql_error{message = Message}) ->
  Message.

-spec error_code(sql_error()) -> integer().
error_code(#sql_error{code = Code}) ->
  Code.
