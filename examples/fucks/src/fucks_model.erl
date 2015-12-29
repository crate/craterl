%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, CRATE Technology GmbH
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

-module(fucks_model).
-author("mat").

%% API
-export([
  prepare_fucks/1,
  get_fucks/4,
  create_fuck/3,
  get_fuck/2,
  search_fucks/2,
  delete_fuck/2]).

-define(CREATE_TABLE, list_to_binary("CREATE TABLE IF NOT EXISTS fucks (
  id string primary key,
  about string,
  author string,
  created timestamp,
  index search_index using fulltext(about, author) with (analyzer='english')
  )")).
-define(INSERT, <<"INSERT INTO fucks (id, author, about, created) VALUES (?, ?, ?, CURRENT_TIMESTAMP)">>).
-define(SINGLE_SELECT, <<"SELECT id, about, author, created FROM fucks WHERE id = ? LIMIT 1">>).
-define(SEARCH_SELECT, <<"SELECT id, about, author, created FROM fucks WHERE match(search_index, ?) ORDER BY _score DESC, created DESC LIMIT 100">>).
-define(DELETE, <<"DELETE from fucks WHERE id = ?">>).
-define(REFRESH, <<"REFRESH TABLE fucks">>).

%% API %%

-spec get_fucks(craterl:craterl_client_ref(), list(), integer(), integer()) -> [map()].
get_fucks(CrateClientRef, Params, Limit, Offset) when is_integer(Limit)
                                                 and is_integer(Offset) ->
  {WhereClause, SqlParams} = create_where_clause(Params),
  BinaryLimit = integer_to_binary(Limit),
  BinaryOffset = integer_to_binary(Offset),
  FormattedStmt = <<"SELECT _id, about, author, created FROM fucks WHERE ", WhereClause/binary, " ORDER BY created DESC LIMIT ", BinaryLimit/binary , " OFFSET ", BinaryOffset/binary >>,
  {ok, Response} = craterl:sql(
    CrateClientRef,
    FormattedStmt,
    SqlParams),
  [format_row(Row) || Row <- craterl_resp:rows(Response)].

-spec prepare_fucks(craterl:craterl_client_ref()) -> {ok, craterl:sql_response()}.
prepare_fucks(CrateClientRef) ->
  %% TODO wait until shards are created
  craterl:sql(CrateClientRef, ?CREATE_TABLE).

-spec create_fuck(craterl:craterl_client_ref(), binary(), binary()) -> binary().
create_fuck(CrateClientRef, Author, About) ->
  Id = create_fuck_with_retry(CrateClientRef, Author, About, 0),
  refresh_fucks(CrateClientRef),
  Id.

-spec get_fuck(craterl:craterl_client_ref(), binary()) -> not_found|map().
get_fuck(CrateClientRef, Id) ->
  {ok, Response} = craterl:sql(CrateClientRef, ?SINGLE_SELECT, [Id]),
  case craterl_resp:row_count(Response) of
    0 -> not_found;
    1 ->
      [Row] = craterl_resp:rows(Response),
      format_row(Row)
  end.

-spec search_fucks(craterl:craterl_client_ref(), binary()) -> [map()].
search_fucks(CrateClientRef, SearchTerm) ->
  {ok, Response} = craterl:sql(CrateClientRef, ?SEARCH_SELECT, [SearchTerm]),
  [format_row(Row) || Row <- craterl_resp:rows(Response)].

delete_fuck(CrateClientRef, Id) ->
  {ok, _Response} = craterl:sql(CrateClientRef, ?DELETE, [Id]),
  refresh_fucks(CrateClientRef).

%% private %%

refresh_fucks(CrateClientRef) ->
  {ok, _Response} = craterl:sql(CrateClientRef, ?REFRESH),
  ok.

-spec format_row(list()) -> map().
format_row([Id, About, Author, Created]) ->
  #{id => Id,
    about => About,
    author => Author,
    created => Created}.

-spec create_where_clause(list()) -> {binary(), list()}.
create_where_clause(Params) ->
  {
    build_where_clause(Params),
    [Val || {_Name, Val, _Op} <- Params]
  }.

-spec build_where_clause([{binary(), term(), binary()}]) -> binary().
build_where_clause([{Name, _Val, Operator}|Rest]) ->
  Name2 = case Name of
    <<"from">> -> <<"created">>;
    <<"to">> -> <<"created">>;
    Other -> Other
  end,
  Identifier = sql_quote_identifier(Name2),
  build_where_clause(Rest, <<Identifier/binary , " ", Operator/binary, " ?">>);
build_where_clause([]) -> <<"true">>.

-spec build_where_clause([{binary(), term(), binary()}], binary()) -> binary().
build_where_clause([{Name, _Val, Operator}|Rest], Acc) ->
  Identifier = sql_quote_identifier(Name),
  build_where_clause(Rest, <<Acc/binary, " AND ", Identifier/binary , " ", Operator/binary, " ?">>);
build_where_clause([], Acc) -> Acc.

-spec sql_quote_identifier(binary()) -> binary().
sql_quote_identifier(Identifier) when is_binary(Identifier) ->
  InnerQuoted = binary:replace(Identifier, <<$">>, <<$", $">>),
  <<$", InnerQuoted/binary ,$">>.

create_fuck_with_retry(_, _, _, Tries) when Tries >= 10 ->
  throw(create_fuck_failed);
create_fuck_with_retry(CrateClientRef, Author, About, Tries) ->
  Id = uuid:uuid_to_string(uuid:get_v4(), binary_standard),
  Args =  [Id, Author, About],

  case craterl:sql(CrateClientRef, ?INSERT, Args) of
    {ok, _Response} -> Id;
    {error,Error} ->
      case craterl_resp:error_code(Error) of
        4091 -> create_fuck_with_retry(CrateClientRef, Author, About, Tries+1);
        _ ->
          io:format("Error creating fuck ~p: ~p ~n", [Args, craterl_resp:error_message(Error)]),
          throw(create_fuck_failed)
      end
  end.