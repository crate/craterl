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

-module(fucks_handler).

%% API
-export([init/3,
         rest_init/2,
         allowed_methods/2,
         content_types_provided/2,
         malformed_request/2,
         content_types_accepted/2,
         resource_exists/2
  ]).

-export([to_json/2, from_json/2]).

-define(IDENTITY, fun (X) -> X end).
-define(DEFAULT_LIMIT, 20).
-define(PARAMS, [
  {<<"about">>, ?IDENTITY, <<"=">>},
  {<<"author">>, ?IDENTITY, <<"=">>},
  {<<"from">>, fun binary_to_integer/1, <<">=">>},
  {<<"to">>, fun binary_to_integer/1,   <<"<=">>},
  {<<"offset">>, fun binary_to_integer/1, <<>>}
]).
-define(WHERE_PARAMS, [<<"about">>, <<"author">>, <<"from">>, <<"to">>]).

init({tcp, http}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, Opts) ->
  {craterl, ClientRef} = lists:keyfind(craterl, 1, Opts),
  {ok, Req, #{craterl => ClientRef}}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>], Req, State}.

content_types_accepted(Req, State) -> {
  [
    {{<<"application">>, <<"json">>, []}, from_json}
  ],
  Req, State
}.

content_types_provided(Req, State) ->
  {[
    {{ <<"application">>, <<"json">>, []}, to_json}
  ], Req, State}.

malformed_request(Req, State) ->
  %% parse qs
  case cowboy_req:method(Req) of
    {<<"GET">>, Req2} ->
      {Vals, Req3} = cowboy_req:qs_vals(Req2),
      try
        Params = validate_get_params(Vals),
        {false, Req3, maps:put(params, Params, State)}
      catch
        ErrorClass:Reason ->
          io:format("Bad Request ~p: ~p~n", [ErrorClass, Reason]),
          {true, Req3, State}
      end;
    {<<"POST">>, Req2} ->
      {ok, Body, Req3} = cowboy_req:body(Req2),
      try
          DecodedBody = jsx:decode(Body, [return_maps]),
          case has_valid_body(DecodedBody) of
            false -> {true, Req3, State};
            true ->  {false, Req3, maps:put(payload, DecodedBody, State)}
          end
      catch
          error:badarg -> {true, Req3, State}
      end
  end.

resource_exists(Req, #{craterl := CrateClientRef}=State) ->
  case cowboy_req:binding(id, Req) of
    {undefined, Req2} ->
      case cowboy_req:method(Req2) of
        {<<"GET">>, Req3} -> {true, Req3, State};
        {<<"POST">>, Req3} -> {false, Req3, State}
      end ;
    {Id, Req2} ->
      case fucks_model:get_fuck(CrateClientRef, Id) of
        not_found -> {false, Req2, State};
        Fuck -> {true, Req2, maps:put(fuck, Fuck, State)}
      end
  end.

has_valid_body(#{<<"author">> := Author, <<"about">> := About}=Map) when is_binary(Author) and is_binary(About) ->
  %% ensure no other values
  maps:size(Map) =:= 2;
has_valid_body(_) -> false.

validate_get_params(Vals) ->
  case has_invalid_params(Vals) of
    false -> validate_params(Vals, ?PARAMS, []);
    true  -> erlang:throw(bad_param)
  end.
validate_params(Vals, [{Name, Parse, Op}| Rest], Acc) ->
  case lists:keyfind(Name, 1, Vals) of
    false -> validate_params(Vals, Rest, Acc);
    {Name, Val} -> validate_params(Vals, Rest, [{Name, Parse(Val), Op}|Acc])
  end;
validate_params(_Vals, [], Acc) -> Acc.

has_invalid_params(Params) ->
  IsNoParam = fun ({Key, _Value}) -> lists:keymember(Key, 1, ?PARAMS) =:= false end,
  lists:any(IsNoParam, Params).

to_json(Req, #{craterl := CrateClientRef, params := Params} = State) ->
  Offset = case lists:keyfind(<<"offset">>, 1, Params) of
      {<<"offset">>, Value, _Op} -> Value;
      false -> 0
  end,
  IsWhereParam = fun ({ParamName, _Value, _Op}) ->
    lists:member(ParamName, ?WHERE_PARAMS)
  end,
  WhereParams = lists:filter(IsWhereParam, Params),
  Fucks = fucks_model:get_fucks(CrateClientRef, WhereParams, ?DEFAULT_LIMIT, Offset),
  {jsx:encode(Fucks), Req, State}.

from_json(Req, #{craterl := CrateClientRef, payload := #{<<"author">> := Author, <<"about">> := About}}=State) ->
  FuckId = fucks_model:create_fuck(CrateClientRef, Author, About),
  Uri = <<"/fucks/", FuckId/binary>>,
  case cowboy_req:method(Req) of
    {<<"POST">>, Req2} -> {{true, Uri}, Req2, State};
    {_, Req2} -> {true, Req2, State}
  end.