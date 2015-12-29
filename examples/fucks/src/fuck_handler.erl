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

-module(fuck_handler).
-author("mat").

%% API
-export([
  init/3,
  rest_init/2,
  allowed_methods/2,
  delete_resource/2,
  resource_exists/2,
  content_types_provided/2]).

-export([to_json/2]).

init({tcp, http}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, Opts) ->
  {craterl, ClientRef} = lists:keyfind(craterl, 1, Opts),
  {ok, Req, #{craterl => ClientRef}}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"DELETE">>], Req, State}.

delete_resource(Req, #{craterl := ClientRef}=State) ->
  {Id, Req2} = cowboy_req:binding(id, Req),
  ok = fucks_model:delete_fuck(ClientRef, Id),
  {true, Req2, State}.

content_types_provided(Req, State) ->
  {[
    {{ <<"application">>, <<"json">>, []}, to_json}
  ], Req, State}.

resource_exists(Req, #{craterl := CrateClientRef}=State) ->
  case cowboy_req:binding(id, Req) of
    {undefined, _Req2} ->
      throw(should_not_happen);
    {Id, Req2} ->
      case fucks_model:get_fuck(CrateClientRef, Id) of
        not_found -> {false, Req2, State};
        Fuck -> {true, Req2, maps:put(fuck, Fuck, State)}
      end
  end.

to_json(Req, #{fuck := Fuck}=State) ->
  {jsx:encode(Fuck), Req, State}.