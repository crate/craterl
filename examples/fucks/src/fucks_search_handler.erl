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

-module(fucks_search_handler).

-behaviour(cowboy_http_handler).

%% API
-export([init/3, handle/2, terminate/3]).

-record(state, {
  craterl :: craterl:craterl_client_ref()
}).

init(_, Req, Opts) ->
  {craterl, CraterlClientRef} = lists:keyfind(craterl, 1, Opts),
  {ok, Req, #state{craterl=CraterlClientRef}}.

handle(Req, #state{craterl=CraterlClientRef}=State) ->
  {ok, Req3} = case cowboy_req:qs_val(<<"q">>, Req) of
    {SearchTerm, Req2} when is_binary(SearchTerm) ->
      Fucks = fucks_model:search_fucks(CraterlClientRef, SearchTerm),
      cowboy_req:reply(200, [
        {<<"content-type">>, <<"application/json">>}
      ], jsx:encode(Fucks), Req2);
    {_, Req2} -> cowboy_req:reply(400, Req2)
  end,
  {ok, Req3, State}.

terminate(_, _, _) ->
  ok.