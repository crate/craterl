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

-module('fucks_app').

-behaviour(application).

%% Application callbacks
-export([start/2
        ,stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  CraterlServerSpec = craterl_url:server_spec(application:get_env(fucks, crate_host, "localhost:4200")),
  ListenPort = application:get_env(fucks, listen_port, 8080),
  CraterlClient = craterl:new([CraterlServerSpec]),
  {ok, _} = fucks_model:prepare_fucks(CraterlClient),
  HandlerOpts = [{craterl, CraterlClient}],
  Dispatch = cowboy_router:compile([
    %% {HostMatch, list({PathMatch, Handler, Opts})}
    {'_', [
      {"/fucks/:id", fuck_handler,  HandlerOpts},
      {"/fucks/",    fucks_handler, HandlerOpts},
      {"/search/",   fucks_search_handler, HandlerOpts}
    ]}
  ]),
  {ok, _} = cowboy:start_http(fucks_listener, 100,
    [
      {port, ListenPort}
    ],
    [
      {env, [
          {dispatch, Dispatch}
        ]
      }
    ]
  ),
  {ok, SupPid} = 'fucks_sup':start_link(),
  {ok, SupPid, #{craterl => CraterlClient}}.

%%--------------------------------------------------------------------
stop(#{craterl := CraterlClientRef}) ->
    craterl:stop_client(CraterlClientRef),
    cowboy:stop_listener(fucks_listener),
    ok;
stop(_State) -> ok.

%%====================================================================
%% Internal functions
%%====================================================================
