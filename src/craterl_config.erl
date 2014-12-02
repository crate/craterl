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
%%% stuff handling craterl configuration
%%% @end
%%%-------------------------------------------------------------------

-module(craterl_config).
-author("Matthias Wahl").

-include("craterl.hrl").

-define(CRATERL_DEFAULT_POOLNAME, crate).
-define(CRATERL_DEFAULT_POOLSIZE, 20).
-define(CRATERL_DEFAULT_TIMEOUT, 60000).
-define(CRATERL_DEFAULT_SSL_INSECURE, true). %% be lenient by default

%% API
-export([apply_defaults/0, apply_defaults/1, get/2, request_config/1]).

-spec request_config(proplists:proplist()) -> proplists:proplist().
%%--------------------------------------------------------------------
%% @doc
%% create the configuration for use in hackney requests from the server config
%%
%% @end
%%--------------------------------------------------------------------
request_config(Config) ->
  PoolName = craterl_config:get(poolname, Config),
  Timeout = craterl_config:get(timeout, Config),
  SSLOptions = craterl_config:get(ssl_options, Config),
  RequestConfig = [
    {pool, PoolName},
    {follow_redirect, true}, % we have to support redirects for blobs
    {force_redirect, true},
    {connect_timeout, Timeout},
    {recv_timeout, Timeout}
  ],
  RequestConfig2 = case SSLOptions of
    undefined -> RequestConfig;
    _ -> [{ssl_options, SSLOptions}|RequestConfig]
  end,
  case craterl_config:get(ssl_insecure, Config) of
    true -> [insecure | RequestConfig2];
    _ -> RequestConfig2
  end.


-spec get(term(), proplists:proplist()) -> undefined | term().
get(Key, Config) ->
  proplists:get_value(Key, Config).

-spec apply_defaults() -> {proplists:proplist(), proplists:proplist()}.
apply_defaults() ->
  apply_defaults([]).

-spec apply_defaults(proplists:proplist()) -> {proplists:proplist(), proplists:proplist()}.
apply_defaults(Options) ->
    Options2 = apply_default(Options, poolname, ?CRATERL_DEFAULT_POOLNAME),
    Options3 = apply_default(Options2, poolsize, ?CRATERL_DEFAULT_POOLSIZE),
    Options4 = apply_default(Options3, ssl_insecure, ?CRATERL_DEFAULT_SSL_INSECURE),
    Options5 = apply_default(Options4, timeout, ?CRATERL_DEFAULT_TIMEOUT),
    {Options5, request_config(Options5)}.

apply_default(Config, Key, Default) ->
  lists:keystore(Key, 1, Config, {Key, proplists:get_value(Key, Config, Default)}).
