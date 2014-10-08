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

-module(craterl_url).
-author("Matthias Wahl").

%% API
-export([
  create_server_url/2
]).

-ifdef(TEST).
-compile(export_all).
-endif.

-include("craterl.hrl").
-compile([{parse_transform, lager_transform}]).

-define(SQLPATH, <<"/_sql">>).

%%% API %%%

-spec create_server_url(craterl_server_spec(), boolean()) -> binary();
                       (craterl_server_spec(), binary())  -> binary().
create_server_url({Host, Port}, true) ->
  create_server_url({Host, Port}, <<?SQLPATH/binary, "?types">>);
create_server_url({Host, Port}, false) ->
  create_server_url({Host, Port}, ?SQLPATH);
create_server_url({Host, Port}, Path) when is_binary(Path) ->
  PortString = integer_to_binary(Port),
  Url = <<Host/binary, ":", PortString/binary, Path/binary>>,
  case Url of
    <<"http://", _/binary>> -> Url;
    <<"https://", _/binary>> -> Url;
    <<_/binary>> -> <<"http://", Url/binary>>
  end.
