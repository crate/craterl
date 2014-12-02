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
-module(bin_to_hex).

-export([hexstring/1]).

hexstring(<<X:128/big-unsigned-integer>>) ->
    list_to_binary(lists:flatten(io_lib:format("~32.16.0b", [X])));
hexstring(<<X:160/big-unsigned-integer>>) ->
    list_to_binary(lists:flatten(io_lib:format("~40.16.0b", [X])));
hexstring(<<X:256/big-unsigned-integer>>) ->
    list_to_binary(lists:flatten(io_lib:format("~64.16.0b", [X])));
hexstring(<<X:512/big-unsigned-integer>>) ->
    list_to_binary(lists:flatten(io_lib:format("~128.16.0b", [X]))).
