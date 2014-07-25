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

-module(bin_to_hex_tests).
-author("Matthias Wahl").

-include_lib("eunit/include/eunit.hrl").

hexstring_128_test() ->
  <<"00000000000000000000000000000000">> = bin_to_hex:hexstring(<<0:128/big-unsigned-integer>>),
  <<"00000000000000000000000000000064">> = bin_to_hex:hexstring(<<100:128/big-unsigned-integer>>),
  <<"ffffffffffffffffffffffffffffffff">> = bin_to_hex:hexstring(<<-1:128/big-unsigned-integer>>),
  <<"00000000000000000000000000000400">> = bin_to_hex:hexstring(<<1024:128/big-unsigned-integer>>).

hexstring_160_test() ->
  <<"0000000000000000000000000000000000000000">> = bin_to_hex:hexstring(<<0:160/big-unsigned-integer>>),
  <<"0000000000000000000000000000000000000064">> = bin_to_hex:hexstring(<<100:160/big-unsigned-integer>>),
  <<"ffffffffffffffffffffffffffffffffffffffff">> = bin_to_hex:hexstring(<<-1:160/big-unsigned-integer>>),
  <<"0000000000000000000000000000000000000400">> = bin_to_hex:hexstring(<<1024:160/big-unsigned-integer>>).

hexstring_256_test() ->
  <<"0000000000000000000000000000000000000000000000000000000000000000">> = bin_to_hex:hexstring(<<0:256/big-unsigned-integer>>),
  <<"0000000000000000000000000000000000000000000000000000000000000064">> = bin_to_hex:hexstring(<<100:256/big-unsigned-integer>>),
  <<"ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff">> = bin_to_hex:hexstring(<<-1:256/big-unsigned-integer>>),
  <<"0000000000000000000000000000000000000000000000000000000000000400">> = bin_to_hex:hexstring(<<1024:256/big-unsigned-integer>>).

hexstring_512_test() ->
  <<"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000">> = bin_to_hex:hexstring(<<0:512/big-unsigned-integer>>),
  <<"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000064">> = bin_to_hex:hexstring(<<100:512/big-unsigned-integer>>),
  <<"ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff">> = bin_to_hex:hexstring(<<-1:512/big-unsigned-integer>>),
  <<"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000400">> = bin_to_hex:hexstring(<<1024:512/big-unsigned-integer>>).

