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
