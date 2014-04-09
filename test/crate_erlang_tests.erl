-module(crate_erlang_tests).
-include_lib("eunit/include/eunit.hrl").

start_test() ->
    crate_erlang:start_link().


