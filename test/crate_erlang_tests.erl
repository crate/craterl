-module(crate_erlang_tests).
-include_lib("eunit/include/eunit.hrl").

sql_select_simple_test() ->
    os:putenv("CRATE_SERVERS", "localhost:4200"),
    crate_erlang:start(),
    {error, _Result} = crate_erlang:sql(<<"select * from craty">>).
