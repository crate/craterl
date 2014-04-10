-module(crate_erlang_tests).
-include_lib("eunit/include/eunit.hrl").

sql_select_simple_test() ->
    connection_manager:start_link([{<<"localhost">>, 4200}]),
    {ok, _Result} = crate_erlang:sql(<<"select * from craty">>).
