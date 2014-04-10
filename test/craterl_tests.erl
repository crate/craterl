-module(craterl_tests).
-include_lib("eunit/include/eunit.hrl").

-define(setup(F, Start), {setup, Start, fun stop/1, F}).


simple_test() ->
  ?assert(true).

sql_select_simple_test_() ->
    {setup,
     fun start_craterl/0,
     fun stop/1,
     fun simple_query_test/1}.

sql_select_simple_test_str_params_test_() ->
    {setup,
     fun start_craterl/0,
     fun stop/1,
     fun simple_query_test_str_params/1}.

start_craterl() ->
    craterl:start().

stop(_) ->
    application:stop(craterl),
    timer:sleep(100).

simple_query_test(_) ->
    [
     ?_assertMatch({ok, {sql_response, _Cols, _Rows, _RowCnt, _Dur, _Wall, _RunT}},
                    craterl:sql(<<"select * from sys.cluster">>))
     
    ].

simple_query_test_str_params(_) ->
    [
     ?_assertMatch({ok, _},
                   craterl:sql("select * from sys.cluster"))
     
    ].

