-module(query_SUITE).

-include_lib("common_test/include/ct.hrl").

-include("craterl.hrl").

-export([all/0,
	 init_per_suite/1, end_per_suite/1,
	 init_per_testcase/2, end_per_testcase/2]).

-export([simple_query_test_binary/1, simple_query_test_str/1]).

all() ->
  [simple_query_test_str, simple_query_test_binary].

init_per_suite(Config) ->
  ok = hackney:start(),
  ok = craterl:start(),
  Config.

end_per_suite(Config) ->
  application:stop(craterl),
  Config.

init_per_testcase(_, Config) ->
  Config.

end_per_testcase(_, Config) ->
  Config.

simple_query_test_binary(_Config) ->
  {ok, Response} = craterl:sql(<<"select id, name from sys.cluster">>),
  [<<"id">>, <<"name">>] = Response#sql_response.cols.

simple_query_test_str(_Config) ->
  {ok, #sql_response{}} = craterl:sql("select * from sys.cluster").
