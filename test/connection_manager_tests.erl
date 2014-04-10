%%%-------------------------------------------------------------------
%%% @author mat
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Apr 2014 9:53 AM
%%%-------------------------------------------------------------------
-module(connection_manager_tests).
-author("mat").

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
  ?assert(true).

connection_manager_test_() ->
  {"test connection manager",
    {foreach,
      fun start/0, % setup function
      fun stop/1,
      [
        fun single_server_test/1
      ]
    }
  }.

%%% setup
start() ->
  os:putenv("CRATE_SERVERS", "localhost:4200"),
  config_provider:start_link(),
  connection_manager:start_link().

%% teardown
stop(_) ->
  config_provider:stop(),
  connection_manager:stop(),
  timer:sleep(100).

single_server_test(_) ->
  [
    ?_assertEqual(connection_manager:get_server(), {ok, <<"localhost:4200">>})
  ].

parse_server_string_test_() ->
  [
    ?_assertEqual(
      connection_manager:parse_servers_string("localhost:123, host:456"),
      [<<"localhost:123">>, <<"host:456">>])
  ].
