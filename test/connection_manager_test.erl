%%%-------------------------------------------------------------------
%%% @author mat
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Apr 2014 9:53 AM
%%%-------------------------------------------------------------------
-module(connection_manager_test).
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
  crate_erlang_sup:start_link(),
  connection_manager:start_link([{<<"localhost">>, 4200}]).

%% teardown
stop(_) -> ok.

single_server_test({ok, _Pid}) ->
  [
    ?_assertEqual(gen_server:call(getserver), {<<"localhost">>, 4200})
  ].
