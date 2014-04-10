%%%-------------------------------------------------------------------
%%% @author mat
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Apr 2014 1:30 PM
%%%-------------------------------------------------------------------
-module(config_provider_tests).
-author("mat").

-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

config_provider_env_test_() ->
  {"test config provider from environment variables", ?setup(fun env_test/1)}.

config_provider_application_env_test_() ->
  {"test config provider from application env variables", ?setup(fun application_env_test/1)}.

config_provider_config_file_test_() ->
  {"test config provider from config file",
    {
      setup,
      fun start_file/0,
      fun stop_file/1,
      fun file_test/1
    }
  }.

%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP/TEARDOWN %%%
%%%%%%%%%%%%%%%%%%%%%%

inner_start() ->
  case config_provider:start_link() of
    {ok, Pid} -> Pid;
    {error, {already_started, Pid}} -> Pid
  end.

start() ->
  application:load(application:get_application()),
  application:unset_env(application:get_application(), key),
  os:unsetenv("KEY"),
  os:unsetenv("CRATE_CONFIG_FILE"),
  inner_start().

stop(_) ->
  {ok, Cwd} = file:get_cwd(),
  file:delete(filename:absname_join(Cwd, "test.conf")),
  config_provider:stop().

start_file() ->
  application:load(application:get_application()),
  application:unset_env(application:get_application(), key),
  os:unsetenv("KEY"),
  {ok, Cwd} = file:get_cwd(),
  FileName = filename:absname_join(Cwd, "test.conf"),
  ok = file:write_file(FileName, [
    <<"{<<\"key\">>, <<\"value\">>}.\n">>,
    <<"{\"string\", \"value\"}.\n">>,
    <<"{atom, value}.\n">>,
    <<"{number, 1}.\n">>
  ]),
  os:putenv("CRATE_CONFIG_FILE", FileName),
  inner_start(),
  FileName.

stop_file(FileName) ->
  stop(FileName),
  file:delete(FileName).



%%%%%%%%%%%%%
%%% TESTS %%%
%%%%%%%%%%%%%

env_test(_) ->
  os:putenv("KEY", "VALUE"),
  [
    ?_assertEqual(<<"VALUE">>, config_provider:get("KEY")),
    ?_assertEqual(<<"VALUE">>, config_provider:get(key)),
    ?_assertEqual(<<"VALUE">>, config_provider:get(<<"KEY">>)),
    ?_assertEqual(<<"VALUE">>, config_provider:get("KEX", <<"VALUE">>)),
    ?_assertEqual(undefined, config_provider:get("KEX"))
  ].

application_env_test(_) ->
  application:set_env(application:get_application(), key, "value"),
  [
    ?_assertEqual(<<"value">>, config_provider:get(key)),
    ?_assertEqual(<<"value">>, config_provider:get("key")),
    ?_assertEqual(<<"value">>, config_provider:get(<<"key">>)),
    ?_assertEqual(undefined, config_provider:get(kex))
  ].

file_test(_) ->
  [
    ?_assertEqual(<<"value">>, config_provider:get(<<"key">>)),
    ?_assertEqual(<<"value">>, config_provider:get(key)),
    ?_assertEqual(<<"value">>, config_provider:get("key")),
    ?_assertEqual(undefined, config_provider:get(kex)),
    ?_assertEqual("value", config_provider:get("string")),
    ?_assertEqual(value, config_provider:get(atom)),
    ?_assertEqual(1, config_provider:get(number))
  ].

