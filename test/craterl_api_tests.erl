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

-module(craterl_api_tests).
-author("Matthias Wahl").

-include("craterl.hrl").
-include_lib("eunit/include/eunit.hrl").

setup() ->
  meck:new(craterl_sql),
  meck:expect(craterl_sql, sql_request,
    fun
      (#sql_request{args = Args, includeTypes = IncludeTypes}, {Host, Port}) ->
        %% return the args as a single row
        case Args of
          [error] -> {error, bla};
          [invalid_response] -> {ok, invalid};
          [sql_error] -> {error, #sql_error{message = <<"oops">>, code=5000}};
           _ -> {ok, #sql_response{rowCount = 1, rows = [[Args, Host, Port, IncludeTypes]]}}
        end;
      (#sql_bulk_request{bulk_args = BulkArgs, includeTypes = IncludeTypes}, _ServerSpec) ->
        %% return the length of the bulk args in a single result
        case BulkArgs of
          [[error]] -> {error, bla};
          [[invalid_response]] -> {ok, invalid};
          [[sql_error]] -> {error, #sql_error{message = <<"oops">>, code=5000}};
          _ ->
            RowCount = case IncludeTypes of
              true -> length(BulkArgs) + 1;
              false -> length(BulkArgs)
            end,
            {ok, #sql_bulk_response{results = [#sql_bulk_result{rowCount = RowCount}]}}
        end
    end
  ),
  meck:new(craterl_blob),
  meck:expect(craterl_blob, blob_request,
    fun
      (_BlobRequest = #blob_request{}, _ServerSpec) ->
        {ok, <<"123456">>}
    end
  ),
  io:format("~p~n", [setup_new()]),
  ok.

teardown(Args) ->
  teardown_new(Args),
  meck:validate(craterl_sql),
  meck:validate(craterl_blob),
  meck:unload().

setup_new() ->
  case whereis(craterl_sup) of
    undefined ->
      ok = hackney:start(),
      ok = hackney_pool:start(),
      {ok, Pid} = craterl_sup:start_link(),
      Pid;
    Pid when is_pid(Pid) -> Pid
  end.

teardown_new(_) ->
  hackney:stop(),
  ssl:stop(),
  case whereis(craterl_sup) of
    Pid when is_pid(Pid) ->
      exit(Pid, normal),
      ok;
    undefined -> ok
  end.

start_app_test_() -> {
  "test that starting crate works correctly",
  {
    setup,
    fun () -> ok end,
    fun (_) ->
      application:stop(craterl),
      application:stop(hackney),
      application:stop(goldrush),
      application:stop(lager),
      ok end,
    fun (_) ->
      [
        ?_assertEqual(ok, craterl:start()),
        ?_assertEqual(true,
          lists:member(craterl,
            lists:map(
              fun ({AppName, _Desc, _Vsn}) -> AppName end,
              application:loaded_applications())
          )
        ),
        ?_assertEqual(ok, craterl:start())
      ]
    end
  }
}.

new_test_() -> {
  "test that creating new clients works as expected",
  {
    setup,
    fun setup_new/0,
    fun teardown_new/1,
    fun(_) ->
      [
        ?_assertEqual({error, not_found}, craterl:stop_client(craterl)),
        ?_assertEqual(craterl, craterl:new()),
        ?_assertEqual({error, {already_started, craterl}}, craterl:new()),
        ?_assertEqual(ok, craterl:stop_client(craterl)),
        ?_assertEqual([], supervisor:which_children(craterl_sup)),
        ?_assertEqual(craterl, craterl:new([?CRATERL_DEFAULT_SERVER])),
        ?_assertEqual({error, {already_started, craterl}}, craterl:new([?CRATERL_DEFAULT_SERVER], [{poolsize, 100}])),
        ?_assertEqual(ok, craterl:stop_client(craterl)),
        ?_assertEqual(craty, craterl:new({local, craty}, [?CRATERL_DEFAULT_SERVER], [])),
        ?_assertEqual({error, {already_started, craty}}, craterl:new({local, craty}, [{<<"localhost">>, 12345}], []))
      ]
    end
  }
}.

sql_test_() -> {
  "test that issueing sql statements/requests works as expected",
  {
    setup,
    fun setup/0,
    fun teardown/1,
    fun (_) ->
      [
        ?_assertEqual(craterl, craterl:new()),
        ?_assertEqual({ok, #sql_response{
              rowCount = 1,
              rows = [[
                [],
                <<"localhost">>,
                4200,
                false
              ]]
            }
          },
          craterl:sql(<<"select id from sys.cluster">>)),
        ?_assertEqual({ok, #sql_response{
              rowCount = 1,
              rows = [[
                [],
                <<"localhost">>,
                4200,
                false
              ]]
            }
          },
          craterl:sql("select id from sys.cluster")),
        ?_assertEqual({ok, #sql_response{
              rowCount = 1,
              rows = [[
                [],
                <<"localhost">>,
                4200,
                false
              ]]
            }
          },
          craterl:sql(#sql_request{stmt = <<"select id from sys.cluster">>, args=[]})),
        ?_assertEqual({ok, #sql_response{
              rowCount = 1,
              rows = [[
                [<<"bla">>],
                <<"localhost">>,
                4200,
                false
              ]]
            }
          },
          craterl:sql(<<"select id from sys.cluster where name=?">>, [<<"bla">>])),
         ?_assertEqual({ok, #sql_response{
              rowCount = 1,
              rows = [[
                [<<"bla">>],
                <<"localhost">>,
                4200,
                false
              ]]
            }
          },
          craterl:sql("select id from sys.cluster where name=?", [<<"bla">>])),
        ?_assertExit(
          {noproc,{gen_server,call,[craty,get_server]}},
          craterl:sql(craty, <<"select * from sys.cluster">>, [], true)
        ),
        ?_assertEqual(
          {error, bla},
          craterl:sql(<<"select * from sys.cluster">>, [error], true) %% trigger error
        ),
        ?_assertEqual(
          {error, "No active server"},
          craterl:sql(<<"select * from sys.cluster">>, [], true)
        ),
        ?_assertEqual(ok, craterl:stop_client(craterl)),
        ?_assertEqual(craterl, craterl:new()),
        ?_assertEqual({error, #sql_error{message = <<"oops">>, code=5000}}, craterl:sql(<<"select * from sys.cluster">>, [sql_error])),
        ?_assertEqual({error, {invalid_response, invalid}}, craterl:sql(<<"select * from sys.cluster">>, [invalid_response]))
      ]
    end
  }
}.

sql_bulk_test_() -> {
  "test that issueing bulk sql statements works as expected",
  {
    setup,
    fun setup/0,
    fun teardown/1,
    fun (_) -> [
        ?_assertEqual(craterl, craterl:new()),
        ?_assertEqual({ok, #sql_bulk_response{
                results = [#sql_bulk_result{rowCount = 2}]
              }},
              craterl:sql_bulk(
                <<"insert into test values (?, ?)">>,
                [[1, <<"abc">>], [2, <<"def">>]]
              )
        ),
        ?_assertEqual({ok, #sql_bulk_response{
                results = [#sql_bulk_result{rowCount = 2}]
              }},
              craterl:sql_bulk(
                "insert into test values (?, ?)",
                [[1, <<"abc">>], [2, <<"def">>]]
              )
        ),
        ?_assertEqual({ok, #sql_bulk_response{
                results = [#sql_bulk_result{rowCount = 3}]
              }},
              craterl:sql_bulk(
                <<"insert into test values (?, ?)">>,
                [[1, <<"abc">>], [2, <<"def">>]],
                true
              )
        ),
        ?_assertEqual({ok, #sql_bulk_response{
                results = [#sql_bulk_result{rowCount = 3}]
              }},
              craterl:sql_bulk(#sql_bulk_request{
                  stmt = <<"insert into test values (?, ?)">>,
                  bulk_args = [[1, <<"abc">>], [2, <<"def">>]],
                  includeTypes = true
                }
              )
        ),
        ?_assertEqual(
          {error, bla},
          craterl:sql_bulk(<<"select * from sys.cluster">>, [[error]], true) %% trigger error
        ),
        ?_assertEqual(
          {error, "No active server"},
          craterl:sql_bulk(<<"select * from sys.cluster">>, [[]])
        ),
        ?_assertEqual(ok, craterl:stop_client(craterl)),
        ?_assertEqual(craterl, craterl:new()),
        ?_assertEqual(
          {error, #sql_error{message = <<"oops">>, code = 5000}},
          craterl:sql_bulk(<<"select * from sys.cluster">>, [[sql_error]], true) %% trigger error
        ),
        ?_assertEqual(
          {error, {invalid_response, invalid}},
          craterl:sql_bulk(<<"select * from sys.cluster">>, [[invalid_response]], true) %% trigger error
        )
      ]
    end
  }
}.
