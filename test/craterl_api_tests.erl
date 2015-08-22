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

-include("../src/craterl_priv.hrl").
-include_lib("eunit/include/eunit.hrl").

setup() ->
  meck:new(craterl_sql),
  meck:expect(craterl_sql, sql_request,
    fun
      (#sql_request{args = Args, includeTypes = IncludeTypes}, #craterl_server_conf{address = {Host, Port}, config=_Config}) ->
        %% return the args as a single row
        case Args of
          [error] -> {error, bla};
          [invalid_response] -> {ok, invalid};
          [sql_error] -> {error, #sql_error{message = <<"oops">>, code=5000}};
           _ -> {ok, #sql_response{rowCount = 1, rows = [[Args, Host, Port, IncludeTypes]]}}
        end;
      (#sql_bulk_request{bulk_args = BulkArgs, includeTypes = IncludeTypes}, _ServerConf) ->
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
      (_BlobRequest = #blob_request{method = get, payload = {file, FilePath}}, _ServerConf) ->
        % blog_get_to_file
        case FilePath of
          <<"error">> -> {ok, invalid};
          _ ->
            {ok, File} = file:open(FilePath, [write]),
            file:write(File, <<"content">>),
            {ok, FilePath}
        end;
      (_BlobRequest = #blob_request{method = get, digest = Digest}, _ServerConf) ->
        % blob_get
        case Digest of
          <<"invalid">> -> {ok, invalid};
          <<"error">> -> {error, bla};
          _ -> {ok, fun() -> {ok, <<"content">>} end }
        end;
      (_BlobRequest = #blob_request{method = head, digest = Digest}, _ServerConf) ->
        % blob_get
        case Digest of
          <<"invalid">> -> {ok, invalid};
          <<"error">> -> {error, bla};
          _ -> {ok, exists}
        end;
      (_BlobRequest = #blob_request{method = delete, digest = Digest}, _ServerConf) ->
        % blob_get
        case Digest of
          <<"invalid">> -> {ok, invalid};
          <<"error">> -> {error, bla};
          _ -> {ok, deleted}
        end;

      (_BlobRequest = #blob_request{method = put, table = Table, payload = Payload}, _ServerConf) ->
        case Payload of
          {data, Content} ->
            % blob_put
            case Content of
              <<"invalid">> -> {ok, bla};
              _ ->
                {ok, Hash} = craterl_hash:sha1Hex(Content),
                {ok, {created, Hash}}
            end;
          {file, FilePath} ->
            % blob put file
            case Table of
              <<"error">> -> {error, bla};
              _ ->
                {ok, Hash} = craterl_hash:sha1HexFile(FilePath),
                {ok, {created, Hash}}
            end
        end
    end
  ),
  setup_new(),
  RandomPath2 = list_to_binary(integer_to_list(erlang:phash2(make_ref()))),
  ok = file:write_file(RandomPath2, <<"content">>),
  RandomPath2.

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
    fun () ->
      application:stop(craterl),
      application:stop(hackney),
      application:stop(goldrush),
      ok
    end,
    fun (_) ->
      application:stop(craterl),
      application:stop(hackney),
      application:stop(goldrush),
      ok
    end,
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
        ?_assertEqual(string, craterl:new({local, string}, ["localhost:4200"], [])),
        ?_assertEqual(binary, craterl:new({local, binary}, [<<"localhost:4200">>], [])),
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
        ?_assertEqual(
          {error, bla},
          craterl:sql_bulk(<<"select * from sys.cluster">>, [[error]], true) %% trigger error
        ),
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


blob_test_() -> {
  "test that blob operations work correctly",
  {
    setup,
    fun setup/0,
    fun teardown/1,
    fun (ContentFilePath) ->
      ClientRef = craterl:new(),
      {ok, Fun} = craterl:blob_get(<<"mytable">>, <<"123456">>),
      {ok, Fun2} = craterl:blob_get(ClientRef, <<"blobtable">>, <<"123456">>),
      RandomPath = list_to_binary(integer_to_list(erlang:phash2(make_ref()))),
      [
        ?_assertEqual({ok, <<"content">>}, Fun()),
        ?_assertEqual({ok, <<"content">>}, Fun2()),

        ?_assertEqual({error, {invalid_response, invalid}}, craterl:blob_get(<<"mytable">>, <<"invalid">>)), %% trigger error
        ?_assertEqual({error, bla}, craterl:blob_get(<<"mytable">>, <<"error">>)), %% trigger error

        ?_assertEqual({ok, {created, <<"040f06fd774092478d450774f5ba30c5da78acc8">>}}, craterl:blob_put(<<"mytable">>, <<"content">>)),
        ?_assertEqual({error, {invalid_response, bla}}, craterl:blob_put(<<"mytable">>, <<"invalid">>)), %% trigger invalid response

        ?_assertEqual({ok, RandomPath}, craterl:blob_get_to_file(<<"mytable">>, <<"12345">>, RandomPath)),
        ?_assertEqual({ok, <<"content">>}, file:read_file(RandomPath)),
        ?_assertEqual({error, {invalid_response, invalid}}, craterl:blob_get_to_file(<<"mytable">>, <<"12345">>, <<"error">>)),

        ?_assertEqual(ok, craterl:blob_exists(<<"mytable">>, <<"12345">>)),
        ?_assertEqual({error, bla}, craterl:blob_exists(<<"mytable">>, <<"error">>)),
        ?_assertEqual({error, {invalid_response, invalid}}, craterl:blob_exists(<<"mytable">>, <<"invalid">>)),

        ?_assertEqual({ok, {created, <<"040f06fd774092478d450774f5ba30c5da78acc8">>}}, craterl:blob_put_file(<<"mytable">>, ContentFilePath)),
        ?_assertEqual({error, bla}, craterl:blob_put_file(<<"error">>, ContentFilePath)),
        ?_assertEqual({error, enoent}, craterl:blob_put_file(<<"mytable">>, <<"does_not_exist">>)),

        ?_assertEqual(ok, craterl:blob_delete(<<"mytable">>, <<"040f06fd774092478d450774f5ba30c5da78acc8">>)),
        ?_assertEqual({error, {invalid_response, invalid}}, craterl:blob_delete(<<"mytable">>, <<"invalid">>))

      ]
    end
  }
}.

