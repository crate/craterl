%%%-------------------------------------------------------------------
%%% @author mat
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Apr 2014 6:45 PM
%%%-------------------------------------------------------------------
-module(crate_request_handler_tests).
-author("mat").

-include("crate_erlang.hrl").
-include_lib("eunit/include/eunit.hrl").

simple_test() ->
  ?assert(true).

server_uri_from_spec_test() ->
  ?assertEqual(
    <<"http://localhost:4200/_sql">>,
    crate_request_handler:create_server_url(<<"localhost:4200">>)),
  ?assertEqual(<<"http://bla:123/_sql">>, crate_request_handler:create_server_url(<<"http://bla:123">>)),
  ?assertEqual(<<"https://secure:22/_sql">>, crate_request_handler:create_server_url(<<"https://secure:22">>))
.

create_payload_test() ->
  ?assertEqual(<<"{\"stmt\":\"select * from craty\",\"args\":[]}">>,
    crate_request_handler:create_payload(<<"select * from craty">>, [])).

build_response_test() ->
  ?assertEqual(#sql_response{rows = [[1,2,3],[4,5,6]], cols=[<<"x">>, <<"y">>, <<"z">>], rowCount = 2, duration = 4},
    crate_request_handler:build_response(<<"{\"rows\":[[1,2,3],[4,5,6]], \"cols\":[\"x\",\"y\",\"z\"], \"rowcount\":2, \"duration\":4}">>)
  ),
  ?assertEqual(#sql_response{},
    crate_request_handler:build_response(<<"{\"some\":\"weird\", \"stuff\":1}">>)
  ).

build_error_response_test() ->
  ?assertEqual(#sql_error{code = 1000, message = <<"DAU Exception">>},
    crate_request_handler:build_error_response(<<"{\"error\":{\"message\":\"DAU Exception\",\"code\":1000}}">>)
  ),
  ?assertEqual(#sql_error{}, crate_request_handler:build_error_response(<<"{}">>)).
