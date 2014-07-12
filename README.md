crate-erlang
============

https://travis-ci.org/crate/craterl.svg?branch=master

Erlang client for crate.

Compatibility
-------------

Tested with Erlang ``R16B03-1`` .

Installation
------------

```
rebar get-deps
rebar compile
```

Shell
-----

Get all the stuff on an erlang shell:

```
erl -pa ebin deps/*/ebin
```

Use craterl from erlang shell, for testing, debugging etc.:

```
1> craterl:start()
ok
2> craterl:sql(<<"select * from tweets where id=?">>, [42]).
{ok,{sql_response,[<<"created_at">>,<<"id">>,
                  <<"retweeted">>,<<"source">>,<<"text">>,<<"user">>],
                  [[1397001600000,<<"42">>,null,null,<<"hello world">>,
                  [{<<"id">>,<<"petersabaini">>}]]],
                  1,3}}
```

Tests
-----

Simply call ```priv/ci/run_tests.sh``` to run the tests for craterl.
