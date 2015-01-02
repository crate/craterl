# Craterl #


[![Build Status](https://travis-ci.org/crate/craterl.svg?branch=master)](https://travis-ci.org/crate/craterl)
![Another Badge](http://img.shields.io/badge/another-badge-green.svg)

Erlang client for crate.

This client is in an alpha state and, though tested very intensively,
not yet verified to be rock-solid in a production environment.

## Compatibility ##

Tested with Erlang ``R16B03-1`` and ``17``.

## Installation ##

Calling make should be enough to get you going:

```
make
```

## Usage ##


If the ```craterl``` application has not yet been started,
start it using:

```erlang
ok = craterl:start().
```

Having a running ``craterl`` application, the next step is starting a client
instance using a variant of the ```craterl:new()``` function:

```erlang
ClientSpec = {local, my_client}.
Servers = [{<<"localhost">>, 4200}, "localhost:4201"].
Options = [{poolsize, 1000}, {timeout, 5000}].
ClientRef = craterl:new(ClientSpec, Servers, Options).
```

It is possible to create many clients on one erlang node.
```craterl``` client instances are created using a client spec which is
a tuple you would use when registering a process, like ```{local, your_name}```.
The process name, ```your_name``` in this example, must be unique on a node.

### Options ###

The following options can be used to change the behaviour of a newly created craterl client:

* poolname - ```string()``` or ```binary()```, the name of the connection pool, handled by hackney (erlang http client)
* poolsize - ```integer()```, the size of the connection pool, also handled by hackney
* timeout - ```integer()```, the receive and connect timeout for connections to crate servers, in milliseconds
* ssl_options - ```term()```, the same options the erlang ssl module accepts as ```ssloptions()```
* ssl_insecure - ```boolean()```, whether ssl certificates should be validated or not

Example:

```erlang
Options = [{poolname, "my_pool"}, {poolsize, 200}, {timeout, 6000}, {ssl_insecure, false}, {ssl_options, [{cipers, [{rsa, aes_256_cbc, sha}]}, {cacerts, MyDerEncodedCaCerts}]}].
ClientRef = craterl:new(craterl, [{<<"localhost", 4200}], Options).
```

See the documentation of the ```craterl``` module for more detailed api documentation.

### SQL ###

Issuing SQL statements using craterl is possible with one of the variants of ```craterl:sql()```:

```erlang
{ok Response} = craterl:sql("select id, name from sys.cluster").
{ok, #sql_response{cols = [<<"id">>,<<"name">>],
                  colTypes = [],
                  rows = [[<<"89b8f6bf-4082-415b-937e-7de66b67f6fe">>,
                           <<"crate">>]],
                  rowCount = 1,duration = 12}}
{ok, Response2} = craterl:sql(ClientRef, <<"select * from user where id in (?, ?, ?)">>, [1, 2, 3]).
Request = #sql_request{stmt = <<"select count(*) from sys.nodes">>}.
#sql_request{stmt = <<"select count(*) from sys.nodes">>,
             args = [],includeTypes = false}
{ok, Response3} = craterl:sql().
{ok,#sql_response{cols = [<<"count(*)">>],
                  colTypes = [],
                  rows = [[1]],
                  rowCount = 1,duration = 240}}
```

For issuing multiple INSERT / DELETE or UPDATE requests, the ```craterl:sql_bulk()```
functions can be used:

```erlang
{ok, BulkResponse} = craterl:sql_bulk(ClientRef, <<"insert into t (id, name) values (?, ?)">>, [[1, <<"Ford">>], [2, <<"Trillian">>], [3, <<"Zaphod">>]]).
{ok,#sql_bulk_response{cols = [],colTypes = [],
                       results = [[{<<"rowcount">>,1}],
                                  [{<<"rowcount">>,1}],
                                  [{<<"rowcount">>,1}]],
                       duration = 135}}
{ok, BulkResponse2} = craterl:sql_bulk(<<"update t set new_column=? where id=?">>, [[<<"funky">>, 1], [<<"shizzle">>, 2], [<<"indeed">>, 3]]).
02:00:13.409 [info] getserver
{ok,#sql_bulk_response{cols = [],colTypes = [],
                       results = [[{<<"rowcount">>,1}],
                                  [{<<"rowcount">>,1}],
                                  [{<<"rowcount">>,1}]],
                       duration = 47}}
craterl:sql("select * from t").
{ok,#sql_response{cols = [<<"id">>,<<"name">>,
                          <<"new_column">>],
                  colTypes = [],
                  rows = [[1,<<"Ford">>,<<"funky">>],
                          [2,<<"Trillian">>,<<"shizzle">>],
                          [3,<<"Zaphod">>,<<"indeed">>]],
                  rowCount = 3,duration = 59}}
```

Every sql api function has a variant that accepts a ```ClientRef``` as first 
argument that is an atom referencing a ```craterl``` client that has been 
started using a variant ```craterl:new()``` in order to issue your request 
against a specific server / cluster.

### Blobs ###

Crate is able to store blobs, files, binary somethings. They can be replicated to 
make sure you won't lose data.

Craterl fully supports storing, retrieving and manipulating blobs.

At first a blob table is needed to store blobs into:

```erlang
{ok, SqlResponse} = craterl:sql("create blob table myblobs with (number_of_replicas=1)").
```

It is possible to upload blobs from stuff you have available in ram or from a file:
 
```erlang
Content = <<"awesome!">>.
{ok, {created, HashDigest}} = craterl:blob_put(<<"myblobs">>, Content).
HashDigest = <<"040f06fd774092478d450774f5ba30c5da78acc8">>.

File = <<"/usr/share/dict/words">>
{ok,{created, Hash}} = craterl:blob_put_file(ClientRef, <<"myblobs">>, <<"/usr/share/dict/words">>).
WordsHash = <<"a62edf8685920f7d5a95113020631cdebd18a185">>.
```

You can get blobs to memory or to file. Both method will make use of chunked HTTP
encoding so you will receive the blob piece by piece and won't load big blobs into memory at once.

```erlang
{ok, GetDataFun} = craterl:blob_get(<<"myblobs">>, WordsHash).
GetDataFun()
{ok,<<"A\na\naa\naal\naalii\naam\nAani\naardvark\naardwolf\nAaron\nAaronic\nAaronical\nAaronite\nAaronitic\nAaru\nAb\naba\nAbabdeh\nA"...>>}
GetDataFun()
{ok,<<"inoposterior\nabdominoscope\nabdominoscopy\nabdominothoracic\nabdominous\nabdominovaginal\nabdominovesical\nabduce\n"...>>}
...
GetDataFun()
{ok, done}

NewWordsFile = <<"/tmp/blobwords">>
{ok, NewWordsFile} = craterl:blob_get_to_file(<<"myblobs">>, WordsHash, NewWordsFile).
file:read_file(NewWordsFile)
{ok,<<"A\na\naa\naal\naalii\naam\nAani\naardvark\naardwolf\nAaron\nAaronic\nAaronical\nAaronite\nAaronitic\nAaru\nAb\naba\nAbabdeh\nA"...>>}
```

Check for existence of blobs:

```erlang
ok = craterl:blob_exists(ClientRef, <<"myblobs">>, WordsHash).
{error, 404} = craterl:blob_exists(craterl, <<"myblobs">>, <<"doesnotexist">>).
```

Delete blobs:

```erlang
ok = craterl:blob_delete(<<"myblobs">>, WordsHash).
{error, 404} = craterl:blob_delete(<<"myblobs">>, WordsHash).
```

Every blob api function, like the sql functions,  has a variant that accepts 
a ```ClientRef``` as first argument that is an atom referencing a ```craterl``` 
client that has been started using a variant ```craterl:new()```.

## Tests ##


Simply call ```make test``` to run the unit and integration tests for ```craterl```.


## Contributions ##

Are very welcome, be it code, constructive feedback, money, or some motivating words!
