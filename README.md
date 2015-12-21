# Craterl #

[![Join the chat at https://gitter.im/crate/craterl](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/crate/craterl?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)


[![Build Status](https://travis-ci.org/crate/craterl.svg?branch=master)](https://travis-ci.org/crate/craterl)
![Another Badge](http://img.shields.io/badge/another-badge-green.svg)

Erlang client for crate.

This client is in an alpha state and, though tested very intensively,
not yet verified to be rock-solid in a production environment.

## Compatibility ##

Tested with OTP releases ``R16B``, ``17`` and ``18``.

## Installation

### Rebar 3

Craterl is built using [rebar3](https://github.com/rebar/rebar3), 
using it in your project is highly recommended. 

Here is how you'd add it to your dependencies.

If you want to load a craterl release from [hex.pm](https://hex.pm/packages/craterl), configure it like so:

```erlang
{deps,[
  {craterl,"0.2.3"}
]}.
```

Or load it directly from github:

```erlang
{deps,[
    {craterl, {git, "git://github.com/crate/craterl.git", {tag, "0.2.3"}}}
]}.
```

### Rebar 2

You can also use craterl in your project using [rebar2](https://github.com/rebar/rebar),
though [rebar3](https://github.com/rebar/rebar3) is recommended.

With rebar2 you need to load craterl from github. 
Use a git tag or a commit ref in order to get reproducable builds:

```erlang
{deps,[
    {craterl, "0.2.3", {git, "git://github.com/crate.craterl.git", {tag, "0.2.3}}}
]}.
```


### Mix

Craterl releases are hosted on [hex.pm](https://hex.pm/packages/craterl).

Declare it as a dependency in your ``mix.exs`` file:

```elixir
def deps do
    [{:craterl, "~> 0.2.3"}]
end
```

Or load it directly from github:

```elixir
def deps do
    [{:craterl, git: "git://github.com/crate/craterl.git", tag: "0.2.3"}]
end
``` 

## Usage ##

Normally you'd start craterl as part of your application's startup, so the 
Erlang/OTP application framework will take care of actually starting craterl.

However, if you want to play around with craterl on the erlang or
elixir shell you can start it up with the following convenience method:

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
Options = [
    {poolname, "my_pool"}, 
    {poolsize, 200}, {timeout, 6000}, 
    {ssl_insecure, false}, 
    {ssl_options, [
        {cipers, [{rsa, aes_256_cbc, sha}]}, 
        {cacerts, MyDerEncodedCaCerts}
    ]}
].
ClientRef = craterl:new({local, craterl}, [{<<"localhost">>, 4200}], Options).
```

See the documentation of the ```craterl``` module for more detailed api documentation.

### SQL ###

Issuing SQL statements using craterl is possible with one of the variants 
of ```craterl:sql()```:

```erlang
{ok Response} = craterl:sql("select id, name from sys.cluster").
[[<<"89b8f6bf-4082-415b-937e-7de66b67f6fe">>, <<"crate">>]] = craterl_resp:rows(Response).
[<<"id">>,<<"name">>] = craterl_resp:column_names(Response).

Stmt = <<"select * from user where id in (?, ?, ?)">>.
Args = [1, 2, 3].
{ok, Response2} = craterl:sql(ClientRef, Stmt, Args).
```

For issuing multiple INSERT / DELETE or UPDATE requests with one roundtrip, 
the ```craterl:sql_bulk()``` functions can be used:

```erlang
Stmt = <<"insert into t (id, name) values (?, ?)">>.
BulkArgs = [[1, <<"Ford">>], [2, <<"Trillian">>], [3, <<"Zaphod">>]].
{ok, BulkResponse} = craterl:sql_bulk(ClientRef, Stmt, BulkArgs).
BulkResults = craterl_resp:bulk_results(BulkResponse).
[1,1,1] = lists:map(fun craterl_resp:row_count/1, BulkResults).

Stmt2 = <<"update t set new_column=? where id=?">>.
BulkArgs2 = [[<<"funky">>, 1], [<<"shizzle">>, 2], [<<"indeed">>, 3]].
{ok, BulkResponse2} = craterl:sql_bulk(Stmt2, BulkArgs2).
BulkResults2 = craterl_resp:bulk_results(BulkResponse2).
[1,1,1] = lists:map(fun craterl_resp:row_count/1, BulkResults2).

{ok, SelectResponse} = craterl:sql("select * from t").
[[1,<<"Ford">>,<<"funky">>],
 [2,<<"Trillian">>,<<"shizzle">>],
 [3,<<"Zaphod">>,<<"indeed">>]] = craterl_resp:rows(SelectResponse).
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
{ok,{created, WordsHash}} = craterl:blob_put_file(ClientRef, <<"myblobs">>, <<"/usr/share/dict/words">>).
WordsHash = <<"a62edf8685920f7d5a95113020631cdebd18a185">>.
```

You can get blobs to memory or to file. Both methods will make use of chunked HTTP
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
client that has been started using a variant of ```craterl:new()```.

## Tests ##


Simply call ```make test``` to run the unit and integration tests for ```craterl```.


## Contributions ##

Are very welcome, be it code, constructive feedback, money, or some motivating words!
