

# Module craterl #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.
Copyright (c) (C) 2014, CRATE Technology GmbH
Licensed to CRATE Technology GmbH ("Crate") under one or more contributor
license agreements.  See the NOTICE file distributed with this work for
additional information regarding copyright ownership.  Crate licenses
this file to you under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.  You may
obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
License for the specific language governing permissions and limitations
under the License.

However, if you have executed another commercial license agreement
with Crate these terms will supersede the license and you may use the
software solely pursuant to the terms of the relevant commercial agreement.
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#blob_delete-2">blob_delete/2</a></td><td>
delete a blob from a blob table referenced by its hashdigest.</td></tr><tr><td valign="top"><a href="#blob_delete-3">blob_delete/3</a></td><td>
delete a blob from a blob table referenced by its hashdigest
using a specific client.</td></tr><tr><td valign="top"><a href="#blob_exists-2">blob_exists/2</a></td><td>
check if a blob exists given a digest and a blob table.</td></tr><tr><td valign="top"><a href="#blob_exists-3">blob_exists/3</a></td><td>
check if a blob exists given a digest and a blob table
using a specicif client.</td></tr><tr><td valign="top"><a href="#blob_get-2">blob_get/2</a></td><td>
Get a blob by digest from a blob table.</td></tr><tr><td valign="top"><a href="#blob_get-3">blob_get/3</a></td><td>
Get a blob by digest from a blob table
using a specific client.</td></tr><tr><td valign="top"><a href="#blob_get_to_file-3">blob_get_to_file/3</a></td><td>
Get a blob by digest from a blob table right to a given file.</td></tr><tr><td valign="top"><a href="#blob_get_to_file-4">blob_get_to_file/4</a></td><td>
Get a blob by digest from a blob table right to a given file
using a specific client.</td></tr><tr><td valign="top"><a href="#blob_put-2">blob_put/2</a></td><td>
put a blob to the crate server given its content
and the blob table to store it into.</td></tr><tr><td valign="top"><a href="#blob_put-3">blob_put/3</a></td><td>
put a blob to the crate server given its content
and the blob table to store it into
using a specific client.</td></tr><tr><td valign="top"><a href="#blob_put_file-2">blob_put_file/2</a></td><td>
put a blob to the crate server given a filename from which to fetch the content
and the blob table to store it into.</td></tr><tr><td valign="top"><a href="#blob_put_file-3">blob_put_file/3</a></td><td>
put a blob to the crate server given a filename from which to fetch the content
and the blob table to store it into
using a specific client.</td></tr><tr><td valign="top"><a href="#new-0">new/0</a></td><td>
create a new crate client with the default settings and
registration name.</td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td>
start a new craterl client instance with the default name craterl
given a list of crate server instances.</td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td>
create a new craterl client instance with the default name craterl
given a list of crate server instances
and a list of options as a proplist.</td></tr><tr><td valign="top"><a href="#new-3">new/3</a></td><td>
create a new craterl client instance given a client specification
comprised of a tuple of the same kind you would use for a call to register(),
e.g.</td></tr><tr><td valign="top"><a href="#sql-1">sql/1</a></td><td>
issue a SQL statement, with optional arguments
or a prebuilt #sql_request{}
using the default client instance.</td></tr><tr><td valign="top"><a href="#sql-2">sql/2</a></td><td>
issue a SQL statement with optional arguments
or a prebuilt #sql_request{} to a specific client.</td></tr><tr><td valign="top"><a href="#sql-3">sql/3</a></td><td>
issue a SQL statement with optional arguments
and a boolean indicating whether you want to receive type information
for the returned columns.</td></tr><tr><td valign="top"><a href="#sql-4">sql/4</a></td><td>
issue a SQL statement with optional arguments
and a boolean indicating whether you want to receive type information
for the returned columns
to a specific client.</td></tr><tr><td valign="top"><a href="#sql_bulk-1">sql_bulk/1</a></td><td>
issue a Bulk SQL statement using a #sql_bulk_request{}.</td></tr><tr><td valign="top"><a href="#sql_bulk-2">sql_bulk/2</a></td><td>
issue a Bulk SQL statement using a #sql_bulk_request{}
to a specific client.</td></tr><tr><td valign="top"><a href="#sql_bulk-3">sql_bulk/3</a></td><td>
issue a Bulk SQL statement with bulk arguments and
a boolean that determines if the response should contain
column types or not.</td></tr><tr><td valign="top"><a href="#sql_bulk-4">sql_bulk/4</a></td><td>
issue a Bulk SQL statement with bulk arguments and
a boolean that determines if the response should contain
column types or not
to a specific client.</td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td>
start the craterl application and all its dependencies.</td></tr><tr><td valign="top"><a href="#stop_client-1">stop_client/1</a></td><td>
stop a running client instance by giving a client reference,
the return value of new().</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="blob_delete-2"></a>

### blob_delete/2 ###


<pre><code>
blob_delete(BlobTable::binary(), HexDigest::binary()) -&gt; ok | {error, term()}
</code></pre>

<br></br>



delete a blob from a blob table referenced by its hashdigest.

<a name="blob_delete-3"></a>

### blob_delete/3 ###


<pre><code>
blob_delete(ClientName::atom(), BlobTable::binary(), HexDigest::binary()) -&gt; ok | {error, term()}
</code></pre>

<br></br>



delete a blob from a blob table referenced by its hashdigest
using a specific client.

<a name="blob_exists-2"></a>

### blob_exists/2 ###


<pre><code>
blob_exists(BlobTable::binary(), HexDigest::binary()) -&gt; ok | {error, term()}
</code></pre>

<br></br>



check if a blob exists given a digest and a blob table.
Will return ok on success.
<a name="blob_exists-3"></a>

### blob_exists/3 ###


<pre><code>
blob_exists(ClientName::atom(), BlobTable::binary(), HexDigest::binary()) -&gt; ok | {error, term()}
</code></pre>

<br></br>




check if a blob exists given a digest and a blob table
using a specicif client.


Will return ok on success.
<a name="blob_get-2"></a>

### blob_get/2 ###


<pre><code>
blob_get(BlobTable::binary(), HexDigest::binary()) -&gt; {ok, term()}
</code></pre>

<br></br>




Get a blob by digest from a blob table.


This method will return {ok, Fun} in case of success where Fun is
a function returning {ok, BinaryData} as long as there is further
data to fetch. When all data is fetched, it returns {ok, done}.
Using this pattern, it is possible to chunk the response from the server
and not load everything into memory.
<a name="blob_get-3"></a>

### blob_get/3 ###


<pre><code>
blob_get(ClientName::atom(), BlobTable::binary(), HexDigest::binary()) -&gt; {ok, term()}
</code></pre>

<br></br>




Get a blob by digest from a blob table
using a specific client.


This method will return {ok, Fun} in case of success where Fun is
a function returning {ok, BinaryData} as long as there is further
data to fetch. When all data is fetched, it returns {ok, done}.
Using this pattern, it is possible to chunk the response from the server
and not load everything into memory.
<a name="blob_get_to_file-3"></a>

### blob_get_to_file/3 ###


<pre><code>
blob_get_to_file(BlobTable::binary(), HexDigest::binary(), FilePath::binary()) -&gt; {ok, binary()}
</code></pre>

<br></br>



Get a blob by digest from a blob table right to a given file.
It will return {ok, FilePath} where FilePath is the path to the file
where the blob got stored
<a name="blob_get_to_file-4"></a>

### blob_get_to_file/4 ###


<pre><code>
blob_get_to_file(ClientName::atom(), BlobTable::binary(), HexDigest::binary(), FilePath::binary()) -&gt; {ok, binary()}
</code></pre>

<br></br>



Get a blob by digest from a blob table right to a given file
using a specific client.
It will return {ok, FilePath} on success where FilePath is the path to the file
where the blob got stored
<a name="blob_put-2"></a>

### blob_put/2 ###


<pre><code>
blob_put(BlobTable::binary(), Content::binary()) -&gt; {ok, {created, binary()}} | {error, term()}
</code></pre>

<br></br>




put a blob to the crate server given its content
and the blob table to store it into.


this function will create the hash of the content and return it like this
on success: {ok, {created, HashDigest}}. Use the hash digest to refer to
your blob in further requests.

<a name="blob_put-3"></a>

### blob_put/3 ###


<pre><code>
blob_put(ClientName::atom(), BlobTable::binary(), Content::binary()) -&gt; {ok, {created, binary()}} | {error, term()}
</code></pre>

<br></br>




put a blob to the crate server given its content
and the blob table to store it into
using a specific client.


this function will create the hash of the content and return it like this
on success: {ok, {created, HashDigest}}. Use the hash digest to refer to
your blob in further requests.

<a name="blob_put_file-2"></a>

### blob_put_file/2 ###


<pre><code>
blob_put_file(BlobTable::binary(), FilePath::binary()) -&gt; {ok, {created, binary()}} | {error, term()}
</code></pre>

<br></br>




put a blob to the crate server given a filename from which to fetch the content
and the blob table to store it into.


this function will create the hash of the file content and return it like this
on success: {ok, {created, HashDigest}}. Use the hash digest to refer to
your blob in further requests.
<a name="blob_put_file-3"></a>

### blob_put_file/3 ###


<pre><code>
blob_put_file(ClientName::atom(), BlobTable::binary(), FilePath::binary()) -&gt; {ok, {created, binary()}} | {error, term()}
</code></pre>

<br></br>




put a blob to the crate server given a filename from which to fetch the content
and the blob table to store it into
using a specific client.


this function will create the hash of the file content and return it like this
on success: {ok, {created, HashDigest}}. Use the hash digest to refer to
your blob in further requests.
<a name="new-0"></a>

### new/0 ###


<pre><code>
new() -&gt; atom()
</code></pre>

<br></br>



create a new crate client with the default settings and
registration name

<a name="new-1"></a>

### new/1 ###


<pre><code>
new(Servers::[<a href="#type-craterl_server_spec">craterl_server_spec()</a>]) -&gt; atom()
</code></pre>

<br></br>




start a new craterl client instance with the default name craterl
given a list of crate server instances.


Example:

```

  craterl = new([{<<"192.168.0.1">>, 4200}, {<<"my.hostname"<<, 44200}]).
```

<a name="new-2"></a>

### new/2 ###


<pre><code>
new(Servers::[<a href="#type-craterl_server_spec">craterl_server_spec()</a>], Options::[term()]) -&gt; atom()
</code></pre>

<br></br>




create a new craterl client instance with the default name craterl
given a list of crate server instances
and a list of options as a proplist.


Example:

```

  craterl = new([{<<"192.168.0.1">>, 4200}], [{poolsize, 100], {timeout, 1000}).
```

<a name="new-3"></a>

### new/3 ###


<pre><code>
new(ClientSpec::<a href="#type-craterl_client_spec">craterl_client_spec()</a>, Servers::[<a href="#type-craterl_server_spec">craterl_server_spec()</a>], Options::[term()]) -&gt; atom()
</code></pre>

<br></br>




create a new craterl client instance given a client specification
comprised of a tuple of the same kind you would use for a call to register(),
e.g. {local, my_client} or {global, my_other_client}.
The client will be registered by the process name given in the tuple.
It must be unique per erlang node.
The second argument is a list of crate servers instances and a list of options as a proplist.


Example:

```

  my_client = new({local, my_client}, [{<<"192.168.0.1">>, 4200}], [{poolsize, 100], {timeout, 1000}).
```

<a name="sql-1"></a>

### sql/1 ###


<pre><code>
sql(Stmt::binary() | string() | <a href="#type-sql_request">sql_request()</a>) -&gt; {ok, <a href="#type-sql_response">sql_response()</a>}
</code></pre>

<br></br>



issue a SQL statement, with optional arguments
or a prebuilt #sql_request{}
using the default client instance.
<a name="sql-2"></a>

### sql/2 ###


<pre><code>
sql(Stmt::binary() | string(), Args::list()) -&gt; {ok, <a href="#type-sql_response">sql_response()</a>}
</code></pre>

<br></br>



issue a SQL statement with optional arguments
or a prebuilt #sql_request{} to a specific client
<a name="sql-3"></a>

### sql/3 ###


<pre><code>
sql(Stmt::binary() | string(), Args::list(), IncludeTypes::boolean()) -&gt; {ok, <a href="#type-sql_response">sql_response()</a>}
</code></pre>

<br></br>



issue a SQL statement with optional arguments
and a boolean indicating whether you want to receive type information
for the returned columns.
<a name="sql-4"></a>

### sql/4 ###


<pre><code>
sql(ClientSpec::atom(), Stmt::binary() | string(), Args::list(), IncludeTypes::boolean()) -&gt; {ok, <a href="#type-sql_response">sql_response()</a>}
</code></pre>

<br></br>



issue a SQL statement with optional arguments
and a boolean indicating whether you want to receive type information
for the returned columns
to a specific client.
<a name="sql_bulk-1"></a>

### sql_bulk/1 ###


<pre><code>
sql_bulk(BulkRequest::<a href="#type-sql_bulk_request">sql_bulk_request()</a>) -&gt; {ok, <a href="#type-sql_bulk_response">sql_bulk_response()</a>} | {error, term()}
</code></pre>

<br></br>




issue a Bulk SQL statement using a #sql_bulk_request{}


Bulk statements are only valid for INSERT/UPDATE and DELETE queries
<a name="sql_bulk-2"></a>

### sql_bulk/2 ###


<pre><code>
sql_bulk(ClientName::atom(), BulkRequest::<a href="#type-sql_bulk_request">sql_bulk_request()</a>) -&gt; {ok, <a href="#type-sql_bulk_response">sql_bulk_response()</a>} | {error, term()}
</code></pre>

<br></br>



issue a Bulk SQL statement using a #sql_bulk_request{}
to a specific client.
Or giving a binary or string statement and a list of bulk arguments
to the default client.

<a name="sql_bulk-3"></a>

### sql_bulk/3 ###

`sql_bulk(Stmt, BulkArgs, IncludeTypes) -> any()`


issue a Bulk SQL statement with bulk arguments and
a boolean that determines if the response should contain
column types or not.
<a name="sql_bulk-4"></a>

### sql_bulk/4 ###


<pre><code>
sql_bulk(ClientName::atom(), Stmt::binary(), BulkArgs::[[term()]], IncludeTypes::boolean()) -&gt; {ok, <a href="#type-sql_bulk_response">sql_bulk_response()</a>} | {error, term()}
</code></pre>

<br></br>



issue a Bulk SQL statement with bulk arguments and
a boolean that determines if the response should contain
column types or not
to a specific client.
<a name="start-0"></a>

### start/0 ###

`start() -> any()`


start the craterl application and all its dependencies

<a name="stop_client-1"></a>

### stop_client/1 ###


<pre><code>
stop_client(ClientName::atom()) -&gt; ok | {error, term()}
</code></pre>

<br></br>



stop a running client instance by giving a client reference,
the return value of new().

