

# Module craterl_gen_server #
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

__Behaviours:__ [`gen_server`](gen_server.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_active-2">add_active/2</a></td><td></td></tr><tr><td valign="top"><a href="#add_inactive-2">add_inactive/2</a></td><td></td></tr><tr><td valign="top"><a href="#format_status-2">format_status/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_server-1">get_server/1</a></td><td></td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#set_servers-2">set_servers/2</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-3">start_link/3</a></td><td>
Starts the server.</td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_active-2"></a>

### add_active/2 ###

<pre><code>
add_active(ClientRef::<a href="#type-craterl_client_ref">craterl_client_ref()</a>, Server::<a href="#type-craterl_server_spec">craterl_server_spec()</a>) -&gt; ok
</code></pre>
<br />

<a name="add_inactive-2"></a>

### add_inactive/2 ###

<pre><code>
add_inactive(ClientRef::<a href="#type-craterl_client_ref">craterl_client_ref()</a>, Server::<a href="#type-craterl_server_spec">craterl_server_spec()</a>) -&gt; ok
</code></pre>
<br />

<a name="format_status-2"></a>

### format_status/2 ###

`format_status(X1, X2) -> any()`

<a name="get_server-1"></a>

### get_server/1 ###

<pre><code>
get_server(ClientRef::<a href="#type-craterl_client_ref">craterl_client_ref()</a>) -&gt; {ok, <a href="#type-craterl_server_conf">craterl_server_conf()</a>} | none_active
</code></pre>
<br />

<a name="handle_call-3"></a>

### handle_call/3 ###

`handle_call(Request, From, State) -> any()`

<a name="set_servers-2"></a>

### set_servers/2 ###

<pre><code>
set_servers(ClientRef::<a href="#type-craterl_client_ref">craterl_client_ref()</a>, ServerList::[<a href="#type-craterl_server_spec">craterl_server_spec()</a>]) -&gt; ok
</code></pre>
<br />

<a name="start_link-3"></a>

### start_link/3 ###

<pre><code>
start_link(ClientSpec, Servers, Options) -&gt; {ok, Pid} | ignore | {error, Error}
</code></pre>
<br />

Starts the server

<a name="stop-1"></a>

### stop/1 ###

<pre><code>
stop(ClientRef::<a href="#type-craterl_client_ref">craterl_client_ref()</a>) -&gt; ok
</code></pre>
<br />

