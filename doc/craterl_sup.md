

# Module craterl_sup #
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

__Behaviours:__ [`supervisor`](supervisor.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_client-3">start_client/3</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr><tr><td valign="top"><a href="#stop_client-1">stop_client/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="init-1"></a>

### init/1 ###

`init(X1) -> any()`

<a name="start_client-3"></a>

### start_client/3 ###

<pre><code>
start_client(ClientSpec::<a href="#type-craterl_client_spec">craterl_client_spec()</a>, Servers::[<a href="#type-craterl_server_spec">craterl_server_spec()</a>], Options::[term()]) -&gt; <a href="#type-craterl_client_ref">craterl_client_ref()</a>
</code></pre>
<br />

<a name="start_link-0"></a>

### start_link/0 ###

`start_link() -> any()`

<a name="stop_client-1"></a>

### stop_client/1 ###

<pre><code>
stop_client(ClientRef::<a href="#type-craterl_client_ref">craterl_client_ref()</a>) -&gt; ok | {error, term()}
</code></pre>
<br />

