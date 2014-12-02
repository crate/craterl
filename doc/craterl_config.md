

# Module craterl_config #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)



stuff handling craterl configuration.
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


__Authors:__ Matthias Wahl.
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#apply_defaults-0">apply_defaults/0</a></td><td></td></tr><tr><td valign="top"><a href="#apply_defaults-1">apply_defaults/1</a></td><td></td></tr><tr><td valign="top"><a href="#get-2">get/2</a></td><td></td></tr><tr><td valign="top"><a href="#request_config-1">request_config/1</a></td><td>
create the configuration for use in hackney requests from the server config.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="apply_defaults-0"></a>

### apply_defaults/0 ###


<pre><code>
apply_defaults() -&gt; {<a href="proplists.md#type-proplist">proplists:proplist()</a>, <a href="proplists.md#type-proplist">proplists:proplist()</a>}
</code></pre>

<br></br>



<a name="apply_defaults-1"></a>

### apply_defaults/1 ###


<pre><code>
apply_defaults(Options::<a href="proplists.md#type-proplist">proplists:proplist()</a>) -&gt; {<a href="proplists.md#type-proplist">proplists:proplist()</a>, <a href="proplists.md#type-proplist">proplists:proplist()</a>}
</code></pre>

<br></br>



<a name="get-2"></a>

### get/2 ###


<pre><code>
get(Key::term(), Config::<a href="proplists.md#type-proplist">proplists:proplist()</a>) -&gt; undefined | term()
</code></pre>

<br></br>



<a name="request_config-1"></a>

### request_config/1 ###


<pre><code>
request_config(Config::<a href="proplists.md#type-proplist">proplists:proplist()</a>) -&gt; <a href="proplists.md#type-proplist">proplists:proplist()</a>
</code></pre>

<br></br>



create the configuration for use in hackney requests from the server config

