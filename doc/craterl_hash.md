

# Module craterl_hash #
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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#sha1Hex-1">sha1Hex/1</a></td><td></td></tr><tr><td valign="top"><a href="#sha1HexFile-1">sha1HexFile/1</a></td><td>hashing the file contents of the file at FilePath.</td></tr><tr><td valign="top"><a href="#sha1HexFileData-1">sha1HexFileData/1</a></td><td>hashing the file contents of the file at FilePath and return the file data too.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="sha1Hex-1"></a>

### sha1Hex/1 ###


<pre><code>
sha1Hex(Content::binary()) -&gt; {ok, binary()}
</code></pre>

<br></br>



<a name="sha1HexFile-1"></a>

### sha1HexFile/1 ###


<pre><code>
sha1HexFile(FilePath::binary() | string()) -&gt; {ok, binary()} | {error, term()}
</code></pre>

<br></br>


hashing the file contents of the file at FilePath

<a name="sha1HexFileData-1"></a>

### sha1HexFileData/1 ###


<pre><code>
sha1HexFileData(FilePath::binary() | string()) -&gt; {ok, binary(), binary()} | {error, term()}
</code></pre>

<br></br>


hashing the file contents of the file at FilePath and return the file data too

