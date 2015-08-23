

# Module craterl_resp #
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

__Authors:__ Matthias Wahl.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#bulk_results-1">bulk_results/1</a></td><td></td></tr><tr><td valign="top"><a href="#column-2">column/2</a></td><td></td></tr><tr><td valign="top"><a href="#column_names-1">column_names/1</a></td><td></td></tr><tr><td valign="top"><a href="#duration-1">duration/1</a></td><td></td></tr><tr><td valign="top"><a href="#error_code-1">error_code/1</a></td><td></td></tr><tr><td valign="top"><a href="#error_message-1">error_message/1</a></td><td></td></tr><tr><td valign="top"><a href="#row_count-1">row_count/1</a></td><td></td></tr><tr><td valign="top"><a href="#rows-1">rows/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_tuple-1">to_tuple/1</a></td><td></td></tr><tr><td valign="top"><a href="#types-1">types/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="bulk_results-1"></a>

### bulk_results/1 ###

<pre><code>
bulk_results(Sql_bulk_response::<a href="#type-sql_bulk_response">sql_bulk_response()</a>) -&gt; [<a href="#type-sql_bulk_result">sql_bulk_result()</a>]
</code></pre>
<br />

<a name="column-2"></a>

### column/2 ###

<pre><code>
column(Sql_response::<a href="#type-sql_response">sql_response()</a>, Index::pos_integer()) -&gt; [any()]
</code></pre>
<br />

<a name="column_names-1"></a>

### column_names/1 ###

<pre><code>
column_names(Sql_response::<a href="#type-sql_response">sql_response()</a> | <a href="#type-sql_bulk_response">sql_bulk_response()</a>) -&gt; [binary()]
</code></pre>
<br />

<a name="duration-1"></a>

### duration/1 ###

<pre><code>
duration(Sql_response::<a href="#type-sql_response">sql_response()</a> | <a href="#type-sql_bulk_response">sql_bulk_response()</a>) -&gt; non_neg_integer()
</code></pre>
<br />

<a name="error_code-1"></a>

### error_code/1 ###

<pre><code>
error_code(Sql_error::<a href="#type-sql_error">sql_error()</a>) -&gt; integer()
</code></pre>
<br />

<a name="error_message-1"></a>

### error_message/1 ###

<pre><code>
error_message(Sql_bulk_result::<a href="#type-sql_bulk_result">sql_bulk_result()</a> | <a href="#type-sql_error">sql_error()</a>) -&gt; binary()
</code></pre>
<br />

<a name="row_count-1"></a>

### row_count/1 ###

<pre><code>
row_count(Sql_response::<a href="#type-sql_response">sql_response()</a> | <a href="#type-sql_bulk_result">sql_bulk_result()</a>) -&gt; integer()
</code></pre>
<br />

<a name="rows-1"></a>

### rows/1 ###

<pre><code>
rows(Sql_response::<a href="#type-sql_response">sql_response()</a>) -&gt; [[term()]]
</code></pre>
<br />

<a name="to_tuple-1"></a>

### to_tuple/1 ###

<pre><code>
to_tuple(Sql_response::<a href="#type-sql_response">sql_response()</a>) -&gt; {[[term]], integer(), [binary()], [integer()], non_neg_integer()}
</code></pre>
<br />

<a name="types-1"></a>

### types/1 ###

<pre><code>
types(Sql_response::<a href="#type-sql_response">sql_response()</a> | <a href="#type-sql_bulk_response">sql_bulk_response()</a>) -&gt; [integer()]
</code></pre>
<br />

