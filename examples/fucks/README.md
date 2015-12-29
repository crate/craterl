# All the given fucks


A REST API for all the given fucks.

## Build

    $ ../../rebar3 compile

## Run

For a quick run with an attached shell:

    $ ../../rebar3 shell --apps fucks

The server will listen on localhost:8080 if not otherwise configured.

## REST Endpoints

 * ``GET /fucks/``

    Get all the given fucks.
    
    Filter by ``author``, ``about``. 
    Fetch fucks created within a given timerange 
    using ``from`` and ``to`` parameters. 
    Use millisecond-precision UTC timestamps here.
    You can page through results, using ``offset``.
     
 * ``POST /fucks/`` 
 
    Give a fuck.
    
    Post a JSON encoded fuck with ``about`` and ``author`` fields only.
    
 * ``GET /fucks/<id>``
 
    Get a fuck.
    
    Take a detailed look at a single JSON encoded fuck which is given.
    
 * ``DELETE /fucks/<id>``
 
    Delete a fuck.
    
    It's like ungiving a fuck, if that is possible.