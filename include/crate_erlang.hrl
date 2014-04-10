%%%-------------------------------------------------------------------
%%% @author mat
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Apr 2014 2:06 PM
%%%-------------------------------------------------------------------
-author("mat").


-define(SQLPATH, <<"/_sql">>).
-define(DEFAULT_PORT, 4200).
-define(DEFAULT_ROWCOUNT, 0).
-define(DEFAULT_DURATION, 0).
-define(DEFAULT_WALLCLOCK, 0).
-define(DEFAULT_RUNTIME, 0).
-define(DEFAULT_MESSAGE, <<"Shit happens">>).
-define(DEFAULT_CODE, 1000).
-define(DEFAULT_SERVER, <<"localhost:4200">>).

-record(sql_response, {cols=[], rows=[], rowCount=?DEFAULT_ROWCOUNT, duration=?DEFAULT_DURATION, wallclock=?DEFAULT_WALLCLOCK, runtime=?DEFAULT_RUNTIME}).
-record(sql_error, {message=?DEFAULT_MESSAGE, code=?DEFAULT_CODE}).
