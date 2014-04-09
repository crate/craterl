%%%-------------------------------------------------------------------
%%% @author mat
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Apr 2014 2:06 PM
%%%-------------------------------------------------------------------
-author("mat").


-define(SQLPATH, "/_sql").
-define(DEFAULT_ROWCOUNT, 0).
-define(DEFAULT_DURATION, 0).
-define(DEFAULT_MESSAGE, <<"Shit happend">>).
-define(DEFAULT_CODE, 1000).

-record(sql_response, {cols=[], rows=[], rowCount=?DEFAULT_ROWCOUNT, duration=?DEFAULT_DURATION}).
-record(sql_error, {message=?DEFAULT_MESSAGE, code=?DEFAULT_CODE}).
