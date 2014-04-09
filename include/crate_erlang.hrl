%%%-------------------------------------------------------------------
%%% @author mat
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Apr 2014 2:06 PM
%%%-------------------------------------------------------------------
-author("mat").

-record(sql_response, {cols, rows=[], rowCount=0, duration=-1}).
