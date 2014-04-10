%%%-------------------------------------------------------------------
%%% @author Peter Sabaini <peter@sabaini.at>
%%% @copyright (C) 2014, Peter Sabaini
%%% @doc 
%%%
%%% @end
%%% Created :  9 Apr 2014 by Peter Sabaini <peter@sabaini.at>
%%%-------------------------------------------------------------------
-module(craterl).

-include("crate_erlang.hrl").

%% API
-export([sql/1, sql/2, start/0]).

-compile([{parse_transform, lager_transform}]).

start() ->
  application:ensure_all_started(jsx),
  application:ensure_all_started(hackney),
  application:ensure_all_started(lager),
  application:start(craterl).


sql(Stmt) ->
    sql(Stmt, []).

sql(Stmt, Args) when is_list(Stmt) ->
    sql(list_to_binary(Stmt), Args);

sql(Stmt, Args) when is_binary(Stmt) ->
    statistics(runtime),
    statistics(wall_clock),
    case connection_manager:get_server() of
        none_active ->
            {error, "No active server"};
        {ok, Server} ->
            {ok, ChildPid} = crate_request_handler_sup:request(
                               Stmt, Args, Server, self()),
            receive
                {ChildPid, {ok, SqlResponse}} ->
                    connection_manager:add_active(Server),
                    {ok, instrument(SqlResponse)};
                {ChildPid, {error, econnrefused}} ->
                    lager:info("sql/econnrefused: ~p~n", [Server]),
                    connection_manager:add_inactive(Server),
                    sql(Stmt, Args);
                {ChildPid, {error, OtherError}} ->
                    lager:info("sql/error other: ~p~n", [OtherError]),
                    {error, OtherError};
                Other ->
                    lager:error("sql/other: ~p~n", [Other]),
                    {error, Other}
            end
    end.

instrument(#sql_response{
              cols=Cols, rows=Rows,
              rowCount=RowCnt,
              duration=Duration,
              wallclock=_, runtime=_
             }) ->   
    {_, RunTime} = statistics(runtime),
    {_, WallClock} = statistics(wall_clock),
    #sql_response{
       cols=Cols, rows=Rows, 
       rowCount=RowCnt, 
       duration=Duration,
       wallclock=WallClock,
       runtime=RunTime
      }.



    
