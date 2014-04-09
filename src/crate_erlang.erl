%%%-------------------------------------------------------------------
%%% @author Peter Sabaini <peter@sabaini.at>
%%% @copyright (C) 2014, Peter Sabaini
%%% @doc
%%%
%%% @end
%%% Created :  9 Apr 2014 by Peter Sabaini <peter@sabaini.at>
%%%-------------------------------------------------------------------
-module(crate_erlang).


%% API
-export([start/0, stop/0, sql/1, sql/2]).

-compile([{parse_transform, lager_transform}]).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
    lager:start(),
    crate_request_handler_sup:start_link(),
    connection_manager:start_link([{<<"localhost">>, 4200}]).

sql(Stmt) ->
    sql(Stmt, []).

sql(Stmt, Args) ->
    case connection_manager:get_server() of
        none_active ->
            {error, "No active server"};
        {ok, Server} ->
            {ok, ChildPid} = crate_request_handler_sup:request(
                               Stmt, Args, Server, self()),
            receive
                {ChildPid, {ok, SqlResponse}} ->
                    connection_manager:add_active(Server),
                    {ok, SqlResponse};
                {ChildPid, {error, econnrefused}} ->
                    connection_manager:add_inactive(Server),
                    sql(Stmt, Args);
                {ChildPid, Other} ->
                    io:format("sql/other: ~p~n", [Other])
            end
    end.

stop() ->
    connection_manager:terminate(normal, {}).
    

    
