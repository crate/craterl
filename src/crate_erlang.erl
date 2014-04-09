%%%-------------------------------------------------------------------
%%% @author mat
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Apr 2014 2:03 PM
%%%-------------------------------------------------------------------
-module(crate_erlang).
-author("mat").

%% API
-export([start/0]).


start() -> application:start(crate_erlang).
