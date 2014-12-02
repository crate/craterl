%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, CRATE Technology GmbH
%%% Licensed to CRATE Technology GmbH ("Crate") under one or more contributor
%%% license agreements.  See the NOTICE file distributed with this work for
%%% additional information regarding copyright ownership.  Crate licenses
%%% this file to you under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.  You may
%%% obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
%%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
%%% License for the specific language governing permissions and limitations
%%% under the License.
%%%
%%% However, if you have executed another commercial license agreement
%%% with Crate these terms will supersede the license and you may use the
%%% software solely pursuant to the terms of the relevant commercial agreement.
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(craterl_gen_server).

-behaviour(gen_server).

-include("craterl.hrl").
-compile([{parse_transform, lager_transform}]).

%% API
-export([start_link/3, stop/1,
  get_server/1, set_servers/2,
  add_active/2, add_inactive/2]).
-ifdef(TEST).
-compile(export_all).
-endif.

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(connections, {
  activelist   :: {[ craterl_server_spec() ], [ craterl_server_spec() ]},
  inactivelist :: [ craterl_server_spec() ]
}).

-record(state, {
  connections = #connections{},
  config :: {proplists:proplist(), proplists:proplist()}
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(ClientSpec, Servers, Options) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------

start_link(ClientSpec, Servers, Options) ->
  gen_server:start_link(ClientSpec, ?MODULE, [{servers, Servers}, {options, Options}], []).

-spec get_server(atom()) -> {ok, craterl_server_conf()} | none_active.
get_server(ClientName) ->
    gen_server:call(ClientName, get_server).

set_servers(ClientName, ServerList) when is_list(ServerList) ->
    gen_server:call(ClientName, {set_servers, ServerList}).

add_active(ClientName, Server) ->
    gen_server:call(ClientName, {add_active, Server}).

add_inactive(ClientName, Server) ->
    gen_server:call(ClientName, {add_inactive, Server}).

stop(ClientName) ->
  gen_server:cast(ClientName, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(Args) ->
    Servers  = proplists:get_value(servers, Args, [?CRATERL_DEFAULT_SERVER]),
    {Options, RequestOptions}  = craterl_config:apply_defaults(proplists:get_value(options, Args, [])),
    PoolName = craterl_config:get(poolname, Options),
    PoolSize = craterl_config:get(poolsize, Options),
    Timeout  = craterl_config:get(timeout, Options),
    ok = hackney_pool:start_pool(PoolName, [{timeout, Timeout}, {max_connections, PoolSize}]),
    {ok,  #state{
            connections = new_servers(Servers),
            config = {Options, RequestOptions}
          }
    }.

new_state(#state{config=Config}, Connections=#connections{}) ->
  #state{
    connections = Connections,
    config = Config
  }.

handle_call(get_server, _From,
            State = #state{connections = Connections,
              config = Config}) ->
    case lookup_server(Connections) of
        none_active -> {reply, none_active, State};
        {ok, Server, NewConnections} ->
          {reply,
            {ok, #craterl_server_conf{address = Server, config = Config}},
            new_state(State, NewConnections)
          }
    end;

handle_call({set_servers, ServerList}, _From, State=#state{}) ->
    NewConnections = new_servers(ServerList),
    {reply, ok, new_state(State, NewConnections)};

handle_call({add_active, Server}, _From,
            State = #state{connections = Connections}) ->
    NewConnections = add_server(Connections, Server),
    {
      reply,
      ok,
      new_state(
        State,
        NewConnections
      )
    };

handle_call({add_inactive, Server}, _From, 
            State=#state{connections = Connections}) ->
    NewConnections = remove_server(Connections, Server),
    {reply, ok, new_state(State, NewConnections)};

handle_call(Request, _From, State) ->
    lager:error("unexpected request ~p, state ~p", [Request, State]),
    {reply, error, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


-spec new_servers(list()) -> #connections{}.
new_servers(Servers) ->
  #connections{activelist = {Servers, []}, inactivelist = []}.


-spec add_server(#connections{}, craterl_server_spec()) -> #connections{}.
add_server(#connections{activelist = {Servers, IteratedServers}} = Connections, Server) ->
  Connections#connections{activelist = {[Server|Servers], IteratedServers}}.


-spec remove_server(#connections{}, craterl_server_spec()) -> #connections{}.
remove_server(#connections{activelist = {Active, Iterated}} = Connections, Server) ->
  Connections#connections{activelist = {lists:delete(Server, Active), lists:delete(Server, Iterated)}}.


-spec lookup_server(#connections{}) -> none_active | {ok, craterl_server_spec(), #connections{}}.
lookup_server(#connections{activelist = {[], []}, inactivelist = []}) ->
  none_active;
lookup_server(#connections{activelist = {[], []}, inactivelist = [Server|Inactive]} = Connections) ->
  {ok,
    Server,
    Connections#connections{activelist = {Inactive, [Server]}, inactivelist = []}
  };
lookup_server(#connections{activelist = {[], [Server|Tail]}} = Connections) ->
  {
    ok,
    Server,
    Connections#connections{activelist = {Tail, [Server]}}
  };
lookup_server(#connections{activelist = {[Server|[]], []}} = Connections) ->
  {ok,
    Server,
    Connections#connections{activelist = {[Server], []}}
  };
lookup_server(#connections{activelist = {[Server|Active], Iterated}} = Connections) ->
  {ok,
    Server,
    Connections#connections{activelist = {Active, [Server|Iterated]}}
  }.
