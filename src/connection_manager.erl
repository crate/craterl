%%%-------------------------------------------------------------------
%%% @author Peter Sabaini <peter@sabaini.at>
%%% @copyright (C) 2014, Peter Sabaini
%%% @doc
%%%
%%% @end
%%% Created :  9 Apr 2014 by Peter Sabaini <peter@sabaini.at>
%%%-------------------------------------------------------------------
-module(connection_manager).

-behaviour(gen_server).

-include("crate_erlang.hrl").
-compile([{parse_transform, lager_transform}]).

%% API
-export([start_link/0, stop/0, get_server/0, add_active/1, add_inactive/1]).
-ifdef(TEST).
-compile(export_all).
-endif.

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(connections, {activelist, inactivelist}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(Args) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_server() ->
    gen_server:call(?MODULE, getserver).

add_active(Server) ->
    gen_server:call(?MODULE, {add_active, Server}).

add_inactive(Server) ->
    gen_server:call(?MODULE, {add_inactive, Server}).

stop() -> gen_server:cast(?MODULE, stop).

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
init(_) ->
    Servers = parse_servers_string(config_provider:get(crate_servers, [?DEFAULT_SERVER])),
    lager:info("Servers configured ~p", [Servers]),
    {ok, #connections{activelist=Servers,
                      inactivelist=[]}}.

parse_servers_string(ServersString) when is_list(ServersString) ->
  parse_servers_string(list_to_binary(ServersString));
parse_servers_string(ServersString) when is_binary(ServersString) ->
  lists:filter(
    fun
      (HostAndPort) when is_binary(HostAndPort) ->
        case binary:split(HostAndPort, <<":">>) of
          [_,_] -> true;
          [_] -> false
        end,
        true;
      (_) -> false
    end,
    lists:map(
      fun(ServerString) -> binary:replace(ServerString, <<" ">>, <<"">>, [global]) end,
      binary:split(ServersString, <<",">>, [global])
    )
  );
parse_servers_string(_) -> [].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(getserver, _From, 
            #connections{activelist=Active, 
                         inactivelist=Inactive}) ->
    io:format("getserver start ~p, ~p ~n", [Active, Inactive]),
    Response = case lookup(Active, Inactive) of
        none_active -> none_active;
        Server      -> {ok, Server}
    end,
    {reply, Response, #connections{activelist=Active, inactivelist=Inactive}};

handle_call({add_active, Server}, _From,
            #connections{activelist=Active,
                         inactivelist=Inactive}) ->
    io:format("getserver active ~p; ~p, ~p ~n", [Server, Active, Inactive]),
    {reply, ok, #connections{activelist=[Server|Active],
                             inactivelist=Inactive}};

handle_call({add_inactive, Server}, _From, 
            #connections{activelist=Active, 
                         inactivelist=Inactive}) ->
    io:format("getserver inactive ~p; ~p, ~p ~n", [Server, Active, Inactive]),
    lists:delete(Server, Active),
    {reply, ok, #connections{activelist=Active,
                             inactivelist=[Server|Inactive]}};

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

lookup([], _Inactive) ->
    none_active;
lookup(Active, _Inactive) when is_list(Active)->
    lists:last(Active).
