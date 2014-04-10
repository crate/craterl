%%%-------------------------------------------------------------------
%%% @author mat
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Apr 2014 1:25 PM
%%%-------------------------------------------------------------------
-module(crate_request_handler).
-author("mat").

-behaviour(gen_server).

%% API
-export([start_link/4]).
-ifdef(TEST).
-compile(export_all).
-endif.


%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-include("crate_erlang.hrl").
-compile([{parse_transform, lager_transform}]).


-define(SERVER, ?MODULE).

-record(state, {stmt, args, serverSpec, callerPid}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(_Stmt, _Args, _ServerSpec, _CallerPid) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Stmt, Args, ServerSpec, CallerPid) ->
  gen_server:start_link(?MODULE, [Stmt, Args, ServerSpec, CallerPid], []).

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
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([Stmt, Args, ServerSpec, CallerPid]) ->
  self() ! {do_start},
  {ok, #state{stmt=Stmt, args=Args, serverSpec = ServerSpec, callerPid = CallerPid}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
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
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info({do_start}, State=#state{}) ->
  do_request(State),
  {stop, normal, State};
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
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
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
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_request(#state{stmt=Stmt, args=Args, serverSpec=ServerSpec, callerPid=CallerPid}) ->
  Response = case hackney:request(<<"POST">>,
    create_server_url(ServerSpec),
    [{<<"Content-Type">>, <<"application/json">>}],
    create_payload(Stmt, Args),
    [{pool, crate}]
  ) of
    {ok, StatusCode, _RespHeaders, ClientRef} ->
       % parse body
       case hackney:body(ClientRef) of
         {ok, Body} ->
           case StatusCode of
             StatusCode when StatusCode < 400 ->
               {ok, build_response(Body)};
             _ErrorCode ->
               {error, build_error_response(Body)}
           end;
         {error, Reason} -> {error, Reason}
       end;
    {error, Reason} ->
      {error, Reason}
  end,
  CallerPid ! {self(), Response}.

create_payload(Stmt, Args) ->
  Payload = [
    {<<"stmt">>, Stmt},
    {<<"args">>, Args}
  ],
  % TODO: handle incomplete input
  jsx:encode(Payload).

normalize_server_url(<<"http://", _/binary>>=Server) -> Server;
normalize_server_url(<<"https://", _/binary>>=Server) -> Server;
normalize_server_url(<<Server/binary>>) -> <<"http://", Server/binary>>.

create_server_url(<<_Host, ":", _Port>> = HostAndPort) ->
  normalize_server_url(<<HostAndPort, ?SQLPATH/binary>>);
create_server_url(Host) when is_binary(Host) ->
  Url = case binary:split(Host, <<":">>) of
    [_, _] ->
      <<Host/binary, ?SQLPATH/binary>>;
    [HostString] ->
      PortString = integer_to_binary(?DEFAULT_PORT),
      <<HostString, ":", PortString, ?SQLPATH/binary>>
  end,
  normalize_server_url(Url);
create_server_url(HostStr) when is_list(HostStr) ->
  create_server_url(list_to_binary(HostStr)).


build_response(Body) when is_binary(Body) ->
  % TODO: handle incomplete input
  Decoded = jsx:decode(Body),
  #sql_response{
    rowCount=proplists:get_value(<<"rowcount">>, Decoded, 0),
    cols=proplists:get_value(<<"cols">>, Decoded, []),
    rows=proplists:get_value(<<"rows">>, Decoded, []),
    duration=proplists:get_value(<<"duration">>, Decoded, 0)
  }.

build_error_response(Body) when is_binary(Body) ->
  % TODO: handle incomplete input
  Decoded = jsx:decode(Body),
  ErrorInfo = proplists:get_value(<<"error">>, Decoded, []),
  #sql_error{
    code=proplists:get_value(<<"code">>, ErrorInfo, ?DEFAULT_CODE),
    message=proplists:get_value(<<"message">>, ErrorInfo, ?DEFAULT_MESSAGE)
  }.
