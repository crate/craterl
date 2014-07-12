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
-export([start_link/3]).
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

-include("craterl.hrl").
-compile([{parse_transform, lager_transform}]).


-define(SERVER, ?MODULE).

-record(state, {request, serverSpec, callerPid}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------

-spec(start_link(Request :: #sql_request{} | #blob_request{}, _ServerSpec, _CallerPid) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(SqlRequest = #sql_request{}, ServerSpec, CallerPid) ->
  gen_server:start_link(?MODULE, [SqlRequest, ServerSpec, CallerPid], []);
start_link(BlobRequest = #blob_request{}, ServerSpec, CallerPid) ->
  gen_server:start_link(?MODULE, [BlobRequest, ServerSpec, CallerPid], []).

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
init([Request, ServerSpec, CallerPid]) ->
  self() ! {do_start},
  {ok, #state{request=Request, serverSpec = ServerSpec, callerPid = CallerPid}}.

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
handle_info({do_start}, State=#state{request = #sql_request{}}) ->
  sql_request(State),
  {stop, normal, State};
handle_info({do_start}, State=#state{request = #blob_request{}}) ->
  blob_request(State),
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


%%% SQL %%%

sql_request(#state{request=#sql_request{stmt=Stmt, args=Args}, serverSpec=ServerSpec, callerPid=CallerPid}) ->
  Response = case hackney:request(post,
    create_server_url(ServerSpec, ?SQLPATH),
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

create_server_url(<<"http://", Host/binary>>, Path) when is_binary(Path) ->
  create_server_url(http, Host, Path);
create_server_url(<<"https://", Host/binary>>, Path) when is_binary(Path) ->
  create_server_url(https, Host, Path);
create_server_url(Host, Path) when is_binary(Host) and is_binary(Path) ->
  create_server_url(http, Host, Path).

create_server_url(Scheme, Host, Path) when is_atom(Scheme) and is_binary(Host) and is_binary(Path) ->
  Url = case binary:split(Host, <<":">>) of
    [_, _] ->
      <<Host/binary, Path/binary>>;
    [HostString] ->
      SchemeString = atom_to_binary(Scheme, utf8),
      PortString = integer_to_binary(?DEFAULT_PORT),
      <<SchemeString/binary, "://", HostString/binary, ":", PortString/binary, Path/binary>>
  end,
  normalize_server_url(Url);
create_server_url(_, _, _) -> {error, invalid_server}.

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


%%% BLOB %%%

blob_request(#state{
  request = #blob_request{method=Method, table=Table, digest=Digest, payload=Payload},
  serverSpec = ServerSpec,
  callerPid = CallerPid}) ->
  Response = case Method of
    get ->
      case Payload of
        undefined -> blob_get_to_mem(ServerSpec, Table, Digest);
        {file, FilePath} -> blob_get_to_file(ServerSpec, Table, Digest, FilePath)
      end;
    head -> blob_exists(ServerSpec, Table, Digest);
    put -> blob_put(ServerSpec, Table, Digest, Payload);
    delete -> blob_delete(ServerSpec, Table, Digest);
    _ -> {error, unsupported}
  end,
  CallerPid ! {self(), Response};
blob_request(_) -> {error, unsupported}.

blob_put(ServerSpec, Table, Digest, Payload) when is_binary(Table) and is_binary(Digest) ->
  case create_server_url(ServerSpec, <<"/_blobs/", Table/binary, "/", Digest/binary>>) of
    {error, Reason} -> {error, Reason};
    Url ->
      lager:debug("putting blob to ~p", [Url]),
      case hackney:request(put,
        Url,
        [
          {<<"Transfer-Encoding">>, <<"chunked">>}
        ],
        stream,
        [{pool, crate}]
      ) of
        {ok, Client} ->
          Body = case Payload of
            {file, Path} -> {file, Path};
            {data, Data} -> Data;
            Data -> Data
          end,
          case hackney:send_body(Client, Body) of
            ok ->
              case hackney:start_response(Client) of
                {ok, StatusCode, _RespHeaders, _ClientRef} ->
                   case StatusCode of
                     201 -> {ok, {created, Digest}};
                     400 -> {error, {bad_request, Digest}};
                     404 -> {error, {not_found, Digest}};
                     409 -> {error, {already_exists, Digest}};
                     _ -> {error, StatusCode}
                   end;
                {error, Reason} ->
                  {error, Reason}
              end;
            {error, Reason} -> {error, Reason}
          end;
          {error, Reason} -> {error, Reason}
      end
  end.

blob_get_to_mem(ServerSpec, Table, Digest) ->
  HandleBodyFun = fun (ClientRef) ->
    GetDataFun = fun() ->
      hackney:stream_body(ClientRef)
    end,
    GetDataFun
  end,
  blob_request(ServerSpec, get, Table, Digest, HandleBodyFun).

blob_get_to_file(ServerSpec, Table, Digest, FilePath) ->
  HandleBodyFun = fun (ClientRef) ->
      case file:open(FilePath, [write, binary, raw]) of
        {ok, FileHandle} ->
          Result = case stream_blob_to_file(ClientRef, FileHandle) of
            ok -> {ok, FilePath};
            {error, Reason} -> {error, Reason}
          end,
          file:close(FileHandle),
          Result;
        {error, Reason} -> {error, Reason}
      end
  end,
  blob_request(ServerSpec, get, Table, Digest, HandleBodyFun).

stream_blob_to_file(ClientRef, FileHandle) ->
  case hackney:stream_body(ClientRef) of
    {ok, Data} ->
      file:write(FileHandle, Data),
      stream_blob_to_file(ClientRef, FileHandle);
    done -> ok;
    {error, Reason} -> {error, Reason}
  end.

blob_exists(ServerSpec, Table, Digest) ->
  SuccessFun = fun (_ClientRef) ->
    ok
  end,
  blob_request(ServerSpec, head, Table, Digest, SuccessFun).

blob_delete(ServerSpec, Table, Digest) ->
  SuccessFun = fun (_ClientRef) ->
    ok
  end,
  blob_request(ServerSpec, delete, Table, Digest, SuccessFun).


%% TODO: maybe rename to avoid ambiguity
blob_request(ServerSpec, Method, Table, Digest, HandleBodyFun) when is_function(HandleBodyFun)
                                                                and is_atom(Method) ->
  case create_server_url(ServerSpec, <<"/_blobs/", Table/binary, "/", Digest/binary>>) of
    {error, Reason} -> {error, Reason};
    Url ->
      lager:debug("blob request to ~p", [Url]),
      Headers = [],
      Options = [{pool, crate}],
      case hackney:request(Method, Url, Headers, <<>>, Options) of
        {ok, StatusCode, _RespHeaders, ClientRef} ->
          case StatusCode of
              Code when Code < 400 -> HandleBodyFun(ClientRef);
              _ -> {error, StatusCode}
          end;
        {error, Reason} -> {error, Reason}
      end
  end.
