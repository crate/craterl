%%%-------------------------------------------------------------------
%%% @author mat
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Apr 2014 10:20 AM
%%%-------------------------------------------------------------------
-module(config_provider).
-author("mat").

-behaviour(gen_server).
-compile([{parse_transform, lager_transform}]).

%% API
-export([start_link/0,
  get/1, get/2,
  reload/0, reload/1
]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {config=[]}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get(Key) ->
  gen_server:call(?SERVER, {get, Key}).
get(Key, Default) ->
  gen_server:call(?SERVER, {get, Key, Default}).

reload() -> gen_server:cast(?SERVER, {reload}).
reload(Path) -> gen_server:cast(?SERVER, {reload, Path}).


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
init([]) ->
  Config = load_config(),
  lager:debug("Starting with config ~p", [Config]),
  {ok, #state{config = Config}}.

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
handle_call({get, Key}, _From, State) ->
  Value = get_config_value(Key, State#state.config),
  {reply, Value, State};
handle_call({get, Key, Default}, _From, State) ->
  Value = get_config_value(Key, State#state.config, Default),
  {reply, Value, State};
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
handle_cast({reload}, _State) ->
  {noreply, load_config()};
handle_cast({reload, Path}, _State) ->
  {noreply, load_config(Path)};
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
load_config() ->
  ConfigFilePath = get_env(crate_config_file),
  load_config(ConfigFilePath).

load_config(ConfigFilePath) ->
  case file:consult(ConfigFilePath) of
      {ok, Terms} -> Terms;
      {error, Reason} ->
        lager:error("Error reading config file ~p: ~p", [ConfigFilePath, Reason]),
        []
  end.

get_config_value(Key, DefaultConfig) ->
  case get_env(Key) of
    undefined -> proplists:get_value(Key, DefaultConfig);
    Value -> Value
  end.

get_config_value(Key, DefaultConfig, DefaultIfNotSet) ->
  case get_config_value(Key, DefaultConfig) of
    undefined -> DefaultIfNotSet;
    Value -> Value
  end.

get_env(Key) when is_atom(Key) ->
  get_env(atom_to_list(Key));
get_env(Key) when is_binary(Key) ->
  get_env(binary_to_list(Key));
get_env(Key) when is_list(Key) ->
  case os:getenv(string:to_upper(Key)) of
    false ->
      case application:get_env(Key) of
        {ok, Val} -> Val;
        undefined -> undefined
      end;
    Value -> Value
  end;
get_env(_) -> undefined.
