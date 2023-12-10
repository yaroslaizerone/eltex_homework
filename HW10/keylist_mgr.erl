-module(keylist_mgr).
-author("kolpa").

-behaviour(gen_server).

%% API
-export([start/0, start_child/1, stop_child/1, stop/0, get_names/0]).

%% Record definition
-record(state, {children=[], permanent=[]}).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API functions

%% @doc Start keylist_mgr
-spec start() -> {ok, Pid :: pid()} | {error, Reason :: term()}.
start() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Start a child process
-spec start_child(Params :: #{name => atom(), restart => permanent | temporary}) -> ok.
start_child(Params) ->
  gen_server:cast(?MODULE, {start_child, Params}).

%% @doc Stop a child process
-spec stop_child(Name :: atom()) -> ok.
stop_child(Name) ->
  gen_server:cast(?MODULE, {stop_child, Name}).

%% @doc Stop keylist_mgr
-spec stop() -> ok.
stop() ->
  gen_server:cast(?MODULE, stop).

%% @doc Get the list of child process names
-spec get_names() -> ok.
get_names() ->
  gen_server:call(?MODULE, get_names).

%% gen_server callbacks

%% @doc Initializes the keylist_mgr process.
init([]) ->
  process_flag(trap_exit, true),
  {ok, #state{}}.

%% @doc Handles synchronous calls to the keylist_mgr process.
handle_call(get_names, _From, State) ->
  {reply, [Name || {Name, _} <- State#state.children], State};

handle_call(_Msg, _From, State) ->
  {reply, ok, State}.

%% @doc Handles asynchronous casts to the keylist_mgr process.
handle_cast({start_child, #{name := Name, restart := Restart}}, State) ->
  case Restart of
    temporary ->
      {noreply, State}; % Do nothing if restart is temporary
    _ ->
      case proplists:is_defined(Name, State#state.children) of
        false ->
          {ok, Pid} = keylist:start_link(Name),
          NewChildren =
            case Restart of
              permanent ->
                [{Name, Pid} | State#state.children];
              _ ->
                erlang:error(type_error)
            end,

          lists:foreach(fun({_, ChildPid}) -> ChildPid ! {added_new_child, Pid, Name} end, State#state.children),
          io:format("ChildList: ~p~n", [State#state.children]),
          {noreply, State#state{children = NewChildren}};
        true ->
          {reply, {error, already_started}, State}
      end
  end;

handle_cast({stop_child, Name}, State) ->
  case proplists:is_defined(Name, State#state.children) of
    true ->
      keylist:stop(Name),
      NewChildren = lists:keydelete(Name, 1, State#state.children),
      {noreply, State#state{children = NewChildren}};
    false ->
      {reply, {error, not_found}, State}
  end;

handle_cast(stop, State) ->
  lists:foreach(fun({_, Pid}) -> exit(Pid, kill) end, State#state.children),
  {stop, normal, ok}.

%% @doc Handles non-call, non-cast messages to the keylist_mgr process.
handle_info(_Info, State) ->
  {noreply, State}.

%% @doc Cleans up the keylist_mgr process.
terminate(_Reason, _State) ->
  ok.

%% @doc Handles code changes in the keylist_mgr process.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.