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
      {ok, Pid} = keylist:start_link(Name),
      NewChildren = [{Name, Pid} | State#state.children],
      {noreply, State#state{children = NewChildren, permanent = State#state.permanent}}; % Do nothing if restart is temporary
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
          NewPermanent = [Pid | State#state.permanent],
          lists:foreach(fun({_, ChildPid}) -> ChildPid ! {added_new_child, Pid, Name} end, State#state.children),
          io:format("ChildList: ~p~n", [State#state.children]),
          {noreply, State#state{children = NewChildren, permanent = NewPermanent}};
        true ->
          {reply, {error, already_started}, State}
      end
  end;

handle_cast({stop_child, Name}, State) ->
  case proplists:is_defined(Name, State#state.children) of
    true ->
      keylist:stop(Name),
      NewChildren = lists:keydelete(Name, 1, State#state.children),
      NewPermanent = lists:keydelete(whereis(Name), 1, State#state.permanent),
      {noreply, State#state{children = NewChildren, permanent = NewPermanent}};
    false ->
      {reply, {error, not_found}, State}
  end;

handle_cast(stop, State) ->
  lists:foreach(fun({_, Pid}) -> exit(Pid, kill) end, State#state.children),
  {stop, normal, ok}.

%% @doc Handle unexpected exits of child processes
handle_info({'EXIT', Pid, Reason}, State) ->
  #state{children=Children, permanent=Permanent} = State,
  case lists:keyfind(Pid, 2, Children) of
    {Name, _} ->
      %lists:member(Pid, Permanent)
      %true -> удалить pid
      case lists:member(Pid, Permanent) of
        true ->
          {ok, NewPid} = keylist:start_link(Name),
          NewChildren = lists:keyreplace(Name, 1, Children, {Name, NewPid}),
          NewPermanent = lists:delete(Pid, Permanent),%lists:delete
          NewPermanent2 = [NewPid|NewPermanent],
          {noreply, State#state{children=NewChildren, permanent=NewPermanent2}};
        false ->
          error_logger:error_msg("Process ~p exited with reason ~p~n", [Pid, Reason]),
          {noreply, State#state{children=lists:keydelete(Name, 1, Children)}}
      end;
    false ->
      {noreply, State}
  end;

handle_info({'DOWN', process, Pid}, State) ->
  #state{children=Children, permanent=Permanent} = State,
  case lists:keyfind(Pid, 2, Children) of
    {Name, _} ->
      %% Process with Name and Pid terminated, handle accordingly
      NewChildren = lists:keydelete(Name, 1, Children),
      NewPermanent = lists:delete(Pid, Permanent),
      {noreply, State#state{children=NewChildren, permanent=NewPermanent}};
    false ->
      %% Process not found in children, handle accordingly
      {noreply, State}
  end;

%% @doc Handles non-call, non-cast messages to the keylist_mgr process.
handle_info(_Info, State) ->
  {noreply, State}.

%% @doc Cleans up the keylist_mgr process.
terminate(_Reason, _State) ->
  ok.

%% @doc Handles code changes in the keylist_mgr process.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.