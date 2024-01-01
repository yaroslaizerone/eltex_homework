-module(keylist).
-author("kolpa").

-behaviour(gen_server).

%% API
-export([start_link/1, select/2, match_object/2, match/2, add/4, is_member/2, take/2, find/2, delete/2, stop/1]).

-include("key.hrl").

-record(state, {
  counter = 0 :: integer()
}).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API functions

%% @doc Start the keylist process
start_link(Name) ->
  io:format("Starting keylist process with Name: ~p~n", [Name]),
  gen_server:start_link({local, Name}, ?MODULE, Name, []).

%% @doc Adds a record to the keylist with the specified key, value, and comment.
-spec add(atom(), any(), any(), any()) -> ok.
add(Name, Key, Value, Comment) ->
  gen_server:call(Name, {add, Key, Value, Comment}).

%% @doc Checks if the specified key is a member of the keylist.
-spec is_member(atom(), any()) -> boolean().
is_member(Name, Key) ->
  gen_server:call(Name, {is_member, Key}).

%% @doc Retrieves the record with the specified key from the keylist.
-spec take(atom(), any()) -> any().
take(Name, Key) ->
  gen_server:call(Name, {take, Key}).

%% @doc Finds the record with the specified key in the keylist.
-spec find(atom(), any()) -> any().
find(Name, Key) ->
  gen_server:call(Name, {find, Key}).

%% @doc Deletes the record with the specified key from the keylist.
-spec delete(atom(), any()) -> ok.
delete(Name, Key) ->
  gen_server:call(Name, {delete, Key}).

%% @doc Stops the keylist process.
-spec stop(atom()) -> ok.
stop(Name) ->
  gen_server:call(Name, stop).

%% @doc Фильтрует и возвращает данные согласно заданному паттерну
-spec match(Name :: atom(), Pattern :: ets:match_pattern()) -> {ok, list()}.
match(Name, Pattern) ->
  gen_server:call(Name, {match, Pattern}).

%% @doc Фильтрует и возвращает объекты согласно заданному паттерну
-spec match_object(Name :: atom(), Pattern :: ets:match_pattern()) -> {ok, list()}.
match_object(Name, Pattern) ->
  gen_server:call(Name, {match_object, Pattern}).

%% @doc Фильтрует и возвращает данные согласно заданной функции фильтрации
-spec select(Name :: atom(), Filter :: fun()) -> {ok, list()}.
select(Name, Filter) ->
  gen_server:call(Name, {select, Filter}).

%% gen_server callbacks

%% @doc Initializes the keylist process.
init(Name) ->
  io:format("Initializing keylist process with Name: ~p~n", [Name]),
  {ok, #state{counter = 0}}.

handle_call({add, Key, Value, Comment}, _From, #state{counter = Counter} = State) ->
  Record = #keylist_record{key = Key, value = Value, comment = Comment},
  ets:insert(keylist_ets, Record),
  NewCounter = Counter + 1,
  NewState = State#state{counter = NewCounter},
  {reply, ok, NewState};

handle_call({is_member, Key}, _From, #state{counter = Counter} = State) ->
  Reply = ets:member(keylist_ets, Key),
  NewState = State#state{counter = Counter},
  {reply, Reply, NewState};

handle_call({take, Key}, _From, #state{counter = Counter} = State) ->
  case ets:lookup(keylist_ets, Key) of
    [] ->
      {reply, not_found, State};

    [Record] ->
      ets:delete(keylist_ets, Key),
      NewCounter = Counter - 1,
      NewState = State#state{counter = NewCounter},
      {reply, Record, NewState}
  end;

handle_call({find, Key}, _From, #state{counter = Counter} = State) ->
  Result = ets:lookup(keylist_ets, Key),
  NewState = State#state{counter = Counter},
  {reply, Result, NewState};

handle_call({delete, Key}, _From, #state{counter = Counter} = State) ->
  ets:delete(keylist_ets, Key),
  NewCounter = Counter - 1,
  NewState = State#state{counter = NewCounter},
  {reply, ok, NewState};

handle_call({match, Pattern}, _From, #state{counter = Counter} = State) ->
  Result = ets:match(keylist_ets, Pattern),
  NewState = State#state{counter = Counter},
  {reply, {ok, Result}, NewState};

handle_call({match_object, Pattern}, _From, #state{counter = Counter} = State) ->
  Result = ets:match_object(keylist_ets, Pattern),
  NewState = State#state{counter = Counter},
  {reply, {ok, Result}, NewState};

handle_call({select, FilterMs}, _From, #state{counter = Counter} = State) ->
  Result = ets:select(keylist_ets, FilterMs),
  NewState = State#state{counter = Counter},
  {reply, {ok, Result}, NewState};

handle_call(stop, _From, State) ->
  {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
  {reply, unknown_request, State}.

%% @doc Handles asynchronous casts to the keylist process.
handle_cast(_Msg, State) ->
  {noreply, State}.

%% @doc Handles non-call, non-cast messages to the keylist process.
handle_info(_Info, State) ->
  {noreply, State}.

%% @doc Cleans up the keylist process.
terminate(_Reason, _State) ->
  ok.

%% @doc Handles code changes in the keylist process.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.