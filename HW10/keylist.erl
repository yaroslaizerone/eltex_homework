-module(keylist).
-author("kolpa").

-behaviour(gen_server).

%% API
-export([start_link/1, add/4, is_member/2, take/2, find/2, delete/2, stop/1]).

%% Record definition
-record(key, {
  key :: any(),
  value :: any(),
  comment :: any()
}).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API functions

%% @doc Start the keylist process
start_link(Name) ->
  gen_server:start_link({local, Name}, ?MODULE, [], []).

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

%% gen_server callbacks

%% @doc Initializes the keylist process.
init([]) ->
  {ok, []}.

%% @doc Handles synchronous calls to the keylist process.
handle_call({add, Key, Value, Comment}, _From, List) ->
  NewList = [{Key, Value, Comment} | List],
  {reply, NewList, NewList};

handle_call({is_member, Key}, _From, List) ->
  Reply = lists:keymember(Key, 1, List),
  {reply, Reply, List};

handle_call({take, Key}, _From, List) ->
  case lists:keytake(Key, 1, List) of
    false ->
      {reply, not_found, List};

    {value, Record, NewList} ->
      {reply, Record, NewList}
  end;

handle_call({find, Key}, _From, List) ->
  Result = lists:keyfind(Key, 1, List),
  {reply, Result, List};

handle_call({delete, Key}, _From, List) ->
  NewList = lists:keydelete(Key, 1, List),
  {reply, ok, NewList};

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