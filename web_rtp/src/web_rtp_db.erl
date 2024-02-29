-module(web_rtp_db).

-behaviour(gen_server).

%% API
-export([start/0, stop/0]).
-export([insert_abonent/2, read_abonent/1,
  delete_abonent/1, read_all_abonent/0]).

%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% Name server
-define(SERVER, ?MODULE).

%% Map-record table abonents
-record(abonents, {
  num     :: non_neg_integer(),
  name    :: string()
}).

%% API-DB function

%% Starting web_rtp_db
-spec start() -> {ok, Pid :: pid()}.
start() ->
  {ok, Pid} = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Async stopping web_rtp_db
-spec stop() -> ok.
stop() ->
  gen_server:cast(?MODULE, stop).

%% Async adding record
-spec insert_abonent(Num :: non_neg_integer(), Name :: string()) -> ok.
insert_abonent(Num, Name) ->
  gen_server:cast(?MODULE, {insert, Num, Name}).

%% Sync reading the record through the key
-spec read_abonent(Num :: non_neg_integer()) ->
  {abonents, Num :: non_neg_integer(), Name :: string()}.
read_abonent(Num) ->
  gen_server:call(?MODULE, {read, Num}).

%% Sync reads all records from abonents
-spec read_all_abonent() ->
  list({abonents, Num :: non_neg_integer(), Name :: string()}).
read_all_abonent()->
  gen_server:call(?MODULE, {read_all}).

%% Async deletes record through the key
-spec delete_abonent(Num :: non_neg_integer()) -> ok.
delete_abonent(Num) ->
  gen_server:cast(?MODULE, {delete, Num}).

%% Start callbacks
init(_Args = State) ->
  io:format("~p init callback, was called!~n", [?MODULE]),
  case create_table() of
    {atomic, ok} ->
      io:format("Table 'abonents' was created!~n");
    {aborted, {already_exists, abonents}} ->
      io:format("Table 'abonents' already exists!~n")
  end,
  {ok, State}.

%% Create table abonents
create_table() ->
  mnesia:create_schema([node()]),
  mnesia:start(),
  mnesia:create_table(abonents, [
    {ram_copies, [node()]}, {attributes, record_info(fields, abonents)}]).

%% Sync handlers
handle_call({read, Num}, _From, State) ->
  Fun =
    fun() ->
      mnesia:read(abonents, Num)
    end,
  {atomic, Result} = mnesia:transaction(Fun),

  case Result of
    [{abonents, Num, Name}] ->
      {reply, {abonents, Num, Name}, State};
    [] ->
      {reply, not_found, State}
  end;
handle_call({read_all}, _From, State) ->
  Fun =
    fun() ->
      mnesia:select(abonents, [{'_', [], ['$_']}])
    end,
  {atomic, Result} = mnesia:transaction(Fun),
  {reply, Result, State}.

%% Async handlers
handle_cast({insert, Num, Name},  State) ->
  Rec = #abonents{num = Num, name = Name},
  Fun =
    fun() ->
      case mnesia:read(abonents, Num) of
        [] ->
          mnesia:write(Rec);
        [_] ->
          {error, already_exists}
      end
    end,
  Result = mnesia:transaction(Fun),
  io:format("Record was insert with code: ~p~n", [Result]),
  {noreply, State};
handle_cast({delete, Num},  State) ->
  Fun =
    fun() ->
      case mnesia:read(abonents, Num) of
        [_] ->
          mnesia:delete({abonents, Num});
        [] ->
          {error, wrong_number}
      end
    end,
  Result = mnesia:transaction(Fun),
  io:format("Record was deleted with code: ~p~n", [Result]),
  {noreply, State};
handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast(Info, State) ->
  io:format("Received async message ~p~n", [Info]),
  {noreply, State}.

%% Info handlers
handle_info(Msg, State) ->
  io:format("Received message ~p~n", [Msg]),
  {noreply, State}.

%% Callback stop server
terminate(Reason, _State) ->
  io:format("Database was terminated with reason: ~p~n", [Reason]),
  case Reason of
    normal ->
      mnesia:delete_table(abonents);
    _ ->
      ok
  end.