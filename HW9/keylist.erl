%%%-------------------------------------------------------------------
%%% @author kolpa
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%% Module API keylist.erl
%%% @end
%%% Created : 07. нояб. 2023 13:08
%%%-------------------------------------------------------------------
-module(keylist).
-author("kolpa").

-export([start_link/1, add/4, is_member/2, take/2, find/2, delete/2, stop/1]).
-export_record([key]).

%% Record definition
-record(key, {
  key :: any(),
  value :: any(),
  comment :: any()
}).

%% @doc Создает новый процесс keylist с указанным именем.
%% Start the keylist process
start_link(Name) ->
  Pid = spawn_link(fun() -> init(Name) end),
  register(Name, Pid),
  Pid.

%% Initialize the keylist
init(Name) ->
  loop(Name, []).

%% Message handling loop
loop(Name, List) ->
  receive
    {From, add, Key, Value, Comment} ->
      NewList = [#key{key = Key, value = Value, comment = Comment} | List],
      From ! ok,
      loop(Name, NewList);

    {From, is_member, Key} ->
      Result = lists:keymember(Key, 1, List),
      From ! Result,
      loop(Name, List);

    {From, take, Key} ->
      {value, Record} = lists:keytake(Key, 1, List),
      From ! Record,
      loop(Name, List);

    {From, find, Key} ->
      Result = lists:keyfind(Key, 1, List),
      From ! Result,
      loop(Name, List);

    {From, delete, Key} ->
      NewList = lists:keydelete(Key, 1, List),
      From ! ok,
      loop(Name, NewList)
  end.

%% API functions

%% @doc Добавляет запись в keylist с указанным ключом, значением и комментарием.
-spec add(atom(), any(), any(), any()) -> ok.
add(Name, Key, Value, Comment) ->
  Name ! {self(), add, Key, Value, Comment}.

%% @doc Проверяет, является ли указанный ключ членом keylist.
-spec is_member(atom(), any()) -> boolean().
is_member(Name, Key) ->
  Name ! {self(), is_member, Key},
  receive
    Result -> Result
  end.

%% @doc Извлекает запись с указанным ключом из keylist.
-spec take(atom(), any()) -> any().
take(Name, Key) ->
  Name ! {self(), take, Key},
  receive
    Result -> Result
  end.

%% @doc Находит запись с указанным ключом в keylist.
-spec find(atom(), any()) -> any().
find(Name, Key) ->
  Name ! {self(), find, Key},
  receive
    Result -> Result
  end.

%% @doc Удаляет запись с указанным ключом из keylist.
-spec delete(atom(), any()) -> ok.
delete(Name, Key) ->
  Name ! {self(), delete, Key},
  receive
    Result -> Result
  end.

%% @doc Завершает процесс с ключом.
-spec stop(atom()) -> ok.
stop(Name) ->
  case whereis(Name) of
    undefined ->
      {error, not_found};
    Pid ->
      exit(Pid, kill),
      ok
  end.