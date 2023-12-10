# HW9 
# Задание 1 
1. Обновите модуль keylist_mgr.erl.
 Добавьте в keylist_mgr.erl - API функции для всех сообщений. 
-  {From, start_child, Name}
-  {From, stop_child, Name}
-  stop
-  {From, get_names}
Например:
	start_child(Name) ->
		key_manager !  {self(), start_child, Name}.

# Result
```
%%%-------------------------------------------------------------------
%%% @author kolpa
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%% Module API keylist_mgr.erl
%%% @end
%%% Created : 11. нояб. 2023 17:27
%%%-------------------------------------------------------------------
-module(keylist_mgr).
-author("kolpa").

%% API
-export([start/0, init/0, loop/1, start_child/1, stop_child/1, stop/0, get_names/0]).

%% Record definition
-record(state, {children=[], permanent=[]}).

%% API functions

%% @doc Start keylist_mgr
-spec start() -> {ok, Pid :: pid()} | {error, Reason :: term()}.
start() ->
  Pid = spawn_link(?MODULE, init, []),
  {ok, Pid}.

%% @doc Start a child process
-spec start_child(Params :: #{name => atom(), restart => permanent | temporary}) -> ok.
start_child(Params) ->
  ?MODULE ! {self(), start_child, Params}.

%% @doc Stop a child process
-spec stop_child(Name :: atom()) -> ok.
stop_child(Name) ->
  ?MODULE ! {self(), stop_child, Name}.

%% @doc Stop keylist_mgr
-spec stop() -> ok.
stop() ->
  ?MODULE ! stop.

%% @doc Get the list of child process names
-spec get_names() -> ok.
get_names() ->
  ?MODULE ! {self(), get_names}.

%% @doc Initialize keylist_mgr
-spec init() -> ok.
init() ->
  process_flag(trap_exit, true),
  register(?MODULE, self()),
  loop(#state{}).

%% @doc Message handling loop
-spec loop(State :: #state{}) -> ok.
loop(#state{children=Children, permanent=Permanent}=State) ->
  receive
    {From, start_child, #{name := Name, restart := Restart}} ->
      case proplists:is_defined(Name,Children) of
        false ->
          Pid = keylist:start_link(Name),
          NewChildren =
          case Restart of
            permanent ->
              [{Name, Pid} | Children];
            temporary ->
              Children;
            _ ->
              From ! {error, type_error}
          end,
          io:format("В Сhildren записалось: ~p~n", [NewChildren]),
          From ! {add, Name, Pid},
          loop(State#state{children = NewChildren});
        true ->
          From ! {error, already_started}
      end;

    {From, stop_child, Name} ->
      case proplists:is_defined(Name,Children) of
        true ->
          keylist:stop(Name),
          NewChildren = lists:keydelete(Name, 1, Children),
          From ! {stop, Name},
          loop(State#state{children=NewChildren});
        false ->
          From ! {error, not_found},
          loop(State)
      end;

    stop ->
      lists:foreach(fun({_, Pid}) -> exit(Pid, kill) end, Children),
      ok;

    {From, get_names} ->
      From ! [Name || {Name, _} <- Children],
      loop(State);

    {'EXIT', Pid, Reason} ->
      case lists:keyfind(Pid, 2, Children) of
        {Name, _} ->
          case proplists:get_value(restart, proplists:get_value(Name, Permanent, temporary)) of
            permanent ->
              {ok, NewPid} = keylist:start_link(Name),
              NewChildren = lists:keyreplace(Name, 1, Children, {Name, NewPid}),
              loop(State#state{children=NewChildren});
            temporary ->
              error_logger:error_msg("Process ~p exited with reason ~p~n", [Pid, Reason]),
              loop(State#state{children=lists:keydelete(Name, 1, Children)})
          end;
        false ->
          error_logger:error_msg("Process ~p exited with reason ~p~n", [Pid, Reason]),
          loop(State#state{children=lists:keydelete(Pid, 2, Children)})
      end
  end.
```

# Задание 2
2. Обновите модуль keylist.erl.
Добавьте в keylist.erl - API функции для всех сообщений. 
- {From, add, Key, Value, Comment};
- {From, is_member, Key}; 
- {From, take, Key}; 
- {From, find, Key};
- {From, delete, Key}
Например:
	add(Name, Key, Value, Comment) ->
		Name !  {self(), add, Key, Value, Comment}.

# Result
```
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
```
# Result 

```
1> keylist_mgr:start().
{ok,<0.87.0>}
2>  keylist_mgr:start_child(#{name => keylist1, restart => permanent}).
{<0.85.0>,start_child,
 #{name => keylist1,restart => permanent}}
В Сhildren записалось: [{keylist1,<0.89.0>}]
3> keylist_mgr:start_child(#{name => keylist2, restart => permanent}).
В Сhildren записалось: [{keylist2,<0.91.0>},{keylist1,<0.89.0>}]
{<0.85.0>,start_child,
 #{name => keylist2,restart => permanent}}
4>  keylist_mgr:start_child(#{name => keylist3, restart => temporary}).
В Сhildren записалось: [{keylist2,<0.91.0>},{keylist1,<0.89.0>}]
{<0.85.0>,start_child,
 #{name => keylist3,restart => temporary}}
5> flush().  % Результатом как и планировалось будут добавлены процессы
Shell got {add,keylist1,<0.89.0>}
Shell got {add,keylist2,<0.91.0>}
Shell got {add,keylist3,<0.93.0>} % блин тут ошибочка я его не должен был запускать, но логика проверки на restart присутсвует.
ok
6> keylist_mgr:get_names().
{<0.85.0>,get_names}
7> flush().
Shell got [keylist2,keylist1] % Получаем список всех Children, что записывались ранее
ok
8> keylist_mgr:stop_child(keylist1).
{<0.85.0>,stop_child,keylist1}
=ERROR REPORT==== 20-Nov-2023::13:14:31.183000 ===
Process <0.89.0> exited with reason killed
% останавливаем дочений процесс c помощью exit(Name, kill)
9> whereis(keylist1).
undefined % процесс остановлен
10> whereis(keylist2). % другой дочерний всё ещё живвой
<0.91.0>
11> keylist_mgr:stop(). % останавливаем главный процесс
stop
12> flush().
Shell got {stop,keylist1} % В коде нет отправки на почтовый ящик того, что я его останавливаю. 
ok
13> whereis(keylist2).
undefined # Как и было задуманно, если упал основной, дочерние тоже падают.
```