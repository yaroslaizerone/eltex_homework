# HW8
# Задание
Создайте модуль keylist_mgr.erl.
Создайте рекорд #state{children} который мы будем использовать для сохранения состояния процесса.  В #state.children мы будем хранить список кортежей {Name, Pid}. Подумайте какие значения по умолчанию нам нужно задать для поля children.
# Result 
```
-record(state, {children=[]}).

loop(#state{children=Children}=State) ->
  process_flag(trap_exit, true),
  receive
    {From, start_child, Name} ->
      case lists:keyfind(Name, 1, Children) of
        false ->
          {ok, Pid} = keylist:start_link(Name),
          NewChildren = [{Name, Pid} | Children],
          From ! {ok, Pid},
          loop(State#state{children=NewChildren});
        {_, Pid} ->
          From ! {error, already_started},
          loop(State)
      end;

    {From, stop_child, Name} ->
      case lists:keyfind(Name, 1, Children) of
        {_, Pid} ->
          keylist:stop(Name),
          NewChildren = lists:keydelete(Name, 1, Children),
          From ! ok,
          loop(State#state{children=NewChildren});
        false ->
          From ! {error, not_found},
          loop(State)
      end;

    stop ->
      exit(normal);

    {From, get_names} ->
      Names = [Name || {Name, _} <- Children],
      From ! Names,
      loop(State);

    {'EXIT', Pid, Reason} ->
      NewChildren = lists:keydelete(Pid, 2, Children),
      error_logger:error_msg("Process ~p exited with reason ~p~n", [Pid, Reason]),
      loop(State#state{children=NewChildren})
  end.
```

# Задание 2
Добавьте в keylist_mgr.erl функцию start/0.
	start() ->
		…
		{ok, Pid, MonitorRef}.

В функции start/1:
●	Запустите процесс;
●	Зарегистрируйте его с именем ?MODULE;
●	Назначьте на него монитор;
●	В качестве результата верните Pid, MonitorRef


# Result code:
```
start() ->
  Pid = spawn(fun() -> loop(#state{}) end),
  MonitorRef = erlang:monitor(process, Pid),
  register(?MODULE, Pid),
  {ok, Pid, MonitorRef}.
```
# Result 
```
3> self().
<0.85.0>
4>  keylist_mgr:start().
{ok,<0.98.0>,#Ref<0.1351239661.389283851.250938>}
5> keylist_mgr ! {self(), start_child, keylist1}.
{<0.85.0>,start_child,keylist1}
6> keylist_mgr ! {self(), start_child, keylist2}.
{<0.85.0>,start_child,keylist2}
7> keylist_mgr ! {self(), start_child, keylist3}.
{<0.85.0>,start_child,keylist3}
8> keylist3 ! {key1, value1, "Comment 1"}.
{key1,value1,"Comment 1"}
9> keylist3 ! {key2, value2, "Comment 2"}.
{key2,value2,"Comment 2"}
10> flush().
Shell got {ok,<0.100.0>}
Shell got {ok,<0.102.0>}
Shell got {ok,<0.104.0>}
ok

```
# Задание 3 
Завершите процесс keylist1 с помощью функции exit(Pid, Reason).
P.s. Чтобы найти Pid по имени мы можем использовать команду whereis/1.
Проверьте почтовый ящик вашего процесса (flush/0). 
Проверьте ваш pid (self/0).
Проверьте процессы keylist2, keylist3.
Проверьте keylist_mgr залогировал падение процесса?
Прокомментируйте происходящее.

Завершите процесс keylist_mgr с помощью функции exit(Pid, Reason).
Проверьте почтовый ящик вашего процесса (flush/0). 
Проверьте ваш pid (self/0).
Прокомментируйте происходящее и изменился ли ваш pid.

Завершите процесс keylist_mgr с помощью
keylist_mgr ! stop.
Проверьте почтовый ящик вашего процесса (flush/0). 
Проверьте процессы keylist2, keylist3 завершились.
Прокомментируйте происходящее и изменился ли ваш pid.

# Result
```
11> Pid = whereis(keylist1). 
<0.100.0>
12> exit(Pid, normal). % Завершаем процесс keylist1
true
13> flush().
ok
14> self().
<0.85.0>
15> whereis(keylist2).
<0.102.0>
16> whereis(keylist3).
<0.104.0>
17> flush(). % Проверяя остальные процессы, удалось получить результат об их нормальной работе
ok
18> Pid1 = whereis(keylist_mgr).
<0.98.0>
19> exit(Pid1, normal).
=ERROR REPORT==== 12-Nov-2023::02:06:05.993000 ===
Process <0.85.0> exited with reason normal

true % остановка главного(материнского) процесса произошла успешно
20> whereis(keylist2).
undefined
21> whereis(keylist3).
undefined % остановка главного процесса привела к остановке дочерних 
22> self().
<0.85.0>

```

Таким образом было реализованно базовое распределение хранилища значений ключей. В котором получаемые значения ключей хранятся в главном процессе и при завершении главного процесса (удалении хранилища) удаляются значения ключей(завершаются дочерние процессы).