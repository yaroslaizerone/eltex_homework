# HW7
## Задание 1
Создайте модуль keylist.erl. 
Создайте рекорд #state{list, counter} который мы будем использовать для сохранения состояния процесса. Подумайте какие значения по умолчанию нам нужно задать для полей list и counter. В list мы будем хранить список кортежей {Key, Value, Comment}. counter мы будем использовать как счетчик выполненных команд.

## Result
```
loop(#state{list = List, counter = Counter} = State) ->
  receive
    {From, add, Key, Value, Comment} ->
      NewList = [{Key, Value, Comment} | List],
      NewCounter = Counter + 1,
      NewState = State#state{list = NewList, counter = NewCounter},
      From ! {ok, NewCounter},
      loop(NewState);

    {From, is_member, Key} ->
      Result = lists:keymember(Key, 1, List),
      From ! Result,
      loop(State);

    {From, take, Key} ->
      {value, Element} = lists:keytake(Key, 1, List),
      NewList = tl(Element),
      From ! Element,
      loop(State#state{list = NewList});

    {From, find, Key} ->
      {value, Element} = lists:keyfind(Key, 1, List),
      From ! Element,
      loop(State);

    {From, delete, Key} ->
      NewList = lists:keydelete(Key, 1, List),
      From ! ok,
      loop(State#state{list = NewList})
  end.


``` 
## Задание 2
Добавьте в keylist.erl функцию start_monitor/1, start_link/1.
	start_monitor(Name) ->
		…
		{ok, Pid, MonitorRef}.

	start_link(Name) ->
		…
		{ok, Pid}.

В функции start/1:
●	Запустите процесс;
●	Зарегистрируйте его с именем Name;
●	Назначьте на него монитор;
●	В качестве результата верните Pid, MonitorRef.

В функции start_link/1:
●	Запустите процесс и установите линк нового процесса с процессом который его запустил;
●	Зарегистрируйте его с именем Name;
●	В качестве результата верните Pid.

```start_monitor(Name) ->
  Pid = spawn_link(fun() -> loop(#state{}) end),
  MonitorRef = erlang:monitor(Name, Pid),
  {ok, Pid, MonitorRef}.

start_link(Name) ->
  Pid = spawn_link(fun() -> loop(#state{}) end),
  true = register(Name, Pid),
  {ok, Pid}.
  ```

## Result
```
1> self().
<0.85.0>
2> {ok, PidMonitor, MonitorRef} = keylist:start_monitor(monitored).
{ok,<0.88.0>,#Ref<0.3537456144.3644850184.76710>}
3> {ok, PidLinked} = keylist:start_link(linked).
{ok,<0.90.0>}
4> PidLinked ! {self(), add, "key1", "value1", "comment1"}.
{<0.85.0>,add,"key1","value1","comment1"}
5> PidLinked ! {self(), is_member, "key1"}.
{<0.85.0>,is_member,"key1"}
6> PidLinked ! {self(), find, "key1"}.
{<0.94.0>,find,"key1"}
9> flush().
ok
11> exit(PidMonitor, normal).
true
12> flush().
ok
13> self().
<0.99.0>
14> exit(PidLinked, normal).
true
15> flush().
ok

```  
## Задание 3 
Завершите процесс Monitored с помощью функции exit(Pid, Reason).
## Result
```
11> exit(PidMonitor, normal).
true
12> flush(). # Должно выводить Shell got {'DOWN',#Ref<0.3537456144.3644850184.76710>,
              process,<0.88.0>,normal}
			   Отправляет сигнал выхода с причиной выхода Reason процессу или порту, определенному Pid
ok
13> self(). # Pid текущего процесса изменится
<0.99.0>

19> exit(PidLinked, normal).
true
20> flush(). # Shell got {'EXIT',<0.190.0>,normal}
ok
21> self(). # Pid текущего процесса изменится
<0.112.0>

``` 
## Задание 4 
Вызовите функцию process_flag(trap_exit, true).
Запустите еще раз keylist:start_link(linked).
Завершите процесс Linked с помощью функции exit(Pid, Reason).
Проверьте почтовый ящик вашего процесса (flush/0). 
Проверьте ваш pid (self/0).
Прокомментируйте содержимое почтового ящика и изменился ли ваш pid.

## Result
```
36> process_flag(trap_exit, true).
false
39> {ok,Pid} = keylist:start_link(linked).
{ok,<0.143.0>}
40> exit(Pid, normal).
true
41> flush(). # Shell got {'EXIT',<0.196.0>,normal}
ok
42> self(). # Pid текущего процесса изменится на преведущий
<0.141.0>
``` 
## Задание 5 
Запустите keylist:start_link(linked1) и keylist:start_link(linked2). У вас теперь два процесса связаны с вашим процессом.
Завершите процесс Linked1 с помощью функции exit(Pid, Reason).
Проверьте ваш pid (self/0). Изменился ли ваш pid? 
Проверьте состояние процесса Linked2. Прокомментируйте.

## Result
```
47> {ok, Lincked1} = keylist:start_link(linked1).
{ok,<0.155.0>}
48> {ok, Lincked2} = keylist:start_link(linked2).
{ok,<0.157.0>}
49> exit(Lincked1, normal).
true
50> self(). # Так как создавая Lincked2 мы обозначили его как дочерний процесс, при завершении материнского процесса, завершился дочерний и pid изменился до момента создания Lincked1
<0.153.0>
process_info(Lincked2).
[{registered_name,linked2},
 {current_function,{keylist,loop,1}},
 {initial_call,{erlang,apply,2}},
 {status,waiting},
 {message_queue_len,0},
 {links,[<0.153.0>]},
 {dictionary,[]},
 {trap_exit,false},
 {error_handler,error_handler},
 {priority,normal},
 {group_leader,<0.70.0>},
 {total_heap_size,233},
 {heap_size,233},
 {stack_size,7},
 {reductions,7},
 {garbage_collection,[{max_heap_size,#{error_logger => true,include_shared_binaries => false,
                                       kill => true,size => 0}},
                      {min_bin_vheap_size,46422},
                      {min_heap_size,233},
                      {fullsweep_after,65535},
                      {minor_gcs,0}]},
 {suspending,[]}]

``` 


# Доработанный Результат
```
1> c(keylist).
{ok,keylist}
2> self().
<0.85.0>
3> keylist:start_monitor(monitored).
{ok,<0.93.0>,#Ref<0.462696011.3418619908.39359>}
4>  keylist:start_link(linked).
{ok,<0.95.0>}
5> MonitoredPid = whereis(monitored).
<0.93.0>
6> LinkedPid = whereis(linked).
<0.95.0>
7> MonitoredPid ! {self(), add, key1, value1, "comment1"}.
{<0.85.0>,add,key1,value1,"comment1"}
8> MonitoredPid ! {self(), add, key2, value2, "comment2"}.
{<0.85.0>,add,key2,value2,"comment2"}
9> MonitoredPid ! {self(), is_member, key1}.
{<0.85.0>,is_member,key1}
10> MonitoredPid ! {self(), take, key1}.
{<0.85.0>,take,key1}
11> flush().
Shell got {ok,1}
Shell got {ok,2}
Shell got true
Shell got {key1,value1,"comment1"}
ok
12> self().
<0.85.0>
13> MonitoredPid ! {self(), find, key2}.
{<0.85.0>,find,key2}
14> MonitoredPid ! {self(), delete, key2}.
{<0.85.0>,delete,key2}
15> flush().
Shell got {key2,value2,"comment2"}
Shell got []
ok
16> exit(MonitoredPid, normal).
true
17> flush().
ok
18> self().
<0.85.0>
19> exit(LinkedPid, normal).
true
20> flush().
ok
21> self().
<0.85.0>
22> process_flag(trap_exit, true).
false
23> self().
<0.85.0>
24> keylist:start_link(linked).
** exception error: bad argument
     in function  register/2
        called as register(linked,<0.116.0>)
        *** argument 1: name is in use
     in call from keylist:start_link/1 (keylist.erl, line 24)
25> keylist:start_link(linked).
{ok,<0.119.0>}
26> exit(LinkedPid, kill).
true
27> flush().
ok
28> self().
<0.117.0>
29> process_flag(trap_exit, false).
false
30> keylist:start_link(linked1).
{ok,<0.125.0>}
31> keylist:start_link(linked2).
{ok,<0.127.0>}
32> exit(LinkedPid, kill).
true
33> self().
<0.117.0>
34> keylist:loop(linked2).
** exception error: no function clause matching keylist:loop(linked2) (keylist.erl, line 27)
35> self().
<0.131.0>
```