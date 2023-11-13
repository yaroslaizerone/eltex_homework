# HW5 
## Задание 1

В Eshell создайте алиасы для функций  recursion:tail_fac/1 recursion:tail_duplicate/1 и вызовите функции через алиасы. (Функции мы определили в 4 ДР)
Пример:
	Fac = recursion:fac/1.
	Fac(10).

## Result

```
1> c(recursion).
{ok,recursion}
2> Fac = fun recursion:tail_fac/1.
fun recursion:tail_fac/1
3> Fac(10).
3628800
4> FacTail = fun recursion:tail_duplicate/1.
fun recursion:tail_duplicate/1
5> FacTail([1,3,7]).
[1,3,7,7,3,1]
```

## Задание 2

2.	Напишите анонимные функции
1) Для умножения 2х элементов
	2) Преобразуйте функцию converter:to_rub/1 (из прошлой ДР) в анонимную  функцию. Пример:
	ToRub = fun ({usd, Amount}) -> {ok, Amount * 75,5}; ({euro, Amount}) -> … end.
Вызовите функцию из Eshell:
	ToRub({usd, 100}).
ToRub({peso, 12}).
ToRub({yene, 30}).
ToRub({euro, -15}).

## Result 1

```
8> Mult = fun(A,B) -> A * B end.
#Fun<erl_eval.41.105768164>
9> Result = Mult(3,4).
12
```

## Result 2

Так как в заднии есть требование о создании аноннимной функции, происходит pattern matching при котором функция to_rub была преобразованна в данный вид:

```
10> ToRub = fun
 ..     ({usd, Amount}) when is_integer(Amount), Amount > 0 ->
 ..         USD = 75.5,
 ..         io:format("Convert ~p to rub, amount ~p~n", [usd, Amount]),
 ..         {ok, Amount * USD};
 ..     ({euro, Amount}) when is_integer(Amount), Amount > 0 ->
 ..         EURO = 80,
 ..         io:format("Convert ~p to rub, amount ~p~n", [euro, Amount]),
 ..         {ok, Amount * EURO};
 ..     ({peso, Amount}) when is_integer(Amount), Amount > 0 ->
 ..         PESO = 29,
 ..         io:format("Convert ~p to rub, amount ~p~n", [peso, Amount]),
 ..         {ok, Amount * PESO};
 ..     ({krone, Amount}) when is_integer(Amount), Amount > 0 ->
 ..         KRONE = 3,
 ..         io:format("Convert ~p to rub, amount ~p~n", [krone, Amount]),
 ..         {ok, Amount * KRONE};
 ..     ({lari, Amount}) when is_integer(Amount), Amount > 0 ->
            LARI = 10,
            io:format("Convert ~p to rub, amount ~p~n", [lari, Amount]),
            {ok, Amount * LARI};
        ({Currency, _Amount}) ->
            io:format("Can't convert to rub, error ~p~n", [Currency]),
            {error, badarg}
    end.
#Fun<erl_eval.42.105768164>
11> ToRub({usd, 100}).
Convert usd to rub, amount 100
{ok,7550.0}
12> ToRub({peso, 12}).
Convert peso to rub, amount 12
{ok,348}
13> ToRub({yene, 30}).
Can't convert to rub, error yene
{error,badarg}
14> ToRub({euro, -15}).
Can't convert to rub, error euro
{error,badarg}

```

# Задание 3 

3.	Создайте заголовочный файл person.hrl
●	кортеж -record(person, {id, name, age, gender}).
●	константы MALE: male, FEMALE: female (используйте define)

Создайте модуль persons.erl
●	filter/2 - Функцию фильтрации списка персон
Пример:
filter(Fun, Persons) -> lists:filter(Fun, Persons).
●	all/2 - Функцию проверки, что все персона подходят под условие (lists:all/2)
●	any/2 - Функцию проверки, что хотя бы одна персона подходит под условие (lists:any/2)
●	update/2 Функцию обновления данных персон
Пример:
			update(Fun, Persons) -> lists:map(Fun, Persons).
●	get_average_age/1 - Функцию подсчета среднего возраста персон из списка
○	Используйте {AgeSum, PersonsCount} = lists:foldl/1
○	В качестве начального значения аккумулятора используйте кортеж {0, 0}, где 0 - начальная сумма аккумулятора и 0 - количество персон в списке.
○	Функция возвращает AgeSum/PersonsCount
○	Обработайте случай, когда список пустой и происходит деление на 0

Eshell Вызовите функции (данные для Persons используйте из таблицы ДР2): 
●	Получите список из персон старше 30 лет
persons:filter(fun(#person{age = Age}) -> Age >= 30 end, Persons).
●	Получите список из мужчин
persons:filter/2
●	Проверьте, что в списке есть хотя бы одна женщина
persons:any/2
●	Проверьт,е что в списке все старше 20 (включая)
persons:all/2
●	Проверьте, что в списке все младше 30 (включая)
persons:all/2
●	Обновите возраст (+1) персоне с именем Jack
UpdateJackAge = 
     fun
         (#person{name = ”Jack”, age = Age} = Person) ->  Person#person{age=Age + 1};
        (Person) -> Person
    end.
persons:update(UpdateJackAge, Persons).

●	Обновите возраст (-1) всем женщинам :) 

```
%% API
-include("person.hrl").
-export([filter/2, all/2, any/2, update/2, get_average_age/1, start/0]).

filter(Fun, Persons) -> lists:filter(Fun, Persons).

all(Fun, Persons) -> lists:all(Fun, Persons).

any(Fun, Persons) -> lists:any(Fun, Persons).

update(Fun, Persons) -> lists:map(Fun, Persons).

get_average_age(Persons) ->
  {AgeSum, PersonsCount} = lists:foldl(fun(#person{age = Age}, {Sum, Count}) -> {Sum + Age, Count + 1} end, {0, 0}, Persons),
  case PersonsCount of
    0 -> {error, "Список персон пустой"};
    _ -> AgeSum / PersonsCount
  end.
```

## Result
```
2> c(persons).
{ok,persons}
3> rr("person.hrl").
[person]
4>  Persons = [#person{id = 1, name = "Bob", age = 23, gender = male},
       #person{id = 2, name = "Kate", age = 20, gender = female},
       #person{id = 3, name = "Jack", age = 34, gender = male},
       #person{id = 4, name = "Nata", age = 54, gender = female}].
[#person{id = 1,name = "Bob",age = 23,gender = male},
 #person{id = 2,name = "Kate",age = 20,gender = female},
 #person{id = 3,name = "Jack",age = 34,gender = male},
 #person{id = 4,name = "Nata",age = 54,gender = female}]
5> persons:filter(fun(#person{age = Age}) -> Age >= 30 end, Persons).
[#person{id = 3,name = "Jack",age = 34,gender = male},
 #person{id = 4,name = "Nata",age = 54,gender = female}]
%% неправильно задавал параметры функции
28> persons:any(fun(#person{gender = FEMALE}) -> true end, Persons).
true
29> persons:all(fun(#person{age = Age}) -> Age >= 20 end, Persons).
true
30> persons:all(fun(#person{age = Age}) -> Age =< 30 end, Persons).
false
31> UpdateJackAge =
         fun
             (#person{name = "Jack", age = Age} = Person) ->  Person#person{age=Age + 1};
            (Person) -> Person
        end.
#Fun<erl_eval.42.105768164>
32> persons:update(UpdateJackAge, Persons).
[#person{id = 1,name = "Bob",age = 23,gender = male},
 #person{id = 2,name = "Kate",age = 20,gender = female},
 #person{id = 3,name = "Jack",age = ___35___,gender = male},
 #person{id = 4,name = "Nata",age = 54,gender = female}]
 33> UpdateWomenAge =
         fun
             (#person{gender = female, age = Age} = Person) ->  Person#person{age=Age - 1};
            (Person) -> Person
        end.
#Fun<erl_eval.42.105768164>
34> persons:update(UpdateWomenAge, Persons).
[#person{id = 1,name = "Bob",age = 23,gender = male},
 #person{id = 2,name = "Kate",age = ___19___,gender = female},
 #person{id = 3,name = "Jack",age = 34,gender = male},
 #person{id = 4,name = "Nata",age = ___53___,gender = female}]

```

# Задание 4 
4.	Добавьте функцию catch_all/1 в новый модуль exceptions.erl
catch_all(Action) when is_function(Action, 0) ->
  try Action() of
    Result -> {ok, Result}
  catch
    throw:Reason ->
      io:format("Action ~p failed, reason ~p ~n", [Action, Reason]),
      throw;
    error:Reason ->
      io:format("Action ~p failed, reason ~p ~n", [Action, Reason]),
      error;
    exit:Reason ->
      io:format("Action ~p failed, reason ~p ~n", [Action, Reason]),
      exit;
    _:_ ->
      io:format("We covered all cases so this line will never be printed~n"),
      "Never will happen"
  end.

В Eshell выполните:
	exceptions:catch_all(fun() -> 1/0 end).
exceptions:catch_all(fun() -> throw(custom_exceptions) end).
exceptions:catch_all(fun() -> exit(killed) end).
exceptions:catch_all(fun() -> erlang:error(runtime_exception) end).

## Result
```
7> exceptions:catch_all(fun() -> 1/0 end).
Action #Fun<erl_eval.43.105768164> failed, reason badarith
** exception error: an error occurred when evaluating an arithmetic expression
     in function  exceptions:catch_all/1 (exceptions.erl, line 21)
8> exceptions:catch_all(fun() -> throw(custom_exceptions) end).
Action #Fun<erl_eval.43.105768164> failed, reason custom_exceptions
** exception throw: custom_exceptions
     in function  exceptions:catch_all/1 (exceptions.erl, line 18)
9> exceptions:catch_all(fun() -> exit(killed) end).
Action #Fun<erl_eval.43.105768164> failed, reason killed
** exception exit: killed
     in function  exceptions:catch_all/1 (exceptions.erl, line 24)
10> exceptions:catch_all(fun() -> erlang:error(runtime_exception) end).
Action #Fun<erl_eval.43.105768164> failed, reason runtime_exception
** exception error: runtime_exception
     in function  exceptions:catch_all/1 (exceptions.erl, line 21)
```