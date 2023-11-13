# Домашняя работа 4

## Задание 1

to_rub2 :
```
to_rub2({Type, Amount} = Arg) when is_integer(Amount)->
  case Amount > 0 of
    true ->
      Result = case Arg of
                 {usd, Amount} ->
                   io:format("Convert ~p to rub, amount ~p~n", [Type, Amount]),
                   USD = 75.5,
                   {ok, USD * Amount};
                 {euro, Amount} ->
                   io:format("Convert ~p to rub, amount ~p~n", [Type, Amount]),
                   EURO = 80,
                   {ok, EURO * Amount};
                 {peso, Amount} ->
                   io:format("Convert ~p to rub, amount ~p~n", [Type, Amount]),
                   PESO = 29,
                   {ok, PESO * Amount};
                 {krone, Amount} ->
                   io:format("Convert ~p to rub, amount ~p~n", [Type, Amount]),
                   KRONE = 3,
                   {ok, KRONE * Amount};
                 {lari, Amount} ->
                   io:format("Convert ~p to rub, amount ~p~n", [Type, Amount]),
                   LARI = 10,
                   {ok, LARI * Amount};
                 _Error ->
                   io:format("Can’t convert to rub, error ~p~n", [_Error]),
                   {error, badarg}
               end,
      io:format("Convert ~p to rub, amount ~p, Result ~p~n", [Type, Amount, Result]),
      Result;
    false ->
      io:format("Incorrect amount value: ~p~n", [Amount]),
      {error, badarg}
  end.
```
### Result
```
20> c("converter.erl").
{ok,converter}
21> converter:to_rub2({usd, 100}).
Convert usd to rub, amount 100
Convert usd to rub, amount 100, Result {ok,7550.0}
{ok,7550.0}
22> converter:to_rub2({peso, 12}).
Convert peso to rub, amount 12
Convert peso to rub, amount 12, Result {ok,348}
{ok,348}
23> converter:to_rub2({yene, 30}).
Can’t convert to rub, error {yene,30}
Convert yene to rub, amount 30, Result {error,badarg}
{error,badarg}
24> converter:to_rub2({euro, -15}).
Incorrect amount value: -15
{error,badarg}
```

to_rub3:

```
to_rub3({Type, Amount} =Arg) when is_integer(Amount)->
  case Amount > 0 of
    true ->
      case Arg of
        {usd, Amount} ->
          io:format("Convert ~p to rub, amount ~p~n|", [Type, Amount]),
          USD = 75.5,
          {ok, USD*Amount};
        {euro, Amount} ->
          io:format("Convert ~p to rub, amount ~p~n", [Type, Amount]),
          EURO = 80,
          {ok, EURO*Amount};
        {peso, Amount} ->
          io:format("Convert ~p to rub, amount ~p~n", [Type, Amount]),
          PESO = 29,
          {ok, PESO*Amount};
        {krone, Amount} ->
          io:format("Convert ~p to rub, amount ~p~n", [Type, Amount]),
          KRONE = 3,
          {ok, KRONE*Amount};
        {lari, Amount} ->
          io:format("Convert ~p to rub, amount ~p~n", [Type, Amount]),
          LARI = 10,
          {ok, LARI*Amount};
        _Error ->
          io:format("Can’t convert to rub, error ~p~n", [_Error]),
          {error, badarg}
      end;
    false ->
      io:format("Incorrect amount value: ~p~n", [Amount]),
      {error, badarg}
end.
```

### Result
```
31> converter:to_rub3({usd, 100}).
Convert usd to rub, amount 100
|{ok,7550.0}
32> converter:to_rub3({peso, 12}).
Convert peso to rub, amount 12
{ok,348}
33> converter:to_rub3({yene, 30}).
Can’t convert to rub, error {yene,30}
{error,badarg}
34> converter:to_rub3({euro, -15}).
Incorrect amount value: -15
{error,badarg}
```

rec_to_rub:
```
rec_to_rub(#conv_info{type = usd, amount = Amount, commission = Commission}) when is_number(Amount), Amount > 0 ->
    USD = 75.5,
    ConvAmount = Amount * USD,
    CommissionResult = ConvAmount * Commission,
    {ok, ConvAmount - CommissionResult};
rec_to_rub(#conv_info{type = euro, amount = Amount, commission = Commission}) when is_number(Amount), Amount > 0 ->
    EURO = 80,
    ConvAmount = Amount * EURO,
    CommissionResult = ConvAmount * Commission,
    {ok, ConvAmount - CommissionResult};
rec_to_rub(#conv_info{type = peso, amount = Amount, commission = Commission}) when is_number(Amount), Amount > 0 ->
    PESO = 29,
    ConvAmount = Amount * PESO,
    CommissionResult = ConvAmount * Commission,
    {ok, ConvAmount - CommissionResult};
rec_to_rub(#conv_info{type = krone, amount = Amount, commission = Commission}) when is_number(Amount), Amount > 0 ->
    KRONE = 3,
    ConvAmount = Amount * KRONE,
    CommissionResult = ConvAmount * Commission,
    {ok, ConvAmount - CommissionResult};
rec_to_rub(#conv_info{type = lari, amount = Amount, commission = Commission}) when is_number(Amount), Amount > 0 ->
    LARI = 10,
    ConvAmount = Amount * LARI,
    CommissionResult = ConvAmount * Commission,
    {ok, ConvAmount - CommissionResult};
rec_to_rub(#conv_info{type = Currency, amount = Amount, commission = Commission}) ->
    io:format("Can’t convert to rub, error ~p ~p ~p~n", [Currency, Amount, Commission]),
    {error, badarg}.
```

## Result

```
> converter:rec_to_rub(#conv_info{type = usd, amount = 100, commission = 0.01}).
{ok,7474.5}
3> converter:rec_to_rub(#conv_info{type = peso, amount = 12, commission = 0.02}).
{ok,341.04}
4> converter:rec_to_rub(#conv_info{type = yene, amount = 30, commission = 0.02}).
Can’t convert to rub, error yene 30 0.02
{error,badarg}
5> converter:rec_to_rub(#conv_info{type = euro, amount = -15, commission = 0.02}).
Can’t convert to rub, error euro -15 0.02
{error,badarg}

```

map_to_rub:
```
rec_to_rub(#conv_info{type = usd, amount = Amount, commission = Commission}) when is_number(Amount), Amount > 0 ->
    USD = 75.5,
    ConvAmount = Amount * USD,
    CommissionResult = ConvAmount * Commission,
    {ok, ConvAmount - CommissionResult};
rec_to_rub(#conv_info{type = euro, amount = Amount, commission = Commission}) when is_number(Amount), Amount > 0 ->
    EURO = 80,
    ConvAmount = Amount * EURO,
    CommissionResult = ConvAmount * Commission,
    {ok, ConvAmount - CommissionResult};
rec_to_rub(#conv_info{type = peso, amount = Amount, commission = Commission}) when is_number(Amount), Amount > 0 ->
    PESO = 29,
    ConvAmount = Amount * PESO,
    CommissionResult = ConvAmount * Commission,
    {ok, ConvAmount - CommissionResult};
rec_to_rub(#conv_info{type = krone, amount = Amount, commission = Commission}) when is_number(Amount), Amount > 0 ->
    KRONE = 3,
    ConvAmount = Amount * KRONE,
    CommissionResult = ConvAmount * Commission,
    {ok, ConvAmount - CommissionResult};
rec_to_rub(#conv_info{type = lari, amount = Amount, commission = Commission}) when is_number(Amount), Amount > 0 ->
    LARI = 10,
    ConvAmount = Amount * LARI,
    CommissionResult = ConvAmount * Commission,
    {ok, ConvAmount - CommissionResult};
rec_to_rub(#conv_info{type = Currency, amount = Amount, commission = Commission}) ->
    io:format("Can’t convert to rub, error ~p ~p ~p~n", [Currency, Amount, Commission]),
    {error, badarg}.

map_to_rub(#{type := usd, amount := Amount, commission := Commission}) when is_number(Amount), Amount > 0 ->
    USD = 75.5,
    Result = Amount * USD,
    CommissionResult = Result * Commission,
    {ok, Result - CommissionResult };
map_to_rub(#{type := euro, amount := Amount, commission := Commission}) when is_number(Amount), Amount > 0 ->
    EURO = 80,
    Result = Amount * EURO,
    CommissionResult = Result * Commission,
    {ok, Result - CommissionResult };
map_to_rub(#{type := peso, amount := Amount, commission := Commission}) when is_number(Amount), Amount > 0 ->
    PESO = 29,
    Result = Amount * PESO,
    CommissionResult = Result * Commission,
    {ok, Result - CommissionResult };
map_to_rub(#{type := krone, amount := Amount, commission := Commission}) when is_number(Amount), Amount > 0 ->
    KRONE = 3,
    Result = Amount * KRONE,
    CommissionResult = Result * Commission,
    {ok, Result - CommissionResult };
map_to_rub(#{type := lari, amount := Amount, commission := Commission}) when is_number(Amount), Amount > 0->
    LARI = 10,
    Result = Amount * LARI,
    CommissionResult = Result * Commission,
    {ok, Result - CommissionResult };
map_to_rub(#{type := Type}) ->
    io:format("Can’t convert to rub, error ~p~n", [Type]),
    {error, badarg}.

```

## Result

```
6> converter: map_to_rub(#{type => usd, amount => 100, commission => 0.01}).
{ok,7474.5}
7> converter: map_to_rub(#{type => peso, amount => 12, commission => 0.02}).
{ok,341.04}
8> converter: map_to_rub(#{type => yene, amount => 30, commission => 0.02}).
Can’t convert to rub, error yene
{error,badarg}
9> converter: map_to_rub(#{type => euro, amount => -15, commission => 0.02}).
Can’t convert to rub, error euro
{error,badarg}

```

# Задание 3.1

```
fac(N) when N == 0 -> 1;
fac(N) when N > 0 -> N*fac(N-1).

tail_fac(N) when N >= 0 -> tail_fac(N, 1).

tail_fac(0, Acc) -> Acc;
tail_fac(N, Acc) when N > 0 -> tail_fac(N-1, N*Acc)
```

# Result

```
12> c(recursion).
{ok,recursion}
13> recursion:tail_fac(16).
20922789888000
14> recursion:tail_fac(0).
1
```

# Задание 3.2 

```
duplicate(List) ->
    duplicate(List, []).

duplicate([], Acc) ->
    lists:reverse(Acc);
duplicate([H|T], Acc) ->
    duplicate(T, [H, H|Acc]).

tail_duplicate(List) ->
    tail_duplicate(List, [], []).

tail_duplicate([], Acc1, Acc2) ->
    lists:reverse(Acc1) ++ Acc2;
tail_duplicate([H|T], Acc1, Acc2) ->
    tail_duplicate(T, [H|Acc1], [H|Acc2]).
```

# Result 
```
1> recursion:duplicate([1, 4, 3]).
[1,1,4,4,3,3]
2> recursion:duplicate([]).
[]
3> recursion:tail_duplicate([1, 4, 3]).
[1,1,4,4,3,3]
4> recursion:tail_duplicate([]).
[]

```