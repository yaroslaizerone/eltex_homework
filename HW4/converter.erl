%%%-------------------------------------------------------------------
%%% @author kolpa
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. окт. 2023 18:30
%%%-------------------------------------------------------------------
-module(converter).
-author("kolpa").
%% API
-export([to_rub/1, to_rub2/1, to_rub3/1, rec_to_rub/1, map_to_rub/1]).
-record(conv_info, {type, amount, commission}).

to_rub({usd, Amount}) when is_integer(Amount), Amount > 0 ->
  USD = 75.5,
  io:format("Convert ~p to rub, amount ~p~n", [usd,Amount]),
  {ok, Amount * USD};
to_rub({euro, Amount}) when is_integer(Amount), Amount > 0->
  EURO = 80,
  io:format("Convert ~p to rub, amount ~p~n", [euro,Amount]),
  {ok, Amount * EURO};
to_rub({peso, Amount}) when is_integer(Amount), Amount > 0->
  PESO = 29,
  io:format("Convert ~p to rub, amount ~p~n", [peso,Amount]),
  {ok, Amount * PESO};
to_rub({krone, Amount}) when is_integer(Amount), Amount > 0->
  KRONE = 3,
  io:format("Convert ~p to rub, amount ~p~n", [krone,Amount]),
  {ok, Amount * KRONE};
to_rub({lari, Amount}) when is_integer(Amount), Amount > 0->
  LARI = 10,
  io:format("Convert ~p to rub, amount ~p~n", [lari,Amount]),
  {ok, Amount * LARI};
to_rub({Currency, _Amount}) ->
  io:format("Can’t convert to rub, error ~p~n", [Currency]),
  {error, badarg}.

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

%% Mult = fun(A,B) -> A * B end.
%% Result = Mult(3,4).
%% Result.
%% 12.
