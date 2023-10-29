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
  io:format("Ошибка: Неизвестная валюта ~p~n", [Currency]),
  {error, badarg}.

to_rub2({Type, Amount} = Arg) when is_integer(Amount), Amount > 0->
    Result =
      case Arg of
        {usd, Amount} ->
          io:format("Convert ~p to rub, amount ~p~n, [usd, amount]"),
          USD = 75.5,
          {ok, USD*Amount};
        {euro, Amount} ->
          io:format("Convert ~p to rub, amount ~p~n, [euro, amount]"),
          EURO = 80,
          {ok, EURO*Amount};
        {peso, Amount} ->
          io:format("Convert ~p to rub, amount ~p~n, [peso, amount]"),
          PESO = 29,
          {ok, PESO*Amount};
        {krone, Amount} ->
          io:format("Convert ~p to rub, amount ~p~n, [krone, amount]"),
          KRONE = 3,
          {ok, KRONE*Amount};
        {lari, Amount} ->
          io:format("Convert ~p to rub, amount ~p~n, [lari, amount]"),
          LARI = 10,
          {ok, LARI*Amount};
        _Error ->
          io:format("Can’t convert to rub, error ~p~n", [_Error]),
          {error, badarg}
      end,
    io:format("Converted ~p to rub, amount ~p, Result ~p~n”, [Type, Amount, Result]"),
  Result.

to_rub3(Arg) when is_integer(Arg), Arg > 0->
  case Arg of
    {usd, Amount} ->
      io:format("Convert ~p to rub, amount ~p~n, [usd, amount]"),
      USD = 75.5,
      {ok, USD*Amount};
    {euro, Amount} ->
      io:format("Convert ~p to rub, amount ~p~n, [euro, amount]"),
      EURO = 80,
      {ok, EURO*Amount};
    {peso, Amount} ->
      io:format("Convert ~p to rub, amount ~p~n, [peso, amount]"),
      PESO = 29,
      {ok, PESO*Amount};
    {krone, Amount} ->
      io:format("Convert ~p to rub, amount ~p~n, [krone, amount]"),
      KRONE = 3,
      {ok, KRONE*Amount};
    {lari, Amount} ->
      io:format("Convert ~p to rub, amount ~p~n, [lari, amount]"),
      LARI = 10,
      {ok, LARI*Amount};
    _Error ->
      io:format("Can’t convert to rub, error ~p~n", [_Error]),
      {error, badarg}
  end.

rec_to_rub(#conv_info{type = usd, amount = Amount, commission = Commission} = ValutParametrs) when is_integer(Amount), Amount > 0->
  case ValutParametrs of
    {usd, Amount, Commission} ->
      USD = 75.5,
      ConvAmount = Amount *  USD,
      CommissionResult = ConvAmount * Commission,
      {ok, ConvAmount - CommissionResult };
    {euro, Amount, Commission} ->
      EURO = 80,
      ConvAmount = Amount *  EURO,
      CommissionResult = ConvAmount * Commission,
      {ok, ConvAmount - CommissionResult };
    {peso, Amount, Commission} ->
      PESO = 29,
      ConvAmount = Amount *  PESO,
      CommissionResult = ConvAmount * Commission,
      {ok, ConvAmount - CommissionResult };
    {krone, Amount, Commission} ->
      KRONE = 3,
      ConvAmount = Amount *  KRONE,
      CommissionResult = ConvAmount * Commission,
      {ok, ConvAmount - CommissionResult };
    {lari, Amount, Commission} ->
      LARI = 10,
      ConvAmount = Amount *  LARI,
      CommissionResult = ConvAmount * Commission,
      {ok, ConvAmount - CommissionResult };
    _Error->
      io:format("Can’t convert to rub, error ~p~n", [_Error]),
      {error, badarg}
  end.

map_to_rub(#{type => Type, amount => Amount, commission => Commission} = Arguments) ->
    case Arguments of
      {usd, Amount, Commission} ->
        USD = 75.5,
        Result = Amount * USD,
        CommissionResult = Result * Commission,
        {ok, Result - CommissionResult };
      {euro, Amount, Commission} ->
        EURO = 80,
        Result = Amount * EURO,
        CommissionResult = Result * Commission,
        {ok, Result - CommissionResult };
      {peso, Amount, Commission} ->
        PESO = 29,
        Result = Amount * PESO,
        CommissionResult = Result * Commission,
        {ok, Result - CommissionResult };
      {krone, Amount, Commission} ->
        KRONE = 3,
        Result = Amount * KRONE,
        CommissionResult = Result * Commission,
        {ok, Result - CommissionResult };
      {lari, Amount} ->
        LARI = 10,
        Result = Amount * LARI,
        CommissionResult = Result * Commission,
        {ok, Result - CommissionResult };
      _Error ->
        io:format("Can’t convert to rub, error ~p~n", [_Error]),
        {error, badarg}
    end,
  io:format("Converted ~p to rub, amount ~p, Result ~p~n”, [Type, Amount, Result]").

%% Mult = fun(A,B) -> A * B end.
%% Result = Mult(3,4).
%% Result.
%% 12.
ToRub = fun
  ({usd, Amount}) when is_integer(Amount), Amount > 0 ->
    USD = 75.5,
    io:format("Convert ~p to rub, amount ~p~n", [usd,Amount]),
    {ok, Amount * USD};
  ({euro, Amount}) when is_integer(Amount), Amount > 0->
    EURO = 80,
    io:format("Convert ~p to rub, amount ~p~n", [euro,Amount]),
    {ok, Amount * EURO};
  ({peso, Amount}) when is_integer(Amount), Amount > 0->
    PESO = 29,
    io:format("Convert ~p to rub, amount ~p~n", [peso,Amount]),
    {ok, Amount * PESO};
  ({krone, Amount}) when is_integer(Amount), Amount > 0->
    KRONE = 3,
    io:format("Convert ~p to rub, amount ~p~n", [krone,Amount]),
    {ok, Amount * KRONE};
  ({lari, Amount}) when is_integer(Amount), Amount > 0->
    LARI = 10,
    io:format("Convert ~p to rub, amount ~p~n", [lari,Amount]),
    {ok, Amount * LARI};
  ({Currency, _Amount}) ->
    io:format("Ошибка: Неизвестная валюта ~p~n", [Currency]),
    {error, badarg}
end.