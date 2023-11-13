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
-export([to_rub/1]).

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

