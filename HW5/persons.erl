%%%-------------------------------------------------------------------
%%% @author kolpa
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. окт. 2023 17:00
%%%-------------------------------------------------------------------
-module(persons).
-author("kolpa").

%% API
-include("person.hrl").
-export([filter/2, all/2, any/2, update/2, get_average_age/1]).

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