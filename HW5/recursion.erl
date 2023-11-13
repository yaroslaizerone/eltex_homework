%%%-------------------------------------------------------------------
%%% @author kolpa
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. окт. 2023 10:20
%%%-------------------------------------------------------------------
-module(recursion).
-author("kolpa").

%% API
-export([fac/1, tail_fac/1, duplicate/1, tail_duplicate/1]).

fac(N) when N == 0 -> 1;
fac(N) when N > 0 -> N*fac(N-1).

tail_fac(N) when N >= 0 -> tail_fac(N, 1).

tail_fac(0, Acc) -> Acc;
tail_fac(N, Acc) when N > 0 -> tail_fac(N-1, N*Acc).

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

