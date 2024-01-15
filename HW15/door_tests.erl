%%%-------------------------------------------------------------------
%%% @author kolpa
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. дек. 2023 2:44
%%%-------------------------------------------------------------------
-module(door_tests).
-include_lib("eunit/include/eunit.hrl").

start_link_test() ->
  {ok, Pid} = door:start_link({1, 2, 3, 4}),
  ?debugFmt("~p~n", [Pid]),
%%  {ok, Pid} = door:start_link({1, 2, 3, 4}),
  ?assertEqual(ok, is_pid(Pid)),
%%  ?assertEqual({ok, next}, door:enter(Pid, 1)),
  ?debugFmt("Door process started successfully!", []).

%%enter_test() ->
%%  {ok, Pid} = door:start_link({1, 2, 3}),
%%  ?assertEqual({ok, next}, door:enter(Pid, 1)),
%%  ?assertEqual({ok, next}, door:enter(Pid, 2)),
%%  ?assertEqual({ok, open}, door:enter(Pid, 3)),
%%  ?assertEqual({error, wrong_code}, door:enter(Pid, 4)).
%%
%%print_entered_nums_test() ->
%%  {ok, Pid} = door:start_link({1, 2, 3, 4}),
%%  ?assertEqual({ok, next}, door:enter(Pid, 1)),
%%  ?assertEqual({ok, next}, door:enter(Pid, 2)),
%%  ?assertEqual(ok, door:print_entered_nums(Pid)),
%%  ?debugFmt("Printed entered numbers successfully!", []).
%%
%%change_code_test() ->
%%  {ok, Pid} = door:start_link({1, 2, 3, 4}),
%%  ?assertEqual({ok, next}, door:enter(Pid, 1)),
%%  ?assertEqual({ok, next}, door:enter(Pid, 2)),
%%  ?assertEqual(ok, door:change_code(Pid, {5, 6, 7, 8})),
%%  ?debugFmt("Changed code successfully!", []).
