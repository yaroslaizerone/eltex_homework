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

%%start_link_test() ->
%%  ?assertEqual(ok, door:start_link({1, 2, 3})),
%%  ?assertMatch({ok, _Pid}, door:start_link({4, 5, 6})),
%%  ?assertMatch({ok, Pid}, door:start_link({7, 8, 9})),
%%  ?assert(is_pid(Pid)),
%%  ?debugFmt("Door process started successfully with code {7, 8, 9}!~n", []).
%%
%%init_test() ->
%%  ?assertMatch({ok, _Pid}, door:start_link({1, 2, 3})),
%%  ?assertMatch({ok, _Pid}, door:start_link({4, 5, 6})),
%%  ?assertMatch({ok, _Pid}, door:start_link({7, 8, 9})),
%%  ?debugFmt("Init callback was called for each start_link!~n", []).

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
