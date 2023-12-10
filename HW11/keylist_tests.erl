%%%-------------------------------------------------------------------
%%% @author kolpa
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. нояб. 2023 13:32
%%%-------------------------------------------------------------------
-module(keylist_tests).
-author("kolpa").

-include_lib("eunit/include/eunit.hrl").

start_link_test() ->
  {ok, Pid} = keylist:start_link(test),
  ?assert(is_pid(Pid)),
  ?assertEqual(Pid, whereis(test)),
  keylist:stop(test),
  ?assertNot(is_process_alive(Pid)),
  ?debugFmt("Процессы запускаются, успешно!", []).

add_test() ->
  {ok, Pid} = keylist:start_link(test),
  ?assert(is_pid(Pid)),
  Key = key1,
  Value = value1,
  Comment = "Comment1",
  List = keylist:add(test, Key, Value, Comment),
  LengthList = length(List),
  ?assertEqual(1, LengthList),
  ?debugFmt("Записи добавляются, успешно!", []).


is_member_test() ->
  {ok, Pid} = keylist:start_link(test1),
  ?assert(is_pid(Pid)),
  Key = key1,
  Value = value1,
  Comment = "Comment1",
  keylist:add(test1, Key, Value, Comment),
  ?assertEqual(true, keylist:is_member(test1, Key)),
  ?debugFmt("Проверка на ключ, успешно!", []).

take_test() ->
  {ok, Pid} = keylist:start_link(test2),
  ?assert(is_pid(Pid)),
  Key = key1,
  Value = value1,
  Comment = "Comment1",
  List = keylist:add(test2, Key, Value, Comment),
  ?assertEqual(List, [keylist:take(test2, Key)]),
  ?debugFmt("Значения получаются, успешно!", []).

find_test() ->
  {ok, Pid} = keylist:start_link(test3),
  ?assert(is_pid(Pid)),
  Key = key1,
  Value = value1,
  Comment = "Comment1",
  keylist:add(test3, Key, Value, Comment),
  ?assertEqual({Key, Value, Comment}, keylist:find(test3, Key)),
  ?debugFmt("Записи ищются, успешно!", []).

delete_test() ->
  {ok, Pid} = keylist:start_link(test4),
  ?assert(is_pid(Pid)),
  Key = key1,
  Value = value1,
  Comment = "Comment1",
  keylist:add(test4, Key, Value, Comment),
  keylist:delete(test4, Key),
  ?assertEqual(false, keylist:is_member(test4, Key)),
  ?debugFmt("Записи удаляются успешно!", []).

stop_test() ->
  {ok, Pid} = keylist:start_link(test5),
  ?assert(is_pid(Pid)),
  keylist:stop(test5),
  ?assertEqual(undefined, whereis(test5)),
  ?debugFmt("Процессы останавливаются, успешно!", []).
