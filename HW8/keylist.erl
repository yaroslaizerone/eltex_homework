%%%-------------------------------------------------------------------
%%% @author kolpa
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. нояб. 2023 13:08
%%%-------------------------------------------------------------------
-module(keylist).
-author("kolpa").

-export([loop/1, start_monitor/1, start_link/1]).

-record(state, {list = [], counter = 0}).

loop(#state{list = List, counter = Counter} = State) ->
  receive
    {From, add, Key, Value, Comment} ->
      NewList = [{Key, Value, Comment} | List],
      NewCounter = Counter + 1,
      NewState = State#state{list = NewList, counter = NewCounter},
      From ! {ok, NewCounter},
      loop(NewState);

    {From, is_member, Key} ->
      Result = lists:keymember(Key, 1, List),
      From ! Result,
      loop(State);

    {From, take, Key} ->
      {value, Element} = lists:keytake(Key, 1, List),
      NewList = tl(Element),
      From ! Element,
      loop(State#state{list = NewList});

    {From, find, Key} ->
      {value, Element} = lists:keyfind(Key, 1, List),
      From ! Element,
      loop(State);

    {From, delete, Key} ->
      NewList = lists:keydelete(Key, 1, List),
      From ! ok,
      loop(State#state{list = NewList})
  end.

start_monitor(Name) ->
  Pid = spawn_link(fun() -> loop(#state{}) end),
  MonitorRef = erlang:monitor(process, Pid),
  {ok, Pid, MonitorRef}.

start_link(Name) ->
  Pid = spawn_link(fun() -> loop(#state{}) end),
  true = register(Name, Pid),
  {ok, Pid}.