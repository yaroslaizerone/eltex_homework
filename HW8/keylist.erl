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

start_monitor(Name) ->
  Pid = spawn(fun() -> loop(#state{}) end),
  register(Name, Pid),
  MonitorRef = erlang:monitor(process, Pid),
  {ok, Pid, MonitorRef}.

start_link(Name) ->
  Pid = spawn_link(fun() -> loop(#state{}) end),
  register(Name, Pid),
  {ok, Pid}.

loop(State = #state{list = List, counter = Counter}) ->
  receive
    {From, add, Key, Value, Comment} ->
      NewState = State#state{list = [{Key, Value, Comment} | List], counter = Counter + 1},
      From ! {ok, NewState#state.counter},
      loop(NewState);

    {From, is_member, Key} ->
      Result = lists:keymember(Key, 1, List),
      From ! Result,
      loop(State);

    {From, take, Key} ->
      {value, Element, NewList} = lists:keytake(Key, 1, List),
      From ! Element,
      loop(State#state{list = NewList});

    {From, find, Key} ->
      Result = lists:keyfind(Key, 1, List),
      From ! Result,
      loop(State);

    {From, delete, Key} ->
      NewList = lists:keydelete(Key, 1, List),
      From ! NewList,
      loop(State#state{list = NewList})
  end.