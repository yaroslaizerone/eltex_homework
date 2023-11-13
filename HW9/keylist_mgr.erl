%%%-------------------------------------------------------------------
%%% @author kolpa
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%% Module API keylist_mgr.erl
%%% @end
%%% Created : 11. нояб. 2023 17:27
%%%-------------------------------------------------------------------
-module(keylist_mgr).
-author("kolpa").

%% API
-export([start/0, init/0, loop/1, start_child/1, stop_child/1, stop/0, get_names/0]).

-record(state, {children=[], permanent=[]}).

%% API функции

%% @doc Запуск keylist_mgr
start() ->
  Pid = spawn_link(keylist_mgr, init, []),
  {ok, Pid}.

%% @doc Запуск дочернего процесса
start_child(Params) ->
  keylist_mgr ! {self(), start_child, Params}.

%% @doc Остановка дочернего процесса
stop_child(Name) ->
  keylist_mgr ! {self(), stop_child, Name}.

%% @doc Остановка keylist_mgr
stop() ->
  keylist_mgr ! stop.

%% @doc Получение списка имен дочерних процессов
get_names() ->
  keylist_mgr ! {self(), get_names}.

%% @doc Инициализация keylist_mgr
init() ->
  process_flag(trap_exit, true),
  register(keylist_mgr, self()),
  loop(#state{}).

%% @doc Цикл обработки сообщений
loop(#state{children=Children, permanent=Permanent}=State) ->
  process_flag(trap_exit, true),
  receive
    {From, start_child, Params} ->
      Name = proplists:get_value(name, Params),
      Restart = proplists:get_value(restart, Params, temporary),
      case lists:keyfind(Name, 1, Children) of
        false ->
          {ok, Pid} = keylist:start_link(Name),
          NewChildren = [{Name, Pid} | Children],
          case Restart of
            permanent ->
              NewPermanent = [{Name, Pid} | Permanent],
              loop(State#state{children=NewChildren, permanent=NewPermanent});
            temporary ->
              loop(State#state{children=NewChildren, permanent=Permanent})
          end;
        {_, Pid} ->
          From ! {error, already_started},
          loop(State)
      end;

    {From, stop_child, Name} ->
      case lists:keyfind(Name, 1, Children) of
        {_, Pid} ->
          keylist:stop(Name),
          NewChildren = lists:keydelete(Name, 1, Children),
          From ! ok,
          loop(State#state{children=NewChildren});
        false ->
          From ! {error, not_found},
          loop(State)
      end;

    stop ->
      exit(normal);

    {From, get_keys} ->
      Names = [Name || {Name, _} <- Children],
      From ! Names,
      loop(State);

    {'EXIT', Pid, Reason} ->
      case lists:keyfind(Pid, 2, Children) of
        {Name, _} ->
          case proplists:get_value(restart, proplists:get_value(Name, Permanent, temporary)) of
            permanent ->
              {ok, NewPid} = keylist:start_link(Name),
              NewChildren = lists:keyreplace(Name, 1, Children, {Name, NewPid}),
              loop(State#state{children=NewChildren});
            temporary ->
              error_logger:error_msg("Process ~p exited with reason ~p~n", [Pid, Reason]),
              loop(State#state{children=lists:keydelete(Name, 1, Children)})
          end;
        false ->
          error_logger:error_msg("Process ~p exited with reason ~p~n", [Pid, Reason]),
          loop(State#state{children=lists:keydelete(Pid, 2, Children)})
      end
  end.