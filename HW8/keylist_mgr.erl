%%%-------------------------------------------------------------------
%%% @author kolpa
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. нояб. 2023 17:27
%%%-------------------------------------------------------------------
-module(keylist_mgr).
-author("kolpa").

%% API
-export([loop/1, start/0, init/0]).

-record(state, {children=[], ref = undefined}).

start() ->
  Pid = spawn_link(keylist_mgr, init, []),
  {ok, Pid}.

init() ->
  process_flag(trap_exit, true),
  register(keylist_mgr, self()),
  loop(#state{}).

loop(#state{children=Children}=State) ->
  process_flag(trap_exit, true),
  receive
    {From, start_child, Name} ->
      case lists:keyfind(Name, 1, Children) of
        false ->
          {ok, Pid} = keylist:start_link(Name),
          NewChildren = [{Name, Pid} | Children],
          From ! {ok, Pid},
          loop(State#state{children=NewChildren});
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

    {From, get_names} ->
      Names = [Name || {Name, _} <- Children],
      From ! Names,
      loop(State);

    {'EXIT', Pid, Reason} ->
      NewChildren = lists:keydelete(Pid, 2, Children),
      error_logger:error_msg("Process ~p exited with reason ~p~n", [Pid, Reason]),
      loop(State#state{children=NewChildren})
  end.