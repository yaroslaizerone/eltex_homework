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

%% Record definition
-record(state, {children=[], permanent=[]}).

%% API functions

%% @doc Start keylist_mgr
-spec start() -> {ok, Pid :: pid()} | {error, Reason :: term()}.
start() ->
  Pid = spawn_link(?MODULE, init, []),
  {ok, Pid}.

%% @doc Start a child process
-spec start_child(Params :: #{name => atom(), restart => permanent | temporary}) -> ok.
start_child(Params) ->
  ?MODULE ! {self(), start_child, Params}.

%% @doc Stop a child process
-spec stop_child(Name :: atom()) -> ok.
stop_child(Name) ->
  ?MODULE ! {self(), stop_child, Name}.

%% @doc Stop keylist_mgr
-spec stop() -> ok.
stop() ->
  ?MODULE ! stop.

%% @doc Get the list of child process names
-spec get_names() -> ok.
get_names() ->
  ?MODULE ! {self(), get_names}.

%% @doc Initialize keylist_mgr
-spec init() -> ok.
init() ->
  process_flag(trap_exit, true),
  register(?MODULE, self()),
  loop(#state{}).

%% @doc Message handling loop
-spec loop(State :: #state{}) -> ok.
loop(#state{children=Children, permanent=Permanent}=State) ->
  receive
    {From, start_child, #{name := Name, restart := Restart}} ->
      case proplists:is_defined(Name,Children) of
        false ->
          Pid = keylist:start_link(Name),
          NewChildren =
          case Restart of
            permanent ->
              [{Name, Pid} | Children];
            temporary ->
              Children;
            _ ->
              From ! {error, type_error}
          end,
          NewPermanent = [Pid | Permanent],
          io:format("В Сhildren записалось: ~p~n", [NewChildren]),
          From ! {add, Name, Pid},
          loop(State#state{children=NewChildren, permanent=NewPermanent});
        true ->
          From ! {error, already_started}
      end;

    {From, stop_child, Name} ->
      case proplists:is_defined(Name,Children) of
        true ->
          keylist:stop(Name),
          NewChildren = lists:keydelete(Name, 1, Children),
          NewPermanent = lists:keydelete(whereis(Name), 1, Permanent),
          From ! {stop, Name},
          loop(State#state{children=NewChildren, permanent=NewPermanent});
        false ->
          From ! {error, not_found},
          loop(State)
      end;

    stop ->
      lists:foreach(fun({_, Pid}) -> exit(Pid, kill) end, Children),
      ok;

    {From, get_names} ->
      From ! [Name || {Name, _} <- Children],
      loop(State);

    {'EXIT', Pid, Reason} ->
      case lists:keyfind(Pid, 2, Children) of
        {Name, _} ->
          case proplists:get_value(restart, proplists:get_value(Name, Permanent, temporary)) of
            permanent ->
              {ok, NewPid} = keylist:start_link(Name),
              NewChildren = lists:keyreplace(Name, 1, Children, {Name, NewPid}),
              NewPermanent = lists:keyreplace(Pid, 1, Permanent, NewPid),
              loop(State#state{children=NewChildren, permanent = NewPermanent});
            temporary ->
              error_logger:error_msg("Process ~p exited with reason ~p~n", [Pid, Reason]),
              loop(State#state{children=lists:keydelete(Name, 1, Children), permanent = lists:keydelete(Pid,1,Permanent)});
            false ->
              not_found
          end;
        false ->
          ok
      end
  end.