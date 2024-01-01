%%%-------------------------------------------------------------------
%%% @author kolpa
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. дек. 2023 20:17
%%%-------------------------------------------------------------------
-module(keylist_sup).
-author("kolpa").

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/1, stop_child/1]).

%% Callback
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Names) ->
  lists:foreach(
    fun(Name) ->
      supervisor:start_child(?MODULE, [Name])
    end,
    Names
  ).

stop_child(Name) ->
  supervisor:terminate_child(?MODULE, whereis(Name)).

init(_Args) ->
  io:format("Init keylist supervisor ~n"),
  SupFlags = #{
    strategy => one_for_one,
    intensity => 1,
    period => 5
  },

  KeylistNames = [keylist1, keylist2, keylist3],

  ChildSpecs = lists:map(
    fun(Name) ->
      #{
        id => Name,
        start => {keylist, start_link, [Name]},
        restart => permanent
      }
    end,
    KeylistNames
  ),

  {ok, {SupFlags, ChildSpecs}}.