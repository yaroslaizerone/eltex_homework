%%%-------------------------------------------------------------------
%%% @author kolpa
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. дек. 2023 20:17
%%%-------------------------------------------------------------------
-module(main_sup).
-author("kolpa").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Callbacks
-export([init/1, terminate/2]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  io:format("Init main supervisor ~p~n", [?MODULE]),

  SupFlags = #{
    strategy => rest_for_one,
    intensity => 1,
    period => 5
  },
  ChildSpecs = [
    #{
      id => keylist_mgr,
      start => {keylist_mgr, start, []},
      restart => permanent
    },

    #{
      id => keylist_sup,
      start => {keylist_sup, start_link, []},
      restart => permanent
    }
  ],

  {ok, {SupFlags, ChildSpecs}}.

terminate(_Reason, _State) ->
  ok = supervisor:terminate_child(main_sup, keylist_mgr),
  ok.