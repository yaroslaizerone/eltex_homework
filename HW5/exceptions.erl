%%%-------------------------------------------------------------------
%%% @author kolpa
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. окт. 2023 17:10
%%%-------------------------------------------------------------------
-module(exceptions).
-export([catch_all/1]).

catch_all(Action) when is_function(Action, 0) ->
  try Action() of
    Result -> {ok, Result}
  catch
    throw:Reason ->
      io:format("Action ~p failed, reason ~p ~n", [Action, Reason]),
      throw(Reason);
    error:Reason ->
      io:format("Action ~p failed, reason ~p ~n", [Action, Reason]),
      error(Reason);
    exit:Reason ->
      io:format("Action ~p failed, reason ~p ~n", [Action, Reason]),
      exit(Reason);
    _:_ ->
      io:format("We covered all cases so this line will never be printed~n"),
      "Never will happen"
  end.