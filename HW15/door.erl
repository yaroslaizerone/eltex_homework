%%%-------------------------------------------------------------------
%%% @author kolpa
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. дек. 2023 12:45
%%%-------------------------------------------------------------------
-module(door).
-author("kolpa").

-behaviour(gen_statem).

%% API
-export([start_link/1, enter/2, print_entered_nums/1, change_code/2]).

% Callbacks
-export([init/1, callback_mode/0, locked/3, open/3, suspended/3, terminate/3]).

-record(door_data, {code, entered = [], attempts = 0}).

-define(SERVER, ?MODULE).

%% API
% Создание процесса
-spec start_link(InitCode :: tuple()) -> {ok, Pid :: pid()}.
start_link(InitCode) ->
  gen_statem:start_link(?MODULE, [InitCode], []).

% Ввод чисел
-spec enter(Pid :: pid(), Num :: integer()) -> ok.
enter(Pid, Num) ->
  gen_statem:call(Pid, {enter, Num}).

% Вывод ранее введённых чисел до достижения open
-spec print_entered_nums(pid()) -> ok.
print_entered_nums(Pid) ->
  gen_statem:cast(Pid, print_entered_nums).

% Изменение кода
-spec change_code(Pid :: pid(), NewCode :: tuple()) -> ok.
change_code(Pid, NewCode) ->
  gen_statem:call(Pid, {change_code, NewCode}).

% Callbacks
init([InitCode]) ->
  io:format("Init callback was called ~n"),
  process_flag(trap_exit, true),
  {ok, locked, #door_data{code = InitCode, attempts = 0}}.

callback_mode() ->
  state_functions.

% Locked Handler
locked({call, From}, {enter, Num}, #door_data{code = Code, entered = Entered, attempts = Attempts} = DoorData) ->
  io:format("Entered code ~p, current data ~p~n", [Num, DoorData]),
  case length([Num | Entered]) == length(Code) of
    true ->
      EnteredCode = lists:reverse([Num | Entered]),
      case Code == EnteredCode of
        % Код верный, открываем дверь
        true ->
          {next_state, open, DoorData#door_data{entered = []}, [{reply, From, {ok, open}}, {state_timeout, 10000, open_door_timeout}]};
        % Попытка неверная прибавляем попытку
        false ->
          NewAttempts = Attempts + 1,
          % 3 Попытки
          case NewAttempts >= 3 of
            true ->
              {next_state, suspended, DoorData#door_data{entered = [], attempts = 0}, [{reply, From,{error, timeout}}, {state_timeout, 10000, door_timeout}]};
            false ->
              {keep_state, DoorData#door_data{entered = [], attempts = NewAttempts}, [{reply, From, {error, wrong_code}}]}
          end
      end;
    false ->
      {keep_state, DoorData#door_data{entered = [Num | Entered]}, [{reply, From, {ok, next}}]}
  end;
locked({call, From}, {change_code, _NewCode}, DoorData) ->
  io:format("Cannot change code when the door is locked.~n"),
  {keep_state, DoorData, [{reply, From, {error, cannot_change_code}}]};
locked(cast, print_entered_nums, #door_data{entered = Entered} = _DoorData) ->
  io:format("Entered nums ~p~n", [lists:reverse(Entered)]),
  keep_state_and_data;
locked(info, {link, Pid}, _Doordata) ->
  link(Pid),
  io:format("Received link, created link with ~p", [Pid]),
  keep_state_and_data;
locked(info, Msg, DoorData) ->
  io:format("Received unhandled msg ~p in State num ~p~n", [Msg, DoorData]),
  keep_state_and_data;
locked(info, {'EXIT', Pid, Reason}, DoorData) ->
  io:format("Received EXIT msg from ~p, reason ~p in State num ~p~n", [Pid, Reason, DoorData]),
  keep_state_and_data.

% Open Handler
open({call, From}, {enter, Num}, _DoorData) ->
  io:format("The door is open, handling Num ~p~n", [Num]),
  {keep_state_and_data, [{reply, From, {error, already_open}}]};
open(cast, print_entered_nums, #door_data{entered = Entered} = _DoorData) ->
  io:format("Entered nums while door is open: ~p~n", [lists:reverse(Entered)]),
  keep_state_and_data;
open({call, From}, {change_code, NewCode}, #door_data{code = _Code} = DoorData) ->
  io:format("Changing code to ~p~n", [NewCode]),
  {keep_state, DoorData#door_data{code = NewCode}, [{reply, From, locked}]};
open(info, {'EXIT', Pid, Reason}, _DoorData) ->
  io:format("Received EXIT msg from ~p, reason ~p", [Pid, Reason]),
  keep_state_and_data;
open(state_timeout, open_door_timeout, DoorData) ->
  {next_state, locked, DoorData#door_data{attempts = 0}}.

% Suspended Handler
suspended({call, From}, {enter, _Num}, _DoorData) ->
  io:format("The door is suspended, rejecting attempts to enter~n"),
  {keep_state_and_data, [{reply, From, {error, suspended}}]};
suspended({call, From}, {change_code, _NewCode}, DoorData) ->
  io:format("Cannot change code when the door is suspended.~n"),
  {keep_state, DoorData, [{reply, From, {error, cannot_change_code}}]};
suspended(cast, _, _DoorData) ->
  io:format("Ignoring cast messages in the suspended state~n"),
  keep_state_and_data;
suspended(state_timeout, door_timeout, DoorData) ->
  {next_state, locked, DoorData#door_data{attempts = 0}}.

% Terminate Handler
terminate(Reason, State, DoorData) ->
  io:format("Terminating with reason ~p, in state ~p, with data ~p~n", [Reason, State, DoorData]),
  ok.