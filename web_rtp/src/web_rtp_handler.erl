-module(web_rtp_handler).
-behavior(cowboy_handler).

%% API
-export([init/2]).

%% Initializing the web handler
init(Req, State) ->
  %% Defining the request method
  Method = cowboy_req:method(Req),
  %% Does the request have a body
  HasBody = cowboy_req:has_body(Req),
  case {Method,HasBody} of
    {<<"POST">>, true} ->
      handle_post(Req);
    {<<"DELETE">>, false} ->
      handle_delete(Req);
    {<<"GET">>, false} ->
      handle_get(Req);
    {_,_} ->
      ok
    end,
  {ok, Req, State}.

%% Handling the request
handle_post(Req) ->
  {ok, Body, Req1} = cowboy_req:read_body(Req),
  io:format("~p~n", [Req1]),
  % Parse JSON
  Json = jsone:decode(Body),
  case Json of
    #{<<"name">> := Name, <<"num">> := Num} ->
      io:format("~p~n", [Json]),
      web_rtp_db:insert_abonent(Num, Name),
      cowboy_req:reply(201, #{<<"content-type">> => <<"text/plain">>}, <<"Successfully! Insert into Database.">>, Req);
    _ ->
      io:format("~p~n", [Json]),
      cowboy_req:reply(400, #{}, <<"Invalid JSON format">>, Req)
  end.

handle_delete(Req) ->
  Path = binary:split(cowboy_req:path(Req), <<"/">>, [global, trim_all]),
  io:format("Path: ~p~n", [Path]),
  case Path of
    [<<"abonent">>, Num ] ->
      IntNum = binary_to_integer(Num),
      % Проверяем наличие записи
      case web_rtp_db:read_abonent(IntNum) of
        {abonents, _, _} ->
          % Если запись существует, удаляем ее
          web_rtp_db:delete_abonent(IntNum),
          cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, "Accuses delete.", Req);
        not_found ->
          % Если запись не найдена, отправляем соответствующий ответ
          cowboy_req:reply(404, #{}, <<"Abonent not found">>, Req)
      end;
    _ ->
      cowboy_req:reply(400, #{}, <<"Invalid abonent number">>, Req)
  end,
  ok.

handle_get(Req) ->
  Path = binary:split(cowboy_req:path(Req), <<"/">>, [global, trim_all]),
  case Path of
    [<<"abonents">>] ->
      % Читаем всех абонентов из базы данных
      Abonents = web_rtp_db:read_all_abonent(),
      % Для каждого абонента выполняем звонок
      lists:foreach(fun({_, Num, _Name}) -> web_rtp_sip:call_abonent(Num) end, Abonents),
      % Отправляем ответ
      AbonentNames = lists:map(fun({_Table, _Num, Name}) -> Name end, Abonents),
      cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, lists:flatten(io_lib:format("Call Abonents: ~p", [AbonentNames])), Req);
    [<<"abonent">>, Num] ->
      IntNum = binary_to_integer(Num),
      % Проверяем наличие записи
      case web_rtp_db:read_abonent(IntNum) of
        {abonents, _, _} ->
          % Если запись существует, инициализируем вызов
          web_rtp_sip:call_abonent(IntNum),
          cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, "Call Abonent.", Req);
        not_found ->
          % Если запись не найдена, отправляем соответствующий ответ
          cowboy_req:reply(404, #{}, <<"Abonent not found">>, Req)
      end
  end,
  ok.