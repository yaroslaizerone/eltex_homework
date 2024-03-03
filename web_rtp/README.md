web_rtp
=====

An OTP application

Build
-----

    $ rebar3 compile


# WEB RTP

Курсовой проект на курсе Elrang/Elixir Eltex

В результате выполнения курсового проекта были выполнены следующие задачи:
- Хранение данных в Mnesia;
- Реализация обработчика HTTP-запросов с использованием веб-сервера Cowboy передоставляющая REST API для работы с Mnesia;
```
GET - http://localhost:8080/abonent/{number_abonent} Вызов абонента и получение данных из бд
GET - http://localhost:8080/abonents Вызов всех абонентов и получение данных из бд
POST - http://localhost:8080/abonent JSON: {"num": {number_abonent}, "name": "{name_abonent}"} Добавление абонета в бд
DELETE - http://localhost:8080/abonent/{number_abonent} Удаление данных абонента из бд
```
- Совершенее вызова абоненту, подключённого к виртуальной АТС ECSS10 с применением протоколов SIP и SDP, реализованной с помощью библиотеки nk_sip;
- Реализайция проекта на языке Erlang с использованием Rebar3;
- Иницированние голосового сообщения;
- Передача голосового пакета в клиент Zoiper5;
- Реализация и сборка проекта в Docker контейнер.

# Демонстрация работы проекта
![2024-03-03 14-32-21.gif](readme_media%2F2024-03-03%2014-32-21.gif)

![2024-03-03 14-32-21.mp4](readme_media%2F2024-03-03%2014-32-21.mp4)

# Dockerfile

Каждый компонент выполняет определенную функцию в контексте сборки и
запуска приложения. Например, этап сборки использует образ Erlang
для создания релиза проекта с использованием Rebar3,
в то время как финальный этап образа устанавливает необходимые библиотеки
для работы с медиафайлами и работой с SIP пакетами
и запускает приложение с указанной командой.
Кроме того, открытие портов 8080 и 8443 позволяет 
приложению взаимодействовать с внешней средой.

```
# Stage 0: App image
# Use the Erlang image as the builder stage
FROM erlang:21-alpine AS builder

# Install necessary build tools
RUN apk add --no-cache git build-base

# Create a directory for building
RUN mkdir /buildroot
# Set the working directory
WORKDIR /buildroot

# Copy project files into the container
COPY . /buildroot/web_rtp
# Change working directory to the project directory
WORKDIR /buildroot/web_rtp

# Build the release using Rebar3
RUN rebar3 as prod release

# Stage 1: Final image
FROM alpine:3.13

# Install some libs
RUN apk add --no-cache git \
    make gcc musl-dev\
    openssl ortp-dev\
    ncurses-libs bctoolbox-dev\
    libstdc++ \
    ffmpeg

COPY --from=builder /buildroot/web_rtp/voice /web_rtp/voice

COPY --from=builder /buildroot/web_rtp/c_src /web_rtp/c_src

# Copy the built release from the builder stage
COPY --from=builder /buildroot/web_rtp/_build/prod/rel/web_rtp /web_rtp

# Start Make for comp
RUN make -C /web_rtp/c_src

# Expose ports
EXPOSE 8080
EXPOSE 8443

# Run the application
CMD ["/web_rtp/bin/web_rtp", "foreground"]
```
# Api DB


Добавление данных в таблицу abonents
```
handle_cast({insert, Num, Name},  State) ->
  Rec = #abonents{num = Num, name = Name},
  Fun =
    fun() ->
      case mnesia:read(abonents, Num) of
        [] ->
          mnesia:write(Rec);
        [_] ->
          {error, already_exists}
      end
    end,
  Result = mnesia:transaction(Fun),
  io:format("Record was insert with code: ~p~n", [Result]),
  {noreply, State};
```
Удаление данных из таблицы abonents
```
handle_cast({insert, Num, Name},  State) ->
  Rec = #abonents{num = Num, name = Name},
  Fun =
    fun() ->
      case mnesia:read(abonents, Num) of
        [] ->
          mnesia:write(Rec);
        [_] ->
          {error, already_exists}
      end
    end,
  Result = mnesia:transaction(Fun),
  io:format("Record was insert with code: ~p~n", [Result]),
  {noreply, State};
handle_cast({delete, Num},  State) ->
  Fun =
    fun() ->
      case mnesia:read(abonents, Num) of
        [_] ->
          mnesia:delete({abonents, Num});
        [] ->
          {error, wrong_number}
      end
    end,
  Result = mnesia:transaction(Fun),
  io:format("Record was deleted with code: ~p~n", [Result]),
  {noreply, State};
```
Чтение записи из таблицы abonents с использованием
```
handle_call({read, Num}, _From, State) ->
  Fun =
    fun() ->
      mnesia:read(abonents, Num)
    end,
  {atomic, Result} = mnesia:transaction(Fun),

  case Result of
    [{abonents, Num, Name}] ->
      {reply, {abonents, Num, Name}, State};
    [] ->
      {reply, not_found, State}
  end;
```
Чтение всех записей из таблицы abonents
```
handle_call({read_all}, _From, State) ->
  Fun =
    fun() ->
      mnesia:select(abonents, [{'_', [], ['$_']}])
    end,
  {atomic, Result} = mnesia:transaction(Fun),
  {reply, Result, State}.
```


# App API

Для реализации обработки запроса был реализован web_rtp_handler.erl для обработки HTTP-запросов:
```
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
```
Обрабатывая запрос происходит распределенеи в зависимости от метода и тела запроса:
- GET/abonent/{number_abonent} - Запрос для вызова и получение данных о абоненте;
- GET/abonents - Запрос для вызова всех абонетов и получение данных о результате выхова и данных пользователей;
```
handle_get(Req) ->
  Path = binary:split(cowboy_req:path(Req), <<"/">>, [global, trim_all]),
  case Path of
    [<<"abonents">>] ->
      % Read all abonents from the database
      Abonents = web_rtp_db:read_all_abonent(),
      % For each abonent, make a call and save the results
      Results = lists:map(fun({_Table, Num, Name}) ->
        {Num, Name, web_rtp_sip:call_abonent(Num)} end, Abonents),
      % Formulate the response containing the call results
      ResponseBody = lists:flatten(io_lib:format("Call Results: ~p", [Results])),
      cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, ResponseBody, Req);
    [<<"abonent">>, Num] ->
      IntNum = binary_to_integer(Num),
      % Check for record existence
      case web_rtp_db:read_abonent(IntNum) of
        {abonents, NumAbonent, Name} when NumAbonent =:= IntNum ->
          % If record exists, initialize the call and return the result
          {_, Result} = web_rtp_sip:call_abonent(IntNum),
          Response = io_lib:format("NUM: ~p\nName: ~p\nResult: ~p", [NumAbonent, Name, Result]),
          cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, Response, Req);
        not_found ->
          % If record not found, send appropriate response
          cowboy_req:reply(404, #{}, <<"Abonent not found">>, Req);
        {abonents, _NumAbonent, _Name} ->
          % Unexpected response, send appropriate response
          cowboy_req:reply(500, #{}, <<"Internal Server Error">>, Req)
      end
  end,
```
- POST/abonent JSON: {"num": {number_abonent} - Запрос для добавления новых абонентов в базу данных;
```
handle_post(Req) ->
  {ok, Body, Req1} = cowboy_req:read_body(Req),
  io:format("~p~n", [Req1]),
  % Parse JSON
  Json = jsone:decode(Body),
  case Json of
    #{<<"name">> := Name, <<"num">> := Num} ->
      case web_rtp_db:read_abonent(Num) of
        {abonents, _, _} ->
          cowboy_req:reply(409, #{<<"content-type">> => <<"text/plain">>}, <<"Abonent already exists">>, Req);
        not_found ->
          io:format("~p~n", [Json]),
          web_rtp_db:insert_abonent(Num, Name),
          cowboy_req:reply(201, #{<<"content-type">> => <<"text/plain">>}, <<"Successfully! Insert into Database.">>, Req)
      end;
    _ ->
      io:format("~p~n", [Json]),
      cowboy_req:reply(400, #{}, <<"Invalid JSON format">>, Req)
  end.
```
- DELETE/abonent/{number_abonent} - Запрос для удаления данных из базы данных.
```
handle_post(Req) ->
  {ok, Body, Req1} = cowboy_req:read_body(Req),
  io:format("~p~n", [Req1]),
  % Parse JSON
  Json = jsone:decode(Body),
  case Json of
    #{<<"name">> := Name, <<"num">> := Num} ->
      case web_rtp_db:read_abonent(Num) of
        {abonents, _, _} ->
          cowboy_req:reply(409, #{<<"content-type">> => <<"text/plain">>}, <<"Abonent already exists">>, Req);
        not_found ->
          io:format("~p~n", [Json]),
          web_rtp_db:insert_abonent(Num, Name),
          cowboy_req:reply(201, #{<<"content-type">> => <<"text/plain">>}, <<"Successfully! Insert into Database.">>, Req)
      end;
    _ ->
      io:format("~p~n", [Json]),
      cowboy_req:reply(400, #{}, <<"Invalid JSON format">>, Req)
  end.
```