-module(web_rtp_handler).

-behavior(cowboy_handler).

%% API
-export([init/2, handle_request/1]).

%% Initializing the web handler
init(Req, State) ->
  handle_request(Req),
  {ok, Req, State}.

%% Processing requests
handle_request(Req) ->
  {Method, HasBody} = {cowboy_req:method(Req), cowboy_req:has_body(Req)},
  io:format("Handler was called.~nReq: ~p~nMethod: ~p~nHasBody: ~p~n", [Req, Method, HasBody]),
  case {Method, HasBody} of
    {<<"GET">>, false} ->
      handle_get(Req);
    {<<"POST">>, true} ->
      handle_post(Req);
    {<<"DELETE">>, false} ->
      handele_delete(Req);
    {_, _} ->
      ok
  end.

%% Processing GET /abonent/<NUM> and GET /abonents requests
handle_get(Req) ->
  %% TODO понять как парсить путь запроса
  {ok,Path, Req} = cowboy_req:read_body(Req),
  io:format(Path),
  case Path of
    [<<"abonents">>] ->
      handle_abonents_get(Req);
    [<<"abonent">>, Number] ->
      handle_abonent_get(Req, Number);
    _ ->
      handle_not_found(Req)
  end.

%% Processing POST requests for /abonent
handle_post(Req) ->
  Path = binary:split(cowboy_req:path(Req), <<"/">>, [global, trim_all]),
  io:format("Path: ~p~n", [Path]),
  case Path of
    [<<"abonent">>] ->
      handle_abonent_post(Req);
    _ ->
      handle_not_found(Req)
  end.

%% Processing DELETE requests for /abonent/<NUM>
handele_delete(Req) ->
  {Path, _, _} = cowboy_req:parse_path(Req),
  case Path of
    [<<"abonent">>, Number] ->
      handle_abonent_delete(Req, Number);
    _ ->
      handle_not_found(Req)
  end.

%% Processing GET requests for /abonents
handle_abonents_get(Req) ->
  Data = web_rtp_db:read_all_abonent(),
  Response = format_response(Data),
  send_response(200, #{<<"content-type">> => <<"application/json">>}, Response, Req).

%% Processing GET requests for /abonent/<NUM>
handle_abonent_get(Req, Number) ->
  Data = web_rtp_db:read_abonent(binary_to_integer(Number)),
  Response = format_response_with_msg(Data, Number),
  send_response(200, #{<<"content-type">> => <<"application/json">>}, Response, Req).

%% Processing POST requests for /abonent
handle_abonent_post(Req) ->
  {ok, DataBin, _} = cowboy_req:read_body(Req),
  Body = jsone:decode(DataBin),
  case maps:get(<<"num">>, Body) of
    {ok, Num} when is_integer(Num) ->
      case maps:get(<<"name">>, Body) of
        {ok, Name} when is_binary(Name) ->
          web_rtp_db:insert_abonent(Num, binary_to_list(Name)),
          send_response(200, #{<<"content-type">> => <<"text/plain">>}, <<"Succesfully insert into Database!\nReuqest was handelled.">>, Req);
        _ ->
          send_response(200, #{<<"content-type">> => <<"text/plain">>}, <<"Wrong Data format, record wasn't insert into Database!\nReuqest was handelled.">>, Req)
      end;
    _ ->
      handle_bad_request(Req)
  end.

%% Processing DELETE requests for /abonent/<NUM>
handle_abonent_delete(Req, Number) ->
  web_rtp_db:delete_abonent(binary_to_integer(Number)),
  send_response(201, #{<<"content-type">> => <<"text/plain">>}, <<"Successfully deleted from Database!\nReuqest was handelled.">>, Req).

%% Handling the case when the requested path is not found
handle_not_found(Req) ->
  send_response(404, #{}, <<"Requested page not found.">>, Req).

%% Handling the case of an invalid POST request
handle_bad_request(Req) ->
  send_response(400, #{}, <<"Bad Request!">>, Req).

%% Formatting the response in JSON format
format_response(Data) ->
  ConvertedData = lists:map(fun({Table, Num, Name}) ->
    #{table => Table,
      num => Num,
      name => erlang:list_to_binary(Name),
      msg => element(2, web_rtp_sip:call_abonent(integer_to_list(Num)))}
                            end, Data),
  jsone:encode(#{response => ConvertedData}).

%% Formatting a response in JSON format with a message
format_response_with_msg(Data, Number) ->
  {ok, RespMsg} = web_rtp_sip:call_abonent(binary_to_list(Number)),
  ConvertedData = lists:map(fun({Table, Num, Name}) ->
    #{table => Table,
      num => Num,
      name => erlang:list_to_binary(Name),
      msg => RespMsg}
                            end, Data),
  jsone:encode(#{response => ConvertedData}).

%% Sending a response to the client
send_response(Status, Headers, Body, Req) ->
  DeliverRes = cowboy_req:reply(Status, Headers, Body, Req),
  io:format("Request was handled!~n"),
  {ok, DeliverRes}.