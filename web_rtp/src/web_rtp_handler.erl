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

handle_delete(Req) ->
  Path = binary:split(cowboy_req:path(Req), <<"/">>, [global, trim_all]),
  io:format("Path: ~p~n", [Path]),
  case Path of
    [<<"abonent">>, Num ] ->
      IntNum = binary_to_integer(Num),
      % Check for record existence
      case web_rtp_db:read_abonent(IntNum) of
        {abonents, _, _} ->
          % If record exists, delete it
          web_rtp_db:delete_abonent(IntNum),
          cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, "Accuses delete.", Req);
        not_found ->
          % If record not found, send appropriate response
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
  ok.