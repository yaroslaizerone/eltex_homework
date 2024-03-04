-module(web_rtp_sip).

-include_lib("nkserver/include/nkserver_module.hrl").
-include_lib("nksip/include/nksip.hrl").

%% API
-export([call_abonent/1, start_sip/0]).

%% API functions
start_sip() ->
  nksip:start_link(nkservice,
    #{sip_listen=>"<sip:all:5060>",
      plugins => [nksip_uac_auto_auth],
      sip_from => "sip:101@test.group"}).

%% Calling abonent using SIP
-spec call_abonent(NumAbonent :: string()) -> {ok, string()}.
call_abonent(NumAbonent) ->
  Uri = "<sip:" ++ integer_to_list(NumAbonent) ++ "@10.0.20.11;transport=tcp>",
  SrvId = nkservice,

  nksip_uac:register(SrvId, Uri, [{sip_pass, "1234"}, contact, {get_meta, [<<"contact">>]}]),

  CodecConfig = [{<<"audio">>, 1080, [{rtpmap, 0, <<"PCMU/8000">>}, sendrecv]}],

  case nksip_uac:invite(SrvId, Uri,
    [{sip_pass, "1234"},
      {body, nksip_sdp:new("10.0.20.11", CodecConfig)},
      {get_meta, [reason_phrase]},
      auto_2xx_ack]) of
    {ok, 200, [{dialog, DialogId},{reason_phrase,<<"OK">>}]} ->
      handle_successful_invite(DialogId);
    {ok, Code, _} ->
      handle_error_response(Code, NumAbonent);
    _ ->
      handle_unhandled_error()
  end.

%% Handling a successful invite response
handle_successful_invite(DialogId) ->
  {ok, Meta} = nksip_dialog:get_meta(invite_remote_sdp, DialogId),
  [MediaList | _] = element(18, Meta),
  Port = erlang:element(3, MediaList),
  Remote_PBX_IP =  erlang:binary_to_list(element(3, element(8, MediaList))),
  voice_call(Port, Remote_PBX_IP),

  nksip_uac:bye(DialogId, []),

  Response = "Dialog was started with code 200.\nDialog was finished successfully!\n",
  io:format("Response: ~p~n", [Response]),
  {ok, Response}.

%% Executing a voice call
voice_call(Port, Remote_PBX_IP) ->
  ConvertVoice = "ffmpeg -i voice/test_call.wav -codec:a pcm_mulaw -ar 8000 -ac 1 voice/output.wav -y",
  StartVoice = "./voice_client voice/output.wav " ++ Remote_PBX_IP ++ " " ++ erlang:integer_to_list(Port),
  Cmd = ConvertVoice ++ " && " ++ StartVoice,
  Res = os:cmd(Cmd),

  io:format("Cmd ~p~nResult ~p~n", [Cmd, Res]).

%% Handling different response codes
handle_error_response(403, NumAbonent) ->
  Response = "Code 403. Dialog wasn't started.\nAbonent " ++ integer_to_list(NumAbonent) ++ " is forbidden to receive calls!\n",
  io:format("Response: ~p~n", [Response]),
  {ok, Response};
handle_error_response(480, NumAbonent) ->
  Response = "Code 480. Dialog wasn't started.\nAbonent " ++ integer_to_list(NumAbonent) ++ " is could not respond now!\n",
  io:format("Response: ~p~n", [Response]),
  {ok, Response};
handle_error_response(486, NumAbonent) ->
  Response = "Code 486. Dialog wasn't started.\nAbonent " ++ integer_to_list(NumAbonent) ++ " is Busy Here\n",
  io:format("Response: ~p~n", [Response]),
  {ok, Response};
handle_error_response(404, NumAbonent) ->
  Response = "Code 404. Error! Dialog wasn't started.\nAbonent " ++ integer_to_list(NumAbonent) ++ " is NOT FOUND!\n",
  io:format("Response: ~p~n", [Response]),
  {ok, Response};
handle_error_response(Code, _NumAbonent) ->
  Response = "Response code " ++ integer_to_list(Code) ++ "!\nAn error occurred!\n",
  io:format("Response: ~p~n", [Response]),
  {ok, Response}.

%% Handling unhandled errors
handle_unhandled_error() ->
  Response = "Error! An unhandled error occurring during invite request!\n",
  io:format("Response: ~p~n", [Response]),
  {ok, Response}.