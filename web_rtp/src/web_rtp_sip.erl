-module(web_rtp_sip).

-include_lib("nkserver/include/nkserver_module.hrl").
-include_lib("nksip/include/nksip.hrl").

-behaviour(supervisor).

%% API
-export([start_link/0, init/1, call_abonent/1]).

%% API functions
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% Calling abonent using SIP
-spec call_abonent(NumAbonent :: string()) -> {ok, string()}.
call_abonent(NumAbonent) ->
  From_Uri = "sip:102@test.domain",
  SrvId = test_ip_102,
  register_server(SrvId, From_Uri),

  Uri = "sip:" ++ integer_to_list(NumAbonent) ++ "@test.domain",
  PBX_IP = "10.0.20.11",
  SDP = create_sdp(PBX_IP),

  InviteOptions = [
    {add, "x-nk-op", ok},
    auto_2xx_ack,
    get_request,
    {route, "<sip:10.0.20.11;lr>"},
    {sip_pass, "1234"},
    {body, SDP}
  ],

  case nksip_uac:invite(SrvId, Uri, InviteOptions) of
    {ok, 200, [{dialog, DialogId}]} ->
      handle_successful_invite(DialogId, PBX_IP);
    {ok, Code, _} ->
      handle_error_response(Code, NumAbonent)
    %_ ->
     % handle_unhandled_error()
  end.

%% Registering SIP server
register_server(SrvId, From_Uri) ->
  case whereis(SrvId) of
    _Pid ->
      nksip_uac:register(SrvId, "sip:10.0.20.11", [{sip_pass, "1234"}, contact, {meta, ["contact"]}]);
    undefined ->
      nksip:start_link(SrvId, #{sip_from => From_Uri,plugins => [nksip_uac_auto_auth], sip_listen => "<sip:all:8443;transport=udp>"}),
      nksip_uac:register(SrvId, "sip:10.0.20.11", [{sip_pass, "1234"}, contact, {meta, ["contact"]}])
  end.

%% Creating SDP for SIP request
create_sdp(PBX_IP) ->
  #sdp{address = {<<"IN">>, <<"IP4">>, erlang:list_to_binary(PBX_IP)},
    connect = {<<"IN">>, <<"IP4">>, erlang:list_to_binary(PBX_IP)},
    time = [{0, 0, []}],
    medias = [#sdp_m{media = <<"audio">>,
      port = 9990,
      proto = <<"RTP/AVP">>,
      fmt = [<<"0">>, <<"101">>],
      attributes = [{<<"sendrecv">>, []}]
    }
    ]
  }.

%% Handling a successful invite response
handle_successful_invite(DialogId, PBX_IP) ->
  {ok, Meta} = nksip_dialog:get_meta(invite_remote_sdp, DialogId),
  [MediaList | _] = element(18, Meta),
  Port = erlang:element(3, MediaList),
  Remote_PBX_IP =  erlang:binary_to_list(element(3, element(8, MediaList))),
  execute_voice_call(Port, Remote_PBX_IP),

  nksip_uac:bye(DialogId, []),

  Response = "Dialog was started with code 200.\nDialog was finished succesfully!\n",
  io:format("Response: ~p~n", [Response]),
  {ok, Response}.

%% Executing a voice call
execute_voice_call(Port, Remote_PBX_IP) ->
  CurrentDir = "cd apps/web_rtp",
  ConvertVoice = "ffmpeg -i priv/voice/generate.wav -codec:a pcm_mulaw -ar 8000 -ac 1 priv/voice/output.wav -y",
  StartVoice = "./voice_client priv/voice/output.wav " ++ Remote_PBX_IP ++ " " ++ erlang:integer_to_list(Port),
  Cmd = CurrentDir ++ " && " ++ ConvertVoice ++ " && " ++ StartVoice,
  Res = os:cmd(Cmd),

  io:format("Cmd ~p~nResult ~p~n", [Cmd, Res]).

%% Handling different response codes
handle_error_response(480, NumAbonent) ->
  Response = "Code 480. Dialog wasn't started.\nAbonent " ++ NumAbonent ++ " is could not respond now!\n",
  io:format("Response: ~p~n", [Response]),
  {ok, Response};
handle_error_response(486, NumAbonent) ->
  Response = "Code 486. Dialog wasn't started.\nAbonent " ++ NumAbonent ++ " is Busy Here\n",
  io:format("Response: ~p~n", [Response]),
  {ok, Response};
handle_error_response(404, NumAbonent) ->
  Response = "Code 404. Error! Dialog wasn't started.\nAbonent " ++ NumAbonent ++ " is NOT FOUND!\n",
  io:format("Response: ~p~n", [Response]),
  {ok, Response};
handle_error_response(Code, _NumAbonent) ->
  Response = "Response code " ++ erlang:integer_to_list(Code) ++ "!\nAn error occured!\n",
  io:format("Response: ~p~n", [Response]),
  {ok, Response}.

%% Handling unhandled errors
handle_unhandled_error() ->
  Response = "Error! An unhandled error occurring during invite request!\n",
  io:format("Response: ~p~n", [Response]),
  {ok, Response}.

%% Callbacks
init([]) ->
  SupSpec = nksip:get_sup_spec(test_ip_102, #{
    sip_from => "sip:102@test.domain",
    plugins => [nksip_uac_auto_auth, nksip_100rel],
    sip_listen => "<sip:all:8443;transport=udp>"
  }),
  {ok, {SupSpec, []}}.