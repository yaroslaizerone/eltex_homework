%%%-------------------------------------------------------------------
%% @doc web_rtp public API
%% @end
%%%-------------------------------------------------------------------

-module(web_rtp_app).

-behaviour(application).

-export([start/2, stop/1]).

%% Starting the application.
start(_StartType, _StartArgs) ->
    io:format("Application started!~n"),
    Dispatch = cowboy_router:compile([
        {'_',[
            {"/", web_rtp_handler, []},
            {"/abonents", web_rtp_handler, []},
            {"/abonent/:abonent_number", web_rtp_handler, []},
            {"/abonent", web_rtp_handler, []}
        ]}
    ]),
    cowboy:start_clear(http,  [{port, 8080}], #{env => #{dispatch => Dispatch}}),
    web_rtp_sup:start_link().

%% Stopping the application
stop(_State) ->
    ok.
