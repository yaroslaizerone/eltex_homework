%%%-------------------------------------------------------------------
%% @doc web_rtp top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(web_rtp_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Callbacks
-export([init/1]).

%% Name server
-define(SERVER, ?MODULE).

% Starting supervisor process
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

% Initialization of child processes
init([]) ->
  io:format("Init web-rtp supervisor ~p~n", [?MODULE]),
  SupFlags = #{strategy => one_for_all,
    intensity => 5,
    period => 10},
  ChildSpecs = [
    #{id => web_rtp_db,
      start => {web_rtp_db, start, []},
      restart => permanent,
      shutdown => 5000},
    #{id => web_rtp_sip,
      start => {web_rtp_sip, start_sip, []},
      restart => permanent}
  ],

  {ok, {SupFlags, ChildSpecs}}.