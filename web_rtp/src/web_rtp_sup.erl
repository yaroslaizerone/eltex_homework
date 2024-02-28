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
      restart => transient,
      shutdown => 5000}
  ],
  {ok, {SupFlags, ChildSpecs}}.

%% Stop supervisor
terminate(_Reason, _State) ->
  supervisor:terminate_child(?MODULE, web_rtp_db),
  ok.