%%%-------------------------------------------------------------------
%% @doc iot top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(iot_sup).
-behaviour(supervisor).
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, zeus_supervisor}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { {rest_for_one, 2, 2},
        [
            {ares_server,{connection_server,start_link,[]},permanent,5000,worker,[connection_server]},      % The Oldest Ares children, his responsibility is to send almost all mongoose requests
            {apollo_supervisor,{apollo_sup,start_link,[]},permanent,5000,supervisor,[apollo_sup]},
            {hermes_sender,{hermes_sender,start_link,[]}, transient, 5000, worker, [hermes_sender]}
        ]
         }
    }.



%%====================================================================
%% Internal functions
%%====================================================================
