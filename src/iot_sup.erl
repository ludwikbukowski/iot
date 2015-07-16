%%%-------------------------------------------------------------------
%% @doc iot top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module('iot_sup').

-behaviour(supervisor).

%% API
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
    {ok, { {one_for_one, 2, 2000},
        [
            {var_server,{var_server,start_link,[[]]},permanent,5000,worker,[var_server]}
            ,{my_error_logger,{my_error_logger,start_link,[[]]},permanent,5000,worker,[my_error_logger]}      % Its more excercise than useful module
          %  ,{connector_child,{connector_child,start_link,[]},permanent,5000,worker,[connector_child]})      Sensor's child looks like this
        ]
         }
    }.



%%====================================================================
%% Internal functions
%%====================================================================
