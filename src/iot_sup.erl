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
    supervisor:start_link({local, var_supervisor}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { {one_for_one, 2, 2000}, [{var_server,{loop,start_link,[[]]},permanent,5000,worker,[loop]}]} }.



%%====================================================================
%% Internal functions
%%====================================================================
