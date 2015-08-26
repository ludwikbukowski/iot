%%%-------------------------------------------------------------------
%%% @author ludwikbukowski
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Aug 2015 3:15 PM
%%%-------------------------------------------------------------------
-module(apollo_sup).
-author("ludwikbukowski").

%%====================================================================
%% API functions
%%====================================================================

-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link({local, apollo_supervisor}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  % First, ask Connection_Server for Time
  connection_server:connect(),
  connection_server:create_node(),
  case application:get_env(iot,refresh_time) of
    {ok, true} ->
      connection_server:save_time();
    _ ->
      ok
  end,
  {ok, { {one_for_one, 2, 2},
    [
      {driver_manager,{driver_manager,start_link,[[]]},permanent,5000,worker,[driver_manager]},
      {my_error_logger,{my_error_logger,start_link,[[]]},permanent,5000,worker,[my_error_logger]},      % Its more excercise than useful module
      {sensor_server,start_link,[sensor_server, os_functions:get_driver_exec_file()], transient, 5000, worker, [driver_server]}
    ]
  }
  }.



%%====================================================================
%% Internal functions
%%====================================================================
