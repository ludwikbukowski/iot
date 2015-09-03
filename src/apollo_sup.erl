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
  case application:get_env(iot,refresh_time) of
    {ok, true} ->
    connection_server:save_time();
    _ ->
      ok
  end,
%%   dbg:tracer(),dbg:tp(connection_server, handle_info,x),dbg:p(all,c),
  connection_server:create_node(),
  {ok, { {one_for_one, 2, 2},
    [
      {calliope,{driver_manager,start_link,[[]]},permanent,5000,worker,[driver_manager]},         % Muse of poetry; Responsibility: write data from sensor to own state
      {melpomene,{my_error_logger,start_link,[[]]},permanent,5000,worker,[my_error_logger]},      % Muse of Tragedy; Responsibility: Log ERRORS
      {terpsichore,{driver_server, start_link,[terpsichore, os_functions:get_driver_exec_file()]}, transient, 5000, worker, [driver_server]} % Muse of dance; Responsibility: Get data from sensor and send to caliope
    ]
  }
  }.



%%====================================================================
%% Internal functions
%%====================================================================
