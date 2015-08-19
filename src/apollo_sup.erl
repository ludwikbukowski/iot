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
  connection_server:save_time(),
  {ok, { {one_for_one, 2, 2},
    [
      {driver_manager,{driver_manager,start_link,[[]]},permanent,5000,worker,[driver_manager]}
    ]
  }
  }.



%%====================================================================
%% Internal functions
%%====================================================================
