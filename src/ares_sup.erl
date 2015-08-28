%%%-------------------------------------------------------------------
%%% @author ludwikbukowski
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Aug 2015 5:29 PM
%%%-------------------------------------------------------------------
-module(ares_sup).
-author("ludwikbukowski").


%%====================================================================
%% API functions
%%====================================================================

-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link({local, ares_supervisor}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  % First, ask Connection_Server for Time

  {ok, { {one_for_one, 4, 2},
    [
      {eros,{connection_server,start_link,[eros]},permanent,5000,worker,[connection_server]}      % The Oldest Ares children, his responsibility is to send almost all mongoose requests
      ,{phobos,{connection_server,start_link,[phobos]},permanent,5000,worker,[connection_server]},  % The rest of children publish data (in loop, they are changing)
      {harmonia,{connection_server,start_link,[harmonia]},permanent,5000,worker,[connection_server]},
      {deimos,{connection_server,start_link,[deimos]},permanent,5000,worker,[connection_server]},
      {pothos,{connection_server,start_link,[pothos]},permanent,5000,worker,[connection_server]},
      {alcippe,{connection_server,start_link,[alcippe]},permanent,5000,worker,[connection_server]},
      {himeros,{connection_server,start_link,[himeros]},permanent,5000,worker,[connection_server]}
    ]
  }
  }.



%%====================================================================
%% Internal functions
%%====================================================================
