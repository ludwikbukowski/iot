%%%-------------------------------------------------------------------
%%% @author ludwikbukowski
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Aug 2015 5:29 PM
%%%-------------------------------------------------------------------
-module(hermes_sender).
-author("ludwikbukowski").
-define(MANAGER_S, driver_manager).
-define(CONNECTION_S, connection_server).
-define(SLEEP_TIME, 1000).
-define(DATA_PORTION, 10).
%% API
-export([start_link/0, init/1]).


start_link() ->
  Pid = spawn(?MODULE, init, [[]]),
  {ok, Pid}.

init(_) ->
%%   Make connection
  loop().

loop() ->
  timer:sleep(?SLEEP_TIME),
  DataList = ?MANAGER_S:remove_data(?DATA_PORTION),
  ?CONNECTION_S:send_data(DataList),
  loop().
