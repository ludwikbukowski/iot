
%%%-------------------------------------------------------------------
%%% @author ludwikbukowski
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Aug 2015 4:29 PM
%%%-------------------------------------------------------------------
-module(time_SUITE).
-author("ludwikbukowski").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(CONNECTION_S,connection_server).
-define(MANAGER_S, driver_manager).
-define(HOST_MAC, <<"iot.net">>).
-define(HOST_PI, <<"iot.net">>).
%% API
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_group/2, end_per_group/2, start_connection/1, get_time/1]).
groups()->[{time,[sequence],[start_connection, get_time]}].
all()->[{group, time}].


%% Init
init_per_suite(Config)->
  ok = application:start(exml),
  ok = application:start(escalus),
  ok = application:start(base16),
  ok = application:start(sasl),
  ok = application:start(meck),
  ok = application:start(iot),
  Config.

end_per_suite(_)->
  ok = application:stop(iot),
  ok = application:stop(meck),
  ok = application:stop(sasl),
  ok = application:stop(base16),
  ok = application:stop(escalus),
  ok = application:stop(exml).


init_per_group(time,Config)->
  case os_functions:detect_os()  of
    linux ->
      application:set_env(iot, host, ?HOST_PI),
      Config;
    darwin ->
    application:set_env(iot, host, ?HOST_MAC),
      Config;
    _ ->
      {skip, unknown_os}
  end.

end_per_group(time,_)->
  ok.

start_connection(_) ->
  connected = ?MANAGER_S:connect_to_mongoose().

get_time(_) ->
  {_,_} = ?CONNECTION_S:get_time().




