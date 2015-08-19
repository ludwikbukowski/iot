
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
-include_lib("escalus/include/escalus.hrl").

-define(CONNECTION_S,connection_server).
-define(OS_FUN,os_functions).
-define(MANAGER_S, driver_manager).
-define(HOST_MAC, <<"iot.net">>).
-define(HOST_PI, <<"iot.net">>).
-define(SOME_NAME, some_name).
%% API
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_group/2, end_per_group/2, start_connection/1, get_time/1, simple_register/1, simple_unregister/1, register_and_receive/1, save_time_to_os/1]).
groups()->[{time,[sequence],[start_connection, get_time, save_time_to_os]},{handling,[sequence],[simple_register, simple_unregister, register_and_receive]}].
all()->[{group, time}, {group, handling}].


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

init_per_group(handling, Config) ->
  Config;
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

end_per_group(handling,_)->
  ok;
end_per_group(time,_)->
  ok.

simple_register(_) ->
  Fun = fun(_) ->
    ok
  end,
  registered = ?CONNECTION_S:register_handler(?SOME_NAME, Fun).

simple_unregister(_) ->
  unregistered = ?CONNECTION_S:unregister_handler(?SOME_NAME).

%% Register handler which sends received Stanza to test proccess
register_and_receive(_) ->
  Self = self(),
  Fun =  fun(Stanza) ->
    Self ! Stanza
  end,
  registered = ?CONNECTION_S:register_handler(?SOME_NAME, Fun),
  ExampleStanza = escalus_stanza:presence(<<"available">>),
  ?CONNECTION_S ! {stanza, ok, ExampleStanza},                            %% Simulate XMPP Msg from server
  receive
    Msg ->
      case escalus_pred:is_presence(Msg) of
        true ->
          Msg = ExampleStanza,
          unregistered = ?CONNECTION_S:unregister_handler(?SOME_NAME);
      _ ->
          erlang:error(wrong_stanza)
      end;
    _ ->
      erlang:error(wrong_receive)
  end.



start_connection(_) ->
  connected = ?MANAGER_S:connect_to_mongoose().


get_time(_) ->
  {_,_} = ?CONNECTION_S:get_time().

% Not really. Cmd:os is called but with no effect on Mac
save_time_to_os(_) ->
  meck:new(?OS_FUN,[unstick,passthrough]),
  meck:expect(?OS_FUN,detect_os,fun()->linux end),
  {A,B} = ?CONNECTION_S:get_time(),
  ?OS_FUN:change_time(B,A),
  meck:unload(?OS_FUN).




