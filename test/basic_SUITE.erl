%%%-------------------------------------------------------------------
%%% @author ludwikbukowski
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Jul 2015 4:49 PM
%%%-------------------------------------------------------------------
-module(basic_SUITE).
-author("ludwikbukowski").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-define(FILE_C,code:priv_dir(iot)++"/driver").
-define(FILE_MOCK,code:priv_dir(iot)++"/driver_mock").
-define(DRIVER_S,driver_server).
-define(VAR_S,var_server).
%% API
-export([all/0, simple_test/1, init_per_suite/1, end_per_suite/1, groups/0, init_per_group/2, end_per_group/2, pass_data_test/1, close_port_test/1]).

groups()->[{driverServer,[sequence],[pass_data_test,close_port_test]}].

all()->[simple_test,{group,driverServer}].


%% Tests
init_per_suite(Config)->
  ok = application:start(meck),
  ok = application:start(iot),
  Config.

end_per_suite(_)->
  ok = application:stop(iot),
  ok = application:stop(meck).

simple_test(_)->
  true = is_pid(whereis(zeus_supervisor)),
  true = is_pid(whereis(var_server)).

init_per_group(driverServer,Config)->
  ?VAR_S:openport(),
  meck:new(?DRIVER_S,[passthrough]),
  meck:expect(?DRIVER_S,init,fun(Init)->{ok,[fake_port]} end),
 ?DRIVER_S:start_link([]),
  Config.

end_per_group(driverServer,_)->                                               %%TODO what about killing started driver_server?
  ok.


pass_data_test(Config) ->
  meck:new(?VAR_S,[unstick,passthrough]),
  meck:expect(?VAR_S,adddata,fun(Msg)->{ok,Msg} end)
  , ?DRIVER_S ! {some_port,{data,<<"Hi there!">>}}.

close_port_test(Config)->
  [] = ?DRIVER_S:closeport().





%error_receive_test1(Config) ->                                                %% TODO Test crashes
 % meck:new(?VAR_S,[unstick,passthrough]),
 % meck:expect(?VAR_S,adddata,fun(Msg)->{ok,Msg} end),
 % Port = open_port({spawn_executable,?FILE_C},[]),                            %% Just for mocking Port
 % ?DRIVER_S ! {'EXIT',Port, normal}.



