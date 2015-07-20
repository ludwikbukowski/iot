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
-define(MANAGER_S, driver_manager).
%% API
-export([all/0, simple_test/1, init_per_suite/1, end_per_suite/1, groups/0, init_per_group/2, end_per_group/2, driver_receive_test/1, close_port_test/1, var_receive_test/1, check_port_test/1, error_receive_test1/1, driver_send_test1/1, driver_send_test2/1, whole_echo_test/1]).

groups()->[{communication,[sequence],[check_port_test,driver_receive_test,var_receive_test,
  driver_send_test1,driver_send_test2,whole_echo_test,close_port_test]}].

all()->[simple_test,{group,communication}].


%% Init
init_per_suite(Config)->
  ok = application:start(meck),
  ok = application:start(iot),
  Config.

end_per_suite(_)->
  ok = application:stop(iot),
  ok = application:stop(meck).


init_per_group(communication,Config)->
  meck:new(?MANAGER_S,[unstick,passthrough]),
  meck:expect(?MANAGER_S,openport,fun()->gen_server:call(driver_manager,{openport,?FILE_MOCK,driver_server}) end),
  ?MANAGER_S:openport(),
  meck:unload(?MANAGER_S),
  Config.

end_per_group(communication,_)->
  ?DRIVER_S:closeport(),
  ok.



%% Tests
simple_test(_)->
  true = is_pid(whereis(zeus_supervisor)),
  true = is_pid(whereis(?MANAGER_S)).

%% Driver_server side
check_port_test(_)->
  List = ?DRIVER_S:getport(),
  1 = length(List),
  [Port] = List,
  true = is_port(Port).

driver_receive_test(_) ->                                                       %% Receive data from fake driver
  meck:new(?MANAGER_S,[unstick,passthrough]),
  meck:new(?DRIVER_S,[unstick,passthrough]),
  meck:expect(?MANAGER_S,adddata,fun(Msg)->{ok,Msg} end),                       %% Mock Var_server response
  ?DRIVER_S ! {some_port,{data,<<"Hej!">>}},
  meck:wait(?DRIVER_S,handle_info,['_','_'],5000),
  meck:unload(?DRIVER_S),
  meck:unload(?MANAGER_S).

%% Var Server side
var_receive_test(_)->                                                          %% Receiving data from var server
  ?MANAGER_S:adddata({some_port,{data,<<"Hi there!">>}}),
  [{some_port,{data,<<"Hi there!">>}}] = ?MANAGER_S:getdata().

%% Mixed
driver_send_test1(_)->                                                          %% Sending data and mocking answer
  meck:new(?DRIVER_S,[unstick,passthrough]),
  meck:expect(?DRIVER_S, call_port,fun(_,_)->{reply,{someport,{data,<<"Some funny stuff.">>}}} end),
  {_,{data,<<"Some funny stuff.">>}} = ?DRIVER_S:senddata("Some funny stuff."),
    meck:unload(?DRIVER_S).

driver_send_test2(_)->                                                          %% Sending data without restoring answer
  meck:new(?MANAGER_S,[unstick,passthrough]),
  meck:expect(?MANAGER_S,adddata,fun(Msg)->{ok,Msg} end),
  ?DRIVER_S:senddata("Some well-known movie quote."),
  meck:unload(?MANAGER_S).

whole_echo_test(_)->                                                          %% Send data to driver, get answer and pass to driver_manager
  {_,{data,_}} = ?DRIVER_S:senddata("Rolling Stones roxs"),
  [_,_,{_,{data,<<"Rolling Stones roxs">>}}] = driver_manager:getdata().

close_port_test(_)->
  [] = ?DRIVER_S:closeport().


error_receive_test1(_) ->                                                %% TODO Test crashes
  meck:new(?MANAGER_S,[unstick,passthrough]),
  meck:new(?DRIVER_S ,[unstick,passthrough]),
  meck:expect(?MANAGER_S,adddata,fun(Msg)->{ok,Msg} end),
  ?DRIVER_S ! {'EXIT',some_port, normal},
  meck:wait(?DRIVER_S,handle_info,['_','_'],5000),
  meck:unload(?MANAGER_S).




