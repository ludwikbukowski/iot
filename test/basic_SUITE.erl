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
  driver_send_test1,driver_send_test2,whole_echo_test]}].

all()->[simple_test,{group,communication}].


%% Init (Its bad to start all application but yet didnt figure out how to do in other way with rebar3)
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

init_per_group(communication,Config)->
  meck:new(?MANAGER_S,[unstick,passthrough]),
  meck:expect(?MANAGER_S,open_port,fun(_,_)->gen_server:call(driver_manager,{openport,?DRIVER_S,?FILE_MOCK,uart}) end),
  ?MANAGER_S:open_port(?DRIVER_S,uart),
  meck:unload(?MANAGER_S),
  Config.

end_per_group(communication,_)->
  ?MANAGER_S:close_port(?DRIVER_S),
  ok.



%% Tests
simple_test(_)->
  true = is_pid(whereis(zeus_supervisor)),
  true = is_pid(whereis(?MANAGER_S)).

%% Driver_server side
check_port_test(_)->
  Port = ?DRIVER_S:get_port(?DRIVER_S),
  true = is_port(Port).

driver_receive_test(_) ->                                                       %% Receive data from fake driver
  meck:new(?MANAGER_S,[unstick,passthrough]),
  meck:new(?DRIVER_S,[unstick,passthrough]),
  meck:expect(?MANAGER_S,add_data,fun(Msg)->{ok,Msg} end),                       %% Mock driver_manager response
  ?DRIVER_S ! {some_port,{data,<<"Hej!">>}},
  meck:wait(?DRIVER_S,handle_info,['_','_'],5000),
  meck:unload(?DRIVER_S),
  meck:unload(?MANAGER_S).

%% Var Server side
var_receive_test(_)->                                                          %% Receiving data from driver_manager
  ?MANAGER_S:add_data({some_port,{data,<<"Hi there!">>}}),
  [{some_port,{data,<<"Hi there!">>}}] = ?MANAGER_S:get_data().

%% Mixed
driver_send_test1(_)->                                                         %% Sending data and mocking answer
  meck:new(?DRIVER_S,[unstick,passthrough]),
  meck:expect(?DRIVER_S, call_port,fun(_,_)->{reply,{someport,{data,<<"Some funny stuff.">>}}} end),
  {_,{data,<<"Some funny stuff.">>}} = ?DRIVER_S:send_data(?DRIVER_S,"Some funny stuff."),
  meck:unload(?DRIVER_S).

driver_send_test2(_)->                                                         %% Sending data without restoring answer
  meck:new(?MANAGER_S,[unstick,passthrough]),
  meck:expect(?MANAGER_S,add_data,fun(Msg)->{ok,Msg} end),
  ?DRIVER_S:send_data(?DRIVER_S,"Some well-known movie quote."),
  meck:unload(?MANAGER_S).

whole_echo_test(_)->                                                           %% Send data to driver, get answer and pass to driver_manager
  {_,{data,_}} = ?DRIVER_S:send_data(?DRIVER_S,"Rolling Stones roxs"),
  [_,_,{_,{data,<<"Rolling Stones roxs">>}}] = driver_manager:get_data().

close_port_test(_)->
  {safe_close,_} = ?MANAGER_S:close_port(?DRIVER_S).


error_receive_test1(_) ->                                                %% TODO Test crashes
  meck:new(?MANAGER_S,[unstick,passthrough]),
  meck:new(?DRIVER_S ,[unstick,passthrough]),
  meck:expect(?MANAGER_S,add_data,fun(_,Msg)->{ok,Msg} end),
  ?DRIVER_S ! {'EXIT',some_port, normal},
  meck:wait(?DRIVER_S,handle_info,['_','_'],5000),
  meck:unload(?MANAGER_S).




