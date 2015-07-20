%%%-------------------------------------------------------------------
%%% @author ludwikbukowski
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Jul 2015 10:03 AM
%%%-------------------------------------------------------------------
-module(driver_manager).
-author("ludwikbukowski").
-behaviour(gen_server).
-define(FILE_C(),case application:get_env(iot,mocked) of {ok,true} -> code:priv_dir(iot)++"/driver_mock"; _->code:priv_dir(iot)++"/driver" end).
-export([start_link/1, init/1, handle_call/3, terminate/2, handle_info/2, code_change/3, handle_cast/2]).
-export([adddata/1, getdata/0,openport/0,myfunc/0,restartport/1]).

%%  Driver Manager's task is to provide console communication, restore data received from sensors and manage sensor drivers.

start_link(InitialValue) ->
  gen_server:start_link(
    {local, driver_manager},
    driver_manager,
    [InitialValue], []).

init(_) ->
  {ok, []}.



%% Api
myfunc()->
  gen_server:call(driver_manager,myfunc).
openport()->
  gen_server:call(driver_manager,{openport,?FILE_C(),driver_server}).
restartport(Id)->
  gen_server:cast(driver_manager,{restartport,Id}).
adddata(Msg)->
  gen_server:call(driver_manager,{msg,Msg}).
getdata()->
  gen_server:call(driver_manager,getdata).


%% Handle Calls and casts
handle_call({msg,Msg},_,Data)->
  {reply,{ok,Msg},Data ++ [Msg]};

handle_call({openport,File,Name},_,Data)->
  {ok,Pid}= supervisor:start_child(
    zeus_supervisor,
    {Name,{driver_server,start_link,[File]},transient,5000,worker,[driver_server]}),              %% Starting data is empty list of Ports
  {reply,{ok,Pid},Data};

handle_call(getdata,_,Data)->
  {reply,Data,Data};

handle_call(_,From,Data)->
  {reply,From,Data}.


handle_cast({restartport,Id}, Data)->
  exit(Id,kill),
  {noreply,Data}.

%% Other
terminate(_, _) ->
  ok.

handle_info(_, _) ->
  error(not_implemented).

code_change(_, _, _) ->
  error(not_implemented).