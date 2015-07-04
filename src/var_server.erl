%%%-------------------------------------------------------------------
%%% @author ludwikbukowski
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Jul 2015 10:03 AM
%%%-------------------------------------------------------------------
-module(var_server).
-author("ludwikbukowski").
-behaviour(gen_server).
%% API
-export([start_link/1, init/1, handle_call/3, myfunc/0, terminate/2, handle_info/2, code_change/3, openport/0, restartport/1, handle_cast/2]).

start_link(InitialValue) ->
  gen_server:start_link(
    {local,var_server},
    var_server,
    [InitialValue], []).

init(_) ->
  {ok, []}.



%% Calls
myfunc()->gen_server:call(var_server,myfunc).
openport()->gen_server:call(var_server,openport).
restartport(Id)->gen_server:cast(var_server,{restartport,Id}).


%% Handle calls
handle_call(myfunc,From,Data)->
  {reply,"Server's task is to provide console communication interface with node.",[]};

handle_call({msg,Msg},From,Data)->
  {reply,{ok,Msg},Data ++ [Msg]};

handle_call(openport,From,Data)->
  {ok,Pid}= supervisor:start_child(
    zeus_supervisor,
    {driver_server,{driver_server,start_link,[[]]},transient,5000,worker,[driver_server]}  %% Starting data is empty list of Ports
                                  ),
  {reply,{ok,Pid},Data};


handle_call(_,From,Data)->
  {reply,From,Data}.


handle_cast({restartport,Id}, Data)->
  exit(Id,kill),
  {noreply,Data}.

%% Other
terminate(Reason, LoopData) ->
  ok.

handle_info(Message, LoopData) ->
  whatssuup.

code_change(OldVsn, State, Extra) ->
  error(not_implemented).