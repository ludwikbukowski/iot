%%%-------------------------------------------------------------------
%%% @author ludwikbukowski
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Jul 2015 10:03 AM
%%%-------------------------------------------------------------------
-module(demeter_server).
-author("ludwikbukowski").
-behaviour(gen_server).
%% API
-export([start_link/1, init/1, handle_call/3, myfunc/0]).

start_link(InitialValue) ->
  gen_server:start_link(
    {local,demeter_server},
    demeter_server,
    [InitialValue], []).

init(InitialValue) ->
  {ok, InitialValue}.

%% Calls
myfunc()->gen_server:call(demeter_server,myfunc).



%% Handle calls
handle_call(myfunc,From,Data)->
  {reply,"Demeter Server's task is to provide console communication interface with node.",Data}.





