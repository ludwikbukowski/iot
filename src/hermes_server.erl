%%%-------------------------------------------------------------------
%%% @author ludwikbukowski
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Jul 2015 10:03 AM
%%%-------------------------------------------------------------------
-module(hermes_server).
-author("ludwikbukowski").
-behaviour(gen_server).
%% API
-export([start_link/1, init/1, handle_call/3, myfunc/0, terminate/2, handle_info/2, code_change/3]).

start_link(InitialValue) ->
  gen_server:start_link(
    {local,hermes_server},
    hermes_server,
    [InitialValue], []).

init(InitialValue) ->
  {ok, InitialValue}.

%% Calls
myfunc()->gen_server:call(hermes_server,myfunc).



%% Handle calls
handle_call(myfunc,From,Data)->
  {reply,"Hermes Server's task is to provide console communication interface with node.",Data};
handle_call(_,From,Data)->
  {reply,From,Data}.


%% Other
terminate(Reason, LoopData) ->
  ok.

handle_info(Message, LoopData) ->
  error(not_implemented).

code_change(OldVsn, State, Extra) ->
  error(not_implemented).