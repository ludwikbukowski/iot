%%%-------------------------------------------------------------------
%%% @author ludwikbukowski
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Jul 2015 10:03 AM
%%%-------------------------------------------------------------------
-module(loop).
-author("ludwikbukowski").
-behaviour(gen_server).
%% API
-export([start_link/1, init/1, browse/0, handle_call/3, looper/0]).
start_link(InitialValue) ->
  gen_server:start_link(
    {local,var_server},
    loop,
    [InitialValue], []).
init(InitialValue) ->
  {ok, InitialValue}.
browse()->gen_server:call(var_server,ibrowse).

handle_call(ibrowse,From,Data)->
  spawn(?MODULE,looper,[]),
  {reply,nothing,Data}.
looper()->  ibrowse:send_req("https://www.google.pl", [], get),
  10+10,
  application:which_applications(),
  timer:sleep(1000),looper().



