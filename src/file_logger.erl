%%%-------------------------------------------------------------------
%%% @author ludwikbukowski
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Jul 2015 4:10 PM
%%%-------------------------------------------------------------------
-module(file_logger).
-author("ludwikbukowski").
-behavoiur(gen_event).
-define(FILE_LOG,"./error.log").
-export([init/1, terminate/2, code_change/3, handle_info/2, handle_call/2, handle_event/2]).
init(_) -> {ok,[]}.

handle_call(_,State) ->
  {ok,ok,State}.

handle_info(_,State) ->
  {ok,State}.

code_change(_, State, _) ->
  {ok,State}.

terminate(_, _) -> ok.

handle_event({log_error,Data},State) ->
  file:write_file(?FILE_LOG, io_lib:fwrite("~p.\n", [Data]),[append]),
  {ok,State}.