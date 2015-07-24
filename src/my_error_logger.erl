%%%-------------------------------------------------------------------
%%% @author ludwikbukowski
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Jul 2015 3:16 PM
%%%-------------------------------------------------------------------
-module(my_error_logger).
-author("ludwikbukowski").
-behavoiur(gen_event).


%%  Right now its now very useful, I was thinking to use it
%%  as an error but whole event manager could be removed.

-export([start_link/1, init/0, log_error/1]).

start_link(_)->
  gen_event:start_link({local,lala}).


init()->
  gen_event:add_handler(my_error_logger,file_logger,[]),
  gen_event:add_handler(my_error_logger,console_logger,[]),
  {ok,[]}.

%% API
log_error(Data)->
  gen_event:notify(my_error_logger,{log_error,Data}).
