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
-define(ERROR_LOGGER,my_error_logger).

%%  Right now its now very useful, I was thinking to use it
%%  as an error but whole event manager could be removed.

-export([start_link/1, log_error/1, get_data/0]).

start_link(_) ->
  {ok,Pid} = gen_event:start_link({local,?ERROR_LOGGER}),
  gen_event:add_handler(Pid,file_logger,[]),
  gen_event:add_handler(Pid,console_logger,[]),
  {ok,Pid}.



%% API
log_error(Data) ->
  gen_event:notify(?ERROR_LOGGER,{log_error,{self(),Data}}).

get_data() ->
  gen_event:call(?ERROR_LOGGER,console_logger,getdata).
