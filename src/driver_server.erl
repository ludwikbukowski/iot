%%%-------------------------------------------------------------------
%%% @author ludwikbukowski
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Jul 2015 3:07 PM
%%%-------------------------------------------------------------------
-module(driver_server).
-author("ludwikbukowski").
-define(FILE_C,code:priv_dir(iot)++"/driver_mock").
-define(FILE_MOCK,code:priv_dir(iot)++"/driver_mock").
-define(ERROR_LOGGER,my_error_logger).
-define(PACKET,{packet,1}).
-define(CLOSE,<<"close">>).
-define(CLOSE_REPLY,<<"closing">>).
-export([start_link/2, init/1, handle_info/2, terminate/2, code_change/3,  handle_call/3]).
-export([close_port/1, send_data/2, get_port/1, call_port/2]).
-behaviour(gen_server).
%%  Driver_server's task is to provide communication with external C Driver.

start_link(Name,Driver) ->
  gen_server:start_link(
    {local,Name},
    driver_server,
    [Driver], []).

init(Driver) ->
  process_flag(trap_exit,true),
  Arguments = case ?PACKET of
                {packet,Count} -> ["packet",integer_to_list(Count)];                           % TODO had some problems with packet another than "1" in C driver
                {line,Count}   -> ["line",integer_to_list(Count)]                              % TODO implement "line" on the driver side
              end,
  Opts = [stderr_to_stdout,binary, use_stdio, exit_status, {args, Arguments}, ?PACKET],
  Port = open_port({spawn_executable,Driver},Opts),
  {ok, [Port]}.


% API
-spec close_port(atom()) -> {stop, normal, any()}.
close_port(Name) ->
  gen_server:call(Name, closeport).

-spec get_port(atom()) -> {reply, port(), any()}.
get_port(Name) ->
  gen_server:call(Name, getport).

-spec send_data(atom(), any()) -> {stop, any(), any()} | {reply, {badreceive, any()}, any()} |  {reply,any(), any()}.
send_data(Name,Msg) ->
  gen_server:call(Name, {senddata,Msg}).



%% Handle Calls and casts
handle_call({senddata, Msg},_,Data) ->
  Reply = call_port(hd(Data),Msg),
  case Reply of
    {error,What,Why} ->
      ?ERROR_LOGGER:log_error({driver_server, What,Why}),
      {stop,{What,Why},Data};
    {reply, ReplyMsg}->
      {ok, ReplyMsg}  = driver_manager:add_data(ReplyMsg),
      {reply,ReplyMsg,Data};
     Stuff->
       ?ERROR_LOGGER:log_error({driver_server, unknown,Stuff}),
       {reply,{badreceive,Stuff},Data}
  end;

handle_call(getport,_,Data) ->
  {reply,hd(Data),Data};

handle_call(closeport,_,Data) ->                                          % Close port
  port_close(hd(Data)),
  {stop,normal,[]}.

%% Receive Data from Driver
handle_info(Error = {'EXIT',Pid, _},Data) when is_pid(Pid) ->               % usual termination (eg by supervisor)
  ?ERROR_LOGGER:log_error({driver_server, normal_termination,Error}),
  {stop,normal_termination,Data};
handle_info(Error = {'EXIT',Port, _},Data) when is_port(Port) ->            %  termination by external drivers death
  ?ERROR_LOGGER:log_error({driver_server, driver_termination,Error}),
  {stop,driver_termination,Data};
handle_info(Error = {_,{exit_status,_}},Data) ->                           % Received exit_status Code. Im not terminating server, because there will be sent also signal "{'Exit', ...
  ?ERROR_LOGGER:log_error({driver_server, Error}),
  {noreply,Data};
% Received data from sensor is sent to var_server
handle_info(Msg,Data) ->
  {ok,Msg} = driver_manager:add_data(Msg),
  {noreply,Data}.

%% Other
terminate(_, _) ->
  ok.

code_change(_, _, _) ->
  error(not_implemented).


%% Internal Function
call_port(Port,Msg) ->
  port_command(Port,Msg),
  receive
    Error = {'EXIT',Pid, _} when is_pid(Pid) ->                               % usual termination (eg by supervisor)
      {error,usual_termination,Error};                                        % TODO dont think it is good way to terminate process
    Error = {'EXIT',Port, _} when is_port(Port) ->                            %  termination by external drivers death
      {error,driver_termination,Error};
    Reply -> {reply,Reply}
    after 5000 ->
      {error,driver_timeout,timeout}
  end.




