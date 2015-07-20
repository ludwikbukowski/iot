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
-define(PACKET,{packet,1}).
-export([start_link/1, init/1, handle_info/2, terminate/2, code_change/3,  handle_call/3]).
-export([closeport/0, senddata/1, myfunc/0, getport/0, call_port/2]).

%%  Driver_server's task is to provide communication with external C Driver.

start_link(Driver) ->
  gen_server:start_link(
    {local,driver_server},
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
closeport()->
  gen_server:call(driver_server,closeport).
getport()->
  gen_server:call(driver_server,getport).
senddata(Msg)->
  gen_server:call(driver_server,{senddata,Msg}).
myfunc()->
  gen_server:call(driver_server,myfunc).



%% Handle Calls and casts
handle_call({senddata,Msg},_,Data)->
  Reply = call_port(hd(Data),Msg),
  case Reply of
    {error,What,Why} ->
      driver_manager:adddata({msg,{What,Why}}),
      exit(whereis(driver_server),kill);
    {reply, {Port,{data,ReplyMsg}}}->driver_manager:adddata({msg,{Port,ReplyMsg}}),
      {reply,{Port,ReplyMsg},Data};
     Stuff-> driver_manager:adddata({msg,unknown}),
     {reply,{badreceive,Stuff},Data}
  end;

handle_call(getport,_,Data)->
  {reply,Data,Data};

handle_call(closeport,_,Data)->
  lists:foreach(fun(X) -> port_close(X)  end,Data),
  {reply,[],[]}.

%% Receive Data from Driver
handle_info({'EXIT',Pid, Reason},Data) when is_pid(Pid) ->               % usual termination (eg by supervisor)
  driver_manager:adddata({msg,{normal_termination,Reason}}),
  exit(whereis(driver_server),kill),
  {noreply,Data};
handle_info({'EXIT',Port, Reason},Data) when is_port(Port) ->            %  termination by external drivers death
  driver_manager:adddata({msg,{driver_termination,Reason}}),
  exit(whereis(driver_server),kill),
  {noreply,Data};
% Received data from sensor is sent to var_server
handle_info(Msg,Data)  ->
  {ok,Msg} = driver_manager:adddata(Msg),
  {noreply,Data}.

%% Other
terminate(_, _) ->
  ok.

code_change(_, _, _) ->
  error(not_implemented).


%% Internal Function
call_port(Port,Msg)->
  port_command(Port,Msg),
  receive
    {'EXIT',Pid, Reason} when is_pid(Pid) ->                               % usual termination (eg by supervisor)
      {error,usual_termination,Reason};                                    % TODO dont think it is good way to terminate process
    {'EXIT',Port, Reason} when is_port(Port) ->                            %  termination by external drivers death
      {error,driver_termination,Reason};
    Reply -> {reply,Reply}
  end.




