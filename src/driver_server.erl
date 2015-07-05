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
-define(FILE_C,code:priv_dir(iot)++"/driver").
-define(FILE_MOCK,code:priv_dir(iot)++"/driver_mock").
-define(PACKET,{packet,1}).
%% API
-export([start_link/1, init/1, handle_info/2, terminate/2, code_change/3, closeport/0, senddata/1, myfunc/0, handle_call/3]).

%% gen_server to provide communication with C driver

start_link(InitialValue) ->
  gen_server:start_link(
    {local,driver_server},
    driver_server,
    [InitialValue], []).

init(_) ->
  process_flag(trap_exit,true),
  Arguments = case ?PACKET of
                {packet,Count} -> ["packet",integer_to_list(Count)];                           % TODO had some problems with packet another than "1" in C driver
                {line,Count}   -> ["line",integer_to_list(Count)]                              % TODO implement "line" on the driver side
              end,
  Opts = [stderr_to_stdout,binary, use_stdio, exit_status, {args, Arguments}, ?PACKET],
  Port = open_port({spawn_executable,?FILE_C},Opts),
  {ok, [Port]}.

% API
closeport()->gen_server:call(driver_server,closeport).
senddata(Msg)->gen_server:cast(driver_server,{senddata,Msg}).
myfunc()->gen_server:call(driver_server,myfunc).

%% Handle Calls and casts
handle_call(myfunc,From,Data)->
  {reply,"Driver Server's task is to maintain communication with Driver C and to pass received data to Variable Server." ,Data};

handle_call({senddata,Msg},From,Data)->
  lists:foreach(fun(X)->port_command(X,Msg) end,Data),
  {reply,ok,Data};


handle_call(closeport,From,Data)->
lists:foreach(fun(X) -> port_close(X)  end,Data),
{reply,[],[]}.

%% Receive Data from Driver
handle_info({'EXIT',Pid, Reason},Data) when is_pid(Pid) ->               % usual termination (eg by supervisor)
  var_server:adddata({msg,{normal_termination,Reason}}),
  exit(whereis(driver_server),kill),                                     % TODO dont think it is good way to terminate process
  {noreply,Data};
handle_info({'EXIT',Port, Reason},Data) when is_port(Port) ->            %  termination by external drivers death
  var_server:adddata({msg,{driver_termination,Reason}}),
  exit(whereis(driver_server),kill),
  {noreply,Data};
handle_info(Msg,Data)  ->
  % received data from sensor is sent to var_server
  {ok,Msg} = var_server:adddata(Msg),
  {noreply,Data}.


%% Other
terminate(Reason, Data) ->
ok.


code_change(OldVsn, State, Extra) ->
  error(not_implemented).











