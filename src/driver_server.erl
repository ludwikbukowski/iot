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
-define(FILE_C,"./bin/driver").
-define(PACKET,{packet,1}).
%% API
-export([start_link/1, init/1, handle_info/2, terminate/2, code_change/3, handle_cast/2, closeport/0, senddata/1]).

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
closeport()->gen_server:cast(driver_server,closeport).
senddata(Msg)->gen_server:cast(driver_server,{senddata,Msg}).

%% Handle Calls and casts
handle_cast({senddata,Msg},Data)->
  lists:foreach(fun(X)->port_command(X,Msg) end,Data),
  {noreply,Data};


handle_cast(closeport,Data)->
lists:foreach(fun(X) -> port_close(X)  end,Data),
{noreply,[]}
.

%% Receive Data from Driver
handle_info({'EXIT',Pid, Reason},Data) when is_pid(Pid) ->               % usual termination (eg by supervisor)
  gen_server:call(var_server,{msg,{normal_termination,Reason}}),
  exit(whereis(driver_server),kill),
  {noreply,Data};
handle_info({'EXIT',Port, Reason},Data) when is_port(Port) ->            %  termination by external drivers death
  gen_server:call(var_server,{msg,{driver_termination,Reason}}),
  exit(whereis(driver_server),kill),
  {noreply,Data};
handle_info(Msg,Data)  ->                                                % received data from sensor is sent to var_server
  {ok,Msg} = gen_server:call(var_server,{msg,Msg}),
  {noreply,Data}.


%% Other
terminate(Reason, Data) ->
ok.


code_change(OldVsn, State, Extra) ->
  error(not_implemented).











