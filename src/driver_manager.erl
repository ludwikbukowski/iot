%%%-------------------------------------------------------------------
%%% @author ludwikbukowski
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Jul 2015 10:03 AM
%%%-------------------------------------------------------------------
-module(driver_manager).
-author("ludwikbukowski").
-behaviour(gen_server).
-export([start_link/1, init/1, handle_call/3, terminate/2, handle_info/2, code_change/3]).
-export([add_data/1, get_data/0, open_port/2, close_port/1, connect_to_mongoose/0, connect_to_mongoose/0, remove_data/1, open_sender/0, close_sender/0]).
-define(NAME, driver_manager).
-define(CONNECTION_SERVER, connection_server).
-define(SENDER, hermes_sender).
-export_type([sensor_type/0, close_type/0]).
-type sensor_type() :: 'uart' | atom().
-type close_type() :: wrong_close | safe_close.
-type server_name() :: atom().
-define(ERROR_LOGGER,my_error_logger).
%%  Driver Manager's task is to provide console communication, restore data received from sensors and manage sensor drivers.

start_link(InitialValue) ->
  gen_server:start_link(
    {local, ?NAME},
    ?NAME,
    [InitialValue], []).

init(_) ->
  {ok, []}.



%% Api
-spec open_port(server_name(), sensor_type()) -> {reply, {ok,pid()}, any()} | {reply, unknown_option, any()}.
open_port(Name, Option) ->
  gen_server:call(?NAME, {openport, Name, os_functions:get_driver_exec_file(), Option}).

-spec close_port(atom()) -> {reply, {close_type(), any()}, any()}.
close_port(Name) ->
  gen_server:call(?NAME, {closeport, Name}).

-spec open_sender() -> {ok, pid()}.
open_sender() ->
  gen_server:call(?NAME, opensender).

-spec close_sender() -> 'safe_close'.
close_sender() ->
  gen_server:call(?NAME, closesender).

-spec add_data(any()) -> {reply, {ok, any()}, any()}.
add_data(Msg) ->
  gen_server:call(?NAME, {msg, Msg}).

-spec get_data()->{reply, any(), any()}.
get_data() ->
  gen_server:call(?NAME, getdata).

-spec remove_data(integer())->{reply, list(), any()}.
remove_data(Last) ->
  gen_server:call(?NAME, {remove, Last}).

-spec connect_to_mongoose() -> 'connected'.
connect_to_mongoose() ->
  gen_server:call(?NAME, connect).






%% Handle Calls and casts
handle_call({msg,Msg},_,Data) ->
  {reply,{ok,Msg},[Msg] ++ Data};

handle_call({openport, Name, File, uart},_,Data) ->
  {ok,Pid}= supervisor:start_child(
    apollo_supervisor,
    {Name,{driver_server,start_link,[Name, File]}, transient, 5000, worker, [driver_server]}),
  {reply,{ok,Pid},Data};

handle_call(connect,_,Data) ->
  _ = ?CONNECTION_SERVER:connect(),
  {reply,connected,Data};

handle_call(opensender,_,Data) ->
  {ok,Pid}= supervisor:start_child(
    zeus_supervisor,
    {?SENDER,{?SENDER,start_link,[]}, transient, 5000, worker, [?SENDER]}),
  {reply,{ok,Pid},Data};

handle_call(closesender,_,Data) ->
  ok = supervisor:terminate_child(zeus_supervisor,?SENDER),
  ok = supervisor:delete_child(zeus_supervisor,?SENDER),
  {reply, safe_close, Data};

handle_call({openport,_,_,_},_,Data) ->
  {reply, unknown_option, Data};

handle_call({closeport,Name},_,Data) ->
  try driver_server:close_port(Name) of
    Reply -> {reply, {wrong_close, {replied, Reply}}, Data}
    catch
    exit:Exit ->
      ok = supervisor:delete_child(apollo_supervisor,Name),
      {reply, {safe_close, Exit}, Data};
    Other:Reason -> {reply, {wrong_close, {Other, Reason}}}
  end;

handle_call(getdata,_,Data) ->
  {reply,Data,Data};

handle_call({remove, _},_,[]) ->
  {reply,empty_list,[]};
handle_call({remove, Last},_,[]) when Last =< 0->
  {reply,wrong_number,[]};
handle_call({remove, Last},_,Data) when length(Data) < Last ->
  {reply,too_less_elements,Data};
handle_call({remove, Last},_,Data) ->
  {Firsts, Lasts} = lists:split(Last, Data),
  {reply,Firsts, []};

handle_call(_,From,Data) ->
  {reply,From,Data}.


%% Other
terminate(_, _) ->
  ok.

handle_info(_, _) ->
  error(not_implemented).

code_change(_, _, _) ->
  error(not_implemented).