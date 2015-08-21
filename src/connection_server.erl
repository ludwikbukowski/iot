%%%-------------------------------------------------------------------
%%% @author ludwikbukowski
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Aug 2015 10:35 AM
%%%-------------------------------------------------------------------

-module(connection_server).
-author("ludwikbukowski").
-behavoiur(gen_server).
-define(NAME, connection_server).
-define(ERROR_LOGGER,my_error_logger).
-define(TIMER,timer).
-define(TIMEOUT,3000).
-define(RECEIVER,<<"mac@iot.net">>).
-include_lib("escalus/include/escalus.hrl").
-include_lib("include/iot_lib.hrl").
-export([start_link/1, init/1, handle_call/3, handle_info/2, terminate/2, code_change/3, stop/0]).
-export([connect/0, register_handler/2, unregister_handler/1, get_time/0, save_time/0, send_data/1, close_connection/0, create_node/1]).
-record(connection_state, {client , handlers}).

start_link(_) ->
  gen_server:start_link(
    {local, ?NAME},
    connection_server,
    [], []).

init(_) ->
  process_flag(trap_exit, true),
  {ok, #connection_state{client = not_connected, handlers = dict:new()}}.

% Api
connect() ->
  {ok, Username} = application:get_env(iot,username),
  {ok, Password} = application:get_env(iot,password),
  {ok, Domain} = application:get_env(iot,domain),
  {ok, Host} = application:get_env(iot,host),
  {ok, Resource} = application:get_env(iot,resource),
  gen_server:call(?NAME, {connect, Username, Password, Domain, Host, Resource}).

close_connection() ->
  gen_server:call(?NAME, close_connection).

get_time() ->
   gen_server:call(?NAME, get_time).

save_time() ->
  gen_server:call(?NAME, save_time).

send_data(Data) ->
  gen_server:call(?NAME,{senddata, Data}).

register_handler(HandlerName, Handler) ->
  gen_server:call(?NAME, {register_handler, HandlerName, Handler}).

unregister_handler(HandlerName) ->
  gen_server:call(?NAME, {unregister_handler, HandlerName}).

create_node(NodeName) ->
  gen_server:call(?NAME, {createnode, NodeName}).

stop() ->
  gen_server:call(?NAME, stop).


%% Handle Calls and casts
handle_call(stop,_ , State) ->
  {stop, stopped_by_client, State};

handle_call({createnode, NodeName}, _, #connection_state{client = Client} = State) ->
  {ok, Host}  = application:get_env(iot, host),
  {ok, Username}  = application:get_env(iot, username),
  {true, _RecvdStanza} = pubsub_tools:create_node(Username,
    Host,
    NodeName),
  {reply, _RecvdStanza,State};

handle_call({senddata, Data}, _, #connection_state{client = Client} = State) ->
  io:format("Sending data ~p~n",[Data]),
  escalus_connection:send(Client, escalus_stanza:chat_to(?RECEIVER, Data)),
  {reply, sent, State};

handle_call({connect, Username, Password, Domain, Host, Resource},_,State) ->
  Cfg = user_spec(Username, Domain, Host, Password, Resource),
  MergedConf = merge_props([], Cfg),
  case escalus_connection:start(MergedConf) of
    {ok, Client, _, _} ->
      send_presence_available(Client),
      receive
        {stanza, _, Stanza} -> case escalus_pred:is_presence(Stanza) of
                 true ->
                   {reply, State#connection_state{client = Client}, State#connection_state{client = Client}};
                 _ ->
                   {stop, {connection_wrong_receive, Stanza}, State}
               end
        end;
    _ ->
      {stop, cannot_connect, State}
  end;

handle_call(close_connection, _, State = #connection_state{client = Client}) ->
  escalus_connection:stop(Client),
  {reply, closed_connection ,State};

handle_call({register_handler, HandlerName, Handler}, _, State = #connection_state{handlers = Handlers}) ->
  NewHandlers = dict:append(HandlerName, Handler, Handlers),
  {reply, registered , State#connection_state {handlers = NewHandlers}};

handle_call({unregister_handler, HandlerName}, _, State = #connection_state{handlers = Handlers}) ->
  case dict:find(HandlerName, Handlers) of
    error ->
    {reply, not_found, State};
    _ ->
      NewHandlers = dict:erase(HandlerName, Handlers),
      {reply, unregistered, State#connection_state{handlers = NewHandlers} }
  end;

handle_call(get_time, _, State = #connection_state{client = Client}) ->
  case time_request(Client) of
    {time, Time} ->
      {reply, Time ,State};
    timeout ->
      {stop, connection_timeout, State}
  end;


handle_call(save_time, _, State = #connection_state{client = Client}) ->
  case time_request(Client) of
    {time, Time} ->
      {Utc, Tzo} = Time,
      os_functions:change_time(Tzo, Utc),
      {reply, {changed, Utc, Tzo}, State};
    timeout ->
      {reply, connection_timeout, State}
  end.

handle_info({stanza, _, Stanza}, State = #connection_state{handlers = Handlers}) ->
  ReturnedAcc = handle_stanza(Stanza, Handlers),                      %%I Should restore it somewhere
  {noreply, State}.

%% Other

terminate(_, #connection_state{client = Client = #client{}}) ->
  escalus_connection:stop(Client),
  ok;

terminate(_, _) ->
  ok.

code_change(_, _, _) ->
  error(not_implemented).




%% Internal functions
user_spec(Username, Domain, Host ,Password, Resource) ->
  [ {username, Username},
    {server, Domain},
    {host, Host},
    {password, Password},
    {carbons, false},
    {stream_management, false},
    {resource, Resource}
  ].


merge_props(New, Old) ->
  lists:foldl(fun({K, _}=El, Acc) ->
    lists:keystore(K, 1, Acc, El)
  end, Old, New).

send_presence_available(Client) ->
  Pres = escalus_stanza:presence(<<"available">>),
  escalus_connection:send(Client, Pres).

handle_stanza(Stanza, Handlers) ->
  dict:fold(fun(_, Handler, Acc) -> [(hd(Handler))(Stanza)] ++ Acc end, [], Handlers).

time_from_stanza(Stanza = #xmlel{name = <<"iq">>, attrs = _, children = [Child]}) ->
    escalus_pred:is_iq_result(Stanza),
    escalus_pred:is_iq_with_ns(?NS_TIME,Stanza),
    case Child of
    #xmlel{name = <<"time">>, attrs = _, children = Times} ->
      case Times of
      [#xmlel{name = <<"tzo">>, attrs = _, children = [#xmlcdata{content = Tzo}]},
        #xmlel{name = <<"utc">>, attrs = _, children = [#xmlcdata{content = Utc}]} ] ->
        {Tzo, Utc};
        _ -> no_timezone
      end;
        _ -> wrong_stanza
    end;

% Received some other stanza than expected, so im waiting for MY time stanza. Looping and flushing all stanzas other than mine
time_from_stanza(Some) ->
  receive
    {stanza, _, NewStanza} ->
      time_from_stanza(NewStanza);
    _ ->
      erlang:exit({wrong_received_stanza, Some})
  after ?TIMEOUT ->
    erlang:exit(timeout)
  end.

time_request(Client) ->
  {ok, Username} = application:get_env(iot,username),
  {ok, Domain} = application:get_env(iot,domain),
  {ok, Host} = application:get_env(iot,host),
  HalfJid = <<Username/binary, <<"@">>/binary>>,
  FullJid = <<HalfJid/binary,Domain/binary>>,
  Stanza = ?TIME_STANZA(FullJid, Host),
  escalus:send(Client, Stanza),
  receive
    {stanza, _, Reply} ->
      ResponseTime = time_from_stanza(Reply),
      {time, ResponseTime}
  after
    ?TIMEOUT ->
      timeout
  end.



