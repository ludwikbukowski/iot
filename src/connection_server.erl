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
-define(ERROR_LOGGER,my_error_logger).
-define(TIMER,timer).
-define(TIMEOUT,3000).
-define(TIMEOUT_MONGOOSE,4000).
-define(RECEIVER,<<"test@iot.net">>).
-define(NODE_NAME,<<"pubsub.iot.net">>).
-define(DEST_ADDR,<<"pubsub.iot.net">>).
-define(ID_GENERATE(), base64:encode(crypto:strong_rand_bytes(10))).
-define(CONNECTION_LIST, [<<"connection1">>, <<"connection2">>, <<"connection3">>,
                           <<"connection4">>, <<"connection5">>]).
-include_lib("escalus/include/escalus.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("iot_lib.hrl").
-export([start_link/0, init/1, close_connection/0, get_time/0, save_time/0, send_data/1, register_handler/2, unregister_handler/1, create_node/0,
         subscribe/0, publish_content/1, stop/0, handle_call/3, handle_info/2, terminate/2, code_change/3, get_bare_jid/0, get_full_jid/1]).

-record(connection_state, {client ,publisher_index ,handlers, notes}).

start_link() ->
  gen_server:start_link(
    {local, ares_server},
    connection_server,
    [], []).

init(_) ->
  process_flag(trap_exit, true),
  {ok, Username} = application:get_env(iot,username),
  {ok, Password} = application:get_env(iot,password),
  {ok, Domain} = application:get_env(iot,domain),
  {ok, Host} = application:get_env(iot,host),
  State = #connection_state{client =  not_connected, publisher_index =  0, handlers = dict:new(), notes = dict:new()},
  Fun = fun(Resource) ->
    Cfg = user_spec(Username, Domain, Host, Password, Resource),
    MergedConf = merge_props([], Cfg),
    case escalus_connection:start(MergedConf) of
      {ok, Client, _, _} ->
        send_presence_available(Client),
        {true, Client};
      Other ->
        {false,{cannot_connect, Other}}
    end
  end,
  try
  ConnectionCreatedList = lists:map(Fun, ?CONNECTION_LIST),
  ConnectionErrors = lists:filtermap(fun({false, Reason}) -> {true, Reason};(_) -> false end, ConnectionCreatedList),
  if length(ConnectionErrors) > 0 ->
    {stop, ConnectionErrors};
    true ->
      ConnectionList = lists:filtermap(fun({false, _}) -> false;(ConnectionCreated) -> ConnectionCreated end, ConnectionCreatedList),
      {ok, State#connection_state{client = ConnectionList}}
  end
    catch
      Error:Reason ->
        {stop, {cannot_connect, Error, Reason}}
    end.

% Api

close_connection() ->
  gen_server:call(ares_server, close_connection).

get_time() ->
   gen_server:call(ares_server, get_time ).

save_time() ->
  gen_server:call(ares_server, save_time ).

send_data(Data) ->
  gen_server:call(ares_server,{senddata, Data}).

%% All handlers are functions with two parameters. First is incomming Stanza and the second is Server's State. Handler has to return NewState (or pass the old one)
register_handler(HandlerName, Handler) ->
  gen_server:call(ares_server, {register_handler, HandlerName, Handler}).

unregister_handler(HandlerName) ->
  gen_server:call(ares_server, {unregister_handler, HandlerName}).

create_node() ->
  gen_server:call(ares_server, {createnode , ?NODE_NAME}).

subscribe() ->
  gen_server:call(ares_server, subscribe).

publish_content(Content) ->
  gen_server:call(ares_server, {publishcontent, Content}).

stop() ->
  gen_server:call(ares_server, stop).


%% Handle Calls and casts
handle_call(stop,_ , State) ->
  {stop, stopped_by_client, State};

handle_call({createnode, NodeName}, _, #connection_state{client = [Client | _], handlers = _, notes = _} = State) ->
  FullJid = get_bare_jid(),
  Id = ?ID_GENERATE(),
  Ret = pubsub_tools:create_node(FullJid, Client, Id, ?DEST_ADDR, NodeName),
  {reply, Ret, State};

handle_call({publishcontent, Content}, _, State = #connection_state{client = ClientList, publisher_index =  I, handlers = Handlers, notes = Notes}) ->
  FullJid = get_bare_jid(),
  Id = ?ID_GENERATE(),
  NewIndex  = (I+1) rem length(ClientList),
  FixedIndex = if NewIndex ==0 ->  1;
    true -> NewIndex
  end,
  Client = lists:nth(FixedIndex,ClientList),
  NewNotes = dict:append(Id, publishcontent, Notes),
  H = publish_content_handler(Id),
  NewHandlers = register_handler_fun(Id, H, Handlers),
  pubsub_tools:publish_content(?NODE_NAME, Id, ?DEST_ADDR, FullJid, Client, Content),
  NewState = State#connection_state{publisher_index = FixedIndex
                                    ,handlers = NewHandlers, notes = NewNotes
  },
  {reply, ok, NewState};


handle_call(subscribe, _, State = #connection_state{client = [Client | _]}) ->
  FullJid = get_bare_jid(),
  pubsub_tools:subscribe_by_user(FullJid, Client, ?NODE_NAME, ?DEST_ADDR),
  {reply, ok, State};

handle_call({senddata, Data}, _, #connection_state{client = [Client | _]} = State) ->
  escalus_connection:send(Client, escalus_stanza:chat_to(?RECEIVER, Data)),
  {reply, sent, State};



handle_call(close_connection, _, State = #connection_state{client = ClientList}) ->
  lists:foreach(fun escalus_connection:stop/1, ClientList),
  {reply, closed_connection ,State};

handle_call({register_handler, HandlerName, Handler}, _, State = #connection_state{handlers = Handlers}) ->
  NewHandlers = register_handler_fun(HandlerName, Handler, Handlers),
  {reply, registered , State#connection_state {handlers = NewHandlers}};

handle_call({unregister_handler, HandlerName}, _, State = #connection_state{handlers = Handlers}) ->
  case unregister_handler_fun(HandlerName, Handlers) of
    not_found ->
      {reply, not_found, Handlers};
    NewHandlers ->
      {reply, unregistered, State#connection_state{handlers = NewHandlers}}
  end;

handle_call(get_time, _, State = #connection_state{client = [Client | _]}) ->
  case time_request(Client) of
    {time, Time} ->
      {reply, Time ,State};
    timeout ->
      {stop, connection_timeout, State}
  end;


handle_call(save_time, _, State = #connection_state{client = [Client | _]}) ->
  case time_request(Client) of
    {time, Time} ->
      {Utc, Tzo} = Time,
      os_functions:change_time(Tzo, Utc),
      {reply, {changed, Utc, Tzo}, State};
    timeout ->
      {reply, connection_timeout, State}
  end.


handle_info({stanza, _, Stanza}, State = #connection_state{}) ->
  NewState = handle_stanza(Stanza, State),                      %%I Should restore it somewhere
  {noreply, NewState}.



%% Other

terminate(_, #connection_state{client = [Client = #client{} | _]}) ->
  try   escalus_connection:stop(Client) of
    _ -> ok
  catch
    exit:_ ->ok;
    _:_ -> ok
  end,
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

handle_stanza(Stanza, State=#connection_state{client = Client, handlers = Handlers, notes = Notes}) ->
  dict:fold(fun(_, Handler, Acc) -> (hd(Handler))(Stanza, Acc) end, State, Handlers).

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
  {ok, Host} = application:get_env(iot, host),
  Stanza = ?TIME_STANZA(get_bare_jid(), Host),
  escalus_connection:send(Client, Stanza),
  receive
    {stanza, _, Reply} ->
      ResponseTime = time_from_stanza(Reply),
      {time, ResponseTime}
  after
    ?TIMEOUT ->
      timeout
  end.

get_bare_jid() ->
  {ok, Username}  = application:get_env(iot, username),
  {ok, Domain}  = application:get_env(iot, domain),
  HalfJid = <<Username/binary, <<"@">>/binary>>,
  <<HalfJid/binary,Domain/binary>>.

get_full_jid(Resource) ->
  {ok, Username}  = application:get_env(iot, username),
  {ok, Domain}  = application:get_env(iot, domain),
  HalfJid = <<Username/binary, <<"@">>/binary>>,
  NoResource = <<HalfJid/binary,Domain/binary>>,
  HalfResource = <<NoResource/binary,<<"/">>/binary>>,
  <<HalfResource/binary, Resource/binary>>.


register_handler_fun(HandlerName, Handler, Handlers) ->
  dict:append(HandlerName, Handler, Handlers).

unregister_handler_fun(HandlerName, Handlers) ->
  case dict:find(HandlerName, Handlers) of
    error ->
      not_found;
    _ ->
      dict:erase(HandlerName, Handlers)
  end.

%% Special Handlers for Pubsub server responses

% Check if response is good
publish_content_handler(Id) ->
  fun(Stanza, State = #connection_state{handlers= Handlers,notes = Notes}) ->
    case escalus_pred:is_iq_result(Stanza) of
      true ->
        case Stanza of
          #xmlel{attrs = Attrs} ->
        ReceivedId = proplists:get_value(<<"id">>, Attrs),
    case ReceivedId of
     Id ->
        case dict:find(Id, Notes) of
          error ->
            my_error_logger:log_error("I got result without request!~n"),
          State;
          _ ->
%%              io:format("all is ok!~n"),
            NewNotes = dict:erase(Id, Notes),
            NewHandlers = dict:erase(Id, Handlers),
            State#connection_state{notes = NewNotes, handlers = NewHandlers}
        end;
      _ ->
%%         io:format("wrong Id:~p and stanza ~p~n",[Id,Stanza]),
        State
    end;
          _ ->
            my_error_logger:log_error("Got stanza with no ID!~n"),
            State
          end;
    _ ->
      State
    end
end.




