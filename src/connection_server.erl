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
-include_lib("escalus/include/escalus.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("iot_lib.hrl").
-export([start_link/1, init/1, handle_call/3, handle_info/2, terminate/2, code_change/3, close_connection/1, get_time/1, save_time/1, send_data/2, register_handler/3,
  unregister_handler/2, create_node/1, subscribe/1, publish_content/2, stop/1, get_bare_jid/0]).
-record(connection_state, {client , handlers, notes}).

start_link(Name) ->
  gen_server:start_link(
    {local, Name},
    connection_server,
    Name, []).

init(Name) ->
  process_flag(trap_exit, true),
  self() ! {connect_to_mongoose, list_to_binary(atom_to_list(Name))},
  {ok, #connection_state{client = not_connected, handlers = dict:new(), notes = dict:new()}}.

% Api

close_connection(Name) ->
  gen_server:call(Name, close_connection).

get_time(Name) ->
   gen_server:call(Name, {get_time,list_to_binary(atom_to_list(Name)) }).

save_time(Name) ->
  gen_server:call(Name, {save_time,list_to_binary(atom_to_list(Name)) }).

send_data(Name,Data) ->
  gen_server:call(Name,{senddata, Data}).

%% All handlers are functions with two parameters. First is incomming Stanza and the second is Server's State. Handler has to return NewState (or pass the old one)
register_handler(Name,HandlerName, Handler) ->
  gen_server:call(Name, {register_handler, HandlerName, Handler}).

unregister_handler(Name,HandlerName) ->
  gen_server:call(Name, {unregister_handler, HandlerName}).

create_node(Name) ->
  gen_server:call(Name, {createnode, list_to_binary(atom_to_list(Name)) , ?NODE_NAME}).

subscribe(Name) ->
  gen_server:call(Name, {subscribe,list_to_binary(atom_to_list(Name)) }).

publish_content(Name,Content) ->
  gen_server:call(Name, {publishcontent,list_to_binary(atom_to_list(Name)) , Content}).

stop(Name) ->
  gen_server:call(Name, stop).


%% Handle Calls and casts
handle_call(stop,_ , State) ->
  {stop, stopped_by_client, State};

handle_call({createnode, Resource, NodeName}, _, #connection_state{client = Client, handlers = Handlers, notes = Notes} = State) ->
  FullJid = get_full_jid(Resource),
  Id = ?ID_GENERATE(),
  pubsub_tools:create_node(FullJid, Client, Id, ?DEST_ADDR, NodeName),
  {reply, ok, State};

handle_call({publishcontent,Resource, Content}, _, State = #connection_state{client = Client, handlers = Handlers, notes = Notes}) ->
  FullJid = get_full_jid(Resource),
  Id = ?ID_GENERATE(),
  NewNotes = dict:append(Id, createnode, Notes),
  H = publish_content_handler(Id),
  NewHandlers = register_handler_fun(Id, H, Handlers),
  pubsub_tools:publish_content(?NODE_NAME, Id, ?DEST_ADDR, FullJid, Client, Content),
  {reply, ok, State#connection_state{handlers = NewHandlers, notes = NewNotes}};


handle_call({subscribe,Resource }, _, State = #connection_state{client = Client}) ->
  FullJid = get_full_jid(Resource),
  pubsub_tools:subscribe_by_user(FullJid, Client, ?NODE_NAME, ?DEST_ADDR),
  {reply, ok, State};

handle_call({senddata, Data}, _, #connection_state{client = Client} = State) ->
  escalus_connection:send(Client, escalus_stanza:chat_to(?RECEIVER, Data)),
  {reply, sent, State};



handle_call(close_connection, _, State = #connection_state{client = Client}) ->
  escalus_connection:stop(Client),
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

handle_call({get_time,Resource}, _, State = #connection_state{client = Client}) ->
  case time_request(Client, Resource) of
    {time, Time} ->
      {reply, Time ,State};
    timeout ->
      {stop, connection_timeout, State}
  end;


handle_call({save_time,Resource}, _, State = #connection_state{client = Client}) ->
  case time_request(Client, Resource) of
    {time, Time} ->
      {Utc, Tzo} = Time,
      os_functions:change_time(Tzo, Utc),
      {reply, {changed, Utc, Tzo}, State};
    timeout ->
      {reply, connection_timeout, State}
  end.

handle_info({connect_to_mongoose, Resource}, State) ->
  io:format("Resource ~p zaczyna sie laczyc~n",[Resource]),
  {ok, Username} = application:get_env(iot,username),
  {ok, Password} = application:get_env(iot,password),
  {ok, Domain} = application:get_env(iot,domain),
  {ok, Host} = application:get_env(iot,host),
  Cfg = user_spec(Username, Domain, Host, Password, Resource),
  MergedConf = merge_props([], Cfg),
  case escalus_connection:start(MergedConf) of
    {ok, Client, _, _} ->
      send_presence_available(Client),
      receive
        {stanza, _, Stanza} -> case escalus_pred:is_presence(Stanza) of
                                 true ->
                                   {noreply, State#connection_state{client = Client}};
                                 Some ->
                                   {stop, {connection_wrong_receive, Some, Stanza}, State}
                               end
      after ?TIMEOUT_MONGOOSE ->
        {stop, connection_timeout, State}
      end;
    Other ->
      {stop, {cannot_connect, Other}, State}
        end;

handle_info({stanza, _, Stanza}, State = #connection_state{}) ->
  NewState = handle_stanza(Stanza, State),                      %%I Should restore it somewhere
  {noreply, NewState}.



%% Other

terminate(_, #connection_state{client = Client = #client{}}) ->
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

time_request(Client, Resource) ->
  {ok, Host} = application:get_env(iot, host),
  Stanza = ?TIME_STANZA(get_full_jid(Resource), Host),
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
publish_content_handler( Id) ->
  fun(Stanza, State = #connection_state{notes = Notes}) ->
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
%%             io:format("all is ok!~n"),
            dict:erase(Id, Notes),
            State
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




