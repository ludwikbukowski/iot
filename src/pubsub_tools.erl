%%%-------------------------------------------------------------------
%%% @author ludwikbukowski
%%% @copyright (C) 2015, <Erlang-Solutions>
%%% @doc
%%%
%%% @end
%%% Created : 20. Aug 2015 3:54 PM
%%%-------------------------------------------------------------------
-module(pubsub_tools).
-author("ludwikbukowski").
-define(TITLE, <<"sensor data">>).
-define(ID_NODE(), base64:encode(crypto:strong_rand_bytes(10))).
-define(ID_PUBLISH, <<"someid2">>).
-define(ID_SUBSCRIBE, <<"someid3">>).
-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("exml/include/exml_stream.hrl").

-export([
  create_node/5,
  delete_node_by_owner/3,
  wait_for_stanza_and_match_result_iq/3,
  get_subscription_confirmation_stanza/1,
  get_publish_response_item_id/1,
  get_event_notification_items_list/1,
  get_event_notification_subscription_change/1,
  get_error_info/1,
  get_items_ids/1,
  get_users_and_subscription_states/1,
  get_subscription_list_by_owner/3,
  assert_subscription_matching/4,
  is_publish_response_matching_item_id/2,
  publish_content/6,
  request_subscription_changes_by_owner/5,
  subscribe_by_user/3,
  subscribe_by_users/3,
  unsubscribe_by_user/3,
  unsubscribe_by_users/3
  , prepare_body_for_publish/2, subscribe_by_user/4]).

%% ----------------------------- HELPER and DIAGNOSTIC functions -----------------------
%% Note ------ functions in this section are not stanza generating functions but:
%% - predicates
%% - high level wrappers for escalus specific to pubsub feature

%% Checks superficialy is IQ from server  matches the sent id, there is "result" and sender is correct.
wait_for_stanza_and_match_result_iq(Client, Id, DestinationNode) ->
  ResultStanza = escalus:wait_for_stanza(Client),
  QueryStanza = escalus_stanza:iq_with_type_id_from(<<"result">>, Id, DestinationNode),
  Result = escalus_pred:is_iq_result(QueryStanza, ResultStanza),
  {Result, ResultStanza}.

create_node(User, Client, Id, DestinationNodeAddr, DestinationNodeName) ->
  PubSubCreateIq = escalus_pubsub_stanza:create_node_stanza(User, Id, DestinationNodeAddr, DestinationNodeName),
  escalus_connection:send(Client, PubSubCreateIq).
%%   case wait_for_stanza_and_match_result_iq(Client, Id, DestinationNodeAddr) of
%%     {true, _} ->
%%       created;
%%     {false, Stanza} ->
%%       case escalus_pred:is_error(<<"cancel">>, <<"conflict">>, Stanza) of
%%         true ->
%%           already_exists;
%%         false ->
%%           unknown_error
%%       end;
%%     Other ->
%%       {unknown_bug, Other}
%%   end.



%% publish items witn contents specifying which sample content to use.
publish_content(DestinationTopicName, Id, DestinationNode, User, Client, Data) ->
  %% Prepare body of publish
  Entry = escalus_pubsub_stanza:publish_entry(prepare_body_for_publish(?TITLE, Data)),
  Content = escalus_pubsub_stanza:publish_item(Id, Entry),
  PublishToNodeIq = escalus_pubsub_stanza:publish_node_with_content_stanza(DestinationTopicName, Content),
  Stanza = escalus_pubsub_stanza:iq_with_id(set, Id, DestinationNode, User, [escalus_pubsub_stanza:pubsub_stanza(PublishToNodeIq, ?NS_PUBSUB)]),
  escalus_connection:send(Client, Stanza).
%%   case wait_for_stanza_and_match_result_iq(Client, Id, DestinationNode) of
%%     {true, ReplyStanza} ->
%%       published;
%%     {false, ReplyStanza} ->
%%       {not_published, ReplyStanza};
%%     Other ->
%%       {error, Other}
%%   end.

subscribe_by_user(User, Client, NodeName, NodeAddress) ->
  SubscribeToNode = escalus_pubsub_stanza:subscribe_by_user_stanza(User, ?ID_SUBSCRIBE, NodeName, NodeAddress),
  escalus_connection:send(Client, SubscribeToNode).
%%   case wait_for_stanza_and_match_result_iq(Client, ?ID_SUBSCRIBE, NodeAddress) of
%%     {true, RecvdStanza} ->
%%       true = assert_subscription_matching(RecvdStanza, User, NodeName, false),
%%       subscribed;
%%     {false, Other} ->
%%       {error_while_subscrbtion, Other}
%%   end.






%% returns servers' response stanza, according to 8.8.1.1 (owner case)
get_subscription_list_by_owner(User, NodeName, NodeAddr) ->
  RetrieveSubscriptions = escalus_pubsub_stanza:retrieve_subscriptions_stanza(NodeName),
  Id = <<"subowner">>,
  GetSubscriptionsIq = escalus_pubsub_stanza:iq_with_id(get, Id, NodeAddr, User, [RetrieveSubscriptions]),
  io:format(" REQUEST RetrieveSubscriptionsByByOwner: ~n~n~p~n",[GetSubscriptionsIq]),
  escalus:send(User, GetSubscriptionsIq ),
  wait_for_stanza_and_match_result_iq(User, Id, NodeAddr).


%% generate dummy subscription confirmation from server. Used to test predicate function.
get_subscription_confirmation_stanza(DestinationNode) ->
  Subscription = #xmlel{name = <<"subscription">>, attrs=[{<<"jid">>, <<"alice@localhost">>}]},
  PubSub = escalus_pubsub_stanza:pubsub_stanza([Subscription], ?NS_PUBSUB),
  %% DestinationNode = ?DEST_NODE_ADDR,
  Id = <<"sub1">>,
  PubSubItemIq  =  escalus_pubsub_stanza:iq_with_id(set, Id, DestinationNode, <<"Alice">>,  [PubSub]),
  %%io:format(" ---- ~n~p~n ", [PubSubItemIq]),
  %%B = exml:to_binary(PubSubItemIq),
  %%io:format(" ---- ~n~p~n ", [B]),
  PubSubItemIq.

get_users_and_subscription_states([]) ->
  dict:new();

%% pass subscription list as came from server - users and subscription states will be returned as dictionary where
%% Jid = key , subscriptionState = value}
get_users_and_subscription_states(SubscriptionList) ->
  R = exml_query:attr(SubscriptionList, <<"type">>),
  true = R =:= <<"result">>,
  R1 = exml_query:subelement(SubscriptionList, <<"pubsub">>),
  R2 = exml_query:subelement(R1, <<"subscriptions">>),
  Elems = exml_query:subelements(R2, <<"subscription">>),


  Result = lists:map(fun(Element) ->
    JidFull = exml_query:attr(Element, <<"jid">>),
    SubscrState = exml_query:attr(Element, <<"subscription">>),
    Jid = hd(binary:split(JidFull, <<"/">>)),
    {Jid, SubscrState}
  end, Elems),

  Res = lists:foldl(
    fun(Element, NewDict) ->
      dict:store(element(1, Element), element(2,Element), NewDict)
    end,
    dict:new(),
    Result),

  io:format(" Dump of users subscriptions: ~n~p ", [dict:to_list(Res)]),
  Res.


assert_subscription_matching(SubscrConfirmation, User, _DestinationNode, OptionalAttrCheck) ->
  R1 = exml_query:subelement(SubscrConfirmation, <<"pubsub">>),
  Subscr =  exml_query:subelement(R1, <<"subscription">>),
  Jid = exml_query:attr(Subscr, <<"jid">>), %% required
  Jid =:= User, %%Sender matches?
  if OptionalAttrCheck ->
    Val1 = exml_query:attr(Subscr, <<"node">>) =/= undefined, %% optional
    Val2 = exml_query:attr(Subscr, <<"subid">>) =/= undefined, %% optional
    Val3 = exml_query:attr(Subscr, <<"subscription">>) =/= undefined, %% optional
    Val1 and Val2 and Val3;
  true -> true
  end.

subscribe_by_user(User, NodeName, NodeAddress) ->
  UserName = escalus_utils:get_username(User),
  Id = <<UserName/binary,<<"binsuffix">>/binary>>,
  SubscribeToNodeIq = escalus_pubsub_stanza:subscribe_by_user_stanza(User, Id, NodeName, NodeAddress),
  io:format(" REQUEST SubscribeToNodeIq from user ~p: ~n~p~n",[User, SubscribeToNodeIq]),
  escalus:send(User, SubscribeToNodeIq),
  {true, RecvdStanza} = wait_for_stanza_and_match_result_iq(User, Id, NodeAddress), %%wait for subscr. confirmation
  io:format(" RESPONSE Subscriptions received : ~p~n",[RecvdStanza]),
  true = assert_subscription_matching(RecvdStanza, User, NodeName, true).

subscribe_by_users(UserList, NodeName, NodeAddress) ->
  lists:map(fun(User) -> subscribe_by_user(User, NodeName, NodeAddress) end, UserList).

%% the user unsubscribes himself
unsubscribe_by_user(User, NodeName, NodeAddress) ->
  UserName = escalus_utils:get_username(User),
  Id = <<UserName/binary,<<"binsuffix">>/binary>>,
  UnSubscribeFromNodeIq = escalus_pubsub_stanza:unsubscribe_by_user_stanza(User, Id, NodeName, NodeAddress),
  ct:pal(" REQUEST UnSubscribeFromNodeIq: ~n~n~p~n",[exml:to_binary(UnSubscribeFromNodeIq)]),
  escalus:send(User, UnSubscribeFromNodeIq),
  {true, _RecvdStanza} = wait_for_stanza_and_match_result_iq(User, Id, NodeAddress).

unsubscribe_by_users(UserList, NodeName, NodeAddress) ->
  lists:map(fun(User) -> unsubscribe_by_user(User, NodeName, NodeAddress) end, UserList).

%% returns value of 3rd level node attribute id, see XEP0060 example 100
get_publish_response_item_id(PublishItemConfirmation = #xmlel{name = <<"iq">>}) ->
  <<"result">> = exml_query:attr(PublishItemConfirmation, <<"type">>),
  L1 = exml_query:subelement(PublishItemConfirmation, <<"pubsub">>),
  L2 = exml_query:subelement(L1, <<"publish">>),
  L3 = exml_query:subelement(L2, <<"item">>),
  io:format(" item element: ~n~p~n", [L3]),
  exml_query:attr(L3, <<"id">>).

%% parameters: previously published item id returned by server
is_publish_response_matching_item_id(ItemTestId, PublishItemConfirmation) ->
  ItemTestId =:= get_publish_response_item_id(PublishItemConfirmation).



%% extract the items from the nested wrapper "items" enclosed in message/event
%% according to example 101 of XEP there comes always only ONE item but we
%% deal with the lists anyway for consistency.
get_event_notification_items_list(EventMessage = #xmlel{name = <<"message">>}) ->
  Event = exml_query:subelement(EventMessage, <<"event">>),
  ItemsWrapper = exml_query:subelement(Event, <<"items">>),
  Items = exml_query:subelements(ItemsWrapper, <<"item">>),
  Items.

get_error_info(ErrorIq = #xmlel{name = <<"iq">>}) ->
  Error = exml_query:subelement(ErrorIq, <<"error">>),
  ErrorType = exml_query:attr(Error, <<"type">>),
  #xmlel{children = ErrorEntry} = Error,
  #xmlel{name = ErrorValue} = hd(ErrorEntry),
  {ErrorType, ErrorValue}.

%% according to example 194 , p 8.8.4
%% return information about current subscription state for a given user (e.g after
%% topic owner changed his subscription state.
%% information is returned as tuple (NodeName, SubscriptionStatus)
get_event_notification_subscription_change(EventMessage = #xmlel{name = <<"message">>}) ->
  Event = exml_query:subelement(EventMessage, <<"event">>),
  true =  ?NS_PUBSUB_EVENT =:= exml_query:attr(Event, <<"xmlns">>),
  Subscription = exml_query:subelement(Event, <<"subscription">>),
  SubscrNode = exml_query:attr(Subscription, <<"node">>),
  SubscrJid = exml_query:attr(Subscription, <<"jid">>),
  SubscrStatus = exml_query:attr(Subscription, <<"subscription">>),
  {SubscrNode, SubscrStatus}.

%% returns servers' response stanza, according to 8.8.1.1 (topic owner case!)
%% pass SubscrChangeData as List of tuples {jid, new_subscription_state}
request_subscription_changes_by_owner(User, NodeName, NodeAddr, SubscriptionChangeData, AllowSpoofing) ->
  SubscriptionChangeDataStanza = escalus_pubsub_stanza:get_subscription_change_list_stanza(SubscriptionChangeData),
  SetSubscriptionsStanza = escalus_pubsub_stanza:set_subscriptions_stanza(NodeName, SubscriptionChangeDataStanza),
  Id = <<"subman2">>,
  SetSubscriptionsIq = escalus_pubsub_stanza:iq_with_id(set, Id, NodeAddr, User, [SetSubscriptionsStanza]),
  io:format(" REQUEST ChangeSubscriptionsByByOwner: ~n~n~p~n",[SetSubscriptionsIq]),
  escalus:send(User, SetSubscriptionsIq ),

  case AllowSpoofing of
    true ->  {true, escalus:wait_for_stanza(User)};
    _ -> wait_for_stanza_and_match_result_iq(User, Id, NodeAddr)
  end.

get_items_ids(ItemList) ->
  lists:map(fun(Item) -> exml_query:attr(Item, <<"id">>) end, ItemList).

delete_node_by_owner(User, NodeName, NodeAddr) ->
  DeleteNode = escalus_pubsub_stanza:delete_node_stanza(NodeName),
  Id = <<"delete1">>,
  DeleteNodeIq  =  escalus_pubsub_stanza:iq_with_id(set, Id, NodeAddr, User,  [DeleteNode]),
  io:format(" REQUEST DeleteNodeIq: ~n~n~p~n",[DeleteNodeIq]),
  escalus:send(User, DeleteNodeIq),
  {true, _RecvdStanza} = wait_for_stanza_and_match_result_iq(User, Id, NodeAddr).



prepare_body_for_publish(Title, Data) ->
  [
    #xmlel{name = <<"title">>, children  = [ #xmlcdata{content=[Title]}]},
    #xmlel{name = <<"data">>, children = [ #xmlcdata{content=[Data]}]}
  ].





