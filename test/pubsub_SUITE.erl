
%%%-------------------------------------------------------------------
%%% @author ludwikbukowski
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Aug 2015 4:29 PM
%%%-------------------------------------------------------------------
-module(pubsub_SUITE).
-author("ludwikbukowski").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("escalus/include/escalus.hrl").
-define(CONNECTION_S,ares_server).
%% API
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_group/2, end_per_group/2]).
groups()->[{pubsub,[sequence],[ ]].
all()->[{group, pubsub}].


%% Init
init_per_suite(Config)->
  connection_server:start_link(),
  Config.

end_per_suite(_)->
  ok.

init_per_group(pubsub,Config)->
  Config.


end_per_group(pubsub,_)->
  ok.

crete_node_stanza_test(Config) ->
  Ret = connection_server:create_node(),
  case Ret of
    created ->
      ok;
    already_exists ->
      ok;
    Other ->
      erlang:error(Other)
  end.

publish_content_test(Config) ->

create_node_test(Config) ->
  created = ?CONNECTION_S:create_node().


