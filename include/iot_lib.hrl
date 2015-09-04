-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").
-define(TIME_STANZA(From, To), #xmlel{name = <<"iq">>,
attrs = [{<<"from">>,From},
{<<"type">>, <<"get">>},
{<<"id">>, <<"time_1">>},{<<"to">>,To}],
children = #xmlel{name = <<"time">>,
attrs = [{<<"xmlns">>, ?NS_TIME}]}}).