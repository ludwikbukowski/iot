%%%-------------------------------------------------------------------
%%% @author ludwikbukowski
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Aug 2015 5:29 PM
%%%-------------------------------------------------------------------
-module(hermes_sender).
-author("ludwikbukowski").
-define(MANAGER_S, driver_manager).
-define(CONNECTION_S, connection_server).
-define(SLEEP_TIME, 100).     %% 10 Hz
-define(DATA_PORTION, 1).
-define(ID, "01"). %% ID of raspberrypi
-define(NAME, hermes_sender).
-define(CM, 2.56).
-define(START, 2).
-record(hermes_state, {list , index}).
-behaviour(gen_server).
%% API
-export([start_link/0, init/1, handle_info/2, format_and_send/1, terminate/2]).


start_link() ->
  gen_server:start_link(
    {local,?NAME},
    hermes_sender,
    #hermes_state{list = [eros
      ,deimos, harmonia, phobos, alcippe, pothos, himeros
    ], index = 1}, []).

init(Init) ->
  self() ! send,
  {ok, Init}.

handle_info(send, State = #hermes_state{list = L, index = I}) ->
  erlang:send_after(?SLEEP_TIME, self(), send),
  Connection = lists:nth(I,L),
  NewIndex = (I+1) rem length(L),
  FixedIndex = if NewIndex ==0 ->  1;
                  true -> NewIndex
                  end,
  NewState = State#hermes_state{index = FixedIndex},
  format_and_send(Connection),
  {noreply, NewState};

handle_info(_, State) ->
  {stop, wrong_receive, State}.

terminate(_,_) ->
  ok.


%% Internal functions
format_and_send(Connection) ->
  DataList = ?MANAGER_S:remove_data(?DATA_PORTION),
  case DataList of
    empty_list ->
      ok;
    _ ->
      AdditionalFilter = fun(Msg) ->
        Distance = inch_to_cm(extract_distance(Msg)),
        Date = extract_time(Msg),
        integer_to_list(Distance) ++ " " ++ Date ++ " " ++ ?ID
      end,
      ConsumedList = filter_list(DataList, AdditionalFilter),
      R = io_lib:format("~p",[ConsumedList]),
      FormatedList = lists:flatten(R),
      case application:get_env(iot, sending_fun) of
        {ok, msg} ->
          ?CONNECTION_S:send_data(Connection, FormatedList);
        {ok,_} ->
     ok   % ?CONNECTION_S:publish_content(Connection, FormatedList)
      end
  end.




%% For example linear regression for bunch of measures
consume(Data) ->
  Data.

filter_list(List, AdditionalFun) ->
  Fun = fun(Tuple) ->
    case Tuple of
      {_, {data, Msg}} ->
        AdditionalFun(binary_to_list(Msg));
      _ -> "wrong data"
    end
    end,
  lists:map(Fun,  List).

inch_to_cm(Number) ->
  round(Number * ?CM).

extract_distance(Text) ->
  list_to_integer(lists:sublist(Text, 2, 3)).

extract_time(Text) ->
  lists:sublist(Text, 6, 19).
