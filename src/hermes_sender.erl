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
-define(SLEEP_TIME, 200).     %% 10 Hz
-define(DATA_PORTION, 1).
-define(CM, 2.56).
-define(START, 2).
%% API
-export([start_link/0, init/1]).


start_link() ->
  Pid = spawn(?MODULE, init, [[]]),
  {ok, Pid}.

init(_) ->
%%   Make connection
  loop().

loop() ->
  timer:sleep(?SLEEP_TIME),
  DataList = ?MANAGER_S:remove_data(?DATA_PORTION),
  AdditionalFilter = fun(Msg) ->
    Distance = inch_to_cm(extract_distance(Msg)),
    Date = extract_time(Msg),
    integer_to_list(Distance) ++ " " ++ Date
    end,
  ConsumedList = filter_list(DataList, AdditionalFilter),
  R = io_lib:format("~p",[ConsumedList]),
  FormatedList = lists:flatten(R),
  ?CONNECTION_S:send_data(FormatedList),
  loop().


%% For example linear regression for bunch of measures
consume(Data) ->
  Data.

filter_list(List, AdditionalFun) ->
  Fun = fun(Tuple) ->
    case Tuple of
      {_, {data, Msg}} ->
        AdditionalFun(binary_to_list(Msg));
      _ -> wrong_data
    end
    end,
  lists:map(Fun,  List).

inch_to_cm(Number) ->
  round(Number * ?CM).

extract_distance(Text) ->
  list_to_integer(lists:sublist(Text, 2, 3)).

extract_time(Text) ->
  lists:sublist(Text, 6, 19).
