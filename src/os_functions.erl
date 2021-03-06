%%%-------------------------------------------------------------------
%%% @author ludwikbukowski
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Aug 2015 9:57 AM
%%%-------------------------------------------------------------------
-module(os_functions).
-author("ludwikbukowski").
-define(DATE_LENGTH, 20).
-type os() :: 'linux' | 'darwin'.
-export([change_time/2, detect_os/0, chceck_data_format/1, get_driver_exec_file/0]).


%% API
-spec detect_os() -> os().
detect_os() ->
  Uname = os:cmd("uname"),
  case Uname of
    "Linux\n" ->
      linux;
    "Darwin\n" ->
      darwin;
    _ ->
      unknown
  end.

% Format of date for raspberrypi should be 'DD/MM/YYYY HH:MM:SS' Tzo and '+/- HH:MM' for Utc
% ``shell``: udo date -s 'YYY/MM/DD HH:MM:SS'; date -d '+HH hours +MM minutes
change_time(Tzo, Utc) ->
  FormatedDate = reformat_tzo(Tzo),
      case os_functions:detect_os() of
        linux ->
          Prefix = "sudo date -s ",
          ChangeDate = Prefix ++ add_quotes(FormatedDate),
          FormatedUtc = reformat_utc(Utc),
          Hours = lists:sublist(FormatedUtc, 3),
          Minutes = [lists:nth(1, FormatedUtc)] ++ lists:sublist(FormatedUtc, 5,2),
          Command = ChangeDate,
          os:cmd(Command),
          Offset = os:cmd("sudo date -d '" ++ Hours ++ " hours " ++ Minutes ++ " minutes'"),
          os:cmd("sudo date -s '"++Offset++"'");                                                            %% At this time have no better idea how to do it another way
        darwin ->
          io:format("*******************~n"),
          io:format("You cannot change time on Darwin, so I will just pretend that time is changed.~n"),      %% I know its stupid...
          io:format("*******************~n");
  _ ->
          erlang:error(wrong_os)
  end.

% Internal function
add_quotes(String) ->
  ("'" ++ String) ++ "'".

% before format date is <<"YYYY-MM-DDTHH:MM:SSZ">>
reformat_tzo(Tzo) ->
  Date = binary_to_list(Tzo),
  case length(Date) of
    ?DATE_LENGTH ->
      SlashedDate = lists:foldl(fun(X,Acc) -> if [X] == "-" -> Acc++"/"; true -> Acc++[X] end end, [], lists:sublist(Date,10)),
      Time = lists:sublist(Date, 12, 8),
      SlashedDate ++ " " ++Time;
    _ ->
  erlang:error(wrong_date_format)
  end.

% Format of utc was <<"HH:MM">> and now is just "HH:MM"
reformat_utc(Utc) ->
  binary_to_list(Utc).

%% Used to check if data from sensor is in right format
chceck_data_format(Data) ->
  24 = length(Data),
  list_to_integer(lists:sublist(Data, 2,3)).

get_driver_exec_file()->
  case application:get_env(iot,mocked) of
    {ok,true} -> code:priv_dir(iot)++"/driver_mock";
    _->code:priv_dir(iot)++"/driver"
  end.



