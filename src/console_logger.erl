%%%-------------------------------------------------------------------
%%% @author ludwikbukowski
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Jul 2015 4:04 PM
%%%-------------------------------------------------------------------
-module(console_logger).
-author("ludwikbukowski").
-behavoiur(gen_event).

%% API
-export([init/1, terminate/2, code_change/3, handle_info/2, handle_call/2, handle_event/2, handle_call/2]).
init(_) -> {ok,[]}.

handle_info(_,State) ->
    {ok,State}.

code_change(_, State, _) ->
    {ok,State}.

terminate(_, _) ->
    ok.

handle_event({log_error,Data},State) ->
    R= io_lib:format("~p",[Data]),                                                  %% Convert Term to list
    ErrorToPrint = lists:flatten(R),
    io:format("Error occurs:~n~s~n",[ErrorToPrint]),
    {ok,State++[Data]}.

handle_call(getdata,State) ->
    {ok,State,State}.