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
-export([init/1, terminate/2, code_change/3, handle_info/2, handle_call/2, handle_event/2]).
init(Args)-> {ok,[]}.

handle_call(_,State) -> {ok,ok,State}.

handle_info(_,State) ->{ok,State}.

code_change(Old, State, Extra) ->{ok,State}.

terminate(Reason, Satte) -> ok.

handle_event({log_error,Data},State) ->
    io:format("Error occurs:~n~s~n",[Data]),
    {ok,State}.