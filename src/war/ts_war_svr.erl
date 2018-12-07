% @Author: weiyanguang
% @Email:  rondsny@gmail.com
% @Date:   2017-01-03 14:09:23
% @Last Modified by:   weiyanguang
% @Last Modified time: 2017-01-22 18:08:05
% @Desc: 一个测试的战斗副本

-module(ts_war_svr).

-behaviour(gen_server).
-include("ts_global.hrl").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).
-export([start_static/0]).

-define(LOOP_INTERVAL, 1000).

%% == External functions ==============================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_static() ->
    tserver_sup:start_child(?MODULE, ?MODULE),
    ok.

%% == Behavioural functions ===========================================

init([]) ->
    ?TRAC_W(start_succ, ?MODULE),
    State = #war{},
    erlang:send_after(?LOOP_INTERVAL, self(), doloop),
    {ok, State}.

handle_call(Msg, From, State) ->
    try
        do_call(Msg, From, State)
    catch
        Err:Reason ->
            ?ERROR_MSG("[~p] do_call error! Msg = ~p, From = ~p, Err = ~p, Reason = ~p~n", [?MODULE, Msg, From, Err, Reason]),
            {reply, error, State}
    end.

handle_cast(Msg, State) ->
    try
        do_cast(Msg, State)
    catch
        Err:Reason ->
            ?ERROR_MSG("[~p] do_cast error! Msg = ~p, Err = ~p, Reason = ~p~n", [?MODULE, Msg, Err, Reason]),
            {noreply, State}
    end.

handle_info(Msg, State) ->
    try
        do_info(Msg, State)
    catch
        Err:Reason ->
            ?ERROR_MSG("[~p] do_info error! Msg = ~p, Err = ~p, Reason = ~p~n", [?MODULE, Msg, Err, Reason]),
            {noreply, State}
    end.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% == Internal functions ==============================================

do_call(_Msg, _From, State) ->
    throw({nomatch, do_call, _Msg}),
    {reply, error, State}.

do_cast(_Msg, State) ->
    throw({nomatch, do_cast, _Msg}),
    {noreply, State}.

do_info(doloop, State) ->
    State2 = doloop(State),
    erlang:send_after(?LOOP_INTERVAL, self(), doloop),
    {noreply, State2};
do_info(_Msg, State) ->
    throw({nomatch, do_info, _Msg}),
    {noreply, State}.


%% %%%%%%%%%%%%
doloop(#war{loop=Loop}=State) ->
    % ?TRAC_W(Loop, cm_util:unixtime()),
    State#war{loop=Loop+1}.