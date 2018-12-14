% @Author: weiyanguang
% @Email:  rondsny@gmail.com
% @Date:   2017-01-10 10:50:46
% @Last Modified by:   weiyanguang
% @Last Modified time: 2018-12-11 14:59:33
% @Desc: 定时器

-module(cm_timer).
-behaviour(gen_server).
-include("ts_global.hrl").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).

%% == External functions ==============================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% == Behavioural functions ===========================================

init([]) ->

    {ok, {}}.


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


do_info(start, State) ->
    loop(State),
    {noreply, State};
do_info(_Msg, State) ->
    throw({nomatch, do_info, _Msg}),
    {noreply, State}.


loop(State) ->
    receive     %% 进程一般时候都是在等待
        add ->
            todo_add,
            loop(State);
        del ->
            todo_del,
            loop(State)
    after
        1000 ->
            send_loop_msg,
            ?TRAC_W(doloop, 1000),
            loop(State)
    end.