% @Author: weiyanguang
% @Email:  rondsny@gmail.com
% @Date:   2016-11-18 14:28:06
% @Last Modified by:   weiyanguang
% @Last Modified time: 2016-11-22 16:04:12
% @Desc: 数据库表进程，一个表一个进程

-module(sql_svr).

-behaviour(gen_server).
-include("ts_global.hrl").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/1]).
% 非进程接口
-export([pname/1]).

-record(state, {
    table    = undefined,
    insert   = [],
    internal = 5000,
    loop     = 0,
    ext      = #{}
}).

%% == External functions ==============================================

start_link(Table) ->
    gen_server:start_link({local, pname(Table)}, ?MODULE, [Table], [{spawn_opt, [{fullsweep_after, 10}]}]).

pname(Table) ->
    list_to_atom(lists:concat([?MODULE, "_", Table])).

%% == Behavioural functions ===========================================

init([Table]) ->
    ok = sql_common:create(Table),
    ?TRAC_W(create_succ, Table),
    State =
        #state{
            table  = Table,
            insert = [],
            loop   = 0
        },
    #state{internal=Internal} = State,
    erlang:send_after(Internal, self(), doloop),
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

do_call({lookup, Table, Id}, _From, State) ->
    Reply = sql_common:lookup(Table, Id),
    {reply, Reply, State};
do_call(_Msg, _From, State) ->
    throw({nomatch, do_call, _Msg}),
    {reply, error, State}.


do_cast({delete, Table, Id}, #state{insert=InsList}=State) ->
    InsList2 = lists:keydelete(Id, 2, InsList),
    sql_common:delete(Table, Id),
    {noreply, State#state{insert=InsList2}};
do_cast({insert, Data}, #state{insert=InsList}=State) ->
    InsList2 = InsList ++ Data,
    {noreply, State#state{insert=InsList2}};
do_cast(_Msg, State) ->
    throw({nomatch, do_cast, _Msg}),
    {noreply, State}.


do_info(doloop, #state{internal=Internal, loop=Loop}=State) ->
    erlang:send_after(Internal, self(), doloop),
    State2 = do_flush(State),
    {noreply, State2#state{loop=Loop+1}};
do_info(_Msg, State) ->
    throw({nomatch, do_info, _Msg}),
    {noreply, State}.

% 循环
do_flush(#state{insert=[]}=State) ->
    State;
do_flush(#state{insert=InsList}=State) ->
    InsList2 = lists:usort(InsList), % 只是去重
    case sql_common:insert_list(InsList2) of
        ok ->
            State#state{insert=[]};
        _ ->
            State#state{insert=InsList2}
    end.