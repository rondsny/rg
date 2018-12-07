% @Author: weiyanguang
% @Email:  rondsny@gmail.com
% @Date:   2016-11-15 14:41:24
% @Last Modified by:   weiyanguang
% @Last Modified time: 2016-11-17 18:43:49
% @Desc: 每一个网络socket都会出现一个net_protocol进程

-module(net_protocol).
-behaviour(gen_server).
-behaviour(ranch_protocol).
-include("ts_global.hrl").

-export([start_link/4]).

-export([init/1]).
-export([init/4]).
-export([handle_info/2]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([code_change/3]).
-export([terminate/2]).

-export([send_rec/2]).

start_link(Ref, Socket, Transport, Opts) ->
    Pid = proc_lib:spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
    gen_server:cast(net_common_svr, {login, Pid}),
    {ok, Pid}.

init(_) -> {ok, undefined}.

init(Ref, Socket, Transport, _Opts = []) ->
    % ok = proc_lib:init_ack({ok, self()}),  % 临时注释，此处目前会引起崩溃
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, true}]),

    {ok, {{I1,I2,I3,I4}=Ip, Port}} = inet:peername(Socket),
    Name = lists:concat([I1,".",I2,".",I3,".",I4,":",Port]),
    State = #net_proc{
        socket = Socket,
        name   = list_to_binary(Name),
        ip     = Ip
    },
    % self() ! link_start,
    gen_server:enter_loop(?MODULE, [], State).


% handle_info(link_start, #net_proc{socket=Socket,name=Name}=State) ->
%     erlang:port_command(Socket, [Name, <<", Welcome to Erlang!\n">>]),
%     {noreply, State};
% handle_info({tcp, _Socket, BinData}, #net_proc{name=Name}=State) ->
%     gen_server:cast(net_common_svr, {bcast, [Name, <<" -> ">>, BinData]}),
%     {noreply, State};
handle_info({tcp, _Socket, BinData}, #net_proc{}=State) ->
    State2 = do_handle(BinData, State),
    {noreply, State2};

handle_info({send_rec, Rec}, #net_proc{}=State) ->
    send_rec(Rec, State),
    {noreply, State};

handle_info({inet_reply, _, ok}, State) ->
    {noreply, State};

handle_info({tcp_closed, _Socket}, State) ->
    gen_server:cast(net_common_svr, {offline, self()}),
    {stop, normal, State};

handle_info({tcp_error, _, _Reason}, State) ->
    gen_server:cast(net_common_svr, {offline, self()}),
    {stop, normal, State};

handle_info(timeout, State) ->
    gen_server:cast(net_common_svr, {offline, self()}),
    {stop, normal, State};

handle_info(_Data, State) ->
    io:format("data=~p~n", [_Data]),
    {noreply, State}.

handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast({send_bin, BinData}, #net_proc{socket=Socket}=State) ->
    erlang:port_command(Socket, BinData),
    {noreply, State};
handle_cast({send_rec, Rec}, #net_proc{}=State) ->
    send_rec(Rec, State),
    {noreply, State};
handle_cast(_, State) ->
    {noreply, State}.

code_change(_, State, _) ->
    {ok, State}.

terminate(_Reason, _State) ->
    gen_server:cast(net_common_svr, {offline, self()}),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send_rec(Rec, #user_rec{net_pid=NetPid}) ->
    ?TRAC_W(user, NetPid),
    gen_server:cast(NetPid, {send_rec, Rec});
send_rec(Rec, #net_proc{socket=Socket}) ->
    case net_enc:encode(Rec) of
        [] ->
            skip;
        BinData ->
            ?TRAC_W(send, Rec),
            true = erlang:port_command(Socket, BinData)
    end.

% @doc
do_handle(Bin, State) ->
    ?TRAC_W({rev, Bin}),
    case net_enc:decode(Bin) of
        {true, Code, Rec} ->
            State2 = do_handle_msg(Code, Rec, State),
            State2;
        _ ->
            State
    end.

do_handle_msg(Code, Rec, #net_proc{pid=undefined}=State) ->
    ?TRAC_W(1),
    State2 = ts_login_mHandler:handle_msg(Code, Rec, State),
    State2;
do_handle_msg(Code, Rec, #net_proc{pid=Pid}=State) ->
    ?TRAC_W(2, Pid),
    gen_server:cast(Pid, {c_msg, Code, Rec}),
    State.