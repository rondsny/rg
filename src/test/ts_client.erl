% @Author: weiyanguang
% @Email:  rondsny@gmail.com
% @Date:   2016-11-17 11:01:05
% @Last Modified by:   weiyanguang
% @Last Modified time: 2016-11-17 18:38:44
% @Desc:

-module(ts_client).
-include("ts_global.hrl").
-export([start/0]).

start() ->
    {ok, Socket} = gen_tcp:connect("localhost", 5555, [binary, {packet, 2}]),
    ok = send_rec(#c_login{username="weiyg", password="123456"}, Socket),
    ok.

% @doc 发送
send_rec(Rec, Socket) ->
    case net_enc:encode(Rec) of
        [] ->
            skip;
        BinMsg ->
            ok = gen_tcp:send(Socket, BinMsg),
            listen(Socket)
    end.

% @doc 监听
listen(Sock) ->
    receive
        {tcp, Socket, BinBack} ->
            ?TRAC_W(rec, BinBack),
            do_handle(BinBack, Socket)
        after 10000 ->
            ok = gen_tcp:close(Sock)
    end,
    ok.

do_handle(Bin, Socket) ->
    case net_enc:decode_c(Bin) of
        {true, Code, Rec} ->
            Socket2 = handle_msg(Code, Rec, Socket),
            Socket2;
        _ ->
            Socket
    end.

handle_msg(_MCode, #s_login{code=Code,msg=Msg}, Socket) ->
    ?TRAC_W({Code, Msg}),
    ok = send_rec(#c_user_list{}, Socket),
    Socket;
handle_msg(_MCode, #s_user_list{}, Socket) ->
    ?TRAC_W(login_succ, stop),
    Socket;
handle_msg(_MCode, _Rec, Socket) ->
    io:format("unkown code = ~p", [_MCode]),
    Socket.