% @Author: weiyanguang
% @Email:  rondsny@gmail.com
% @Date:   2016-11-17 11:26:31
% @Last Modified by:   weiyanguang
% @Last Modified time: 2016-11-17 17:18:46
% @Desc: net_encrypt encode/decode

-module(net_enc).
-include("ts_global.hrl").
-export([encode/1]).
-export([decode/1]).
-export([decode_c/1]).

% @doc encode协议
encode(Rec) ->
    Key = erlang:element(1, Rec),
    case data_msg_code:code(Key) of
        undef ->
            [];
        Code ->
            Bin  = msg_pb:encode(Rec),
            Len  = erlang:iolist_size(Bin) + 2,
            Bin3 = [<<Len:16, Code:16>>, Bin],
            ?TRAC_W(send_bin, Bin3),
            Bin3
    end.

% @doc decode协议
decode(<<Len0:16, Bin0:Len0/binary, _Res0/binary>>) -> %% 去掉头
    <<Len:16, Bin:Len/binary, _Res1/binary>> = Bin0,
    <<Code:16, Bin2/binary>> = Bin,
    case data_msg_code:get(Code) of
        undef ->
            false;
        Key ->
            Rec = msg_pb:decode(Key, Bin2),
            ?TRAC_W(Code, Rec),
            {true, Code, Rec}
    end.


% @doc decode协议(客户端)
decode_c(Bin) ->
    <<Code:16, Bin2/binary>> = Bin,
    case data_msg_code:get(Code) of
        undef ->
            false;
        Key ->
            Rec = msg_pb:decode(Key, Bin2),
            ?TRAC_W(Code, Rec),
            {true, Code, Rec}
    end.