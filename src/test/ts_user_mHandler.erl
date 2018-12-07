% @Author: weiyanguang
% @Email:  rondsny@gmail.com
% @Date:   2016-11-16 17:46:11
% @Last Modified by:   weiyanguang
% @Last Modified time: 2016-11-17 18:47:51
% @Desc: 协议处理

-module(ts_user_mHandler).
-include("ts_global.hrl").
-export([handle_msg/3]).

handle_msg(1003, _Rec, User) ->
    ?TRAC_W(_Rec, User),
    net_protocol:send_rec(#s_user_list{list=[]}, User),
    User;
handle_msg(_, _, User) ->
    User.