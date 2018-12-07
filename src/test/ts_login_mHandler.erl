% @Author: weiyanguang
% @Email:  rondsny@gmail.com
% @Date:   2016-11-16 17:46:11
% @Last Modified by:   weiyanguang
% @Last Modified time: 2016-11-17 15:20:47
% @Desc: 协议处理

-module(ts_login_mHandler).
-include("ts_global.hrl").
-export([handle_msg/3]).

handle_msg(1001, #c_login{username=User,password=Pass}, Net) ->
    ?TRAC_W({User, Pass}),
    Net2 = ts_login:c_login(User, Pass, Net),
    Net2;
handle_msg(_, _, Net) ->
    Net.