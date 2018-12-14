% @Author: weiyanguang
% @Email:  rondsny@gmail.com
% @Date:   2016-11-15 14:42:17
% @Last Modified by:   weiyanguang
% @Last Modified time: 2018-12-06 17:05:54
% @Desc:

-module(net_core).
-include("ts_global.hrl").
-export([start/0]).

start() ->
    {ok, _} = net_common_svr:start(),
    {ok, _} = ranch:start_listener(tserver, 1, ranch_tcp, [{port, 3000}], net_protocol, []),
    ?TRAC_W("[START] net start succ"),
    ok.