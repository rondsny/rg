% @Author: weiyanguang
% @Email:  rondsny@gmail.com
% @Date:   2016-11-15 14:42:17
% @Last Modified by:   weiyanguang
% @Last Modified time: 2016-11-21 11:15:22
% @Desc:

-module(net_core).
-include("ts_global.hrl").
-export([start/0]).

start() ->
    {ok, _} = net_common_svr:start(),
    {ok, _} = ranch:start_listener(tserver, 1, ranch_tcp, [{port, 5555}], net_protocol, []),
    ?TRAC_W("[START] net start succ"),
    ok.