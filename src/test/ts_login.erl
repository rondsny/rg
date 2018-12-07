% @Author: weiyanguang
% @Email:  rondsny@gmail.com
% @Date:   2016-11-16 18:37:26
% @Last Modified by:   weiyanguang
% @Last Modified time: 2016-11-17 17:55:25
% @Desc:

-module(ts_login).
-include("ts_global.hrl").
-export([c_login/3]).


c_login(_User, _Pass, #net_proc{}=Net) ->
    case ts_user_svr:start_link() of
        {ok, Pid} when is_pid(Pid) ->
            ?TRAC_W({pid, Pid}),
            Net2 = Net#net_proc{pid=Pid},
            Rec = #s_login{code=1,msg="login succ"},
            erlang:send_after(1000, self(), {send_rec, Rec}),
            Net2;
        _ ->
            ?TRAC_W(init_user, fail),
            Net
    end.