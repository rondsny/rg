% @Author: weiyanguang
% @Email:  rondsny@gmail.com
% @Date:   2017-01-03 16:14:34
% @Last Modified by:   weiyanguang
% @Last Modified time: 2017-01-10 10:53:09
% @Desc: 常见函数集

-module(cm_util).
-export([ceil/1]).
-export([unixtime/0]).

ceil(N) ->
    case trunc(N) of
        M when N == M ->
            M;
        M when N > 0 ->
            M + 1;
        M ->
            M
    end.

unixtime() ->
    erlang:system_time(seconds).

