-include("msg.hrl").

-define(true, true).
-define(false, false).
-define(undefined, undefined).
-define(IF(X, Y, Z), case X of true -> Y; _ -> Z end).

-define(ERROR_MSG(A, B), io:format(A, B)).
-define(WARNING(A, B), io:format(A, B)).

-define(TRAC_W(Param), io:format("[WYG] ~p:~p => ~p~n", [?MODULE, ?LINE, Param])).
-define(TRAC_W(A, B), ?TRAC_W({A, B})).


% 网络进程结构
-record(net_proc, {
    socket,
    pid, % 玩家进程
    name,
    ip
}).

% 用户进程结构
-record(user_rec, {
    net_pid, % 网络进程
    ext
}).

% 一个副本
-record(war, {
    id    = 0,      % 预留
    units = [],     % 存在的单位（战斗单位）
    loop  = 0,
    ext   = #{}
}).

% 一个战斗单位
-record(unit, {
    id  = 0,
    pos = {0, 0},
    ext = #{}
}).