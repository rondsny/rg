% @Author: weiyanguang
% @Email:  rondsny@gmail.com
% @Date:   2016-11-15 14:22:36
% @Last Modified by:   weiyanguang
% @Last Modified time: 2018-12-06 16:44:56
% @Desc:

-module(tserver_main).
-include("ts_global.hrl").
-export([start/0]).
-export([stop/0]).

% @doc 开服
start() ->
    io:format("starting ...~n"),
    application:ensure_all_started(tserver),
    io:format("~n"),
    ?TRAC_W("[START] start svr"),
    ok = net_core:start(),
    % ok = sql_api:start(),

    ok = ts_war_svr:start_static(),
    ?TRAC_W("[START] start svr done!"),
    ok.

% @doc 关服
stop() ->
    io:format("stoping ...~n"),
    init:stop().


% boot_db() ->
%     todo.