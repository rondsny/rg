% @Author: weiyanguang
% @Email:  rondsny@gmail.com
% @Date:   2016-11-11 16:51:50
% @Last Modified by:   weiyanguang
% @Last Modified time: 2016-11-15 16:34:03
% @Desc:

-module(tserver_app).
-behaviour(application).
-export([start/2]).
-export([stop/1]).

start(_, _) ->
    tserver_sup:start_link().

stop(_) ->
    ok.