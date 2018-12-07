% @Author: weiyanguang
% @Email:  rondsny@gmail.com
% @Date:   2016-11-15 14:32:02
% @Last Modified by:   weiyanguang
% @Last Modified time: 2017-01-09 15:31:20
% @Desc:

-module(tserver_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-export([start_child/2]).

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 10, 10}, []}}.

%%%%%%%%%
start_child(Id, Module) ->
    ChildSpec = #{
        id       => Id,
        start    => {Module, start_link, []},
        restart  => permanent,
        shutdown => 15000,
        type     => worker,
        modules  => [Module]
    },
    supervisor:start_child(?MODULE, ChildSpec).