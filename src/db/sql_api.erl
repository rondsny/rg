% @Author: weiyanguang
% @Email:  rondsny@gmail.com
% @Date:   2016-11-22 10:57:06
% @Last Modified by:   weiyanguang
% @Last Modified time: 2016-11-22 15:31:59
% @Desc:

-module(sql_api).
-export([start/0]).
-export([open/2]).
-export([delete/2]).
-export([insert/2]).
-export([lookup/2]).
% -export([sync/1]).

start() ->
    sql_common:start().

open(Name, Opts) ->
    case cc_ets:new(Name, Opts) of
        Name ->
            sql_svr:start_link(Name);
        _ ->
            error
    end.

delete(Table, Id) ->
    ets:delete(Table, Id),
    gen_server:cast(pname(Table), {delete, Table, Id}).

insert(Table, Data) ->
    ets:insert(Table, Data),
    Ids = [element(2, Ele)||Ele <- Data],
    gen_server:cast(pname(Table), {insert, Ids}).

lookup(Table, Id) ->
    case ets:lookup(Table, Id) of
        [] ->
            case gen_server:call(pname(Table), {lookup, Table, Id}) of
                [] ->
                    [];
                Data ->
                    ets:insert(Table, Data),
                    Data
            end;
        Data ->
            Data
    end.

% sync(_) -> ok.

%%
pname(Table) ->
    sql_svr:pname(Table).