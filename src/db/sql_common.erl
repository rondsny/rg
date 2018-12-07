% @Author: weiyanguang
% @Email:  rondsny@gmail.com
% @Date:   2016-11-18 15:12:33
% @Last Modified by:   weiyanguang
% @Last Modified time: 2016-11-22 15:05:05
% @Desc: emysql启动/各个表管理/对外接口

-module(sql_common).
-include_lib("emysql/include/emysql.hrl").
-include("ts_global.hrl").
-export([start/0]).

-export([create/1]).
-export([delete/2]).
-export([insert/3]).
-export([insert_list/2]).
-export([lookup/2]).

-define(POOL, tserver).

%%% 主进程 %%%%%

% @doc 初始化emysql
start() ->
    application:start(crypto),
    application:start(emysql),
    emysql:add_pool(?POOL, [
            {size, 1},
            {host, "100.84.35.75"},
            {port, 3306},
            {user, "x4"},
            {password, "123456"},
            {database, "test_tserver"},
            {encoding, latin1}
        ]),
    ?TRAC_W("[START] emysql start succ"),
    ok.

%%%%%%%% 数据库表进程 %%%%%%%%%

% @doc 创建表
create(Table) ->
    BinTab = atom_to_binary(Table, latin1),
    Sql = <<
        "CREATE TABLE IF NOT EXISTS ", BinTab/binary, " (",
        "`id` INT UNSIGNED PRIMARY KEY,",
        "`data` MEDIUMBLOB NOT NULL,",
        "`modified_time` TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP)"
    >>,
    Result = emysql:execute(?POOL, Sql),
    ?TRAC_W(Sql, Result),
    ok.

delete(Table, Id) ->
    BinTab = atom_to_binary(Table, latin1),
    BinId  = integer_to_binary(Id),
    Sql =  <<
        "DELETE FROM ", BinTab/binary, " WHERE `id` = ", BinId/binary
    >>,
    Result = emysql:execute(?POOL, Sql),
    ?TRAC_W(Sql, Result),
    ok.

insert(Table, Id, Data) ->
    BinTab  = atom_to_binary(Table, latin1),
    BinId   = integer_to_binary(Id),
    BinData = term_to_binary(Data),
    Sql =  <<
        "INSERT INTO ", BinTab/binary, " (`id`, `data`) VALUES (", BinId/binary, ", '", BinData/binary,
        "') ON DUPLICATE KEY UPDATE data=VALUES(data)"
    >>,
    Result = emysql:execute(?POOL, Sql),
    ?TRAC_W(Sql, Result),
    ok.

insert_list(Table, List) ->
    BinTab  = atom_to_binary(Table, latin1),
    List2   = [ <<"(", (integer_to_binary(element(2, Ele)))/binary, ",", (term_to_binary(Ele))/binary, ")" >>
                ||Ele <- List],
    Bin     =
        case List2 of
            [H|T] ->
                Seq = <<",">>,
                L1 = << <<Seq/binary, I/binary>> || I <- T >>,
                <<H, L1>>;
            _ ->
                <<>>
        end,
    Sql =  <<
        "INSERT INTO ", BinTab/binary,
        " (`id`, `data`) VALUES ", Bin/binary,
        " ON DUPLICATE KEY UPDATE data=VALUES(data)"
    >>,
    Result = emysql:execute(?POOL, Sql),
    ?TRAC_W(Sql, Result),
    ok.

lookup(Table, Id) ->
    BinTab = atom_to_binary(Table, latin1),
    BinId  = integer_to_binary(Id),
    Sql =  <<
        "SELECT `data` FROM ", BinTab/binary, " WHERE `id` = ", BinId/binary
    >>,
    #result_packet{rows=Rows} = _Result = emysql:execute(?POOL, Sql),
    ?TRAC_W(1, _Result),
    Result2 = [R || [R] <- Rows],
    ?TRAC_W(2, Result2),
    Result3 = [erlang:binary_to_term(D) || D <- Result2],
    ?TRAC_W(3, Result3),
    Result3.

