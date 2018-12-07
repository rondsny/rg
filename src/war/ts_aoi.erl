% @Author: weiyanguang
% @Email:  rondsny@gmail.com
% @Date:   2017-01-03 13:57:56
% @Last Modified by:   weiyanguang
% @Last Modified time: 2017-01-10 10:51:25
% @Desc: 一个测试的AOI管理(网格)

-module(ts_aoi).
-include("ts_global.hrl").
-export([init/0]).                  % 初始化地图AOI
-export([map_enter/3]).
-export([map_leave/2]).
-export([map_move/0]).

-define(POS_MIN, {0, 0}).           % 左下角坐标
-define(POS_MAX, {10000, 10000}).   % 右上角坐标
-define(X_GRID_LEN, 100).
-define(Y_GRID_LEN, 100).

-record(aoi_info, {
        % id           = 0,
        x_min        = 0,
        y_min        = 0,
        x_grid_count = 0,
        y_grid_count = 0
        % grids        = []           % 网格id列表，有人的才初始化
        % grids_info   = []           % 使用进程字典保存，方便查找 {aoi_id, uuid}
    }).

init() ->
    {XMin, YMin} = ?POS_MIN,
    {XMax, YMax} = ?POS_MAX,

    XLen         = (XMax - XMin),
    XGridCount0  = XLen div ?X_GRID_LEN,
    XGridCount1  = ?IF(XLen rem ?X_GRID_LEN > 0, XGridCount0 + 1, XGridCount0),

    YLen         = (YMax - YMin),
    YGridCount0  = YLen div ?Y_GRID_LEN,
    YGridCount1  = ?IF(YLen rem ?Y_GRID_LEN > 0, YGridCount0 + 1, YGridCount0),

    #aoi_info{
        x_min        = XMin,
        y_min        = YMin,
        x_grid_count = XGridCount1,
        y_grid_count = YGridCount1
    }.


map_enter(UUID, {_X, _Y}=Pos, #aoi_info{}=Info) ->
    GridId = get_grid_id(Pos, Info),
    ?TRAC_W(id, GridId),
    add_member(GridId, UUID, Pos),
    Info.

map_leave(UUID, #aoi_info{}=Info) ->
    case erlang:get({uuid, UUID}) of
        {_X, _Y} = Pos ->
            GridId = get_grid_id(Pos, Info),
            ?TRAC_W(id, GridId),
            del_member(GridId, UUID),
            Info;
        _ ->
            Info
    end.

map_move() ->
    todo.

%% ---------------------------------------
get_grid_id({X, Y},
    #aoi_info{
        x_min        = XMin,
        y_min        = YMin,
        x_grid_count = XGridCount0
    }) ->
    XT = cm_util:ceil((X - XMin) / ?X_GRID_LEN),
    YT = ((Y - YMin) div ?Y_GRID_LEN) * XGridCount0,
    XT + YT.

add_member(GridId, UUID, Pos) ->
    Old = get_member(GridId),
    New =
        case lists:member(UUID, Old) of
            ?true -> Old;
            _ -> [UUID|Old]
        end,
    set_uuid(UUID, Pos),
    erlang:put({grid, GridId}, New).

del_member(GridId, UUID) ->
    Old = get_member(GridId),
    New = lists:delete(UUID, Old),
    set_uuid(UUID, ?undefined),
    erlang:put({grid, GridId}, New).

get_member(GridId) ->
    case erlang:get({grid, GridId}) of
        ?undefined ->
            [];
        List ->
            List
    end.

set_uuid(UUID, Pos) ->
    erlang:put({uuid, UUID}, Pos).

%% -------------------------------------

-compile([export_all]).
test_print() ->
    MaxC = ?X_GRID_LEN * ?Y_GRID_LEN,
    test_print_1(MaxC).

test_print_1(N) when N > 0 ->
    case erlang:get({grid, N}) of
        ?undefined ->
            skip;
        _List ->
            ?TRAC_W(list, _List)
    end,
    test_print_1(N-1);
test_print_1(_) ->
    ok.
