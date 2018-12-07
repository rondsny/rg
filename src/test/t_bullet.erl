% @Author: weiyanguang
% @Email:  rondsny@gmail.com
% @Date:   2017-01-22 18:04:28
% @Last Modified by:   weiyanguang
% @Last Modified time: 2017-02-13 15:03:16
% @Desc:

-module(t_bullet).
-export([
     open_scene/0
    ,close_scene/0

    ,add_box/3
    ,add_sphere/3
    ,add_cupsule/4
    ,add_mesh/6

    ,col_check_capsule/3
    ,ray_hit/2
    ]).

-on_load(init/0).

init() ->
    Code = filename:join("./priv", atom_to_list(?MODULE)),
    erlang:load_nif(Code, 0).

%% 初始化场景
open_scene() ->
    erlang:nif_error(undef).

%% 删除场景内资源，回收内存
close_scene() ->
    erlang:nif_error(undef).

add_box(_BoxHalf, _Bpos, _Rotation) ->
    erlang:nif_error(undef).

add_sphere(_Radius, _Bpos, _Rotation) ->
    erlang:nif_error(undef).

add_cupsule(_Radius, _Height, _Bpos, _Rotation) ->
    erlang:nif_error(undef).

add_mesh(_VtxCount, _IdxCount, _VtxList, _IdxList, _Bpos, _Rotation) ->
    erlang:nif_error(undef).

%% 检查胶囊体与场景内是否发生碰撞
%% 胶囊体两端坐标，半径
%% @return {1, {x,y,z}}| {0,{A.x, A.y, A.z}}
col_check_capsule(_PosA, _PosB, _Raduis) ->
    erlang:nif_error(undef).

%% 射线击中检测
ray_hit(_From, _To) ->
    erlang:nif_error(undef).