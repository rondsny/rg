% @Author: weiyanguang
% @Email:  rondsny@gmail.com
% @Date:   2017-02-09 14:47:16
% @Last Modified by:   weiyanguang
% @Last Modified time: 2017-02-13 11:57:27
% @Desc:

-module(t_btest).
-include("ts_global.hrl").
-include("data.hrl").
-compile([export_all]).

init() ->
    t_bullet:open_scene(),

    Ids = data_scene:gets(),
    Cfsg = [data_scene:get(Id)||Id<-Ids],
    lists:foreach(fun add_obj/1, Cfsg),
    ok.

add_obj(#data_scene{
            type     = box,
            position = Bpos,
            rotation = Rota,
            scale    = Scal
        }) ->
    t_bullet:add_box(Scal, Bpos, Rota);

add_obj(#data_scene{
            type     = sphere,
            position = Bpos,
            rotation = Rota,
            scale    = {Radius, _, _}
        }) ->
    t_bullet:add_sphere(Radius, Bpos, Rota);
add_obj(#data_scene{
            type     = sphere,
            position = Bpos,
            rotation = Rota,
            scale    = Radius
        }) when is_number(Radius) ->
    t_bullet:add_sphere(Radius, Bpos, Rota);
add_obj(#data_scene{
            type     = mesh,
            position = Bpos,
            rotation = Rota,
            vtx      = Vts,
            idx      = Idx
        }) ->
    t_bullet:add_mesh(length(Vts)/3, length(Idx), Vts, Idx, Bpos, Rota);
add_obj(_Unk) ->
    skip.