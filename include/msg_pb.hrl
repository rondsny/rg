-ifndef(PERSON_PB_H).
-define(PERSON_PB_H, true).
-record(person, {
    name = erlang:error({required, name}),
    address = erlang:error({required, address}),
    phone_number = erlang:error({required, phone_number}),
    age = erlang:error({required, age}),
    location
}).
-endif.

-ifndef(LOCATION_PB_H).
-define(LOCATION_PB_H, true).
-record(location, {
    region = erlang:error({required, region}),
    country = erlang:error({required, country})
}).
-endif.

-ifndef(C_LOGIN_PB_H).
-define(C_LOGIN_PB_H, true).
-record(c_login, {
    username = erlang:error({required, username}),
    password = erlang:error({required, password})
}).
-endif.

-ifndef(S_LOGIN_PB_H).
-define(S_LOGIN_PB_H, true).
-record(s_login, {
    code = erlang:error({required, code}),
    msg,
    info
}).
-endif.

-ifndef(C_USER_LIST_PB_H).
-define(C_USER_LIST_PB_H, true).
-record(c_user_list, {
    type
}).
-endif.

-ifndef(S_USER_LIST_PB_H).
-define(S_USER_LIST_PB_H, true).
-record(s_user_list, {
    list = []
}).
-endif.

-ifndef(P_USER_INFO_PB_H).
-define(P_USER_INFO_PB_H, true).
-record(p_user_info, {
    id = erlang:error({required, id}),
    state = erlang:error({required, state}),
    username = erlang:error({required, username}),
    nickname = erlang:error({required, nickname})
}).
-endif.

