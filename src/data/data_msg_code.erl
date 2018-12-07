% @Author: weiyanguang
% @Email:  rondsny@gmail.com
% @Date:   2016-11-16 17:37:50
% @Last Modified by:   weiyanguang
% @Last Modified time: 2016-11-16 17:43:45
% @Desc:

-module(data_msg_code).
-export([get/1]).
-export([code/1]).

get(1001) -> c_login;
get(1002) -> s_login;
get(1003) -> c_user_list;
get(1004) -> s_user_list;
get(_)    -> undef.


code(c_login)     -> 1001;
code(s_login)     -> 1002;
code(c_user_list) -> 1003;
code(s_user_list) -> 1004;
code(_)           -> undef.