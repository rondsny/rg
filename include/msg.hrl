%% -*- coding: utf-8 -*-
%% Automatically generated, do not edit
%% Generated by gpb_compile version 4.4.1

-ifndef(msg).
-define(msg, true).

-define(msg_gpb_version, "4.4.1").

-ifndef('PERSON_PB_H').
-define('PERSON_PB_H', true).
-record('Person',
        {name = []              :: iolist() | undefined, % = 1
         address = []           :: iolist() | undefined, % = 2
         phone_number = []      :: iolist() | undefined, % = 3
         age = 0                :: integer() | undefined, % = 4, 32 bits
         location = undefined   :: msg:'Location'() | undefined % = 5
        }).
-endif.

-ifndef('LOCATION_PB_H').
-define('LOCATION_PB_H', true).
-record('Location',
        {region = []            :: iolist() | undefined, % = 1
         country = []           :: iolist() | undefined % = 2
        }).
-endif.

-ifndef('C_LOGIN_PB_H').
-define('C_LOGIN_PB_H', true).
-record(c_login,
        {username = []          :: iolist() | undefined, % = 1
         password = []          :: iolist() | undefined % = 2
        }).
-endif.

-ifndef('S_LOGIN_PB_H').
-define('S_LOGIN_PB_H', true).
-record(s_login,
        {code = 0               :: non_neg_integer() | undefined, % = 1, 32 bits
         msg = []               :: iolist() | undefined, % = 2
         info = undefined       :: msg:p_user_info() | undefined % = 3
        }).
-endif.

-ifndef('C_USER_LIST_PB_H').
-define('C_USER_LIST_PB_H', true).
-record(c_user_list,
        {type = 0               :: non_neg_integer() | undefined % = 1, 32 bits
        }).
-endif.

-ifndef('S_USER_LIST_PB_H').
-define('S_USER_LIST_PB_H', true).
-record(s_user_list,
        {list = []              :: [msg:p_user_info()] | undefined % = 1
        }).
-endif.

-ifndef('P_USER_INFO_PB_H').
-define('P_USER_INFO_PB_H', true).
-record(p_user_info,
        {id = 0                 :: non_neg_integer() | undefined, % = 1, 32 bits
         state = 0              :: non_neg_integer() | undefined, % = 2, 32 bits
         username = []          :: iolist() | undefined, % = 3
         nickname = []          :: iolist() | undefined % = 4
        }).
-endif.

-endif.