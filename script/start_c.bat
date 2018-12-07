@REM @Author: weiyanguang
@REM @Date:   2016-11-17 14:50:02
@REM @Last Modified by:   weiyanguang
@REM Modified time: 2016-11-17 14:50:17

setlocal enabledelayedexpansion
cd %~dp0
cd ..
set PA=
for /d %%i in (deps\*) do @ set PA=!PA! %%i\ebin
start werl ^
    -pa ebin %PA% ^
    -hidden ^
    -name tserver_c@127.0.0.1 ^
    -setcookie tserver_c
exit