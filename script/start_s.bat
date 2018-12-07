@REM @Author: weiyanguang
@REM @Date:   2016-11-15 15:00:11
@REM @Last Modified by:   weiyanguang
@REM Modified time: 2017-01-23 17:53:59

setlocal enabledelayedexpansion
cd %~dp0
cd ..
set PA=
for /d %%i in (deps\*) do @ set PA=!PA! %%i\ebin
start erl ^
    -pa ebin %PA% ^
    -hidden ^
    -name tserver@127.0.0.1 ^
    -setcookie tserver ^
    -s tserver_main start
exit