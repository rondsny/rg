chcp 65001
rem @set PATH=F:\Program Files\erl7.3\bin;%PATH%
setlocal
cd %~dp0
cd ..
call vcvarsall amd64
rem call rebar clean
call .\rebar.cmd co

ping 127.0.0.1 -n 1 > nul

PAUSE