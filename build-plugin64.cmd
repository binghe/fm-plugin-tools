@echo off
REM +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
REM + Example build script for Windows                            +
REM +                                                             +
REM + Change the following `set' statements to adapt them to your +
REM + local settings:                                             +
REM +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

REM The LispWorks executable
set lispworks="C:\Program Files\LispWorks\lispworks-7-0-0-x64-windows.exe"

REM +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
REM + Don't change anything below this point.                     +
REM +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

%lispworks% -build deliver.lisp FMPLisp
if errorlevel 1 (
    echo Couldn't build DLL.
    pause
)
