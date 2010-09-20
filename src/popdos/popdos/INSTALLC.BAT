echo off
cls
IF NOT EXIST EZR.EXE GOTO WRONGDSK
goto START
:WRONGDSK
cls
echo ÿ
echo ÿ   Can not find MouseWare installation files
echo ÿ
pause
goto theend
:START
cls
echo ÿ
echo ÿ    loading Install files...
echo ÿ
ezr -i inst
if errorlevel 2 goto theend
if errorlevel 1 goto testmse
goto theend
:testmse
IF NOT EXIST COMCHECK.EXE GOTO WRONGDSK
mouse > nul
if errorlevel 4 goto ps2err
if errorlevel 2 goto serial2
if errorlevel 1 goto buserr
logimenu > nul
click > nul
mtest
goto theend
:serial2
mouse ser 2 >nul
if errorlevel 1 goto sererr
logimenu
click
mtest
goto theend
:DISKERR
cls
echo ÿ
echo ÿ   Can not find MouseWare installation files
echo ÿ
goto testmse
:BUSERR
cls
echo ÿ
echo ÿ        Your Bus Version mouse is not responding...
echo ÿ
echo ÿ        Please re-check the mouse cable connection
echo ÿ        and Bus board installation (see Chapter 1, How
echo ÿ        To Install Your Mouse, in the "Getting Started"
echo ÿ        booklet).
echo ÿ
goto theend
:PS2ERR
cls
echo ÿ
echo ÿ        Your Serial-PS/2 mouse is not responding...
echo ÿ
echo ÿ        Please re-check the mouse cable connection.
echo ÿ        (see Chapter 1, How To Install Your Mouse,
echo ÿ        in the "Getting Started" booklet).
echo ÿ
goto theend
:SERERR
comcheck
pause
:theend
echo on

