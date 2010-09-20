echo off
cls
IF NOT EXIST a:EZR.EXE GOTO WRONGDSK
goto START
:WRONGDSK
cls
echo ÿ
echo ÿ
echo ÿ        Please insert MouseWare Disk #1 into drive A
echo ÿ
echo ÿ
pause
IF NOT EXIST a:EZR.EXE GOTO WRONGDSK
:START
cls
echo ÿ
echo ÿ
echo     loading Install...
echo ÿ
echo ÿ
a:ezr -i a:inst
if errorlevel 2 goto theend
if errorlevel 1 goto testmse
goto theend
:testmse
IF NOT EXIST a:COMCHECK.EXE GOTO DISKERR
a:mouse > nul
if errorlevel 4 goto ps2err
if errorlevel 2 goto serial2
if errorlevel 1 goto buserr
a:logimenu > nul
a:click > nul
a:mtest
goto theend
:serial2
a:mouse ser 2 >nul
if errorlevel 1 goto sererr
a:logimenu
a:click
a:mtest
goto theend
:DISKERR
cls
echo ÿ
echo ÿ
echo ÿ
echo ÿ
echo ÿ        Please insert MouseWare Disk #1 into drive A
echo ÿ
echo ÿ
echo ÿ        ÿ
goto testmse
:BUSERR
cls
echo ÿ
echo ÿ
echo ÿ
echo ÿ
echo ÿ        Your Bus Version mouse is not responding...
echo ÿ
echo ÿ        Please re-check the mouse cable connection
echo ÿ        and Bus board installation (see Chapter 1, How
echo ÿ        To Install Your Mouse, in the "Getting Started"
echo ÿ        booklet).
echo ÿ
echo ÿ
echo ÿ
echo ÿ
goto theend
:PS2ERR
cls
echo ÿ
echo ÿ
echo ÿ
echo ÿ
echo ÿ        Your Serial-PS/2 mouse is not responding...
echo ÿ
echo ÿ        Please re-check the mouse cable connection.
echo ÿ        (see Chapter 1, How To Install Your Mouse,
echo ÿ        in the "Getting Started" booklet).
echo ÿ
echo ÿ
echo ÿ
echo ÿ
goto theend
:SERERR
a:comcheck
pause
:theend
echo on
cls
