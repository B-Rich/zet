echo off
cls
IF NOT EXIST a:EZR.EXE GOTO WRONGDSK
goto START
:WRONGDSK
cls
echo �
echo �
echo �        Please insert MouseWare Disk #1 into drive A
echo �
echo �
pause
IF NOT EXIST a:EZR.EXE GOTO WRONGDSK
:START
cls
echo �
echo �
echo     loading Install...
echo �
echo �
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
echo �
echo �
echo �
echo �
echo �        Please insert MouseWare Disk #1 into drive A
echo �
echo �
echo �        �
goto testmse
:BUSERR
cls
echo �
echo �
echo �
echo �
echo �        Your Bus Version mouse is not responding...
echo �
echo �        Please re-check the mouse cable connection
echo �        and Bus board installation (see Chapter 1, How
echo �        To Install Your Mouse, in the "Getting Started"
echo �        booklet).
echo �
echo �
echo �
echo �
goto theend
:PS2ERR
cls
echo �
echo �
echo �
echo �
echo �        Your Serial-PS/2 mouse is not responding...
echo �
echo �        Please re-check the mouse cable connection.
echo �        (see Chapter 1, How To Install Your Mouse,
echo �        in the "Getting Started" booklet).
echo �
echo �
echo �
echo �
goto theend
:SERERR
a:comcheck
pause
:theend
echo on
cls
