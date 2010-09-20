			LOGITECH MouseWare Release 4.01
			-------------------------------

------------------------------------------------------------------------

Errata
======

In "Getting The Most From Your Mouse", there is an error on page 67:
In the first table the color codes on lines 4 and 5  (+48 and +64)
should be reversed. There is also an error on page 78: The last
Warning message should say "(type LOGIMENU)". 

------------------------------------------------------------------------

Support For Lotus 1-2-3
=======================

Mouse-2-3 currently supports Lotus 1-2-3 releases 1.x and 2.x.

We have however enclosed on the distribution disk a new 123R3.DEF menu
file for Lotus ver 3.0 that has limited mouse support due to the
graphical environment of this Lotus version. 

To use this menu file, make sure you copy it to the Lotus 123 ver 3.0
directory and compile it using the "NEWMENU 123r3" command. To enable
mouse support in Lotus, load the command "LOGIMENU 123r3" before you
start your Lotus session.

------------------------------------------------------------------------

Using the Logitech Mouse under GEM
==================================

When installing GEM, if there is no selection for the Logitech mouse,
choose the mouse selection that says "uses MOUSE.COM".

------------------------------------------------------------------------

Using the Logitech Mouse under MicroSoft Windows
================================================

Proper installation under MS Windows, release 2.0 or higher, for the
various types of mice is as follows:

Serial Mouse, or Serial-PS/2 Mouse configured as a Serial Mouse:
	Select "Logitech Mouse" from the Mouse selection menu.

Bus Mouse:
	Select "Logitech Mouse" from the Mouse selection menu.

PS/2 Mouse, or Serial-PS/2 Mouse configured as a PS/2 Mouse:
	Select "MicroSoft Mouse Connected to Serial-PS/2 Interface"
	from the Mouse selection menu. Also note that using this entry
	will limit you to use only 2 buttons since you will be in
	Microsoft mouse emulation mode.

For release 1.xx of Windows, the file LMOUSE.DRV is required.  Due to
lack of space, it is no longer included on the 5.25 inch diskettes. 
If your system cannot use 3.5 inch diskettes, contact Logitech
Technical Support (see Appendix B in "Getting Started") to request a
5.25 inch disk with LMOUSE.DRV.

------------------------------------------------------------------------

Using the Logitech Mouse under IBM DOS 4.00
===========================================

After installing IBM DOS 4.00 on your computer, you end up with new 
AUTOEXEC.BAT file at the root directory and DOSSHELL.BAT file in the 
\DOS directory.
        
In order to be able to use the Logitech Mouse (any type) with the
mouse  driver loaded in memory, you have to modify both the
AUTOEXEC.BAT and DOSSHELL.BAT files.
        
With a simple text editor, open the DOSSHELL.BAT file and look for 
this statement:

	@SHELLC /MOS:driver.MOS/TRAN/COLOR/ etc.......        	
        (where driver can be PCIBMDRV, PCMSDRV, or PCMSPDRV)

delete the following string "/MOS:driver.MOS" from the @SHELLC 
statement, leave the rest as is. 
The modified statement should look like this:

	@SHELLC /TRAN/COLOR/ etc.......        	
        
Next, modify your AUTOEXEC.BAT file to include the \MOUSE 
sub-directory in the PATH and load MOUSE, MENU, and CLICK before 
loading the DOSSHELL command.
        
You will then be able to use the Logitech Mouse inside and outside 
the DOS Shell.
        
------------------------------------------------------------------------
               
Using LOGITECH Products under IBM OS/2
======================================

 Mouse Hardware & Software
 -------------------------

All LOGITECH Mice work with the IBM OS/2 mouse drivers. 
Proper installation for the various types of mice is as follows:

Serial-PS/2 Mouse configured as a Serial Mouse:
	Select "Mouse Systems Mouse" during OS/2 installation
	("DEVICE=MOUSEx00.SYS SERIAL=COMn" in CONFIG.SYS). 
	Replace "x" in "MOUSEx00.SYS" with "A" for AT-type computers 
	or "B" for PS/2 computers.

Serial-PS/2 Mouse configured as a PS/2 Mouse:
	Install as IBM PS/2 Mouse during OS/2 installation 
	("DEVICE=MOUSEB05.SYS" in config.sys).

Bus Mouse:
	Select "Microsoft Mouse, Bus Version" during OS/2 installation
	("DEVICE=MOUSEA03.SYS" in config.sys).  
	Note that the OS/2 driver will not recognize the middle button. 


All of the software mentioned below can only be run in the DOS
Compatibility Box (also known as Real Mode). 

Driver
The DOS mouse driver can only be run if the OS/2 mouse driver is not
loaded.  The DOS mouse driver works perfectly in the Compatibility
Box.  Customers who do not have OS/2 applications that use a mouse
should use the DOS mouse driver, since it appears to update the
cursor more frequently, giving smoother motion. MOUSE.SYS does not
work under OS/2, use MOUSE.COM in the compatibility box instead.

Click & LogiMenu are both compatible with Real Mode, if the DOS mouse
driver is installed.

------------------------------------------------------------------------

Technical Tips 
--------------

1- To make use of the Unlimited Product Support Program offered by
   Logitech, please refer to Appendix B in the manual titled "Getting
   Started". 

2- When using Pop-Up DOS on a system equipped with a Logitech 
   EGA-&-Mouse board, make sure that switch 8 is set to OFF.

3- If you are upgrading from a previous version of the Logitech Plus
   Package Software and you have the mouse MENU command in the
   AUTOEXEC.BAT file, make sure to change this command to LOGIMENU.

4- When defining applications to be used from within Pop-Up DOS that 
   use a batch file containing the environment "SET" statements, you
   should move these commands to a batch file which will set the
   environment variable prior to starting Pop-Up DOS.  For example:
   "SET TEMP=C:\TEMP" command uses the environment space. This
   environment space cannot be allocated after Pop-Up DOS has been
   started.

5- When defining applications to be used from within Pop-Up DOS that
   use any memory resident programs (programs that will stay resident
   in memory at all times after termination), you will have to load
   these TSR's before starting Pop-Up DOS. Some applications such as
   Logitech's PaintShow Plus use memory resident utilities like
   CATCH.COM.  In this case, you need to use a batch file which will
   load the resident program before the application and removes the
   resident program before returning to Pop-Up DOS. If your batch file
   fails to remove the resident utility, then the memory used will not
   be freed thus leaving less memory available for Pop-Up Dos and your
   applications.

