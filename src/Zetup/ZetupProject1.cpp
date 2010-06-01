//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USEFORM("About.cpp", AboutBox);
USEFORM("Main.cpp", Form1);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
    try
    {
         Application->Initialize();
         Application->Title = "ZET Setup";
         Application->HelpFile = "C:\\Dev1\\DOS\\Zet\\Setup\\Zetup.hlp";
         Application->CreateForm(__classid(TForm1), &Form1);
         Application->CreateForm(__classid(TAboutBox), &AboutBox);
         Application->Run();
    }
    catch (Exception &exception)
    {
         Application->ShowException(&exception);
    }
    catch (...)
    {
         try
         {
             throw Exception("");
         }
         catch (Exception &exception)
         {
             Application->ShowException(&exception);
         }
    }
    return 0;
}
//---------------------------------------------------------------------------
