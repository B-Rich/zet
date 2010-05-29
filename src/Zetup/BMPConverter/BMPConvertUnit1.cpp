//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
#include "BMPConvertUnit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::LoadButton1Click(TObject *Sender)
{
    if(OpenPictureDialog1->Execute()) {
        Image1->Picture->Bitmap->LoadFromFile(OpenPictureDialog1->FileName);
        ConverButton1->Enabled = true;
        StatusBar1->SimpleText = OpenPictureDialog1->FileName + "  Loaded";
    }
}
//---------------------------------------------------------------------------
#define Threshold 127           // Black to white threshold
//---------------------------------------------------------------------------
void __fastcall TForm1::ConverButton1Click(TObject *Sender)
{
    StatusBar1->SimpleText = "Converting...";

    TStringList *mifile = new TStringList();
    mifile->Add("WIDTH=8;");
    mifile->Add("DEPTH=26000;");
    mifile->Add("ADDRESS_RADIX=HEX;");
    mifile->Add("DATA_RADIX=HEX;");
    mifile->Add("CONTENT BEGIN");

    AnsiString Tmp;
    int n = 0;
    for(int y = 0; y < Image1->Height; y++) {
        for(int x = 0; x < Image1->Width/8; x++) {
            byte Data = 0;
            for(int i = 0; i < 8; i++) {
                TColor pixel = Image1->Picture->Bitmap->Canvas->Pixels[x*8+i][y];
                byte R = GetRValue(int(pixel));
                byte G = GetGValue(int(pixel));
                byte B = GetBValue(int(pixel));
                int bit = bool((R>Threshold)&&(G>Threshold)&&(B>Threshold));
                Data |= bit<<(7-i);
            }
            Tmp = IntToHex(n,4) + " : " + IntToHex(Data,2) + ";";
            mifile->Add(Tmp);
            n++;
        }
    }
    mifile->Add("END");
    mifile->SaveToFile("ImageData.mif");
    delete mifile;
    StatusBar1->SimpleText = "Conversion complete";
}
//---------------------------------------------------------------------------
void __fastcall TForm1::ExitButton1Click(TObject *Sender)
{
    Close();    
}
//---------------------------------------------------------------------------

