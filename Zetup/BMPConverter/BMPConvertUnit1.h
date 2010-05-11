//---------------------------------------------------------------------------
#ifndef BMPConvertUnit1H
#define BMPConvertUnit1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
#include <Dialogs.hpp>
#include <ExtDlgs.hpp>
#include <ComCtrls.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
    TPanel *Panel1;
    TButton *LoadButton1;
    TButton *ConverButton1;
    TLabel *Label1;
    TImage *Image1;
    TOpenPictureDialog *OpenPictureDialog1;
    TStatusBar *StatusBar1;
    TButton *ExitButton1;
    void __fastcall LoadButton1Click(TObject *Sender);
    void __fastcall ConverButton1Click(TObject *Sender);
    void __fastcall ExitButton1Click(TObject *Sender);

private:	// User declarations

public:		// User declarations

    __fastcall TForm1(TComponent* Owner);

    
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
