object Form1: TForm1
  Left = 229
  Top = 103
  BorderStyle = bsDialog
  Caption = ' BMP to MIF Converter'
  ClientHeight = 466
  ClientWidth = 525
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 152
    Top = 200
    Width = 206
    Height = 23
    Caption = '520 x 400 Monochrome'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Arial'
    Font.Style = [fsItalic]
    ParentFont = False
  end
  object Image1: TImage
    Left = 2
    Top = 44
    Width = 520
    Height = 400
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 525
    Height = 41
    Align = alTop
    TabOrder = 0
    object LoadButton1: TButton
      Left = 8
      Top = 8
      Width = 97
      Height = 25
      Caption = 'Load BMP File'
      TabOrder = 0
      OnClick = LoadButton1Click
    end
    object ExitButton1: TButton
      Left = 440
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Exit'
      TabOrder = 1
      OnClick = ExitButton1Click
    end
  end
  object ConverButton1: TButton
    Left = 112
    Top = 8
    Width = 97
    Height = 25
    Caption = 'Convert to MIF'
    Enabled = False
    TabOrder = 1
    OnClick = ConverButton1Click
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 447
    Width = 525
    Height = 19
    Panels = <>
    SimplePanel = True
    SimpleText = 'Ready'
  end
  object OpenPictureDialog1: TOpenPictureDialog
    DefaultExt = 'BMP'
    Filter = 'Bitmaps (*.bmp)|*.bmp|All Files (*.*)|*.*'
    InitialDir = '.'
    Title = 'Load BMP'
    Left = 360
    Top = 8
  end
end
