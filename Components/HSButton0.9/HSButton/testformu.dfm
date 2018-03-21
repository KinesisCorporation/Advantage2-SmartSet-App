object Form1: TForm1
  Left = 165
  Top = 129
  Caption = 'Form1'
  ClientHeight = 437
  ClientWidth = 637
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClick = FormClick
  OnCreate = FormCreate
  OnPaint = FormPaint
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 480
    Top = 60
    Width = 32
    Height = 13
    Caption = 'Border'
  end
  object Label2: TLabel
    Left = 480
    Top = 120
    Width = 36
    Height = 13
    Caption = 'Smooth'
  end
  object Label3: TLabel
    Left = 440
    Top = 28
    Width = 176
    Height = 13
    Caption = 'Try Moving the scroll bars and see...'
  end
  object ScrollBar1: TScrollBar
    Left = 480
    Top = 79
    Width = 121
    Height = 17
    Max = 10
    PageSize = 0
    Position = 8
    TabOrder = 0
    OnChange = ScrollBar1Change
  end
  object ScrollBar2: TScrollBar
    Left = 480
    Top = 139
    Width = 121
    Height = 17
    Max = 10
    PageSize = 0
    Position = 2
    TabOrder = 1
  end
  object CheckBox1: TCheckBox
    Left = 480
    Top = 176
    Width = 97
    Height = 17
    Caption = 'Flat'
    TabOrder = 2
    OnClick = CheckBox1Click
  end
end
