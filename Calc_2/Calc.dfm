object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 445
  ClientWidth = 391
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 128
    Top = 11
    Width = 3
    Height = 13
  end
  object Edit1: TEdit
    Left = 8
    Top = 8
    Width = 49
    Height = 21
    TabOrder = 0
  end
  object Edit2: TEdit
    Left = 63
    Top = 8
    Width = 50
    Height = 21
    TabOrder = 1
  end
  object Button1: TButton
    Left = 8
    Top = 40
    Width = 33
    Height = 25
    Caption = '+'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 47
    Top = 40
    Width = 34
    Height = 25
    Caption = '-'
    TabOrder = 3
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 87
    Top = 40
    Width = 34
    Height = 25
    Caption = '*'
    TabOrder = 4
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 127
    Top = 40
    Width = 34
    Height = 25
    Caption = '/'
    TabOrder = 5
    OnClick = Button4Click
  end
end
