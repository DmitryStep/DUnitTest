object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 701
  ClientWidth = 1111
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 24
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object Label2: TLabel
    Left = 24
    Top = 64
    Width = 31
    Height = 13
    Caption = 'Label2'
  end
  object Label3: TLabel
    Left = 24
    Top = 152
    Width = 31
    Height = 13
    Caption = 'Label3'
  end
  object Label4: TLabel
    Left = 24
    Top = 216
    Width = 31
    Height = 13
    Caption = 'Label4'
  end
  object Label5: TLabel
    Left = 32
    Top = 304
    Width = 31
    Height = 13
    Caption = 'Label5'
  end
  object Button2: TButton
    Left = 24
    Top = 480
    Width = 75
    Height = 25
    Caption = 'Button2'
    TabOrder = 0
    OnClick = Button2Click
  end
  object Memo1: TMemo
    Left = 546
    Top = 8
    Width = 557
    Height = 685
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
  object ListBox1: TListBox
    Left = 168
    Top = 8
    Width = 372
    Height = 685
    ItemHeight = 13
    TabOrder = 2
  end
  object Button1: TButton
    Left = 24
    Top = 536
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 3
    OnClick = Button1Click
  end
end
