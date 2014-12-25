object GameForm: TGameForm
  Left = 227
  Top = 108
  BorderStyle = bsDialog
  Caption = 'Button trigger'
  ClientHeight = 370
  ClientWidth = 373
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poScreenCenter
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 16
    Width = 65
    Height = 13
    Caption = 'Game device:'
  end
  object OKBtn: TButton
    Left = 115
    Top = 337
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object CancelBtn: TButton
    Left = 196
    Top = 337
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object GameComboBox: TComboBox
    Left = 100
    Top = 13
    Width = 245
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
  end
  object GroupBox1: TGroupBox
    Left = 24
    Top = 48
    Width = 321
    Height = 273
    Caption = ' Device buttons '
    TabOrder = 3
    object ScrollBox1: TScrollBox
      Left = 2
      Top = 15
      Width = 317
      Height = 256
      Align = alClient
      TabOrder = 0
    end
  end
end
