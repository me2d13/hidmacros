object MouseForm: TMouseForm
  Left = 227
  Top = 108
  BorderStyle = bsDialog
  Caption = 'Mouse trigger'
  ClientHeight = 228
  ClientWidth = 373
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 16
    Width = 35
    Height = 13
    Caption = 'Mouse:'
  end
  object OKBtn: TButton
    Left = 123
    Top = 191
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object CancelBtn: TButton
    Left = 204
    Top = 191
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object MouseComboBox: TComboBox
    Left = 88
    Top = 13
    Width = 257
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
  end
  object GroupBox1: TGroupBox
    Left = 24
    Top = 48
    Width = 321
    Height = 137
    Caption = ' Mouse event '
    TabOrder = 3
    object Label2: TLabel
      Left = 76
      Top = 25
      Width = 30
      Height = 13
      Caption = 'Wheel'
    end
    object Label3: TLabel
      Left = 24
      Top = 72
      Width = 54
      Height = 13
      Caption = 'Left button'
    end
    object Label4: TLabel
      Left = 128
      Top = 72
      Width = 65
      Height = 13
      Caption = 'Middle button'
    end
    object Label5: TLabel
      Left = 247
      Top = 72
      Width = 60
      Height = 13
      Caption = 'Right button'
    end
    object RBWheelUp: TRadioButton
      Left = 128
      Top = 24
      Width = 145
      Height = 17
      Caption = 'Up'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object RBWheelDown: TRadioButton
      Left = 128
      Top = 47
      Width = 127
      Height = 17
      Caption = 'Down'
      TabOrder = 1
    end
    object RBLeftUp: TRadioButton
      Left = 24
      Top = 114
      Width = 82
      Height = 17
      Caption = 'Up'
      TabOrder = 2
    end
    object RBMiddleUp: TRadioButton
      Left = 128
      Top = 114
      Width = 81
      Height = 17
      Caption = 'Up'
      TabOrder = 5
    end
    object RBRightUp: TRadioButton
      Left = 247
      Top = 114
      Width = 71
      Height = 17
      Caption = 'Up'
      TabOrder = 7
    end
    object RBLeftDown: TRadioButton
      Left = 24
      Top = 91
      Width = 82
      Height = 17
      Caption = 'Down'
      TabOrder = 3
    end
    object RBMiddleDown: TRadioButton
      Left = 128
      Top = 91
      Width = 81
      Height = 17
      HelpType = htKeyword
      HelpKeyword = 'BMiddleDown'
      Caption = 'Down'
      TabOrder = 4
    end
    object RBRightDown: TRadioButton
      Left = 247
      Top = 91
      Width = 71
      Height = 17
      Caption = 'Down'
      TabOrder = 6
    end
  end
end
