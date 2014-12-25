object MacrosMoveForm: TMacrosMoveForm
  Left = 0
  Top = 0
  Caption = 'Move macros...'
  ClientHeight = 282
  ClientWidth = 418
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 8
    Width = 63
    Height = 13
    Caption = 'Move macros'
  end
  object FromLabel: TLabel
    Left = 24
    Top = 40
    Width = 24
    Height = 13
    Caption = 'From'
  end
  object FromDevice: TLabel
    Left = 104
    Top = 40
    Width = 289
    Height = 13
    AutoSize = False
    Caption = 'FromDevice'
  end
  object ToLabel: TLabel
    Left = 24
    Top = 72
    Width = 12
    Height = 13
    Caption = 'To'
  end
  object ResultLabel: TLabel
    Left = 24
    Top = 104
    Width = 30
    Height = 13
    Caption = 'Result'
  end
  object ResultText: TLabel
    Left = 104
    Top = 104
    Width = 281
    Height = 105
    AutoSize = False
    WordWrap = True
  end
  object ToDevice: TComboBox
    Left = 104
    Top = 69
    Width = 289
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
  end
  object MoveButton: TButton
    Left = 72
    Top = 240
    Width = 105
    Height = 25
    Caption = 'Move'
    TabOrder = 1
    OnClick = MoveButtonClick
  end
  object CancelButton: TButton
    Left = 232
    Top = 240
    Width = 97
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object OKButton: TButton
    Left = 152
    Top = 240
    Width = 105
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 3
    Visible = False
  end
end
