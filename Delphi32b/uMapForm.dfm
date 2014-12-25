object MapForm: TMapForm
  Left = 0
  Top = 0
  Caption = 'Moving map'
  ClientHeight = 282
  ClientWidth = 418
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnHide = FormHide
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object FlowPanel1: TFlowPanel
    Left = 0
    Top = 0
    Width = 418
    Height = 24
    Align = alTop
    TabOrder = 0
    object LockedCB: TCheckBox
      Left = 1
      Top = 1
      Width = 152
      Height = 17
      Caption = 'Locked'
      TabOrder = 0
    end
    object AlwaysOnTopCB: TCheckBox
      Left = 153
      Top = 1
      Width = 152
      Height = 17
      Caption = 'Always On Top'
      TabOrder = 1
      OnClick = AlwaysOnTopCBClick
    end
    object ShowLayersCB: TCheckBox
      Left = 305
      Top = 1
      Width = 97
      Height = 17
      Caption = 'Show info layer'
      TabOrder = 2
      OnClick = ShowLayersCBClick
    end
  end
  object WebBrowser1: TWebBrowser
    Left = 0
    Top = 24
    Width = 418
    Height = 258
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alClient
    TabOrder = 1
    ExplicitLeft = 40
    ExplicitTop = 72
    ExplicitWidth = 300
    ExplicitHeight = 150
    ControlData = {
      4C000000342B0000AA1A00000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E12620A000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
  object Timer1: TTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 120
    Top = 88
  end
end
