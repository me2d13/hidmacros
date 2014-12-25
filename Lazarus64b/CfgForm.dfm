object HIDMacrosForm: THIDMacrosForm
  Left = 353
  Top = 279
  Caption = 'HID macros'
  ClientHeight = 448
  ClientWidth = 559
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar: TStatusBar
    Left = 0
    Top = 429
    Width = 559
    Height = 19
    Panels = <
      item
        Width = 100
      end
      item
        Style = psOwnerDraw
        Width = 150
      end
      item
        Width = 150
      end
      item
        Width = 150
      end>
    OnDrawPanel = StatusBarDrawPanel
  end
  object ActionToolBar: TActionToolBar
    Left = 0
    Top = 0
    Width = 559
    Height = 26
    ActionManager = ActionManager
    Caption = 'ActionToolBar'
    ColorMap.HighlightColor = clWhite
    ColorMap.BtnSelectedColor = clBtnFace
    ColorMap.UnusedColor = clWhite
    Spacing = 0
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 26
    Width = 559
    Height = 403
    ActivePage = TabSheet2
    Align = alClient
    TabOrder = 2
    OnChange = PageControl1Change
    object TabSheet1: TTabSheet
      Caption = 'Devices'
      object ListView1: TListView
        Left = 0
        Top = 26
        Width = 551
        Height = 349
        Align = alClient
        Columns = <
          item
            Caption = 'Name'
            MaxWidth = 400
            MinWidth = 50
            Width = 100
          end
          item
            Caption = 'Type'
          end
          item
            Caption = 'Macros #'
          end
          item
            AutoSize = True
            Caption = 'System ID'
          end>
        ColumnClick = False
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnAdvancedCustomDrawItem = ListView1AdvancedCustomDrawItem
        OnClick = ListView1Click
        OnEdited = ListView1Edited
      end
      object DevicesActionToolBar: TActionToolBar
        Left = 0
        Top = 0
        Width = 551
        Height = 26
        ActionManager = ActionManager
        Caption = 'DevicesActionToolBar'
        ColorMap.HighlightColor = clWhite
        ColorMap.BtnSelectedColor = clBtnFace
        ColorMap.UnusedColor = clWhite
        Spacing = 0
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Macros'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Splitter1: TSplitter
        Left = 185
        Top = 0
        Height = 375
        ExplicitLeft = 248
        ExplicitTop = 232
        ExplicitHeight = 100
      end
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 185
        Height = 375
        Align = alLeft
        Caption = 'Panel1'
        TabOrder = 0
        object ActionToolBar1: TActionToolBar
          Left = 1
          Top = 1
          Width = 183
          Height = 26
          ActionManager = ActionManager
          Caption = 'ActionToolBar1'
          ColorMap.HighlightColor = clWhite
          ColorMap.BtnSelectedColor = clBtnFace
          ColorMap.UnusedColor = clWhite
          Spacing = 0
        end
        object MacrosLB: TListBox
          Left = 1
          Top = 27
          Width = 183
          Height = 347
          Align = alClient
          ItemHeight = 13
          Sorted = True
          TabOrder = 1
          OnClick = MacrosLBClick
        end
      end
      object Panel2: TPanel
        Left = 188
        Top = 0
        Width = 363
        Height = 375
        Align = alClient
        Caption = 'Panel2'
        TabOrder = 1
        object GroupBox1: TGroupBox
          Left = 1
          Top = 1
          Width = 361
          Height = 56
          Align = alTop
          Caption = ' Test area - last input event '
          TabOrder = 0
          object Label1: TLabel
            Left = 5
            Top = 28
            Width = 36
            Height = 13
            Caption = 'Device:'
          end
          object Label2: TLabel
            Left = 193
            Top = 29
            Width = 32
            Height = 13
            Caption = 'Event:'
          end
          object TestKeyboardName: TEdit
            Left = 61
            Top = 26
            Width = 121
            Height = 21
            Enabled = False
            TabOrder = 0
          end
          object TestScanCode: TEdit
            Left = 231
            Top = 26
            Width = 114
            Height = 21
            Enabled = False
            TabOrder = 1
          end
        end
        object GroupBox2: TGroupBox
          Left = 1
          Top = 57
          Width = 361
          Height = 317
          Align = alClient
          Caption = ' Edit Macro '
          TabOrder = 1
          object Label3: TLabel
            Left = 5
            Top = 24
            Width = 31
            Height = 13
            Caption = 'Name:'
          end
          object Label4: TLabel
            Left = 5
            Top = 51
            Width = 38
            Height = 13
            Caption = 'Trigger:'
          end
          object ActionGB: TGroupBox
            Left = 2
            Top = 70
            Width = 357
            Height = 245
            Align = alBottom
            Caption = ' Action '
            TabOrder = 0
            object PageControl2: TPageControl
              Left = 2
              Top = 15
              Width = 353
              Height = 228
              ActivePage = PredefinedActionTS
              Align = alClient
              TabOrder = 0
              OnChange = PageControl2Change
              object PredefinedActionTS: TTabSheet
                Caption = 'Predefined'
                ExplicitLeft = 0
                ExplicitTop = 0
                ExplicitWidth = 0
                ExplicitHeight = 0
                object Label13: TLabel
                  Left = 176
                  Top = 81
                  Width = 39
                  Height = 13
                  Caption = 'Params:'
                end
                object SCParCounLabel: TLabel
                  Left = 322
                  Top = 96
                  Width = 3
                  Height = 13
                end
                object KeyboardRB: TRadioButton
                  Tag = 1
                  Left = 3
                  Top = 0
                  Width = 246
                  Height = 17
                  Caption = 'send keyboard sequence'
                  TabOrder = 0
                  OnClick = KeyboardRBClick
                end
                object SimConnectRB: TRadioButton
                  Tag = 1
                  Left = 3
                  Top = 42
                  Width = 214
                  Height = 17
                  Caption = 'send Simconnect event (FSX)'
                  TabOrder = 2
                  OnClick = KeyboardRBClick
                end
                object SequenceEdit: TEdit
                  Left = 24
                  Top = 15
                  Width = 313
                  Height = 21
                  TabOrder = 1
                  OnChange = SequenceEditChange
                end
                object SimconnectCB: TComboBox
                  Left = 24
                  Top = 57
                  Width = 313
                  Height = 21
                  Style = csDropDownList
                  ItemHeight = 0
                  TabOrder = 3
                  OnChange = SimconnectCBChange
                end
                object SCTextChB: TCheckBox
                  Left = 24
                  Top = 80
                  Width = 146
                  Height = 17
                  Caption = 'With text notification'
                  TabOrder = 4
                  OnClick = SCTextChBClick
                end
                object SCParamsEdit: TEdit
                  Left = 240
                  Top = 78
                  Width = 81
                  Height = 21
                  TabOrder = 5
                  OnChange = SCParamsEditChange
                end
                object SysCommandRB: TRadioButton
                  Tag = 1
                  Left = 3
                  Top = 136
                  Width = 246
                  Height = 17
                  Caption = 'run application'
                  TabOrder = 8
                  OnClick = KeyboardRBClick
                end
                object CommandEdit: TEdit
                  Left = 24
                  Top = 154
                  Width = 289
                  Height = 21
                  TabOrder = 9
                  OnChange = CommandEditChange
                end
                object FileOpenButton: TButton
                  Left = 319
                  Top = 152
                  Width = 25
                  Height = 25
                  Caption = '...'
                  TabOrder = 10
                  OnClick = FileOpenButtonClick
                end
                object SendToBufferRB: TRadioButton
                  Tag = 1
                  Left = 3
                  Top = 182
                  Width = 246
                  Height = 17
                  Caption = 'send to buffer (keyboard macros only)'
                  TabOrder = 11
                  OnClick = KeyboardRBClick
                end
                object XPLCommandRB: TRadioButton
                  Tag = 1
                  Left = 3
                  Top = 100
                  Width = 214
                  Height = 17
                  Caption = 'send X-plane command'
                  TabOrder = 6
                  OnClick = KeyboardRBClick
                end
                object XPLCommandEdit: TComboBox
                  Left = 24
                  Top = 114
                  Width = 313
                  Height = 21
                  AutoDropDown = True
                  ItemHeight = 0
                  TabOrder = 7
                  OnChange = XPLCommandEditOldChange
                end
              end
              object ScriptedActionTS: TTabSheet
                Caption = 'Scripted'
                ImageIndex = 1
                ExplicitLeft = 0
                ExplicitTop = 0
                ExplicitWidth = 0
                ExplicitHeight = 0
                object MacroScriptRichEdit: TRichEdit
                  Left = 0
                  Top = 0
                  Width = 345
                  Height = 176
                  Align = alClient
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -11
                  Font.Name = 'Courier New'
                  Font.Style = []
                  ParentFont = False
                  ScrollBars = ssBoth
                  TabOrder = 0
                  WordWrap = False
                  OnChange = MacroScriptRichEditChange
                  OnKeyUp = MacroScriptRichEditKeyUp
                  OnMouseDown = MacroScriptRichEditMouseDown
                end
                object Panel6: TPanel
                  Left = 0
                  Top = 176
                  Width = 345
                  Height = 24
                  Align = alBottom
                  BevelOuter = bvLowered
                  TabOrder = 1
                  object MacroErrImage: TImage
                    Left = 45
                    Top = 5
                    Width = 16
                    Height = 16
                    Picture.Data = {
                      055449636F6E0000010001000F0F000001001800340300001600000028000000
                      0F0000001E0000000100180000000000000000002C0100002C01000000000000
                      00000000000000000000000000FDFDFDB2B2AA72725641410B2C2C001B1B0A4C
                      4C4B9D9D9DFCFCFC000000000000000000000000000000000000E8E8E6666633
                      46460059591256565359596B5353493D3D100D0D00292929DDDDDD0000000000
                      00000000000000E1E1DC65651263630B4A4A830E0EEC0000FF0000FF0000FF10
                      10EA42427D29290A0E0E0ECDCDCD0000000000000000008C8C3E73730D2C2CBF
                      0000FF0000FF0000FF0000FF0000FF0000FF0000FF3232AE2C2C092E2E2EFEFE
                      FE000000CFCFBC7E7E004242990000FF0000FF0000FF0000FF0000FF0000FF00
                      00FF0000FF0000FF4242800F0F00A0A0A0000000A3A3568181200707F60000FF
                      0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF0F0FEC4141144040
                      3F0000008B8B0063636B0000FF0000FF0000FF0000FF0000FF0000FF0000FF00
                      00FF0000FF0000FF0000FF5151581616000000009B9B004343A30000FF0000FF
                      0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF5858732F2F
                      00000000B0B0006666720000FF0000FF0000FF0000FF0000FF0000FF0000FF00
                      00FF0000FF0000FF0000FF58585A3A3A00000000CECE6295952D0505F70000FF
                      0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF0A0AF15D5D166B6B
                      4A000000E7E7CABBBB014141A70000FF0000FF0000FF0000FF0000FF0000FF00
                      00FF0000FF0000FF46468B484800B6B6AD000000000000DEDE56A4A4231D1DD8
                      0000FF0000FF0000FF0000FF0000FF0000FF0000FF2C2CBF66660D6B6B38FEFE
                      FE000000000000F5F5ECEDED46ACAC194444A30606F70000FF0000FF0000FF0A
                      0AF249498F75750C6A6A12DEDEDA000000000000000000000000F8F8F3DCDC4F
                      BBBB009B9B236C6C676969736B6B5D8888168080008A8A38EBEBE80000000000
                      00000000000000000000000000FEFEFEE6E6C7D1D172B5B5109C9C0090900DAB
                      AB66CFCFBBFEFEFE000000000000000000000000E00E0000C006000080020000
                      8000000000000000000000000000000000000000000000000000000000000000
                      8000000080020000C0060000E00E0000}
                  end
                  object MacroOkImage: TImage
                    Left = 45
                    Top = 5
                    Width = 16
                    Height = 16
                    Picture.Data = {
                      055449636F6E0000010001000F0F000001001800340300001600000028000000
                      0F0000001E0000000100180000000000000000002C0100002C01000000000000
                      00000000000000000000000000FDFDFDB2B2AA72725641410B2C2C001B1B0A4C
                      4C4B9D9D9DFCFCFC000000000000000000000000000000000000E8E8E6666633
                      464600596B0055AA0059C5004D9F003C4D000D0D00292929DDDDDD0000000000
                      00000000000000E1E1DC656512636F0049CE000DFB0000FF0000FF0000FF000F
                      FB0041C0002933000E0E0ECDCDCD0000000000000000008C8C3E6F830023EF00
                      00FF0000FF0000FF0000FF0000FF0000FF0000FF0031E2002C36002E2E2EFEFE
                      FE000000CFCFBC7E7F0041DC0000FF0000FF0000FF0000FF0000FF0000FF0000
                      FF0000FF0000FF0041C3000F0F00A0A0A0000000A3A35680A30006FD0000FF00
                      00FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF000EFB004155004040
                      3F0000008B8B0062CF0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000
                      FF0000FF0000FF0000FF0050AA001616000000009B9B0040E80000FF0000FF00
                      00FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF0057CC002F2F
                      00000000B0B0005EDB0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000
                      FF0000FF0000FF0000FF0051B5003A3A00000000CECE6294C30005FE0000FF00
                      00FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF000AFC005D74006B6B
                      4A000000E7E7CABBBC0040E90000FF0000FF0000FF0000FF0000FF0000FF0000
                      FF0000FF0000FF003FD400484900B6B6AD000000000000DEDE56A4C5021DF600
                      00FF0000FF0000FF0000FF0000FF0000FF0000FF0025EF006673006B6B38FEFE
                      FE000000000000F5F5ECEDED46A5C50241E90005FE0000FF0000FF0000FF0009
                      FD0042DB007582006A6A12DEDEDA000000000000000000000000F8F8F3DCDC4F
                      BBBB009BBF0068D5004AE30065CA00869F008080008A8A38EBEBE80000000000
                      00000000000000000000000000FEFEFEE6E6C7D1D172B5B5109C9C0090900DAB
                      AB66CFCFBBFEFEFE000000000000000000000000E00E0000C006000080020000
                      8000000000000000000000000000000000000000000000000000000000000000
                      8000000080020000C0060000E00E0000}
                  end
                  object Label18: TLabel
                    Left = 72
                    Top = 8
                    Width = 37
                    Height = 13
                    Caption = 'Label18'
                  end
                  object Label24: TLabel
                    Left = 2
                    Top = 6
                    Width = 40
                    Height = 13
                    AutoSize = False
                  end
                  object TestMacroScriptB: TBitBtn
                    Left = 269
                    Top = 1
                    Width = 75
                    Height = 22
                    Align = alRight
                    Caption = 'Test'
                    TabOrder = 0
                    OnClick = TestMacroScriptBClick
                  end
                  object CheckMacroScriptB: TBitBtn
                    Left = 194
                    Top = 1
                    Width = 75
                    Height = 22
                    Align = alRight
                    Caption = 'Compile'
                    TabOrder = 1
                    OnClick = CheckMacroScriptBClick
                  end
                end
              end
            end
          end
          object NameEdit: TEdit
            Left = 61
            Top = 21
            Width = 284
            Height = 21
            TabOrder = 1
            OnChange = NameEditChange
          end
          object EditKeyboardName: TEdit
            Left = 128
            Top = 48
            Width = 89
            Height = 21
            Enabled = False
            TabOrder = 2
          end
          object EditScanCode: TEdit
            Left = 223
            Top = 48
            Width = 124
            Height = 21
            Enabled = False
            TabOrder = 3
          end
          object SetButton: TButton
            Left = 61
            Top = 48
            Width = 61
            Height = 21
            Caption = 'Scan'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 4
            OnClick = SetButtonClick
          end
        end
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Script'
      ImageIndex = 4
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object RoutinesGroupBox: TGroupBox
        Left = 0
        Top = 0
        Width = 551
        Height = 307
        Align = alClient
        Caption = ' Routines '
        TabOrder = 0
        object RoutinesEdit: TRichEdit
          Left = 2
          Top = 15
          Width = 547
          Height = 263
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          ScrollBars = ssBoth
          TabOrder = 0
          WordWrap = False
          OnChange = RoutinesEditChange
          OnKeyUp = RoutinesEditKeyUp
          OnMouseDown = RoutinesEditMouseDown
        end
        object Panel4: TPanel
          Left = 2
          Top = 278
          Width = 547
          Height = 27
          Align = alBottom
          TabOrder = 1
          object RoutinesOKImage: TImage
            Left = 60
            Top = 5
            Width = 16
            Height = 16
            Picture.Data = {
              055449636F6E0000010001000F0F000001001800340300001600000028000000
              0F0000001E0000000100180000000000000000002C0100002C01000000000000
              00000000000000000000000000FDFDFDB2B2AA72725641410B2C2C001B1B0A4C
              4C4B9D9D9DFCFCFC000000000000000000000000000000000000E8E8E6666633
              464600596B0055AA0059C5004D9F003C4D000D0D00292929DDDDDD0000000000
              00000000000000E1E1DC656512636F0049CE000DFB0000FF0000FF0000FF000F
              FB0041C0002933000E0E0ECDCDCD0000000000000000008C8C3E6F830023EF00
              00FF0000FF0000FF0000FF0000FF0000FF0000FF0031E2002C36002E2E2EFEFE
              FE000000CFCFBC7E7F0041DC0000FF0000FF0000FF0000FF0000FF0000FF0000
              FF0000FF0000FF0041C3000F0F00A0A0A0000000A3A35680A30006FD0000FF00
              00FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF000EFB004155004040
              3F0000008B8B0062CF0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000
              FF0000FF0000FF0000FF0050AA001616000000009B9B0040E80000FF0000FF00
              00FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF0057CC002F2F
              00000000B0B0005EDB0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000
              FF0000FF0000FF0000FF0051B5003A3A00000000CECE6294C30005FE0000FF00
              00FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF000AFC005D74006B6B
              4A000000E7E7CABBBC0040E90000FF0000FF0000FF0000FF0000FF0000FF0000
              FF0000FF0000FF003FD400484900B6B6AD000000000000DEDE56A4C5021DF600
              00FF0000FF0000FF0000FF0000FF0000FF0000FF0025EF006673006B6B38FEFE
              FE000000000000F5F5ECEDED46A5C50241E90005FE0000FF0000FF0000FF0009
              FD0042DB007582006A6A12DEDEDA000000000000000000000000F8F8F3DCDC4F
              BBBB009BBF0068D5004AE30065CA00869F008080008A8A38EBEBE80000000000
              00000000000000000000000000FEFEFEE6E6C7D1D172B5B5109C9C0090900DAB
              AB66CFCFBBFEFEFE000000000000000000000000E00E0000C006000080020000
              8000000000000000000000000000000000000000000000000000000000000000
              8000000080020000C0060000E00E0000}
          end
          object RoutinesErrImage: TImage
            Left = 60
            Top = 5
            Width = 16
            Height = 16
            Picture.Data = {
              055449636F6E0000010001000F0F000001001800340300001600000028000000
              0F0000001E0000000100180000000000000000002C0100002C01000000000000
              00000000000000000000000000FDFDFDB2B2AA72725641410B2C2C001B1B0A4C
              4C4B9D9D9DFCFCFC000000000000000000000000000000000000E8E8E6666633
              46460059591256565359596B5353493D3D100D0D00292929DDDDDD0000000000
              00000000000000E1E1DC65651263630B4A4A830E0EEC0000FF0000FF0000FF10
              10EA42427D29290A0E0E0ECDCDCD0000000000000000008C8C3E73730D2C2CBF
              0000FF0000FF0000FF0000FF0000FF0000FF0000FF3232AE2C2C092E2E2EFEFE
              FE000000CFCFBC7E7E004242990000FF0000FF0000FF0000FF0000FF0000FF00
              00FF0000FF0000FF4242800F0F00A0A0A0000000A3A3568181200707F60000FF
              0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF0F0FEC4141144040
              3F0000008B8B0063636B0000FF0000FF0000FF0000FF0000FF0000FF0000FF00
              00FF0000FF0000FF0000FF5151581616000000009B9B004343A30000FF0000FF
              0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF5858732F2F
              00000000B0B0006666720000FF0000FF0000FF0000FF0000FF0000FF0000FF00
              00FF0000FF0000FF0000FF58585A3A3A00000000CECE6295952D0505F70000FF
              0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF0A0AF15D5D166B6B
              4A000000E7E7CABBBB014141A70000FF0000FF0000FF0000FF0000FF0000FF00
              00FF0000FF0000FF46468B484800B6B6AD000000000000DEDE56A4A4231D1DD8
              0000FF0000FF0000FF0000FF0000FF0000FF0000FF2C2CBF66660D6B6B38FEFE
              FE000000000000F5F5ECEDED46ACAC194444A30606F70000FF0000FF0000FF0A
              0AF249498F75750C6A6A12DEDEDA000000000000000000000000F8F8F3DCDC4F
              BBBB009B9B236C6C676969736B6B5D8888168080008A8A38EBEBE80000000000
              00000000000000000000000000FEFEFEE6E6C7D1D172B5B5109C9C0090900DAB
              AB66CFCFBBFEFEFE000000000000000000000000E00E0000C006000080020000
              8000000000000000000000000000000000000000000000000000000000000000
              8000000080020000C0060000E00E0000}
          end
          object Label17: TLabel
            Left = 86
            Top = 8
            Width = 37
            Height = 13
            Caption = 'Label17'
          end
          object Label25: TLabel
            Left = 3
            Top = 8
            Width = 50
            Height = 13
            AutoSize = False
          end
          object LoadRoutinesButton: TButton
            Left = 480
            Top = 0
            Width = 75
            Height = 25
            Caption = 'Compile'
            TabOrder = 0
            OnClick = LoadRoutinesButtonClick
          end
        end
      end
      object ScriptTestGroupBox: TGroupBox
        Left = 0
        Top = 307
        Width = 551
        Height = 68
        Align = alBottom
        Caption = ' Script test '
        TabOrder = 1
        object Panel5: TPanel
          Left = 459
          Top = 15
          Width = 90
          Height = 51
          Align = alRight
          BevelOuter = bvLowered
          TabOrder = 0
          object ScriptTestButton: TButton
            Left = 8
            Top = 8
            Width = 75
            Height = 25
            Caption = 'Test'
            TabOrder = 0
            OnClick = ScriptTestButtonClick
          end
        end
        object TestRichEdit: TRichEdit
          Left = 2
          Top = 15
          Width = 457
          Height = 51
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          ScrollBars = ssBoth
          TabOrder = 1
        end
      end
    end
    object ScriptToolsTabSheet: TTabSheet
      Caption = ' Script tools '
      ImageIndex = 6
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object PageControl3: TPageControl
        Left = 0
        Top = 0
        Width = 551
        Height = 375
        ActivePage = TSMouseCoordinates
        Align = alClient
        TabOrder = 0
        object TSMouseCoordinates: TTabSheet
          Caption = 'Mouse coordinates'
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object Label19: TLabel
            Left = 16
            Top = 20
            Width = 6
            Height = 13
            Caption = 'X'
          end
          object Label20: TLabel
            Left = 16
            Top = 40
            Width = 6
            Height = 13
            Caption = 'Y'
          end
          object EditX: TEdit
            Left = 48
            Top = 16
            Width = 121
            Height = 21
            ReadOnly = True
            TabOrder = 0
          end
          object EditY: TEdit
            Left = 48
            Top = 38
            Width = 121
            Height = 21
            ReadOnly = True
            TabOrder = 1
          end
        end
        object TSWindowTitles: TTabSheet
          Caption = 'Window titles'
          ImageIndex = 1
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object WindowTitlesMemo: TMemo
            Left = 0
            Top = 0
            Width = 543
            Height = 347
            Align = alClient
            Color = cl3DLight
            ReadOnly = True
            ScrollBars = ssBoth
            TabOrder = 0
          end
        end
        object TSGameAxis: TTabSheet
          Caption = 'Game axis'
          ImageIndex = 2
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object GameAxisStringGrid: TStringGrid
            Left = 0
            Top = 0
            Width = 543
            Height = 347
            Align = alClient
            ColCount = 2
            DefaultRowHeight = 18
            RowCount = 2
            Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing]
            TabOrder = 0
          end
        end
        object TSErrorLog: TTabSheet
          Caption = 'Error log'
          ImageIndex = 3
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object ErrorLogMemo: TMemo
            Left = 0
            Top = 0
            Width = 543
            Height = 347
            Align = alClient
            Color = cl3DLight
            ReadOnly = True
            ScrollBars = ssBoth
            TabOrder = 0
          end
        end
      end
    end
    object SettingsTabSheet: TTabSheet
      Caption = 'Settings'
      ImageIndex = 5
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GeneralGroupBox: TGroupBox
        Left = 0
        Top = 0
        Width = 551
        Height = 179
        Align = alTop
        Caption = ' General '
        TabOrder = 0
        object Label15: TLabel
          Left = 15
          Top = 20
          Width = 51
          Height = 13
          Caption = 'Language:'
        end
        object Label23: TLabel
          Left = 15
          Top = 116
          Width = 97
          Height = 13
          Caption = 'Buffer reset in [ms]:'
        end
        object Label26: TLabel
          Left = 15
          Top = 148
          Width = 43
          Height = 13
          Caption = 'Backups:'
        end
        object Label27: TLabel
          Left = 72
          Top = 148
          Width = 23
          Height = 13
          Caption = 'keep'
        end
        object Label28: TLabel
          Left = 159
          Top = 148
          Width = 108
          Height = 13
          Caption = 'config files in directory'
        end
        object LangComboBox: TComboBox
          Left = 159
          Top = 20
          Width = 189
          Height = 21
          Style = csDropDownList
          ItemHeight = 0
          TabOrder = 0
          OnChange = LangComboBoxChange
        end
        object MinimizeToTrayCB: TCheckBox
          Left = 72
          Top = 47
          Width = 433
          Height = 17
          Caption = 'Minimize to tray'
          TabOrder = 1
          OnClick = MinimizeToTrayCBClick
        end
        object StartMinimizedCB: TCheckBox
          Left = 72
          Top = 70
          Width = 457
          Height = 17
          Caption = 'Start minimized'
          TabOrder = 2
          OnClick = StartMinimizedCBClick
        end
        object ShowBufferChkB: TCheckBox
          Left = 72
          Top = 93
          Width = 457
          Height = 17
          Caption = 'Show buffer content'
          TabOrder = 3
          OnClick = ShowBufferChkBClick
        end
        object BufferResetTOEdit: TEdit
          Left = 159
          Top = 115
          Width = 74
          Height = 21
          TabOrder = 4
          Text = '0'
          OnChange = BufferResetTOEditChange
        end
        object NoOfBackupsEdit: TEdit
          Left = 127
          Top = 145
          Width = 26
          Height = 21
          TabOrder = 5
          Text = '0'
          OnChange = NoOfBackupsEditChange
          OnExit = NoOfBackupsEditExit
        end
        object BackupPathEdit: TEdit
          Left = 351
          Top = 145
          Width = 162
          Height = 21
          TabOrder = 6
          Text = 'backup'
          OnChange = BackupPathEditChange
        end
        object SelectPathButton: TButton
          Left = 520
          Top = 144
          Width = 25
          Height = 25
          Caption = '...'
          TabOrder = 7
          OnClick = SelectPathButtonClick
        end
      end
      object ExperimentalGroupBox: TGroupBox
        Left = 0
        Top = 283
        Width = 551
        Height = 88
        Align = alTop
        Caption = ' Experimental '
        TabOrder = 1
        object Label21: TLabel
          Left = 16
          Top = 32
          Width = 137
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Script timeout:'
        end
        object ScriptTimeOutEdit: TEdit
          Left = 159
          Top = 29
          Width = 42
          Height = 21
          TabOrder = 0
          Text = '10'
          OnChange = ScriptTimeOutEditChange
        end
        object AllowScriptGUICheckBox: TCheckBox
          Left = 159
          Top = 56
          Width = 314
          Height = 17
          Caption = 'Allow script GUI'
          TabOrder = 1
          OnClick = AllowScriptGUICheckBoxClick
        end
      end
      object GroupBox5: TGroupBox
        Left = 0
        Top = 179
        Width = 551
        Height = 104
        Align = alTop
        Caption = ' Script '
        TabOrder = 2
        object Label16: TLabel
          Left = 15
          Top = 19
          Width = 47
          Height = 13
          Caption = 'Language'
        end
        object Label5: TLabel
          Left = 16
          Top = 46
          Width = 78
          Height = 13
          Caption = 'Procedure begin'
        end
        object Label22: TLabel
          Left = 15
          Top = 73
          Width = 70
          Height = 13
          Caption = 'Procedure end'
        end
        object ScriptLanguageCB: TComboBox
          Left = 159
          Top = 16
          Width = 137
          Height = 21
          ItemHeight = 13
          ItemIndex = 0
          TabOrder = 0
          Text = 'VBScript'
          OnChange = ScriptLanguageCBChange
          Items.Strings = (
            'VBScript'
            'JScript')
        end
        object ProcBeginEdit: TEdit
          Left = 159
          Top = 43
          Width = 234
          Height = 21
          TabOrder = 1
        end
        object ProcEndEdit: TEdit
          Left = 159
          Top = 70
          Width = 234
          Height = 21
          TabOrder = 2
        end
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'About'
      ImageIndex = 3
      OnShow = TabSheet4Show
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label6: TLabel
        Left = 3
        Top = 14
        Width = 548
        Height = 19
        Alignment = taCenter
        AutoSize = False
        Caption = 'HID macros'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label7: TLabel
        Left = 3
        Top = 39
        Width = 548
        Height = 42
        AutoSize = False
        Caption = 
          'This utility can activate macros triggered by unique keyboard - ' +
          'key combination. When macro is fired, it can even send keystroke' +
          's to active window or SimConnect event to Flight Simulator.'
        WordWrap = True
      end
      object Label8: TLabel
        Left = 61
        Top = 87
        Width = 37
        Height = 13
        Alignment = taRightJustify
        Caption = 'Author:'
      end
      object Label9: TLabel
        Left = 43
        Top = 131
        Width = 55
        Height = 13
        Alignment = taRightJustify
        Caption = 'Homepage:'
      end
      object Label10: TLabel
        Left = 104
        Top = 87
        Width = 54
        Height = 13
        Caption = 'Petr Medek'
      end
      object Label11: TLabel
        Left = 104
        Top = 131
        Width = 124
        Height = 13
        Cursor = crHandPoint
        Caption = 'http://www.hidmacros.eu'
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clHotLight
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        OnClick = Label11Click
      end
      object Label12: TLabel
        Left = 59
        Top = 181
        Width = 39
        Height = 13
        Alignment = taRightJustify
        Caption = 'Version:'
      end
      object VersionLabel: TLabel
        Left = 104
        Top = 181
        Width = 26
        Height = 13
        Caption = 'beta'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label14: TLabel
        Left = 5
        Top = 263
        Width = 548
        Height = 26
        Alignment = taCenter
        AutoSize = False
        Caption = 
          'If you like this tool,  you can donate its development for next ' +
          'improvements.'
        WordWrap = True
      end
      object Panel3: TPanel
        Left = 244
        Top = 305
        Width = 74
        Height = 48
        Cursor = crHandPoint
        TabOrder = 0
        OnClick = Panel3Click
        object Image1: TImage
          Left = 1
          Top = 1
          Width = 72
          Height = 46
          Align = alClient
          Center = True
          Picture.Data = {
            07544269746D6170F60B0000424DF60B00000000000036040000280000003E00
            00001F0000000100080000000000C00700000000000000000000000100000000
            0000F5FBFF00E5F5FF002DADFF00A7A39D0064361B0084CFFE0095D5FE0055C2
            FE0035B1FF005FBFFF0066C3FF00DDF2FF00CAE5EC00B0D5E600C6DDE10044BA
            FF0028ABFF0076CAFF003DB5FF00A2D7ED007B63530039B2FF00F6FCFF00BEE6
            FF00D2EEFF00CFCCC000A9B9B700C7E9FF00BBE5FF00B8AA9300BDC8C6009E6C
            3A00BCA68B00A89B9200F1FAFF00C5C1BD006C422900D6E6E900A47D5200A4AF
            A800ADBDC1009C693500B2967400AFE0FF00EDF9FF00A99578006AC6FF00673C
            2300EBF1F300ECF9FF00BBEAFF00D4CEC600CBF0FE00D5D2C700CABDAB0023A8
            FF0078CDFF00A1724200A8DDFF00E1F5FE00C6EEFF0022A8FF00B2ACA400B99B
            7A00B0E2FF00EAF8FF00C9EAFF002FAEFF007ECDFE00B8E1F000C5BAA900DDDC
            D500776B62006E483200B5E5FE00AC8962004CBDFF009F7D5600B5E2FF0044B6
            FF0081655200E3EBEE00F8FDFF00947A6A009C8576003AB4FF00C3B099008DD7
            FE00AB865E00CBD3D300CEEDFF009DDCFE002BACFF00B2E5FE00C8EEFF008672
            6300A48E6E00B9B4AE0032B0FF00A57A4C008DCCEC00EEFAFF00C3D1D300BBA8
            8E00E3E1DB00F0FAFF0094D9FF00FEFFFF0076544000B5BAB50083CDF7009CDB
            FE00BDA284003EB3FF007B5A4500AC8D68009E8C8000A1DDFE00D4F0FF00D5EF
            FF0086D1FE0082D2FE0088D2FA0095DCFF00BCB6A400734D370094817300A476
            470075533E00A0703E00D9F1FF004ABDFF0095D9FB00D6F6FF00886C5A00BEBB
            B600B2916B0091D4FF007D7C7800A6855E00E6F9FF0079564100A4DCFE00D1C9
            BC007F5E4A009BD8FE00A29A870086D4FF007DCFFF00C5CBC2009F7140008060
            4C006E462E00E8F6FF00FAFDFF00E9F6FF009B673200FBFDFF0026A9FF00A2DB
            FE00FBFEFF0022A7FF0026AAFF00EFF9FF00CEEBFF00D5E0DC00C3B9A700A594
            880057BCFF00C9C7BA00D6EAF2008274690087786D0092A8B100E9F5F800BFBE
            BA0089705F00B89F8000C6C6C400B7E7FE00C3CAC800CFC2AF005FB3E00093D5
            F5006FC8FF009ED8F60099D7FE00C6EBFB006AA1BA00C2E9FF00D8F1F800C2BB
            B600A3DBF800A2DCF90094CCE500E8EEEF00A0908400ABB0AC00D0DDE000B1A4
            8C00A9E0FE00A1E2FF0060C7FD00AECCD5009EE1FF0087CFFF008F918D00F0F5
            F6008A969900AC916F0072C8FE00AECACF00F2F9FC00E9F7FF00E4F1F300D6F1
            F900D0F0FF00A48A6700039AFF00E0E4E000B38F6800B6B3AD00EAF7FF0092D3
            FF00B7B4A300CDCFCF00CED9D400967F70008CC2DD0084675500D9DADA00D1E8
            EF0075584700E7F7FF00C9C1B000E1F2F80098DEFF00BDB7B200A6805500A880
            5500D2D5D00049B7FF008ED2FF003CB2FF006AC4FF0027AAFF0030ADFF007BCB
            FF0059BDFF00A8DCFF00CDEBFF00BCE4FF00A5DBFF00663312000099FF00FFFF
            FF00FFFF00FFFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFE
            FEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFF00FFFF
            0000FFFFFE0989FCF9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9
            F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9FC8909FEFFFF
            0000A3FE061742FA5AFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFA
            FAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFA5AFA421706FEA3
            0000FFA84EFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFB
            FBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFB4EA8FF
            0000FECDFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFC
            FCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCCDFE
            0000FECDF2F2F2F2F2F2F2F2F2F2F2F2F2F2F2FDFDFDFDF2F2F2FDFDF2F2FDF2
            F2FDF2FDFDFDFDF2FDFDF2F2FDFDFDF2F2F2F2F2F2F2F2F2F2F2F2F2F2F2CDFE
            0000FE11F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7FDF7F7F7FDF7FDF7F7FDF7FDF7
            F7FDF7FDF7F7FDF7FDF7F7FDF7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F711FE
            0000FE0AF4F4F4F4F4F4F4F4F4F4F4F4F4F4F4FDF4F4F4FDF4FDF4F4FDF4FDF4
            F4FDF4F4FDFDFDF4FDF4F4FDFDFDFDF4F4F4F4F4F4F4F4F4F4F4F4F4F4F40AFE
            0000FEA8F8F8F8F8F8F8F8F8F8F8F8F8F8F8F8FDF8F8F8FDF8FDF8F8FDF8FDF8
            F8FDF8F8F8F8FDF8FDF8F8FDF8F8FDF8F8F8F8F8F8F8F8F8F8F8F8F8F8F8A8FE
            0000FE4FF1F1F1F1F1F1F1F1F1F1F1F1F1F1F1FDF1F1F1FDF1F1FDFDF1F1FDFD
            FDFDF1FDFDFDFDFDFDFDF1F1FDFDF1F1F1F1F1F1F1F1F1F1F1F1F1F1F1F14FFE
            0000FE15F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3FDFDFDFDF3F3F3717171717171
            71F3F3F3F3F3F3F3FDF3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F315FE
            0000FE02F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F60808F3551515
            08F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F602FE
            0000FE9EF5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5105C430855121255
            624310F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F59EFE
            0000FEA13737373737379E9EF5F5A29E9E379E9EF5105C5C5C4308120F0F0F12
            12625CF5A29E9E37379E9EF5105C5C5C1010F5F5A29E3737373737373737A1FE
            0000FEA137373737379E105C4343025C10F510026208155555120F4F482F8ACA
            4C12084343025C10F51002620815151515086262435C109E373737373737A1FE
            0000FEA1373737379E1043085512550843024308120F834C4C4C07CABC042FB6
            CA070F1212156243024308120F4C4C4C830F0F0F550843109E3737373737A1FE
            0000FE3DA2F1090A2EB811386E6E7994383838947964E4847BB7ECC913480448
            CC7B577A7A9494383838949364C27B7B846A577A799411B82EF40AF84F373DFE
            0000FE15D244050578F26A6F1449D0C15B6F5B75AD7D24E8ABE8CE32457E042F
            28325D924D1AC05B6F5B75272681D9604D6DD34D92B96A8978050505440A02FE
            0000FEF706060606069175408004860DB35DB30DAC049703860472340C500404
            50343C2D9C2D45B35D5D45D99CD1E08B9CC71E9626CBC875910606060689D2FE
            0000FE919F9F9F9F8EC84032AC249828345E3CBBAC045F856698490EE77D0404
            2FC5857C1F7F0E343C3C5E8B9C1D8595967395731F6DB3403A8E9F9F9F9FBAFE
            0000FE3A2B2B2B2B404E32BD038D046C505F210C66902FE5B02F986DE704247E
            046CBEE281298B4B2A7CD795EE1FD17329EEA9679C1D3C174A402B2B2B2B3AFE
            0000FE4E1C1C1C1C1C171B5A1EE5042F980404B0B4C63EC47E722FC459047DC6
            8D04033B639C29399C1F2AA5A54667B1631FA6A69C2AD842BD171C1C1C1C4EFE
            0000FE171B1B1B1B1B425A76E77E0486AF619004E3593E873E9704866104908C
            DD049825889C2A19EA639C5635A91936EF9C1D199C58AA765A421B1B1B1B17FE
            0000FE42181818181818760B3B210486D6653E0472A797808D2F7D212104538C
            EBE504A720292A6565209C732058EF2629638F479663F00B82771818181842FE
            0000FE770B0B0B0B0B0B0B3BD58798242123E3048DBFDD54E3C4ED512374B2E9
            D5E1C403468181B533889C583336B13F678FAEDB581F3601010B0B0B0B0B18FE
            0000FE0B999999999999D5DE31E650042F2F0498543065652C2C2C2C31313141
            31312C65687F9C1F1F9C39563069652C2C6565C3709C705141D5999999990BFE
            0000FE18A3A3A3A3A3A3A3692230549898247D53332200222222222222222222
            22222200CFDC7F8181633F6822002222222222D4367F704722A3A3A3A3A35AFE
            0000FF0600000000000000001616165252525252161616160000000000000000
            000016161616525252521616161616000016161616161616000000000000DFFF
            0000FFFEA42C529A9A9A9A9A9A9D9D9DA0A0A09D9D9A9A9A9A9A9A9A9A9A9A9A
            9A9A9A9A9D9D9DA0A0A09D9D9A9A9A9A9A9A9A9A9D9D9D9A9A9A9A522CA4FEA0
            0000FFFFFE06420199999999999999999999999B9B999999999B9B99999B9B99
            99999999999B999B9B99999B99999B9B999B9B99999B99999999014206FEFFFF
            0000FFFFFFFFFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFE
            FEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEDAFEFF6BFFFF
            0000}
          OnClick = Panel3Click
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 73
          ExplicitHeight = 49
        end
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'Help'
      ImageIndex = 4
      OnShow = TabSheet7Show
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object RichEdit1: TRichEdit
        Left = 0
        Top = 0
        Width = 551
        Height = 375
        Align = alClient
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
  end
  object SplashPanel: TPanel
    Left = 324
    Top = 135
    Width = 185
    Height = 69
    BevelInner = bvLowered
    BevelKind = bkFlat
    BevelWidth = 2
    Caption = 'Press a key or perform mouse event'
    TabOrder = 3
    Visible = False
  end
  object ActionManager: TActionManager
    ActionBars = <
      item
        Items = <
          item
            Action = StartEvent
            Caption = '&SimConnect'
            ImageIndex = 2
          end
          item
            Caption = '-'
          end
          item
            Action = FileExit
            ImageIndex = 0
          end
          item
            Caption = '-'
          end
          item
            Action = CompileAllAction
            Caption = '&Compile All'
            ImageIndex = 5
          end
          item
            Caption = '-'
          end
          item
            Action = MapToggleAction
            Caption = '&Map window'
          end
          item
            Caption = '-'
          end
          item
            Action = SaveAction
            Caption = 'S&ave configuration'
            ImageIndex = 9
          end>
        ActionBar = ActionToolBar
      end
      item
        Items = <
          item
            Action = NewMacroAction
            Caption = '&New'
            ImageIndex = 4
          end
          item
            Action = DeleteMacroAction
            Caption = '&Delete'
            ImageIndex = 3
          end
          item
            Action = FindAction
            Caption = '&Find'
            ImageIndex = 6
          end>
        ActionBar = ActionToolBar1
      end
      item
        Items = <
          item
            Action = RenameDeviceAction
            Caption = '&Rename'
            ImageIndex = 7
          end
          item
            Action = MoveMacrosAction
            Caption = '&Move macros'
            ImageIndex = 8
          end>
        ActionBar = DevicesActionToolBar
      end>
    Images = Images
    Left = 480
    Top = 120
    StyleName = 'XP Style'
    object FileExit: TFileExit
      Category = 'File'
      Caption = 'E&xit'
      Hint = 'Exit|Quits the application'
      ImageIndex = 0
    end
    object StartEvent: TAction
      Category = 'SimConnect'
      Caption = 'SimConnect'
      ImageIndex = 2
      OnExecute = StartEventExecute
    end
    object NewMacroAction: TAction
      Caption = 'New'
      ImageIndex = 4
      OnExecute = NewMacroActionExecute
    end
    object DeleteMacroAction: TAction
      Caption = 'Delete'
      Enabled = False
      ImageIndex = 3
      OnExecute = DeleteMacroActionExecute
    end
    object CompileAllAction: TAction
      Caption = 'Compile All'
      ImageIndex = 5
      OnExecute = CompileAllActionExecute
    end
    object FindAction: TAction
      Caption = 'Find'
      ImageIndex = 6
      OnExecute = FindActionExecute
    end
    object MapToggleAction: TAction
      Caption = 'Map window'
      OnExecute = MapToggleActionExecute
    end
    object RenameDeviceAction: TAction
      Category = 'Devices'
      Caption = 'Rename'
      ImageIndex = 7
      OnExecute = RenameDeviceActionExecute
    end
    object MoveMacrosAction: TAction
      Category = 'Devices'
      Caption = 'Move macros'
      ImageIndex = 8
      OnExecute = MoveMacrosActionExecute
    end
    object SaveAction: TAction
      Category = 'File'
      Caption = 'Save configuration'
      ImageIndex = 9
      OnExecute = SaveActionExecute
    end
  end
  object Images: TImageList
    Left = 512
    Top = 144
    Bitmap = {
      494C01010A000E00040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000004000000001002000000000000040
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007F7F7F000000
      00007F7F7F007F7F7F0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
      0000000000000000000000000000FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000BFBFBF000000
      0000BFBFBF00BFBFBF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
      FF000000FF000000FF0000000000FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000BFBFBF00BFBF
      BF00BFBFBF00BFBFBF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
      FF000000FF000000FF0000000000FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
      FF000000FF000000FF0000000000FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
      0000000000000000000000000000FFFFFF0000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00000000000000
      0000FFFFFF000000000000000000BFBFBF0000000000FF000000FF000000FF00
      00000000FF00FF000000FF000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF000000
      00000000000000000000FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      0000000000000000000000000000FFFFFF0000000000FFFFFF00000000000000
      00000000000000000000FFFFFF0000000000FFFFFF00000000000000FF000000
      FF000000FF000000FF000000FF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFF0000FFFF
      0000FFFF00000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF0000000000FFFFFF00000000000000
      0000FFFFFF000000000000000000000000000000000000000000000000000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFF0000FFFF
      0000FFFF00000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF00FFFFFF000000000000000000000000000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFF0000FFFF
      0000FFFF00000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF0000000000FFFFFF0000000000BFBF
      BF00FFFFFF0000000000FFFFFF000000000000000000000000007F7F7F000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000FF000000FF000000FF000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C2A6A400C2A6
      A400C2A6A400C2A6A400C2A6A400C2A6A400C2A6A400C2A6A400C2A6A400C2A6
      A400C2A6A400C2A6A4000000000000000000FAF8F000FCFAF200FFFFF600FFFF
      F700FFFFF700FFFFF400FFFFF400FFFFF200FFFFED00FFFFF100FFF5E400FFFF
      F200FFFFF400FFFDEE00FFFFF500FFFDF0000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007F7F
      7F007F7F7F00000000007F7F7F007F7F7F007F7F7F007F7F7F007F7F7F007F7F
      7F00000000007F7F7F007F7F7F00000000000000000000000000C2A6A400FEFC
      FB00FEFCFB00FEFCFB00FEFCFB00FEFCFB00FEFCFB00FEFCFB00FEFCFB00FEFC
      FB00FEFCFB00C2A6A4000000000000000000FFFFFB006E686100453F38004A43
      3A0041372D0064584C0053483A004B3E2E005C4F3F00625545005B4C3C003F32
      24005F544600595043004B423500544B3E000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000007F7F7F000000000000000000C2A6A400FEFC
      FB00FEFCFB00FEFCFB00FEFCFB00FEFCFB00FEFCFB00FEFCFB00FEFCFB00FEFC
      FB00FEFCFB00C2A6A4000000000000000000FFF4F200483B3900FAEBE800FFF1
      EB00FFF0E900F5E2DA00F7E3D800FFF8EB00E5D2C500F0DDD000FFEBDF00FAE6
      DB00FCE8DD00E0CDC500F1DED6005D4C43000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000BFBFBF007F7F7F00000000007F7F7F000000000000000000C2A6A400FEFA
      F500FEFCFB00FEFAF500FEFAF500FEFCFB00FEFAF500FEFAF500FEFCFB00FEFA
      F500FEFAF500C2A6A4000000000000000000FFFDFE006E5C5D00FFEBEA008570
      6E00F8E2DD007D655F00FFEBE400573C3200FFFDF300573D3100F9DED400553C
      3200F2D8D1006E574F00FBE3DD005C443E00FFFF0000000000000000000000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000BFBF
      BF00BFBFBF00BFBFBF0000000000000000000000000000000000C2A6A400FEFA
      F500FEFAF500FEFAF500FEFAF500FEFAF500FEFAF500FEFAF500FEFAF500FEFA
      F500FEFAF500C2A6A4000000000000000000FFF7F900705A5C00FDE5E500FFFD
      FB00FFFDF900FFEAE500FFF4ED00FFF7EE00FDDED500FFFEF500FFF9F200FFF0
      E900FFE8E300FFEEE900FFE9E50050353100FFFF00000000000000FFFF00FFFF
      FF0000FFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008080000080
      8000BFBFBF000000000000000000000000000000000000000000C2A6A400FEF7
      F000FEF7F000FEF7F000FEF7F000FEF7F000FEF7F000FEF7F000FEF7F000FEF7
      F000FEF7F000C2A6A4000000000000000000FFFEFC0065585600FFFEFB008171
      6B00FFFEF90098857D00FFEAE20078645900FFEFE3006A544800FFF4E9005C48
      3D00FFF9F100624F4700E7D4CD006A585100FFFF000000000000FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000008080000080800000FF
      FF00000000000000000000000000000000000000000000000000C2A6A400FEF7
      F000FEF7F000FEF7F000FEF7F000FEF3E900FEF7F000FEF7F000FEF3E900FEF7
      F000FEF7F000C2A6A4000000000000000000FBFFFB00555D5300F6FEF300F4FB
      EE00FFFFF700ECEFDF00FFFFF400F3F3E100FFFFF100E9EAD600FFFFF200ECEE
      DB00FFFFF100E7EADA00FFFFF5004F524300FFFF00000000000000FFFF00FFFF
      FF0000FFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000008080000080800000FFFF000000
      0000000000000000000000000000000000000000000000000000C2A6A400FEF3
      E900FEF3E900FEF3E900FEF3E900FEF3E900FEF3E900FEF3E900FEF3E900FEF3
      E900FEF3E900C2A6A4000000000000000000FFFFF80077756A00FCF9EB00FFFF
      F400E9E2D100605A4700FFFFEE00FFFFEC00FFFFEE00FFFCE600FFF9E5006A61
      4D00F6EFDC00FFFFF100FFFFF20049423100FFFF000000000000FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FF
      FF0000000000000000000000FF000000FF000000000000000000000000000000
      0000000000000000000000000000008080000080800000FFFF00000000000000
      0000000000000000000000000000000000000000000000000000C2A6A400FFF0
      E200FFF0E200FEF3E900FFEEDE00FEF3E900FFEEDE00FEF3E900FFEEDE00FEF3
      E900FFEEDE00C2A6A4000000000000000000FFFAF100A2776E00FFFDF200FFF6
      E900AC7D6F00AE806F0086544200FFFFED00FFFAE600FFFFEB00B7857100A775
      63008F5E4E00FFF4E400FFFEF0007B4C3E00FFFF00000000000000FFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF000000FF000000000000000000000000000000
      00000000000000000000008080000080800000FFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000C2A6A400FEF3
      E900FFEEDE00FFF0E200FEF3E900FFEEDE00FFF0E200DDCFC200DDCFC200DDCF
      C200DDCFC200C2A6A4000000000000000000FFFCF2009E6D6300FFFCF200AC78
      6C00D5A19100B9837200AE76650092584500FFFEEB00B2796400CB927D00C088
      7500B47C6B009B655400FFE4D5008854440000000000000000000000000000FF
      FF00FFFFFF0000FFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000008080000080800000FFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C2A6A400FFEE
      DE00FFEEDE00FFEEDE00FFEEDE00FFEEDE00FFEEDE00C3B4A800C3B4A800C3B4
      A800C3B4A800C2A6A4000000000000000000FFEEE2009F8A7B00FFF7E800FFFF
      F100FFF4E3009D826E00FFFFED00FFF8E100FFFFEB00FFFFEB00FFFFEB00A285
      7000FFF3DE00FFFFEE00FFFFF000735948000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000008080000080800000FFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C2A6A400FFEE
      DE00FFEAD700FFEEDE00FFEAD700FFEAD700FFEEDE00B0A29600B0A29600B0A2
      9600B0A29600C2A6A4000000000000000000FFFFF700FFFFF700FFFDF000FFFF
      F400FFFBEA008C806E00FFFDEA00FFFFF00055483200FFFFEA00FFFFF0008173
      6000FFFFF100FFFFF100FFF7E600FFFFEE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000080
      80000000000000FFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C2A6A400FFEA
      D700FFEAD700FFEAD700FFEAD700FFEAD700C9B9AC00FEFAF500FEF7F000E6DA
      D900C2A6A400000000000000000000000000F6F5F100F5F4F00058555000F5F2
      EA00FFFFF800FFFFF700372F2200FFFFF400FFFFF400FEF5E7005C534500FFFA
      EB00FFFFF500FFFFF5005C584D00FFFFF7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000FFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C2A6A400FFEA
      D700FFE6D000FFEAD700FFE6D000FFEAD700C5B5A900FEFAF500DDCFC200C2A6
      A40000000000000000000000000000000000FDFEFF00FDFFFF00FFFFFF00FFFF
      FE0059555400FFFFFB00FFFFFB00FFFEF700645C5500FFFFF800FCF4ED00FFFF
      F90047433E00F8F5F000E8E5E100FFFFFC000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000080000000FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C2A6A400FFE6
      D000FFE6D000FFE6D000FFE6D000FFE6D000C9B9AC00DDCFC200C2A6A4000000
      000000000000000000000000000000000000E7E8F200FBFCFF007E7D8600F1EF
      F500FFFDFF00F6F1F30079727500F4EEEF00F3EBEB00FFFEFE006D656600FFF9
      FA00FFFDFF00FFFEFF005F595E00FFFDFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000FF000000FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C2A6A400C2A6
      A400C2A6A400C2A6A400C2A6A400C2A6A400C2A6A400C2A6A400000000000000
      000000000000000000000000000000000000FBFBFF00F7F7FF00F8F6FF00FDFC
      FF00F7F1FC00FFFDFF00FFFCFF00FFFDFF00FFFDFF00FFF7FA00F9F2F700FFFD
      FF00F8F1F800FCF5FC00FFFCFF00F5F0F9000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008484840084848400848484008484
      8400848484008484840084848400848484008484840084848400FFFFFF00C6C6
      C60084848400C6C6C600FFFFFF00C6C6C6000000000000000000000000000000
      0000000000006D332700853C130095440D0096450D00873D1200703425000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000004490600055B0900066C0C00066C0C00055E0A00044C06000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000732DE000732DE008484840084848400848484008484
      840084848400848484008484840084848400848484008484840084848400FFFF
      FF0084848400FFFFFF0084848400848484000000000000000000000000007037
      2A0070372A00CD772700E8AD7000F3CCA100F4CDA300E9B17600D07C2C006F35
      29006F3529000000000000000000000000000000000000000000000000000560
      0900056009000891130009B0180009B31A0009B3190009B11900079614000568
      0C0005680C00000000000000000000000000000000000732DE000732DE000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000732DE000732DE00000000008400000084000000840000008400
      00000000000000000000848484008484840084848400FFFFFF00FFFFFF00FFFF
      FF0084000000840000008400000084000000000000000000000086411D00C062
      0B00F0C29200FFFEFA00FDFAF600F5E3D100F5E2D000FDF8F400FFFFFD00F2C9
      9E00C66911007B3A2100000000000000000000000000000000000A6A15000A7F
      15000BB61C0009B91A0008B4180007B2160009B3190009B4190009B81A0009B9
      1A0007831000044D06000000000000000000000000000732DE000732DE000732
      DE00000000000000000000000000000000000000000000000000000000000000
      00000732DE000732DE0000000000000000000000000000000000000000008400
      0000FF00000084000000000000000000000084848400FFFFFF00FFFFFF00FFFF
      FF0084000000000000000000000000000000000000008C451C00C1610700F7DB
      BD00FFFEFE00E0A46B00CE6D1300C75C0000C9610000CE6E1200DE9D5F00FDFA
      F700FAE5CC00C6680D006F35280000000000000000000B6A15000F85220016BD
      340011B727000BB21C0007B1160008B1170009B2190009B2190009B2190009B4
      190009BA1A000784100006670C0000000000000000000732DE000732DD000732
      DE000732DE000000000000000000000000000000000000000000000000000732
      DE000732DE000000000000000000000000000000000000000000000000008400
      000084000000FF0000008400000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0084000000000000000000000000000000000000008C451C00ECBD8B00FFFF
      FF00DA8F4300C6560000FFFFFF00DC975100C75B0000CA620000C75B0000D583
      3300FDFAF800F3CB9F006F35280000000000000000000B6A150020BE49001BBD
      400014B730000AB21F0028BC3600DFF5E100EEFAEF0063CE6D0009B2190009B2
      190009B3190009BA1A0006670C000000000000000000000000000534ED000732
      DF000732DE000732DE00000000000000000000000000000000000732DE000732
      DE00000000000000000000000000000000000000000000000000000000008400
      0000FF00000084000000FF00000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0084000000000000000000000000000000A04D1000CE772100FFFDFB00E8B6
      8400D06B0400D06B0700FFFFFF00FFFFFF00E1A87000C95E0000CA630000C75B
      0000DFA06100FFFFFF00CF7B280070352500087210001B9A3A002AC65B001DBB
      45000EB425000BB31B0011B421009ADFA000FFFFFF00F7FDF8005ACB650009B2
      190009B2190009B81A0008941300045D09000000000000000000000000000000
      00000732DE000732DE000732DD00000000000732DD000732DE000732DE000000
      0000000000000000000000000000000000000000000000000000000000008400
      000084000000FF0000008400000000000000FFFFFF00FFFF0000FFFFFF00FFFF
      000084000000000000000000000000000000AF550700E5AA6F00FFFFFF00DD8F
      3F00DA812800D87B1E00FFFFFF00FFFFFF00FFFFFF00EAC19800CC670800C95F
      0000CE6E0D00FDFAF600E9B1750070352500087210002AB65B002CC5650022BD
      4D000FB422000AB21A000CB31C000AB219008DDB9500FDFEFD00F6FCF70058CB
      630009B2190009B51A0008AB1700045D09000000000000000000000000000000
      0000000000000732DD000633E6000633E6000633E9000732DC00000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000FF00000084000000FF00000000000000FFFF0000FFFFFF00FFFF0000FFFF
      FF0084000000000000000000000000000000BB5F0A00F0CAA100FCF4ED00E193
      4300E2924200DF8A3400FFFFFF00FFFFFF00FFFFFF00FFFFFF00F3DEC600CF70
      1700C95F0000F5E3D000F3CEA400703525000F821C0037C26C0033C76C00CDF1
      DA00C9EFD300C7EED000C8EFD200C5EED000C7EECF00F8FDF900FFFFFF00F2FB
      F3006FD2790008B4190009B3190005650B000000000000000000000000000000
      000000000000000000000633E3000732E3000534EF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      000084000000FF0000008400000000000000FFFFFF00FFFF0000FFFFFF00FFFF
      000084000000000000000000000000000000C1650F00F2CDA600FDF7F000E9A1
      5800E9A05600E6994A00FFFFFF00FFFFFF00FFFFFF00FFFFFF00EBC39B00CD6A
      0D00C9610000F6E6D400F3CCA10070352500138D230058CC830042C97700FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FDFEFD00FFFFFF00FFFF
      FF00BCEAC1000AB41A0009B31900066D0D000000000000000000000000000000
      0000000000000732DD000534ED000533E9000434EF000434F500000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000FF00000084000000FF00000000000000FFFF0000FFFFFF00FFFF0000FFFF
      FF0084000000000000000000000000000000C1640D00EEBC8800FFFFFF00F1B8
      7C00F0AE6900EEA75F00FFFFFF00FFFFFF00FFFFFF00E7B17A00CE690200C961
      0000CF711100FEFCFA00E7AC6D00703525000F911D006FD293005FD38D006DD4
      950072D6990071D6980072D6990064D28C0092DFA800FBFEFB00FFFFFF00ACE5
      B8002EBF4C0011B82B0008B1190005610A000000000000000000000000000000
      00000434F4000534EF000533EB0000000000000000000434F4000335F8000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000840000008400000084000000840000008400000084000000840000008400
      000084000000000000000000000000000000BF600600E5A05900FFFDFA00FBE0
      C400F8BA7B00F4B47100FFFFFF00FFFFFF00E8AB6D00D87B1D00D2741400C85C
      0000E2AA7100FFFFFE00CC752000703525000F911D0067CC83009BE5BA0038C6
      700030C3690038C56F0038C56F0070D69700E8F8EE00FFFFFF009FE2B10020BD
      48001AB93E0010BA290008A3170005610A000000000000000000000000000335
      FC000534EF000434F800000000000000000000000000000000000335FC000335
      FB00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C3620400FAD9B800FFFF
      FF00FEDCB800F7B87700FFFFFF00EBAD6D00E08D3700DA822800D06B0500DB92
      4A00FFFFFF00EFC08C006B342C00000000000000000025AE3900BCEDD20082DB
      A40028C063002FC2670053CD8200F7FDF900FFFFFF009CE2B20022BC4B001DBA
      410018B7360014C030000A8517000000000000000000000000000335FB000335
      FB000335FC000000000000000000000000000000000000000000000000000335
      FB000335FB000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C3620400E79E5500FEEB
      D700FFFFFF00FBDFC300F1B57800E89F5500E2944500DE914200E8B78600FFFF
      FF00F6D8B700BE5F06006B342C00000000000000000025AE390071D28C00D2F4
      E10080DAA30036C46D0039C56F00BCECCE00ABE6C2002DC2630024BE560023BC
      4D001FC1460016AE34000A85170000000000000000000335FB000335FB000335
      FB00000000000000000000000000000000000000000000000000000000000000
      0000000000000335FB0000000000000000000000000000000000000000000000
      0000000000000000000000840000008400000084000000840000000000000000
      0000000000000000000000000000000000000000000000000000C6670C00E69E
      5500FAD9B600FFFBF600FFFFFF00FEF8F200FDF6EF00FFFFFF00FEF9F200ECB8
      8400BE5F0900753826000000000000000000000000000000000025AE390084D8
      9F00DBF7EA00AFE8C6006BD4930052CC810044C9780049CA7B0048CB780039CB
      6A0021B649000F7C1F0000000000000000000335FB000335FB000335FB000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C060
      0500C0600500E49F5A00EEBA8600F2CAA000F0C59900E4A76800CC741E00783A
      2700783A270000000000000000000000000000000000000000000000000025AE
      390025AE3900ADE8C500CCF2DE00BAEDD100A6E7C20091E2B30064D492002FB1
      57002FB157000000000000000000000000000335FB000335FB00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000B65C0A00B8601200B9611300B25A0F00A24F0E008E451A000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000032B74E0025AE390025AE390025AE390025AE390024A342000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000400000000100010000000000000200000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000008000FF00000000008000FF0000000000
      C000FF0000000000E000FF0000000000F000000000000000F800000000000000
      FC00000000000000FE00000000000000FF00002300000000FF80000100000000
      838000000000000083E000230000000083E000630000000083E000C300000000
      8384010700000000FFFE03FF00000000C0030000FFFFE408C0030000FFF8FFF0
      C003000020F8FFE0C0030000007FFFC1C0030000007CFF83C0030000003CFF07
      C0030000000FFE0FC00300000004FC1FC0030000000CF83FC003000001FFF07F
      C0030000E3FCE0FFC0030000FFFCC1FFC0070000FFFF83FFC00F0000FFF807FF
      C01F0000FFF80FFFC03F0000FFFF9FFF0000F81FF81FFFFC0000E007E0079FF9
      0000C003C0038FF3E0078001800187E7E00780018001C3CFE00700000000F11F
      E00700000000F83FE00700000000FC7FE00700000000F83FE00700000000F19F
      E00700000000E3CFFFFF80018001C7E7F81F800180018FFBF81FC003C0031FFF
      F81FE007E0073FFFFFFFF81FF81FFFFF00000000000000000000000000000000
      000000000000}
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'exe'
    Filter = 'Applications|*.exe|All files|*.*'
    Left = 400
    Top = 176
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 50
    OnTimer = Timer1Timer
    Left = 496
    Top = 16
  end
  object TrayIcon1: TTrayIcon
    Hint = 'HidMacros'
    Icon.Data = {
      0000010001001010000001000800680500001600000028000000100000002000
      0000010008000000000000000000600000006000000000010000000000000D0D
      0D0013131300171717001E1E1E001F1F1F002121210022222200232323002424
      2400252525002727270028282800292929002A2A2A002B2B2B002C2C2C002D2D
      2D002E2E2E003030300031313100323232003333330034343400353535003636
      36003737370038383800393939003F3F3F00484848006A6A6A00717171007474
      7400757575007676760078787800797979007B7B7B007C7C7C007D7D7D007E7E
      7E000000FF008080800081818100828282008484840085858500868686008787
      870088888800898989008A8A8A008B8B8B008C8C8C0090909000929292009595
      9500999999009A9A9A00BCBCBC00C1C1C100C2C2C200C6C6C600C8C8C800CECE
      CE00D1D1D100D2D2D200D7D7D700D9D9D900DADADA00DBDBDB00DEDEDE00E1E1
      E100E3E3E300E4E4E400E5E5E500E6E6E600E7E7E700E8E8E800E9E9E900EAEA
      EA00EBEBEB00F2F2F200F4F4F400F6F6F600F9F9F900FBFBFB00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000005858
      5858585858585858585858585858585858585858585858585858585858585858
      58585858585858585858585858585817140D001C120C10090E0410120A582E24
      2D32282327342C2C2B28352D271535552757441A575357545006573D3009234F
      21161801190B160B041B07182B0E2F4A33574211485606575002573B310C2B51
      330311140B06130A0F061605310B2C4B20573F08574D0752401D563E2C0F3445
      39263825252035363A1E3722270E33484E504947574C4643504F41573C0C582A
      27351F282F2D343A232D2E302C58585858585858585858585858585858585858
      585858585858585858585858585858585858585858585858585858585858FFFF
      0000FFFF0000FFFF000080010000000000000000000000000000000000000000
      000000000000000000000000000080010000FFFF0000FFFF0000FFFF0000}
    OnClick = TrayIcon1DblClick
    OnDblClick = TrayIcon1DblClick
    Left = 392
    Top = 24
  end
  object ApplicationEvents1: TApplicationEvents
    OnMinimize = ApplicationEvents1Minimize
    Left = 440
    Top = 16
  end
  object Timer2: TTimer
    Enabled = False
    Interval = 500
    OnTimer = Timer2Timer
    Left = 424
    Top = 40
  end
end
