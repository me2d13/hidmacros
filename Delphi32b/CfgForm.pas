unit CfgForm;


interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ImgList, ToolWin, ActnMan, ActnCtrls, XPStyleActnCtrls,
  ActnList, ComCtrls, SimConnect, StdActns, uRawInput, ExtCtrls, KbdMacro,
  Buttons, GameDevDialog, Localizer, MouseDialog, AppEvnts, xmldom,
  XMLIntf, msxmldom, XMLDoc, MSScriptControl_TLB, FSXcontrol, uScriptEngine,
  FSUIPCcontrol, uGameDevices, Grids, uHookCommon, MemMap, uConfig,
  uMidiDevices, MidiType;

const
  HookLib = 'Winhook.dll';

const
  WM_SCANNED = WM_USER + 301;
  WM_FIRE = WM_USER + 310;
  WM_FINDMACRO = WM_USER + 311;

  InputEventBufferSize = 5000;


type

  TRawInputEventDirection = (edUp, edDown);
  TRawInputEventType = (etKeyboard, etMouse);
  TScanAction = (saDisabled, saDefine, saFind);

  PRawInputEvent = ^TRawInputEvent;
  TRawInputEvent = record
    EventType: TRawInputEventType;
    DeviceHandle: THANDLE;
    Code: Word;
    MessageId: Word;
    Direction: TRawInputEventDirection;
    Time: Integer;
  end;

  // The form
  THIDMacrosForm = class(TForm)
    StatusBar: TStatusBar;
    ActionManager: TActionManager;
    ActionToolBar: TActionToolBar;
    Images: TImageList;
    FileExit: TFileExit;
    StartEvent: TAction;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    ListView1: TListView;
    Panel1: TPanel;
    Splitter1: TSplitter;
    Panel2: TPanel;
    ActionToolBar1: TActionToolBar;
    NewMacroAction: TAction;
    MacrosLB: TListBox;
    DeleteMacroAction: TAction;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    TestKeyboardName: TEdit;
    Label2: TLabel;
    TestScanCode: TEdit;
    GroupBox2: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    ActionGB: TGroupBox;
    KeyboardRB: TRadioButton;
    SimConnectRB: TRadioButton;
    SequenceEdit: TEdit;
    SimconnectCB: TComboBox;
    NameEdit: TEdit;
    EditKeyboardName: TEdit;
    EditScanCode: TEdit;
    SetButton: TButton;
    SplashPanel: TPanel;
    TabSheet4: TTabSheet;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    VersionLabel: TLabel;
    SCTextChB: TCheckBox;
    Label13: TLabel;
    SCParamsEdit: TEdit;
    SCParCounLabel: TLabel;
    TabSheet5: TTabSheet;
    SysCommandRB: TRadioButton;
    CommandEdit: TEdit;
    OpenDialog1: TOpenDialog;
    FileOpenButton: TButton;
    Label14: TLabel;
    Panel3: TPanel;
    Image1: TImage;
    Timer1: TTimer;
    TrayIcon1: TTrayIcon;
    ApplicationEvents1: TApplicationEvents;
    TabSheet3: TTabSheet;
    RoutinesGroupBox: TGroupBox;
    ScriptTestGroupBox: TGroupBox;
    RoutinesEdit: TRichEdit;
    Panel5: TPanel;
    ScriptTestButton: TButton;
    TestRichEdit: TRichEdit;
    PageControl2: TPageControl;
    PredefinedActionTS: TTabSheet;
    ScriptedActionTS: TTabSheet;
    MacroScriptRichEdit: TRichEdit;
    Panel6: TPanel;
    TestMacroScriptB: TBitBtn;
    CheckMacroScriptB: TBitBtn;
    CompileAllAction: TAction;
    MacroErrImage: TImage;
    MacroOkImage: TImage;
    Label18: TLabel;
    Timer2: TTimer;
    SettingsTabSheet: TTabSheet;
    GeneralGroupBox: TGroupBox;
    ExperimentalGroupBox: TGroupBox;
    Label15: TLabel;
    LangComboBox: TComboBox;
    MinimizeToTrayCB: TCheckBox;
    StartMinimizedCB: TCheckBox;
    ScriptToolsTabSheet: TTabSheet;
    RichEdit1: TRichEdit;
    Label21: TLabel;
    ScriptTimeOutEdit: TEdit;
    AllowScriptGUICheckBox: TCheckBox;
    GroupBox5: TGroupBox;
    Label16: TLabel;
    ScriptLanguageCB: TComboBox;
    Label5: TLabel;
    Label22: TLabel;
    ProcBeginEdit: TEdit;
    ProcEndEdit: TEdit;
    SendToBufferRB: TRadioButton;
    ShowBufferChkB: TCheckBox;
    Label23: TLabel;
    BufferResetTOEdit: TEdit;
    Label24: TLabel;
    Panel4: TPanel;
    RoutinesOKImage: TImage;
    RoutinesErrImage: TImage;
    Label17: TLabel;
    LoadRoutinesButton: TButton;
    Label25: TLabel;
    PageControl3: TPageControl;
    TSMouseCoordinates: TTabSheet;
    Label19: TLabel;
    Label20: TLabel;
    EditX: TEdit;
    EditY: TEdit;
    TSWindowTitles: TTabSheet;
    WindowTitlesMemo: TMemo;
    TSGameAxis: TTabSheet;
    GameAxisStringGrid: TStringGrid;
    TSErrorLog: TTabSheet;
    ErrorLogMemo: TMemo;
    FindAction: TAction;
    XPLCommandRB: TRadioButton;
    XPLCommandEdit: TComboBox;
    MapToggleAction: TAction;
    Label26: TLabel;
    Label27: TLabel;
    NoOfBackupsEdit: TEdit;
    Label28: TLabel;
    BackupPathEdit: TEdit;
    SelectPathButton: TButton;
    DevicesActionToolBar: TActionToolBar;
    RenameDeviceAction: TAction;
    MoveMacrosAction: TAction;
    SaveAction: TAction;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure SimConnectMessage(var Message: TMessage); message
      WM_USER_SIMCONNECT;
    procedure StartEventExecute(Sender: TObject);
    procedure WmInputMessage(var Message: TMessage); message WM_INPUT;
    procedure WmHookMessage(var Message: TMessage); message WM_ASKHDMFORM;
    procedure WmFireMessage(var Message: TMessage); message WM_FIRE;
    procedure WmScannedMessage(var Message: TMessage); message WM_SCANNED;
    procedure WmFindMacroMessage(var Message: TMessage); message WM_FINDMACRO;
    procedure WmJoyButtonDown(var Message: TMessage); message WM_JOY_BUTTON_DOWN;
    procedure WmJoyButtonUp(var Message: TMessage); message WM_JOY_BUTTON_UP;
    procedure WmMidiKeyEvent(var Message: TMessage); message WM_MIDI_KEY;
    procedure ListView1Edited(Sender: TObject; Item: TListItem; var S: string);
    procedure FormDestroy(Sender: TObject);
    procedure NewMacroActionExecute(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure MacrosLBClick(Sender: TObject);
    procedure NameEditChange(Sender: TObject);
    procedure KeyboardRBClick(Sender: TObject);
    procedure SequenceEditChange(Sender: TObject);
    procedure SimconnectCBChange(Sender: TObject);
    procedure SetButtonClick(Sender: TObject);
    procedure DeleteMacroActionExecute(Sender: TObject);
    procedure Label11Click(Sender: TObject);
    procedure WindowCBChange(Sender: TObject);
    procedure SCTextChBClick(Sender: TObject);
    procedure SCParamsEditChange(Sender: TObject);
    procedure CommandEditChange(Sender: TObject);
    procedure FileOpenButtonClick(Sender: TObject);
    procedure Panel3Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure SaveScriptExecute(Sender: TObject);
    procedure LangComboBoxChange(Sender: TObject);
    procedure MinimizeToTrayCBClick(Sender: TObject);
    procedure StartMinimizedCBClick(Sender: TObject);
    procedure TrayIcon1DblClick(Sender: TObject);
    procedure ApplicationEvents1Minimize(Sender: TObject);
    procedure LoadRoutinesButtonClick(Sender: TObject);
    procedure ScriptTestButtonClick(Sender: TObject);
    procedure PageControl2Change(Sender: TObject);
    procedure CheckMacroScriptBClick(Sender: TObject);
    procedure MacroScriptRichEditChange(Sender: TObject);
    procedure TestMacroScriptBClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure RoutinesEditChange(Sender: TObject);
    procedure ScriptLanguageCBChange(Sender: TObject);
    procedure StatusBarDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
    procedure CompileAllActionExecute(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure TabSheet7Show(Sender: TObject);
    procedure ScriptTimeOutEditChange(Sender: TObject);
    procedure AllowScriptGUICheckBoxClick(Sender: TObject);
    procedure BufferResetTOEditChange(Sender: TObject);
    procedure ShowBufferChkBClick(Sender: TObject);
    procedure MacroScriptRichEditKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure MacroScriptRichEditMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure RoutinesEditKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure RoutinesEditMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FindActionExecute(Sender: TObject);
    procedure XPLCommandEditOldChange(Sender: TObject);
    procedure MapToggleActionExecute(Sender: TObject);
    procedure TabSheet4Show(Sender: TObject);
    procedure SelectPathButtonClick(Sender: TObject);
    procedure NoOfBackupsEditChange(Sender: TObject);
    procedure NoOfBackupsEditExit(Sender: TObject);
    procedure BackupPathEditChange(Sender: TObject);
    procedure RenameDeviceActionExecute(Sender: TObject);
    procedure ListView1Click(Sender: TObject);
    procedure ListView1AdvancedCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;
      var DefaultDraw: Boolean);
    procedure MoveMacrosActionExecute(Sender: TObject);
    procedure SaveActionExecute(Sender: TObject);
  private
    { Private declarations }
    Quit: boolean;                      // True when signalled to quit
    FHookSet: Boolean;
    LastDevId : HWND;
    LastMouseDevId : HWND;
    Scanning : TScanAction;
    IgnoreInputMessage : boolean;
    CfgFileName : String;
    mouseIgnoreStart: Integer;
    GameDevCounter: Integer;
    eb: TList;
    ignoreEvents: TList; // keyboard || moue ups and others
    MouseMovementMessageCounter: Integer;
    ScannedEvent: TRawInputEvent;
    GameDialog: TGameForm;
    MouseDialog: TMouseForm;
    Language: String;
    MinimizeToTray: Boolean;
    StartMinimized: Boolean;
    Lcl: TLocalizer;
    AllMacrosCompiled : Boolean;
    fBufferNotification: Boolean;
    fMMF: TMemMap;
    fSharedPtr: PMMFData;
    procedure RegisterEvent;
    procedure RefreshEditArea;
    procedure RecalculateSplashScreen;
    procedure HookKeyboard;
    procedure KeyboardNameChanged(kbd: THIDKeyboard);
    procedure SaveToIni;
    procedure LoadFromIni;
    procedure DebugLog(Value: String);
    procedure RegisterEvents2FS;
    function IncEbIndex(Value: Word; Limit: Word): Word;
    procedure LogEb;
    procedure BuildIgnoreList;
    function GetMessageId(Message: Word): String;
    function SearchEvent(WParam: Integer; LParam: Integer): Integer;
    procedure ShortenEB;
    function CheckFire(e: PRawInputEvent; m: TKeyboardMacro): Boolean;
    procedure UpdateTestArea;
    procedure GameButtonEvent(pDevice: THIDJoystick; pButton: Integer; pDirection: TRawInputEventDirection);
    procedure MidiEvent(pDevice: TMIDIDevice; pEvent: TMyMidiEvent);
    procedure InitLanguages;
    procedure ChangeLanguage(NewL: String);
    procedure ManageTray;
    function MessageDlgTranslated(const Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; DefaultButton: TMsgDlgBtn; HelpCtx: Longint = 0): Integer;
    procedure SCDisconnect(Sender: TObject);
    function CompileMacro(am: TKeyboardMacro) : Boolean;
    function CompileAll(CompileLog: TStrings) : Boolean;
    procedure RefreshCompileIndicators;
    procedure RefreshCompileMacroIndicators(am: TKeyboardMacro);
    procedure BufferChanged(Sender: TObject);
    function GetRECursorPos(pRichEdit: TRichEdit): String;
    procedure InitGameGrid;
    procedure FillGameGrid;
    procedure ScriptLogHandler(pValue: String);
    procedure MapFormToggle(Sender: TObject);
    procedure InitMMF;
    procedure FreeMMF;
    procedure DisplayDevices;
    procedure RecalcLinkedMacros;
    procedure LinkMacros2DevicesByNames;
    procedure RefreshDeviceActions;
  public
    { Public declarations }
    Txt: TStringList;
    procedure SaveToXml(RootNode, parentNode : IXMLNode);
    procedure LoadFromXml(RootNode: IXMLNode);
  end;

var
  HIDMacrosForm       : THIDMacrosForm;

function SetHook: Boolean; stdcall; external HookLib;
function FreeHook: Boolean; stdcall; external HookLib;

implementation

uses Inifiles, ShellAPI, ActiveX, uGlobals, uMapForm, FileCtrl, uHIDControl,
  uMoveMacrosForm;

{$R *.dfm}

function GetCharFromVirtualKey(Key: Word): string;
var
   keyboardState: TKeyboardState;
   asciiResult: Integer;
begin
   GetKeyboardState(keyboardState) ;

   SetLength(Result, 2) ;
   asciiResult := ToAscii(key, MapVirtualKey(key, 0), keyboardState, @Result[1], 0) ;
   case asciiResult of
     0: Result := '';
     1: SetLength(Result, 1) ;
     2:;
     else
       Result := '';
   end;
end;

function EnumWindowsProc(WHandle: HWND; lParam: LPARAM): BOOL; stdcall;
const
  MAX_WINDOW_NAME_LEN = 120;
var
  WindowName : array[0..MAX_WINDOW_NAME_LEN] of char;
  Res : TStrings;
  Wstyle: LongInt;
begin
  GetWindowText(WHandle,WindowName,MAX_WINDOW_NAME_LEN);
  WStyle := GetWindowLong(WHandle, GWL_STYLE);
  Res := TStrings(lParam);
  if (Res <> nil) and (Res is TStrings) and
     (StrLen(WindowName) > 0) and (Wstyle and WS_VISIBLE > 0) then
    Res.Add(WindowName);
  Result := True;
end;

procedure PostKeyExHWND(hWindow: HWnd; key: Word; const shift: TShiftState;
  specialkey: Boolean);
{************************************************************
* Procedure PostKeyEx
*
* Parameters:
*  hWindow: target window to be send the keystroke
*  key    : virtual keycode of the key to send. For printable
*           keys this is simply the ANSI code (Ord(character)).
*  shift  : state of the modifier keys. This is a set, so you
*           can set several of these keys (shift, control, alt,
*           mouse buttons) in tandem. The TShiftState type is
*           declared in the Classes Unit.
*  specialkey: normally this should be False. Set it to True to
*           specify a key on the numeric keypad, for example.
*           If this parameter is true, bit 24 of the lparam for
*           the posted WM_KEY* messages will be set.
* Description:
*  This
procedure sets up Windows key state array to correctly
*  reflect the requested pattern of modifier keys and then posts
*  a WM_KEYDOWN/WM_KEYUP message pair to the target window. Then
*  Application.ProcessMessages is called to process the messages
*  before the keyboard state is restored.
* Error Conditions:
*  May fail due to lack of memory for the two key state buffers.
*  Will raise an exception in this case.
* NOTE:
*  Setting the keyboard state will not work across applications
*  running in different memory spaces on Win32 unless AttachThreadInput
*  is used to connect to the target thread first.
*Created: 02/21/96 16:39:00 by P. Below
************************************************************}
type
  TBuffers = array [0..1] of TKeyboardState;
var
  pKeyBuffers: ^TBuffers;
  lParam: LongInt;
begin
  (* check if the target window exists *)
  if IsWindow(hWindow) then
  begin
    (* set local variables to default values *)
    pKeyBuffers := nil;
    lParam := MakeLong(0, MapVirtualKey(key, 0));
    (* modify lparam if special key requested *)
    if specialkey then
      lParam := lParam or $1000000;
    (* allocate space for the key state buffers *)
    New(pKeyBuffers);
    try
      (* Fill buffer 1 with current state so we can later restore it.
         Null out buffer 0 to get a "no key pressed" state. *)
      GetKeyboardState(pKeyBuffers^[1]);
      FillChar(pKeyBuffers^[0], SizeOf(TKeyboardState), 0);
      (* set the requested modifier keys to "down" state in the buffer*)
      if ssShift in shift then
        pKeyBuffers^[0][VK_SHIFT] := $80;
      if ssAlt in shift then
      begin
        (* Alt needs special treatment since a bit in lparam needs also be set *)
        pKeyBuffers^[0][VK_MENU] := $80;
        lParam := lParam or $20000000;
      end;
      if ssCtrl in shift then
        pKeyBuffers^[0][VK_CONTROL] := $80;
      if ssLeft in shift then
        pKeyBuffers^[0][VK_LBUTTON] := $80;
      if ssRight in shift then
        pKeyBuffers^[0][VK_RBUTTON] := $80;
      if ssMiddle in shift then
        pKeyBuffers^[0][VK_MBUTTON] := $80;
      (* make out new key state array the active key state map *)
      SetKeyboardState(pKeyBuffers^[0]);
      (* post the key messages *)
      if ssAlt in Shift then
      begin
        PostMessage(hWindow, WM_SYSKEYDOWN, key, lParam);
        PostMessage(hWindow, WM_SYSKEYUP, key, lParam or $C0000000);
      end
      else
      begin
        PostMessage(hWindow, WM_KEYDOWN, key, lParam);
        PostMessage(hWindow, WM_KEYUP, key, lParam or $C0000000);
      end;
      (* process the messages *)
      Application.ProcessMessages;
      (* restore the old key state map *)
      SetKeyboardState(pKeyBuffers^[1]);
    finally
      (* free the memory for the key state buffers *)
      if pKeyBuffers <> nil then
        Dispose(pKeyBuffers);
    end; { If }
  end;
end; { PostKeyEx }

procedure SendKeysToWin(wh: HWND; buffer: string);
var i:integer;
    w:word;
    D:DWORD;
    P:^DWORD;
begin
  P:=@D;
  SystemParametersInfo(                      //get flashing timeout on win98
         SPI_GETFOREGROUNDLOCKTIMEOUT,
         0,
         P,
         0);
  SetForeGroundWindow(wh);
  for i:=1 to length(buffer) do
  begin
    w:=VkKeyScan(buffer[i]);
    keybd_event(w,0,0,0);
    keybd_event(w,0,KEYEVENTF_KEYUP,0);
  end;
  SystemParametersInfo(                     //set flashing TimeOut=0
         SPI_SETFOREGROUNDLOCKTIMEOUT,
         0,
         nil,
         0);
  SetForegroundWindow(HIDMacrosForm.Handle);
                                            //->typecast working...
  SystemParametersInfo(                     //set flashing TimeOut=previous value
         SPI_SETFOREGROUNDLOCKTIMEOUT,
         D,
         nil,
         0);
end;


procedure THIDMacrosForm.AllowScriptGUICheckBoxClick(Sender: TObject);
begin
  Glb.ScriptEngine.AllowGUI := AllowScriptGUICheckBox.Checked;
end;

procedure THIDMacrosForm.ApplicationEvents1Minimize(Sender: TObject);
begin
  if (MinimizeToTray) then
    Hide;
end;

procedure THIDMacrosForm.BackupPathEditChange(Sender: TObject);
begin
  Glb.AppConfig.BackupPath := BackupPathEdit.Text;
end;

procedure THIDMacrosForm.BufferChanged(Sender: TObject);
begin
  if fBufferNotification then
    if MinimizeToTray then
    begin
      TrayIcon1.BalloonHint := Glb.Buffer.Value;
      TrayIcon1.ShowBalloonHint;
    end
    else
      Application.Title := Glb.Buffer.Value + ' HidMacros';
end;

procedure THIDMacrosForm.BufferResetTOEditChange(Sender: TObject);
begin
  Glb.Buffer.ResetInterval := StrToIntDef(BufferResetTOEdit.Text, 0);
end;

procedure THIDMacrosForm.BuildIgnoreList;
var I, J: Integer;
  Found: Boolean;
  mr, m2: TKeyboardMacro;
begin
  for I := 0 to ignoreEvents.Count - 1 do
    TKeyboardMacro(ignoreEvents.Items[I]).Free;
  ignoreEvents.Clear;
  for I := 0 to MacrosLB.Count - 1 do
  begin
    mr := TKeyboardMacro(MacrosLB.Items.Objects[I]);
    // only for keyboard/mouse macros and when Device is assigned
    if (mr.Keyboard <> nil) and ((mr.MouseEvent <> None) or (mr.KeyCode > 0)) then
    begin
      // now look for event with opposite direction
      Found := False;
      for J := 0 to MacrosLB.Count - 1 do
      begin
        m2 := TKeyboardMacro(MacrosLB.Items.Objects[J]);
        if (m2.Keyboard <> nil) then
          if (m2.Keyboard.Handle = mr.Keyboard.Handle) and (
            //mouse part
            ( (mr.MouseEvent <> None) and
              (m2.MouseEvent = mr.MouseEvent) and
              (m2.Direction <> mr.Direction)
             ) or
             // keyboard part
            ( (mr.KeyCode > 0) and
              (m2.KeyCode = mr.KeyCode) and
              (m2.Direction <> mr.Direction)
             )) then
          begin
            Found := True;
            break;
          end;
      end;
      if not Found then
      begin
        // create macro with oposite direction to ignore list
        m2 := TKeyboardMacro.Create;
        m2.Keyboard := mr.Keyboard;
        m2.MouseEvent := mr.MouseEvent;
        m2.KeyCode := mr.KeyCode;
        if mr.Direction = mdDown then
          m2.Direction := mdUp
        else
          m2.Direction := mdDown;
        ignoreEvents.Add(m2);
      end;
    end;
  end;
end;

procedure THIDMacrosForm.ChangeLanguage(NewL: String);
var
  l: TStrings;
  I: Integer;
begin
  Lcl.Language := NewL;
  if (FileExists(ExtractFilePath(Application.ExeName)+'\lang\'+NewL+'.rtf')) then
    RichEdit1.Lines.LoadFromFile(ExtractFilePath(Application.ExeName)+'\lang\'+NewL+'.rtf');
  l := TStringList.Create;
  try
    // this form
    l.Add('SetButton=SetB');
    l.Add('TabSheet1=Devices');
    l.Add('NameCol=NameCol');
    l.Add('TypeCol=TypeCol');
    l.Add('SystemID=SysCol');
    l.Add('TabSheet2=Macros');
    l.Add('GroupBox1=TestArea');
    l.Add('Label1=Device');
    l.Add('Label2=Event');
    l.Add('GroupBox2=EditMacro');
    l.Add('Label3=Name');
    l.Add('Label4=Trigger');
    l.Add('ActionGB=Action');
    l.Add('Label13=Params');
    l.Add('KeyboardRB=KeySeq');
    l.Add('SimConnectRB=Simconnect');
    l.Add('SCTextChB=TextInfo');
    l.Add('SysCommandRB=RunApp');
    l.Add('TabSheet4=About');
    l.Add('Label7=WhatIsIt');
    l.Add('Label8=Author');
    l.Add('Label9=Web');
    l.Add('Label12=Version');
    l.Add('Label14=Donate');
    l.Add('Label15=Language');
    l.Add('TabSheet5=Help');
    l.Add('SplashPanel=PressKey');
    l.Add('NewMacroAction=New');
    l.Add('DeleteMacroAction=Delete');
    l.Add('FileExit=Exit');
    l.Add('StartEvent=SimConnectButton');
    l.Add('MinimizeToTrayCB=MinimizeToTray');
    l.Add('StartMinimizedCB=StartMinimized');
    l.Add('TabSheet3=Script');
    l.Add('Label16=ScriptLanguage');
    l.Add('LoadRoutinesButton=LoadRoutines');
    l.Add('ScriptTestGroupBox=ScriptTest');
    l.Add('RoutinesGroupBox=ScriptRoutines');
    l.Add('ScriptTestButton=DoTest');
    l.Add('TestMacroScriptB=TestMacroScript');
    l.Add('ScriptedActionTS=Scripted');
    l.Add('PredefinedActionTS=Predefined');
    l.Add('CheckMacroScriptB=CheckMacroScript');
    l.Add('TSMouseCoordinates=MouseCoordinates');
    l.Add('TSWindowTitles=WindowTitles');
    l.Add('TSGameAxis=GameAxis');
    l.Add('TSErrorLof=ErrorLog');
    l.Add('SettingsTabSheet=Settings');
    l.Add('GeneralGroupBox=General');
    l.Add('ScriptToolsTabSheet=ScriptTools');
    l.Add('Label21=ScriptTimeout');
    l.Add('AllowScriptGUICheckBox=AllowScriptGUI');
    l.Add('CompileAllAction=CompileAll');
    l.Add('GroupBox5=ScriptSettings');
    l.Add('Label5=ProcBegin');
    l.Add('Label22=ProcEnd');
    l.Add('SendToBufferRB=SendToBuffer');
    l.Add('Label23=BufferResetTimeout');
    l.Add('ShowBufferChkB=ShowBufferContent');
    l.Add('FindAction=Find');
    l.Add('XPLCommandRB=XPLCommand');
    l.Add('MapToggleAction=MapToggle');
//    l.Add('=');
    Lcl.Localize(self, l);
    // mouse dialog
    l.Clear;
    l.Add('Label1=Mouse');
    l.Add('GroupBox1=MouseEvent');
    l.Add('MouseForm=MouseTrigger');
    l.Add('Label2=Wheel');
    l.Add('Label3=LeftButton');
    l.Add('Label4=MiddleButton');
    l.Add('Label5=RightButton');
    l.Add('RBWheelUp=TriggerWheelUp');
    l.Add('RBWheelDown=TriggerWheelDown');
    l.Add('RBLeftDown=Down');
    l.Add('RBLeftUp=Up');
    l.Add('RBMiddleDown=Down');
    l.Add('RBMiddleUp=Up');
    l.Add('RBRightDown=Down');
    l.Add('RBRightUp=Up');
    l.Add('CancelBtn=Cancel');
    Lcl.Localize(MouseDialog, l);
//    l.Add('=');
    // game dialog
    l.Clear;
    l.Add('GameForm=ButtonTriger');
    l.Add('Label1=GameDevice');
    l.Add('GroupBox1=DeviceButtons');
    Lcl.Localize(GameDialog, l);
    // Map form
    l.Clear;
    l.Add('LockedCB=Locked');
    l.Add('AlwaysOnTopCB=AlwaysOnTop');
    l.Add('MapForm=MapForm');
    l.Add('ShowLayersCB=ShowLayers');
    Lcl.Localize(Glb.MapForm, l);


    // strings
    Lcl.LoadStrings(txt);
    GameDialog.TXTdown := txt.Values['Down'];
    GameDialog.TXTup := txt.Values['Up'];
    // refresh device list
    ListView1.Column[0].Caption := txt.Values['NameCol'];
    ListView1.Column[1].Caption := txt.Values['TypeCol'];
    ListView1.Column[2].Caption := txt.Values['MacrosCountCol'];
    ListView1.Column[3].Caption := txt.Values['SysCol'];
    for I := 0 to ListView1.Items.Count - 1 do
    begin
      if (TObject(ListView1.Items[I].Data) is THIDKeyboard) and not (
      (TObject(ListView1.Items[I].Data) is THIDMouse) or (TObject(ListView1.Items[I].Data) is THIDJoystick)
      ) then
        ListView1.Items[I].SubItems[0] := txt.Values['KeyboardDT']
      else if (TObject(ListView1.Items[I].Data) is THIDMouse) then
        ListView1.Items[I].SubItems[0] := txt.Values['MouseDT']
      else if (TObject(ListView1.Items[I].Data) is THIDJoystick) then
        ListView1.Items[I].SubItems[0] := txt.Values['GameDT']
    end;
    // refresh status bar
    if Glb.FSX.SCAvailable then
    begin
      if Glb.FSX.SCConnected then
        StatusBar.Panels[0].Text := txt.Values['SCConnected']
      else
        StatusBar.Panels[0].Text := txt.Values['SCNotConnected'];
      StatusBar.Panels[2].Text := txt.Values['SCLoaded'];
    end
    else
    begin
      StatusBar.Panels[2].Text := txt.Values['SCNotFound'];
      StatusBar.Panels[0].Text := txt.Values['SCNA'];
    end;
    RefreshCompileIndicators;
    //if (FHookSet) then
    //  StatusBar.Panels[1].Text := txt.Values['KeybHookOK']
    //else
    //  StatusBar.Panels[1].Text := txt.Values['KeybHookERR'];
  finally
    l.Free;
  end;
end;

procedure THIDMacrosForm.MidiEvent(pDevice: TMIDIDevice; pEvent: TMyMidiEvent);
var
  lTestStr: String;
  lDirection: TRawInputEventDirection;
  I: Integer;
  lMsgCode: Cardinal;
  mr: TKeyboardMacro;
begin
  try
    //lTestStr := IntToStr(pEvent.MidiMessage) + ':' + IntToStr(pEvent.Data1)
    //    + ':' + IntToStr(pEvent.Data2);
    TestKeyboardName.Text := pDevice.Name;
    lTestStr := IntToStr(pEvent.Data1);
    if pEvent.Data2 = 0 then
    begin
       lTestStr := lTestStr + ' ' + txt.Values['Up'];
       lDirection := edUp;
    end
    else
    begin
      lTestStr := lTestStr + ' ' + txt.Values['Down'];
       lDirection := edDown;
    end;
    TestScanCode.Text := lTestStr;
    if Scanning <> saDisabled then
    begin
      ScannedEvent.Direction := lDirection;
      ScannedEvent.MessageId := WM_MIDI_KEY;
      ScannedEvent.DeviceHandle := THANDLE(pDevice);
      SplashPanel.Visible := False;
      if Scanning = saDefine then
        lMsgCode := WM_SCANNED
      else if Scanning = saFind then
        lMsgCode := WM_FINDMACRO;
      Scanning := saDisabled;
      // PostMessage to not delay DLL message processing (app is waiting for an answer)
      PostMessage(Handle, lMsgCode, pEvent.Data1, 0)
    end else
      // check if there's macro to fire
      for I := 0 to MacrosLB.Items.Count - 1 do
      begin
        mr := TKeyboardMacro(MacrosLB.Items.Objects[I]);
        if  (mr.Keyboard <> nil) and
            (mr.Keyboard.SystemID = pDevice.SystemID) and
            (mr.ButtonIndex = pEvent.Data1) and
            (((mr.Direction = mdUp) and (lDirection = edUp)) or ((mr.Direction = mdDown) and (lDirection = edDown))) then
        begin
          mr.Fire;
          DebugLog('Fired macro ' + mr.Name);
          break;
        end;
      end;
  finally
    pEvent.Free;
  end;
end;

procedure THIDMacrosForm.MinimizeToTrayCBClick(Sender: TObject);
begin
  MinimizeToTray := MinimizeToTrayCB.Checked;
  ManageTray;
end;

procedure THIDMacrosForm.MoveMacrosActionExecute(Sender: TObject);
var
  lMMF: TMacrosMoveForm;
  lRes: Integer;
begin
  lMMF := TMacrosMoveForm.Create(self);
  try
    lMMF.SourceDev := THIDKeyboard(ListView1.Selected.Data);
    lRes := lMMF.ShowModal;
  finally
    lMMF.Free;
  end;
  if lRes = mrOk then
    RecalcLinkedMacros;
end;

function THIDMacrosForm.CheckFire(e: PRawInputEvent;
  m: TKeyboardMacro): Boolean;
var
  I: Integer;
begin
  Result := False;
  //if (m.Keyboard <> nil) then
  //  DebugLog('Checking macro with device ' + IntToStr(m.Keyboard.Handle) + ' code ' + IntToStr(m.KeyCode) +
  //  ' event dev ' + IntToStr(e^.DeviceHandle));

  if (m.Keyboard <> nil) and (m.Keyboard.Handle = e^.DeviceHandle) then
  begin
    if e^.EventType = etKeyboard then
    begin
      if (m.KeyCode = e^.Code) and (
          ((m.Direction = mdDown) and (e^.Direction = edDown)) or
          ((m.Direction = mdUp) and (e^.Direction = edUp)))
      then
      begin
        Result := True;
      end;
    end;
    if e^.EventType = etMouse then
    begin
      case e^.MessageId of
        WM_LBUTTONDOWN : Result := (m.MouseEvent = KbdMacro.Left) and (m.Direction = mdDown);
        WM_LBUTTONUP   : Result := (m.MouseEvent = KbdMacro.Left) and (m.Direction = mdUp);
        WM_MBUTTONDOWN : Result := (m.MouseEvent = KbdMacro.Middle) and (m.Direction = mdDown);
        WM_MBUTTONUP   : Result := (m.MouseEvent = KbdMacro.Middle) and (m.Direction = mdUp);
        WM_RBUTTONDOWN : Result := (m.MouseEvent = KbdMacro.Right) and (m.Direction = mdDown);
        WM_RBUTTONUP   : Result := (m.MouseEvent = KbdMacro.Right) and (m.Direction = mdUp);
        WM_MOUSEWHEEL  : Result := (m.MouseEvent = KbdMacro.Wheel) and (
          ((m.Direction = mdDown) and (e^.Direction = edDown)) or
          ((m.Direction = mdUp) and (e^.Direction = edUp)));
      end;
    end;
  end;
end;

procedure THIDMacrosForm.CheckMacroScriptBClick(Sender: TObject);
var am: TKeyboardMacro;
begin
  try
    Glb.ScriptEngine.Compiling := True;
    if (MacrosLB.ItemIndex >= 0) then
    begin
      am := TKeyboardMacro(MacrosLB.Items.Objects[MacrosLB.ItemIndex]);
      if not CompileMacro(am) then
      begin
        // place cursor to error
        MacroScriptRichEdit.SetFocus;
        MacroScriptRichEdit.SelStart := MacroScriptRichEdit.Perform(
            EM_LINEINDEX, Glb.ScriptEngine.TestingSC.Error.Line-2, 0) + Glb.ScriptEngine.TestingSC.Error.Column;
      end;
      RefreshCompileMacroIndicators(am);
    end;
  finally
    Glb.ScriptEngine.Compiling := False;
  end;
end;

procedure THIDMacrosForm.CommandEditChange(Sender: TObject);
var am: TKeyboardMacro;
begin
  if (MacrosLB.ItemIndex >= 0) then
  begin
    am := TKeyboardMacro(MacrosLB.Items.Objects[MacrosLB.ItemIndex]);
    am.SysCommand := CommandEdit.Text;
  end;
end;

function THIDMacrosForm.CompileAll(CompileLog: TStrings): Boolean;
var
  I: Integer;
  mr: TKeyboardMacro;
  MacroScript: String;
  MacroProcName: String;
begin
  Result := True;
  Glb.ScriptEngine.Compiling := True;
  try
    Glb.ScriptEngine.Language := ScriptLanguageCB.Text;
    Glb.ScriptEngine.ProcBegin := ProcBeginEdit.Text;
    Glb.ScriptEngine.ProcEnd := ProcEndEdit.Text;
    Glb.ScriptEngine.ResetExecutionSC;
    if RoutinesEdit.Text > '' then
    begin
      Glb.ScriptEngine.RoutinesSource := RoutinesEdit.Text;
      CompileLog.Add(txt.Values['CompilingRoutines']);
      try
        Glb.ScriptEngine.ExecutionSC.AddCode(RoutinesEdit.Text);
        Glb.ScriptEngine.RoutinesCompiled := True;
      except
        On E: Exception do
        begin
          CompileLog.Add(Format(txt.Values['CompileErrorS'], [E.Message]));
          Glb.ScriptEngine.RoutinesCompiled := False;
          Result := False;
        end;
      end;
      if not Glb.ScriptEngine.RoutinesCompiled then
      begin
        // Macros will be compiled without routines
        Glb.ScriptEngine.Language := ScriptLanguageCB.Text;
        Glb.ScriptEngine.ProcBegin := ProcBeginEdit.Text;
        Glb.ScriptEngine.ProcEnd := ProcEndEdit.Text;
        Glb.ScriptEngine.ResetExecutionSC;
      end;
    end
    else
    begin
      Glb.ScriptEngine.RoutinesSource := '';
      Glb.ScriptEngine.RoutinesCompiled := True;
    end;

    AllMacrosCompiled := True;
    for I := 0 to MacrosLB.Count - 1 do
    begin
      mr := TKeyboardMacro(MacrosLB.Items.Objects[I]);
      if (mr.Action = maScript) then
      begin
        CompileLog.Add(Format(txt.Values['CompilingMacroS'], [mr.Name]));
        try
          Glb.ScriptEngine.Language := ScriptLanguageCB.Text;
          Glb.ScriptEngine.ProcBegin := ProcBeginEdit.Text;
          Glb.ScriptEngine.ProcEnd := ProcEndEdit.Text;
          Glb.ScriptEngine.ResetTestingSC;
          if Glb.ScriptEngine.RoutinesCompiled then
            Glb.ScriptEngine.TestingSC.AddCode(RoutinesEdit.Text);
          MacroProcName := 'Macro'+ IntToStr(mr.Id);
          MacroScript := Format(Glb.ScriptEngine.ProcBegin + #13#10 + '%s' + #13#10 + Glb.ScriptEngine.ProcEnd,
           [MacroProcName, mr.ScriptSource]);
          Glb.ScriptEngine.TestingSC.AddCode(MacroScript);
          // now there was no exception so it's safe to add macro code
          // to execution SC
          Glb.ScriptEngine.ExecutionSC.AddCode(MacroScript);
          mr.ScriptCompiled := True;
        except
          On E: Exception do
          begin
            mr.ScriptCompiled := False;
            AllMacrosCompiled := False;
            CompileLog.Add(Format(txt.Values['CompileErrorS'], [E.Message]));
            Result := False;
          end;
        end;
      end;
    end;
  finally
    Glb.ScriptEngine.Compiling := False;
  end;
end;

procedure THIDMacrosForm.CompileAllActionExecute(Sender: TObject);
var
  CompileLog: TStrings;
  I: Integer;
  mr: TKeyboardMacro;
  lErrorMessage: String;
begin
  CompileLog := TStringList.Create;
  try
    CompileAllAction.Caption := txt.Values['Compiling'];
    CompileAllAction.Enabled := False;
    if not CompileAll(CompileLog) then
    begin
      lErrorMessage := txt.Values['CompilationErrors'];
      lErrorMessage := lErrorMessage + #13 + txt.Values['CompilationLog'] + #13;
      lErrorMessage := lErrorMessage + CompileLog.Text;
     ShowMessage(lErrorMessage);
    end;
  finally
    CompileAllAction.Caption := txt.Values['CompileAll'];
    CompileAllAction.Enabled := True;
    CompileLog.Free;
  end;
  RefreshCompileIndicators;
end;

function THIDMacrosForm.CompileMacro(am: TKeyboardMacro): Boolean;
var
  Bounds : array [0..0] of TSafeArrayBound;
  Params : PSafeArray;
  tmpCode: String;
  tmpProcName: string;
  CompileLog: TStrings;
begin
  Result := True;
  tmpProcName := 'Macro' + IntToStr(am.Id);
  tmpCode := Format(Glb.ScriptEngine.ProcBegin + #13#10 + '%s' + #13#10 + Glb.ScriptEngine.ProcEnd,
     [tmpProcName, MacroScriptRichEdit.Text]);
  Glb.ScriptEngine.Language := ScriptLanguageCB.Text;
  Glb.ScriptEngine.ProcBegin := ProcBeginEdit.Text;
  Glb.ScriptEngine.ProcEnd := ProcEndEdit.Text;
  Glb.ScriptEngine.ResetTestingSC;
  if (not Glb.ScriptEngine.RoutinesCompiled) then
  begin
    if MessageDlg(txt.Values['CompileNoRoutines'], mtWarning, [mbYes, mbCancel], 0) <> mrYes then
      exit;
  end else
    Glb.ScriptEngine.TestingSC.AddCode(Glb.ScriptEngine.RoutinesSource);
  try
    Glb.ScriptEngine.TestingSC.AddCode(tmpCode);
    // no exception here, set compiled flag
    CompileLog := TStringList.Create;
    try
      CompileAll(CompileLog);
    finally
      CompileLog.Free;
    end;
    am.ScriptCompiled := True;
  except
    On E: Exception do
    begin
     ShowMessage(Format(txt.Values['CompileErrorS'], [E.Message]));
     Result := False;
     //ShowMessage(Format('Line/col %d:%d', [ScriptEngine.TestingSC.Error.Line, ScriptEngine.TestingSC.Error.Column]));
    end;
  end;
end;

procedure THIDMacrosForm.DebugLog(Value: String);
begin
  Glb.DebugLog(Value, 'GEN');
end;

procedure THIDMacrosForm.DeleteMacroActionExecute(Sender: TObject);
var am: TKeyboardMacro;
begin
  if (MacrosLB.ItemIndex >= 0) then
  begin
    am := TKeyboardMacro(MacrosLB.Items.Objects[MacrosLB.ItemIndex]);
    Glb.MacrosList.Delete(am);
    am.Free;
    MacrosLB.DeleteSelected;
    RefreshEditArea;
  end;
end;

procedure THIDMacrosForm.DisplayDevices;
var
  newLI : TListItem;
  lDevice: THIDKeyboard;
  I : Integer;
begin
  ListView1.Clear;
  // check if the name already exist
  for I := 0 to Glb.HIDControl.Count - 1 do
  begin
    newLI := ListView1.Items.Add;
    lDevice := Glb.HIDControl.Items[I];
    newLI.Caption := lDevice.Name;
    if (lDevice is THIDJoystick) then
      newLi.SubItems.Add(txt.Values['GameDT'])
    else if (lDevice is THIDMouse) then
      newLi.SubItems.Add(txt.Values['MouseDT'])
    else if (lDevice is TMIDIDevice) then
      newLi.SubItems.Add(txt.Values['MidiDT'])
    else
      newLi.SubItems.Add(txt.Values['KeyboardDT']);
    //newLi.SubItems.Add(newLI.Caption + ' - ' + lDevice.SystemID);
    //newLi.SubItems.Add(IntToStr(lDevice.LinkedMacrosCount));
    newLi.SubItems.Add('?');
    newLi.SubItems.Add(lDevice.SystemID);
    newLI.Data := lDevice;
  end;
  RecalcLinkedMacros;
end;

procedure THIDMacrosForm.FileOpenButtonClick(Sender: TObject);
begin
  //OpenDialog1.FileName := CommandEdit.Text;
  if OpenDialog1.Execute then
  begin
    CommandEdit.Text := OpenDialog1.FileName;
    if Length(Trim(CommandEdit.Text)) > 0 then
    begin
      if (Pos(' ', Trim(CommandEdit.Text)) > 0) and (Pos('"', Trim(CommandEdit.Text)) <> 1) then
        CommandEdit.Text := '"' + CommandEdit.Text + '"';
    end;
  end;
end;

procedure THIDMacrosForm.FillGameGrid;
var I, IR, IC: Integer;
  lDev: THIDJoystick;
begin
  IC := 1;
  for I := 0 to ListView1.Items.Count - 1 do
  begin
    if (TObject(ListView1.Items[I].Data) is THIDJoystick) then
    begin
      lDev := THIDJoystick(ListView1.Items[I].Data);
      if (lDev <> nil) then
        for IR := 1 to GameAxisStringGrid.RowCount - 1 do
          GameAxisStringGrid.Cells[IC, IR] := IntToStr(
              lDev.GetAxis(GameAxisStringGrid.Cells[0, IR]));
      Inc(IC);
    end;
  end;
end;

procedure THIDMacrosForm.FindActionExecute(Sender: TObject);
begin
  Scanning := saFind;
  // show splash
  SplashPanel.Visible := True;
end;

{  GameAxisStringGrid.RowCount := 27;
  I := 1;
  //GameAxisStringGrid.Cells[0, 1] := '';
  GameAxisStringGrid.Cells[0, I] := 'X'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'Y'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'Z'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'Rx'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'Ry'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'Rz'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'Slider'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'POV'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'VX'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'VY'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'VZ'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'VRx'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'VRy'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'VRz'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'VSlider'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'AX'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'AY'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'AZ'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'ARx'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'ARy'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'ARz'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'ASlider'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'FRx'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'FRy'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'FRz'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'FSlider'; Inc(I);
  }


procedure THIDMacrosForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  Quit := True;
  CanClose := True;
  if (FHookSet) then
    FHookSet := FreeHook;
  Glb.AppConfig.SaveToXml;
end;

procedure THIDMacrosForm.FormCreate(Sender: TObject);
var
  lDebugStr: String;
begin
  VersionLabel.Caption := '?';
  InitMMF;
  fBufferNotification := True;
  AllMacrosCompiled := False;
  Txt := TStringList.Create;
  Glb := THDMGlobals.Create(self);
  Glb.MainForm := self;
  Glb.OnUserLog := ScriptLogHandler;
  InitLanguages;
  IgnoreInputMessage := False;
  CfgFileName := 'hidmacros.ini';
  eb := TList.Create;
  Lcl := TLocalizer.Create;
  ignoreEvents := TList.Create;
  MouseMovementMessageCounter := 0;
  Glb.HIDControl.Init;
  // Detec HID devices
  Glb.HIDControl.DetectDevices;
  DebugLog('XML file name: ' + Glb.AppConfig.XMLFileName);
  if FileExists(Glb.AppConfig.XmlFileName) then
    StatusBar.Panels[3].Text := Glb.AppConfig.XmlFileName
  else
    if FileExists(CfgFileName) then
      StatusBar.Panels[3].Text := CfgFileName
    else
      StatusBar.Panels[3].Text := Glb.AppConfig.XmlFileName; // new will be created
  FHookSet := false;
  LastDevId := 0;
  Scanning := saDisabled;
  Quit := False;
  GameDialog := TGameForm.Create(self);
  MouseDialog := TMouseForm.Create(self);
  Glb.MapForm := TMapForm.Create(self);
  Glb.MapForm.OnToggle := MapFormToggle;
  // Init sim connect
  Glb.FSX.Init;
  // Fill SC events combo
  SimconnectCB.Items.Add('');
  Glb.FSX.OnDisconnect := SCDisconnect;
  SimconnectCB.Items := Glb.FSX.EventCaptions;
  Glb.Xpl.AddCommands(XPLCommandEdit.Items);
  Glb.Buffer.OnValueChanged := BufferChanged;
  // RAW input - register for WM_INPUT event
  RegisterEvent;
  // modify splash
  RecalculateSplashScreen;
  // load help
  if (FileExists('Help.rtf')) then
    RichEdit1.Lines.LoadFromFile('Help.rtf')
  else
    RichEdit1.Lines.Add('File Help.rtf not found.');
  // Load settings
  if FileExists(ExtractFilePath(Application.ExeName)+'\'+Glb.AppConfig.XmlFileName) then
    Glb.AppConfig.LoadFromXml
  else
    LoadFromIni;
  Glb.HIDControl.AssignDefaultNames;
  LinkMacros2DevicesByNames;
  DisplayDevices;
  Glb.ScriptEngine.Language := ScriptLanguageCB.Text;
  Glb.ScriptEngine.ProcBegin := ProcBeginEdit.Text;
  Glb.ScriptEngine.ProcEnd := ProcEndEdit.Text;
  ScriptLanguageCBChange(nil);
  // Keyboard hook
  HookKeyboard;
  BuildIgnoreList;
  RefreshEditArea;
  if Glb.FSX.SCAvailable then
  begin
    StatusBar.Panels[0].Text := txt.Values['SCNotConnected'];
    StatusBar.Panels[2].Text := txt.Values['SCLoaded'];
    // try autoconnect
    StartEventExecute(nil);
  end
  else
  begin
    StatusBar.Panels[2].Text := txt.Values['SCNotFound'];
    StatusBar.Panels[0].Text := txt.Values['SCNA'];
    StartEvent.Enabled := False;
  end;
  CompileAllActionExecute(self);
  if Glb.HIDControl.GameAvailable then
  begin
    InitGameGrid;
    Timer1.Enabled := True;
  end;
  //DebugLog('My handle: '+IntToHex(Handle, 8));
  //DebugLog('Seq handle: '+IntToHex(SequenceEdit.Handle, 8));
  //DebugLog('Seq handle parent: '+IntToHex(GetAncestor(SequenceEdit.Handle, GA_ROOT), 8));
end;

procedure THIDMacrosForm.FormDestroy(Sender: TObject);
var I:Integer;
begin
// BIG TO BE REFACTORED!!!
//Devices should be freed in THIDControl.Destroy, not here
  for I := 0 to ListView1.Items.Count - 1 do
    THIDKeyboard(ListView1.Items[I].Data).Free;
  for I := 0 to MacrosLB.Count - 1 do
    MacrosLB.Items.Objects[I].Free;
  if Glb.DebugGroups <> '' then
  begin
    if eb.Count > 0 then
      LogEb;
  end;
  for I := eb.Count - 1 downto 0 do
  begin
    Dispose(eb.Items[I]);
    eb.Delete(I);
  end;
  eb.Free;
  FreeMMF;
  GameDialog.Free;
  MouseDialog.Free;
  Glb.MapForm.Free;
  Lcl.Free;
  for I := 0 to ignoreEvents.Count - 1 do
    TKeyboardMacro(ignoreEvents[I]).Free;
  ignoreEvents.Free;
  Txt.Free;
  Glb.Free;
end;

procedure THIDMacrosForm.GameButtonEvent(pDevice: THIDJoystick;
  pButton: Integer; pDirection: TRawInputEventDirection);
var
  I: Integer;
  mr: TKeyboardMacro;
  lMsgCode: Cardinal;
begin
  TestKeyboardName.Text := pDevice.Name;
  if pDirection = edDown then
    TestScanCode.Text := txt.Values['Button'] + ' ' + IntToStr(pButton + 1) + ' ' + txt.Values['Down']
  else
    TestScanCode.Text := txt.Values['Button'] + ' ' + IntToStr(pButton + 1) + ' ' + txt.Values['Up'];
  if Scanning <> saDisabled then
  begin
    ScannedEvent.Direction := pDirection;
    ScannedEvent.MessageId := WM_JOY_BUTTON_DOWN;
    ScannedEvent.DeviceHandle := THANDLE(pDevice);
    SplashPanel.Visible := False;
    if Scanning = saDefine then
      lMsgCode := WM_SCANNED
    else if Scanning = saFind then
      lMsgCode := WM_FINDMACRO;
    Scanning := saDisabled;
    // PostMessage to not delay DLL message processing (app is waiting for an answer)
    PostMessage(Handle, lMsgCode, pButton, 0)
  end else
    // check if there's macro to fire
    for I := 0 to MacrosLB.Items.Count - 1 do
    begin
      mr := TKeyboardMacro(MacrosLB.Items.Objects[I]);
      if  (mr.Keyboard <> nil) and
          (mr.Keyboard.SystemID = pDevice.SystemID) and
          (mr.ButtonIndex = pButton) and
          (((mr.Direction = mdUp) and (pDirection = edUp)) or ((mr.Direction = mdDown) and (pDirection = edDown))) then
      begin
        mr.Fire;
        DebugLog('Fired macro ' + mr.Name);
        break;
      end;
    end;
end;

function THIDMacrosForm.GetRECursorPos(pRichEdit: TRichEdit): String;
var
  Line, Column : integer;
begin
  // gecursor pos
   Line := pRichEdit.Perform(EM_LINEFROMCHAR,pRichEdit.SelStart, 0) ;
   Column := pRichEdit.SelStart - pRichEdit.Perform(EM_LINEINDEX, Line, 0) ;
   Result := Format('%d:%d', [Line+1, Column+1]);
end;

function THIDMacrosForm.GetMessageId(Message: Word): String;
begin
  case Message of
    WM_KEYDOWN:         Result := 'WM_KEYDOWN';
    WM_SYSKEYDOWN:      Result := 'WM_SYSKEYDOWN';
    WM_KEYUP:           Result := 'WM_KEYUP';
    WM_SYSKEYUP:        Result := 'WM_SYSKEYUP';
    WM_LBUTTONDOWN:     Result := 'WM_LBUTTONDOWN';
    WM_MBUTTONDOWN:     Result := 'WM_MBUTTONDOWN';
    WM_RBUTTONDOWN:     Result := 'WM_RBUTTONDOWN';
    WM_LBUTTONUP:       Result := 'WM_LBUTTONUP';
    WM_MBUTTONUP:       Result := 'WM_MBUTTONUP';
    WM_RBUTTONUP:       Result := 'WM_RBUTTONUP';
    WM_MOUSEWHEEL:      Result := 'WM_MOUSEWHEEL';
    else Result := IntToHex(Message, 4);
  end;
end;

procedure THIDMacrosForm.HookKeyboard;
begin
  if (not FHookSet) then
  begin
    FHookSet := LongBool(SetHook);
    if (FHookSet) then
    begin
      DebugLog('Hook SET');
      //StatusBar.Panels[1].Text := txt.Values['KeybHookOK'];
    end
    else
    begin
      DebugLog('Hook NOT SET');
      //StatusBar.Panels[1].Text := txt.Values['KeybHookERR'];
    end;
  end;
end;

function THIDMacrosForm.IncEbIndex(Value: Word; Limit: Word): Word;
begin
  Result := Value + 1;
  if (Value > InputEventBufferSize) then
    Result := 0;
  if Result = Limit then
  begin
    // something is wrong, buffer is full
    DebugLog('ERROR: Something is wrong, buffer full. Trying to increase '+
        IntToStr(Value)+', but got to limit.');
  end;
end;

procedure THIDMacrosForm.InitGameGrid;
var I, DevNo: Integer;
    DevCount: Integer;
begin
  DevCount := 0;
  for I := 0 to ListView1.Items.Count - 1 do
    if (TObject(ListView1.Items[I].Data) is THIDJoystick) then
      Inc(DevCount);

  GameAxisStringGrid.ColCount := DevCount+1;
  DevNo := 0;
  for I := 0 to ListView1.Items.Count - 1 do
  begin
    if (TObject(ListView1.Items[I].Data) is THIDJoystick) then
    begin
      GameAxisStringGrid.Cells[DevNo+1, 0] := THIDJoystick(ListView1.Items[I].Data).Name;
      Inc(DevNo);
    end;
  end;

  GameAxisStringGrid.RowCount := 34;
  I := 1;
  //GameAxisStringGrid.Cells[0, 1] := '';
  GameAxisStringGrid.Cells[0, I] := 'X'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'Y'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'Z'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'Rx'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'Ry'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'Rz'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'Slider1'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'Slider2'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'POV1'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'POV2'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'POV3'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'POV4'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'VX'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'VY'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'VZ'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'VRx'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'VRy'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'VRz'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'VSlider1'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'VSlider2'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'AX'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'AY'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'AZ'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'ARx'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'ARy'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'ARz'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'ASlider1'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'ASlider2'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'FRx'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'FRy'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'FRz'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'FSlider1'; Inc(I);
  GameAxisStringGrid.Cells[0, I] := 'FSlider2'; Inc(I);
end;

procedure THIDMacrosForm.InitLanguages;
var
  BaseDir, JustName: String;
  TmpResult: Integer;
  SearchRec: TSearchRec;
begin
  // search for lng files in lang subdirectory
  BaseDir := ExtractFilePath(Application.ExeName)+'\lang';
  if DirectoryExists(BaseDir) then
  begin
    LangComboBox.Items.Clear;
    TmpResult := FindFirst(BaseDir+'\*.lng', 0, SearchRec);
    while TmpResult = 0 do
    begin
      JustName := ExtractFileName(SearchRec.Name);
      LangComboBox.Items.Add(Copy(JustName, 1, Length(JustName) - 4));
      TmpResult := FindNext(SearchRec);
    end;
    FindClose(SearchRec);
  end;
  txt.Clear;
  txt.Add('NameCol=Name');
  txt.Add('TypeCol=Type');
  txt.Add('SysCol=System ID');
  txt.Add('MacrosCountCol=Macros #');
  txt.Add('KeyboardDT=Keyboard');
  txt.Add('MouseDT=Mouse');
  txt.Add('GameDT=Game');
  txt.Add('MidiDT=Midi');
  txt.Add('SCNotConnected=Not Connected');
  txt.Add('SCLoaded=SimConnect.dll Loaded');
  txt.Add('SCNotFound=SimConnect.dll NOT FOUND');
  txt.Add('SCNA=SimConnect not available');
  txt.Add('Button=Button');
  txt.Add('Down=down');
  txt.Add('Up=up');
  txt.Add('KeybHookOK=Keyb hook OK');
  txt.Add('KeybHookERR=Keyb hook ERR');
  txt.Add('SCConnected=Connected');
  txt.Add('Left=Left');
  txt.Add('Middle=Middle');
  txt.Add('Right=Right');
  txt.Add('WheelDown=Wheel down');
  txt.Add('WheelUp=Wheel up');
  txt.Add('AlreadyUsedS=This key is already used for macro %s.');
  txt.Add('AUIgnoreS=Press Ignore to use this key anyway and unlink macro %s.');
  txt.Add('AURetry=Press Retry to scan different key.');
  txt.Add('AUAbort=Press Abort to cancel scanning.');
  txt.Add('Warning=Warning');
  txt.Add('Ignore=Ignore');
  txt.Add('Abort=Abort');
  txt.Add('Retry=Retry');
  txt.Add('LeftButton=Left button');
  txt.Add('MiddleButton=Middle button');
  txt.Add('RightButton=Right button');
  txt.Add('FSXQuit=FSX quit');
  txt.Add('LoadErrorS=Error during configuration load (%s).');
  txt.Add('CompileNoRoutines=Routines not compiled, compile macro without routines?');
  txt.Add('CompileErrorS=Error during compilation: %s.');
  txt.Add('CompilingRoutines=Compiling routines...');
  txt.Add('CompilingMacroS=Compiling macro %s...');
  txt.Add('SomeMacrosNotCompiledWithRoutines=Some macros can not be compiled with these routines.');
  txt.Add('CompilationLog=Compilation log:');
  txt.Add('Compiled=Compiled');
  txt.Add('NotCompiled=Not compiled');
  txt.Add('CodeErrorS=Error during execution: %s.');
  txt.Add('TestCodeErrorRoutinesSS=Error during execution: %s. Routines compiled with error: %s.');
  txt.Add('AllCompiled=Scripts compiled');
  txt.Add('AllNotCompiled=Not compiled');
  txt.Add('CompilationErrors=There were errors during compilation.');
  txt.Add('Compiling=Compiling');
  txt.Add('CompileAll=Compile all');
  //txt.Add('');
end;

{DelphiX - celkem naprd
var
  DXInput: TDXInput;
begin
  DXInput := TDXInput.Create(self);
  try
    DXInput.
  finally
    DXInput.Free;
  end;
end;}


procedure THIDMacrosForm.KeyboardNameChanged(kbd: THIDKeyboard);
var I: Integer;
  km : TKeyboardMacro;
begin
  // remap events linked to exisitng object
  if kbd <> nil then
  begin
    for I := 0 to MacrosLB.Count - 1 do
    begin
      km := TKeyboardMacro(MacrosLB.Items.Objects[I]);
      if (km.Keyboard = kbd) then
        km.KbdName := kbd.Name;
      // map macros without linked Keyboard
      if (km.Keyboard = nil) and (UpperCase(km.KbdName) = UpperCase(kbd.Name)) then
      begin
        km.Keyboard := kbd;
        km.KbdName := kbd.Name; // just to alligne upper chars
      end;
    end;
    RefreshEditArea;
  end;
end;

procedure THIDMacrosForm.KeyboardRBClick(Sender: TObject);
var am: TKeyboardMacro;
begin
  if (MacrosLB.ItemIndex >= 0) then
  begin
    am := TKeyboardMacro(MacrosLB.Items.Objects[MacrosLB.ItemIndex]);
    if KeyboardRB.Checked then
      am.Action := KeyboardSequence;
    if SimConnectRB.Checked then
      am.Action := SimConnectEvent;
    if SysCommandRB.Checked then
      am.Action := maSysCommand;
    if SendToBufferRB.Checked then
      am.Action := maSendToBuffer;
    if XPLCommandRB.Checked then
      am.Action := maXPLCommand;
  end;
end;

procedure THIDMacrosForm.Label11Click(Sender: TObject);
begin
 ShellExecute(Application.Handle,
            PChar('open'),
            PChar(Label11.Caption),
            PChar(0),
            nil,
            SW_NORMAL);
end;

procedure THIDMacrosForm.LangComboBoxChange(Sender: TObject);
begin
  Language := LangComboBox.Text;
  ChangeLanguage(Language);
end;

procedure THIDMacrosForm.LinkMacros2DevicesByNames;
var
  I: Integer;
  mr: TKeyboardMacro;
  lDev: THIDKeyboard;
begin
  for I := 0 to MacrosLB.Items.Count - 1 do
  begin
    mr := TKeyboardMacro(MacrosLB.Items.Objects[I]);
    lDev := Glb.HIDControl.GetDevice(mr.KbdName);
    mr.Keyboard := lDev;
  end;
end;

procedure THIDMacrosForm.ListView1AdvancedCustomDrawItem(
  Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  Stage: TCustomDrawStage; var DefaultDraw: Boolean);
var lItem: THIDKeyboard;
begin
  lItem := THIDKeyboard(Item.Data);
  if (lItem <> nil) and (not lItem.IsAlive) then
  begin
    Sender.Canvas.Font.Color := clInactiveCaption;
    Sender.Canvas.Font.Style := Sender.Canvas.Font.Style + [fsItalic];
  end;
end;

procedure THIDMacrosForm.ListView1Click(Sender: TObject);
begin
  RefreshDeviceActions;
end;

procedure THIDMacrosForm.ListView1Edited(Sender: TObject; Item: TListItem;
  var S: string);
var
  I : Integer;
begin
  if (Item <> nil) then
  begin
    // search if caption is already used
    for I := 0 to ListView1.Items.Count - 1 do
      if (UpperCase(ListView1.Items[I].Caption) = UpperCase(S)) then
      begin
        //S := '';
        S := Item.Caption; // old value
        break;
      end;
    if (S <> Item.Caption) then
    begin
      THIDKeyboard(Item.Data).Name := S;
      KeyboardNameChanged(THIDKeyboard(Item.Data));
    end;
  end;
end;

procedure THIDMacrosForm.LoadFromIni;
var
  ini: TiniFile;
  I, J, SysIDIndex, NameIndex:Integer;
  kb : THIDKeyboard;
  mr : TKeyboardMacro;
  fn, tmpSysID, tmpName: String;
begin
  fn := ExtractFilePath(Application.ExeName)+'\'+CfgFileName;
  if (not FileExists(fn)) then
    Exit;
  ini := TIniFile.Create(fn);
  try
    // load language
    Language := ini.ReadString('Settings', 'Language', '');
    I := LangComboBox.Items.IndexOf(Language);
    if I < 0 then
      Language := ''
    else
    begin
      ChangeLanguage(Language);
      LangComboBox.ItemIndex := I;
    end;
    MinimizeToTray := ini.ReadBool('Settings', 'MinimizeToTray', false);
    StartMinimized := ini.ReadBool('Settings', 'StartMinimized', false);
    MinimizeToTrayCB.Checked := MinimizeToTray;
    StartMinimizedCB.Checked := StartMinimized;
    if StartMinimized then
    begin
      if MinimizeToTray then
        Application.ShowMainForm := False
      else
        WindowState := wsMinimized;
    end;
    ManageTray;
    // keyboards were created from system configuration, we'll try to match names l8r
    // load macros
    I := 0;
    while ini.SectionExists('Macro.'+IntToStr(I)) do
    begin
      mr := TKeyboardMacro.Create;
      if (mr.loadFromIni(ini, 'Macro.'+IntToStr(I))) then
      begin
        // add to list
        MacrosLB.Items.AddObject(mr.Name, mr);
      end
      else
        // not valid, free it
        mr.Free;
      Inc(I);
    end;
    // load & match keyboards
    I := 0;
    while ini.SectionExists('Keyboard.'+IntToStr(I)) do
    begin
      SysIDIndex := -1;
      NameIndex := -1;
      tmpSysID := ini.ReadString('Keyboard.'+IntToStr(I), 'SystemID', '');
      tmpName := ini.ReadString('Keyboard.'+IntToStr(I), 'Name', '');
      if (tmpSysID <> '') then
        for J := 0 to ListView1.Items.Count - 1 do
        begin
          kb := THIDKeyboard(ListView1.Items[J].Data);
          if (kb.SystemID = tmpSysID) then
            SysIDIndex := J;
          if (UpperCase(kb.Name) = UpperCase(tmpName)) and (SysIDIndex <> J) then
            NameIndex := J;
        end;
      // rename on ID match and Name doesn't exist
      if (SysIDIndex >= 0) and (NameIndex < 0) then
      begin
        ListView1.Items[SysIDIndex].Caption := tmpName;
        kb := THIDKeyboard(ListView1.Items[SysIDIndex].Data);
        kb.Name := tmpName;
        KeyboardNameChanged(kb);
      end;
      Inc(I);
    end;
    // load & match mouses
    I := 0;
    while ini.SectionExists('Mouse.'+IntToStr(I)) do
    begin
      SysIDIndex := -1;
      NameIndex := -1;
      tmpSysID := ini.ReadString('Mouse.'+IntToStr(I), 'SystemID', '');
      tmpName := ini.ReadString('Mouse.'+IntToStr(I), 'Name', '');
      if (tmpSysID <> '') then
        for J := 0 to ListView1.Items.Count - 1 do
        begin
          kb := THIDKeyboard(ListView1.Items[J].Data);
          if (kb.SystemID = tmpSysID) then
            SysIDIndex := J;
          if (UpperCase(kb.Name) = UpperCase(tmpName)) and (SysIDIndex <> J) then
            NameIndex := J;
        end;
      // rename on ID match and Name doesn't exist
      if (SysIDIndex >= 0) and (NameIndex < 0) then
      begin
        ListView1.Items[SysIDIndex].Caption := tmpName;
        kb := THIDKeyboard(ListView1.Items[SysIDIndex].Data);
        kb.Name := tmpName;
        KeyboardNameChanged(kb);
      end;
      Inc(I);
    end;
    // load & match game devs
    I := 0;
    while ini.SectionExists('Game.'+IntToStr(I)) do
    begin
      SysIDIndex := -1;
      NameIndex := -1;
      tmpSysID := ini.ReadString('Game.'+IntToStr(I), 'SystemID', '');
      tmpName := ini.ReadString('Game.'+IntToStr(I), 'Name', '');
      if (tmpSysID <> '') then
        for J := 0 to ListView1.Items.Count - 1 do
        begin
          kb := THIDKeyboard(ListView1.Items[J].Data);
          if (kb.SystemID = tmpSysID) then
            SysIDIndex := J;
          if (UpperCase(kb.Name) = UpperCase(tmpName)) and (SysIDIndex <> J) then
            NameIndex := J;
        end;
      // rename on ID match and Name doesn't exist
      if (SysIDIndex >= 0) and (NameIndex < 0) then
      begin
        ListView1.Items[SysIDIndex].Caption := tmpName;
        kb := THIDKeyboard(ListView1.Items[SysIDIndex].Data);
        kb.Name := tmpName;
        KeyboardNameChanged(kb);
      end;
      Inc(I);
    end;
  finally
    ini.Free;
 end;
  RefreshEditArea;
end;

procedure THIDMacrosForm.LoadFromXml(RootNode : IXMLNode);
var
  parentNode, aNode: IXMLNode;
  I, J, SysIDIndex, NameIndex:Integer;
  kb : THIDKeyboard;
  mr : TKeyboardMacro;
  tmpSysID, tmpName: String;
  lProcBegin, lProcEnd: String;
begin
  // default values
  Glb.ScriptEngine.AllowGUI := False;
  Glb.ScriptEngine.ScriptTimeout := 10;
  parentNode := RootNode;
  while parentNode <> nil do
  begin
    if UpperCase(parentNode.NodeName) = 'GENERAL' then
    begin
      aNode := parentNode.ChildNodes.First;
      while aNode <> nil do
      begin
        if UpperCase(aNode.NodeName) = 'LANGUAGE' then
        begin
          Language := aNode.Text;
          I := LangComboBox.Items.IndexOf(Language);
          if I < 0 then
            Language := ''
          else
          begin
            ChangeLanguage(Language);
            LangComboBox.ItemIndex := I;
          end;
        end;
        if UpperCase(aNode.NodeName) = 'MINIMIZETOTRAY' then
          MinimizeToTray := aNode.Text = '1';
        if UpperCase(aNode.NodeName) = 'STARTMINIMIZED' then
          StartMinimized := aNode.Text = '1';
        if UpperCase(aNode.NodeName) = 'SCRIPTLANGUAGE' then
        begin
          ScriptLanguageCB.Text := aNode.Text;
        end;
        if UpperCase(aNode.NodeName) = 'PROCBEGIN' then
          lProcBegin := aNode.Text;
        if UpperCase(aNode.NodeName) = 'PROCEND' then
          lProcEnd := aNode.Text;
        if UpperCase(aNode.NodeName) = 'SCRIPTROUTINES' then
          RoutinesEdit.Text := aNode.Text;
        if UpperCase(aNode.NodeName) = 'ALLOWSCRIPTGUI' then
          Glb.ScriptEngine.AllowGUI := aNode.Text = '1';
        if UpperCase(aNode.NodeName) = 'SCRIPTTIMEOUT' then
          Glb.ScriptEngine.ScriptTimeout := StrToIntDef(aNode.Text, 10);
        if UpperCase(aNode.NodeName) = 'BUFFERTIMEOUT' then
          Glb.Buffer.ResetInterval := StrToIntDef(aNode.Text, 2000);
        if UpperCase(aNode.NodeName) = 'BUFFERNOTIFICATION' then
          fBufferNotification := aNode.Text = '1';
        if UpperCase(aNode.NodeName) = 'GAMETICK' then
          Timer1.Interval := StrToIntDef(aNode.Text, 50);
        if UpperCase(aNode.NodeName) = 'XPLDURINGCOMPILATION' then
          Glb.Xpl.ActiveDuringCompilation := aNode.Text = '1';
        aNode := aNode.NextSibling;
      end;
    end;
    if UpperCase(parentNode.NodeName) = 'MACROS' then
    begin
      Glb.DebugLog('Loading macros...', 'XML');
      aNode := parentNode.ChildNodes.First;
      while aNode <> nil do
      begin
        if UpperCase(aNode.NodeName) = 'MACRO' then
        begin
          mr := TKeyboardMacro.Create;
          if (mr.loadFromXml(aNode)) then
          begin
            // add to list
            MacrosLB.Items.AddObject(mr.Name, mr);
            mr.ScriptControl := Glb.ScriptEngine.ExecutionSC;
            Glb.MacrosList.Add(mr);
          end
          else
            // not valid, free it
            mr.Free;
        end;
        aNode := aNode.NextSibling;
      end;
    end;
    parentNode := parentNode.NextSibling;
  end;
  MinimizeToTrayCB.Checked := MinimizeToTray;
  StartMinimizedCB.Checked := StartMinimized;
  ShowBufferChkB.Checked := fBufferNotification;
  BufferResetTOEdit.Text := IntToStr(Glb.Buffer.ResetInterval);
  if StartMinimized then
  begin
    if MinimizeToTray then
      Application.ShowMainForm := False
    else
      WindowState := wsMinimized;
  end;
  ManageTray;
  RefreshEditArea;
  AllowScriptGUICheckBox.Checked := Glb.ScriptEngine.AllowGUI;
  ScriptTimeOutEdit.Text := IntToStr(Glb.ScriptEngine.ScriptTimeout);
  Glb.ScriptEngine.Language := ScriptLanguageCB.Text;
  Glb.ScriptEngine.ProcBegin := lProcBegin;
  Glb.ScriptEngine.ProcEnd := lProcEnd;
  ProcBeginEdit.Text := lProcBegin;
  ProcEndEdit.Text := lProcEnd;
  BackupPathEdit.Text := Glb.AppConfig.BackupPath;
  NoOfBackupsEdit.Text := IntToStr(Glb.AppConfig.BackupFileCount);
  if Glb.MapForm.VisibleByDefault then
    MapToggleAction.Execute;
end;

procedure THIDMacrosForm.LoadRoutinesButtonClick(Sender: TObject);
var
  CompileLog: TStrings;
  I: Integer;
  mr: TKeyboardMacro;
  lErrorMessage: String;
begin
  try
    Glb.ScriptEngine.Compiling := True;
    Glb.ScriptEngine.Language := ScriptLanguageCB.Text;
    Glb.ScriptEngine.ProcBegin := ProcBeginEdit.Text;
    Glb.ScriptEngine.ProcEnd := ProcEndEdit.Text;
    Glb.ScriptEngine.ResetTestingSC;
    try
      Glb.ScriptEngine.RoutinesSource := RoutinesEdit.Text;
      Glb.ScriptEngine.TestingSC.AddCode(RoutinesEdit.Text);
      Glb.ScriptEngine.RoutinesCompiled := True;
      CompileLog := TStringList.Create;
      try
        if not CompileAll(CompileLog) then
        begin
          lErrorMessage := txt.Values['SomeMacrosNotCompiledWithRoutines'];
          lErrorMessage := lErrorMessage + #13 + txt.Values['CompilationLog'] + #13;
          lErrorMessage := lErrorMessage + CompileLog.Text;
          ShowMessage(lErrorMessage);
        end;
      finally
        CompileLog.Free;
      end;
    except
      On E: Exception do
      begin
        Glb.ScriptEngine.RoutinesCompiled := False;
        for I := 0 to MacrosLB.Count - 1 do
        begin
          mr := TKeyboardMacro(MacrosLB.Items.Objects[I]);
          if (mr.Action = maScript) then
            mr.ScriptCompiled := False;
        end;
        ShowMessage(Format(txt.Values['CompileErrorS'], [E.Message]));
        RoutinesEdit.SetFocus;
        RoutinesEdit.SelStart := RoutinesEdit.Perform(
            EM_LINEINDEX, Glb.ScriptEngine.TestingSC.Error.Line-1, 0) + Glb.ScriptEngine.TestingSC.Error.Column;
      end;
    end;
  finally
    Glb.ScriptEngine.Compiling := False;
  end;
  RefreshCompileIndicators;
end;

procedure THIDMacrosForm.LogEb;
var
  I: Integer;
  Line: String;
  P: PRawInputEvent;
begin
  DebugLog('*** Event buffer *** (' + IntToStr(eb.Count) + ' items)');
  for I := 0 to eb.Count - 1 do
  begin
    P := PRawInputEvent(eb.Items[I]);
    Line := 'DevType: ';
    case P^.EventType of
      etKeyboard: Line := Line + 'Keyboard';
      etMouse: Line := Line + 'Mouse';
      else Line := Line + IntToStr(Ord(P^.EventType));
    end;
    Line := Line + '. Id: ' + GetMessageId(P^.MessageId);
    Line := Line + '. Direction: ';
    case P^.Direction of
      edUp: Line := Line + 'Up';
      edDown: Line := Line + 'Down';
    end;
    Line := Line + '. Code: ' + IntToStr(P^.Code);
    Line := Line + '. Time: ' + IntToStr(P^.Time);
    DebugLog(Line);
  end;
end;

procedure THIDMacrosForm.MacroScriptRichEditChange(Sender: TObject);
var am: TKeyboardMacro;
begin
  // save the source
  if (MacrosLB.ItemIndex >= 0) then
  begin
    am := TKeyboardMacro(MacrosLB.Items.Objects[MacrosLB.ItemIndex]);
    if am.ScriptSource <> MacroScriptRichEdit.Text then
    begin
      am.ScriptSource := MacroScriptRichEdit.Text;
      if am.ScriptCompiled then
      begin
        am.ScriptCompiled := False;
        RefreshCompileMacroIndicators(am);
      end;
    end;
  end;
end;

procedure THIDMacrosForm.MacroScriptRichEditKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  Label24.Caption := GetRECursorPos(MacroScriptRichEdit);
end;

procedure THIDMacrosForm.MacroScriptRichEditMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Label24.Caption := GetRECursorPos(MacroScriptRichEdit);
end;

procedure THIDMacrosForm.MacrosLBClick(Sender: TObject);
begin
  RefreshEditArea;
end;

procedure THIDMacrosForm.ManageTray;
begin
  //if (MinimizeToTray) then
  begin
    TrayIcon1.Visible := MinimizeToTray;
  end;
end;

procedure THIDMacrosForm.MapFormToggle(Sender: TObject);
begin
  MapToggleAction.Checked := Glb.MapForm.IsVisible;
end;

procedure THIDMacrosForm.MapToggleActionExecute(Sender: TObject);
begin
  //MapToggleAction.Checked := not MapToggleAction.Checked;
  Glb.MapForm.Toggle;
end;

procedure THIDMacrosForm.NameEditChange(Sender: TObject);
var am: TKeyboardMacro;
begin
  if (MacrosLB.ItemIndex >= 0) then
  begin
    am := TKeyboardMacro(MacrosLB.Items.Objects[MacrosLB.ItemIndex]);
    am.Name := NameEdit.Text;
    if (am.Name <> '') then
      MacrosLB.Items.Strings[MacrosLB.ItemIndex] := am.Name
    else
      MacrosLB.Items.Strings[MacrosLB.ItemIndex] := '(no name)';
  end;
end;

procedure THIDMacrosForm.NewMacroActionExecute(Sender: TObject);
var
  NewMacro: TKeyboardMacro;
  lNewIndex: Integer;
begin
  NewMacro := TKeyboardMacro.Create;
  NewMacro.Name := 'New Macro';
  NewMacro.ScriptControl := Glb.ScriptEngine.ExecutionSC;
  // add to List
  lNewIndex := MacrosLB.Items.AddObject(NewMacro.Name, NewMacro);
  Glb.MacrosList.Add(NewMacro);
  MacrosLB.ItemIndex := lNewIndex;
  RefreshEditArea;
  NameEdit.SetFocus;
  NameEdit.SelectAll;
end;

procedure THIDMacrosForm.NoOfBackupsEditChange(Sender: TObject);
begin
  Glb.AppConfig.BackupFileCount := StrToIntDef(NoOfBackupsEdit.Text, 0);
end;

procedure THIDMacrosForm.NoOfBackupsEditExit(Sender: TObject);
begin
  NoOfBackupsEdit.Text := IntToStr(Glb.AppConfig.BackupFileCount);
end;

procedure THIDMacrosForm.PageControl1Change(Sender: TObject);
begin
  RefreshEditArea;
  if (PageControl1.ActivePage = ScriptToolsTabSheet) then
  begin
    Timer2.Enabled := True;
    WindowTitlesMemo.Lines.Clear;
    EnumWindows(@EnumWindowsProc,Integer(WindowTitlesMemo.Lines));
  end else if (PageControl1.ActivePage = TabSheet1) then
    RecalcLinkedMacros;
end;

procedure THIDMacrosForm.PageControl2Change(Sender: TObject);
var am: TKeyboardMacro;
begin
  if (MacrosLB.ItemIndex >= 0) then
  begin
    am := TKeyboardMacro(MacrosLB.Items.Objects[MacrosLB.ItemIndex]);
    if PageControl2.ActivePage = ScriptedActionTS then
    begin
      am.Action := maScript;
    end;
  end;
end;

procedure THIDMacrosForm.Panel3Click(Sender: TObject);
begin
 ShellExecute(Application.Handle,
            PChar('open'),
            PChar('http://www.hidmacros.eu/donate.htm'),
            PChar(0),
            nil,
            SW_NORMAL);
end;

procedure THIDMacrosForm.RecalcLinkedMacros;
var
  lDevice: THIDKeyboard;
  I : Integer;
  mr: TKeyboardMacro;
  lLI: TListItem;
begin
  // count linked macros
  for I := 0 to Glb.HIDControl.Count - 1 do
    Glb.HIDControl.Items[I].ResetMacrosCount;
  for I := 0 to MacrosLB.Items.Count - 1 do
  begin
    mr := TKeyboardMacro(MacrosLB.Items.Objects[I]);
    if (mr.Keyboard <> nil) then
      mr.Keyboard.IncMacrosCount;
  end;
  for I := 0 to ListView1.Items.Count - 1 do
  begin
    lLI := ListView1.Items[I];
    lDevice := THIDKeyboard(lLI.Data);
    lLI.SubItems.Strings[1] := IntToStr(lDevice.LinkedMacrosCount);
  end;
end;

procedure THIDMacrosForm.RecalculateSplashScreen;
begin
  if (Width > 200) and (Height > 200) then
  begin
    SplashPanel.Left := 50;
    SplashPanel.Top := 50;
    SplashPanel.Width := Width - 100;
    SplashPanel.Height := Height - 100;
  end
  else
  begin
    SplashPanel.Left := 0;
    SplashPanel.Top := 0;
    SplashPanel.Width := Width;
    SplashPanel.Height := Height;
  end
end;

procedure THIDMacrosForm.RefreshCompileIndicators;
var am: TKeyboardMacro;
begin
  if (MacrosLB.ItemIndex >= 0) then
    am := TKeyboardMacro(MacrosLB.Items.Objects[MacrosLB.ItemIndex])
  else
    am := nil;
  RoutinesOKImage.Visible := Glb.ScriptEngine.RoutinesCompiled;
  RoutinesErrImage.Visible := not Glb.ScriptEngine.RoutinesCompiled;
  if Glb.ScriptEngine.RoutinesCompiled then
    Label17.Caption := txt.Values['Compiled']
  else
    Label17.Caption := txt.Values['NotCompiled'];
  if AllMacrosCompiled and Glb.ScriptEngine.RoutinesCompiled then
  begin
    StatusBar.Panels[1].Text := txt.Values['AllCompiled'];
  end
  else
  begin
    StatusBar.Panels[1].Text := txt.Values['AllNotCompiled'];
  end;
  if (am <> nil) then
    RefreshCompileMacroIndicators(am);
end;

procedure THIDMacrosForm.RefreshCompileMacroIndicators(am: TKeyboardMacro);
begin
  MacroOkImage.Visible := am.ScriptCompiled;
  MacroErrImage.Visible := not am.ScriptCompiled;
  if am.ScriptCompiled then
    Label18.Caption := txt.Values['Compiled']
  else
    Label18.Caption := txt.Values['NotCompiled'];
end;

procedure THIDMacrosForm.RefreshDeviceActions;
var
  lItem: THIDKeyboard;
begin
  if (ListView1.Selected = nil) then
  begin
    RenameDeviceAction.Enabled := False;
    MoveMacrosAction.Enabled := False;
  end
  else
  begin
    RenameDeviceAction.Enabled := True;
    if (ListView1.Selected.SubItems.Strings[1] = '0') then
      MoveMacrosAction.Enabled := False
    else
    begin
      lItem := THIDKeyboard(ListView1.Selected.Data);
      if (lItem is THIDJoystick) then
        MoveMacrosAction.Enabled := Glb.HIDControl.GameCount > 1
      else if (lItem is THIDMouse) then
        MoveMacrosAction.Enabled := Glb.HIDControl.MousesCount > 1
      else
        MoveMacrosAction.Enabled := Glb.HIDControl.KeyboardsCount > 1
    end;
  end;
end;

procedure THIDMacrosForm.RefreshEditArea;
var am: TKeyboardMacro;
    tmpI: Integer;
begin
  if (MacrosLB.ItemIndex >= 0) then
    am := TKeyboardMacro(MacrosLB.Items.Objects[MacrosLB.ItemIndex])
  else
    am := nil;
  // set enabled
  NameEdit.Enabled := (am <> nil);
  SetButton.Enabled := (am <> nil);
  KeyboardRB.Enabled := (am <> nil);
  SimConnectRB.Enabled := (am <> nil) and Glb.FSX.SCAvailable;
  SequenceEdit.Enabled := (am <> nil);
  SimconnectCB.Enabled := (am <> nil) and Glb.FSX.SCAvailable;
  DeleteMacroAction.Enabled := (am <> nil);
  //WindowCB.Enabled := (am <> nil);
  SCTextChB.Enabled := (am <> nil);
  SCParamsEdit.Enabled := (am <> nil);
  SysCommandRB.Enabled := (am <> nil);
  SendToBufferRB.Enabled := (am <> nil);
  CommandEdit.Enabled := (am <> nil);
  FileOpenButton.Enabled := (am <> nil);
  Label18.Visible := (am <> nil);
  MacroOkImage.Visible := (am <> nil);
  MacroErrImage.Visible := (am <> nil);
  XPLCommandRB.Enabled := (am <> nil);
  XPLCommandEdit.Enabled := (am <> nil);
  if (am = nil) then
  begin
    NameEdit.Text := '';
    KeyboardRB.Checked := False;
    SimConnectRB.Checked := False;
    SequenceEdit.Text := '';
    SimconnectCB.Text := '';
    EditKeyboardName.Text := '';
    EditScanCode.Text := '';
    //WindowCB.Text := '';
    SCTextChB.Checked := False;
    SCParamsEdit.Text := '';
    CommandEdit.Text := '';
    XPLCommandEdit.Text := '';
    PageControl2.ActivePage := PredefinedActionTS;
  end
  else
  begin
    NameEdit.Text := am.Name;
    if (am.Keyboard = nil) or (not am.Keyboard.IsAlive) then
      NameEdit.Font.Color := clRed
    else
      NameEdit.Font.Color := clWindowText;
    KeyboardRB.Checked := (am.Action = KeyboardSequence);
    SimConnectRB.Checked := (am.Action = SimConnectEvent);
    SysCommandRB.Checked := (am.Action = maSysCommand);
    SendToBufferRB.Checked := (am.Action = maSendToBuffer);
    XPLCommandRB.Checked := (am.Action = maXPLCommand);
    SendToBufferRB.Enabled := (am.KeyCode >= 0);
    MacroScriptRichEdit.Text := am.ScriptSource;
    if (am.Action = maScript) then
    begin
      PageControl2.ActivePage := ScriptedActionTS
    end
    else
    begin
      PageControl2.ActivePage := PredefinedActionTS;
    end;
    RefreshCompileMacroIndicators(am);
    SequenceEdit.Text := am.Sequence;
    tmpI := Glb.FSX.EventValues.IndexOf(am.SCEvent);
    if (tmpI = -1) then
      tmpI := 0;
    SimconnectCB.ItemIndex := tmpI;
    EditKeyboardName.Text := am.KbdName;
    CommandEdit.Text := am.SysCommand;
    XPLCommandEdit.Text := am.XPLCommand;
    if (am.KeyCode >= 0) then
      EditScanCode.Text := IntToStr(am.KeyCode) +
              '  ('+GetCharFromVirtualKey(am.KeyCode)+')'
    else
    begin
      if am.MouseEvent <> None then
      begin
        if am.MouseEvent = Wheel then
        begin
          if am.Direction = mdDown then
            EditScanCode.Text := txt.Values['WheelDown']
          else
            EditScanCode.Text := txt.Values['WheelUp']
        end
        else
        begin
          case am.MouseEvent of
            KbdMacro.None:   EditScanCode.Text := '';
            KbdMacro.Left:   EditScanCode.Text := txt.Values['LeftButton'];
            KbdMacro.Middle: EditScanCode.Text := txt.Values['MiddleButton'];
            KbdMacro.Right:  EditScanCode.Text := txt.Values['RightButton'];
          end;
          if am.Direction = mdDown then
            EditScanCode.Text := EditScanCode.Text + ' ' + txt.Values['Down']
          else
            EditScanCode.Text := EditScanCode.Text + ' ' + txt.Values['Up'];
        end;
      end
      else if am.ButtonIndex >= 0 then
      begin
        EditScanCode.Text := txt.Values['Button'] + ' ' + IntToStr(am.ButtonIndex + 1);
        if am.Direction = mdDown then
          EditScanCode.Text := EditScanCode.Text + ' ' + txt.Values['Down']
        else
          EditScanCode.Text := EditScanCode.Text + ' ' + txt.Values['Up'];
      end;
    end;

    //WindowCB.Text := am.WindowName;
    SCTextChB.Checked := am.SCText;
    SCParamsEdit.Text := am.SCParams;
    SCParCounLabel.Caption := IntToStr(am.SCParamsCount);
  end;
  RoutinesOKImage.Visible := Glb.ScriptEngine.RoutinesCompiled;
  RoutinesErrImage.Visible := not Glb.ScriptEngine.RoutinesCompiled;
end;

procedure THIDMacrosForm.RegisterEvent;
var
  ids: array[0..1] of RAWINPUTDEVICE;
begin
  ids[0].usUsagePage := 1;
  ids[0].usUsage := 6;  // keyboard
  ids[0].dwFlags := RIDEV_INPUTSINK; // + RIDEV_NOLEGACY;
  ids[0].hwndTarget := Handle;
  ids[1].usUsagePage := 1;
  ids[1].usUsage := 2;  // mouse
  ids[1].dwFlags := RIDEV_INPUTSINK; // + RIDEV_NOLEGACY;
  ids[1].hwndTarget := Handle;
  RegisterRawInputDevices(@ids, 2, sizeOf(RAWINPUTDEVICE));
end;

procedure THIDMacrosForm.RegisterEvents2FS;
var
  I, J: Integer;
  mr: TKeyboardMacro;
begin
  if (not Glb.FSX.SCConnected) then
    Exit;
  J := 0;
  for I := 0 to MacrosLB.Count - 1 do
  begin
    mr := TKeyboardMacro(MacrosLB.Items.Objects[I]);
    if (mr.Action = SimConnectEvent) and (mr.SCEvent > '') then
    begin
      if (SUCCEEDED(SimConnect_MapClientEventToSimEvent(Glb.FSX.hSimConnect, J, mr.SCEvent))) then
      begin
        mr.SCEventIndex := J;
        mr.SCHandle := Glb.FSX.hSimConnect;
        DebugLog('SC: Event ' + mr.SCEvent + ' registered as ' + IntToStr(J));
        Inc(J);
      end
      else
        DebugLog('SC: Error registering event ' + mr.SCEvent + ' as ' + IntToStr(J));
    end;
  end;
end;

procedure THIDMacrosForm.RenameDeviceActionExecute(Sender: TObject);
begin
  if ListView1.Selected <> nil then
  begin
    ListView1.Selected.EditCaption;
  end;
end;

procedure THIDMacrosForm.RoutinesEditChange(Sender: TObject);
begin
  if Glb.ScriptEngine.RoutinesCompiled then
  begin
    Glb.ScriptEngine.RoutinesCompiled := False;
    RefreshCompileIndicators;
  end;
end;

procedure THIDMacrosForm.RoutinesEditKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Label25.Caption := GetRECursorPos(RoutinesEdit);
end;

procedure THIDMacrosForm.RoutinesEditMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Label25.Caption := GetRECursorPos(RoutinesEdit);
end;

{-----------------------------------------------------------------------------
  Procedure: SimConnectMessage
  This uses CallDispatch, but could probably avoid the callback and use
  SimConnect_GetNextDispatch instead.
  Author:    ken.adam
  Date:      11-Jan-2007
  Arguments:
    var Message: TMessage
-----------------------------------------------------------------------------}

procedure THIDMacrosForm.SaveActionExecute(Sender: TObject);
begin
  Glb.AppConfig.SaveToXml(True);
end;

procedure THIDMacrosForm.SaveScriptExecute(Sender: TObject);
begin
  SaveToIni;
end;


procedure THIDMacrosForm.SaveToIni;
var
  ini: TiniFile;
  I, DevNo:Integer;
  kb : THIDKeyboard;
  mr : TKeyboardMacro;
  fn : String;
begin
  fn := ExtractFilePath(Application.ExeName)+'\'+CfgFileName;
  if (FileExists(fn)) then
    DeleteFile(fn);
  ini := TIniFile.Create(fn);
  try
    // Save language
    ini.WriteString('Settings', 'Language', Language);
    ini.WriteBool('Settings', 'MinimizeToTray', MinimizeToTray);
    ini.WriteBool('Settings', 'StartMinimized', StartMinimized);

    // save keyboards
    DevNo := 0;
    for I := 0 to ListView1.Items.Count - 1 do
    begin
      if (TObject(ListView1.Items[I].Data) is THIDKeyboard) and not (
      (TObject(ListView1.Items[I].Data) is THIDMouse) or (TObject(ListView1.Items[I].Data) is THIDJoystick)
      ) then
      begin
        kb := THIDKeyboard(ListView1.Items[I].Data);
        kb.SaveToIni(ini, 'Keyboard.'+IntToStr(DevNo));
        Inc(DevNo);
      end;
    end;
    // save mouses
    DevNo := 0;
    for I := 0 to ListView1.Items.Count - 1 do
    begin
      if (TObject(ListView1.Items[I].Data) is THIDMouse) then
      begin
        kb := THIDKeyboard(ListView1.Items[I].Data);
        kb.SaveToIni(ini, 'Mouse.'+IntToStr(DevNo));
        Inc(DevNo);
      end;
    end;
    // save game devs
    DevNo := 0;
    for I := 0 to ListView1.Items.Count - 1 do
    begin
      if (TObject(ListView1.Items[I].Data) is THIDJoystick) then
      begin
        kb := THIDJoystick(ListView1.Items[I].Data);
        kb.SaveToIni(ini, 'Game.'+IntToStr(DevNo));
        Inc(DevNo);
      end;
    end;
    // save macros
    for I := 0 to MacrosLB.Count - 1 do
    begin
      mr := TKeyboardMacro(MacrosLB.Items.Objects[I]);
      mr.SaveToIni(ini, 'Macro.'+IntToStr(I));
    end;
  finally
    ini.Free;
  end;
end;

procedure THIDMacrosForm.SaveToXml(RootNode, parentNode : IXMLNode);
var
  I:Integer;
  mr : TKeyboardMacro;
  aNode: IXMLNode;
begin
  aNode := parentNode.AddChild('Language');
  aNode.Text := Language;
  aNode := parentNode.AddChild('ScriptLanguage');
  aNode.Text := ScriptLanguageCB.Text;
  aNode := parentNode.AddChild('ProcBegin');
  //aNode.Text := ScriptEngine.ProcBegin;
  aNode.Text := ProcBeginEdit.Text; // could be changed but not loaded to SE
  aNode := parentNode.AddChild('ProcEnd');
  //aNode.Text := ScriptEngine.ProcEnd;
  aNode.Text := ProcEndEdit.Text;
  aNode := parentNode.AddChild('ScriptRoutines');
  aNode.Text := RoutinesEdit.Text;
  aNode := parentNode.AddChild('MinimizeToTray');
  if MinimizeToTray then
    aNode.Text := '1'
  else
    aNode.Text := '0';
  aNode := parentNode.AddChild('StartMinimized');
  if StartMinimized then
    aNode.Text := '1'
  else
    aNode.Text := '0';
  aNode := parentNode.AddChild('AllowScriptGUI');
  if Glb.ScriptEngine.AllowGUI then
    aNode.Text := '1'
  else
    aNode.Text := '0';
  aNode := parentNode.AddChild('ScriptTimeout');
  aNode.Text := IntToStr(Glb.ScriptEngine.ScriptTimeout);
  aNode := parentNode.AddChild('BufferTimeout');
  aNode.Text := IntToStr(Glb.Buffer.ResetInterval);
  aNode := parentNode.AddChild('BufferNotification');
  if fBufferNotification then
    aNode.Text := '1'
  else
    aNode.Text := '0';
  if (Timer1.Interval <> 50) then
  begin
    aNode := parentNode.AddChild('GameTick');
    aNode.Text := IntToStr(Timer1.Interval);
  end;
  if Glb.Xpl.ActiveDuringCompilation then
  begin
    aNode := parentNode.AddChild('XPLDuringCompilation');
    aNode.Text := '1';
  end;
  // save macros
  parentNode := RootNode.AddChild('Macros');
  for I := 0 to MacrosLB.Count - 1 do
  begin
    mr := TKeyboardMacro(MacrosLB.Items.Objects[I]);
    mr.SaveToXml(parentNode, 'Macro');
  end;
end;


procedure THIDMacrosForm.SCDisconnect(Sender: TObject);
begin
  Quit := True;
  StatusBar.Panels[0].Text := txt.Values['FSXQuit'];
  StartEvent.Enabled := True;
end;

procedure THIDMacrosForm.SCParamsEditChange(Sender: TObject);
var am: TKeyboardMacro;
begin
  if (MacrosLB.ItemIndex >= 0) then
  begin
    //ShowMessage(Copy(SCParamsEdit.Text, 1, 1));
    am := TKeyboardMacro(MacrosLB.Items.Objects[MacrosLB.ItemIndex]);
    am.SCParams := SCParamsEdit.Text;
    SCParCounLabel.Caption := IntToStr(am.SCParamsCount);
  end;
end;

procedure THIDMacrosForm.ScriptLanguageCBChange(Sender: TObject);
begin
  if Glb.ScriptEngine.RoutinesCompiled then
  begin
    Glb.ScriptEngine.RoutinesCompiled := False;
    RefreshCompileIndicators;
  end;
  if (UpperCase(ScriptLanguageCB.Text) = 'VBSCRIPT') or
     (UpperCase(ScriptLanguageCB.Text) = 'JSCRIPT') then
  begin
    Glb.ScriptEngine.Language := ScriptLanguageCB.Text;
    ProcBeginEdit.Text := Glb.ScriptEngine.ProcBegin;
    ProcEndEdit.Text := Glb.ScriptEngine.ProcEnd;
    ProcBeginEdit.Enabled := False;
    ProcEndEdit.Enabled := False;
  end
  else
  begin
    ProcBeginEdit.Enabled := True;
    ProcEndEdit.Enabled := True;
  end;
end;

procedure THIDMacrosForm.ScriptLogHandler(pValue: String);
var lDate: String;
begin
  lDate := FormatDateTime('h:nn:ss.zzz', Now());
  ErrorLogMemo.Lines.Insert(0, lDate+': '+pValue);
  while ErrorLogMemo.Lines.Count > 100 do
    ErrorLogMemo.Lines.Delete(ErrorLogMemo.Lines.Count - 1);
end;

procedure THIDMacrosForm.ScriptTestButtonClick(Sender: TObject);
var
  Bounds : array [0..0] of TSafeArrayBound;
  Params : PSafeArray;
  tmpCode, RoutinesErrorMessage: String;
begin
  Glb.ScriptEngine.Language := ScriptLanguageCB.Text;
  Glb.ScriptEngine.ProcBegin := ProcBeginEdit.Text;
  Glb.ScriptEngine.ProcEnd := ProcEndEdit.Text;
  Glb.ScriptEngine.ResetTestingSC;
  try
    Glb.ScriptEngine.RoutinesSource := RoutinesEdit.Text;
    Glb.ScriptEngine.TestingSC.AddCode(RoutinesEdit.Text);
    Glb.ScriptEngine.RoutinesCompiled := True;
  except
    On E: Exception do
    begin
      Glb.ScriptEngine.RoutinesCompiled := False;
      RoutinesErrorMessage := E.Message;
    end;
  end;
  if not Glb.ScriptEngine.RoutinesCompiled then
  begin
    // exec test code without routines, if it fails, we display
    // also error frm routines
    Glb.ScriptEngine.Language := ScriptLanguageCB.Text;
    Glb.ScriptEngine.ProcBegin := ProcBeginEdit.Text;
    Glb.ScriptEngine.ProcEnd := ProcEndEdit.Text;
    Glb.ScriptEngine.ResetTestingSC;
  end;
  tmpCode := Format(Glb.ScriptEngine.ProcBegin + #13#10 + '%s' + #13#10 + Glb.ScriptEngine.ProcEnd,
      ['tmpCode', TestRichEdit.Text]);
  try
    Glb.ScriptEngine.TestingSC.AddCode(tmpCode);
    // no params
    Bounds[0].cElements:=0;
    Bounds[0].lLbound:=0;
    Params:=SafeArrayCreate(varVariant, 1, Bounds);
    Glb.ScriptEngine.TestingSC.Run('tmpCode', Params);
  except
    On E: Exception do
    begin
      if Glb.ScriptEngine.RoutinesCompiled then
        ShowMessage(Format(txt.Values['CodeErrorS'], [E.Message]))
      else
        ShowMessage(Format(txt.Values['TestCodeErrorRoutinesSS'], [E.Message, RoutinesErrorMessage]));
    end;
  end;
  RefreshCompileIndicators;
end;

procedure THIDMacrosForm.ScriptTimeOutEditChange(Sender: TObject);
begin
  Glb.ScriptEngine.ScriptTimeout := StrToIntDef(ScriptTimeOutEdit.Text, 10);
end;

procedure THIDMacrosForm.SCTextChBClick(Sender: TObject);
var am: TKeyboardMacro;
begin
  if (MacrosLB.ItemIndex >= 0) then
  begin
    am := TKeyboardMacro(MacrosLB.Items.Objects[MacrosLB.ItemIndex]);
    am.SCText := SCTextChB.Checked;
  end;
end;

function THIDMacrosForm.SearchEvent(WParam, LParam: Integer): Integer;
var
  I: Integer;
  ie: PRawInputEvent;
begin
  Result := -1;
  for I := 0 to eb.Count - 1 do
  begin
    ie := PRawInputEvent(eb[I]);
    if (ie^.MessageId = WParam) then
    begin
      if (ie^.EventType = etKeyboard) then
      begin
        if (ie^.Code = LParam) then
        begin
          Result := I;
          break;
        end
      end
      else if (ie^.EventType = etMouse) then
      begin
        // here I don't have mouse direction from DLL so have to ignore that
        // I just received the same message
        Result := I;
        break;
      end;
    end;
  end;
end;

procedure THIDMacrosForm.SelectPathButtonClick(Sender: TObject);
var
Directory: string;
begin
  if SelectDirectory('Select directory', '', Directory) then
    ShowMessage('You''ve selected directory ' + Directory);
end;

procedure THIDMacrosForm.SequenceEditChange(Sender: TObject);
var am: TKeyboardMacro;
begin
  if (MacrosLB.ItemIndex >= 0) then
  begin
    am := TKeyboardMacro(MacrosLB.Items.Objects[MacrosLB.ItemIndex]);
    am.Sequence := SequenceEdit.Text;
  end;
end;

procedure THIDMacrosForm.SetButtonClick(Sender: TObject);
begin
  Scanning := saDefine;
  // show splash
  SplashPanel.Visible := True;
end;

procedure THIDMacrosForm.ShortenEB;
var
  tc : Cardinal;
begin
  if eb.Count > 0 then
  begin
    tc := GetTickCount;
    if (PRawInputEvent(eb[0])^.Time + 100 < tc) or
       ((PRawInputEvent(eb[0])^.Time > tc) and (tc > 100)) then
    begin
      DebugLog('Warning: Too old message index 0 will be removed now. Current log:');
      LogEb;
      Dispose(eb.Items[0]);
      eb.Delete(0);
    end;
    //if (TraceLog <> nil) then
    //  TabSheet5.Caption := IntToStr(eb.Count);
  end;
end;

procedure THIDMacrosForm.ShowBufferChkBClick(Sender: TObject);
begin
  fBufferNotification := ShowBufferChkB.Checked;
end;

procedure THIDMacrosForm.SimconnectCBChange(Sender: TObject);
var am: TKeyboardMacro;
begin
  if (MacrosLB.ItemIndex >= 0) then
  begin
    am := TKeyboardMacro(MacrosLB.Items.Objects[MacrosLB.ItemIndex]);
    am.SCEvent := Glb.FSX.EventValues[SimconnectCB.ItemIndex];
  end;
end;

procedure THIDMacrosForm.SimConnectMessage(var Message: TMessage);
begin
  Glb.FSX.SimConnectMessage(Message);
end;

procedure THIDMacrosForm.SpeedButton1Click(Sender: TObject);
begin

end;

{-----------------------------------------------------------------------------
  Procedure: StartEventExecute
  Opens the connection for Event driven handling. If successful sets up the data
  requests and hooks the system event "SimStart".
  Author:    ken.adam
  Date:      11-Jan-2007
  Arguments:
    Sender: TObject
-----------------------------------------------------------------------------}

procedure THIDMacrosForm.StartEventExecute(Sender: TObject);
begin
  Glb.FSX.Connect(Handle);
  if Glb.FSX.SCConnected then
  begin
    StatusBar.Panels[0].Text := txt.Values['SCConnected']; // this was also in SC proc...
    StartEvent.Enabled := False;
  end;
end;

procedure THIDMacrosForm.StartMinimizedCBClick(Sender: TObject);
begin
  StartMinimized := StartMinimizedCB.Checked;
end;

procedure THIDMacrosForm.StatusBarDrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
begin
  if Panel.Index = 1 then
  begin
    if (not AllMacrosCompiled) or (not Glb.ScriptEngine.RoutinesCompiled) then
      StatusBar.Canvas.Font.Color := clRed;
   //Panel background color
   StatusBar.Canvas.FillRect(Rect) ;
   //Panel Text
   StatusBar.Canvas.TextRect(Rect,2 + Rect.Left, Rect.Top,Panel.Text) ;
  end;
end;

procedure THIDMacrosForm.TabSheet4Show(Sender: TObject);
begin
  if VersionLabel.Caption = '?' then
    VersionLabel.Caption := Sto_GetFmtFileVersion();
end;

procedure THIDMacrosForm.TabSheet7Show(Sender: TObject);
begin
  Timer2.Enabled := True;
  WindowTitlesMemo.Lines.Clear;
  EnumWindows(@EnumWindowsProc,Integer(WindowTitlesMemo.Lines));
end;

procedure THIDMacrosForm.TestMacroScriptBClick(Sender: TObject);
var am: TKeyboardMacro;
begin
  if (MacrosLB.ItemIndex >= 0) then
  begin
    am := TKeyboardMacro(MacrosLB.Items.Objects[MacrosLB.ItemIndex]);
    if am.ScriptCompiled then
      am.Fire
    else
    begin
      if CompileMacro(am) then
        am.Fire;
    end;
    RefreshCompileMacroIndicators(am);
  end;
end;

procedure THIDMacrosForm.Timer1Timer(Sender: TObject);
var
  I: Integer;
  joy: THIDJoystick;
begin
  if not Glb.HIDControl.GameAvailable then
    exit;
  // go through game devices
  for I := 0 to ListView1.Items.Count - 1 do
    if (TObject(ListView1.Items[I].Data) is THIDJoystick) then
    begin
      joy := THIDJoystick(ListView1.Items[I].Data);
      joy.GenerateEvents(Handle);
    end;
  //DebugLog('TICK');
end;

procedure THIDMacrosForm.Timer2Timer(Sender: TObject);
var
  MausPos: TPoint;
begin
  if (PageControl1.ActivePage = ScriptToolsTabSheet) then
  begin
    if (PageControl3.ActivePage = TSMouseCoordinates) then
    begin
      GetCursorPos(MausPos);
      EditX.Text := IntToStr(MausPos.x);
      EditY.Text := IntToStr(MausPos.y);
    end;
    if (PageControl3.ActivePage = TSGameAxis) then
    begin
      FillGameGrid;
    end;
  end
  else
  begin
    Timer2.Enabled := False;
    EditX.Text := '-';
    EditY.Text := '-';
  end;
end;

procedure THIDMacrosForm.TrayIcon1DblClick(Sender: TObject);
begin
  Show;
  Application.Restore;
  Application.BringToFront;
end;

procedure THIDMacrosForm.UpdateTestArea;
var
  ie: PRawInputEvent;
  I, DeviceIndex: Integer;
  kbd: THIDKeyboard;
  lMsgCode: Cardinal;
begin
  if (eb.Count > 0) then
  begin
    ie := PRawInputEvent(eb[eb.Count - 1]);
    // search for Keyboard
    for I := 0 to ListView1.Items.Count - 1 do
    begin
      kbd := THIDKeyboard(ListView1.Items[I].Data);
      if (kbd.Handle = ie^.DeviceHandle) then
      begin
        TestKeyboardName.Text := kbd.Name;
        DeviceIndex := I;
        break;
      end;
    end;
    // update test area
    if ie^.EventType = etKeyboard then
      TestScanCode.Text := IntToStr(ie^.Code) +
          '  ('+GetCharFromVirtualKey(ie^.Code)+')'
    else if ie^.EventType = etMouse then
    begin
      case ie^.MessageId of
        WM_LBUTTONDOWN : TestScanCode.Text := txt.Values['Left'] + ' ' + LowerCase(txt.Values['Button']) + ' ' + txt.Values['Down'];
        WM_LBUTTONUP   : TestScanCode.Text := txt.Values['Left'] + ' ' + LowerCase(txt.Values['Button']) + ' ' + txt.Values['Up'];
        WM_MBUTTONDOWN : TestScanCode.Text := txt.Values['Middle'] + ' ' + LowerCase(txt.Values['Button']) + ' ' + txt.Values['Down'];
        WM_MBUTTONUP   : TestScanCode.Text := txt.Values['Middle'] + ' ' + LowerCase(txt.Values['Button']) + ' ' + txt.Values['Up'];
        WM_RBUTTONDOWN : TestScanCode.Text := txt.Values['Right'] + ' ' + LowerCase(txt.Values['Button']) + ' ' + txt.Values['Down'];
        WM_RBUTTONUP   : TestScanCode.Text := txt.Values['Right'] + ' ' + LowerCase(txt.Values['Button']) + ' ' + txt.Values['Up'];
        WM_MOUSEWHEEL  : begin
          if ie^.Direction = edDown then
            TestScanCode.Text := txt.Values['WheelDown']
          else
            TestScanCode.Text := txt.Values['WheelUp']
        end;
      end;
    end;
    if Scanning <> saDisabled then
    begin
      ScannedEvent := ie^;
      if Scanning = saDefine then
        lMsgCode := WM_SCANNED
      else if Scanning = saFind then
        lMsgCode := WM_FINDMACRO;
      Scanning := saDisabled;
      SplashPanel.Visible := False;
      // PostMessage to not delay DLL message processing (app is waiting for an answer)
      PostMessage(Handle, lMsgCode, DeviceIndex, 0);
    end;
  end;
end;

procedure THIDMacrosForm.WindowCBChange(Sender: TObject);
var am: TKeyboardMacro;
begin
  if (MacrosLB.ItemIndex >= 0) then
  begin
    //am := TKeyboardMacro(MacrosLB.Items.Objects[MacrosLB.ItemIndex]);
    //am.WindowName := WindowCB.Text;
  end;
end;

procedure THIDMacrosForm.WmFindMacroMessage(var Message: TMessage);
var
  mr: TKeyboardMacro;
  I, foundI: Integer;
begin
  foundI := -1;
  // go thorugh macros
  for I := 0 to MacrosLB.Count - 1 do
  begin
    mr := TKeyboardMacro(MacrosLB.Items.Objects[I]);
    if (ScannedEvent.MessageId = WM_JOY_BUTTON_DOWN) and
       (mr.Keyboard = THIDKeyboard(ScannedEvent.DeviceHandle)) and
       (mr.ButtonIndex = Message.WParam) and
       (
         ((ScannedEvent.Direction = edDown) and (mr.Direction = mdDown)) or
         ((ScannedEvent.Direction = edUp) and (mr.Direction = mdUp))
       ) then
    begin
      foundI := I;
      break;
    end;
    if CheckFire(@ScannedEvent, mr) then
    begin
      foundI := I;
      break;
    end;
  end;
  if foundI >= 0 then
  begin
    MacrosLB.ItemIndex := foundI;
    MacrosLB.SetFocus;
    RefreshEditArea;
  end
end;

procedure THIDMacrosForm.WmFireMessage(var Message: TMessage);
var
  mr: TKeyboardMacro;
begin
  mr := TKeyboardMacro(Message.WParam);
  if mr <> nil then
    mr.Fire;
end;

procedure THIDMacrosForm.WmHookMessage(var Message: TMessage);
var
  I: Integer;
  mr: TKeyboardMacro;
  im : tagMSG;
  imTMessage: TMessage;
  ie: PRawInputEvent;
  HookedMessgae: Word;
  EventIndex: Integer;
  BeFired: Boolean;
  lMacroThread: TMacroThread;
begin
  // for now just log the message
  DebugLog('Received DLL asking message: WParam ' + GetMessageId(Message.WParam) + ', LParam ' + IntToHex(Message.LParam, 4));
  // convert non-client area events
  case Message.WParam of
    WM_NCLBUTTONDOWN: HookedMessgae := WM_LBUTTONDOWN;
    WM_NCLBUTTONUP: HookedMessgae := WM_LBUTTONUP;
    WM_NCMBUTTONDOWN: HookedMessgae := WM_MBUTTONDOWN;
    WM_NCMBUTTONUP: HookedMessgae := WM_MBUTTONUP;
    WM_NCRBUTTONDOWN: HookedMessgae := WM_RBUTTONDOWN;
    WM_NCRBUTTONUP: HookedMessgae := WM_RBUTTONUP;
    else HookedMessgae := Message.WParam;
  end;
  if (Message.LParam = 1) and (
      (HookedMessgae = WM_LBUTTONDOWN) or
      (HookedMessgae = WM_LBUTTONUP) or
      (HookedMessgae = WM_MBUTTONDOWN) or
      (HookedMessgae = WM_MBUTTONUP) or
      (HookedMessgae = WM_RBUTTONDOWN) or
      (HookedMessgae = WM_RBUTTONUP) or
      (HookedMessgae = WM_MOUSEWHEEL))
      then
  begin
    // mouse message for this window, can't call peekmessage - it would go
    // to infite loop (probably because of MS bug)
    Message.Result := 0; // let the message pass through hook
    // but let's try to search & dispose
    EventIndex := SearchEvent(HookedMessgae, Message.LParam);
    if (EventIndex >= 0) then
    begin
      ie := PRawInputEvent(eb[EventIndex]);
      Dispose(ie);
      eb.Delete(EventIndex);
    end;
    UpdateTestArea;
    ShortenEB;
    exit;
  end;
  // search for the message in event list
  EventIndex := SearchEvent(HookedMessgae, Message.LParam);
  if EventIndex < 0 then
  begin
    // sometimes this message comes before WM_IPNUT and HID device is not set then
    // so check whether there's some WM_INPUT message in the queue
    DebugLog('Nothing found, trying to process pending WM_INPUT messages.');
    MouseMovementMessageCounter := 0;
    while (PeekMessage(im, Handle, WM_INPUT, WM_INPUT, PM_REMOVE)) do
    begin
      //DebugLog('Hook message '+IntToStr(Message.WParam)+' came, but there is some input, so process it first.');
      imTMessage.Msg := im.message;
      imTMessage.WParam := im.wParam;
      imTMessage.LParam:= im.lParam;
      WmInputMessage(imTMessage);
    end;
    DebugLog('End of processing WM_INPUT. Ignored '+IntToStr(MouseMovementMessageCounter)+' mouse empty messages.');
    EventIndex := SearchEvent(HookedMessgae, Message.LParam);
  end;
  Message.Result := 0; // let the message pass through hook
  if EventIndex >= 0 then
  begin
    ie := PRawInputEvent(eb[EventIndex]);
    // event found, look in macro list and ignore list
    for I := 0 to MacrosLB.Items.Count - 1 do
    begin
      mr := TKeyboardMacro(MacrosLB.Items.Objects[I]);
      if CheckFire(ie, mr) then
      begin
        // cancel real key meaning
        Message.Result := -1;
        try
          //lMacroThread := TMacroThread.Create(mr);
          //mr.Fire; - can not delay DLL questions,
          //schedule macro execution via PostMessage to myself
          PostMessage(Handle, WM_FIRE, Integer(mr), 0);
          //lMacroThread.Resume;
          DebugLog('Fired macro ' + mr.Name);
        except
          on E:Exception do
            DebugLog('Macro exception ' + E.Message);
        end;
        break;
      end;
    end;
    // now ignore list
    for I := 0 to ignoreEvents.Count - 1 do
    begin
      mr := TKeyboardMacro(ignoreEvents[I]);
      if CheckFire(ie, mr) then
      begin
        // cancel real key meaning
        Message.Result := -1;
        DebugLog('Found in ignore list, no fire, just stop message.');
        break;
      end;
    end;
    // update test area
    UpdateTestArea;
    // release recorded WM_INPUT event
    Dispose(ie);
    eb.Delete(EventIndex);
  end;
  // now check if there if no expired message in log
  // shouldn't happen, but let's prevent overflow
  ShortenEB;
end;


{
Old version:

procedure THIDMacrosForm.WmHookMessage(var Message: TMessage);
var
  I: Integer;
  mr: TKeyboardMacro;
  im : tagMSG;
  imTMessage: TMessage;
begin
  // sometimes this message comes before WM_IPNUT and HID device is not set then
  // so check whether there's some WM_INPUT message in the queue
  if (PeekMessage(im, Handle, WM_INPUT, WM_INPUT, PM_REMOVE)) then
  begin
    DebugLog('Hook message '+IntToStr(Message.WParam)+' came, but there is some input, so process it first.');
    imTMessage.Msg := im.message;
    imTMessage.WParam := im.wParam;
    imTMessage.LParam:= im.lParam;
    WmInputMessage(imTMessage);
  end;
  DebugLog('Hook message: ' + IntToStr(Message.WParam) + ', last dev ' + IntToStr(LastDevId));
  // go through Macros
  for I := 0 to MacrosLB.Count - 1 do
  begin
    mr := TKeyboardMacro(MacrosLB.Items.Objects[I]);
    if (mr.Keyboard <> nil) and (mr.Keyboard.Handle = LastDevId) and
        (mr.KeyCode = Message.WParam) then
    begin
      LastDevId := 0; // to not fire trigger by itself
      // fired
      mr.Fire;
      DebugLog('Fired');
      // cancel real key meaning
      Message.Result := -1;
      break;
    end;
  end;
end;
}

procedure THIDMacrosForm.WmInputMessage(var Message: TMessage);
var
  pcbSize: UINT;
  buff: PRAWINPUT;
  ValidEntry: Boolean;
  NewEvent: PRawInputEvent;
begin
  if (IgnoreInputMessage) then
    Exit;
  GetRawInputData(Message.LParam, RID_INPUT, nil, pcbSize, sizeOf(RAWINPUTHEADER));
  GetMem(buff, pcbSize);
  try
    if (GetRawInputData(Message.LParam, RID_INPUT, buff,
        pcbSize, sizeOf(RAWINPUTHEADER)) = pcbSize) then
    begin
      //
      if (buff^.header.dwType = RIM_TYPEKEYBOARD)then
      begin
        ValidEntry := True;
        New(NewEvent);
        NewEvent^.DeviceHandle := buff^.header.hDevice;
        NewEvent^.Time := GetMessageTime;
        NewEvent^.EventType := etKeyboard;
        case buff^.keyboard.Message of
          WM_KEYDOWN, WM_SYSKEYDOWN: NewEvent^.Direction := edDown;
          WM_KEYUP, WM_SYSKEYUP: NewEvent^.Direction := edUp;
          else
            ValidEntry := False;
        end;
        NewEvent^.Code := buff^.keyboard.VKey;
        NewEvent^.MessageId := buff^.keyboard.Message;
        if (ValidEntry) then
          eb.Add(NewEvent)
        else
          Dispose(NewEvent);

        DebugLog('WM_INPUT ('+IntToStr(GetMessageTime)+') KEYBOARD message ' + GetMessageId(buff^.keyboard.Message) +
            '. Key code: ' + IntToStr(buff^.keyboard.VKey) +
            '. Dev handle: ' + IntToStr(buff^.header.hDevice) + ', ext info:' + IntToStr(buff^.keyboard.ExtraInformation));
        //if (TraceLog <> nil) then
        //  TabSheet5.Caption := IntToStr(eb.Count);
      end
      else
      begin
        // now the mouse part
        if (buff^.header.dwType = RIM_TYPEMOUSE) then //and (not MyApplicationActive) then
        begin
          // full log - uncomment for moevement & empty msg trace
          //DebugLog('WM_INPUT MOUSE usFlags ' + IntToStr(buff^.mouse.usFlags) +
          //  '. Button flags: ' + IntToStr(buff^.mouse.union.usButtonFlags) +
          //  '. Button data :' + IntToStr(buff^.mouse.union.usButtonData) +
          //  '. X :' + IntToStr(buff^.mouse.lLastX) +
          //  '. Y :' + IntToStr(buff^.mouse.lLastY) +
          //  '. ExtraInfo :' + IntToStr(buff^.mouse.ulExtraInformation)
          //  );
          if (buff^.mouse.union.usButtonFlags > 0)  then
          begin
            New(NewEvent);
            ValidEntry := True;
            NewEvent^.DeviceHandle := buff^.header.hDevice;
            NewEvent^.Time := GetMessageTime;
            NewEvent^.EventType := etMouse;
            NewEvent^.Code := 0;
            case buff^.mouse.union.usButtonFlags of
              RI_MOUSE_LEFT_BUTTON_DOWN: begin
                NewEvent^.Direction := edDown;
                NewEvent^.MessageId := WM_LBUTTONDOWN; // borrow constants for WM message for easy match
              end;
              RI_MOUSE_MIDDLE_BUTTON_DOWN: begin
                NewEvent^.Direction := edDown;
                NewEvent^.MessageId := WM_MBUTTONDOWN;
              end;
              RI_MOUSE_RIGHT_BUTTON_DOWN: begin
                NewEvent^.Direction := edDown;
                NewEvent^.MessageId := WM_RBUTTONDOWN;
              end;
              RI_MOUSE_LEFT_BUTTON_UP: begin
                NewEvent^.Direction := edUp;
                NewEvent^.MessageId := WM_LBUTTONUP;
              end;
              RI_MOUSE_MIDDLE_BUTTON_UP: begin
                NewEvent^.Direction := edUp;
                NewEvent^.MessageId := WM_MBUTTONUP;
              end;
              RI_MOUSE_RIGHT_BUTTON_UP: begin
                NewEvent^.Direction := edUp;
                NewEvent^.MessageId := WM_RBUTTONUP;
              end;
              RI_MOUSE_WHEEL: begin
                if (buff^.mouse.union.usButtonData < $08000) then
                  NewEvent^.Direction := edUp
                else
                  NewEvent^.Direction := edDown;
                NewEvent^.MessageId := WM_MOUSEWHEEL;
              end
              else
                ValidEntry := False;
            end;
            if (ValidEntry) then
              eb.Add(NewEvent)
            else
              Dispose(NewEvent);

            DebugLog('WM_INPUT ('+IntToStr(GetMessageTime)+') MOUSE usFlags ' + IntToStr(buff^.mouse.usFlags) +
              '. Button flags: ' + IntToStr(buff^.mouse.union.usButtonFlags) +
              //'. Button data :' + IntToStr(buff^.mouse.union.usButtonData) +
              '. Dev handle: ' + IntToStr(buff^.header.hDevice) +
              '. X :' + IntToStr(buff^.mouse.lLastX) +
              '. Y :' + IntToStr(buff^.mouse.lLastY) +
              '. ExtraInfo :' + IntToStr(buff^.mouse.ulExtraInformation)
              );
            //if (TraceLog <> nil) then
            //  TabSheet5.Caption := IntToStr(eb.Count);
          end
          else
            Inc(MouseMovementMessageCounter); // counter of useless mouse messages
        end;
      end;
    end;
  finally
    FreeMem(buff);
  end;
  //inherited WndProc(Message);
end;


procedure THIDMacrosForm.WmJoyButtonDown(var Message: TMessage);
begin
  GameButtonEvent(THIDJoystick(Message.wParam), Message.LParam, edDown);
end;

procedure THIDMacrosForm.WmJoyButtonUp(var Message: TMessage);
begin
  GameButtonEvent(THIDJoystick(Message.wParam), Message.LParam, edUp);
end;

procedure THIDMacrosForm.WmMidiKeyEvent(var Message: TMessage);
begin
  MidiEvent(TMIDIDevice(Message.wParam), TMyMidiEvent(Message.LParam));
end;

procedure THIDMacrosForm.WmScannedMessage(var Message: TMessage);
var
  am, mr, tmpMacro, foundMacro: TKeyboardMacro;
  I, ResI: Integer;
begin
  // find active macro
  if MacrosLB.ItemIndex >= 0 then
    am := TKeyboardMacro(MacrosLB.Items.Objects[MacrosLB.ItemIndex])
  else
    am := nil;
  if am = nil then
    exit;
  tmpMacro := TKeyboardMacro.Create;
  // prepare scanned values to tmpMacro for better handling
  if ScannedEvent.MessageId = WM_MIDI_KEY then
  begin
    tmpMacro.Keyboard := TMIDIDevice(ScannedEvent.DeviceHandle);
    tmpMacro.MouseEvent := None;
    tmpMacro.KeyCode := -1;
    tmpMacro.ButtonIndex := Message.WParam;
    if ScannedEvent.Direction = edDown then
      tmpMacro.Direction := mdDown
    else
      tmpMacro.Direction := mdUp;
  end
  else
  if ScannedEvent.MessageId = WM_JOY_BUTTON_DOWN then
  begin
    tmpMacro.Keyboard := THIDKeyboard(ScannedEvent.DeviceHandle);
    tmpMacro.MouseEvent := None;
    tmpMacro.KeyCode := -1;
    tmpMacro.ButtonIndex := Message.WParam;
    if ScannedEvent.Direction = edDown then
      tmpMacro.Direction := mdDown
    else
      tmpMacro.Direction := mdUp;
    // Display form to tune up/down event
    GameDialog.InitByMacro(tmpMacro);
    //SetDialogHandle(GameDialog.Handle);
    if (GameDialog.ShowModal = mrOk) then
    begin
      // store values
      GameDialog.SetMacroAttribs(tmpMacro);
    end
    else
      am := nil; // user cancelled scan
    //SetDialogHandle(0);
  end
  else
  begin
    tmpMacro.Keyboard := THIDKeyboard(ListView1.Items[Message.wParam].Data);
    // if mouse scan was performed, tune final setting with dialog
    if ScannedEvent.EventType = etMouse then
    begin
      // show window for mouse set
      MouseDialog.MouseComboBox.Items.Clear;
      MouseDialog.MouseComboBox.Items.Add(ListView1.Items[Message.wParam].Caption);
      MouseDialog.MouseComboBox.ItemIndex := 0;
      MouseDialog.MouseComboBox.Enabled := False;
      case ScannedEvent.MessageId of
        WM_LBUTTONDOWN : MouseDialog.RBLeftDown.Checked := True;
        WM_LBUTTONUP   : MouseDialog.RBLeftDown.Checked := True;
        WM_MBUTTONDOWN : MouseDialog.RBMiddleDown.Checked := True;
        WM_MBUTTONUP   : MouseDialog.RBMiddleDown.Checked := True;
        WM_RBUTTONDOWN : MouseDialog.RBRightDown.Checked := True;
        WM_RBUTTONUP   : MouseDialog.RBRightDown.Checked := True;
        WM_MOUSEWHEEL  : begin
          if ScannedEvent.Direction = edDown then
            MouseDialog.RBWheelDown.Checked := True
          else
            MouseDialog.RBWheelUp.Checked := True;
        end;
      end;
      //SetDialogHandle(MouseDialog.Handle);
      if (MouseDialog.ShowModal = mrOk) then
      begin
        // store values
        MouseDialog.SetMouseAttribs(tmpMacro);
      end
      else
        am := nil; // user cancelled scan
      //SetDialogHandle(0);
      tmpMacro.KeyCode := -1;
      tmpMacro.ButtonIndex := -1;
    end else if ScannedEvent.EventType = etKeyboard then
    begin
      tmpMacro.MouseEvent := None;
      tmpMacro.KeyCode := ScannedEvent.Code;
      tmpMacro.Direction := mdDown; // keys are always down for now
      tmpMacro.ButtonIndex := -1;
    end;
  end;
  if am = nil then
  begin
    tmpMacro.Free;
    exit;
  end;
  // now search for duplicity
  foundMacro := nil;
  for I := 0 to MacrosLB.Count - 1 do
    if (I <> MacrosLB.ItemIndex) then
    begin
      mr := TKeyboardMacro(MacrosLB.Items.Objects[I]);
      if tmpMacro.HaveSameTrigger(mr) then
      begin
        foundMacro := mr;
        break;
      end;
    end;
  if (foundMacro <> nil) then
  begin
    ResI := MessageDlgTranslated(
        Format(txt.Values['AlreadyUsedS'], [foundMacro.Name])+#13+#10+#13+#10+
        Format(txt.Values['AUIgnoreS'], [foundMacro.Name])+#13+#10+
        txt.Values['AURetry']+#13+#10+
        txt.Values['AUAbort'],
        mtWarning, [mbIgnore, mbRetry, mbAbort], mbIgnore);
    case ResI of
      mrRetry: SetButtonClick(nil);
      mrAbort: am := nil;
      mrIgnore:
      begin
        // unlink old macro
        foundMacro.KbdName := '';
        foundMacro.Keyboard := nil;
        foundMacro.KeyCode := -1;
        foundMacro.ButtonIndex := -1;
        foundMacro.MouseEvent := None;
      end;
    end
  end;
  if am = nil then
  begin
    tmpMacro.Free;
    exit;
  end;
  // finaly change macro's definition
  am.KbdName := tmpMacro.Keyboard.Name;
  am.Keyboard := tmpMacro.Keyboard;
  am.KeyCode := tmpMacro.KeyCode;
  am.MouseEvent := tmpMacro.MouseEvent;
  am.Direction := tmpMacro.Direction;
  am.ButtonIndex := tmpMacro.ButtonIndex;
  // refresh
  BuildIgnoreList;
  RefreshEditArea;
  tmpMacro.Free;
end;

procedure THIDMacrosForm.XPLCommandEditOldChange(Sender: TObject);
var am: TKeyboardMacro;
begin
  if (MacrosLB.ItemIndex >= 0) then
  begin
    am := TKeyboardMacro(MacrosLB.Items.Objects[MacrosLB.ItemIndex]);
    am.XPLCommand := XPLCommandEdit.Text;
  end;
end;

procedure THIDMacrosForm.InitMMF;
begin
  fSharedPtr := nil;
  try
    fMMF := TMemMap.Create(MMFName, SizeOf(TMMFData));
    fSharedPtr := fMMF.Memory;
  except
    on EMemMapException do
    begin
      OutputDebugString('GEN: Main form can''t create MMF.');
      fMMF := nil;
    end;
  end;
  if fSharedPtr <> nil then
  begin
    fSharedPtr^.MainWinHandle := Handle;
    fSharedPtr^.HdmPID := GetCurrentProcessId;
  end; 
end;

procedure THIDMacrosForm.FreeMMF;
begin
  if fMMF <> nil then
    try
      fSharedPtr := nil;
      fMMF.Free;
    except
      on EMemMapException do
        OutputDebugString('GEN: Main form can''t release MMF.');
    end;
end;

function THIDMacrosForm.MessageDlgTranslated(const Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; DefaultButton: TMsgDlgBtn; HelpCtx: Longint = 0): Integer;
var d: TForm;
    b: TButton;
begin
  d := CreateMessageDialog(Msg, DlgType, Buttons);
  try
    d.Position := poScreenCenter;
    case DlgType of
      mtWarning: d.Caption := txt.Values['Warning'];
      mtError: d.Caption := txt.Values['Error'];
      mtInformation: d.Caption := txt.Values['Information'];
      mtConfirmation: d.Caption := txt.Values['Confirmation'];
    end;

    b := d.FindChildControl('Yes') as TButton;
    if b <> nil then
    begin
      b.Caption := txt.Values['Yes'];
      if DefaultButton = mbYes then
        b.TabOrder := 0;
    end;
    b := d.FindChildControl('No') as TButton;
    if b <> nil then
    begin
      b.Caption := txt.Values['No'];
      if DefaultButton = mbNo then
        b.TabOrder := 0;
    end;
    b := d.FindChildControl('Cancel') as TButton;
    if b <> nil then
    begin
      b.Caption := txt.Values['Cancel'];
      if DefaultButton = mbCancel then
        b.TabOrder := 0;
    end;
    b := d.FindChildControl('OK') as TButton;
    if b <> nil then
    begin
      b.Caption := txt.Values['Ok'];
      if DefaultButton = mbOK then
        b.TabOrder := 0;
    end;
    b := d.FindChildControl('Abort') as TButton;
    if b <> nil then
    begin
      b.Caption := txt.Values['Abort'];
      if DefaultButton = mbAbort then
        b.TabOrder := 0;
    end;
    b := d.FindChildControl('Retry') as TButton;
    if b <> nil then
    begin
      b.Caption := txt.Values['Retry'];
      b.Width := b.Width + 20;
      if DefaultButton = mbRetry then
        b.TabOrder := 0;
    end;
    b := d.FindChildControl('Ignore') as TButton;
    if b <> nil then
    begin
      b.Caption := txt.Values['Ignore'];
      b.Left := b.Left + 20;
      if DefaultButton = mbIgnore then
        b.TabOrder := 0;
    end;
    b := d.FindChildControl('Yes to All') as TButton;
    if b <> nil then
    begin
      b.Caption := txt.Values['YesToAll'];
      if DefaultButton = mbYesToAll then
        b.TabOrder := 0;
    end;
    b := d.FindChildControl('No to All') as TButton;
    if b <> nil then
    begin
      b.Caption := txt.Values['NoToAll'];
      if DefaultButton = mbNoToAll then
        b.TabOrder := 0;
    end;
    b := d.FindChildControl('Help') as TButton;
    if b <> nil then
    begin
      b.Caption := txt.Values['Help'];
      if DefaultButton = mbHelp then
        b.TabOrder := 0;
    end;
    Result := d.ShowModal;
  finally
    d.Free;
  end;
end;

{
Old version:

procedure THIDMacrosForm.WmInputMessage(var Message: TMessage);
var
  pcbSize: UINT;
  buff: PRAWINPUT;
  I, J, ResI, Wheel: Integer;
  am: TKeyboardMacro;
  kbd, mou : THIDKeyboard;
  mr, foundMacro: TKeyboardMacro;
  theME: TMouseEvent;
begin
  if (IgnoreInputMessage) then
    Exit;
  GetRawInputData(Message.LParam, RID_INPUT, nil, pcbSize, sizeOf(RAWINPUTHEADER));
  kbd := nil;
  GetMem(buff, pcbSize);
  try
    if (GetRawInputData(Message.LParam, RID_INPUT, buff,
        pcbSize, sizeOf(RAWINPUTHEADER)) = pcbSize) then
    begin
      if (buff^.header.dwType = RIM_TYPEKEYBOARD) and (buff^.keyboard.ExtraInformation <> 13) then // 13 = special value in sendkey macro code (sndkey32)
      begin
        DebugLog('WM_INPUT ('+IntToStr(GetMessageTime)+') KEYBOARD message ' + IntToStr(buff^.keyboard.Message) +
        '. Key code: ' + IntToStr(buff^.keyboard.VKey) +
            '. Dev handle: ' + IntToStr(buff^.header.hDevice) + ', ext info:' + IntToStr(buff^.keyboard.ExtraInformation));
        if (buff^.keyboard.Message = WM_KEYDOWN) or (buff^.keyboard.Message = WM_SYSKEYDOWN) then // only for key down
        begin
          LastDevId := buff^.header.hDevice;

          // search for Keyboard
          for I := 0 to ListView1.Items.Count - 1 do
          begin
            kbd := THIDKeyboard(ListView1.Items[I].Data);
            if (kbd.Handle = LastDevId) then
            begin
              TestKeyboardName.Text := kbd.Name;
              break;
            end;
          end;
          // update test area
          TestScanCode.Text := IntToStr(buff^.keyboard.VKey) +
              '  ('+GetCharFromVirtualKey(buff^.keyboard.VKey)+')';
          if (Scanning) and (kbd <> nil) and (MacrosLB.ItemIndex >= 0) then
          begin
            DebugLog('Recording scan code...');
            // go through existing macros
            foundMacro := nil;
            for J := 0 to MacrosLB.Count - 1 do
              if (J <> MacrosLB.ItemIndex) then
              begin
                mr := TKeyboardMacro(MacrosLB.Items.Objects[J]);
                if (mr.KbdName = TestKeyboardName.Text) and (mr.KeyCode = buff^.keyboard.VKey) then
                begin
                  foundMacro := mr;
                  break;
                end;
              end;
            ResI := -1;
            if (foundMacro <> nil) then
            begin
              IgnoreInputMessage := True;
              ResI := MessageDlg(
                  'This key is already used for macro ' + foundMacro.Name + '.'+#13+#10+#13+#10+
                  'Press Ignore to use this key anyway and unlink macro '+foundMacro.Name+'.'+#13+#10+
                  'Press Retry to scan different key.'+#13+#10+
                  'Press Abort to cancel scanning.',
                  mtWarning, [mbIgnore, mbRetry, mbAbort], 0);
              IgnoreInputMessage := False;
            end;
            if ResI <> mrRetry then
            begin
              Scanning := False;
              // splash off
              SplashPanel.Visible := False;
              if ResI <> mrAbort then
              begin
                am := TKeyboardMacro(MacrosLB.Items.Objects[MacrosLB.ItemIndex]);
                am.KbdName := kbd.Name;
                am.Keyboard := kbd;
                am.KeyCode := buff^.keyboard.VKey;
                if (ResI = mrIgnore) then
                begin
                  // unlink old macro
                  foundMacro.KbdName := '';
                  foundMacro.Keyboard := nil;
                  foundMacro.KeyCode := 0;
                end;
                //DebugLog('Panel off...');
                RefreshEditArea;
              end;
            end;
          end
          else
            Scanning := False;
        end
      end else
      // now the mouse part
      if (buff^.header.dwType = RIM_TYPEMOUSE) then
        begin
          // full log - uncomment for moevement & empty msg trace
          //DebugLog('WM_INPUT MOUSE usFlags ' + IntToStr(buff^.mouse.usFlags) +
          //  '. Button flags: ' + IntToStr(buff^.mouse.union.usButtonFlags) +
          //  '. Button data :' + IntToStr(buff^.mouse.union.usButtonData) +
          //  '. X :' + IntToStr(buff^.mouse.lLastX) +
          //  '. Y :' + IntToStr(buff^.mouse.lLastY) +
          //  '. ExtraInfo :' + IntToStr(buff^.mouse.ulExtraInformation)
          //  );

        if (buff^.mouse.union.usButtonFlags > 0) then
        begin
          LastMouseDevId := buff^.header.hDevice;
          DebugLog('WM_INPUT ('+IntToStr(GetMessageTime)+') MOUSE usFlags ' + IntToStr(buff^.mouse.usFlags) +
            '. Button flags: ' + IntToStr(buff^.mouse.union.usButtonFlags) +
            '. Button data :' + IntToStr(buff^.mouse.union.usButtonData) +
            '. X :' + IntToStr(buff^.mouse.lLastX) +
            '. Y :' + IntToStr(buff^.mouse.lLastY) +
            '. ExtraInfo :' + IntToStr(buff^.mouse.ulExtraInformation)
            );
          // search for Mouse
          for I := 0 to ListView1.Items.Count - 1 do
          begin
            mou := THIDKeyboard(ListView1.Items[I].Data); // can be used, inheritence
            if (mou.Handle = LastMouseDevId) then
            begin
              TestMouseName.Text := mou.Name;
              break;
            end;
          end;
          // update test area
          theME := None;
          if (buff^.mouse.union.usButtonFlags = RI_MOUSE_WHEEL) then
          begin
            Wheel :=  (buff^.mouse.union.usButtonData);
            if (Wheel < $08000) then
            begin
              theME := WheelUp;
            end;
            if (Wheel > $08000) then
            begin
              theME := WheelDown;
            end;
          end else if (buff^.mouse.union.usButtonFlags = RI_MOUSE_LEFT_BUTTON_DOWN) then
          begin
            theME := KbdMacro.Left;
          end else if (buff^.mouse.union.usButtonFlags = RI_MOUSE_MIDDLE_BUTTON_DOWN) then
          begin
            theME := Middle;
          end else if (buff^.mouse.union.usButtonFlags = RI_MOUSE_RIGHT_BUTTON_DOWN) then
          begin
            theME := Right;
          end;
          if (theME <> None) then
            TestMouseEvent.Text := MouseEventCaptions[theME];
          // something to fire?
          // go through Macros
          for I := 0 to MacrosLB.Count - 1 do
          begin
            mr := TKeyboardMacro(MacrosLB.Items.Objects[I]);
            if (mr.Keyboard <> nil) and (mr.Keyboard.Handle = LastMouseDevId) and
               (mr.MouseEvent = theME) then
            begin
              mouseIgnoreStart := GetMessageTime();
              // fired
              mr.Fire;
              DebugLog('Fired - mouse time ' + IntToStr(mouseIgnoreStart));
              SetLastMouseFireTick(mouseIgnoreStart);
              break;
            end;
          end;
        end;
      end;
    end;
  finally
    FreeMem(buff);
  end;
  //inherited WndProc(Message);
end;
}

end.

