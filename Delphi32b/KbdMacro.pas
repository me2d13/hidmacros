unit KbdMacro;

interface

uses Windows, IniFiles, Messages, XMLIntf, MSScriptControl_TLB,
  Classes;

type
  TMacroAction = (KeyboardSequence, SimConnectEvent, maSysCommand, maScript,
      maSendToBuffer, maXPLCommand);
  TMouseEvent = (None, Wheel, Left, Middle, Right);
  TMacroDirection = (mdDown, mdUp);


type THIDKeyboard = class
  private
    fName: string;
    fSystemID: string;
    fHandle: HWND;
    fLinkedMacrosCount: Integer;
    function GetIsAlive: boolean; virtual;
  public
    property Name : string read fName write fName;
    property Handle : HWND read fHandle;
    property SystemID : string read fSystemID;
    property IsAlive: boolean read GetIsAlive;
    property LinkedMacrosCount: Integer read fLinkedMacrosCount;
    procedure SaveToIni(f : TIniFile; Section: String); virtual;
    procedure SaveToXML(devices : IXMLNode; MyName: String); virtual;
    procedure ResetMacrosCount;
    procedure IncMacrosCount;
    constructor Create(pSystemID: string; pHandle: HWND);
end;

type THIDMouse = class (THIDKeyboard)
  // nothing, just inherit all
end;

type TKeyboardMacro = class
  private
    fId : Integer;
    fName: string;
    fAction: TMacroAction;
    fSequence: string;
    fSCEvent: string;
    fXPLCommand: string;
    fKbdName: string;
    fKeyboard: THIDKeyboard;
    fKeyCode: Integer;
    fButtonIndex: Integer; // for game devices
    fSCEventIndex: Integer;
    fSCHandle: HWND;
    fWindowName: String;
    fSCText: Boolean;
    fSCParams: String;
    fSCParamsArray: array of Cardinal;
    fSCParamsIndex: Integer;
    fEnteredParams: String;
    fMouseEvent: TMouseEvent;
    fDirection: TMacroDirection;
    fSysCommand: String;
    fScriptSource: String;
    fScriptCompiled: boolean;
    fScriptControl: TScriptControl;
    procedure SetSCParams(const Value: String);
    function GetSCParamsCount: Integer;
    procedure FireSysCommand;
    procedure FireScript();
  public
    property KbdName : string read fKbdName write fKbdName;
    property Keyboard : THIDKeyboard read fKeyboard write fKeyboard;
    property KeyCode: Integer read fKeyCode write fKeyCode;
    property ButtonIndex: Integer read fButtonIndex write fButtonIndex;
    property MouseEvent: TMouseEvent read fMouseEvent write fMouseEvent;
    property Direction: TMacroDirection read fDirection write fDirection;
    property Name : string read fName write fName;
    property Action : TMacroAction read fAction write fAction;
    property Sequence : string read fSequence write fSequence;
    property SCEvent : string read fSCEvent write fSCEvent;
    property XPLCommand : string read fXPLCommand write fXPLCommand; 
    property SCEventIndex : Integer read fSCEventIndex write fSCEventIndex;
    property SCHandle: HWND read fSCHandle write fSCHandle;
    property SCText: Boolean read fSCText write fSCText;
    property WindowName: String read fWindowName write fWindowName;
    property SCParams: String read fEnteredParams write SetSCParams;
    property SCParamsCount: Integer read GetSCParamsCount;
    property Id: Integer read fId;
    property SysCommand: String read fSysCommand write fSysCommand;
    property ScriptSource : string read fScriptSource write fScriptSource;
    property ScriptCompiled: Boolean read fScriptCompiled write fScriptCompiled;
    property ScriptControl: TScriptControl read fScriptControl write fScriptControl;
    procedure SaveToIni(f : TIniFile; Section: String);
    procedure SaveToXML(devices : IXMLNode; MyName: String);
    function LoadFromIni(f : TIniFile; Section: String): Boolean;
    function LoadFromXml(parent : IXMLNode): Boolean;
    function HaveSameTrigger(m: TKeyboardMacro): Boolean;
    procedure Fire;
    constructor Create;
end;

 TMacroThread = class (TThread)
  private
    fMacro: TKeyboardMacro;
  protected
    procedure Execute; override;
  public
    // Creates thread waiting on request:
    constructor Create(pMacro: TKeyboardMacro);
    destructor Destroy; override;
end;

  TMacrosList = class
  private
    fItems: TList;
    function GetCount: Integer;
    function GetItems(Index: Integer): TKeyboardMacro;
  public
    constructor Create;
    destructor Destroy; Override;
    procedure Add(pMacro: TKeyboardMacro);
    function Delete(pMacro: TKeyboardMacro): Boolean;
    procedure MoveFromTo(pFrom, pTo: THIDKeyboard; var pMoved, pNotMoved: Integer);
    property Items[Index: Integer]: TKeyboardMacro read GetItems;
    property Count: Integer read GetCount;
  end;


implementation

uses uSendKeys, SysUtils, SimConnect, Forms, ShellAPI,
     ActiveX, FSXcontrol, uGlobals; //, Dialogs;

var TMacroId : Integer = 0;

function GetCharFromVKey(vkey: Word): string;
var
   keystate: TKeyboardState;
   retcode: Integer;
begin
   Win32Check(GetKeyboardState(keystate)) ;
   SetLength(Result, 2) ;
   retcode := ToAscii(vkey,
     MapVirtualKey(vkey, 0),
     keystate, @Result[1],
     0) ;
   case retcode of
     0: Result := '';
     1: SetLength(Result, 1) ;
     2: ;
     else
       Result := '';
   end;
end;

{ TKseyboardMacro }

constructor TKeyboardMacro.Create;
begin
  fAction := KeyboardSequence;
  fScriptCompiled := False;
  fKeyboard := nil;
  fSCEventIndex := -1;
  fSCHandle := 0;
  fSCParams := '';
  fSCParamsIndex := 0;
  fEnteredParams := '';
  fKeyCode := -1;
  fButtonIndex := -1;
  fMouseEvent := None;
  fDirection := mdDown;
  fSysCommand := '';
  fId := TMacroId;
  Inc(TMacroId);
  inherited;
end;

procedure TKeyboardMacro.Fire;
var arg: PChar;
    Text2Send: String;
    scValC: Cardinal;
    lSndKey: TKeySequence;
    lFormHasFocus: Boolean;
begin
  //OutputDebugString('[HDM]Fired macro');
  lFormHasFocus := Application.Active;
  // Following condition is present because of bug (probalby)
  // When application starts minimized to tray
  // with Application.ShowMainForm := False
  // then Application.Active return always true for some reason
  // but Screen.ActiveForm is nil luckily
  if lFormHasFocus and (Screen.ActiveForm = nil) then
    lFormHasFocus := False;
  if (fAction = KeyboardSequence) then
  begin
    arg := StrAlloc(Length(fSequence)+1);
    try
      StrPCopy(arg, fSequence);
      begin
        // don't send keystrokes to myself
        if (fWindowName <> '') or (not lFormHasFocus) then
        begin
          //SendKeys(arg, False, fWindowName);
          //SendKeys(arg, False);
          lSndKey := TKeySequence.Create;
          lSndKey.Sequence := fSequence;
          lSndKey.Resume;
        end;
      end;
    finally
      StrDispose(arg);
    end;
  end;
  if (fAction = SimConnectEvent) then
  begin
    if (Length(fSCParamsArray) = 0) then
    begin
      scValC := 0;
      Text2Send := fName;
    end
    else
    begin
      scValC := fSCParamsArray[fSCParamsIndex];
      Text2Send := fName + ' ' + IntToStr(scValC);
      Inc(fSCParamsIndex);
      if (fSCParamsIndex >= Length(fSCParamsArray)) then
        fSCParamsIndex := 0;
    end;
    Glb.FSX.SendEvent(fSCEvent, scValC);
    // text notification
    if fSCText then
      Glb.FSX.SendText(Text2Send);
  end;
  if (fAction = maSysCommand) and (Trim(fSysCommand) <> '') then
  begin
    FireSysCommand();
  end;
  if (fAction = maScript) and (Trim(fScriptSource) <> '') then
  begin
    FireScript();
  end;
  if (fAction = maSendToBuffer) and (fKeyCode >= 0) then // only for keyboard macros
  begin
    Glb.Buffer.Add(GetCharFromVKey(fKeyCode));
  end;
  if (fAction = maXPLCommand) and (Trim(fXPLCommand) <> '') then
  begin
    Glb.Xpl.ExecuteCommand(fXPLCommand);
  end;
end;

procedure TKeyboardMacro.FireScript;
var
  Bounds : array [0..0] of TSafeArrayBound;
  Params : PSafeArray;
begin
  if fScriptCompiled then
  begin
    // no params
    Bounds[0].cElements:=0;
    Bounds[0].lLbound:=0;
    Params:=SafeArrayCreate(varVariant, 1, Bounds);
    fScriptControl.Run('Macro'+intToStr(fId), Params);
    //ShowMessage('Fired');
  end;
end;

procedure TKeyboardMacro.FireSysCommand;
var
  arg, comd, pars: String;
  I: Integer;
begin
  arg := Trim(fSysCommand);
  if Length(arg) = 0 then
    exit;
  if arg[1] = '"' then
  begin
    // serach for second "
    I := Pos('"', Copy(arg, 2, 1000));
    if I = 0 then
      exit;
    comd := Copy(arg,2,I-1);
    I := I + 2;
  end
  else
  begin
    comd := arg;
    I := Pos(' ', arg);
    if I = 0 then
      I := Length(comd)
    else
      comd := Copy(arg, 1, I);
  end;
  // check for params
  if I < Length(arg) then
  begin
    pars := Trim(Copy(arg, I, 1000));
  end
  else
    pars := '';
  ShellExecute(Application.MainForm.Handle, nil, PChar(comd), PChar(pars), nil, SW_SHOWNORMAL);
  //ShellExecute(Application.MainForm.Handle, nil, PChar('C:\Program Files\PuTTY\putty.exe'), PChar('@Linux'), nil, SW_SHOWNORMAL);
  //Showmessage(comd + ' pars: >' + pars + '<');
end;

function TKeyboardMacro.GetSCParamsCount: Integer;
begin
  Result := Length(fSCParamsArray);
end;

function TKeyboardMacro.HaveSameTrigger(m: TKeyboardMacro): Boolean;
begin
  Result := False;
  if (m.Keyboard = nil) or (fKeyboard = nil) then
    exit;
  if m.Keyboard.SystemID <> fKeyboard.SystemID then
    exit;
  if fMouseEvent <> m.MouseEvent then
    exit;
  if fDirection <> m.Direction then
    exit;
  // keyboard test
  if (fKeyCode > 0) and (m.KeyCode <> fKeyCode) then
    exit;
  // game test
  if (fButtonIndex >= 0) and (m.ButtonIndex <> fButtonIndex) then
    exit;
  // mouse event and direction already checked, so it's ok
  Result := True;
end;

function TKeyboardMacro.LoadFromIni(f: TIniFile; Section: String): Boolean;
var tmp: String;
  A: Integer;
  B: TMouseEvent;
begin
  Result := True;
  fKbdName := f.ReadString(Section, 'Keyboard', '');
  fName := f.ReadString(Section, 'Name', '');
  fKeyCode := f.ReadInteger(Section, 'KeyCode', -1);
  fButtonIndex := f.ReadInteger(Section, 'ButtonIndex', 0) - 1;
  A := f.ReadInteger(Section, 'MouseEvent', 0);
  for B := Low(TMouseEvent) to High(TMouseEvent) do
    if (Ord(B) = A) then
    begin
      fMouseEvent := B;
    end;
  tmp := f.ReadString(Section, 'Direction', 'down');
  if UpperCase(tmp) = 'DOWN' then
    fDirection := mdDown
  else if UpperCase(tmp) = 'UP' then
    fDirection := mdUp
  else
    Result := False;
  tmp := f.ReadString(Section, 'Action', '');
  if (tmp = 'SIMC') then
    fAction := SimConnectEvent
  else if (tmp = 'SEQ') then
    fAction := KeyboardSequence
  else if (tmp = 'CMD') then
    fAction := maSysCommand
  else
    Result := False;
  fSequence := f.ReadString(Section, 'Sequence', '');
  fSCEvent := f.ReadString(Section, 'SCEvent', '');
  SCParams := f.ReadString(Section, 'SCParams', '');
  fSCText := f.ReadBool(Section, 'SCText', False);
  fSysCommand := f.ReadString(Section, 'Command', '');
  if FileExists(fSysCommand) and
     (Pos(' ', Trim(fSysCommand)) > 0) and
     (Pos('"', Trim(fSysCommand)) <> 1) then
    fSysCommand := '"'+fSysCommand+'"';
  //if (fKbdName = '') or
  //   (fKeyCode < 0) then
  //  Result := False;
end;

function TKeyboardMacro.LoadFromXml(parent : IXMLNode): Boolean;
var tmp: String;
  A: Integer;
  B: TMouseEvent;
  aNode: IXMLNode;
begin
  Result := True;
  aNode := parent.ChildNodes.First;
  while aNode <> nil do
  begin
    if UpperCase(aNode.NodeName) = 'DEVICE' then
      fKbdName := aNode.Text
    else if UpperCase(aNode.NodeName) = 'NAME' then
      fName := aNode.Text
    else if UpperCase(aNode.NodeName) = 'KEYCODE' then
      fKeyCode := StrToIntDef(aNode.Text, -1)
    else if UpperCase(aNode.NodeName) = 'BUTTONINDEX' then
      fButtonIndex := StrToIntDef(aNode.Text, 0) - 1
    else if UpperCase(aNode.NodeName) = 'SEQUENCE' then
      fSequence := aNode.Text
    else if UpperCase(aNode.NodeName) = 'SCEVENT' then
      fSCEvent := aNode.Text
    else if UpperCase(aNode.NodeName) = 'XPLCOMMAND' then
      fXPLCommand := aNode.Text
    else if UpperCase(aNode.NodeName) = 'SCPARAMS' then
      SCParams := aNode.Text
    else if UpperCase(aNode.NodeName) = 'SCTEXT' then
      fSCText := aNode.Text = '1'
    else if UpperCase(aNode.NodeName) = 'COMMAND' then
      fSysCommand := aNode.Text
    else if UpperCase(aNode.NodeName) = 'SCRIPTSOURCE' then
      fScriptSource := aNode.Text
    else if UpperCase(aNode.NodeName) = 'MOUSEEVENT' then
    begin
      A := StrToIntDef(aNode.Text, 0);
      for B := Low(TMouseEvent) to High(TMouseEvent) do
        if (Ord(B) = A) then
          fMouseEvent := B;
    end
    else if UpperCase(aNode.NodeName) = 'DIRECTION' then
    begin
      tmp := aNode.Text;
      if UpperCase(tmp) = 'DOWN' then
        fDirection := mdDown
      else if UpperCase(tmp) = 'UP' then
        fDirection := mdUp
      else
        Result := False;
    end
    else if UpperCase(aNode.NodeName) = 'ACTION' then
    begin
      tmp := aNode.Text;
      if (tmp = 'SIMC') then
        fAction := SimConnectEvent
      else if (tmp = 'SEQ') then
        fAction := KeyboardSequence
      else if (tmp = 'CMD') then
        fAction := maSysCommand
      else if (tmp = 'BUF') then
        fAction := maSendToBuffer
      else if (tmp = 'XPLC') then
        fAction := maXPLCommand
      else if (tmp = 'SCR') then
        fAction := maScript
      else
        Result := False;
    end;
    aNode := aNode.NextSibling;
  end;
  if FileExists(fSysCommand) and
     (Pos(' ', Trim(fSysCommand)) > 0) and
     (Pos('"', Trim(fSysCommand)) <> 1) then
    fSysCommand := '"'+fSysCommand+'"';
end;

procedure TKeyboardMacro.SaveToIni(f: TIniFile; Section: String);
begin
  f.WriteString(Section, 'Keyboard', fKbdName);
  f.WriteString(Section, 'Name', fName);
  if (fMouseEvent = None) then
  begin
    if (fKeyCode >= 0) then
      f.WriteInteger(Section, 'KeyCode', fKeyCode);
    if (fButtonIndex >= 0) then
      f.WriteInteger(Section, 'ButtonIndex', fButtonIndex + 1);
  end else
    f.WriteInteger(Section, 'MouseEvent', Ord(fMouseEvent));
  if (fDirection=mdDown) then
    f.WriteString(Section, 'Direction', 'down');
  if (fDirection=mdUp) then
    f.WriteString(Section, 'Direction', 'up');
  if (fAction=SimConnectEvent) then
    f.WriteString(Section, 'Action', 'SIMC');
  if (fAction=KeyboardSequence) then
    f.WriteString(Section, 'Action', 'SEQ');
  if (fAction=maSysCommand) then
    f.WriteString(Section, 'Action', 'CMD');
  f.WriteString(Section, 'Sequence', fSequence);
  f.WriteString(Section, 'SCEvent', fSCEvent);
  f.WriteBool(Section, 'SCText', fSCText);
  f.WriteString(Section, 'SCParams', fEnteredParams);
  f.WriteString(Section, 'Command', fSysCommand);
end;

procedure TKeyboardMacro.SaveToXml(devices : IXMLNode; MyName: String);
var parent, x: IXMLNode;
begin
  parent := devices.AddChild(MyName);
  x := parent.AddChild('Device');
  x.Text := fKbdName;
  x := parent.AddChild('Name');
  x.Text := fName;
  if (fMouseEvent = None) then
  begin
    if (fKeyCode >= 0) then
    begin
      x := parent.AddChild('KeyCode');
      x.Text := IntToStr(fKeyCode);
    end;
    if (fButtonIndex >= 0) then
    begin
      x := parent.AddChild('ButtonIndex');
      x.Text := IntToStr(fButtonIndex + 1);
    end;
  end
  else
  begin
    x := parent.AddChild('MouseEvent');
    x.Text := IntToStr(Ord(fMouseEvent));
  end;
  if (fDirection=mdDown) then
  begin
    x := parent.AddChild('Direction');
    x.Text := 'down';
  end;
  if (fDirection=mdUp) then
  begin
    x := parent.AddChild('Direction');
    x.Text := 'up';
  end;
  if (fAction=SimConnectEvent) then
  begin
    x := parent.AddChild('Action');
    x.Text := 'SIMC';
  end;
  if (fAction=KeyboardSequence) then
  begin
    x := parent.AddChild('Action');
    x.Text := 'SEQ';
  end;
  if (fAction=maSysCommand) then
  begin
    x := parent.AddChild('Action');
    x.Text := 'CMD';
  end;
  if (fAction=maScript) then
  begin
    x := parent.AddChild('Action');
    x.Text := 'SCR';
  end;
  if (fAction=maSendToBuffer) then
  begin
    x := parent.AddChild('Action');
    x.Text := 'BUF';
  end;
  if (fAction=maXPLCommand) then
  begin
    x := parent.AddChild('Action');
    x.Text := 'XPLC';
  end;
  x := parent.AddChild('Sequence'); x.Text := fSequence;
  x := parent.AddChild('SCEvent'); x.Text := fSCEvent;
  x := parent.AddChild('XPLCommand'); x.Text := fXPLCommand;
  x := parent.AddChild('ScriptSource'); x.Text := fScriptSource;
  x := parent.AddChild('SCText');
  if fSCText then
    x.Text := '1'
  else
    x.Text := '0';
  x := parent.AddChild('SCParams'); x.Text := fEnteredParams;
  x := parent.AddChild('Command'); x.Text := fSysCommand;
end;


procedure TKeyboardMacro.SetSCParams(const Value: String);
var
  Pos2 : Integer;
  TmpC : Cardinal;
  ToDo : String;
begin
  fSCParams := '';
  fSCParamsIndex := 0;
  fEnteredParams := Value;
  Todo := Value;
  SetLength(fSCParamsArray, 0);
  if (Trim(Value) = '') then
    Exit;
  repeat
    Pos2 := Pos(';', Todo);
    if (Pos2 = 0) then
      Pos2 := Length(ToDo)+1;
    try
      TmpC := StrToInt64(Copy(ToDo, 1, Pos2-1));
      SetLength(fSCParamsArray, Length(fSCParamsArray) + 1);
      fSCParamsArray[Length(fSCParamsArray)-1] := TmpC;
      if (Length(fSCParams) = 0) then
        fSCParams := IntToStr(TmpC)
      else
        fSCParams := fSCParams + ';' + IntToStr(TmpC);
    except
      on E: EConvertError do
        ;
    end;
    ToDo := Copy(ToDo, Pos2+1, Length(Value));
  until Length(ToDo) = 0;
end;

{ THIDKeyboard }

constructor THIDKeyboard.Create(pSystemID: string; pHandle: HWND);
begin
  fSystemID := pSystemID;
  fHandle := pHandle;
  inherited Create;
end;

function THIDKeyboard.GetIsAlive: boolean;
begin
  Result := (fHandle > 0);
end;

procedure THIDKeyboard.IncMacrosCount;
begin
  Inc(fLinkedMacrosCount);
end;

procedure THIDKeyboard.ResetMacrosCount;
begin
  fLinkedMacrosCount := 0;
end;

procedure THIDKeyboard.SaveToIni(f: TIniFile; Section: String);
begin
  f.WriteString(Section, 'Name', fName);
  f.WriteString(Section, 'SystemID', fSystemID);
end;

procedure THIDKeyboard.SaveToXML(devices : IXMLNode; MyName: String);
var parent, x: IXMLNode;
begin
  parent := devices.AddChild(MyName);
  x := parent.AddChild('Name');
  x.Text := fName;
  x := parent.AddChild('SystemID');
  x.Text := fSystemID;
end;


{ TMacroThread }

constructor TMacroThread.Create(pMacro: TKeyboardMacro);
begin
  inherited Create(True);
  fMacro := pMacro;
  Self.FreeOnTerminate:=True;
end;

destructor TMacroThread.Destroy;
begin
  inherited;
end;

procedure TMacroThread.Execute;
var
  tmp: array[0..255] of char;
begin
  try
    fMacro.Fire;
  except
    on E:Exception do
    begin
      StrPCopy(tmp, 'HIDMACROS: Thread exception ' + E.Message);
      OutputDebugString(tmp);
    end;
  end;
end;

{ THIDControl }
{ TMacrosList }

procedure TMacrosList.Add(pMacro: TKeyboardMacro);
begin
  fItems.Add(pMacro);
end;

constructor TMacrosList.Create;
begin
  fItems := TList.Create;
end;

function TMacrosList.Delete(pMacro: TKeyboardMacro): Boolean;
var
  lCount: Integer;
begin
  lCount := fItems.Count;
  Result := fItems.Remove(pMacro) < lCount;
end;

destructor TMacrosList.Destroy;
begin
  fItems.Free;
  inherited;
end;

function TMacrosList.GetCount: Integer;
begin
  Result := fItems.Count;
end;

function TMacrosList.GetItems(Index: Integer): TKeyboardMacro;
begin
  Result := TKeyboardMacro(fItems[Index]);
end;

procedure TMacrosList.MoveFromTo(pFrom, pTo: THIDKeyboard; var pMoved, pNotMoved: Integer);
var
  I, J: Integer;
begin
  pMoved := 0; 
  pNotMoved := 0;
  for I := 0 to fItems.Count - 1 do
    if Items[i].fKeyboard = pFrom then
    begin
      // try to set, then compare if there's any other with the same trigger
      Items[i].fKeyboard := pTo;
      Inc(pMoved);
      Items[i].fKbdName := pTo.Name;
      for J := 0 to fItems.Count - 1 do
        if (i <> j) and (Items[j].HaveSameTrigger(Items[i])) then
        begin
          // can not be moved, revert assignment
          Items[i].fKeyboard := pFrom;
          Items[i].fKbdName := pFrom.Name;
          Dec(pMoved);
          Inc(pNotMoved);
        end;
    end;
end;

end.
