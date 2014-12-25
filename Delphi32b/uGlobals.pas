unit uGlobals;

interface

uses
  uHIDControl, Classes, FSUIPCcontrol, FSXControl, uBuffer, uGameDevices, uXplControl,
  uScriptEngine, uMapForm, CfgForm, KbdMacro, uConfig;

type

  TOnLogEvent = procedure(pMessage: String) of object;

  THDMGlobals = class (TObject)
  private
    fOwner: TComponent;
    fFSX: TFSXcontrol;
    fXpl: TXPLcontrol;
    fFSUIPC: TFSUIPCcontrol;
    fBuffer: THDMBuffer;
    fDebugGroups: String;
    fHIDControl: THIDControl;
    fMacrosList: TMacrosList;
    fMapForm: TMapForm;
    fMainForm: THIDMacrosForm;
    fTraceLog: TStrings;
    fAppConfig: TAppConfig;
    fScriptEngine: TScriptEngine;
    fOnUserLogEvent: TOnLogEvent;
    procedure SetDebugGroups(const Value: String);
  public
    constructor Create(pOwner: TComponent);
    destructor Destroy; Override;
    procedure DebugLog(Value: String; pGroup: String);
    procedure ForcedDebugLog(Value: String; pGroup: String);
    procedure LogError(pValue: String);
    procedure LogMessage(pValue, pType: String);
    function IsModuleLogged(pName: String): Boolean;
    property FSX: TFSXcontrol read fFSX;
    property Xpl: TXPLcontrol read fXpl;
    property FSUIPC: TFSUIPCcontrol read fFSUIPC;
    property Buffer: THDMBuffer read fBuffer;
    property DebugGroups: String read fDebugGroups write SetDebugGroups;
    property Owner: TComponent read fOwner;
    property HIDControl: THIDControl read fHIDControl;
    property MacrosList: TMacrosList read fMacrosList;
    property ScriptEngine: TScriptEngine read fScriptEngine;
    property MapForm: TMapForm read fMapForm write fMapForm;
    property MainForm: THIDMacrosForm read fMainForm write fMainForm;
    property AppConfig: TAppConfig read fAppConfig;
    property OnUserLog: TOnLogEvent read fOnUserLogEvent write fOnUserLogEvent;
  end;

var
  Glb       : THDMGlobals;

function Sto_GetFmtFileVersion(const FileName: String = '';
  const Fmt: String = '%d.%d.%d.%d'): String;

implementation

uses SysUtils, Windows;

function Sto_GetFmtFileVersion(const FileName: String = '';
  const Fmt: String = '%d.%d.%d.%d'): String;
var
  sFileName: String;
  iBufferSize: DWORD;
  iDummy: DWORD;
  pBuffer: Pointer;
  pFileInfo: Pointer;
  iVer: array[1..4] of Word;
begin
  // set default value
  Result := '';
  // get filename of exe/dll if no filename is specified
  sFileName := FileName;
  if (sFileName = '') then
  begin
    // prepare buffer for path and terminating #0
    SetLength(sFileName, MAX_PATH + 1);
    SetLength(sFileName,
      GetModuleFileName(hInstance, PChar(sFileName), MAX_PATH + 1));
  end;
  // get size of version info (0 if no version info exists)
  iBufferSize := GetFileVersionInfoSize(PChar(sFileName), iDummy);
  if (iBufferSize > 0) then
  begin
    GetMem(pBuffer, iBufferSize);
    try
    // get fixed file info (language independent)
    GetFileVersionInfo(PChar(sFileName), 0, iBufferSize, pBuffer);
    VerQueryValue(pBuffer, '\', pFileInfo, iDummy);
    // read version blocks
    iVer[1] := HiWord(PVSFixedFileInfo(pFileInfo)^.dwFileVersionMS);
    iVer[2] := LoWord(PVSFixedFileInfo(pFileInfo)^.dwFileVersionMS);
    iVer[3] := HiWord(PVSFixedFileInfo(pFileInfo)^.dwFileVersionLS);
    iVer[4] := LoWord(PVSFixedFileInfo(pFileInfo)^.dwFileVersionLS);
    finally
      FreeMem(pBuffer);
    end;
    // format result string
    Result := Format(Fmt, [iVer[1], iVer[2], iVer[3], iVer[4]]);
  end;
end;


{ THDMGlobals }

constructor THDMGlobals.Create(pOwner: TComponent);
begin
  fOwner := pOwner;
  fTraceLog := nil;
  if (pOwner is THIDMacrosForm) then
    fMainForm := THIDMacrosForm(pOwner);
  fAppConfig := TAppConfig.Create(self);
  fFSX := TFSXcontrol.Create;
  fXpl := TXPLcontrol.Create(self);
  fFSUIPC := TFSUIPCcontrol.Create;
  fBuffer := THDMBuffer.Create(pOwner);
  fHIDControl := THIDControl.Create;
  fScriptEngine := TScriptEngine.Create(pOwner);
  fMacrosList := TMacrosList.Create;
  fOnUserLogEvent := nil;
end;

procedure THDMGlobals.DebugLog(Value: String; pGroup: String);
begin
  if (fTraceLog <> nil) and (Pos(pGroup, fDebugGroups) > 0) then
  begin
    ForcedDebugLog(Value, pGroup);
  end
end;

destructor THDMGlobals.Destroy;
begin
  if fTraceLog <> nil then
  begin
    fTraceLog.Insert(0, 'Exe build number: ' + Sto_GetFmtFileVersion());
    fTraceLog.SaveToFile('debug.log');
    fTraceLog.Free;
    fTraceLog := nil;
  end;
  fMacrosList.Free;
  fHIDControl.Free;
  fBuffer.Free;
  fFSUIPC.Free;
  fXpl.Free;
  fFSX.Free;
  fScriptEngine.Free;
  fAppConfig.Free;
  inherited;
end;

procedure THDMGlobals.ForcedDebugLog(Value, pGroup: String);
// no checking for debug groups - always log
var
  tmp: PChar;
  lVal: String;
begin
  lVal := 'HIDMACROS:'+ pGroup + ':' + Value;
  GetMem(tmp, Length(lVal) + 1);
  try
    StrPCopy(tmp, lVal);
    OutputDebugString(tmp);
    if (fTraceLog <> nil) then
    begin
      fTraceLog.Add(Format('%s (%d): %s', [FormatDateTime('hh:nn:ss.zzz', time), GetTickCount, Value]));
      // If we have more then 5000, something went wrong. Let's save it to analyze and stop trace
      if fTraceLog.Count > 5000 then
      begin
        fTraceLog.SaveToFile('debug_full.log');
        fTraceLog.Free;
        fTraceLog := nil;
      end;
    end;
  finally
    FreeMem(tmp);
  end;
end;

function THDMGlobals.IsModuleLogged(pName: String): Boolean;
begin
  Result := (fTraceLog <> nil) and (Pos(pName, fDebugGroups) > 0);
end;

procedure THDMGlobals.SetDebugGroups(const Value: String);
begin
  if Value = '' then
    fDebugGroups := 'GEN:HOOK:FSX:FSUIPC:BUF:XPL:XML:HID'
  else
    fDebugGroups := Value;
  //OutputDebugString(PChar(Value));
  if fTraceLog = nil then
    fTraceLog := TStringList.Create;
  ForcedDebugLog('Using debug groups ' + fDebugGroups, 'CONF');
end;

procedure THDMGlobals.LogError(pValue: String);
begin
  LogMessage(pValue, 'ERR');
end;

procedure THDMGlobals.LogMessage(pValue, pType: String);
begin
  if Assigned(fOnUserLogEvent) then
    fOnUserLogEvent(pType+':'+pValue);
end;

end.
