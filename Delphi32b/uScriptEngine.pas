unit uScriptEngine;

interface

uses
  Classes, Windows, MSScriptControl_TLB, uHidmacrosIntf;

type

  TScriptEngine = class
  private
    { Private declarations }
    fTestingSC: TScriptControl;
    fExecutionSC: TScriptControl;
    fRoutinesSource: String;
    fRoutinesCompiled: Boolean;
    fHIDMacrosIntf: THIDMacrosIntf;
    fProcBegin: String;
    fProcEnd: String;
    fCompiling: boolean;
    //fOnDisconnect: TNotifyEvent;
    procedure DebugLog(Value: String);
    function GetLanguage: String;
    procedure SetLanguage(const Value: String);
    procedure SetRoutinesSource(const Value: String);
    function GetAllowGUI: Boolean;
    function GetScriptTimeout: Integer;
    procedure SetAllowGUI(const Value: Boolean);
    procedure SetScriptTimeout(const Value: Integer);
    procedure SetProcBegin(const Value: String);
    procedure SetProcEnd(const Value: String);
  public
    { Public declarations }
    constructor Create(pOwner: TComponent);
    destructor Destroy; Override;
    procedure ResetTestingSC;
    procedure ResetExecutionSC;
    property TestingSC: TScriptControl read fTestingSC;
    property ExecutionSC: TScriptControl read fExecutionSC;
    property RoutinesSource: String read fRoutinesSource write SetRoutinesSource;
    property RoutinesCompiled: Boolean read fRoutinesCompiled write fRoutinesCompiled;
    property Language: String read GetLanguage write SetLanguage;
    property ScriptTimeout: Integer read GetScriptTimeout write SetScriptTimeout;
    property AllowGUI: Boolean read GetAllowGUI write SetAllowGUI;
    property ProcBegin: String read fProcBegin write SetProcBegin;
    property ProcEnd: String read fProcEnd write SetProcEnd;
    property Compiling: Boolean read fCompiling write fCompiling;    
    //property OnDisconnect: TNotifyEvent read fOnDisconnect write fOnDisconnect;
  end;

implementation

uses SysUtils, ComObj, Forms;

constructor TScriptEngine.Create(pOwner: TComponent);
begin
  //inherited;
  fHIDMacrosIntf := THIDMacrosIntf.Create;
  fTestingSC := TScriptControl.Create(pOwner);
  fExecutionSC := TScriptControl.Create(pOwner);
  fTestingSC.SitehWnd := (pOwner as TForm).Handle;
  fExecutionSC.SitehWnd := (pOwner as TForm).Handle;
  SetAllowGUI(False);
  SetLanguage('VBscript');
  AllowGUI := False;
  ScriptTimeout := 10;
  fCompiling := False;
end;

destructor TScriptEngine.Destroy;
begin
  fHIDMacrosIntf.ObjRelease;
  fTestingSC.Free;
  fExecutionSC.Free;
  inherited;
end;

function TScriptEngine.GetAllowGUI: Boolean;
begin
  Result := fExecutionSC.AllowUI;
end;

function TScriptEngine.GetLanguage: String;
begin
  Result := fExecutionSC.Language;
end;

function TScriptEngine.GetScriptTimeout: Integer;
begin
  Result := fExecutionSC.Timeout div 1000;
end;

procedure TScriptEngine.ResetExecutionSC;
begin
  fExecutionSC.Reset;
  fExecutionSC.AddObject('HIDMacros', fHIDMacrosIntf, True);
  fHIDMacrosIntf.ObjAddRef;
end;

procedure TScriptEngine.ResetTestingSC;
begin
  fTestingSC.Reset;
  fTestingSC.AddObject('HIDMacros', fHIDMacrosIntf, True);
  fHIDMacrosIntf.ObjAddRef;
end;

procedure TScriptEngine.SetAllowGUI(const Value: Boolean);
begin
  fExecutionSC.AllowUI := Value;
  fTestingSC.AllowUI := Value;
end;

procedure TScriptEngine.SetLanguage(const Value: String);
begin
  fExecutionSC.Language := Value;
  fTestingSC.Language := Value;
  if UpperCase(Value) = 'VBSCRIPT' then
  begin
    fProcBegin := 'Sub %s';
    fProcEnd := 'End Sub';
  end else if UpperCase(Value) = 'JSCRIPT' then
  begin
    fProcBegin := 'function %s() {';
    fProcEnd := '}';
  end;
end;

procedure TScriptEngine.SetProcBegin(const Value: String);
begin
  if (UpperCase(Value) <> 'VBSCRIPT') and
     (UpperCase(Value) <> 'JSCRIPT') then
      fProcBegin := Value;
end;

procedure TScriptEngine.SetProcEnd(const Value: String);
begin
  if (UpperCase(Value) <> 'VBSCRIPT') and
     (UpperCase(Value) <> 'JSCRIPT') then
    fProcEnd := Value;
end;

procedure TScriptEngine.SetRoutinesSource(const Value: String);
begin
  fRoutinesSource := Value;
end;

procedure TScriptEngine.SetScriptTimeout(const Value: Integer);
begin
  fExecutionSC.Timeout := Value*1000;
  fTestingSC.Timeout := Value*1000;
end;

procedure TScriptEngine.DebugLog(Value: String);
begin
  //if Assigned(fOnClientLog) then
  //  fOnClientLog(Value);
end;

end.
