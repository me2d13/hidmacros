unit uBuffer;

interface

uses Classes, ExtCtrls;

type
  THDMBuffer = class (TObject)
  private
    fOwner: TComponent;
    fTimer: TTimer;
    fValue: String;
    fOnValueChange: TNotifyEvent;
    procedure OnTimer(Sender: TObject);
    function GetResetInterval: Integer;
    procedure SetResetInterval(const pValue: Integer);
    procedure SetValue(const pValue: String);
    procedure DebugLog(Value: String);
  public
    constructor Create(pOwner: TComponent);
    destructor Destroy; Override;
    procedure Add(const pValue: String);
    procedure Clear;
    property Value: String read fValue write SetValue;
    property ResetInterval: Integer read GetResetInterval write SetResetInterval;
    property OnValueChanged: TNotifyEvent read fOnValueChange write fOnValueChange;
  end;


implementation

uses uGlobals, SysUtils;

{ THDMBuffer }

procedure THDMBuffer.Add(const pValue: String);
begin
  Value := fValue + pValue;
end;

procedure THDMBuffer.Clear;
begin
  DebugLog('Buffer clear on Clear call.');
  Value := '';
end;

constructor THDMBuffer.Create(pOwner: TComponent);
begin
  fOwner := pOwner;
  fTimer := TTimer.Create(fOwner);
  fTimer.Enabled := False;
  fTimer.OnTimer := OnTimer;
  fTimer.Interval := 2000;
end;

procedure THDMBuffer.DebugLog(Value: String);
begin
  Glb.DebugLog(Value, 'BUF');
end;

destructor THDMBuffer.Destroy;
begin
  fTimer.Free;
  inherited;
end;

function THDMBuffer.GetResetInterval: Integer;
begin
  Result := fTimer.Interval;
end;

procedure THDMBuffer.OnTimer(Sender: TObject);
begin
  Value := '';
  fTimer.Enabled := False;
  DebugLog('Buffer clear on timeout.');
end;

procedure THDMBuffer.SetResetInterval(const pValue: Integer);
begin
  if (pValue <> 0) and (pValue < 100) then
    fTimer.Interval := 100
  else
    fTimer.Interval := pValue;
  DebugLog('Reset interval is ' + IntToStr(fTimer.Interval) +
     ', requested ' + IntToStr(pValue));
end;

procedure THDMBuffer.SetValue(const pValue: String);
var
  lChanged: Boolean;
begin
  lChanged := (fValue <> pValue);
  fValue := pValue;
  // reset timer
  if fTimer.Enabled then
    fTimer.Enabled := False; // restart
  if fTimer.Interval > 0 then
    fTimer.Enabled := True;
  if lChanged and Assigned(fOnValueChange) then
    fOnValueChange(Self);
  DebugLog('Buffer: ' + fValue);
end;

end.
