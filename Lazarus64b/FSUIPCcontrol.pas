unit FSUIPCcontrol;

interface

uses
  Classes, FPCuser, Windows, Messages;

type

  TFSUIPCcontrol = class
  private
    { Private declarations }
    fConnected : boolean;
    procedure DebugLog(Value: String);
  public
    { Public declarations }
    constructor Create;
    destructor Destroy; Override;
    function Open: Boolean;
    procedure Close;
    function GetDouble(pOffset: Cardinal): Double;
    function GetFloat(pOffset: Cardinal; pSize: Short): Double;
    function GetInt(pOffset: Cardinal; pSize: Short): Int64;
    procedure SetFloat(pOffset: Cardinal; pSize: Short; pValue: Double);
    procedure SetInt(pOffset: Cardinal; pSize: Short; pValue: Int64);
    function GetRaw(pOffset: Cardinal; pSize: Short): String;
    function GetString(pOffset: Cardinal; pSize: Short): String;
    procedure SetString(pOffset: Cardinal; pSize: Short; pValue: String);
    property Connected : boolean read fConnected;
  end;

implementation

uses SysUtils, uGlobals;


procedure TFSUIPCcontrol.Close;
begin
  if fConnected then
  begin
    FSUIPC_Close;
    fConnected := False;
  end;
end;

constructor TFSUIPCcontrol.Create;
begin
  //inherited;
  fConnected := False;
end;

destructor TFSUIPCcontrol.Destroy;
begin
  inherited;
end;

function TFSUIPCcontrol.GetDouble(pOffset: Cardinal): Double;
var
  dwResult : DWORD;
begin
  Result := 0;
  if not fConnected then
    Open;
  if fConnected then
  begin
    if FSUIPC_Read(pOffset, 8, @Result, dwResult) then
    begin
      // "Reed" proceeded without any problems
      if FSUIPC_Process(dwResult) then
        DebugLog('FSUIPC: Variable ' + IntToHex(pOffset, 4) + ' read as '
            + FloatToStr(Result))
      else
        DebugLog('FSUIPC: Error processing variable ' + IntToHex(pOffset, 4) +
            ' dwResult ' + IntToStr(dwResult));
    end
    else
      DebugLog('FSUIPC: Error reading variable ' + IntToHex(pOffset, 4) +
          ' dwResult ' + IntToStr(dwResult));
  end;
end;

function TFSUIPCcontrol.GetFloat(pOffset: Cardinal; pSize: Short): Double;
var
  dwResult : DWORD;
  lValue: Single;
  lValuePtr: PSingle;
  lRes: Boolean;
  lBuffer: array[0..3] of byte;
begin
  Result := 0;
  if not fConnected then
    Open;
  if fConnected then
  begin
    if (pSize <> 4) and (pSize <> 8) then
      pSize := 8;
    if pSize = 4 then
      lRes := FSUIPC_Read(pOffset, 4, @lBuffer[0], dwResult)
    else
      lRes := FSUIPC_Read(pOffset, 8, @Result, dwResult);
    if lRes then
    begin
      // "Reed" proceeded without any problems
      if FSUIPC_Process(dwResult) then
      begin
        if pSize = 4 then
        begin
          lValuePtr := @lBuffer[0];
          Result := lValuePtr^;
          DebugLog(Format('FSUIPC: Variable %x raw data: %2x %2x %2x %2x',
              [pOffset, lBuffer[0], lBuffer[1], lBuffer[2], lBuffer[3]]));
        end;
        DebugLog(Format('FSUIPC: Variable %x read as %5.3f', [pOffset, Result]));
      end
      else
        DebugLog('FSUIPC: Error processing variable ' + IntToHex(pOffset, 4) +
            ' dwResult ' + IntToStr(dwResult));
    end
    else
      DebugLog('FSUIPC: Error reading variable ' + IntToHex(pOffset, 4) +
          ' dwResult ' + IntToStr(dwResult));
  end;
end;

function TFSUIPCcontrol.GetInt(pOffset: Cardinal; pSize: Short): Int64;
var
  dwResult : DWORD;
begin
  Result := 0;
  if not fConnected then
    Open;
  if fConnected then
  begin
    if (pSize < 1) or (pSize > 8) then
      pSize := 1;
    if FSUIPC_Read(pOffset, pSize, @Result, dwResult) then
    begin
      // "Reed" proceeded without any problems
      if FSUIPC_Process(dwResult) then
        DebugLog('FSUIPC: Variable ' + IntToHex(pOffset, 4) + ' read as '
            + IntToStr(Result))
      else
        DebugLog('FSUIPC: Error processing variable ' + IntToHex(pOffset, 4) +
            ' dwResult ' + IntToStr(dwResult));
    end
    else
      DebugLog('FSUIPC: Error reading variable ' + IntToHex(pOffset, 4) +
          ' dwResult ' + IntToStr(dwResult));
  end;
end;

function TFSUIPCcontrol.GetRaw(pOffset: Cardinal; pSize: Short): String;
var
  dwResult : DWORD;
  lRes: Boolean;
  lBuffer, lTmp: ^byte;
  I: Integer;
begin
  if not fConnected then
    Open;
  if fConnected then
  begin
    if (pSize < 0) or (pSize > 256) then
      pSize := 256;
    GetMem(lBuffer, pSize);
    try
      lRes := FSUIPC_Read(pOffset, pSize, lBuffer, dwResult);
      if lRes then
      begin
        // "Reed" proceeded without any problems
        if FSUIPC_Process(dwResult) then
        begin
          Result := '';
          lTmp := lBuffer;
          for I := 0 to pSize - 1 do
          begin
            Result := Result + IntToHex(lTmp^, 2);
            Inc(lTmp);
          end;
          DebugLog(Format('FSUIPC: Variable %x read as %s', [pOffset, Result]))
        end
        else
          DebugLog('FSUIPC: Error processing variable ' + IntToHex(pOffset, 4) +
              ' dwResult ' + IntToStr(dwResult));
      end
      else
        DebugLog('FSUIPC: Error reading variable ' + IntToHex(pOffset, 4) +
            ' dwResult ' + IntToStr(dwResult))
    finally
      FreeMem(lBuffer);
    end;
  end;
end;

function TFSUIPCcontrol.GetString(pOffset: Cardinal; pSize: Short): String;
var
  dwResult : DWORD;
  lRes: Boolean;
  lBuffer, lTmp: ^byte;
  I: Integer;
begin
  if not fConnected then
    Open;
  if fConnected then
  begin
    if (pSize < 0) or (pSize > 256) then
      pSize := 256;
    GetMem(lBuffer, pSize);
    try
      lRes := FSUIPC_Read(pOffset, pSize, lBuffer, dwResult);
      if lRes then
      begin
        // "Read" proceeded without any problems
        if FSUIPC_Process(dwResult) then
        begin
          System.SetString(Result, PChar(lBuffer), pSize);
          lTmp := lBuffer;
          for I := 0 to pSize - 1 do
          begin
            if lTmp^ = 0 then
            begin
              SetLength(Result, I);
              break;
            end;
            Inc(lTmp);
          end;
          DebugLog(Format('FSUIPC: Variable %x read as %s', [pOffset, Result]))
        end
        else
          DebugLog('FSUIPC: Error processing variable ' + IntToHex(pOffset, 4) +
              ' dwResult ' + IntToStr(dwResult));
      end
      else
        DebugLog('FSUIPC: Error reading variable ' + IntToHex(pOffset, 4) +
            ' dwResult ' + IntToStr(dwResult))
    finally
      FreeMem(lBuffer);
    end;
  end;
end;

function TFSUIPCcontrol.Open: Boolean;
var
  dwResult : DWORD;
begin
  if fConnected then
  begin
    FSUIPC_Close;
    fConnected := False;
  end;

  // Try to connect to FSUIPC (or WideFS)
  if FSUIPC_Open(SIM_ANY, dwResult) then
  begin
    fConnected := True;
    DebugLog('FSUIPC: Connected, dwResult ' + IntToStr(dwResult));
  end
  else
    DebugLog('FSUIPC: Not connected, dwResult ' + IntToStr(dwResult));
  Result := fConnected;
end;

procedure TFSUIPCcontrol.SetFloat(pOffset: Cardinal; pSize: Short;
  pValue: Double);
var
  dwResult : DWORD;
begin
  if not fConnected then
    Open;
  if fConnected then
  begin
    if (pSize <> 4) and (pSize <> 8) then
      pSize := 8;
    if FSUIPC_Write(pOffset, pSize, @pValue, dwResult) then
    begin
      // "Reed" proceeded without any problems
      if FSUIPC_Process(dwResult) then
        DebugLog('FSUIPC: Variable ' + IntToHex(pOffset, 4) + ' set to '
            + FloatToStr(pValue))
      else
        DebugLog('FSUIPC: Error processing variable ' + IntToHex(pOffset, 4) +
            ' dwResult ' + IntToStr(dwResult));
    end
    else
      DebugLog('FSUIPC: Error setting variable ' + IntToHex(pOffset, 4) +
          ' dwResult ' + IntToStr(dwResult));
  end;
end;

procedure TFSUIPCcontrol.SetInt(pOffset: Cardinal; pSize: Short;
  pValue: Int64);
var
  dwResult : DWORD;
begin
  if not fConnected then
    Open;
  if fConnected then
  begin
    if (pSize < 1) or (pSize > 8) then
      pSize := 1;
    if FSUIPC_Write(pOffset, pSize, @pValue, dwResult) then
    begin
      // "Reed" proceeded without any problems
      if FSUIPC_Process(dwResult) then
        DebugLog('FSUIPC: Variable ' + IntToHex(pOffset, 4) + ' set to '
            + IntToStr(pValue))
      else
        DebugLog('FSUIPC: Error processing variable ' + IntToHex(pOffset, 4) +
            ' dwResult ' + IntToStr(dwResult));
    end
    else
      DebugLog('FSUIPC: Error setting variable ' + IntToHex(pOffset, 4) +
          ' dwResult ' + IntToStr(dwResult));
  end;
end;

procedure TFSUIPCcontrol.SetString(pOffset: Cardinal; pSize: Short;
  pValue: String);
var
  dwResult : DWORD;
  lRes: Boolean;
  lBuffer: ^byte;
  I: Integer;
begin
  if not fConnected then
    Open;
  if fConnected then
  begin
    if (pSize < 0) or (pSize > 256) then
      pSize := 256;
    GetMem(lBuffer, pSize);
    try
      //FillChar(lBuffer, pSize, 0);
      StrLCopy(PChar(lBuffer), PChar(pValue), pSize-1); // +#0
      if FSUIPC_Write(pOffset, pSize, lBuffer, dwResult) then
      begin
        // "Reed" proceeded without any problems
        if FSUIPC_Process(dwResult) then
          DebugLog('FSUIPC: Variable ' + IntToHex(pOffset, 4) + ' set to '
              + pValue)
        else
          DebugLog('FSUIPC: Error processing variable ' + IntToHex(pOffset, 4) +
              ' dwResult ' + IntToStr(dwResult));
      end
      else
        DebugLog('FSUIPC: Error setting variable ' + IntToHex(pOffset, 4) +
            ' dwResult ' + IntToStr(dwResult));
    finally
      FreeMem(lBuffer);
    end;
  end;
end;

procedure TFSUIPCcontrol.DebugLog(Value: String);
begin
  Glb.DebugLog(Value, 'FSUIPC');
end;

end.
