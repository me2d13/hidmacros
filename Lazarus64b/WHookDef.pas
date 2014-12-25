unit WHookDef;

interface

uses
  Windows;

function SetHook(WinHandle: HWND; MsgToSend: Integer): Boolean; stdcall;
function FreeHook: Boolean; stdcall;

implementation

function SetHook; external 'WINHOOK.DLL' Index 1;
function FreeHook; external 'WINHOOK.DLL' Index 2;

end.
