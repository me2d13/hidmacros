{ 08/02/2007 14:06:50 (GMT+0:00) > [Akadamia] checked in   }
{ 08/02/2007 14:06:50 (GMT+0:00) > [Akadamia] checked in   }
{ 15/01/2007 13:29:11 (GMT+0:00) > [ken.adam on GLKC2353537] checked in FSX SDK Set Data example  }
{ 15/01/2007 13:29:08 (GMT+0:00) > [ken.adam on GLKC2353537] checked in FSX SDK Set Data example  }
{ 11/01/2007 15:18:15 (GMT+0:00) > [ken.adam on GLKC2353537] checked in   }
{ 11/01/2007 15:14:14 (GMT+0:00) > [ken.adam on GLKC2353537] checked in Prototype translation of SimConnect.h to Pascal  }
{ 11/01/2007 15:14:10 (GMT+0:00) > [ken.adam on GLKC2353537] checked in Prototype translation of SimConnect.h to Pascal  }
program HIDMacros_noFSX;

uses
  Forms,
  Windows,
  CfgForm in 'CfgForm.pas' {HIDMacrosForm},
  uRawInput in 'uRawInput.pas',
  KbdMacro in 'KbdMacro.pas',
  sndkey32 in 'sndkey32.pas',
  GameDevDialog in 'GameDevDialog.pas' {GameForm},
  MouseDialog in 'MouseDialog.pas',
  Localizer in 'Localizer.pas',
  MSScriptControl_TLB in 'MSScriptControl_TLB.pas',
  uScriptEngine in 'uScriptEngine.pas',
  HIDMacros_noFSX_TLB in 'HIDMacros_noFSX_TLB.pas',
  uHidMacrosIntf in 'uHidMacrosIntf.pas' {THIDMacrosIntf: CoClass},
  FSUIPCcontrol in 'FSUIPCcontrol.pas',
  FPCuser in 'FPCuser.Pas',
  FSXcontrol in 'FSXcontrol.pas',
  uAutoPilot in 'uAutoPilot.pas',
  uGlobals in 'uGlobals.pas',
  uBuffer in 'uBuffer.pas',
  uGameDevices in 'uGameDevices.pas',
  uXplControl in 'uXplControl.pas',
  MemMap in 'MemMap.pas',
  uXplCommon in 'uXplCommon.pas';

{$R *.TLB}

{$R *.res}

var
  PreviousHandle : THandle;
begin
  PreviousHandle := FindWindow('THIDMacrosForm','HID macros');
  if PreviousHandle = 0 then
  begin
    Application.Initialize;
    Application.Title := 'HID macros for FS';
    Application.CreateForm(THIDMacrosForm, HIDMacrosForm);
  Application.Run;
    //OutputDebugString('[HDM]Application shutdown');
  end
  else
    SetForegroundWindow(PreviousHandle);
end.

