{ 08/02/2007 14:06:50 (GMT+0:00) > [Akadamia] checked in   }
{ 08/02/2007 14:06:50 (GMT+0:00) > [Akadamia] checked in   }
{ 15/01/2007 13:29:11 (GMT+0:00) > [ken.adam on GLKC2353537] checked in FSX SDK Set Data example  }
{ 15/01/2007 13:29:08 (GMT+0:00) > [ken.adam on GLKC2353537] checked in FSX SDK Set Data example  }
{ 11/01/2007 15:18:15 (GMT+0:00) > [ken.adam on GLKC2353537] checked in   }
{ 11/01/2007 15:14:14 (GMT+0:00) > [ken.adam on GLKC2353537] checked in Prototype translation of SimConnect.h to Pascal  }
{ 11/01/2007 15:14:10 (GMT+0:00) > [ken.adam on GLKC2353537] checked in Prototype translation of SimConnect.h to Pascal  }
program HIDMacros;

{%TogetherDiagram 'ModelSupport_HIDMacros\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_HIDMacros\HIDMacros\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_HIDMacros\uHIDControl\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_HIDMacros\uGlobals\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_HIDMacros\GameDevDialog\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_HIDMacros\MemMap\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_HIDMacros\uSendKeys\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_HIDMacros\uMidiDevices\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_HIDMacros\uScriptEngine\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_HIDMacros\HIDMacros_TLB\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_HIDMacros\uGameDevices\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_HIDMacros\FPCuser\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_HIDMacros\MouseDialog\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_HIDMacros\uBuffer\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_HIDMacros\uMoveMacrosForm\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_HIDMacros\MSScriptControl_TLB\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_HIDMacros\FSUIPCcontrol\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_HIDMacros\uXplControl\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_HIDMacros\uHidMacrosIntf\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_HIDMacros\uMapForm\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_HIDMacros\uHookCommon\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_HIDMacros\Localizer\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_HIDMacros\uRawInput\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_HIDMacros\uAutoPilot\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_HIDMacros\CfgForm\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_HIDMacros\FSXcontrol\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_HIDMacros\uConfig\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_HIDMacros\uXplCommon\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_HIDMacros\KbdMacro\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_HIDMacros\default.txvpck'}

uses
  Forms,
  Windows,
  CfgForm in 'CfgForm.pas' {HIDMacrosForm},
  uRawInput in 'uRawInput.pas',
  KbdMacro in 'KbdMacro.pas',
  GameDevDialog in 'GameDevDialog.pas' {GameForm},
  MouseDialog in 'MouseDialog.pas',
  Localizer in 'Localizer.pas',
  MSScriptControl_TLB in 'MSScriptControl_TLB.pas',
  uScriptEngine in 'uScriptEngine.pas',
  FSUIPCcontrol in 'FSUIPCcontrol.pas',
  FPCuser in 'FPCuser.Pas',
  FSXcontrol in 'FSXcontrol.pas',
  uAutoPilot in 'uAutoPilot.pas',
  uGlobals in 'uGlobals.pas',
  uBuffer in 'uBuffer.pas',
  uMidiDevices in 'uMidiDevices.pas',
  uXplControl in 'uXplControl.pas',
  uXplCommon in 'uXplCommon.pas',
  uMapForm in 'uMapForm.pas' {MapForm},
  uHookCommon in 'uHookCommon.pas',
  MemMap in 'MemMap.pas',
  uSendKeys in 'uSendKeys.pas',
  uConfig in 'uConfig.pas',
  uHIDControl in 'uHIDControl.pas',
  uMoveMacrosForm in 'uMoveMacrosForm.pas' {MacrosMoveForm},
  uGameDevices in 'uGameDevices.pas',
  HIDMacros_TLB in 'HIDMacros_TLB.pas',
  uHidMacrosIntf in 'uHidMacrosIntf.pas';

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

