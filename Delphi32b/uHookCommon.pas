unit uHookCommon;

interface

uses Messages, Windows;

const
  MMFName = 'HIDMacrosSharedMem';
  WM_ASKHDMFORM = WM_USER + 300;

type
  PMMFData = ^TMMFData;
  TMMFData = record
    MainWinHandle: HWND;
    HdmPID: DWORD;
  end;


implementation

end.
