unit HIDMacros_TLB;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// PASTLWTR : 1.2
// File generated on 30.7.2014 23:05:33 from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\Work\MySources\HidMacros\HIDMacros.tlb (1)
// LIBID: {4EB711B8-7EAC-45BA-A756-1D9BCE0BDA5B}
// LCID: 0
// Helpfile: 
// HelpString: HIDMacros Library
// DepndLst: 
//   (1) v2.0 stdole, (C:\Windows\SysWOW64\stdole2.tlb)
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
interface

uses Windows, ActiveX, Classes, Graphics, StdVCL, Variants;
  

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  HIDMacrosMajorVersion = 1;
  HIDMacrosMinorVersion = 0;

  LIBID_HIDMacros: TGUID = '{4EB711B8-7EAC-45BA-A756-1D9BCE0BDA5B}';

  IID_IHIDMacrosIntf: TGUID = '{D8B14946-3585-4AEE-94A9-D1B5854201F8}';
  CLASS_THIDMacrosIntf: TGUID = '{44F73B9E-30B1-4B22-9C08-421C927AA688}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IHIDMacrosIntf = interface;
  IHIDMacrosIntfDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  THIDMacrosIntf = IHIDMacrosIntf;


// *********************************************************************//
// Interface: IHIDMacrosIntf
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D8B14946-3585-4AEE-94A9-D1B5854201F8}
// *********************************************************************//
  IHIDMacrosIntf = interface(IDispatch)
    ['{D8B14946-3585-4AEE-94A9-D1B5854201F8}']
    procedure Sleep(pMsec: SYSINT); safecall;
    procedure FSXtext(const lValue: WideString); safecall;
    procedure FSXEvent(const pName: WideString; pParam: Double); safecall;
    procedure MouseClick(pLeft: SYSINT; pTop: SYSINT; pAction: Smallint); safecall;
    procedure SendKeys(const pSeqence: WideString); safecall;
    procedure RegisterFSXVariable(const pName: WideString; const pUnits: WideString); safecall;
    function GetFSXVariable(const pName: WideString): OleVariant; safecall;
    procedure SetFSXVariable(const pName: WideString; const pUnits: WideString; pValue: Double); safecall;
    function GetFSUIPCFloat(pOffset: SYSINT; pSize: Smallint): OleVariant; safecall;
    function GetFSUIPCInt(pOffset: SYSINT; pSize: Smallint): OleVariant; safecall;
    procedure SetFSUIPCInt(pOffset: SYSINT; pSize: Smallint; pValue: SYSINT); safecall;
    procedure SetFSUIPCFloat(pOffset: SYSINT; pSize: Smallint; pValue: Double); safecall;
    function GetFSUIPCRaw(pOffset: SYSINT; pSize: Smallint): OleVariant; safecall;
    procedure PlaySound(const aFileName: WideString); safecall;
    procedure Debug(const pMessage: WideString); safecall;
    function GetFSUIPCString(pOffset: SYSINT; pSize: Smallint): OleVariant; safecall;
    procedure SetBuffer(const pValue: WideString); safecall;
    procedure AddToBuffer(const pValue: WideString); safecall;
    procedure ClearBuffer; safecall;
    function GetBuffer: OleVariant; safecall;
    function IsButtonPressed(const pDevice: WideString; pNumber: Smallint): OleVariant; safecall;
    function StrRPad(const pString: WideString; const pChar: WideString; pLength: Smallint): OleVariant; safecall;
    function GetXplVariable(const aName: WideString): OleVariant; safecall;
    procedure SetXplVariable(const pName: WideString; pValue: OleVariant); safecall;
    function GetXplArrayItem(const pName: WideString; pIndex: SYSINT): OleVariant; safecall;
    procedure SetXplArrayItem(const pName: WideString; pIndex: SYSINT; pValue: OleVariant); safecall;
    function GetAxis(const pDevice: WideString; const pAxis: WideString): OleVariant; safecall;
    procedure RegisterAxisEvent(const pDevice: WideString; const pAxis: WideString; 
                                const pProcName: WideString; pDelta: SYSINT); safecall;
    procedure UnRegisterAxisEvent(const pDevice: WideString; const pAxis: WideString); safecall;
    function Axis2Int(pValue: SYSINT; pLowValue: SYSINT; pHighValue: SYSINT): OleVariant; safecall;
    function Axis2Float(pValue: SYSINT; pLowValue: Double; pHighValue: Double): OleVariant; safecall;
    function AxisRemap(pValue: SYSINT; pMin: Double; pMax: Double; pLowValue: Double; 
                       pHighValue: Double; pDefault: Double): OleVariant; safecall;
    function GetKeyState(pKey: SYSINT): OleVariant; safecall;
    procedure SendKeysSlow(const pSequence: WideString; pDealy: SYSINT); safecall;
    procedure XPLCommand(const pCmdName: WideString); safecall;
    procedure ToggleMovingMap; safecall;
    procedure SetMovingMapVisible(pVisible: SYSINT); safecall;
    procedure SetFSUIPCString(pOffset: SYSINT; pSize: Smallint; const pValue: WideString); safecall;
    procedure XPLCommandBegin(const pName: WideString); safecall;
    procedure XPLCommandEnd(const pName: WideString); safecall;
    procedure FSXEventByTitle(const pTitle: WideString; pParam: Double); safecall;
    function GetTriggerDevice: OleVariant; safecall;
    function GetTriggerItem: OleVariant; safecall;
    function GetTriggerDirection: OleVariant; safecall;
    procedure XPLToggleNextDataref(const pDataRef: WideString; const pValues: WideString); safecall;
    procedure XPLTogglePreviousDataref(const pDataRef: WideString; const pValues: WideString); safecall;
    procedure XPLSwitchNextDataref(const pDataRef: WideString; const pValues: WideString); safecall;
    procedure XPLSwitchPreviousDataref(const pDataRef: WideString; const pValues: WideString); safecall;
    procedure XPLMessage(const Message: WideString); safecall;
    procedure XPLMessageAt(const Message: WideString; Position: Double); safecall;
    procedure XPLMessageAtFor(const Message: WideString; YPos: Double; Timeout: SYSINT); safecall;
  end;

// *********************************************************************//
// DispIntf:  IHIDMacrosIntfDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D8B14946-3585-4AEE-94A9-D1B5854201F8}
// *********************************************************************//
  IHIDMacrosIntfDisp = dispinterface
    ['{D8B14946-3585-4AEE-94A9-D1B5854201F8}']
    procedure Sleep(pMsec: SYSINT); dispid 201;
    procedure FSXtext(const lValue: WideString); dispid 202;
    procedure FSXEvent(const pName: WideString; pParam: Double); dispid 203;
    procedure MouseClick(pLeft: SYSINT; pTop: SYSINT; pAction: Smallint); dispid 204;
    procedure SendKeys(const pSeqence: WideString); dispid 205;
    procedure RegisterFSXVariable(const pName: WideString; const pUnits: WideString); dispid 206;
    function GetFSXVariable(const pName: WideString): OleVariant; dispid 207;
    procedure SetFSXVariable(const pName: WideString; const pUnits: WideString; pValue: Double); dispid 208;
    function GetFSUIPCFloat(pOffset: SYSINT; pSize: Smallint): OleVariant; dispid 209;
    function GetFSUIPCInt(pOffset: SYSINT; pSize: Smallint): OleVariant; dispid 210;
    procedure SetFSUIPCInt(pOffset: SYSINT; pSize: Smallint; pValue: SYSINT); dispid 211;
    procedure SetFSUIPCFloat(pOffset: SYSINT; pSize: Smallint; pValue: Double); dispid 212;
    function GetFSUIPCRaw(pOffset: SYSINT; pSize: Smallint): OleVariant; dispid 213;
    procedure PlaySound(const aFileName: WideString); dispid 214;
    procedure Debug(const pMessage: WideString); dispid 215;
    function GetFSUIPCString(pOffset: SYSINT; pSize: Smallint): OleVariant; dispid 218;
    procedure SetBuffer(const pValue: WideString); dispid 219;
    procedure AddToBuffer(const pValue: WideString); dispid 220;
    procedure ClearBuffer; dispid 221;
    function GetBuffer: OleVariant; dispid 222;
    function IsButtonPressed(const pDevice: WideString; pNumber: Smallint): OleVariant; dispid 223;
    function StrRPad(const pString: WideString; const pChar: WideString; pLength: Smallint): OleVariant; dispid 224;
    function GetXplVariable(const aName: WideString): OleVariant; dispid 225;
    procedure SetXplVariable(const pName: WideString; pValue: OleVariant); dispid 226;
    function GetXplArrayItem(const pName: WideString; pIndex: SYSINT): OleVariant; dispid 227;
    procedure SetXplArrayItem(const pName: WideString; pIndex: SYSINT; pValue: OleVariant); dispid 228;
    function GetAxis(const pDevice: WideString; const pAxis: WideString): OleVariant; dispid 216;
    procedure RegisterAxisEvent(const pDevice: WideString; const pAxis: WideString; 
                                const pProcName: WideString; pDelta: SYSINT); dispid 217;
    procedure UnRegisterAxisEvent(const pDevice: WideString; const pAxis: WideString); dispid 229;
    function Axis2Int(pValue: SYSINT; pLowValue: SYSINT; pHighValue: SYSINT): OleVariant; dispid 230;
    function Axis2Float(pValue: SYSINT; pLowValue: Double; pHighValue: Double): OleVariant; dispid 231;
    function AxisRemap(pValue: SYSINT; pMin: Double; pMax: Double; pLowValue: Double; 
                       pHighValue: Double; pDefault: Double): OleVariant; dispid 232;
    function GetKeyState(pKey: SYSINT): OleVariant; dispid 233;
    procedure SendKeysSlow(const pSequence: WideString; pDealy: SYSINT); dispid 234;
    procedure XPLCommand(const pCmdName: WideString); dispid 235;
    procedure ToggleMovingMap; dispid 236;
    procedure SetMovingMapVisible(pVisible: SYSINT); dispid 237;
    procedure SetFSUIPCString(pOffset: SYSINT; pSize: Smallint; const pValue: WideString); dispid 238;
    procedure XPLCommandBegin(const pName: WideString); dispid 239;
    procedure XPLCommandEnd(const pName: WideString); dispid 240;
    procedure FSXEventByTitle(const pTitle: WideString; pParam: Double); dispid 241;
    function GetTriggerDevice: OleVariant; dispid 242;
    function GetTriggerItem: OleVariant; dispid 243;
    function GetTriggerDirection: OleVariant; dispid 244;
    procedure XPLToggleNextDataref(const pDataRef: WideString; const pValues: WideString); dispid 245;
    procedure XPLTogglePreviousDataref(const pDataRef: WideString; const pValues: WideString); dispid 246;
    procedure XPLSwitchNextDataref(const pDataRef: WideString; const pValues: WideString); dispid 247;
    procedure XPLSwitchPreviousDataref(const pDataRef: WideString; const pValues: WideString); dispid 248;
    procedure XPLMessage(const Message: WideString); dispid 249;
    procedure XPLMessageAt(const Message: WideString; Position: Double); dispid 250;
    procedure XPLMessageAtFor(const Message: WideString; YPos: Double; Timeout: SYSINT); dispid 251;
  end;

// *********************************************************************//
// The Class CoTHIDMacrosIntf provides a Create and CreateRemote method to          
// create instances of the default interface IHIDMacrosIntf exposed by              
// the CoClass THIDMacrosIntf. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoTHIDMacrosIntf = class
    class function Create: IHIDMacrosIntf;
    class function CreateRemote(const MachineName: string): IHIDMacrosIntf;
  end;

implementation

uses ComObj;

class function CoTHIDMacrosIntf.Create: IHIDMacrosIntf;
begin
  Result := CreateComObject(CLASS_THIDMacrosIntf) as IHIDMacrosIntf;
end;

class function CoTHIDMacrosIntf.CreateRemote(const MachineName: string): IHIDMacrosIntf;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_THIDMacrosIntf) as IHIDMacrosIntf;
end;

end.
