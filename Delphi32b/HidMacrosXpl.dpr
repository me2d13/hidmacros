library HidMacrosXpl;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

uses
  SysUtils,
  Classes,
  Windows,
  Graphics,
  XPLMDefs,
  XPLMDisplay,
  XPLMProcessing,
  XPLMUtilities,
  XPLMDataAccess,
  MemMap in 'MemMap.pas',
  uXplPluginEngine in 'uXplPluginEngine.pas',
  uXplCommon in 'uXplCommon.pas';

// The Delphi Compiler must create a file with a .XPL extention. The default
// extention for a Delphi library is .DLL - thus we use the $E compiler
// directive to override the default and change it to .XPL
{$E xpl}

// Default directive to include any RESOURCE files our project may have
{$R *.res}

var
 //gLatitutde : XPLMDataRef;
 gEng: TXplEngine;

function MyFlightLoopCallback(inElapsedSinceLastCall    : Single;
                              inElapsedTimeSinceLastFlightLoop : Single;
                              inCounter : LongInt;
                              inRefcon   : pointer ) : Single; cdecl;
begin
  //* If any data refs are missing, do not proceed.
  if (gEng = nil) or (gEng.pBuffer = nil) then
  begin
    result := 1;
    exit;
  end;

  gEng.XplTick;

  result := -1;
end;

{/*
 * XPluginStart
 *
 * Our start routine registers our callback and does any other initialization we
 * must do.
 *
 */}
function XPluginStart( outName : PChar;
                       outSig  : PChar;
                       outDesc : PChar ) : integer; cdecl;
begin

  //* First record our plugin information.
  StrPCopy(outName, 'HIDmacrosController');
  StrPCopy( outSig,  'PetrMedek.HidMacros.Controller' );
  StrPCopy(outDesc, 'A plugin to access sim variables in HIDmacros.');

  gEng := TXplEngine.Create;

  //outputPath := @Buffer;
	//XPLMGetSystemPath(outputPath);

	{/* Find the data refs we want to record. */}
	//gLatitutde := XPLMFindDataRef('sim/flightmodel/position/latitude');

  if gEng.pBuffer <> nil then
  	XPLMRegisterFlightLoopCallback( MyFlightLoopCallback,	// Callback
             			                  -1.0,	                // Interval - every second frame
                                    nil);	                // refcon not used.

  result := 1
end;

{/*
 * XPluginStop
 *
 * Our cleanup routine unregistering our callback.
 *
 */}
procedure XPluginStop; cdecl;
begin
	{/* Unregister the callback */}
	XPLMUnregisterFlightLoopCallback(MyFlightLoopCallback, nil);
  gEng.Free;
  gEng := nil;
end;

{/*
 * XPluginDisable
 *
 * We do not need to do anything when we are disabled, but we must provide the handler.
 *
 */}
procedure XPluginDisable; cdecl;
begin
  // nothing to do
end;

{/*
 * XPluginEnable.
 *
 * We don't do any enable-specific initialization, but we must return 1 to indicate
 * that we may be enabled at this time.
 *
 */}
function XPluginEnable : integer; cdecl;
begin
  result := 1;
end;

{/*
 * XPluginReceiveMessage
 *
 * We don't have to do anything in our receive message handler, but we must provide one.
 *
 */}
procedure XPluginReceiveMessage(   inFromWho : XPLMPluginID;
                                   inMessage : longint;
                                   inParam   : pointer); cdecl;
begin
  // nothing to do
end;

// In order to make these calls in our DLL visible to the outside world, we
// need to export them.
exports
  XPluginStop,
  XPluginStart,
  XPluginReceiveMessage,
  XPluginEnable,
  XPluginDisable;

begin
end.

//eof

