{ 25/05/2007 11:17:03 (GMT+1:00) > [Akadamia] checked in   }
{ 25/05/2007 10:37:27 (GMT+1:00) > [Akadamia] checked out /}
{ 14/02/2007 08:32:29 (GMT+0:00) > [Akadamia] checked in   }
{ 14/02/2007 08:32:17 (GMT+0:00) > [Akadamia] checked out /}
{ 12/02/2007 10:16:44 (GMT+0:00) > [Akadamia] checked in   }
{ 08/02/2007 14:08:23 (GMT+0:00) > [Akadamia] checked in   }
{-----------------------------------------------------------------------------
 Unit Name: OCU
 Author:    ken.adam
 Date:      15-Jan-2007
 Purpose:   Translation of Open and Close example to Delphi
            Opens and immediately Closes SimConnect
 History:
-----------------------------------------------------------------------------}
unit OCU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ImgList, ToolWin, ActnMan, ActnCtrls, XPStyleActnCtrls,
  ActnList, ComCtrls, SimConnect, StdActns;
const
  // Define a user message for message driven version of the example
  WM_USER_SIMCONNECT = WM_USER + 2;
type

  // The form
  TOpenAndCloseForm = class(TForm)
    StatusBar: TStatusBar;
    ActionManager: TActionManager;
    ActionToolBar: TActionToolBar;
    Images: TImageList;
    Connect: TAction;
    FileExit: TFileExit;
    Memo: TMemo;
    procedure ConnectExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    RxCount: integer;                   // Count of Rx messages
    Quit: boolean;                      // True when signalled to quit
    hSimConnect: THandle;               // Handle for the SimConection
  public
    { Public declarations }
  end;

var
  OpenAndCloseForm       : TOpenAndCloseForm;

implementation

resourcestring
  StrRx6d           = 'Rx: %6d';

{$R *.dfm}

{-----------------------------------------------------------------------------
  Procedure: FormCreate
  We are using  run-time dynamic loading of SimConnect.dll, so that the program
  will execute and fail gracefully if the DLL does not exist.
  Author:    ken.adam
  Date:      11-Jan-2007
  Arguments:
    Sender: TObject
  Result:    None
-----------------------------------------------------------------------------}

procedure TOpenAndCloseForm.FormCreate(Sender: TObject);
begin
  if InitSimConnect then
    StatusBar.Panels[2].Text := 'SimConnect.dll Loaded'
  else
    StatusBar.Panels[2].Text := 'SimConnect.dll NOT FOUND';
  Quit := False;
  hSimConnect := 0;
  StatusBar.Panels[0].Text := 'Not Connected';
  RxCount := 0;
  StatusBar.Panels[1].Text := Format(StrRx6d, [RxCount]);
end;

{-----------------------------------------------------------------------------
  Procedure: ConnectExecute
  Opens the connection for Polled access. If successful closes the connection.
  Author:    ken.adam
  Date:      11-Jan-2007
  Arguments:
    Sender: TObject
-----------------------------------------------------------------------------}

procedure TOpenAndCloseForm.ConnectExecute(Sender: TObject);
var
  hr                : HRESULT;
begin
  if (SUCCEEDED(SimConnect_Open(hSimConnect, 'Open And Close', 0, 0, 0, 0))) then
  begin
    StatusBar.Panels[0].Text := 'Connected';
    Memo.Lines.Add('Connected to Flight Simulator');
    hr := SimConnect_Close(hSimConnect);
  end
  else
    Memo.Lines.Add('Failed to connect to Flight Simulator');
  StatusBar.Panels[0].Text := 'Not Connected';
end;

end.

