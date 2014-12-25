unit uMoveMacrosForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, KbdMacro;

type
  TMacrosMoveForm = class(TForm)
    Label1: TLabel;
    FromLabel: TLabel;
    FromDevice: TLabel;
    ToLabel: TLabel;
    ToDevice: TComboBox;
    ResultLabel: TLabel;
    ResultText: TLabel;
    MoveButton: TButton;
    CancelButton: TButton;
    OKButton: TButton;
    procedure MoveButtonClick(Sender: TObject);
  private
    { Private declarations }
    fSourceDev: THIDKeyboard;
    procedure SetSourceDev(const Value: THIDKeyboard);
  public
    { Public declarations }
    property SourceDev: THIDKeyboard read fSourceDev write SetSourceDev;
  end;


implementation

uses uGlobals;

{$R *.dfm}

{ TMacrosMoveForm }

procedure TMacrosMoveForm.MoveButtonClick(Sender: TObject);
var
  lMoved, lNotMoved: Integer;
begin
  if ToDevice.ItemIndex >= 0 then
  begin
    Glb.MacrosList.MoveFromTo(fSourceDev,
        THIDKeyboard(ToDevice.Items.Objects[ToDevice.ItemIndex]),
        lMoved, lNotMoved);
    if lNotMoved = 0 then
      if lMoved = 0 then
        ResultText.Caption := 'No macros found.'
      else
        ResultText.Caption := Format('All %d macros were moved.', [lMoved])
    else
      ResultText.Caption := Format('Out of %d existing macros %d were moved. ' +
        'Remaining %d macros can not be moved because same trigger is already used on target device.',
        [lMoved + lNotMoved, lMoved, lNotMoved]);
    MoveButton.Visible := False;
    CancelButton.Visible := False;
    OKButton.Visible := True;
  end;
end;

procedure TMacrosMoveForm.SetSourceDev(const Value: THIDKeyboard);
var
  I: Integer;
begin
  fSourceDev := Value;
  ToDevice.Items.Clear;
  for I := 0 to Glb.HIDControl.Count - 1 do
    if (Value.ClassType = Glb.HIDControl.Items[i].ClassType) and
       (Value <> Glb.HIDControl.Items[i]) then
      ToDevice.Items.AddObject(Glb.HIDControl.Items[i].Name, Glb.HIDControl.Items[i]);
  FromDevice.Caption := Value.Name;
  ToDevice.ItemIndex := 0;
end;

end.
