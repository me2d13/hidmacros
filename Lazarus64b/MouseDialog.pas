unit MouseDialog;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, KbdMacro;

type
  TMouseForm = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Label1: TLabel;
    MouseComboBox: TComboBox;
    GroupBox1: TGroupBox;
    RBWheelUp: TRadioButton;
    RBWheelDown: TRadioButton;
    RBLeftUp: TRadioButton;
    RBMiddleUp: TRadioButton;
    RBRightUp: TRadioButton;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    RBLeftDown: TRadioButton;
    RBMiddleDown: TRadioButton;
    RBRightDown: TRadioButton;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetMouseAttribs(pMacro: TKeyboardMacro);
    procedure InitForm(pMacro: TKeyboardMacro);
  end;

implementation

{$R *.dfm}

uses Dialogs;

procedure TMouseForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if (MouseComboBox.ItemIndex = -1) and (ModalResult = mrOK) then
  begin
    MessageDlg('Select mouse or cancel dialog.', mtError, [mbOK], 0);
    CanClose := False;
  end
end;

procedure TMouseForm.InitForm(pMacro: TKeyboardMacro);
begin
  if pMacro = nil then
    exit;
  RBWheelUp.Checked := (pMacro.MouseEvent = Wheel) and (pMacro.Direction = mdUp);
  RBWheelDown.Checked := (pMacro.MouseEvent = Wheel) and (pMacro.Direction = mdDown);
  RBLeftUp.Checked := (pMacro.MouseEvent = KbdMacro.Left) and (pMacro.Direction = mdUp);
  RBLeftDown.Checked := (pMacro.MouseEvent = KbdMacro.Left) and (pMacro.Direction = mdDown);
  RBMiddleUp.Checked := (pMacro.MouseEvent = Middle) and (pMacro.Direction = mdUp);
  RBMiddleDown.Checked := (pMacro.MouseEvent = Middle) and (pMacro.Direction = mdDown);
  RBRightUp.Checked := (pMacro.MouseEvent = Right) and (pMacro.Direction = mdUp);
  RBRightDown.Checked := (pMacro.MouseEvent = Right) and (pMacro.Direction = mdDown);
end;

procedure TMouseForm.SetMouseAttribs(pMacro: TKeyboardMacro);
begin
  if pMacro = nil then
    exit;
  if (RBWheelUp.Checked) then
  begin
    pMacro.MouseEvent := Wheel;
    pMacro.Direction := mdUp;
  end;
  if (RBWheelDown.Checked) then
  begin
    pMacro.MouseEvent := Wheel;
    pMacro.Direction := mdDown;
  end;
  if (RBLeftUp.Checked) then
  begin
    pMacro.MouseEvent := KbdMacro.Left;
    pMacro.Direction := mdUp;
  end;
  if (RBLeftDown.Checked) then
  begin
    pMacro.MouseEvent := KbdMacro.Left;
    pMacro.Direction := mdDown;
  end;
  if (RBMiddleUp.Checked) then
  begin
    pMacro.MouseEvent := Middle;
    pMacro.Direction := mdUp;
  end;
  if (RBMiddleDown.Checked) then
  begin
    pMacro.MouseEvent := Middle;
    pMacro.Direction := mdDown;
  end;
  if (RBRightUp.Checked) then
  begin
    pMacro.MouseEvent := Right;
    pMacro.Direction := mdUp;
  end;
  if (RBRightDown.Checked) then
  begin
    pMacro.MouseEvent := Right;
    pMacro.Direction := mdDown;
  end;
end;


end.
