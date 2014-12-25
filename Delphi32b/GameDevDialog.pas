unit GameDevDialog;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, KbdMacro;

type
  TGameForm = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Label1: TLabel;
    GameComboBox: TComboBox;
    GroupBox1: TGroupBox;
    ScrollBox1: TScrollBox;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    fLastIndex: Integer;
    fLastDirection: TMacroDirection;
  public
    { Public declarations }
    TXTdown: String;
    TXTup: String;
    procedure SetMacroAttribs(pMacro: TKeyboardMacro);
    procedure InitByMacro(pMacro: TKeyboardMacro);
  end;

implementation

{$R *.dfm}

uses Dialogs, uGameDevices;

procedure TGameForm.FormClose(Sender: TObject; var Action: TCloseAction);
var I:Integer;
  tmpC: TControl;
begin
  // free all childs of ScrollBox1
  for I := ScrollBox1.ControlCount - 1 downto 0 do
  begin
    tmpC := ScrollBox1.Controls[I];
    if Copy(tmpC.Name, 1, 6) = 'Button' then
      if (tmpC is TRadioButton) then
        if (tmpC as TRadioButton).Checked then
        begin
          fLastIndex := tmpC.Tag;
          if tmpC.Name = 'Button'+IntToStr(fLastIndex)+'Up' then
            fLastDirection := mdUp
          else
            fLastDirection := mdDown;
        end;
    tmpC.Free;
  end;
end;

procedure TGameForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if (GameComboBox.ItemIndex = -1) and (ModalResult = mrOK) then
  begin
    MessageDlg('Select game device or cancel dialog.', mtError, [mbOK], 0);
    CanClose := False;
  end
end;

procedure TGameForm.FormCreate(Sender: TObject);
begin
  TXTdown := 'down';
  TXTup := 'up';
end;

procedure TGameForm.InitByMacro(pMacro: TKeyboardMacro);
var
  I: Integer;
  tmpRB: TRadioButton;
  tmpLabel: TLabel;
begin
  if pMacro = nil then
    exit;
  GameComboBox.Items.Clear;
  GameComboBox.Items.Add(pMacro.Keyboard.Name);
  GameComboBox.ItemIndex := 0;
  GameComboBox.Enabled := False;
  for I := 0 to (pMacro.Keyboard as THIDJoystick).ButtonsCount - 1 do
  begin
    tmpLabel := TLabel.Create(self);
    with tmpLabel do
    begin
      Parent := ScrollBox1;
      Name := 'LabelButton'+IntToStr(I);
      Left := 10;
      Top := I*25+5;
      Caption := 'Button ' +IntToStr(I+1);
      Visible := True;
    end;
    tmpRB := TRadioButton.Create(self);
    tmpRB.Parent := ScrollBox1;
    tmpRB.Name := 'Button'+IntToStr(I)+'Down';
    tmpRB.Left := 90;
    tmpRB.Top := I*25+5;
    tmpRB.Tag := I;
    tmpRB.Caption := TXTdown;
    tmpRB.Checked := (pMacro.ButtonIndex = I) and (pMacro.Direction = mdDown);
    tmpRB.Visible := True;
    tmpRB := TRadioButton.Create(self);
    tmpRB.Parent := ScrollBox1;
    tmpRB.Name := 'Button'+IntToStr(I)+'Up';
    tmpRB.Left := 160;
    tmpRB.Top := I*25+5;
    tmpRB.Tag := I;
    tmpRB.Caption := TXTup;
    tmpRB.Checked := (pMacro.ButtonIndex = I) and (pMacro.Direction = mdUp);
    tmpRB.Visible := True;
  end;
end;

procedure TGameForm.SetMacroAttribs(pMacro: TKeyboardMacro);
var
  I: Integer;
begin
  if pMacro = nil then
    exit;
  if fLastIndex < (pMacro.Keyboard as THIDJoystick).ButtonsCount then
  begin
    pMacro.ButtonIndex := fLastIndex;
    pMacro.Direction := fLastDirection;
  end;
end;


end.
