unit Localizer;

interface

uses Forms, Classes;

type
  TLocalizer = class
    private
      fLanguage: String;
    procedure SetLanguage(const Value: String);
    public
      procedure Localize(pForm: TForm; pList: TStrings);
      procedure LoadStrings(pList: TStrings);
      property language: String read fLanguage write SetLanguage;
  end;


implementation

uses
  Inifiles, SysUtils, Controls, StdCtrls, ActnList, ComCtrls,
  ExtCtrls, cUnicodeCodecs, Dialogs;
{ TLocalizer }

procedure TLocalizer.LoadStrings(pList: TStrings);
var
  I: Integer;
  lFn, lCaption: String;
  lLF: TiniFile;
  lComp: TComponent;
begin
  lFn := ExtractFilePath(Application.ExeName)+'\lang\'+fLanguage+'.lng';
  if (not FileExists(lFn)) then
    Exit;
  lLF := TIniFile.Create(lFn);
  try
    for I := 0 to pList.Count - 1 do
    begin
      pList.Values[pList.Names[i]] := UTF8StringToWideString(lLF.ReadString('HidMacros', pList.Names[i], pList.Values[pList.Names[i]]));
    end;
  finally
    lLF.Free;
  end;
end;

procedure TLocalizer.Localize(pForm: TForm; pList: TStrings);
var
  I: Integer;
  lFn: String;
  lRawCaption: String;
  lCaption: TCaption;
  lLF: TiniFile;
  lComp: TObject;
begin
  lFn := ExtractFilePath(Application.ExeName)+'\lang\'+fLanguage+'.lng';
  if (not FileExists(lFn)) then
    Exit;
  lLF := TIniFile.Create(lFn);
  try
    for I := 0 to pList.Count - 1 do
    begin
      lComp := pForm.FindComponent(pList.Names[i]);
      if lComp <> nil then
      begin
        lRawCaption := lLF.ReadString('HidMacros', pList.Values[pList.Names[i]], '');
        if lRawCaption <> '' then
        begin
          lCaption := UTF8StringToWideString(lRawCaption);
          if lComp is TLabel then
            (lComp as TLabel).Caption:= lCaption
          else if lComp is TGroupBox then
            (lComp as TGroupBox).Caption:= lCaption
          else if lComp is TButton then
            (lComp as TButton).Caption:= lCaption
          else if lComp is TCheckBox then
            (lComp as TCheckBox).Caption:= lCaption
          else if lComp is TCustomAction then
            (lComp as TCustomAction).Caption:= lCaption
          else if lComp is TRadioButton then
            (lComp as TRadioButton).Caption:= lCaption
          else if lComp is TTabSheet then
            (lComp as TTabSheet).Caption:= lCaption
          else if lComp is TListColumn then
            (lComp as TListColumn).Caption:= lCaption
          else if lComp is TPanel then
            (lComp as TPanel).Caption:= lCaption;
        end;
      end
      else if pForm.Name = pList.Names[i] then
      begin
        lRawCaption := lLF.ReadString('HidMacros', pList.Values[pList.Names[i]], '');
        if lRawCaption <> '' then
        begin
          lCaption := UTF8StringToWideString(lRawCaption);
          pForm.Caption := lCaption;
        end;
      end;
    end;
  finally
    lLF.Free;
  end;
end;

procedure TLocalizer.SetLanguage(const Value: String);
begin
  fLanguage := Value;
end;

end.
