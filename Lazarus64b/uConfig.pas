unit uConfig;

interface

uses XMLIntf, XMLDoc, Classes;

type

  TAppConfig = class (TObject)
  private
    fBackupFileCount: Integer;
    fBackupPath: String;
    fXMLFileName: String;
    fShortFileName: String;
    fAbsoluteBackupPath: String;
    fLoadedDoc: TXMLDocument;
    procedure SaveToXmlDoc(pDoc: TXMLDocument);
    function AreConfigurationsEqual(pN1, pN2: IXMLNode): Boolean;
    function AreNodesEqual(pN1, pN2: IXMLNode): Boolean;
    function AreNodesEqualByChildNode(pN1, pN2: IXMLNode; pChildNodeName: String): Boolean;
    procedure DebugLog(pMessage: String);
  public
    constructor Create(pOwner: TObject);
    destructor Destroy; override;
    procedure SaveToXml(pForceIfNotChanged: Boolean = false);
    procedure LoadFromXml;
    procedure SaveBackups(pDoc: TXMLDocument);
    function CheckBackupPath: boolean;
    property BackupFileCount: Integer read fBackupFileCount write fBackupFileCount;
    property BackupPath: String read fBackupPath write fBackupPath;
    property XMLFileName: String read fXMLFileName;
    property AbsoluteBackupPath: String read fAbsoluteBackupPath;
  end;


implementation

uses SysUtils, uGlobals, Forms, Windows, Dialogs, XmlDom;

{ TAppConfig }

function IsDirectoryWriteable(const AName: string): Boolean;
var
  FileName: String;
  H: THandle;
begin
  FileName := IncludeTrailingPathDelimiter(AName) + 'chk.tmp';
  H := CreateFile(PChar(FileName), GENERIC_READ or GENERIC_WRITE, 0, nil,
    CREATE_NEW, FILE_ATTRIBUTE_TEMPORARY or FILE_FLAG_DELETE_ON_CLOSE, 0);
  Result := H <> INVALID_HANDLE_VALUE;
  if Result then CloseHandle(H);
end;

function TAppConfig.AreConfigurationsEqual(pN1, pN2: IXMLNode): Boolean;
var I: Integer;
  lA1, lA2: IXMLNodeList;
  lN2: IXMLNode;
  lC1, lC2: Integer;
begin
  Result := False;
  if (pN1 = nil) and (pN2 = nil) then
  begin
    Result := True;
    exit;
  end;
  if (pN1 = nil) or (pN2 = nil) then
    exit;
  // children
  lA1 := pN1.ChildNodes;
  lA2 := pN2.ChildNodes;
  lC1 := 0;
  lC2 := 0;
  if (lA1 <> nil) and (lA2 <> nil) then
  begin
    for I := 0 to lA1.Count - 1 do
      if lA1.Nodes[i].NodeName <> '#text' then
      begin
        Inc(lC1);
        lN2 := lA2.FindNode(lA1.Nodes[i].NodeName);
        if lN2 = nil then
          exit;
        if (UpperCase(lA1.Nodes[i].NodeName) = 'MACROS') or
           (UpperCase(lA1.Nodes[i].NodeName) = 'DEVICES')
        then
        begin
          if not AreNodesEqualByChildNode(lA1.Nodes[i], lN2, 'Name') then
            exit;
        end
        else
          if not AreNodesEqual(lA1.Nodes[i], lN2) then
            exit;
      end;
    for I := 0 to lA2.Count - 1 do
      if lA2.Nodes[i].NodeName <> '#text' then
      begin
        Inc(lC2);
      end;
    if (lC1 <> lC2) then
    begin
      DebugLog('Diff: Count children of ' + pN1.NodeName + ' (' +
        IntToStr(lC1) + ' <> ' + IntToStr(lC2) + ')');
    end;
  end;
  Result := True;
end;

function TAppConfig.AreNodesEqual(pN1, pN2: IXMLNode): Boolean;
var I: Integer;
  lA1, lA2: IXMLNodeList;
  lN2: IXMLNode;
  lC1, lC2: Integer;
begin
  Result := False;
  lC1 := 0;
  lC2 := 0;
  if (pN1 = nil) and (pN2 = nil) then
  begin
    Result := True;
    exit;
  end;
  if (pN1 = nil) or (pN2 = nil) then
    exit;
  DebugLog('Comparing ' + pN1.LocalName + ' and ' + pN2.LocalName);
  // compare attribs
  lA1 := pN1.AttributeNodes;
  lA2 := pN2.AttributeNodes;
  if (lA1 <> nil) and (lA2 <> nil) then
  begin
    if lA1.Count <> lA2.Count then
      exit;
    for I := 0 to lA1.Count - 1 do
      if lA1.Nodes[I].Text <> lA2.Nodes[I].Text then
        Exit;
  end else
    if (lA1 <> nil) or (lA2 <> nil) then
      exit;

  // children
  lA1 := pN1.ChildNodes;
  lA2 := pN2.ChildNodes;
  if (lA1 <> nil) and (lA2 <> nil) then
  begin
    for I := 0 to lA1.Count - 1 do
      if lA1.Nodes[i].NodeName <> '#text' then
      begin
        Inc(lC1);
        lN2 := lA2.FindNode(lA1.Nodes[i].NodeName);
        if lN2 = nil then
          exit;
        if (lA1.Nodes[i].IsTextElement) and (lN2.IsTextElement) then
        begin
          if lA1.Nodes[I].Text <> lN2.Text then
          begin
            DebugLog('Diff: ' + lA1.Nodes[I].Text + ', ' + lN2.Text);
            Exit;
          end;
        end else if (lA1.Nodes[i].HasChildNodes) and (lN2.HasChildNodes) then
        begin
          Result := AreNodesEqual(lA1.Nodes[i], lN2);
          if (not Result) then
            exit;
          Result := False;
        end else
          exit;
      end;
    for I := 0 to lA2.Count - 1 do
      if lA2.Nodes[i].NodeName <> '#text' then
      begin
        Inc(lC2);
      end;
    if (lC1 <> lC2) then
    begin
      DebugLog('Diff: Count children of ' + pN1.NodeName + ' (' +
        IntToStr(lC1) + ' <> ' + IntToStr(lC2) + ')');
    end;
  end else
    if (lA1 <> nil) or (lA2 <> nil) then
      exit;
  DebugLog('Comp: ' + pN1.NodeName + ' ' + pN2.NodeName + ' OK');
  Result := True;
end;

function TAppConfig.AreNodesEqualByChildNode(pN1, pN2: IXMLNode;
  pChildNodeName: String): Boolean;
var I, J: Integer;
  lA1, lA2, lA1s, lA2s: IXMLNodeList;
  lN1, lN2: IXMLNode;
  lC1, lC2: Integer;
  lNodeOK: Boolean;
begin
  Result := False;
  lC1 := 0;
  lC2 := 0;
  if (pN1 = nil) and (pN2 = nil) then
  begin
    Result := True;
    exit;
  end;
  if (pN1 = nil) or (pN2 = nil) then
    exit;
  DebugLog('Comparing ' + pN1.LocalName + ' and ' + pN2.LocalName);
  // no need to compare attribs - not used in general parents
  // children
  lA1 := pN1.ChildNodes;
  lA2 := pN2.ChildNodes;
  if (lA1 <> nil) and (lA2 <> nil) then
  begin
    for I := 0 to lA1.Count - 1 do
      if lA1.Nodes[i].NodeName <> '#text' then
      begin
        Inc(lC1);
        // locate child content
        lA1s := lA1.Nodes[i].ChildNodes;
        if lA1s = nil then
          exit;
        lN1 := lA1s.FindNode(pChildNodeName);
        if lN1 = nil then
          exit;
        lNodeOK := False;
        // look for node with same name and same children content
        for J := 0 to lA2.Count - 1 do
          if lA2.Nodes[j].NodeName <> '#text' then
          begin
            if lA1.Nodes[i].NodeName = lA2.Nodes[j].NodeName then
            begin
              lA2s := lA2.Nodes[j].ChildNodes;
              if lA2s = nil then
                exit;
              lN2 := lA2s.FindNode(pChildNodeName);
              if lN2 = nil then
                exit;
              // now we have child node - supposed to be text node
              if (lN1.IsTextElement) and (lN2.IsTextElement) and
                  (lN1.Text = lN2.Text) then
              begin
                // so they contain same child node (name), compare nodes
                lNodeOK := AreNodesEqual(lA1.Nodes[i], lA2.Nodes[j]);
                Break;
              end;
            end;
          end;
        if (not lNodeOK) then
        begin
          DebugLog('Diff on child compare: ' + pN1.NodeName + ' - ' +
              pChildNodeName + ' - ' + lN1.Text);
          exit;
        end;
      end;
    for I := 0 to lA2.Count - 1 do
      if lA2.Nodes[i].NodeName <> '#text' then
      begin
        Inc(lC2);
      end;
    if (lC1 <> lC2) then
    begin
      DebugLog('Diff: Count children of ' + pN1.NodeName + ' (' +
        IntToStr(lC1) + ' <> ' + IntToStr(lC2) + ')');
    end;
  end;
  DebugLog('Comp: ' + pN1.NodeName + ' ' + pN2.NodeName + ' OK');
  Result := True;
end;

function TAppConfig.CheckBackupPath: boolean;
begin
  fBackupPath := Trim(fBackupPath);
  if ((Length(fBackupPath) > 1) and (fBackupPath[1] = '\'))
    or ((Length(fBackupPath) > 1) and (Copy(fBackupPath, 2, 2) = ':\')) then
  begin
    fAbsoluteBackupPath := fBackupPath;
  end
  else
  begin
    fAbsoluteBackupPath := ExtractFilePath(Application.ExeName)+'\'+fBackupPath;
  end;
  // check if exists and is writable
  Result := IsDirectoryWriteable(fAbsoluteBackupPath);
end;

constructor TAppConfig.Create(pOwner: TObject);
var
  lDebugStr: String;
begin
  fBackupFileCount := 0;
  fLoadedDoc := TXMLDocument.Create((pOwner as THDMGlobals).MainForm);
  fLoadedDoc.Options := [doNodeAutoIndent];
  fLoadedDoc.Active := True;
  fBackupPath := 'backup';
  fShortFileName := 'hidmacros';
  if (ParamCount > 0) then
  begin
    lDebugStr := UpperCase(ParamStr(1));
    if (Copy(lDebugStr, 1, 5) = 'DEBUG') then
    begin
      (pOwner as THDMGlobals).DebugGroups := Copy(lDebugStr, 7, 200);
    end
    else
    begin
      fShortFileName := Trim(ParamStr(1));
    end;
  end;
  fXmlFileName := fShortFileName + '.xml';
end;

procedure TAppConfig.DebugLog(pMessage: String);
begin
  Glb.DebugLog(pMessage, 'XML');
end;

destructor TAppConfig.Destroy;
begin
  fLoadedDoc.Free;
  inherited;
end;

function ExtractNumberFromFileName(pFileName: String): Integer;
var
  lPos: Integer;
begin
  lPos := LastDelimiter('_', pFileName);
  Result := StrToInt(Copy(pFileName, lPos+1, Length(pFileName) - 4 - lPos));
end;

function SortFilesByName(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := - ExtractNumberFromFileName(List[Index2]) + ExtractNumberFromFileName(List[Index1]);
end;

procedure TAppConfig.SaveBackups(pDoc: TXMLDocument);
var
  lFiles: TStringList;
  lSR: TSearchRec;
  lNextNo: Integer;
  I: Integer;
begin
  if (fBackupFileCount = 0) then
    Exit;
  lNextNo := 1;
  if (not CheckBackupPath) then
  begin
    DebugLog('Directory ' + fAbsoluteBackupPath + ' not writable, backup not stored');
    exit;
  end;
  // get existing files
  lFiles := TStringList.Create;
  try
    if (FindFirst(fAbsoluteBackupPath + '\'+ fShortFileName + '_*.xml',
        faAnyFile and not faDirectory and not faHidden, lSR) = 0) then
      repeat
        lFiles.Add(lSR.Name)
       until FindNext(lSR) <> 0;
    SysUtils.FindClose(lSR);
    // sort them
    if lFiles.Count > 1 then
      lFiles.CustomSort(SortFilesByName);
    if lFiles.Count > 0 then
      lNextNo := ExtractNumberFromFileName(lFiles[lFiles.Count-1]) + 1;
    // delete old files
    for I := 0 to lFiles.Count - fBackupFileCount do
      SysUtils.DeleteFile(fAbsoluteBackupPath + '\'+lFiles[I]);
  finally
    lFiles.Free;
  end;
  // create new
  pDoc.SaveToFile(fAbsoluteBackupPath + '\'+ fShortFileName + '_' +
      IntToStr(lNextNo)+ '.xml');
end;

procedure TAppConfig.SaveToXml(pForceIfNotChanged: Boolean = false);
var
  fn : String;
  Xdoc, lTmp4Swap : TXMLDocument;
  lEquals: boolean;
begin
  fn := ExtractFilePath(Application.ExeName)+'\'+fXmlFileName;
  Xdoc := TXMLDocument.Create(Glb.MainForm);
  try
    Xdoc.Options := [doNodeAutoIndent];
    Xdoc.Active := True;
    SaveToXmlDoc(Xdoc);
    try
    lEquals := (fLoadedDoc = nil) or // when there was no xml - after install
        AreConfigurationsEqual(Xdoc.DocumentElement, fLoadedDoc.DocumentElement);
    except
      on EXMLDocError do
      begin
        lEquals := True; // no backup if previous file was kind of wrong
        pForceIfNotChanged := True; // but make sure current config is saved
      end;
    end;
    if (not lEquals) or pForceIfNotChanged then
    begin
      if (FileExists(fn)) then
      begin
        DebugLog('Deleting old XML file ' + fn);
        SysUtils.DeleteFile(fn);
      end;
      DebugLog('Saving to XML file ' + fn);
      Xdoc.SaveToFile(fn);
    end;
    if (not lEquals) then
    begin
      SaveBackups(Xdoc);
    end;
    // swap current xdoc and last loaded doc
    // as xdoc variable will be freed
    lTmp4Swap := fLoadedDoc;
    fLoadedDoc := Xdoc;
    Xdoc := lTmp4Swap;
  finally
    Xdoc.Free;
  end;
end;


procedure TAppConfig.SaveToXmlDoc(pDoc: TXMLDocument);
var
  fn : String;
  RootNode, generalNode, aNode: IXMLNode;
begin
  RootNode := pDoc.AddChild('Config');
  generalNode := RootNode.AddChild('General');
  Glb.MainForm.SaveToXml(RootNode, generalNode);
  // save my values
  aNode := generalNode.AddChild('BackupsCount');
  aNode.Text := IntToStr(fBackupFileCount);
  aNode := generalNode.AddChild('BackupPath');
  aNode.Text := fBackupPath;
  // save devices
  aNode := RootNode.AddChild('Devices');
  Glb.HIDControl.SaveToXML(aNode);
  // save Map
  aNode := RootNode.AddChild('Map');
  Glb.MapForm.SaveToXML(aNode);
end;

procedure TAppConfig.LoadFromXml;
var
  RootNode, parentNode, aNode, devicesNode: IXMLNode;
  I, J, SysIDIndex, NameIndex:Integer;
  fn, tmpSysID, tmpName: String;
  lProcBegin, lProcEnd: String;
begin
  // default values
  Glb.ScriptEngine.AllowGUI := False;
  Glb.ScriptEngine.ScriptTimeout := 10;
  fn := ExtractFilePath(Application.ExeName)+'\'+fXmlFileName;
  DebugLog('XML file name is ' + fn);
  if (not FileExists(fn)) then
  begin
    DebugLog('File ' + fn + 'does not exist.');
    Exit;
  end;
  try
    fLoadedDoc.LoadFromFile(fn);
    fLoadedDoc.Active := True;
    RootNode := fLoadedDoc.DocumentElement.ChildNodes.First;
    if (RootNode = nil) then
      DebugLog('Can not parse XML ini file, no loading.');
    parentNode := RootNode;
    while parentNode <> nil do
    begin
      if UpperCase(parentNode.NodeName) = 'GENERAL' then
      begin
        DebugLog('Loading General settings...');
        aNode := parentNode.ChildNodes.First;
        while aNode <> nil do
        begin
          if UpperCase(aNode.NodeName) = 'BACKUPSCOUNT' then
            fBackupFileCount := StrToIntDef(aNode.Text, 0);
          if UpperCase(aNode.NodeName) = 'BACKUPPATH' then
            fBackupPath := aNode.Text;
          aNode := aNode.NextSibling;
        end;
      end;
      if UpperCase(parentNode.NodeName) = 'MAP' then
      begin
        DebugLog('Loading Map settings...');
        Glb.MapForm.LoadFromXml(parentNode);
      end;
      if UpperCase(parentNode.NodeName) = 'DEVICES' then
      begin
        Glb.HIDControl.LoadFromXml(parentNode);
      end;
      parentNode := parentNode.NextSibling;
    end;
  except
    On E: EXMLDocError do
    begin
      ShowMessage(Format(Glb.MainForm.txt.Values['LoadErrorS'], [E.Message]));
    end;
    On E: EDOMParseError do
    begin
      ShowMessage(Format(Glb.MainForm.txt.Values['LoadErrorS'], [E.Message]));
    end;
  end;
  Glb.MainForm.LoadFromXml(RootNode);
end;

end.
