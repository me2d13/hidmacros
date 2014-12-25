unit uMapForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, OleCtrls, SHDocVw, ExtCtrls, MSHTML, StdCtrls, XMLIntf;

type
  TMapStyle = class
  public
    TypeValue: String;
    MarkerIconName: String;
    PolygonFillColor: String;
    PolygonFillOpacity: String;
    PolygonStrokeColor: String;
    PolygonStrokeWeight: String;
    PolygonStrokeOpacity: String;
    LineStrokeColor: String;
    LineStrokeWeight: String;
    LineStrokeOpacity: String;
    function GenerateJavaScript(pIndex: Integer; pTypeColumn: String): String;
    procedure SaveToXML(pNode : IXMLNode);
    function LoadFromXml(parent : IXMLNode): Boolean;
  end;

  TMapLayer = class
  private
    fStyles: TList;
  public
    Id: Integer;
    Name: String;
    TableId: String;
    LocationColumn: String;
    TypeColumn: String;
    function GenerateJavaScript(pIndex: Integer; pShow: Boolean): String;
    procedure SaveToXML(pMapNode : IXMLNode);
    function LoadFromXml(parent : IXMLNode): Boolean;
    constructor Create;
    destructor Destroy; Override;
    property Styles: TList read fStyles;
  end;

  TMapForm = class(TForm)
    FlowPanel1: TFlowPanel;
    WebBrowser1: TWebBrowser;
    Timer1: TTimer;
    LockedCB: TCheckBox;
    AlwaysOnTopCB: TCheckBox;
    ShowLayersCB: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure AlwaysOnTopCBClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ShowLayersCBClick(Sender: TObject);
  protected
    procedure CreateParams(var Params: TCreateParams) ; override;
  private
    { Private declarations }
    HTMLWindow2: IHTMLWindow2;
    fLoaded: Boolean;
    fVisibleByDefault: Boolean;
    fLatitude: double;
    fLongitude: double;
    fHeading: Integer;
    fZoom: real;
    fWinLeft: Integer;
    fWinTop: Integer;
    fWinHeight: Integer;
    fWinWidth: Integer;
    fHeight: double;
    fPlaneVisble: boolean;
    fDefaultRefreshInterval: Integer;
    fOnToggle: TNotifyEvent;
    fIsVisible: Boolean;
    fShowLayers: Boolean;
    fLayers: TStringList;
    procedure ReadMapValues;
    function GetElementIdValue(TagName, TagId, TagAttrib: string):string;
    function GetLock: boolean;
    procedure SetLock(const Value: boolean);
    procedure SaveWindowPos;
    procedure RestoreWindowPos;
    function GetHtml(pAddFunction: String = ''): String;
    procedure AddLayers;
    function GetLayersHtml: String;
    procedure CreateDefaultLayer;
  public
    { Public declarations }
    procedure Load;
    procedure SetMapCenter(pLat, pLong: double);
    procedure SetPlane(pLat, pLong: double; pHeading: Integer);
    procedure SetPlaneVisible(pVisible: boolean);
    procedure SaveToXML(pMapNode : IXMLNode);
    procedure SaveLayersToXML(pNode : IXMLNode);
    function LoadFromXml(parent : IXMLNode): Boolean;
    procedure Toggle;
    procedure SetVisibility(pVisible: Boolean);
    property Loaded: boolean read fLoaded;
    property Lock: boolean read GetLock write SetLock;
    property Latitude: double read fLatitude write fLatitude;
    property Longitude: double read fLongitude write fLongitude;
    property Zoom: real read fZoom write fZoom;
    property Elevation: double read fHeight write fHeight;
    property VisibleByDefault: Boolean read fVisibleByDefault;
    property OnToggle: TNotifyEvent read fOnToggle write fOnToggle;
    property IsVisible: Boolean read fIsVisible;
  end;


implementation

uses
   ActiveX, uGlobals;

{$R *.dfm}

const
  defLatitude = 50.10188601965;
  defLongitude = 14.2264752029291;

HTMLStr1: String =
'<html> '+ #13 +
'<head> '+ #13 +
'<meta name="viewport" content="initial-scale=1.0, user-scalable=yes" /> '+ #13 +
'<script type="text/javascript" src="http://maps.google.com/maps/api/js?sensor=true"></script> '+ #13 +
'<script type="text/javascript"> '+ #13 +
''+ #13 +
''+ #13 +
'  var geocoder; '+ #13 +
'  var map;  '+ #13 +
'  var plane;  '+ #13 +
'  var zoomLevel;  '+ #13 +
'  var plane_sprite = new Array(); '+ #13 +
'  var sprite_index = -1; '+ #13 +
'  var layers = new Array(); '+ #13 +
'  var layerStyles = new Array(); '+ #13 +
' '+ #13 +
'  function GetFileName(index) { '+ #13 +
'    if (index < 10) '+ #13 +
'      return("'; HTMLStr2: String = 'plane/plane_0"+i+".png"); '+ #13 +
'    else '+ #13 +
'      return("'; HTMLStr3: String = 'plane/plane_"+i+".png"); '+ #13 +
'  } '+ #13 +
' '+ #13 +
'  for (i = 0; i < 36; i++) { '+ #13 +
'    plane_sprite[i] = new google.maps.MarkerImage(GetFileName(i), '+ #13 +
'      new google.maps.Size(30, 30), '+ #13 +
'      new google.maps.Point(0,0),  '+ #13 +
'      new google.maps.Point(15, 15));  '+ #13 +
'  } '+ #13 +
' '+ #13 +
''+ #13 +
''+ #13 +
'  function initialize() { '+ #13 +
'    geocoder = new google.maps.Geocoder();'+ #13 +
'    var latlng = new google.maps.LatLng(%2.8f,%2.8f);' + #13 +
'    var myOptions = { '+ #13 +
'      zoom: %2.3f, '+ #13 +
'      center: latlng, '+ #13 +
'      mapTypeId: google.maps.MapTypeId.ROADMAP '+ #13 +
'    }; '+ #13 +
'    map = new google.maps.Map(document.getElementById("map_canvas"), myOptions); '+ #13 +
'    google.maps.event.addListener(map, "zoom_changed", function() { ' + #13 +
'    zoomLevel = map.getZoom(); ' + #13 +
'    document.getElementById("zoomLevelField").value = zoomLevel; '+ #13 +
'  }); '+ #13 +
'  InitLayers(); ' + #13 +
'  } '+ #13 +
'';

const
HTMLStr4: String =
''+ #13 +
''+ #13 +
'  function GotoLatLng(Lat, Lang) { '+ #13 +
'   var latlng = new google.maps.LatLng(Lat,Lang);'+ #13 +
'   map.setCenter(latlng);'+ #13 +
'  } '+ #13 +
''+ #13 +
''+ #13 +
'function HidePlane() {  '+ #13 +
'  if (plane) {        '+ #13 +
'    plane.setMap(null); '+ #13 +
'  } '+ #13 +
'}  '+ #13 +
''+ #13 +
'  function ShowPlane(Lat, Lang, Heading, Msg) { '+ #13 +
'  if (!plane) { ' + #13 +
'    var latlng = new google.maps.LatLng(Lat,Lang);'+ #13 +
'    var new_index = Math.round(Heading / 10);'+ #13 +
'    if (new_index >= 36) '+ #13 +
'      new_index = 0; '+ #13 +
'    if (new_index != sprite_index) '+ #13 +
'      sprite_index = new_index; '+ #13 +
'    plane = new google.maps.Marker({'+ #13 +
'      clickable: false, ' + #13 +
'      position: latlng, ' + #13 +
'      map: map,'+ #13 +
'      icon: plane_sprite[sprite_index], ' + #13 +
'      title: Msg'+ #13 +
'    });'+ #13 +
'  }'+ #13 +
' }'+ #13 +
'  '+ #13 +
'function SetPlanePos(Lat, Lng, Heading, lock) {'+ #13 +
'  if (!plane)'+ #13 +
'    exit;'+ #13 +
'  var latlng = new google.maps.LatLng(Lat,Lng);'+ #13 +
'  plane.setPosition(latlng);'+ #13 +
'  var new_index = Math.round(Heading / 10);'+ #13 +
'  if (new_index >= 36) '+ #13 +
'    new_index = 0; '+ #13 +
'  if (new_index != sprite_index) { '+ #13 +
'    sprite_index = new_index; '+ #13 +
'    plane.setIcon(plane_sprite[sprite_index]); '+ #13 +
'  } '+ #13 +
'  if (lock) '+ #13 +
'    map.setCenter(latlng); '+ #13 +
'} '+ #13 +
' '+ #13 +
'function SetLayer(pIndex, pFrom, pWhat, pShow) {'+ #13 +
'  layerStyles[pIndex] = [];'+ #13 +
'  layers[pIndex] = new google.maps.FusionTablesLayer({'+ #13 +
'      query: {'+ #13 +
'      select: pWhat,'+ #13 +
'      from: pFrom'+ #13 +
'      },'+ #13 +
'      styles: layerStyles[pIndex]'+ #13 +
'    });'+ #13 +
'  if (pShow) '+ #13 +
'    layers[pIndex].setMap(map);'+ #13 +
'}'+ #13 +
''+ #13 +
'function AddStyle(pIndex, pCondition, pMarkerOptions, pPolylineOptions, pPolygonOptions) {'+ #13 +
'  layerStyles[pIndex][layerStyles[pIndex].length] = {'+ #13 +
'    where: pCondition,'+ #13 +
'    markerOptions: pMarkerOptions,'+ #13 +
'    polylineOptions: pPolylineOptions,'+ #13 +
'    polygonOptions: pPolygonOptions'+ #13 +
'  };'+ #13 +
'}'+ #13 +
'function RefreshLayer(pIndex) {'+ #13 +
'  layers[pIndex].setMap(null);'+ #13 +
'  layers[pIndex].setMap(map);'+ #13 +
'}'+ #13 +
' '+ #13 +
'function SetLayersVisbility(pValue) {' + #13 +
' var lMap = null;'+ #13 +
' if (pValue) lMap = map; '+ #13 +
' for (key in layers) '+ #13 +
'   layers[key].setMap(lMap);'+ #13 +
'}'+ #13 +
' '+ #13;

const
HTMLStr5: String =
''+ #13 +
''+'</script> '+ #13 +
'</head> '+ #13 +
'<body style="margin: 0px" onload="initialize()" > '+ #13 +
'  <input type="hidden" id="zoomLevelField" value="" /> '+ #13 +
'  <div id="map_canvas" style="width:100%; height:100%"></div> '+ #13 +
'</body> '+ #13 +
'</html> ';


constructor TMapLayer.Create;
begin
  fStyles := TList.Create;
  TypeColumn := 'Type';
end;

destructor TMapLayer.Destroy;
var
  I: Integer;
begin
  for I := 0 to fStyles.Count - 1 do
    TMapStyle(fStyles.Items[I]).Free;
  fStyles.Free;
  inherited;
end;

function TMapLayer.GenerateJavaScript(pIndex: Integer; pShow: Boolean): String;
begin
  if pShow then
    Result := Format('SetLayer(%d, "%s", "%s", true)',
        [pIndex, TableId, LocationColumn])
  else
    Result := Format('SetLayer(%d, "%s", "%s", false)',
        [pIndex, TableId, LocationColumn]);
end;

procedure TMapLayer.SaveToXML(pMapNode : IXMLNode);
var
  x: IXMLNode;
  I: Integer;
begin
  x := pMapNode.AddChild('Id');
  x.Text := IntToStr(Id);
  x := pMapNode.AddChild('Name');
  x.Text := Name;
  x := pMapNode.AddChild('TableId');
  x.Text := TableId;
  x := pMapNode.AddChild('LocationColumn');
  x.Text := LocationColumn;
  if (TypeColumn <> 'Type') then
  begin
    x := pMapNode.AddChild('TypeColumn');
    x.Text := TypeColumn;
  end;
  for I := 0 to fStyles.Count - 1 do
  begin
    x := pMapNode.AddChild('Style');
    TMapStyle(fStyles[I]).SaveToXML(x);
  end;       
end;

function TMapLayer.LoadFromXml(parent : IXMLNode): Boolean;
var tmp: String;
  lStyle: TMapStyle;
  aNode: IXMLNode;
begin
  Result := True;
  aNode := parent.ChildNodes.First;
  while aNode <> nil do
  begin
    if UpperCase(aNode.NodeName) = 'ID' then
      Id := StrToIntDef(aNode.Text, -1)
    else if UpperCase(aNode.NodeName) = 'NAME' then
      Name := aNode.Text
    else if UpperCase(aNode.NodeName) = 'LOCATIONCOLUMN' then
      LocationColumn := aNode.Text
    else if UpperCase(aNode.NodeName) = 'TYPECOLUMN' then
      TypeColumn := aNode.Text
    else if UpperCase(aNode.NodeName) = 'TABLEID' then
      TableId := aNode.Text
    else if UpperCase(aNode.NodeName) = 'STYLE' then
    begin
      lStyle := TMapStyle.Create;
      if lStyle.LoadFromXml(aNode) then
        fStyles.Add(lStyle)
      else
        lStyle.Free;
    end;
    aNode := aNode.NextSibling;
  end;
  if (Id = -1) or (Name = '') then
    Result := False;
end;

procedure TMapForm.AddLayers;
var
  I, J: Integer;
  lLayer: TMapLayer;
begin
  for I := 0 to fLayers.Count - 1 do
  begin
    lLayer := fLayers.Objects[I] as TMapLayer;
    HTMLWindow2.execScript(lLayer.GenerateJavaScript(I, False), 'JavaScript');
    for J := 0 to lLayer.Styles.Count - 1 do
      HTMLWindow2.execScript(TMapStyle(lLayer.Styles[J]).GenerateJavaScript(
        I, lLayer.TypeColumn), 'JavaScript');
    HTMLWindow2.execScript('RefreshLayer('+IntToStr(I)+');', 'JavaScript');
  end;
end;

procedure TMapForm.AlwaysOnTopCBClick(Sender: TObject);
begin
  if AlwaysOnTopCB.Checked then
    FormStyle := fsStayOnTop
  else
    FormStyle := fsNormal;
end;

procedure TMapForm.Button2Click(Sender: TObject);
begin
HTMLWindow2.execScript('Debug();', 'JavaScript');
end;

procedure TMapForm.CreateDefaultLayer;
var
  lL: TMapLayer;
  lS: TMapStyle;
begin
   lL := TMapLayer.Create;
   lL.Id := 1;
   lL.Name := 'Data';
   lL.TableId := '924862';
   lL.LocationColumn := 'Kml';
   fLayers.AddObject(lL.Name, lL);
   lS := TMapStyle.Create();
   lS.TypeValue := 'AIRPORT';
   lS.MarkerIconName := 'airports';
   lL.Styles.Add(lS);
   lS := TMapStyle.Create();
   lS.TypeValue := 'VFR_POINT';
   lS.MarkerIconName := 'open_diamond';
   lL.Styles.Add(lS);
   lS := TMapStyle.Create();
   lS.TypeValue := 'VFR_PATH';
   lS.LineStrokeColor := '#0000CC';
   lS.LineStrokeWeight := '5';
   lS.LineStrokeOpacity := '0.5';
   lL.Styles.Add(lS);
end;

procedure TMapForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW;
  Params.WndParent := GetDesktopWindow;
end;

procedure TMapForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  fIsVisible := False;
  if Assigned(fOnToggle) then
    fOnToggle(self);
end;

procedure TMapForm.FormCreate(Sender: TObject);
begin
   fLoaded := False;
   LockedCB.Checked := True;
   fLatitude := defLatitude;
   fLongitude := defLongitude;
   fHeading := 0;
   fPlaneVisble := False;
   fDefaultRefreshInterval := 30;
   SaveWindowPos;
   fVisibleByDefault := False;
   fOnToggle := nil;
   fIsVisible := False;
   fLayers := TStringList.Create;
   fShowLayers := True;
end;

procedure TMapForm.FormDestroy(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to fLayers.Count - 1 do
    (fLayers.Objects[I] as TMapLayer).Free;
  fLayers.Free;
end;

procedure TMapForm.FormHide(Sender: TObject);
begin
  fIsVisible := False;
  if WindowState = wsNormal then
    SaveWindowPos;
  Timer1.Enabled := False;
end;

procedure TMapForm.FormShow(Sender: TObject);
begin
  fIsVisible := True;
  if not fLoaded then
    Load;
  Glb.Xpl.PosRefreshInterval := fDefaultRefreshInterval;
  if WindowState = wsNormal then
  begin
    Left := fWinLeft;
    Top := fWinTop;
  end;
  Timer1.Enabled := True;
end;

function TMapForm.GetHtml(pAddFunction: String = ''): String;
var
  lPath: String;
begin
  lPath := ExtractFilePath(Application.ExeName);
  lPath := StringReplace(lPath, '\', '/', [rfReplaceAll]);
  lPath := 'file:///'+lPath;
  Result := HTMLStr1 + lPath + HTMLStr2 + lPath;
  Result := Result + Format(HTMLStr3, [fLatitude, fLongitude, fZoom]);
  Result := Result + HTMLStr4 + pAddFunction + HTMLStr5;
end;

procedure TMapForm.Load;
var
  aStream     : TStringStream;
  //tmp: TStringList;
  lLHtml: String;
begin
  if fLoaded then
    exit;
  DecimalSeparator := '.';
   WebBrowser1.Navigate('about:blank');
    if Assigned(WebBrowser1.Document) then
    begin
      lLHtml := 'function InitLayers() {' + #13;
      if fShowLayers then
        lLHtml := lLHtml + GetLayersHtml + #13;
      lLHtml := lLHtml + '};'+ #13;
      aStream := TStringStream.Create(GetHtml(lLHtml));
      try
         //aStream.WriteBuffer(Pointer(HTMLStr)^, Length(HTMLStr));
         //aStream.WriteString(HTMLStr);
         //aStream.Write(HTMLStr[1], Length(HTMLStr));
         aStream.Seek(0, soFromBeginning);
         (WebBrowser1.Document as IPersistStreamInit).Load(TStreamAdapter.Create(aStream));
         //debug, save to file
         //tmp := TStringList.Create;
         //tmp.Text := GetHtml;
         //tmp.SaveToFile('tmp.html');
         //tmp.free;
      finally
         aStream.Free;
      end;
      HTMLWindow2 := (WebBrowser1.Document as IHTMLDocument2).parentWindow;
    end;
   fLoaded := True;
   //Memo1.Lines.Text := HTMLStr;
end;

function TMapForm.LoadFromXml(parent: IXMLNode): Boolean;
var tmp: String;
  A: Integer;
  B: TMouseEvent;
  aNode: IXMLNode;
  lMaximized: Boolean;
  lLayer: TMapLayer;
begin
  Result := True;
  DecimalSeparator := '.';
  aNode := parent.ChildNodes.First;
  while aNode <> nil do
  begin
    if UpperCase(aNode.NodeName) = 'LASTLATITUDE' then
      fLatitude := StrToFloatDef(aNode.Text, defLatitude)
    else if UpperCase(aNode.NodeName) = 'LASTLONGITUDE' then
      fLongitude := StrToFloatDef(aNode.Text, defLongitude)
    else if UpperCase(aNode.NodeName) = 'XPLREFRESHINTERVAL' then
      fDefaultRefreshInterval := StrToIntDef(aNode.Text, 30)
    else if UpperCase(aNode.NodeName) = 'LASTZOOM' then
      fZoom := StrToFloatDef(aNode.Text, 13)
    else if UpperCase(aNode.NodeName) = 'LOCKED' then
      LockedCB.Checked := (StrToIntDef(aNode.Text, 1) = 1)
    else if UpperCase(aNode.NodeName) = 'ALWAYSONTOP' then
      AlwaysOnTopCB.Checked := (StrToIntDef(aNode.Text, 1) = 1)
    else if UpperCase(aNode.NodeName) = 'WINDOWLEFT' then
      fWinLeft := StrToIntDef(aNode.Text, Left)
    else if UpperCase(aNode.NodeName) = 'WINDOWTOP' then
      fWinTop := StrToIntDef(aNode.Text, Top)
    else if UpperCase(aNode.NodeName) = 'WINDOWWIDTH' then
      fWinWidth := StrToIntDef(aNode.Text, Width)
    else if UpperCase(aNode.NodeName) = 'WINDOWHEIGHT' then
      fWinHeight := StrToIntDef(aNode.Text, Height)
    else if UpperCase(aNode.NodeName) = 'WINDOWVISIBLE' then
      fVisibleByDefault := (StrToIntDef(aNode.Text, 1) = 1)
    else if UpperCase(aNode.NodeName) = 'WINDOWMAXIMIZED' then
      lMaximized := (StrToIntDef(aNode.Text, 1) = 1)
    else if UpperCase(aNode.NodeName) = 'SHOWLAYERS' then
      fShowLayers := (StrToIntDef(aNode.Text, 1) = 1)
    else if UpperCase(aNode.NodeName) = 'LAYER' then
    begin
      lLayer := TMapLayer.Create;
      if lLayer.LoadFromXml(aNode) then
        fLayers.AddObject(lLayer.Name, lLayer)
      else
        lLayer.Free;
    end;
    aNode := aNode.NextSibling;
  end;
  //if fLayers.Count = 0 then
  //  CreateDefaultLayer;
  // somehow Left is destroyed during show when it links to 2nd
  RestoreWindowPos;
  AlwaysOnTopCBClick(nil);
  if lMaximized then
    WindowState := wsMaximized;
  ShowLayersCB.Checked := fShowLayers;
end;

procedure TMapForm.ReadMapValues;
var
  tmpVar : String;
begin
  //tmpVar := OleVariant(HTMLWindow2.Document).zoomLevel;
  tmpVar := GetElementIdValue('input', 'zoomLevelField', 'value');
  fZoom := StrToFloatDef(tmpVar, 13);
end;

procedure TMapForm.RestoreWindowPos;
begin
  Left := fWinLeft;
  Top := fWinTop;
  Height := fWinHeight;
  Width := fWinWidth;
end;

procedure TMapForm.SaveLayersToXML(pNode: IXMLNode);
var
  I: Integer;
  x: IXMLNode;
begin
  for I := 0 to fLayers.Count - 1 do
  begin
    x := pNode.AddChild('Layer');
    (fLayers.Objects[I] as TMapLayer).SaveToXML(x);
  end;
end;

procedure TMapForm.SaveToXML(pMapNode: IXMLNode);
var x: IXMLNode;
begin
  ReadMapValues;
  if Visible and (WindowState = wsNormal) then
    SaveWindowPos;
  x := pMapNode.AddChild('LastLatitude');
  x.Text := FloatToStr(fLatitude);
  x := pMapNode.AddChild('LastLongitude');
  x.Text := FloatToStr(fLongitude);
  x := pMapNode.AddChild('LastZoom');
  x.Text := FloatToStr(fZoom);
  if fDefaultRefreshInterval <> 30 then
  begin
    x := pMapNode.AddChild('XplRefreshInterval');
    x.Text := IntToStr(fDefaultRefreshInterval);
  end;
  x := pMapNode.AddChild('Locked');
  if LockedCB.Checked then
    x.Text := '1'
  else
    x.Text := '0';
  x := pMapNode.AddChild('AlwaysOnTop');
  if AlwaysOnTopCB.Checked then
    x.Text := '1'
  else
    x.Text := '0';
  // win position
  x := pMapNode.AddChild('WindowLeft');
  x.Text := IntToStr(fWinLeft);
  x := pMapNode.AddChild('WindowTop');
  x.Text := IntToStr(fWinTop);
  x := pMapNode.AddChild('WindowWidth');
  x.Text := IntToStr(fWinWidth);
  x := pMapNode.AddChild('WindowHeight');
  x.Text := IntToStr(fWinHeight);
  x := pMapNode.AddChild('WindowMaximized');
  if WindowState = wsMaximized then
    x.Text := '1'
  else
    x.Text := '0';
  x := pMapNode.AddChild('WindowVisible');
  if Visible then
    x.Text := '1'
  else
    x.Text := '0';
  x := pMapNode.AddChild('ShowLayers');
  if fShowLayers then
    x.Text := '1'
  else
    x.Text := '0';
  SaveLayersToXML(pMapNode);
end;

procedure TMapForm.SaveWindowPos;
begin
  fWinLeft := Left;
  fWinTop := Top;
  fWinHeight := Height;
  fWinWidth := Width;
end;

procedure TMapForm.SetLock(const Value: boolean);
begin
  LockedCB.Checked := Value;
end;

procedure TMapForm.SetMapCenter(pLat, pLong: double);
begin
  HTMLWindow2.execScript(Format('GotoLatLng(%2.8e,%2.8e)',[pLat,pLong]), 'JavaScript');
  //HTMLWindow2.execScript('GotoLatLng(50.102, 14.226)', 'JavaScript');
  //HTMLWindow2.execScript('GotoLatLng('+Float+', '++')', 'JavaScript');
end;

procedure TMapForm.SetPlane(pLat, pLong: double; pHeading: Integer);
var lLock: String;
begin
  if LockedCB.Checked then
    lLock := 'true'
  else
    lLock := 'false';
  HTMLWindow2.execScript(Format('SetPlanePos(%2.8e,%2.8e, %d, %s)',[pLat,pLong, pHeading, lLock]), 'JavaScript');
end;

procedure TMapForm.SetPlaneVisible(pVisible: boolean);
begin
  if pVisible <> fPlaneVisble then
  begin
    if pVisible then
    begin
      HTMLWindow2.execScript(Format('ShowPlane(%2.8f,%2.8f, %d, "%s")',
        [fLatitude,fLongitude, fHeading, 'My plane']), 'JavaScript');
      //ShowMessage(Format('ShowPlane(%2.8f,%2.8f, %d, "%s")',
      //  [fLatitude,fLongitude, fHeading, 'My plane']));
    end
    else
      HTMLWindow2.execScript('HidePlane()', 'JavaScript');
    fPlaneVisble := pVisible;
  end;
end;

procedure TMapForm.SetVisibility(pVisible: Boolean);
begin
  if pVisible <> Visible then
    Toggle;
end;

procedure TMapForm.ShowLayersCBClick(Sender: TObject);
begin
  fShowLayers := ShowLayersCB.Checked;
  if fLoaded then
    if fShowLayers then
      HTMLWindow2.execScript('SetLayersVisbility(true);', 'JavaScript')
    else
      HTMLWindow2.execScript('SetLayersVisbility(false);', 'JavaScript')
end;

procedure TMapForm.Timer1Timer(Sender: TObject);
var
  lLatitude, lLongitude: double;
  lHeading: Integer;
begin
  if Glb.Xpl.IsXplaneConnected then
  begin
    if not fPlaneVisble then
      SetPlaneVisible(true);
    lLatitude := Glb.Xpl.Latitude;
    lLongitude := Glb.Xpl.Longitude;
    lHeading := Round(Glb.Xpl.Heading);
    if (fLatitude <> lLatitude) or (fLongitude <> lLongitude) or
       (fHeading <> lHeading) then
    begin
      fLatitude := lLatitude;
      fLongitude := lLongitude;
      fHeading := lHeading;
      SetPlane(fLatitude, fLongitude, fHeading);
    end;
  end
  else
  begin
    if fPlaneVisble then
      SetPlaneVisible(false);
  end;
end;

procedure TMapForm.Toggle;
begin
  if Visible then
    Hide
  else
    Show;
  if Assigned(fOnToggle) then
    fOnToggle(self);
end;

function TMapForm.GetElementIdValue(TagName, TagId, TagAttrib: string):string;
var
  Document: IHTMLDocument2;
  Body: IHTMLElement2;
  Tags: IHTMLElementCollection;
  Tag: IHTMLElement;
  I: Integer;
begin
  Result:='';
  if not Supports(WebBrowser1.Document, IHTMLDocument2, Document) then
    //raise Exception.Create('Invalid HTML document');
    exit;
  if not Supports(Document.body, IHTMLElement2, Body) then
    //raise Exception.Create('Can''t find <body> element');
    exit;
  Tags := Body.getElementsByTagName(UpperCase(TagName));
  for I := 0 to Pred(Tags.length) do begin
    Tag:=Tags.item(I, EmptyParam) as IHTMLElement;
    if Tag.id=TagId then Result := Tag.getAttribute(TagAttrib, 0);
  end;
end;

function TMapForm.GetLayersHtml: String;
var
  I, J: Integer;
  lLayer: TMapLayer;
begin
  Result := '';
  for I := 0 to fLayers.Count - 1 do
  begin
    lLayer := fLayers.Objects[I] as TMapLayer;
    Result := Result + lLayer.GenerateJavaScript(I, False) + #13;
    for J := 0 to lLayer.Styles.Count - 1 do
      Result := Result + TMapStyle(lLayer.Styles[J]).GenerateJavaScript(
        I, lLayer.TypeColumn) + #13;
    Result := Result + 'RefreshLayer('+IntToStr(I)+');' + #13;
  end;
end;

function TMapForm.GetLock: boolean;
begin
  Result := LockedCB.Checked;
end;

{ TMapStyle }

function TMapStyle.LoadFromXml(parent: IXMLNode): Boolean;
var tmp: String;
  aNode: IXMLNode;
begin
  Result := True;
  aNode := parent.ChildNodes.First;
  while aNode <> nil do
  begin
    if UpperCase(aNode.NodeName) = 'TYPE' then
      TypeValue := aNode.Text
    else if UpperCase(aNode.NodeName) = 'MARKERICONNAME' then
      MarkerIconName := Trim(aNode.Text)
    else if UpperCase(aNode.NodeName) = 'POLYGONFILLCOLOR' then
      PolygonFillColor := Trim(aNode.Text)
    else if UpperCase(aNode.NodeName) = 'POLYGONFILLOPACITY' then
      PolygonFillOpacity := Trim(aNode.Text)
    else if UpperCase(aNode.NodeName) = 'POLYGONSTROKECOLOR' then
      PolygonStrokeColor := Trim(aNode.Text)
    else if UpperCase(aNode.NodeName) = 'POLYGONSTROKEWEIGHT' then
      PolygonStrokeWeight := Trim(aNode.Text)
    else if UpperCase(aNode.NodeName) = 'POLYGONSTROKEOPACITY' then
      PolygonStrokeOpacity := Trim(aNode.Text)
    else if UpperCase(aNode.NodeName) = 'LINESTROKECOLOR' then
      LineStrokeColor := Trim(aNode.Text)
    else if UpperCase(aNode.NodeName) = 'LINESTROKEWEIGHT' then
      LineStrokeWeight := Trim(aNode.Text)
    else if UpperCase(aNode.NodeName) = 'LINESTROKEOPACITY' then
      LineStrokeOpacity := Trim(aNode.Text);
    aNode := aNode.NextSibling;
  end;
end;

procedure TMapStyle.SaveToXML(pNode: IXMLNode);
var
  x: IXMLNode;
begin
  x := pNode.AddChild('Type');
  x.Text := TypeValue;
  if MarkerIconName <> '' then
  begin
    x := pNode.AddChild('MarkerIconName');
    x.Text := MarkerIconName;
  end;
  if PolygonFillColor <> '' then
  begin
    x := pNode.AddChild('PolygonFillColor');
    x.Text := PolygonFillColor;
  end;
  if PolygonFillOpacity <> '' then
  begin
    x := pNode.AddChild('PolygonFillOpacity');
    x.Text := PolygonFillOpacity;
  end;
  if PolygonStrokeColor <> '' then
  begin
    x := pNode.AddChild('PolygonStrokeColor');
    x.Text := PolygonStrokeColor;
  end;
  if PolygonStrokeWeight <> '' then
  begin
    x := pNode.AddChild('PolygonStrokeWeight');
    x.Text := PolygonStrokeWeight;
  end;
  if PolygonStrokeOpacity <> '' then
  begin
    x := pNode.AddChild('PolygonStrokeOpacity');
    x.Text := PolygonStrokeOpacity;
  end;
  if LineStrokeColor <> '' then
  begin
    x := pNode.AddChild('LineStrokeColor');
    x.Text := LineStrokeColor;
  end;
  if LineStrokeWeight <> '' then
  begin
    x := pNode.AddChild('LineStrokeWeight');
    x.Text := LineStrokeWeight;
  end;
  if LineStrokeOpacity <> '' then
  begin
    x := pNode.AddChild('LineStrokeOpacity');
    x.Text := LineStrokeOpacity;
  end;
end;

function TMapStyle.GenerateJavaScript(pIndex: Integer; pTypeColumn: String): String;
var
  MarkerOptions: String;
  PolygonOptions: String;
  LineOptions: String;
  Condition: String; 
begin
  //AddStyle(0, "Type = 'VFR_PATH'", null, {strokeColor: '#0000CC', strokeWeight: 5, strokeOpacity: 0.3}, null);
  // condition
  if TypeValue = '' then
    Condition := 'null'
  else
    Condition := '"'+pTypeColumn+' = '''+TypeValue+'''"';
  // icon
  if MarkerIconName <> '' then
    MarkerOptions := Format('{iconName: ''%s''}', [MarkerIconName])
  else
    MarkerOptions := 'null';
  // line
  if 
    (LineStrokeColor <> '') or
    (LineStrokeWeight <> '') or
    (LineStrokeOpacity <> '') then
  begin
    LineOptions := '';
    if LineStrokeColor <> '' then
      LineOptions := LineOptions + ', strokeColor: ''' + LineStrokeColor + '''';
    if LineStrokeWeight <> '' then
      LineOptions := LineOptions + ', strokeWeight: ' + LineStrokeWeight;
    if LineStrokeOpacity <> '' then
      LineOptions := LineOptions + ', strokeOpacity: ' + LineStrokeOpacity;
    LineOptions := LineOptions + '}';
    LineOptions[1] := '{';
  end
  else
    LineOptions := 'null';
  // polygon
  if 
    (PolygonFillColor <> '') or
    (PolygonFillOpacity <> '') or
    (PolygonStrokeColor <> '') or
    (PolygonStrokeWeight <> '') or
    (PolygonStrokeOpacity <> '') then
  begin
    PolygonOptions := '';
    if PolygonFillColor <> '' then
      PolygonOptions := PolygonOptions + ', fillColor: ''' + PolygonFillColor + ''''; 
    if PolygonFillOpacity <> '' then
      PolygonOptions := PolygonOptions + ', fillOpacity: ' + PolygonFillOpacity; 
    if PolygonStrokeColor <> '' then
      PolygonOptions := PolygonOptions + ', strokeColor: ''' + PolygonStrokeColor + ''''; 
    if PolygonStrokeWeight <> '' then
      PolygonOptions := PolygonOptions + ', strokeWeight: ' + PolygonStrokeWeight; 
    if PolygonStrokeOpacity <> '' then
      PolygonOptions := PolygonOptions + ', strokeOpacity: ' + PolygonStrokeOpacity; 
    PolygonOptions := PolygonOptions + '}';
    PolygonOptions[1] := '{';
  end
  else
    PolygonOptions := 'null';
  Result := Format('AddStyle(%d, %s, %s, %s, %s);', [pIndex, Condition, 
      MarkerOptions, LineOptions, PolygonOptions]);
end;

end.
