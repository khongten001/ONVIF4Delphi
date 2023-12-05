unit uMainForm;
interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, ONVIF,
  Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls, ONVIF.Structure.Profile, TypInfo,ONVIF.Structure.PTZ,
  System.Rtti, ONVIF.Structure.Capabilities,System.IniFiles, ONVIF.Structure.Imaging; 
  
type
  TForm1 = class(TForm)
    tv1: TTreeView;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    PageControl2: TPageControl;
    TabPTZ: TTabSheet;
    Label108: TLabel;
    Label109: TLabel;
    btnPTZTiltUp: TButton;
    btnPTZPanLeft: TButton;
    btnPTZPanRight: TButton;
    btnPTZTiltDown: TButton;
    btnPTZZoomOut: TButton;
    btnPTZZoomIn: TButton;
    Panel1: TPanel;
    EUrl: TLabeledEdit;
    Button1: TButton;
    EUser: TLabeledEdit;
    Epwd: TLabeledEdit;
    Label1: TLabel;
    Memo1: TMemo;
    Panel2: TPanel;
    ListView1: TListView;
    ECurrentToken: TEdit;
    Label2: TLabel;
    Button3: TButton;
    Button2: TButton;
    Button7: TButton;
    PnlPreset: TPanel;
    Button4: TButton;
    Button6: TButton;
    Button5: TButton;
    Button8: TButton;
    procedure btnPTZPanRightMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnPTZTiltDownMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnPTZPanRightMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnPTZPanLeftMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Button1Click(Sender: TObject);
    procedure btnPTZZoomInMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnPTZZoomOutMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnPTZTiltUpMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Button2Click(Sender: TObject);
    procedure ListView1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    FONVIFManager: TONVIFManager;
    procedure BuildProfileTreeView(Node: TTreeNode; const Profile: TProfile);
    procedure BuildRecordTreeView(Node: TTreeNode; const FieldName: string;const RecordValue: TValue);
    procedure BuildCapabilitiesTreeView(Node: TTreeNode;const aCapabilities: TCapabilitiesONVIF);
    procedure BuildPTZNodeTreeView(Node: TTreeNode;const aPTZNode: TPTZNode);    
    procedure BuildImagingSettingsTreeView(Node: TTreeNode;const aImaginingSettings:TImagingSettings );    
    procedure DoONProfileTokenFound(const aName,aToken: String;
      var aSetForDefault: Boolean);
    procedure DoOnWriteLog(const aMethodName, aDescription: String;
      aLevel: TPONVIFLivLog; IsVerboseLog: boolean=False);
    procedure processTValue(aNode: TTreeNode; aField: TRttiField;
      aValue: TValue);
    procedure BuildImagingFocusOptionsTreeView(Node: TTreeNode;
      const aFocusOptions: TImagingFocusSettings);
  public
    { Public declarations }
  end;
var
  Form1: TForm1;
implementation

{$R *.dfm}
procedure TForm1.btnPTZPanLeftMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin

  FONVIFManager.PTZ.StartMoveContinuous(opcLeft);
end;

procedure TForm1.btnPTZPanRightMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FONVIFManager.PTZ.StartMoveContinuous(opcRight);
end;

procedure TForm1.btnPTZPanRightMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FONVIFManager.PTZ.Stop;
end;

procedure TForm1.btnPTZTiltDownMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FONVIFManager.PTZ.StartMoveContinuous(opcBotton);
end;

procedure TForm1.btnPTZTiltUpMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    FONVIFManager.PTZ.StartMoveContinuous(opcTop);
end;

procedure TForm1.btnPTZZoomInMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FONVIFManager.PTZ.Zoom(True);
end;

procedure TForm1.btnPTZZoomOutMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FONVIFManager.PTZ.Zoom(False);
end;

procedure TForm1.DoONProfileTokenFound(const aName, aToken : String;var aSetForDefault:Boolean);
begin 
  with ListView1.Items.Add do
  begin
    Caption     := aName;
    SubItems.Add(aToken);
  end;
end;

procedure TForm1.DoOnWriteLog(Const aMethodName,aDescription:String;aLevel : TPONVIFLivLog;IsVerboseLog:boolean=False);
begin
  case aLevel of
    tpLivInfo        : Memo1.Lines.Add( Format('%s [INFO     ] %s --> %s',[DateTimeToStr(Now),aMethodName,aDescription]));
    tpLivError       : Memo1.Lines.Add( Format('%s [ERROR    ] %s --> %s',[DateTimeToStr(Now),aMethodName,aDescription]));
    tpLivWarning     : Memo1.Lines.Add( Format('%s [WARNING  ] %s --> %s',[DateTimeToStr(Now),aMethodName,aDescription]));
    tpLivException   : Memo1.Lines.Add( Format('%s [EXCEPTION] %s --> %s',[DateTimeToStr(Now),aMethodName,aDescription]));
    tpLivXMLResp     : begin
    
                       end;
    
  end;                 
end;

procedure TForm1.FormCreate(Sender: TObject);
var LIniFile : TIniFile;
begin
  LIniFile := TIniFile.Create(ExtractFilePath(Application.ExeName)+ 'OnvifDemoConfig.ini');
  Try
     EUrl.Text  :=  LIniFile.ReadString('CONFIG','URL',String.Empty);
     EUser.Text :=  LIniFile.ReadString('CONFIG','USER',String.Empty);
     Epwd.Text  :=  LIniFile.ReadString('CONFIG','PWD',String.Empty);     
  finally
    FreeAndNil(LIniFile);
  end
  
end;

procedure TForm1.ListView1Click(Sender: TObject);
begin
  if Assigned(ListView1.Selected) then
    ECurrentToken.Text := ListView1.Selected.SubItems[0];
end;

procedure TForm1.Button1Click(Sender: TObject);
Var LRootNode  : TTreeNode; 
    LChildNode : TTreeNode;
    I          : INteger;
begin
  Memo1.Lines.Clear;
  if Assigned(FONVIFManager) then
    FreeAndNil(FONVIFManager);
  FONVIFManager                    := TONVIFManager.Create(EUrl.Text,EUser.Text,Epwd.Text); 
  FONVIFManager.SaveResponseOnDisk := True;
  FONVIFManager.OnWriteLog         := DoOnWriteLog;
  FONVIFManager.OnPTZTokenFound    := DoONProfileTokenFound;
  
  ListView1.Items.BeginUpdate;
  Try
    ListView1.Clear;
    FONVIFManager.ReadInfo;
  Finally
    ListView1.Items.EndUpdate;
  End;
  
  ECurrentToken.Text := FONVIFManager.PTZ.Token;
  Tv1.Items.BeginUpdate;
  tv1.Items.Clear;
  try
    LRootNode := tv1.Items.Add(nil,FONVIFManager.Device.Manufacturer);
    LChildNode := tv1.Items.AddChild(LRootNode,'DeviceInfo');
    tv1.Items.AddChild(LChildNode,Format('Model: %s',[FONVIFManager.Device.Model]));
    tv1.Items.AddChild(LChildNode,Format('FirmwareVersion: %s',[FONVIFManager.Device.FirmwareVersion]));
    tv1.Items.AddChild(LChildNode,Format('SerialNumber: %s',[FONVIFManager.Device.SerialNumber]));
    tv1.Items.AddChild(LChildNode,Format('HardwareId: %s',[FONVIFManager.Device.HardwareId])) ;   
    BuildCapabilitiesTreeView(LRootNode,FONVIFManager.Capabilities);

    LRootNode := tv1.Items.Add(nil,'Profiles');

    for I := Low(FONVIFManager.Profiles) to High(FONVIFManager.Profiles) do      
      BuildProfileTreeView(LRootNode,FONVIFManager.Profiles[I]);
    BuildPTZNodeTreeView(LRootNode,FONVIFManager.PTZ.PTZNode);
    BuildImagingSettingsTreeView(LRootNode,FONVIFManager.Imaging.ImagingSettings);      
    BuildImagingFocusOptionsTreeView(LRootNode,FONVIFManager.Imaging.FocusSettings);      

  Finally
    Tv1.Items.EndUpdate
  End;
  TabPTZ.Enabled := ListView1.Items.Count > 0;
end;

    

procedure TForm1.Button2Click(Sender: TObject);
begin
  if not Assigned(FONVIFManager) then Exit;
   FONVIFManager.PTZ.LoadPresetList;
   PnlPreset.Enabled := FONVIFManager.PTZ.PresetList.Count > 0;
   ShowMessage(Format('Preset found [%d]',[FONVIFManager.PTZ.PresetList.Count]));
end;

procedure TForm1.Button3Click(Sender: TObject);
var LNewIp : String;
begin
  LNewIp := '192.168.0.';
  if InputQuery('New ip','',LNewIP) then
    EURL.Text := Format('onvif://%S:80/',[LNewIP]);
  
end;

procedure TForm1.Button4Click(Sender: TObject);
var Lindex : String;
begin
  if not Assigned(FONVIFManager) then Exit;
  
  if InputQuery('Index preset','',Lindex) then  
    FONVIFManager.PTZ.GoToPreset(Lindex.ToInteger)
end;

procedure TForm1.Button5Click(Sender: TObject);
var LPresetName : String;
    LnewIndex   : Integer;
begin
  if not Assigned(FONVIFManager) then Exit;
  
  if InputQuery('New preset','',LPresetName) then  
    FONVIFManager.PTZ.SetPreset(LPresetName,LnewIndex,-1);  
end;

procedure TForm1.Button6Click(Sender: TObject);
var Lindex : String;
begin
  if not Assigned(FONVIFManager) then Exit;
  
  if InputQuery('Index preset','',Lindex) then  
    FONVIFManager.PTZ.RemovePreset(Lindex.ToInteger)
end;

procedure TForm1.Button7Click(Sender: TObject);
begin 
  if not Assigned(FONVIFManager) then Exit;
  FONVIFManager.PTZ.GotoHomePosition;
end;

procedure TForm1.Button8Click(Sender: TObject);
begin
  if not Assigned(FONVIFManager) then Exit;
  FONVIFManager.PTZ.SetHomePosition;
end;


procedure TForm1.BuildImagingFocusOptionsTreeView(Node: TTreeNode;const aFocusOptions:TImagingFocusSettings );    
var LContext: TRttiContext;
    LTypeObj: TRttiType;
    LField  : TRttiField;
    LValue  : TValue; 
    LIndex  : Integer;
begin
  Node := tv1.Items.AddChild(nil, 'FocusOptions');

  LContext := TRttiContext.Create;
  try
    LTypeObj := LContext.GetType(TypeInfo(TImagingFocusSettings));

    for LField in LTypeObj.GetFields do
    begin
      LValue := LField.GetValue(@aFocusOptions);
      processTValue(Node,LField,LValue);
    end;
  finally
    LContext.Free;
  end;
end;


procedure TForm1.BuildImagingSettingsTreeView(Node: TTreeNode;const aImaginingSettings:TImagingSettings );    
var LContext: TRttiContext;
    LTypeObj: TRttiType;
    LField  : TRttiField;
    LValue  : TValue; 
    LIndex  : Integer;
begin
  Node := tv1.Items.AddChild(nil, 'ImaginingSettings');

  LContext := TRttiContext.Create;
  try
    LTypeObj := LContext.GetType(TypeInfo(TImagingSettings));

    for LField in LTypeObj.GetFields do
    begin
      LValue := LField.GetValue(@aImaginingSettings);
      processTValue(Node,LField,LValue);
    end;
  finally
    LContext.Free;
  end;
end;

procedure TForm1.BuildCapabilitiesTreeView(Node: TTreeNode; const aCapabilities: TCapabilitiesONVIF);
var LContext: TRttiContext;
    LTypeObj: TRttiType;
    LField  : TRttiField;
    LValue  : TValue; 
begin
  Node := tv1.Items.AddChild(nil, 'Capabilities');

  LContext := TRttiContext.Create;
  try
    LTypeObj := LContext.GetType(TypeInfo(TCapabilitiesONVIF));

    for LField in LTypeObj.GetFields do
    begin
      LValue := LField.GetValue(@aCapabilities);
      processTValue(Node,LField,LValue);
    end;
  finally
    LContext.Free;
  end;
end;

procedure TForm1.BuildProfileTreeView(Node: TTreeNode; const Profile: TProfile);
var LContext: TRttiContext;
    LTypeObj: TRttiType;
    LField  : TRttiField;
    LValue  : TValue; 
begin
  Node    := tv1.Items.AddChild(Node, 'Profile');
  LContext := TRttiContext.Create;
  try
    LTypeObj := LContext.GetType(TypeInfo(TProfile));
    for LField in LTypeObj.GetFields do
    begin
      LValue := LField.GetValue(@Profile);
      processTValue(Node,LField,LValue);
    end;
  finally
    LContext.Free;
  end;
end;

procedure TForm1.BuildPTZNodeTreeView(Node: TTreeNode;
  const aPTZNode: TPTZNode);
var LContext: TRttiContext;
    LTypeObj: TRttiType;
    LField  : TRttiField;
    LValue  : TValue; 
begin
  Node    := tv1.Items.AddChild(nil, 'PTZNode');
  LContext := TRttiContext.Create;
  try
    LTypeObj := LContext.GetType(TypeInfo(TPTZNode));
    for LField in LTypeObj.GetFields do
    begin
      LValue := LField.GetValue(@aPTZNode);
      processTValue(Node,LField,LValue);
    end;
  finally
    LContext.Free;
  end;
end;

procedure TForm1.processTValue(aNode: TTreeNode; aField: TRttiField; aValue: TValue);
var
  LIndex: Integer;
  LElementValue: TValue;
  LElementNode: TTreeNode;
  LFields: TArray<TRttiField>;
  I: Integer;
  LFieldValue : TValue;
begin
  if aField.FieldType.TypeKind = tkRecord then
    BuildRecordTreeView(aNode, aField.Name, aValue)
  else if aField.FieldType.TypeKind = tkDynArray then
  begin
    // Handle dynamic arrays of records
    for LIndex := 0 to aValue.GetArrayLength - 1 do
    begin
      LElementValue := aValue.GetArrayElement(LIndex);

      // Create a new node for each element in the dynamic array
      LElementNode := tv1.Items.AddChild(aNode, Format('%s[%d]', [aField.Name, LIndex+1]));

      // Recursively process each element of the dynamic array
      LFields := TRttiContext.Create.GetType(LElementValue.TypeInfo).GetFields;

      if Length(LFields) > 0 then
      begin
       
         for I := 0 to Length(LFields)-1 do
         begin
          LFieldValue := LFields[I].GetValue(LElementValue.GetReferenceToRawData);

          if LFields[I].FieldType.TypeKind = tkRecord then
            BuildRecordTreeView(LElementNode, LFields[I].Name, LFieldValue)
          else
            processTValue(LElementNode, LFields[I], LFieldValue)
         end;
      end
      else            
        tv1.Items.AddChild(LElementNode, LElementValue.ToString);
    end;
  end
  else
    tv1.Items.AddChild(aNode, Format('%s: %s', [aField.Name, aValue.ToString]));
end;


procedure TForm1.BuildRecordTreeView(Node: TTreeNode; const FieldName: string; const RecordValue: TValue);
var LField: TRttiField;
    LValue: TValue;
begin
  Node := tv1.Items.AddChild(Node, FieldName);

  for LField in TRttiContext.Create.GetType(RecordValue.TypeInfo).GetFields do
  begin
    LValue := LField.GetValue(RecordValue.GetReferenceToRawData);
    processTValue(Node,LField,LValue);
  end;
end;


end.

