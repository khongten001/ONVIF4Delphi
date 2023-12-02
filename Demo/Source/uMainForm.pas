unit uMainForm;
interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, ONVIF,
  Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls, ONVIF.Structure.Profile, TypInfo,
  System.Rtti, ONVIF.Structure.Capabilities,System.IniFiles ;
  
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
    Fonvfprb1: TONVIFManager;
    procedure BuildProfileTreeView(Node: TTreeNode; const Profile: TProfile);
    procedure BuildRecordTreeView(Node: TTreeNode; const FieldName: string;const RecordValue: TValue);
    procedure BuildCapabilitiesTreeView(Node: TTreeNode;const aCapabilities: TCapabilitiesONVIF);
    procedure DoONProfileTokenFound(const aName,aToken: String;
      var aSetForDefault: Boolean);
    procedure DoOnWriteLog(const aMethodName, aDescription: String;
      aLevel: TPONVIFLivLog; IsVerboseLog: boolean=False);
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

  Fonvfprb1.PTZ.StartMove(opmvContinuousMove,opcLeft);
end;

procedure TForm1.btnPTZPanRightMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Fonvfprb1.PTZ.StartMove(opmvContinuousMove,opcRight);
end;

procedure TForm1.btnPTZPanRightMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Fonvfprb1.PTZ.Stop;
end;

procedure TForm1.btnPTZTiltDownMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Fonvfprb1.PTZ.StartMove(opmvContinuousMove,opcBotton);
end;

procedure TForm1.btnPTZTiltUpMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    Fonvfprb1.PTZ.StartMove(opmvContinuousMove,opcTop);
end;

procedure TForm1.btnPTZZoomInMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Fonvfprb1.PTZ.Zoom(opmvContinuousMove,True);
end;

procedure TForm1.btnPTZZoomOutMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Fonvfprb1.PTZ.Zoom(opmvContinuousMove,False);
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
    tpLiveException  : Memo1.Lines.Add( Format('%s [EXCEPTION] %s --> %s',[DateTimeToStr(Now),aMethodName,aDescription]));
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
  if Assigned(Fonvfprb1) then
    FreeAndNil(Fonvfprb1);
  Fonvfprb1                    := TONVIFManager.Create(EUrl.Text,EUser.Text,Epwd.Text); 
  Fonvfprb1.SaveResponseOnDisk := True;
  Fonvfprb1.OnWriteLog         := DoOnWriteLog;
  Fonvfprb1.OnProfileTokenFound:= DoONProfileTokenFound;
  
  ListView1.Items.BeginUpdate;
  Try
    Fonvfprb1.ReadInfo;
  Finally
    ListView1.Items.EndUpdate;
  End;
  
  ECurrentToken.Text := Fonvfprb1.Token;
  Tv1.Items.BeginUpdate;
  tv1.Items.Clear;
  try
    LRootNode := tv1.Items.Add(nil,Fonvfprb1.Device.Manufacturer);
    LChildNode := tv1.Items.AddChild(LRootNode,'DeviceInfo');
    tv1.Items.AddChild(LChildNode,Format('Model: %s',[Fonvfprb1.Device.Model]));
    tv1.Items.AddChild(LChildNode,Format('FirmwareVersion: %s',[Fonvfprb1.Device.FirmwareVersion]));
    tv1.Items.AddChild(LChildNode,Format('SerialNumber: %s',[Fonvfprb1.Device.SerialNumber]));
    tv1.Items.AddChild(LChildNode,Format('HardwareId: %s',[Fonvfprb1.Device.HardwareId])) ;   
    BuildCapabilitiesTreeView(LRootNode,Fonvfprb1.Capabilities);

    LRootNode := tv1.Items.Add(nil,'Profiles');

    for I := Low(Fonvfprb1.Profiles) to High(Fonvfprb1.Profiles) do      
      BuildProfileTreeView(LRootNode,Fonvfprb1.Profiles[I]);
  Finally
    Tv1.Items.EndUpdate
  End;
  TabPTZ.Enabled := ListView1.Items.Count > 0;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if not Assigned(Fonvfprb1) then Exit;
   Fonvfprb1.PTZ.LoadPresetList;
   PnlPreset.Enabled := Fonvfprb1.PTZ.PresetList.Count > 0;
   ShowMessage(Format('Preset found [%d]',[Fonvfprb1.PTZ.PresetList.Count]));
end;

procedure TForm1.Button3Click(Sender: TObject);
var LNewIp : String;
begin
  if InputQuery('New ip','',LNewIP) then
    EURL.Text := Format('onvif://%S:80/',[LNewIP]);
  
end;

procedure TForm1.Button4Click(Sender: TObject);
var Lindex : String;
begin
  if not Assigned(Fonvfprb1) then Exit;
  
  if InputQuery('Index preset','',Lindex) then  
    Fonvfprb1.PTZ.GoToPreset(Lindex.ToInteger)
end;

procedure TForm1.Button5Click(Sender: TObject);
var LPresetName : String;
    LnewIndex   : Integer;
begin
  if not Assigned(Fonvfprb1) then Exit;
  
  if InputQuery('New preset','',LPresetName) then  
    Fonvfprb1.PTZ.SetPreset(LPresetName,LnewIndex,-1);  
end;

procedure TForm1.Button6Click(Sender: TObject);
var Lindex : String;
begin
  if not Assigned(Fonvfprb1) then Exit;
  
  if InputQuery('Index preset','',Lindex) then  
    Fonvfprb1.PTZ.RemovePreset(Lindex.ToInteger)
end;

procedure TForm1.Button7Click(Sender: TObject);
begin 
  if not Assigned(Fonvfprb1) then Exit;
  Fonvfprb1.PTZ.GotoHomePosition;
end;

procedure TForm1.Button8Click(Sender: TObject);
begin
  if not Assigned(Fonvfprb1) then Exit;
  Fonvfprb1.PTZ.SetHomePosition;
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

      if LField.FieldType.TypeKind = tkRecord then
         BuildRecordTreeView(Node, LField.Name, LValue)
      else
        tv1.Items.AddChild(Node, Format('%s: %s', [LField.Name, LValue.ToString]));
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

      if LField.FieldType.TypeKind = tkRecord then
         BuildRecordTreeView(Node, LField.Name, LValue)
      else
        tv1.Items.AddChild(Node, Format('%s: %s', [LField.Name, LValue.ToString]));
    end;
  finally
    LContext.Free;
  end;
end;

procedure TForm1.BuildRecordTreeView(Node: TTreeNode; const FieldName: string; const RecordValue: TValue);
var LField: TRttiField;
    LValue: TValue;
begin
  Node := tv1.Items.AddChild(Node, FieldName);

  for LField in TRttiContext.Create.GetType(RecordValue.TypeInfo).GetFields do
  begin
    LValue := LField.GetValue(RecordValue.GetReferenceToRawData);

    if LField.FieldType.TypeKind = tkRecord then
      BuildRecordTreeView(Node, LField.Name, LValue)
    else
      tv1.Items.AddChild(Node, Format('%s: %s', [LField.Name, LValue.ToString]));
  end;
end;

end.

