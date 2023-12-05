//*************************************************************
//                        ONVIF_WDSL                          *
//				                                        	  *
//                     Freeware Library                       *
//                       For Delphi 10.4                      *
//                            by                              *
//                     Alessandro Mancini                     *
//				                                        	  *
//*************************************************************
{LICENSE:
THIS SOFTWARE IS PROVIDED TO YOU "AS IS" WITHOUT WARRANTY OF ANY KIND,
EITHER EXPRESSED OR IMPLIED INCLUDING BUT NOT LIMITED TO THE APPLIED
WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR PURPOSE.
YOU ASSUME THE ENTIRE RISK AS TO THE ACCURACY AND THE USE OF THE SOFTWARE
AND ALL OTHER RISK ARISING OUT OF THE USE OR PERFORMANCE OF THIS SOFTWARE
AND DOCUMENTATION. PRODUCTIONS DOES NOT WARRANT THAT THE SOFTWARE IS ERROR-FREE
OR WILL OPERATE WITHOUT INTERRUPTION. THE SOFTWARE IS NOT DESIGNED, INTENDED
OR LICENSED FOR USE IN HAZARDOUS ENVIRONMENTS REQUIRING FAIL-SAFE CONTROLS,
INCLUDING WITHOUT LIMITATION, THE DESIGN, CONSTRUCTION, MAINTENANCE OR
OPERATION OF NUCLEAR FACILITIES, AIRCRAFT NAVIGATION OR COMMUNICATION SYSTEMS,
AIR TRAFFIC CONTROL, AND LIFE SUPPORT OR WEAPONS SYSTEMS. PRODUCTIONS SPECIFICALLY
DISCLAIMS ANY EXPRESS OR IMPLIED WARRANTY OF FITNESS FOR SUCH PURPOSE.

You may use/change/modify the component under 1 conditions:
1. In your application, add credits to "ONVIF WDSL"
{*******************************************************************************}

unit ONVIF.PTZ;

interface
                          
uses
  ONVIF.Types, ONVIF.XML.Utils, XmlDoc, XmlIntf, XMLDom, ONVIF.Constant.Error,
  System.Math, ONVIF.Structure.PTZ, System.Classes, ONVIF.Intf, System.SysUtils,
  Soap.XSBuiltIns;

Type
  TONVIFPTZManager     = class;
  /// <summary>
  ///   Class representing supported PTZ (Pan-Tilt-Zoom) information.
  ///   • Home: Property indicating whether home preset is supported.
  ///   • MaxPreset: Property representing the maximum preset value.
  ///   • ContinuousMode: Property indicating whether continuous mode is supported.
  ///   • AbsoluteMode: Property indicating whether absolute mode is supported.
  ///   • RelativeMode: Property indicating whether relative mode is supported.
  /// </summary>  
  TSupportedPTZInfo = class(TPersistent)
    private
    FOwnerPTZManager : TONVIFPTZManager;
    /// <summary>
    ///   Function to retrieve the maximum preset value.
    /// </summary>    
    function GetMaxPreset: Integer;
    
    /// <summary>
    ///   Function to check if absolute mode is supported.
    /// </summary>    
    function GetSupportedAbsoluteMode: Boolean;
    
    /// <summary>
    ///   Function to check if continuous mode is supported.
    /// </summary>    
    function GetSupportedContinuousMode: Boolean;

    /// <summary>
    ///   Function to check if home preset is supported.
    /// </summary>    
    function GetSupportedHome: Boolean;

    /// <summary>
    ///   Function to check if relative mode is supported.
    /// </summary>    
    function GetSupportedRelativeModeMode: Boolean;  
  public
    /// <summary>
    ///   Constructor for creating an instance of TSupportedPTZInfo.
    /// </summary>  
    constructor Create(aOwnerPTZManager : TONVIFPTZManager);
    
    /// <summary>
    ///   Property indicating whether home preset is supported.
    /// </summary>    
    property Home           : Boolean read GetSupportedHome;
    
    /// <summary>
    ///   Property representing the maximum preset value.
    /// </summary>
    property MaxPreset      : Integer read GetMaxPreset;
    
    /// <summary>
    ///   Property indicating whether continuous mode is supported.
    /// </summary>
    property ContinuousMode : Boolean read GetSupportedContinuousMode;        

    /// <summary>
    ///   Property indicating whether absolute mode is supported.
    /// </summary>    
    property AbsoluteMode   : Boolean read GetSupportedAbsoluteMode;  

    /// <summary>
    ///   Property indicating whether relative mode is supported.
    /// </summary>    
    property RelativeMode   : Boolean read GetSupportedRelativeModeMode; 
  end;  
    
  /// https://www.onvif.org/specs/srv/ptz/ONVIF-PTZ-Service-Spec-v1706.pdf?441d4a&441d4a
  ///
  /// <summary>    
  /// The PTZ model groups the possible movements of the PTZ unit into a Pan/Tilt component and 
  /// nto a Zoom component. To steer the PTZ unit, the service provides absolute move, relative
  /// ove and continuous move operations. Different coordinate systems and units are used to feed 
  /// hese operations.
  /// he PTZ service provides an AbsoluteMove operation to move the PTZ device to an absolute 
  /// osition. The service expects the absolute position as an argument referencing an absolute 
  /// oordinate system. The speed of the Pan/Tilt movement and the Zoom movement can be 
  /// pecified optionally. Speed values are positive scalars and do not contain any directional 
  /// nformation. It is not possible to specify speeds for Pan and Tilt separately without knowledge 
  /// bout the current position. This approach to specifying a desired position generally produces a 
  /// on-smooth and non-intuitive action.
  ///  RelativeMove operation is introduced by the PTZ service in order to steer the dome relative to 
  /// he current position, but without the need to know the current position. The operation expects a 
  /// ositional translation as an argument referencing a relative coordinate system. This 
  /// pecification distinguishes between relative and absolute coordinate systems, since there are 
  /// ases where no absolute coordinate system exists for a well-defined relative coordinate system. 
  /// An optional speed argument can be added to the RelativeMove operation with the same 
  /// meaning as for the AbsoluteMove operation.
  /// Finally, the PTZ device can be moved continuously via the ContinuousMove command in a 
  /// certain direction with a certain speed. Thereby, a velocity vector represents both, the direction 
  /// and the speed information. The latter is expressed by the length of the vector. 
  /// The Pan/Tilt and Zoom coordinates can be uniquely specified by augmenting the coordinates 
  /// with appropriate space URIs. A space URI uniquely represents the underlying coordinate system. 
  /// Section 5.7 defines a standard set of coordinate systems. A PTZ Node shall implement these 
  /// coordinate systems if the corresponding type of movement is supported by the PTZ Node. In 
  /// many cases, the Pan/Tilt position is represented by pan and tilt angles in a spherical coordinate 
  /// system. A digital PTZ, operating on a fixed megapixel camera, may express the camera’s 
  /// viewing direction by a pixel position on a static projection plane. Therefore, different coordinate 
  /// systems are needed in this case in order to capture the physical or virtual movements of the 
  /// PTZ device. Optionally, the PTZ Node may define its own device specific coordinate systems to 
  /// enable clients to take advantage of the specific properties of this PTZ Node.
  /// The PTZ Node description retrieved via the GetNode or GetNodes operation contains all 
  /// coordinate systems supported by a specific PTZ Node. Each coordinate system belongs to one 
  /// of the following groups:
  /// • AbsolutePanTiltPositionSpace
  /// • RelativePanTiltTranslationSpace
  /// • ContinuousPanTiltVelocitySpace
  /// • PanTiltSpeedSpace
  /// • AbsoluteZoomPositionSpace
  /// • RelativeZoomTranslationSpace
  /// • ContinuousZoomVelocitySpace
  /// • ZoomSpeedSpace
  /// If the PTZ node does not support the coordinate systems of a certain group, the corresponding 
  /// move operation will not be available for this PTZ node. For instance, if the list does not contain 
  /// an AbsolutePanTiltPositionSpace, the AbsoluteMove operation shall fail when an absolute 
  /// Pan/Tilt position is specified. The correspond
  /// </summary>
  TONVIFPTZManager = Class
  private
    FONVIFManager  : IONVIFManager;
    FPresentList   : TPTZPresetList;
    FToken         : String;    
    FPTZNode       : TPTZNode; 
    FSupportedInfo : TSupportedPTZInfo;
    FPTZStatus     : TPTZStatus;

    /// <summary>
    ///   Function to check the validity of a token for a given method name request.
    /// </summary>    
    function isValidToken(const aMathodNameRequest:String): Boolean;

    /// <summary>
    ///   Procedure to reset PTZ (Pan-Tilt-Zoom) node settings.
    /// </summary>    
    procedure ResetPTZNode;

    /// <summary>
    ///   Procedure to set a token value.
    /// </summary>    
    procedure SetToken(const Value: String);

    /// <summary>
    ///   Procedure to reset PTZ status.
    /// </summary>    
    procedure ResetPTZStatus;
  public
    /// <summary>
    ///   Constructor for initializing a new instance of the ONVIF PTZ Manager.
    /// </summary>
    /// <param name="aONVIFManager">Reference to the parent ONVIF manager instance.</param>  
    constructor Create(aONVIFManager : IONVIFManager);

    /// <summary>
    ///   Destructor for freeing resources associated with the ONVIF PTZ Manager.
    /// </summary>    
    Destructor Destroy;override;
    /// <summary>
    /// A PTZ-capable device shall implement this operation and return all PTZ nodes available on the 
    /// device.
    /// REQUEST:
    /// This is an empty message.
    /// RESPONSE:
    ///   • PTZNode - optional, unbounded [tt:PTZNode]
    /// List of the existing PTZ Nodes on the device
    /// </summary>
    /// <returns>
    ///   True if the operation successfully , False otherwise.</returns>  
    /// </returns>
    function GetNodes: Boolean;    

    /// <summary>
    ///   Property providing access to supported PTZ (Pan-Tilt-Zoom) information.
    /// </summary>    
    property SupportedInfo : TSupportedPTZInfo  read FSupportedInfo;
    
    {Preset}
    /// <summary>
    ///   Loads the list of presets associated with the PTZ functionality.
    /// </summary>
    /// <returns>True if the preset list is successfully loaded, False otherwise.</returns>
    function LoadPresetList: Boolean;    

    /// <summary>
    ///   The GotoPreset operation recalls a previously set preset. If the speed parameter is omitted, the 
    /// default speed of the corresponding PTZ configuration shall be used. The speed parameter can 
    /// only be specified when speed spaces are available for the PTZ node. The GotoPreset command 
    /// is a non-blocking operation and can be interrupted by other move commands. 
    /// REQUEST:
    /// • ProfileToken [tt:ReferenceToken]
    /// Reference to an existing media profile.
    /// • PresetToken [tt:ReferenceToken]
    /// Reference to an existing preset token.
    /// • Speed - optional [PTZSpeed]
    /// Optional speed.
    /// </summary>
    /// <param name="aIndexPreset">Index of the preset position to which the camera should move.</param>
    /// <returns>True if the camera successfully moves to the preset position, False otherwise.</returns>    
    function GoToPreset(const aIndexPreset : Integer) : Boolean;

    /// <summary>
    /// The RemovePreset operation removes a previously set preset.
    /// REQUEST:
    /// • ProfileToken [tt:ReferenceToken]
    /// Reference to an existing media profile.
    /// • PresetToken [tt:ReferenceToken]
    /// Existing preset token to be removed.
    /// </summary>
    /// <param name="aIndexPreset">Index of the preset position to be removed.</param>
    /// <returns>True if the preset is successfully removed, False otherwise.</returns>    
    function RemovePreset(const aIndexPreset : Integer) : Boolean;

    /// <summary>
    ///   The SetPreset command saves the current device position parameters so that the device can 
    ///   move to the saved preset position through the GotoPreset operation.
    ///   If the PresetToken parameter is absent, the device shall create a new preset. Otherwise it shall 
    ///   update the stored position and optionally the name of the given preset. If creation is successful, 
    ///   the response contains the PresetToken which uniquely identifies the preset. An existing preset 
    ///   can be overwritten by specifying the PresetToken of the corresponding preset. In both cases 
    ///   (overwriting or creation) an optional PresetName can be specified. The operation fails if the PTZ 
    ///   device is moving during the SetPreset operation. 
    ///   ONVIF™ – 17 – PTZ Spec. – Ver. 17.12
    ///   The device MAY internally save additional states such as imaging properties in the PTZ preset 
    ///   which then should be recalled in the GotoPreset operation. A device shall accept a valid 
    ///   SetPresetRequest that does not include the optional element PresetName.
    ///   Devices may require unique preset names and reject a request that contains an already existing 
    ///   PresetName by responding with the error message ter:PresetExist.
    ///   REQUEST:
    ///   • ProfileToken [tt:ReferenceToken]
    ///   Reference to an existing media profile.
    ///   • PresetToken - optional [tt:ReferenceToken]
    ///   Optional existing preset token to update a preset position.
    ///   • PresetName - optional [xs:string]
    ///   Optional name to be assigned to the preset position.
    ///   RESPONSE:
    ///   • PresetToken [tt:ReferenceToken]
    ///   Reference token assigned by the device to the prese
    /// <param name="aPresetName">Name o preset</param>
    /// <param name="aIndexExistsPreset">Index of the preset if the  parameter is egual to -1, the device shall create a new preset</param>
    /// <param name="aNewIndexPreset">New index of preset in internal list aIndexExistsPreset is egual to -1 otherwise the token of preset retrive by aIndexExistsPreset</param>
    /// <returns>True if the preset is successfully removed, False otherwise.</returns>      
    function SetPreset(const aPresetName:String;var aNewIndexPreset:Integer;aIndexExistsPreset: Integer=-1):Boolean;

    /// <summary>
    ///   Gets the list of PTZ presets associated with the PTZ manager.
    /// </summary>
    /// <remarks>
    ///   The <c>PresetList</c> property provides access to the list of PTZ presets associated with the
    ///   PTZ manager. PTZ presets represent specific camera positions that can be saved and recalled
    ///   using the PTZ functionality. The property allows the application to interact with and manage
    ///   the list of available PTZ presets.
    /// </remarks>    
    property PresetList : TPTZPresetList read FPresentList;

    {PTZ}
    /// <summary>    
    ///	A PTZ-capable device shall be able to report its PTZ status through the GetStatus command. 
    ///	The PTZ status contains the following information:
    ///	• Position (optional) – Specifies the absolute position of the PTZ unit together with the 
    ///	space references. The default absolute spaces of the corresponding PTZ configuration 
    ///	shall be referenced within the position element. This information shall be present if the 
    ///	device signals support via the capability StatusPosition.
    ///	• MoveStatus (optional) – Indicates if the Pan/Tilt/Zoom device unit is currently moving, idle 
    ///	or in an unknown state. This information shall be present if the device signals support 
    ///	via the capability MoveStatus. The state Unknown shall not be used during normal 
    ///	operation, but is reserved to initialization or error conditions.
    ///	• Error (optional) – States a current PTZ error condition. This field shall be present if the 
    ///	MoveStatus signals Unkown.
    ///	• UTC Time – Specifies the UTC time when this status was generated.
    /// </summary>   
    function GetStatus:Boolean;
    /// <summary>

    /// </summary>
    /// <param name="aInZoom">
    ///   Indicates whether it is an "in" zoom operation (True) or "out" zoom operation (False).
    /// </param>
    /// <returns>
    ///   True if the PTZ start zoom operation is executed successfully; False otherwise.
    /// </returns>
    function Zoom(aInZoom: Boolean): Boolean;
    
    /// <summary>
    /// Absolute movements
    ///	If a PTZ node supports absolute Pan/Tilt or absolute Zoom movements, it shall support the
    ///	AbsoluteMove operation. The position argument of this command specifies the absolute position 
    ///	to which the PTZ unit moves. It splits into an optional Pan/Tilt element and an optional Zoom 
    ///	element. If the Pan/Tilt position is omitted, the current Pan/Tilt movement shall NOT be affected 
    ///	by this command. The same holds for the zoom position. 
    ///	The spaces referenced within the position shall be absolute position spaces supported by the 
    ///	PTZ node. If the space information is omitted, the corresponding default spaces of the PTZ 
    ///	configuration, a part of the specified Media Profile, is used. A device may support absolute 
    ///	Pan/Tilt movements, absolute Zoom movements or no absolute movements by providing only 
    ///	absolute position spaces for the supported cases.
    ///	An existing Speed argument overrides the DefaultSpeed of the corresponding PTZ configuration 
    ///	during movement to the requested position. If spaces are referenced within the Speed argument, 
    ///	they shall be Speed Spaces supported by the PTZ Node.
    ///	The operation shall fail if the requested absolute position is not reachable
    function StartMoveAbsolute(const aPan,aTilt,aZoom : Double ): Boolean; 

    /// <summary>
    /// continuous movements
    ///   A PTZ-capable device shall support continuous movements. The velocity argument of this 
    ///   command specifies a signed speed value for the Pan, Tilt and Zoom. The combined Pan/Tilt 
    ///   element is optional and the Zoom element itself is optional. If the Pan/Tilt element is omitted, 
    ///   the current Pan/Tilt movement shall NOT be affected by this command. The same holds for the 
    ///   Zoom element. The spaces referenced within the velocity element shall be velocity spaces 
    ///   supported by the PTZ Node. If the space information is omitted for the velocity argument, the 
    ///   corresponding default spaces of the PTZ configuration belonging to the specified Media Profile 
    ///   is used. A device MAY support continuous Pan/Tilt movements and/or continuous Zoom 
    ///   movements by providing only velocity spaces for the supported cases.
    ///   An existing timeout argument overrides the DefaultPTZTimeout parameter of the corresponding 
    ///   PTZ configuration for this Move operation. The timeout parameter specifies how long the PTZ 
    ///   node continues to move.
    ///   A device shall stop movement in a particular axis (Pan, Tilt, or Zoom) when zero is sent as the 
    ///   ContinuousMove parameter for that axis. Stopping shall have the same effect independent of 
    ///   the velocity space referenced. This command has the same effect on a continuous move as the 
    ///   stop command specified in section 5.3.5.
    ///   If the requested velocity leads to absolute positions which cannot be reached, the PTZ node 
    ///   shall move to a reachable position along the border of its range. A typical application of the 
    ///   continuous move operation is controlling PTZ via joystick.
    /// 
    /// REQUEST:
    /// • ProfileToken [tt:ReferenceToken]
    /// Reference to an existing media profile.
    /// • Velocity [tt:PTZSpeed]
    /// Speed vector specifying the velocity of pan, tilt and zoom.
    /// • Timeout - optional [xs:duration]
    /// Optional timeout.
    /// </summary>
    /// <param name="aDirection">
    ///   The type of PTZ move command to be executed.
    /// </param>
    /// <returns>
    ///   True if the PTZ move operation is executed successfully; False otherwise.
    /// </returns>
    function StartMoveContinuous(const aDirection: TONVIF_PTZ_CommandType;aTimeoutSec : integer = 0 ): Boolean; 

    /// <summary>
    /// A PTZ-capable device shall support the stop operation. If no stop filter arguments are present, 
    /// this command stops all ongoing pan, tilt and zoom movements. The stop operation can be 
    /// filtered to stop a specific movement by setting the corresponding stop argument.
    /// REQUEST:
    /// • ProfileToken [tt:ReferenceToken]
    /// Reference to an existing media profile.
    /// • PanTilt - optional [xs:boolean]
    /// Stop pan and tilt operation (defaults to true).
    /// • Zoom - optional [xs:boolean]
    /// Stop zoom operation (defaults to true)
    /// </summary>
    /// <param name="aResultStr">
    ///   Returns the result string after stopping the PTZ move operation.
    /// </param>
    /// <returns>
    ///   True if the PTZ move operation is successfully stopped; False otherwise.
    /// </returns>
    function Stop: Boolean; 

    /// <summary>
    ///   This operation moves the PTZ unit to its home position. If the speed parameter is omitted, the 
    ///   default speed of the corresponding PTZ configuration shall be used. The speed parameter can 
    ///   only be specified when speed spaces are available for the PTZ node.The command is nonblocking and can be interrupted by other move commands.
    ///   REQUEST:
    ///   • ProfileToken [tt:ReferenceToken]
    ///   Reference to an existing media profile.
    ///   • Speed - optional [PTZSpeed]
    ///   Optional speed    
    /// </summary>
    /// <returns>
    ///   True if the PTZ move operation is successfully ; False otherwise.
    /// </returns>
    function GotoHomePosition: Boolean;
    
    /// <summary>   
    /// The SetHome operation saves the current position parameters as the home position, so that the 
    /// GotoHome operation can request that the device move to the home position.
    /// The SetHomePosition command shall return with a failure if the “home” position is fixed and 
    /// cannot be overwritten. If the SetHomePosition is successful, it shall be possible to recall the 
    /// home position with the GotoHomePosition command.
    /// REQUEST:
    /// • ProfileToken [tt:ReferenceToken]
    /// Reference to an existing media profile 
    /// </summary>
    /// <returns>
    ///   True if the PTZ move operation is successfully ; False otherwise.
    /// </returns>
    function SetHomePosition:Boolean;
    
    /// <summary>
    ///   Property providing access to PTZ (Pan-Tilt-Zoom) node settings.
    /// </summary>    
    property PTZNode                 : TPTZNode            read FPTZNode;        
    
    /// <summary>
    ///   Gets or sets the token of the ONVIF camera.
    /// </summary>    
    property Token                   : String              read FToken                   write SetToken;    
  End;
    

implementation

{ TONVIFPTZManager }
constructor TONVIFPTZManager.Create(aONVIFManager: IONVIFManager);
begin
  FONVIFManager := aONVIFManager;
  FPresentList  := TPTZPresetList.Create;
  FSupportedInfo:= TSupportedPTZInfo.Create(self);
  ResetPTZNode;
  ResetPTZStatus;
end;

destructor TONVIFPTZManager.Destroy;
begin
  ResetPTZStatus;
  ResetPTZNode;
  FreeAndNil(FSupportedInfo);
  FreeAndNil(FPresentList);
  inherited;
end;

function TONVIFPTZManager.isValidToken(const aMathodNameRequest:String): Boolean;
begin
  Result := False;
  if FToken.Trim.IsEmpty then
  begin
    FONVIFManager.SetLastStatusCode(aMathodNameRequest,ONVIF_ERROR_PTZ_TOKEN_IS_EMPTY);
    Exit;
  end;
  Result := True;  
end;

function TONVIFPTZManager.StartMoveAbsolute(const aPan, aTilt, aZoom: Double): Boolean;
begin
  Result := False;
  if not isValidToken('TONVIFPTZManager.StartMoveContinuous') then Exit;

  if not SupportedInfo.GetSupportedAbsoluteMode then
  begin
    FONVIFManager.SetLastStatusCode('TONVIFPTZManager.StartMoveContinuous',ONVIF_ERROR_PTZ_MOVE_ABSOLUTE_NOT_SUPPORTED);
    Exit;
  end;
  raise Exception.Create('not implemented');
  {
  TODO Check range
  How check ? on all Node of array ?
  FPTZNode.SupportedPTZSpaces.AbsolutePanTiltPositionSpace
  }
  
//s  Result := FONVIFManager.ExecuteRequest(FONVIFManager.GetUrlByType(atPtz), FONVIFManager.GetSOAPBuilder.PrepareStartMoveContinuousRequest(FToken,LCommandStr), LResultStr);      

end;

function TONVIFPTZManager.StartMoveContinuous(const aDirection: TONVIF_PTZ_CommandType;aTimeoutSec : integer = 0): Boolean;
var LCommandStr: String;  
    LResultStr : String;                                                  
begin
  Result := False;
  if not isValidToken('TONVIFPTZManager.StartMoveContinuous') then Exit;

  if not SupportedInfo.GetSupportedContinuousMode then
  begin
    FONVIFManager.SetLastStatusCode('TONVIFPTZManager.StartMoveContinuous',ONVIF_ERROR_PTZ_MOVE_CONTINUOUS_NOT_SUPPORTED);
    Exit;
  end;
  
  case aDirection of
    opcTop          : LCommandStr := Format('x="0" y="0.%d"',[FONVIFManager.Speed]);
    opcBotton       : LCommandStr := Format('x="0" y="-0.%d"',[FONVIFManager.Speed]);
    opcRight        : LCommandStr := Format('x="0.%d" y="0"',[FONVIFManager.Speed]);  
    opcLeft         : LCommandStr := Format('x="-0.%d" y="0"',[FONVIFManager.Speed]);                                 
    opcTopRight     : LCommandStr := Format('x="0.%d" y="0.%d"',[FONVIFManager.Speed,FONVIFManager.Speed]);                                  
    opcTopLeft      : LCommandStr := Format('x="-0.%d" y="0.%d"',[FONVIFManager.Speed,FONVIFManager.Speed]);                                  
    opcBottonLeft   : LCommandStr := Format('x="-0.%d" y="-0.%d"',[FONVIFManager.Speed,FONVIFManager.Speed]);
    opcBottonRight  : LCommandStr := Format('x="0.%d" y="-0.%d"',[FONVIFManager.Speed,FONVIFManager.Speed]);                                  
  end;

  //TODO aTimeoutSec , call GetConfigurationOptions for range within which timeouts are accepted by the PTZ node
  Result := FONVIFManager.ExecuteRequest(atPtz,'TONVIFPTZManager.StartMoveContinuous', FONVIFManager.GetSOAPBuilder.PrepareStartMoveContinuousRequest(FToken,LCommandStr), LResultStr);      
end;

function TONVIFPTZManager.Zoom(aInZoom: Boolean): Boolean;
var LCommand   : String;
    LResultStr : String; 
begin
  Result := False;
  if not isValidToken('TONVIFPTZManager.Zoom') then Exit;

  if not SupportedInfo.GetSupportedContinuousMode then
  begin
    FONVIFManager.SetLastStatusCode('TONVIFPTZManager.Zoom',ONVIF_ERROR_PTZ_MOVE_CONTINUOUS_NOT_SUPPORTED);
    Exit;
  end;  

  if aInZoom then
     LCommand := Format('x="-0.%d"',[FONVIFManager.Speed])
  else
     LCommand := Format('x="0.%d"',[FONVIFManager.Speed]);
        
  Result := FONVIFManager.ExecuteRequest(atPtz,'TONVIFPTZManager.Zoom',FONVIFManager.GetSOAPBuilder.PrepareStartZoomRequest(FToken,LCommand), LResultStr);      
end;

function TONVIFPTZManager.Stop: Boolean;
var LResultStr: String;
begin
  Result := false;
  if not isValidToken('TONVIFPTZManager.Stop') then Exit;
  Result := FONVIFManager.ExecuteRequest(atPtz,'TONVIFPTZManager.Stop',FONVIFManager.GetSOAPBuilder.PrepareStopMoveRequest(FToken), LResultStr);
end;

function TONVIFPTZManager.LoadPresetList : Boolean;
var LResponseStr    : String;
    LXMLDoc         : IXMLDocument;
    LSoapBodyNode   : IXMLNode;
    LPresetListNode : IXMLNode;
    LPtzPosNode     : IXMLNode;
    LPanTiltNode    : IXMLNode;    
    LPreset         : PTPTZPreset;
    I               : Integer;
    LErrorFound     : Boolean;
begin
  FPresentList.Clear;
  Result := False;
  if not isValidToken('TONVIFPTZManager.LoadPresetList') then Exit;

  Result := FONVIFManager.ExecuteRequest(atPTZ,'TONVIFPTZManager.LoadPresetList',FONVIFManager.GetSOAPBuilder.PrepareGetPresetList(FToken),LResponseStr);

  if Result then
  begin
    {$REGION 'Log'}
    {TSI:IGNORE ON}
        FONVIFManager.DoWriteLog('TONVIFPTZManager.LoadPresetList',LResponseStr,tpLivXMLResp,true);      
    {TSI:IGNORE OFF}
    {$ENDREGION}
    LXMLDoc := TXMLDocument.Create(nil);
    LXMLDoc.LoadFromXML(LResponseStr);

    if not FONVIFManager.IsValidSoapXML(LXMLDoc.DocumentElement,LErrorFound) then exit;
    
    LSoapBodyNode   := TONVIFXMLUtils.GetSoapBody(LXMLDoc.DocumentElement);        
    LPresetListNode := TONVIFXMLUtils.RecursiveFindNode(LSoapBodyNode,'GetPresetsResponse');  

    for I := 0 to LPresetListNode.ChildNodes.Count -1 do
    begin
      New(LPreset);
      LPreset^.Token := String(LPresetListNode.ChildNodes[I].Attributes['token']);
      LPreset^.Name  := TONVIFXMLUtils.GetChildNodeValue(LPresetListNode.ChildNodes[I],'Name');     
      LPtzPosNode    := TONVIFXMLUtils.RecursiveFindNode(LPresetListNode.ChildNodes[I],'PTZPosition');
      if Assigned(LPtzPosNode) then
      begin
         LPreset^.PTZPosition.Zoom := StrToFloatLocale(TONVIFXMLUtils.GetAttribute(TONVIFXMLUtils.RecursiveFindNode(LPtzPosNode,'Zoom'),'x'),-1);
         LPanTiltNode              := TONVIFXMLUtils.RecursiveFindNode(LPtzPosNode,'PanTilt');
         if Assigned(LPanTiltNode) then
         begin
            LPreset^.PTZPosition.PanTilt.X := StrToFloatLocale(TONVIFXMLUtils.GetAttribute(LPanTiltNode,'x'),-1);
            LPreset^.PTZPosition.PanTilt.y := StrToFloatLocale(TONVIFXMLUtils.GetAttribute(LPanTiltNode,'y'),-1);            
         end;         
      end;

      FPresentList.Add(LPreset)
    end;
    {$REGION 'Log'}
    {TSI:IGNORE ON}
        FONVIFManager.DoWriteLog('TONVIFPTZManager.LoadPresetList',Format(' Found [%d] preset ',[FPresentList.Count]),tpLivInfo,True);      
    {TSI:IGNORE OFF}
    {$ENDREGION}
  end;
end;

function TONVIFPTZManager.GetNodes: Boolean;
var LResponseStr   : String;
    LXMLDoc        : IXMLDocument;
    LSoapBodyNode  : IXMLNode;
    LNodes         : IXMLNode;
    LNodesList     : IXMLNode;
    LNodeTmp       : IXMLNode;
    I              : Integer;
    X              : Integer;
    Z              : Integer;
    LCountAux      : Integer;
    LCountAuxValue : Integer;
    LFoundAux      : Boolean;
    LTmpStr        : string;  
    LTmpStrSplit   : string;
    LErrorFound    : Boolean;

    Procedure BuildArrayXY(aParentNode: IXMLNode;var aArrayXY: TArrayXY);
    var LCount           : Integer;
        LNodeRangeBuild  : IXMLNode;    
    begin
      LCount := Length(aArrayXY);
      SetLength(aArrayXY,LCount+1);

      aArrayXY[LCount].URI :=  TONVIFXMLUtils.GetChildNodeValue(aParentNode,'URI');
      LNodeRangeBuild      := TONVIFXMLUtils.RecursiveFindNode(aParentNode,'XRange');
      if Assigned(LNodeRangeBuild) then
      begin
        aArrayXY[LCount].XRange.Min := StrToFloatLocale(TONVIFXMLUtils.GetChildNodeValue(LNodeRangeBuild,'Min'),-1);
        aArrayXY[LCount].XRange.Max := StrToFloatLocale(TONVIFXMLUtils.GetChildNodeValue(LNodeRangeBuild,'Max'),-1);
      end;
          
      LNodeRangeBuild := TONVIFXMLUtils.RecursiveFindNode(aParentNode,'YRange');          
      if Assigned(LNodeRangeBuild) then
      begin
        aArrayXY[LCount].YRange.Min := StrToFloatLocale(TONVIFXMLUtils.GetChildNodeValue(LNodeRangeBuild,'Min'),-1);
        aArrayXY[LCount].YRange.Max := StrToFloatLocale(TONVIFXMLUtils.GetChildNodeValue(LNodeRangeBuild,'Max'),-1);
      end;     
    end;

    Procedure BuildArrayX(aParentNode: IXMLNode;var aArrayX: TArrayX);
    var LCount           : Integer;
        LNodeRangeBuild  : IXMLNode;    
    begin
      LCount := Length(aArrayX);
      SetLength(aArrayX,LCount+1);

      aArrayX[LCount].URI :=  TONVIFXMLUtils.GetChildNodeValue(aParentNode,'URI');
      LNodeRangeBuild     := TONVIFXMLUtils.RecursiveFindNode(aParentNode,'XRange');
      if Assigned(LNodeRangeBuild) then
      begin
        aArrayX[LCount].XRange.Min := StrToFloatLocale(TONVIFXMLUtils.GetChildNodeValue(LNodeRangeBuild,'Min'),-1);
        aArrayX[LCount].XRange.Max := StrToFloatLocale(TONVIFXMLUtils.GetChildNodeValue(LNodeRangeBuild,'Max'),-1);
      end;
      
    end;    
begin
  Result := False;  
  ResetPTZNode;
  if not isValidToken('TONVIFPTZManager.GetNodes') then Exit;

  Result := FONVIFManager.ExecuteRequest(atPTZ,'TONVIFPTZManager.GetNodes',FONVIFManager.GetSOAPBuilder.PrepareGetNodes,LResponseStr);  

  if Result then
  begin
    Result := False;  
    {$REGION 'Log'}
    {TSI:IGNORE ON}
        FONVIFManager.DoWriteLog('TONVIFPTZManager.GetNodes',LResponseStr,tpLivXMLResp,true);      
    {TSI:IGNORE OFF}
    {$ENDREGION}
    LXMLDoc := TXMLDocument.Create(nil);
    LXMLDoc.LoadFromXML(LResponseStr);

    if not FONVIFManager.IsValidSoapXML(LXMLDoc.DocumentElement,LErrorFound) then exit;
    
    LSoapBodyNode   := TONVIFXMLUtils.GetSoapBody(LXMLDoc.DocumentElement);
    LNodes          := TONVIFXMLUtils.RecursiveFindNode(LSoapBodyNode,'GetNodesResponse');
    if not Assigned(LNodes) then Exit;

    for I := 0 to LNodes.ChildNodes.Count -1 do
    begin
      FPTZNode.Token             := String(LNodes.ChildNodes[I].Attributes['token']);
      FPTZNode.FixedHomePosition := StrToBoolDef(TONVIFXMLUtils.GetAttribute(LNodes.ChildNodes[I],'FixedHomePosition'),False);
      FPTZNode.GeoMove           := StrToBoolDef(TONVIFXMLUtils.GetAttribute(LNodes.ChildNodes[I],'GeoMove'),False); 
      LNodesList                 := TONVIFXMLUtils.RecursiveFindNode(LNodes.ChildNodes[I],'SupportedPTZSpaces');

      if not Assigned(LNodesList) then
      begin
        {$REGION 'Log'}
        {TSI:IGNORE ON}
            FONVIFManager.DoWriteLog('TONVIFPTZManager.GetNodes','SupportedPTZSpaces not found',tpLivError);      
        {TSI:IGNORE OFF}
        {$ENDREGION}
        continue;
      end;
      
      for X := 0 to LNodesList.ChildNodes.Count -1 do
      begin
                                            
        if SameText(LNodesList.ChildNodes[X].DOMNode.localName,'AbsolutePanTiltPositionSpace') then
          BuildArrayXY(LNodesList.ChildNodes[X],FPTZNode.SupportedPTZSpaces.AbsolutePanTiltPositionSpace)
        else if SameText(LNodesList.ChildNodes[X].DOMNode.localName,'AbsoluteZoomPositionSpace') then
          BuildArrayX(LNodesList.ChildNodes[X],FPTZNode.SupportedPTZSpaces.AbsoluteZoomPositionSpace)
        else if SameText(LNodesList.ChildNodes[X].DOMNode.localName,'RelativePanTiltTranslationSpace') then
          BuildArrayXY(LNodesList.ChildNodes[X],FPTZNode.SupportedPTZSpaces.RelativePanTiltTranslationSpace)
        else if SameText(LNodesList.ChildNodes[X].DOMNode.localName,'RelativeZoomTranslationSpace') then
          BuildArrayX(LNodesList.ChildNodes[X],FPTZNode.SupportedPTZSpaces.RelativeZoomTranslationSpace)
        else if SameText(LNodesList.ChildNodes[X].DOMNode.localName,'ContinuousPanTiltVelocitySpace') then
          BuildArrayXY(LNodesList.ChildNodes[X],FPTZNode.SupportedPTZSpaces.ContinuousPanTiltVelocitySpace)
        else if SameText(LNodesList.ChildNodes[X].DOMNode.localName,'ContinuousZoomVelocitySpace') then
          BuildArrayX(LNodesList.ChildNodes[X],FPTZNode.SupportedPTZSpaces.ContinuousZoomVelocitySpace)
        else if SameText(LNodesList.ChildNodes[X].DOMNode.localName,'PanTiltSpeedSpace')then
        begin
          FPTZNode.SupportedPTZSpaces.PanTiltSpeedSpace.URI := TONVIFXMLUtils.GetChildNodeValue(LNodesList.ChildNodes[X],'URI');
          LNodeTmp                                          := TONVIFXMLUtils.RecursiveFindNode(LNodesList.ChildNodes[X],'XRange');
          if Assigned(LNodeTmp) then
          begin
            FPTZNode.SupportedPTZSpaces.PanTiltSpeedSpace.XRange.Min := StrToFloatLocale(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp,'Min'),-1);
            FPTZNode.SupportedPTZSpaces.PanTiltSpeedSpace.XRange.Max := StrToFloatLocale(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp,'Max'),-1);
          end;        
        end                                
        else if SameText(LNodesList.ChildNodes[X].DOMNode.localName,'ZoomSpeedSpace') then
        begin
          FPTZNode.SupportedPTZSpaces.ZoomSpeedSpace.URI := TONVIFXMLUtils.GetChildNodeValue(LNodesList.ChildNodes[X],'URI');
          LNodeTmp                                       := TONVIFXMLUtils.RecursiveFindNode(LNodesList.ChildNodes[X],'XRange');
          if Assigned(LNodeTmp) then
          begin
            FPTZNode.SupportedPTZSpaces.ZoomSpeedSpace.XRange.Min := StrToFloatLocale(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp,'Min'),-1);
            FPTZNode.SupportedPTZSpaces.ZoomSpeedSpace.XRange.Max := StrToFloatLocale(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp,'Max'),-1);
          end;           
        end
        else  
          {$REGION 'Log'}
          {TSI:IGNORE ON}
              FONVIFManager.DoWriteLog('TONVIFPTZManager.GetNodes',Format('Unsupported node name [%s]',[LNodesList.ChildNodes[X].DOMNode.localName]),tpLivWarning);      
          {TSI:IGNORE OFF}
          {$ENDREGION}                                                
      end;
      FPTZNode.MaximumNumberOfPresets := StrToIntDef(TONVIFXMLUtils.GetChildNodeValue(LNodes.ChildNodes[I],'MaximumNumberOfPresets'),-1);  
      FPTZNode.HomeSupported          := StrToBoolDef(TONVIFXMLUtils.GetChildNodeValue(LNodes.ChildNodes[I],'HomeSupported'),False); 
      LTmpStr                         := LNodes.ChildNodes[i].DOMNode.localName;

      for X := 0 to LNodes.ChildNodes[i].ChildNodes.Count -1 do
      begin  
        if SameText(LNodes.ChildNodes[i].ChildNodes[X].DOMNode.localName,'AuxiliaryCommands') then
        begin
          LFoundAux    := False;        
          LTmpStr      := LNodes.ChildNodes[i].ChildNodes[X].Text.Replace('tt:',String.Empty,[rfIgnoreCase]);
          if not LTmpStr.Trim.IsEmpty then
          begin
            LTmpStrSplit := LTmpStr;
            LCountAux    := Length(FPTZNode.AuxiliaryCommands);

            if LTmpStr.Contains('|') then
              LTmpStrSplit := LTmpStr.Split(['|'])[0];
            
            for Z := 0 to LCountAux -1 do
            begin
              if SameText(FPTZNode.AuxiliaryCommands[Z].Name,LTmpStrSplit) then
              begin
                LFoundAux      := True;
                LCountAuxValue := Length(FPTZNode.AuxiliaryCommands[Z].Values);
                SetLength(FPTZNode.AuxiliaryCommands[Z].Values,LCountAuxValue+1);
                if LTmpStr.Contains('|') then            
                  FPTZNode.AuxiliaryCommands[Z].Values[LCountAuxValue] := LTmpStr.Split(['|'])[1]
                else
                  FPTZNode.AuxiliaryCommands[Z].Values[LCountAuxValue] := LTmpStrSplit;
                Break;
              end
            end;
          
            if not LFoundAux then
            begin
              SetLength(FPTZNode.AuxiliaryCommands,LCountAux+1); 
              FPTZNode.AuxiliaryCommands[LCountAux].Name := LTmpStrSplit;
                         
              SetLength(FPTZNode.AuxiliaryCommands[LCountAux].Values,1);        
              if LTmpStr.Contains('|') then            
                FPTZNode.AuxiliaryCommands[LCountAux].Values[0] :=  LTmpStr.Split(['|'])[1]              
              else
                FPTZNode.AuxiliaryCommands[LCountAux].Values[0]:= LTmpStr;        
            end;
          end
        end;
      end;

      LNodesList := TONVIFXMLUtils.RecursiveFindNode(LNodes.ChildNodes[I],'Extension');
      if Assigned(LNodesList) then
      begin
        LNodeTmp := TONVIFXMLUtils.RecursiveFindNode(LNodes.ChildNodes[I],'SupportedPresetTour'); 
        for Z := 0 to LNodesList.ChildNodes.Count -1 do
        begin
          if not SameText(LNodesList.ChildNodes[Z].DOMNode.localName,'SupportedPresetTour') then
            {$REGION 'Log'}
            {TSI:IGNORE ON}
                FONVIFManager.DoWriteLog('TONVIFPTZManager.GetCapabilities',Format('Unsupported node name [%s]',[LNodesList.ChildNodes[X].DOMNode.localName]),tpLivWarning);      
            {TSI:IGNORE OFF}
            {$ENDREGION}                                                
        end;            
             
        if Assigned(LNodeTmp) then
        begin
          FPTZNode.Extension.SupportedPresetTour.MaximumNumberOfPresetTours := StrToIntDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp,'MaximumNumberOfPresetTours'),-1);
          
          for X := 0 to LNodeTmp.ChildNodes.Count -1 do
          begin
            if SameText(LNodeTmp.ChildNodes[X].DOMNode.localName,'PTZPresetTourOperation') then
            begin
              LCountAux := Length(FPTZNode.Extension.SupportedPresetTour.PTZPresetTourOperation);
              SetLength(FPTZNode.Extension.SupportedPresetTour.PTZPresetTourOperation,LCountAux+1);
              FPTZNode.Extension.SupportedPresetTour.PTZPresetTourOperation[LCountAux] := LNodeTmp.ChildNodes[X].Text; 
            end 
            else if not SameText(LNodeTmp.ChildNodes[X].DOMNode.localName,'MaximumNumberOfPresetTours') then                 
              {$REGION 'Log'}
              {TSI:IGNORE ON}
                  FONVIFManager.DoWriteLog('TONVIFPTZManager.GetCapabilities',Format('Unsupported node name [%s]',[LNodeTmp.ChildNodes[X].DOMNode.localName]),tpLivWarning);      
              {TSI:IGNORE OFF}
              {$ENDREGION}              
          end
        end;
      end            
    end;
      
    Result := True;     
  end
  else
    {$REGION 'Log'}
    {TSI:IGNORE ON}
        FONVIFManager.WriteLastErrorCodeLog('TONVIFManager.GetNodes')
    {TSI:IGNORE OFF}
    {$ENDREGION}        
end;

function TONVIFPTZManager.GetStatus: Boolean;
var LResponseStr       : String;
    LXMLDoc            : IXMLDocument;
    LSoapBodyNode      : IXMLNode;
    LPTZStatusNode     : IXMLNode;
    LErrorFound        : Boolean;
    I                  : Integer;
begin
  Result := False;  
  ResetPTZStatus;
  if not isValidToken('TONVIFPTZManager.GetStatus') then Exit;

  Result := FONVIFManager.ExecuteRequest(atPTZ,'TONVIFPTZManager.GetStatus',FONVIFManager.GetSOAPBuilder.PrepareGetStatus(FToken),LResponseStr);
  if Result then
  begin
    Result := False;  
    {$REGION 'Log'}
    {TSI:IGNORE ON}
        FONVIFManager.DoWriteLog('TONVIFPTZManager.GetStatus',LResponseStr,tpLivXMLResp,true);      
    {TSI:IGNORE OFF}
    {$ENDREGION}
    LXMLDoc := TXMLDocument.Create(nil);
    LXMLDoc.LoadFromXML(LResponseStr);

    if not FONVIFManager.IsValidSoapXML(LXMLDoc.DocumentElement,LErrorFound) then exit;
    
    LSoapBodyNode   := TONVIFXMLUtils.GetSoapBody(LXMLDoc.DocumentElement);
    LPTZStatusNode  := TONVIFXMLUtils.RecursiveFindNode(LSoapBodyNode,'PTZStatus');

    for I := 0 to LPTZStatusNode.ChildNodes.Count -1 do
    begin
      if SameText(LPTZStatusNode.ChildNodes[I].DOMNode.localName,'Position') then
      begin
         FPTZStatus.PTZPosition.PanTilt.X := StrToFloatLocale(TONVIFXMLUtils.GetAttribute(TONVIFXMLUtils.RecursiveFindNode(LPTZStatusNode.ChildNodes[I],'PanTilt'),'x'),-1);
         FPTZStatus.PTZPosition.PanTilt.Y := StrToFloatLocale(TONVIFXMLUtils.GetAttribute(TONVIFXMLUtils.RecursiveFindNode(LPTZStatusNode.ChildNodes[I],'PanTilt'),'x'),-1);
      end
      else if SameText(LPTZStatusNode.ChildNodes[I].DOMNode.localName,'MoveStatus') then
      begin
         FPTZStatus.MoveStatus.PanTilt := TONVIFXMLUtils.GetChildNodeValue(LPTZStatusNode.ChildNodes[I],'PanTilt');
         FPTZStatus.MoveStatus.Zoom    := TONVIFXMLUtils.GetChildNodeValue(LPTZStatusNode.ChildNodes[I],'Zoom');
      end
      else if SameText(LPTZStatusNode.ChildNodes[I].DOMNode.localName,'Error') then
      begin
        FPTZStatus.Error := LPTZStatusNode.ChildNodes[I].Text; 
      end
      else if SameText(LPTZStatusNode.ChildNodes[I].DOMNode.localName,'UtcTime') then
      begin
        if not LPTZStatusNode.ChildNodes[I].Text.Trim.IsEmpty then
          FPTZStatus.UtcTime := XMLTimeToDateTime(LPTZStatusNode.ChildNodes[I].Text,True)
      end
      else
        {$REGION 'Log'}
        {TSI:IGNORE ON}
            FONVIFManager.DoWriteLog('TONVIFPTZManager.GetStatus',Format('Unsupported node name [%s]',[LPTZStatusNode.ChildNodes[I].DOMNode.localName]),tpLivWarning);      
        {TSI:IGNORE OFF}
        {$ENDREGION}                                                
    end;      
  end
  else
    {$REGION 'Log'}
    {TSI:IGNORE ON}
        FONVIFManager.WriteLastErrorCodeLog('TONVIFManager.GetStatus');
    {TSI:IGNORE OFF}
    {$ENDREGION}        
end;

function TONVIFPTZManager.GotoHomePosition: Boolean;
var LResponseStr : String;
begin
  Result := False;  
  if not SupportedInfo.Home then
  begin
    FONVIFManager.SetLastStatusCode('TONVIFPTZManager.GotoHomePosition',ONVIF_ERROR_PTZ_HOME_COMMAND_NOT_SUPPORTED);
    Exit;
  end;
  
  if not isValidToken('TONVIFPTZManager.GotoHomePosition') then Exit;

  Result := FONVIFManager.ExecuteRequest(atPTz,'TONVIFPTZManager.GotoHomePosition',FONVIFManager.GetSOAPBuilder.PrepareGotoHome(FToken),LResponseStr);
end;

function TONVIFPTZManager.SetHomePosition: Boolean;
var LResponseStr : String;
begin
  Result := False;  
  if not SupportedInfo.Home then
  begin
    FONVIFManager.SetLastStatusCode('TONVIFPTZManager.SetHomePosition',ONVIF_ERROR_PTZ_HOME_COMMAND_NOT_SUPPORTED);
    Exit;
  end;

  if not isValidToken('TONVIFPTZManager.GotoHomePosition') then Exit;
  Result := FONVIFManager.ExecuteRequest(atPTZ,'TONVIFPTZManager.GotoHomePosition',FONVIFManager.GetSOAPBuilder.PrepareSetHomePosition(FToken),LResponseStr);
end;

function TONVIFPTZManager.GoToPreset(const aIndexPreset: Integer) : Boolean;
var LResponseStr : String;
begin
  Result := False;  
  if not isValidToken('TONVIFPTZManager.GoToPreset') then Exit;
  
  if not inRange(aIndexPreset,0,FPresentList.Count -1) then
  begin
    FONVIFManager.SetLastStatusCode('TONVIFPTZManager.GoToPreset',ONVIF_ERROR_PTZ_INVALID_PRESET_INDEX);
    Exit;
  end;

  Result := FONVIFManager.ExecuteRequest(atPTZ,'TONVIFPTZManager.GoToPreset',FONVIFManager.GetSOAPBuilder.PrepareGotoPreset(FToken,FPresentList[aIndexPreset]^.Token),LResponseStr);
end;

function TONVIFPTZManager.SetPreset(const aPresetName:String;var aNewIndexPreset:Integer;aIndexExistsPreset: Integer=-1):Boolean;
var LResponseStr       : String;
    LNewSetPresetToken : String;
    LPreset            : PTPTZPreset;
    LXMLDoc            : IXMLDocument;
    LSoapBodyNode      : IXMLNode;
    LPresetSetResponse : IXMLNode;
    LErrorFound        : Boolean;
begin
  Result            := False;  
  aNewIndexPreset   := -1;
  if not isValidToken('TONVIFPTZManager.SetPreset') then Exit;

  if aPresetName.Trim.IsEmpty then
  begin
    FONVIFManager.SetLastStatusCode('TONVIFPTZManager.SetPreset',ONVIF_ERROR_PTZ_PRESETNAME_IS_EMPTY);
    Exit;
  end;

  LNewSetPresetToken := (FPresentList.Count +1).ToString;
  if aIndexExistsPreset > -1 then
  begin
    if not inRange(aIndexExistsPreset,0,FPresentList.Count -1) then
    begin
      FONVIFManager.SetLastStatusCode('TONVIFPTZManager.SetPreset',ONVIF_ERROR_PTZ_INVALID_PRESET_INDEX);
      Exit;
    end;

    LNewSetPresetToken := FPresentList[aIndexExistsPreset].Token;
  end
  else if FPresentList.Count +1 > SupportedInfo.MaxPreset then
  begin    
    FONVIFManager.SetLastStatusCode('TONVIFPTZManager.SetPreset',ONVIF_ERROR_PTZ_MAX_PRESET);    
    Exit;
  end;    

  Result := FONVIFManager.ExecuteRequest(atPTZ,'TONVIFPTZManager.SetPreset',FONVIFManager.GetSOAPBuilder.PrepareSetPreset(FToken,LNewSetPresetToken,aPresetName)  ,LResponseStr);

  if Result then
  begin
    {$REGION 'Log'}
    {TSI:IGNORE ON}
       FONVIFManager.DoWriteLog('TONVIFManager.SetPreset',LResponseStr,tpLivXMLResp,true);      
    {TSI:IGNORE OFF}
    {$ENDREGION}  
    if aIndexExistsPreset > -1 then
      FPresentList[aIndexExistsPreset]^.Name := aPresetName
    else
    begin
      Result := False;
      New(LPreset);
    
      LPreset^.Name  := aPresetName;
      LXMLDoc        := TXMLDocument.Create(nil);
      LXMLDoc.LoadFromXML(LResponseStr);

      if not FONVIFManager.IsValidSoapXML(LXMLDoc.DocumentElement,LErrorFound) then exit;
    
      LSoapBodyNode      := TONVIFXMLUtils.GetSoapBody(LXMLDoc.DocumentElement);
      LPresetSetResponse := TONVIFXMLUtils.RecursiveFindNode(LSoapBodyNode,'SetPresetsResponse');      
      LPreset^.Token     := String(LPresetSetResponse.Attributes['token']);      
      aNewIndexPreset    := FPresentList.Add(LPreset);
      Result             := True;      
    end;
  end;    
end;

procedure TONVIFPTZManager.SetToken(const Value: String);
begin
  if FToken <> value then
  begin    
    FToken := Value;
    if FToken.Trim.IsEmpty then
    begin
      ResetPTZNode;
      ResetPTZStatus;
      FPresentList.Clear;
    end
    else
    begin
      GetStatus;
      GetNodes; 
//  TODO    GetConfigurationOptions;
      FONVIFManager.SetTokenImagingByPTZToken 
    end;
  end;
end;

function TONVIFPTZManager.RemovePreset(const aIndexPreset: Integer): Boolean;
var LResponseStr : String;
begin
  Result := False;  
  if not isValidToken('TONVIFPTZManager.RemovePreset') then Exit;
  
  if not inRange(aIndexPreset,0,FPresentList.Count -1) then
  begin
    FONVIFManager.SetLastStatusCode('',ONVIF_ERROR_PTZ_INVALID_PRESET_INDEX);    
    Exit;
  end;

  Result := FONVIFManager.ExecuteRequest(atPTZ,'TONVIFPTZManager.RemovePreset',FONVIFManager.GetSOAPBuilder.PrepareRemovePreset(FToken,FPresentList[aIndexPreset].Token),LResponseStr);

  if Result then
    FPresentList.Delete(aIndexPreset);
end;

Procedure TONVIFPTZManager.ResetPTZStatus;
begin
   FPTZStatus.PTZPosition.PanTilt.X := -1;
   FPTZStatus.PTZPosition.PanTilt.Y := -1;   
   FPTZStatus.PTZPosition.Zoom      := -1;   
   FPTZStatus.MoveStatus.PanTilt    := String.Empty;   
   FPTZStatus.MoveStatus.Zoom       := String.Empty;   
   FPTZStatus.Error                 := String.Empty;
   FPTZStatus.UtcTime               := 0;
end;

procedure TONVIFPTZManager.ResetPTZNode;
begin
  FPTZNode.FixedHomePosition                                             := False;
  FPTZNode.GeoMove                                                       := False;
  FPTZNode.Token                                                         := String.Empty;
  SetLength(FPTZNode.SupportedPTZSpaces.AbsolutePanTiltPositionSpace,0);
  SetLength(FPTZNode.SupportedPTZSpaces.AbsoluteZoomPositionSpace,0);
  SetLength(FPTZNode.SupportedPTZSpaces.RelativePanTiltTranslationSpace,0);
  SetLength(FPTZNode.SupportedPTZSpaces.RelativeZoomTranslationSpace,0);
  SetLength(FPTZNode.SupportedPTZSpaces.ContinuousPanTiltVelocitySpace,0);
  SetLength(FPTZNode.SupportedPTZSpaces.ContinuousZoomVelocitySpace,0);    
  FPTZNode.SupportedPTZSpaces.PanTiltSpeedSpace.URI                      := String.Empty;  
  FPTZNode.SupportedPTZSpaces.PanTiltSpeedSpace.XRange.Min               := -1;
  FPTZNode.SupportedPTZSpaces.PanTiltSpeedSpace.XRange.Max               := -1;
  FPTZNode.SupportedPTZSpaces.ZoomSpeedSpace.URI                         := String.Empty;  
  FPTZNode.SupportedPTZSpaces.ZoomSpeedSpace.XRange.Min                  := -1;
  FPTZNode.SupportedPTZSpaces.ZoomSpeedSpace.XRange.Max                  := -1;  
end;

{ TSupportedInfo }
constructor TSupportedPTZInfo.Create(aOwnerPTZManager: TONVIFPTZManager);
begin
  FOwnerPTZManager := aOwnerPTZManager;
end;

function TSupportedPTZInfo.GetMaxPreset: Integer;
begin
  Result := FOwnerPTZManager.FPTZNode.MaximumNumberOfPresets;
end;

function TSupportedPTZInfo.GetSupportedAbsoluteMode: Boolean;
begin
   Result := Length(FOwnerPTZManager.FPTZNode.SupportedPTZSpaces.AbsolutePanTiltPositionSpace) >0;
end;

function TSupportedPTZInfo.GetSupportedContinuousMode: Boolean;
begin
  Result := Length(FOwnerPTZManager.FPTZNode.SupportedPTZSpaces.ContinuousPanTiltVelocitySpace) >0;
end;

function TSupportedPTZInfo.GetSupportedHome: Boolean;
begin
  Result := FOwnerPTZManager.FPTZNode.HomeSupported
end;

function TSupportedPTZInfo.GetSupportedRelativeModeMode: Boolean;
begin
  Result := Length(FOwnerPTZManager.FPTZNode.SupportedPTZSpaces.RelativePanTiltTranslationSpace) >0;
end;


end.
