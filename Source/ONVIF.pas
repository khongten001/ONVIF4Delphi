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

unit ONVIF;

interface

uses
  System.Classes, System.SysUtils, System.SyncObjs, System.Messaging,ONVIF.SOAP.Builder,
  System.IOUtils,ONVIF.Structure.Imaging,Soap.XSBuiltIns,  
  IdAuthenticationDigest, Winsock, XmlDoc, XmlIntf, XMLDom,System.Math, System.NetConsts,
  ONVIF.Structure.Device, ONVIF.Structure.Profile,System.Net.HttpClient,System.net.UrlClient,
  ONVIF.Structure.Capabilities,ONVIF.Structure.PTZ,ONVIF.XML.Utils;

CONST {Error Code}
      ONVIF_ERROR_URL_EMPTY                          = -1000;
      ONVIF_ERROR_SOAP_INVALID                       = -1001;
      ONVIF_ERROR_SOAP_NOBODY                        = -1002;
      ONVIF_ERROR_SOAP_FAULTCODE_NOT_FOUND           = -1003;     
      ONVIF_ERROR_PTZ_INVALID_PRESET_INDEX           = -1004;
      ONVIF_ERROR_DELPHI_EXCEPTION                   = -1005;
      ONVIF_ERROR_PTZ_TOKEN_IS_EMPTY                 = -1006;
      ONVIF_ERROR_PTZ_PRESETNAME_IS_EMPTY            = -1007;
      ONVIF_ERROR_PTZ_HOME_COMMAND_NOT_SUPPORTED     = -1008;
      ONVIF_ERROR_PTZ_MAX_PRESET                     = -1009; 
      ONVIF_ERROR_PTZ_MOVE_CONTINUOUS_NOT_SUPPORTED  = -1010;  
      ONVIF_ERROR_PTZ_IMMAGING_IS_EMPTY              = -1011;

      {Imaging}
      DEFAULT_TOKEN_IMAGING   = 'VideoSourceToken';
      DEFAULT_TOKEN_IMAGING_2 = 'VideoSourceConfig';
      
      AUTO_TOKEN_IMAGING    = 'VideoSource_%d';
      
Type

  {   
    TODO 
      - PresetTour
      - Recoding
      - Auxiliar
      - Imaging 
         -- Saturation, contrast ecc
         -- IRCut 
         -- IRIS 
         -- Focus
              -- Absolute,Relative
         others --> https://www.onvif.org/specs/srv/img/ONVIF-Imaging-Service-Spec.pdf?441d4a&441d4a


        -- Move AbsoluteMove,RelativeMove
        -- GetServiceCapabilities 
            --- Flip: Indicates whether or not E-Flip is supported.
            --- Reverse: Indicates whether or not reversing of PT control direction is supported.
            --- GetCompatibleConfigurations: Indicates the support for GetCompatibleConfigurations command.
            --- MoveStatus Indicates that the PTZStatus includes MoveStatus information.
            --- StatusPosition Indicates that the PTZStatus includes Position information.   
        -- Events A device supporting PTZ service dispatchs events listed in this chapter through the event 
        -- PresetTours
        others --> https://www.onvif.org/specs/srv/ptz/ONVIF-PTZ-Service-Spec-v1712.pdf?441d4a&441d4a
               --> https://www.onvif.org/specs/srv/ptz/ONVIF-PTZ-Service-Spec-v221.pdf

      https://www.onvif.org/specs/core/ONVIF-Core-Specification.pdf  
      - PRofile parser in record
        -- AudioEncoderConfiguration  : TAudioEncoderConfiguration;
        -= VideoAnalyticsConfiguration: TVideoAnalyticsConfiguration;
        -- Extension                  : TExtension;          
          --- Need example  
                  
      - SystemdateTime
   }


  /// <summary>
  ///   Specifies the type of ONVIF address, including device service, media, and PTZ and others.
  /// </summary>
  TONVIFAddrType = (atDeviceService, atMedia, atPtz,atImaging);

  /// <summary>
  ///   Specifies the type of ONVIF PTZ (Pan-Tilt-Zoom) command, including left, top, right,
  ///   bottom, top-right, top-left, bottom-left, and bottom-right.
  /// </summary>
  TONVIF_PTZ_CommandType = (opcNone,opcLeft, opcTop, opcRight, opcBotton, opcTopRight, opcTopLeft, opcBottonLeft, opcBottonRight);

  /// <summary>
  ///   Enumerates the logging levels for ONVIF events.
  /// </summary>
  /// <remarks>
  ///   Possible values are: Information, Error, Warning, and Exception.
  /// </remarks>
  TPONVIFLivLog = (tpLivInfo,tpLivError,tpLivWarning,tpLivException,tpLivXMLResp);

  /// <summary>
  ///   Defines a procedure type for writing logs with specific parameters.
  /// </summary>
  /// <param name="Funzione">
  ///   Name of the function or operation.
  /// </param>
  /// <param name="Descrizione">
  ///   Description of the log entry.
  /// </param>
  /// <param name="Livello">
  ///   Logging level (Information, Error, Warning, or Exception).
  /// </param>
  /// <param name="IsVerboseLog">
  ///   Indicates whether the log entry is verbose. Default is False.
  /// </param>
  /// <remarks>
  ///   This procedure is used to write logs with detailed information based on the specified parameters.
  /// </remarks>  
  TEventWriteLog    = procedure (Const aMethodName,aDescription:String;aLevel : TPONVIFLivLog;IsVerboseLog:boolean=False) of object;

  /// <summary>
  ///   Defines a procedure type for handling token events with specific parameters.
  /// </summary>
  /// <param name="aToken">
  ///   The token string encountered during processing of profiles.
  /// </param>
  /// <param name="aSetForDefault">
  ///   A flag indicating whether the token is set for the default for ONVIF command.
  /// </param>
  /// <remarks>
  ///   This procedure type is used for handling token events, providing information about
  ///   the encountered token on profiles and allowing modification of the default behavior.
  /// </remarks>
  TEventTokenFound  = procedure (const aName,aToken : String;var aSetForDefault:Boolean) of object;
  
  TONVIFManager        = class;
  TONVIFImagingManager = class;
  TONVIFPTZManager     = class;
  
  TSupportedImagingInfo = class(TPersistent)
  private
    FOwnerImagingManager : TONVIFImagingManager;
    function GetFocusSupported: Boolean;
  public
    constructor Create(aOwnerImagingManager : TONVIFImagingManager);  
    property FocusSupported : Boolean read GetFocusSupported;
  end;
  

  /// <summary>
  ///
  ///   https://www.onvif.org/specs/srv/img/ONVIF-Imaging-Service-Spec-v1606.pdf
  ///
  ///   The imaging service provides configuration and control data for imaging specific properties. 
  ///   WSDL is part of the framework and provided in the Imaging WSDL file. 
  ///   The service includes the following operations: 
  ///    Get and set imaging configurations (exposure time, gain and white balance, for 
  ///   example). 
  ///    Get imaging configuration options (valid ranges for imaging parameters). 
  ///    Move focus lens. 
  ///    Stop ongoing focus movement. 
  ///    Get current position and move status for focus. 
  /// </summary>  
  TONVIFImagingManager = Class
  private
    FONVIFManager              : TONVIFManager;
    FSupportImagingPresets     : Boolean;
    FSupportImageStabilization : Boolean;
    FToken                     : String;
    FImagingSettings           : TImagingSettings;
    FSupportedInfo             : TSupportedImagingInfo;
    FFocusSettings             : TImagingFocusSettings;
    
    /// <summary>    
    //  The capabilities reflect optional functions and functionality of a service. The information is 
    /// static and does not change during device operation. The following capabilites are available: 
    //    ImageStabilization: Indicates whether or not Image Stabilization feature is supported. 
    //    ImagingPresets    : Indicates whether or not Imaging Presets feature is supported  
    /// </summary>           
    function GetCapabilities: Boolean;
    function isValidToken(const aMathodNameRequest:String): Boolean;
    procedure SetToken(const aValue: String);
    procedure ResetImagingSettings;
    /// <summary>
    ///  This operation requests the imaging setting for a video source on the device. A device 
    ///  implementing the imaging service shall support this command. 
    ///  If the Video Source supports any of the imaging settings as defined by the ImagingSettings 
    ///  type in the [ONVIF Schema], then it should be possible to retrieve the imaging settings from 
     /// the device through the GetImagingSettings command.  
    /// </summary>      
    function GetImagingSettings: Boolean;
    /// <summary>
    ///  The GetMoveOptions command retrieves the focus lens move options to be used in the move 
    ///  command as defined in Section 5.1.4. A device that supports the imaging service shall 
    ///  support the GetMoveOptions command. The response to the command shall include all 
    ///  supported Move Operations. If focus move is not supported at all, the reponse shall be empty
    /// </summary>
    function GetMoveOptions: Boolean;
    function GetStatus: Boolean;
    
  public
    /// <summary>
    ///   Constructor for initializing a new instance of the Imaging Manager.
    /// </summary>
    /// <param name="aONVIFManager">Reference to the parent ONVIF manager instance.</param>  
    constructor Create(aONVIFManager : TONVIFManager); 
    destructor Destroy;override; 
    property FocusSettings             : TImagingFocusSettings read FFocusSettings;
    property ImagingSettings           : TImagingSettings      read FImagingSettings;
    property SupportedInfo             : TSupportedImagingInfo read FSupportedInfo; 
    property SupportImageStabilization : Boolean               read FSupportImageStabilization;
    property SupportImagingPresets     : Boolean               read FSupportImagingPresets;    
    /// <summary>
    ///   Gets or sets the token of the VideoSource.
    /// </summary>    
    property Token                    : String              read FToken                         write SetToken;    
    
  End;
  
  TSupportedPTZInfo = class(TPersistent)
    private
    FOwnerPTZManager : TONVIFPTZManager;
    function GetMaxPreset: Integer;
    function GetSupportedAbsoluteMode: Boolean;
    function GetSupportedContinuousMode: Boolean;
    function GetSupportedHome: Boolean;
    function GetSupportedRelativeModeMode: Boolean;  
  public
    constructor Create(aOwnerPTZManager : TONVIFPTZManager);
    property Home           : Boolean read GetSupportedHome;
    property MaxPreset      : Integer read GetMaxPreset;
    property ContinuousMode : Boolean read GetSupportedContinuousMode;        
    property AbsoluteMode   : Boolean read GetSupportedAbsoluteMode;  
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
    FONVIFManager  : TONVIFManager;
    FPresentList   : TPTZPresetList;
    FToken         : String;    
    FPTZNode       : TPTZNode; 
    FSupportedInfo : TSupportedPTZInfo;
    FPTZStatus     : TPTZStatus;
    function isValidToken(const aMathodNameRequest:String): Boolean;
    procedure ResetPTZNode;
    procedure SetToken(const Value: String);
    procedure ResetPTZStatus;
  public
    /// <summary>
    ///   Constructor for initializing a new instance of the ONVIF PTZ Manager.
    /// </summary>
    /// <param name="aONVIFManager">Reference to the parent ONVIF manager instance.</param>  
    constructor Create(aONVIFManager : TONVIFManager);

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
    function StartMoveContinuous(const aDirection: TONVIF_PTZ_CommandType): Boolean; //TODO Timeout and rename in ConinuosMove

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
    
    
    property PTZNode                 : TPTZNode            read FPTZNode;        
    
    /// <summary>
    ///   Gets or sets the token of the ONVIF camera.
    /// </summary>    
    property Token                   : String              read FToken                   write SetToken;    
  End;
    
  /// <summary>
  ///   Represents a manager class for handling ONVIF-related functionalities.
  /// </summary>  
  TONVIFManager = class
  private
    FUrl                     : String;
    FLastResponse            : String;
    FPathFileResponseOnDisk  : String;
    FSaveResponseOnDisk      : Boolean;
    FIsFixedToken            : Boolean;
    FLastStatusCode          : Integer;
    FSpeed                   : Byte;
    FDevice                  : TDeviceInformation;
    FSOAPBuilder             : TONVIFSOAPBuilder;  

    FProfiles                : TProfiles;    
    FCapabilities            : TCapabilitiesONVIF;
    FPTZ                     : TONVIFPTZManager;
    FImaging                 : TONVIFImagingManager;
    {Event}
    FOnWriteLog              : TEventWriteLog;
    FOnReadInfoComplete      : TNotifyEvent; 
    FOnPTZTokenFound     : TEventTokenFound;
    FOnSourceiideoTokenFound : TEventTokenFound;


    /// <summary>
    ///   Generates a URL based on the specified ONVIF address type.
    /// </summary>
    /// <param name="aUrlType">
    ///   The type of ONVIF address for which the URL needs to be generated.
    /// </param>
    /// <returns>
    ///   The generated URL as a string.
    /// </returns>
    /// <remarks>
    ///   The function takes an ONVIF address type as input and returns
    ///   the corresponding URL. Make sure to handle unexpected address types
    ///   appropriately.
    /// </remarks>    
    function GetUrlByType(const aUrlType: TONVIFAddrType): string;
    
    /// <summary>
    ///   Resets the state or configuration of internal record like TDeviceInformation.
    /// </summary>    
    procedure Reset;

    procedure SetLastStatusCode(const aMethodName:String;const aErrorCode:Integer);

    /// <summary>
    ///   Executes an ONVIF request using the provided address, input stream, and output stream.
    /// </summary>
    /// <param name="Addr">
    ///   The address for the ONVIF request.
    /// </param>
    /// <param name="InStream">
    ///   The input stream containing the ONVIF request data.
    /// </param>
    /// <param name="OutStream">
    ///   The output stream to store the ONVIF response data.
    /// </param>
    /// <returns>
    ///   True if the request is executed successfully; False otherwise.
    /// </returns>
    function ExecuteRequest(const aAddr: String; const aInStream, aOutStream: TStringStream): Boolean; overload;

    /// <summary>
    ///   Executes an ONVIF request using the provided address and request string.
    /// </summary>
    /// <param name="Addr">
    ///   The address for the ONVIF request.
    /// </param>
    /// <param name="Request">
    ///   The ONVIF request string.
    /// </param>
    /// <param name="Answer">
    ///   Returns the ONVIF response string after executing the request.
    /// </param>
    /// <returns>
    ///   True if the request is executed successfully; False otherwise.
    /// </returns>
    function ExecuteRequest(const aAddr, aRequest: String; var aAnswer: String): Boolean; overload;

    /// <summary>
    ///   Sets the URL used for ONVIF communication.
    /// </summary>
    /// <param name="aValue">
    ///   The URL to be set.
    /// </param>
    procedure SetUrl(const aValue: String);

  
    /// <summary>
    ///   Writes a log entry with specified parameters.
    /// </summary>
    /// <param name="aFunction">
    ///   Name of the function or operation.
    /// </param>
    /// <param name="aDescription">
    ///   Description of the log entry.
    /// </param>
    /// <param name="aLivel">
    ///   Logging level (Information, Error, Warning, or Exception).
    /// </param>
    /// <param name="aIsVerboseLog">
    ///   Indicates whether the log entry is verbose. Default is False.
    /// </param>
    procedure DoWriteLog(const aFunction, aDescription: String; aLevel: TPONVIFLivLog; aIsVerboseLog: boolean = False);

    /// <summary>
    ///   Checks if the set URL is valid for ONVIF communication.
    /// </summary>
    /// <param name="aMathodNameRequest">
    ///   name of function that check ther url for logging level 
    /// </param>
    /// <returns>
    ///   True if the URL is valid; otherwise, False.
    /// </returns>
    function UrlIsValid(const aMathodNameRequest:String): Boolean;

    /// <summary>
    ///   Converts an internal error code to a human-readable string.
    /// </summary>
    /// <param name="aMathodNameRequest">
    ///   name of function that check ther url for logging level 
    /// </param>
    /// <param name="aErrorCode">
    ///   The internal error code to be converted.
    /// </param>
    /// <returns>
    ///   The human-readable string representation of the internal error.
    /// </returns>
    function InternalErrorToString(const aMathodNameRequest:String;const aErrorCode: Integer): String;
    
    /// <summary>
    /// Checks whether the given XML node is a valid SOAP XML.
    /// </summary>
    /// <param name="aRootNode">The root node of the XML document.</param>
    /// <returns>True if the XML is a valid SOAP XML; otherwise, false.</returns>
    function IsValidSoapXML(const aRootNode: IXMLNode;var aErrorFound:Boolean): Boolean;


    /// <summary>
    ///   Resets the ONVIF capabilities of the device to default values.
    /// </summary>
    procedure ResetCapabilities;

    /// <summary>
    ///   Resets the ONVIF device information to default values.
    /// </summary>
    procedure ResetDevice;

    /// <summary>
    ///   Resets the ONVIF profiles information to default values.
    /// </summary>
    procedure ResetProfiles;
    
    /// <summary>
    ///   Executes the DoOnReadInfoCompleate event, triggering the retrieval of ONVIF information by ReadInfoAsync.
    /// </summary>    
    procedure DoOnReadInfoCompleate;

    procedure SetTokenImagingByPTZToken;    
  public
    /// <summary>
    ///   Initializes a new instance of the TONVIFManager class with the specified ONVIF service details.
    /// </summary>
    /// <param name="aUrl">
    ///   The URL of the ONVIF service.
    /// </param>
    /// <param name="aLogin">
    ///   The login credentials for the ONVIF service.
    /// </param>
    /// <param name="aPassword">
    ///   The password credentials for the ONVIF service.
    /// </param>
    constructor Create(const aUrl, aLogin, aPassword:String);overload;
  
    /// <summary>
    ///   Initializes a new instance of the TONVIFManager class with the specified ONVIF service details.
    /// </summary>
    /// <param name="aUrl">
    ///   The URL of the ONVIF service.
    /// </param>
    /// <param name="aLogin">
    ///   The login credentials for the ONVIF service.
    /// </param>
    /// <param name="aPassword">
    ///   The password credentials for the ONVIF service.
    /// </param>
    /// <param name="aToken">
    ///   The security token for the ONVIF service.
    /// </param>
    constructor Create(const aUrl, aLogin, aPassword, aToken: String);overload;
    
    /// <summary>
    ///   Destructor for the class instance.
    /// </summary>
    destructor Destroy;override;

    /// <summary>
    ///   Retrieves device information ,Capabilities and  Profiles
    /// </summary>    
    procedure ReadInfo;

    /// <summary>
    ///   Retrieves device information ,Capabilities and  Profiles in other  thread and fire event OnGetAllInfo
    /// </summary>    
    procedure ReadInfoAsync;    

    /// <summary>
    ///   Retrieves device information and returns a Boolean indicating the success of the operation.
    /// </summary>
    /// <returns>
    ///   True if the device information is successfully retrieved; False otherwise, compile TDeviceInformation record.
    /// </returns>    
    function GetDeviceInformation: Boolean;
    
    /// <summary>
    ///   Retrieves and processes capabilities for the current ONVIF device.
    /// </summary>
    /// <returns>
    ///   Returns True if the capabilities are successfully retrieved and processed; otherwise, returns False.
    /// </returns>   
    function GetCapabilities: Boolean; 
    
    /// <summary>
    ///   Retrieves the profiles associated with the ONVIF device.
    /// </summary>
    /// <param name="aResultStr">
    ///   Returns the result string containing the profiles after executing the operation.
    /// </param>
    /// <returns>
    ///   True if the operation is executed successfully; False otherwise.
    /// </returns>
    function GetProfiles: Boolean;

    /// <summary>
    ///   Gets or sets the speed parameter for PTZ operations.
    /// </summary>
    property Speed                   : Byte                read FSpeed                   write FSpeed;

    /// <summary>
    ///   Gets or sets the URL of the ONVIF service.
    /// </summary>
    property Url                     : String              read Furl                     write SetUrl;
    
    /// <summary>
    ///   Event handler for writing logs with specific parameters.
    /// </summary>
    /// <remarks>
    ///   Use this event to handle log writing with detailed information based on the specified parameters.
    /// </remarks>
    property OnWriteLog               : TEventWriteLog     read FOnWriteLog              write FOnWriteLog;
    
    /// <summary>
    ///   Event triggered when retrieving comprehensive ONVIF information through the GetAllInfo method.
    /// </summary>
    /// <remarks>
    ///   The <c>FOnReadInfoComplete</c> event is triggered when the GetAllInfo method is executed, facilitating
    ///   the retrieval of detailed ONVIF information. By assigning a handler to this event, the application
    ///   can respond to the completion of the GetAllInfo operation, allowing for further processing or
    ///   presentation of the obtained ONVIF data.
    /// </remarks>    
    property OnReadInfoComplete       : TNotifyEvent        read FOnReadInfoComplete            write FOnReadInfoComplete;

    /// <summary>
    ///   Property representing an event handler for the token found in profiles.
    /// </summary>
    /// <remarks>
    ///   Use this property to assign a handler for the token found event in PTZConfigurazion.
    ///   The assigned handler should be of type TEventTokenFound, allowing customization
    ///   of the default behavior for ONVIF commands related to PTZ tokens.
    /// </remarks>    
    property OnPTZTokenFound          : TEventTokenFound    read FOnPTZTokenFound                    write FOnPTZTokenFound;   

    /// <summary>
    ///   Property representing an event handler for the token found in SourceVideo.
    /// </summary>
    /// <remarks>
    ///   Use this property to assign a handler for the token found event in VideoSource.
    ///   The assigned handler should be of type TEventTokenFound, allowing customization
    ///   of the default behavior for ONVIF commands related to VideoSource tokens.
    /// </remarks>    
    property OnSourceiideoTokenFound   : TEventTokenFound   read FOnSourceiideoTokenFound            write FOnSourceiideoTokenFound;       

     

    /// <summary>
    ///   Gets or sets whether to save the last HTTP response on disk.
    /// </summary>
    /// <remarks>
    ///   Set this property to True if you want to save the last HTTP response on disk.
    /// </remarks>
    property SaveResponseOnDisk       : Boolean            read FSaveResponseOnDisk      write FSaveResponseOnDisk;  

    /// <summary>
    ///   Gets or sets the file path for storing ONVIF response data on disk default DumpResponse.log
    /// </summary>
    /// <remarks>
    ///   The property <c>PathFileResponseOnDisk</c> is used to specify the location where ONVIF
    ///   response data will be saved on disk. This path can be set to a directory or a full file path,
    ///   depending on the application's requirements. Responses from ONVIF operations, such as
    ///   device information retrieval or profile creation, may be stored at this location for reference
    ///   or debugging purposes.
    /// </remarks>
    property PathFileResponseOnDisk   : String             read FPathFileResponseOnDisk  write FPathFileResponseOnDisk;

    /// <summary>
    ///   Gets the last HTTP status code received.
    /// </summary>
    /// <remarks>
    ///   Use this property to retrieve the last HTTP status code received during communication.
    /// </remarks>    
    property LastStatusCode           : Integer            read FLastStatusCode;
    
    /// <summary>
    ///   Gets the last HTTP response received.
    /// </summary>
    /// <remarks>
    ///   Use this property to retrieve the last HTTP response received during communication.
    /// </remarks>    
    property LastResponse             : String             read FLastResponse;  

    /// <summary>
    ///   Gets information about the ONVIF device.
    /// </summary>
    property Device                   : TDeviceInformation read FDevice;
    
    /// <summary>
    ///   Gets the profiles associated with the ONVIF communication.
    /// </summary>
    /// <remarks>
    ///   Use this property to retrieve the profiles associated with the ONVIF communication.
    /// </remarks>    
    property Profiles                 : TProfiles          read FProfiles;  
    
    /// <summary>
    ///   Represents the ONVIF capabilities of the device.
    /// </summary>
    /// <remarks>
    ///   The ONVIF capabilities, including device, events, PTZ, and extension capabilities.
    /// </remarks>     
    property Capabilities             : TCapabilitiesONVIF read FCapabilities; 
    
    /// <summary>
    ///   Gets the ONVIF Pan-Tilt-Zoom (PTZ) manager for controlling PTZ-related functionalities.
    /// </summary>
    /// <remarks>
    ///   The property <c>PTZ</c> provides access to the ONVIF Pan-Tilt-Zoom manager, allowing the
    ///   application to control PTZ-related functionalities such as camera movement and zoom levels.
    ///   The PTZ manager encapsulates ONVIF commands and operations related to pan, tilt, and zoom
    ///   controls, providing a convenient interface for managing PTZ functionality in an ONVIF-enabled
    ///   environment.
    /// </remarks>
    property PTZ                      : TONVIFPTZManager   read FPTZ;

    /// <summary>
    ///   The imaging service provides configuration and control data for imaging specific properties. 
    ///   WSDL is part of the framework and provided in the Imaging WSDL file. 
    ///   The service includes the following operations: 
    ///    Get and set imaging configurations (exposure time, gain and white balance, for 
    ///   example). 
    ///    Get imaging configuration options (valid ranges for imaging parameters). 
    ///    Move focus lens. 
    ///    Stop ongoing focus movement. 
    ///    Get current position and move status for focus. 
    /// </summary>  
    property Imaging                  :  TONVIFImagingManager read FImaging;     
  end;

implementation

Uses  IdURI;

Function StrToFloatLocale(const S: string; const Default: Extended):Extended;
begin
  if FormatSettings.DecimalSeparator = '.' then
    Result := StrToFloatDef(s.Replace(',','.'),Default)
  else    
    Result := StrToFloatDef(s.Replace('.',','),Default)      
end;

constructor TONVIFManager.Create(const aUrl, aLogin, aPassword:String);
begin
  Create(aUrl, aLogin, aPassword,String.Empty);
end;


constructor TONVIFManager.Create(const aUrl,aLogin,aPassword,aToken:String);
begin
  FSOAPBuilder            := TONVIFSOAPBuilder.Create(aLogin,aPassword);
  FSaveResponseOnDisk     := False;
  FPathFileResponseOnDisk := 'DumpResponse.log';
  FIsFixedToken           := not aToken.IsEmpty;
  FPTZ                    := TONVIFPTZManager.Create(self);
  FPTZ.Token              := aToken;
  FImaging                := TONVIFImagingManager.Create(self);
  Url                     := aUrl;    // execute setUrl;  
  FSpeed                  := 6;
end;

destructor TONVIFManager.Destroy;
begin
  FreeAndNil(FSOAPBuilder);
  FreeAndNil(FImaging);
  FreeAndNil(FPTZ);
  inherited;
end;

procedure TONVIFManager.DoWriteLog(const aFunction, aDescription: String;aLevel: TPONVIFLivLog; aIsVerboseLog: boolean=false);
begin
  if Assigned(FOnWriteLog) then
    FOnWriteLog(aFunction,aDescription,aLevel,aIsVerboseLog)
end;

procedure TONVIFManager.ReadInfo;
begin
  {TODO get system date time to be use for password? }
  Try
  if GetCapabilities then
  begin
    GetDeviceInformation;
    GetProfiles;
  end;
  Finally
    TThread.Queue(nil, DoOnReadInfoCompleate);  
  End;  
end;

procedure TONVIFManager.DoOnReadInfoCompleate;
begin
  if Assigned(FOnReadInfoComplete) then
    FOnReadInfoComplete(Self);
end;

procedure TONVIFManager.ReadInfoAsync;
var LThread: TThread;
begin
  LThread := TThread.CreateAnonymousThread(
    procedure
    begin
      ReadInfo;
    end
  );
  LThread.Start;
end;

procedure TONVIFManager.SetLastStatusCode(const aMethodName:String;const aErrorCode: Integer);
begin
  FLastStatusCode := aErrorCode;
  FLastResponse   := InternalErrorToString(aMethodName,aErrorCode);
end;

procedure TONVIFManager.SetTokenImagingByPTZToken;
var I: Integer;
begin
  {$REGION 'Log'}
  {TSI:IGNORE ON}
      DoWriteLog('TONVIFManager.SetTokenImagingByPTZToken',Format('Search PTZ Token  [%s]',[PTZ.Token.Trim]),tpLivInfo,true);      
  {TSI:IGNORE OFF}
  {$ENDREGION}
  for I := Low(FProfiles) to High(FProfiles) do
  begin
    if SameText(FProfiles[I].PTZConfiguration.token.Trim,PTZ.Token.Trim) or
       SameText(FProfiles[I].Token.Trim,PTZ.Token.Trim)  then
    begin
      {$REGION 'Log'}
      {TSI:IGNORE ON}
          DoWriteLog('TONVIFManager.SetTokenImagingByPTZToken',Format('New Token found [%s]',[FProfiles[I].VideoSourceConfiguration.token]),tpLivInfo,true);      
      {TSI:IGNORE OFF}
      {$ENDREGION}
    
      if not FProfiles[I].VideoSourceConfiguration.token.IsEmpty then
      begin
        if (FProfiles[I].VideoSourceConfiguration.token.Trim = DEFAULT_TOKEN_IMAGING) or // How can I identify without using constants?
           (FProfiles[I].VideoSourceConfiguration.token.Trim = DEFAULT_TOKEN_IMAGING_2)
        then
          FImaging.Token  := Format(AUTO_TOKEN_IMAGING,[I+1])
        else
          FImaging.Token  := FProfiles[I].VideoSourceConfiguration.token.Trim;
        Break; 
      end;
    end;
  end;
end;

procedure TONVIFManager.SetUrl(const aValue: String);
begin
  if Furl <> aValue then
  begin
    Reset;
    Furl := aValue;
  end;
end;

function TONVIFManager.UrlIsValid(const aMathodNameRequest:String): Boolean;
begin
  Result := not FUrl.Trim.IsEmpty;
  if not result then
    SetLastStatusCode(aMathodNameRequest,ONVIF_ERROR_URL_EMPTY);
end;

function TONVIFManager.InternalErrorToString(const aMathodNameRequest:String;const aErrorCode :Integer):String;
begin

  case aErrorCode of
    ONVIF_ERROR_URL_EMPTY                          : Result := 'Url is empty'; 
    ONVIF_ERROR_SOAP_INVALID                       : result := 'Root node is not SOAP envelope';
    ONVIF_ERROR_SOAP_NOBODY                        : result := 'Body SOAP node not found';
    ONVIF_ERROR_SOAP_FAULTCODE_NOT_FOUND           : Result := 'SOAP Fault code not found';
    ONVIF_ERROR_PTZ_INVALID_PRESET_INDEX           : Result := 'Index of preset is out of range';
    ONVIF_ERROR_PTZ_TOKEN_IS_EMPTY                 : Result := 'PTZ token is empty';
    ONVIF_ERROR_PTZ_PRESETNAME_IS_EMPTY            : Result := 'Preset name is empty';
    ONVIF_ERROR_PTZ_HOME_COMMAND_NOT_SUPPORTED     : Result := 'Home command are not supported by camera';
    ONVIF_ERROR_PTZ_MAX_PRESET                     : Result := 'Maximum number of presets reached';
    ONVIF_ERROR_PTZ_MOVE_CONTINUOUS_NOT_SUPPORTED  : Result := 'Continuous mode not supported by camera';
    ONVIF_ERROR_PTZ_IMMAGING_IS_EMPTY              : Result := 'VideoSource token is empty';
  else
    result := 'Unknow error' 
  end;
  {$REGION 'Log'}
  {TSI:IGNORE ON}
      DoWriteLog(aMathodNameRequest,Format('Url [%s] Internal error [%d] description [%s]',[FUrl,aErrorCode,result]),tpLivError);
  {TSI:IGNORE OFF}
  {$ENDREGION}
end;

function TONVIFManager.GetUrlByType(const aUrlType: TONVIFAddrType): string;
CONST   
      URL_DEVICE_SERVICE = 'onvif/device_service';
      URL_PTZ_SERVICE    = 'onvif/ptz_service';  
      URL_MEDIA          = 'onvif/media';
      URL_IMAGING        = 'onvif/imaging';

Var LUri: TIdURI;
begin
  Result := String.Empty;
  if not UrlIsValid('TONVIFManager.GetUrlByType') then Exit;
  
  LUri := TIdURI.Create(FUrl);
  try
    case aUrlType of
      atDeviceService: LUri.Document := URL_DEVICE_SERVICE;
      atMedia        : LUri.Document := URL_MEDIA;
      atPtz          : LUri.Document := URL_PTZ_SERVICE;
      atImaging      : LUri.Document := URL_IMAGING;
    end;
    Result := LUri.Uri;                                  
  finally
    FreeAndNil(LUri);
  end;
end;

function TONVIFManager.GetCapabilities: Boolean;
var LResultStr         : String;
    LXMLDoc            : IXMLDocument;
    LSoapBodyNode      : IXMLNode;
    LCapabilitieNode   : IXMLNode;
    LNodeTmp1          : IXMLNode;
    LNodeTmp2          : IXMLNode;    
    I                  : Integer;
    X                  : Integer;    
    LErrorFound        : Boolean;
begin
  Result := false;

  ResetCapabilities;
  Result := ExecuteRequest(GetUrlByType(atDeviceService), FSOAPBuilder.PrepareGetCapabilitiesRequest, LResultStr);

  if Result then
  begin
    {$REGION 'Log'}
    {TSI:IGNORE ON}
        DoWriteLog('TONVIFManager.GetCapabilities',LResultStr,tpLivXMLResp,true);      
    {TSI:IGNORE OFF}
    {$ENDREGION}
    LXMLDoc := TXMLDocument.Create(nil);
    LXMLDoc.LoadFromXML(LResultStr);

    if not IsValidSoapXML(LXMLDoc.DocumentElement,LErrorFound) then exit;
    
    LSoapBodyNode     := TONVIFXMLUtils.GetSoapBody(LXMLDoc.DocumentElement);
    LCapabilitieNode  := TONVIFXMLUtils.RecursiveFindNode(LSoapBodyNode,'Device');

    if Assigned(LCapabilitieNode) then
    begin
      FCapabilities.Device.XAddr := TONVIFXMLUtils.GetChildNodeValue(LCapabilitieNode,'XAddr');
      LNodeTmp1                  := TONVIFXMLUtils.RecursiveFindNode(LCapabilitieNode,'Network');

      if Assigned(LNodeTmp1) then
      begin
        FCapabilities.Device.Network.IPFilter          := StrToBoolDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'IPFilter'),False);
        FCapabilities.Device.Network.ZeroConfiguration := StrToBoolDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'IPFilter'),False);  
        FCapabilities.Device.Network.IPVersion6        := StrToBoolDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'IPVersion6'),False);   
        FCapabilities.Device.Network.DynDNS            := StrToBoolDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'DynDNS'),False);  

        LNodeTmp2                                      := TONVIFXMLUtils.RecursiveFindNode(LNodeTmp1,'Extension');
        if Assigned(LNodeTmp2) then      
        begin  
          FCapabilities.Device.Network.Extension.Dot11Configuration := StrToBoolDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp2,'Dot11Configuration'),False);
          for I := 0 to LNodeTmp2.ChildNodes.Count -1 do
          begin
            if not SameText(LNodeTmp2.ChildNodes[I].DOMNode.localName,'Dot11Configuration') then
              {$REGION 'Log'}
              {TSI:IGNORE ON}
                  DoWriteLog('TONVIFPTZManager.GetCapabilities',Format('Unsupported node name [%s]',[LNodeTmp2.ChildNodes[I].DOMNode.localName]),tpLivWarning);      
              {TSI:IGNORE OFF}
              {$ENDREGION}                                                
          end;            
        end;
      end;

      LNodeTmp1 := TONVIFXMLUtils.RecursiveFindNode(LCapabilitieNode,'System');

      if Assigned(LNodeTmp1) then
      begin
        FCapabilities.Device.System.DiscoveryResolve        := StrToBoolDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'DiscoveryResolve'),False);
        FCapabilities.Device.System.DiscoveryBye            := StrToBoolDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'DiscoveryBye'),False);
        FCapabilities.Device.System.RemoteDiscovery         := StrToBoolDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'RemoteDiscovery'),False);
        FCapabilities.Device.System.SystemBackup            := StrToBoolDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'SystemBackup'),False);
        FCapabilities.Device.System.SystemLogging           := StrToBoolDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'SystemLogging'),False);
        FCapabilities.Device.System.FirmwareUpgrade         := StrToBoolDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'FirmwareUpgrade'),False);
        
        LNodeTmp2                                           := TONVIFXMLUtils.RecursiveFindNode(LNodeTmp1,'SupportedVersions');
        if Assigned(LNodeTmp2) then
        begin        
          FCapabilities.Device.System.SupportedVersions.Major := StrToIntDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp2,'Major'),-1);
          FCapabilities.Device.System.SupportedVersions.Minor := StrToIntDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp2,'Minor'),-1);
        end;
      end;    

      {event}
      LCapabilitieNode := TONVIFXMLUtils.RecursiveFindNode(LSoapBodyNode,'Events');

      if Assigned(LCapabilitieNode) then
      begin
        FCapabilities.Events.XAddr                                         := TONVIFXMLUtils.GetChildNodeValue(LCapabilitieNode,'XAddr');
        FCapabilities.Events.WSSubscriptionPolicySupport                   := StrToBoolDef(TONVIFXMLUtils.GetChildNodeValue(LCapabilitieNode,'WSSubscriptionPolicySupport'),False);
        FCapabilities.Events.WSPullPointSupport                            := StrToBoolDef(TONVIFXMLUtils.GetChildNodeValue(LCapabilitieNode,'WSPullPointSupport'),False);
        FCapabilities.Events.WSPausableSubscriptionManagerInterfaceSupport := StrToBoolDef(TONVIFXMLUtils.GetChildNodeValue(LCapabilitieNode,'WSPausableSubscriptionManagerInterfaceSupport'),False);
      end;

      {Media}
      LCapabilitieNode := TONVIFXMLUtils.RecursiveFindNode(LSoapBodyNode,'Media'); 
      if Assigned(LCapabilitieNode) then
      begin            
        FCapabilities.Media.XAddr := TONVIFXMLUtils.GetChildNodeValue(LCapabilitieNode,'XAddr');
        LNodeTmp1                 := TONVIFXMLUtils.RecursiveFindNode(LCapabilitieNode,'StreamingCapabilities');
        if Assigned(LNodeTmp1) then
        begin
          FCapabilities.Media.StreamingCapabilities.RTPMulticast := StrToBoolDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'RTPMulticast'),False);
          FCapabilities.Media.StreamingCapabilities.RTP_TCP      := StrToBoolDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'RTP_TCP'),False);
          FCapabilities.Media.StreamingCapabilities.RTP_RTSP_TCP := StrToBoolDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'RTP_RTSP_TCP'),False);    
        end;
      end;
      {PTZ}
      LCapabilitieNode := TONVIFXMLUtils.RecursiveFindNode(LSoapBodyNode,'PTZ'); 
      if Assigned(LCapabilitieNode) then      
        FCapabilities.PTZ.XAddr := TONVIFXMLUtils.GetChildNodeValue(LCapabilitieNode,'XAddr');
        
      {Extension}
      LCapabilitieNode := TONVIFXMLUtils.RecursiveFindNode(LSoapBodyNode,'Extension',True); 

      if Assigned(LCapabilitieNode) then
      begin       
        for I := 0 to LCapabilitieNode.ChildNodes.Count -1 do
        begin
          for X := 0 to LCapabilitieNode.ChildNodes[I].ChildNodes.Count -1 do
          begin                             
            if not SameText(LCapabilitieNode.ChildNodes[I].ChildNodes[X].DOMNode.localName,'Search') and 
               not SameText(LCapabilitieNode.ChildNodes[I].ChildNodes[X].DOMNode.localName,'Replay') and          
               not SameText(LCapabilitieNode.ChildNodes[I].ChildNodes[X].DOMNode.localName,'Extension') and 
               not SameText(LCapabilitieNode.ChildNodes[I].ChildNodes[X].DOMNode.localName,'DeviceIO') and 
               not SameText(LCapabilitieNode.ChildNodes[I].ChildNodes[X].DOMNode.localName,'VideoSources') and 
               not SameText(LCapabilitieNode.ChildNodes[I].ChildNodes[X].DOMNode.localName,'VideoOutputs') and 
               not SameText(LCapabilitieNode.ChildNodes[I].ChildNodes[X].DOMNode.localName,'AudioSources') and 
               not SameText(LCapabilitieNode.ChildNodes[I].ChildNodes[X].DOMNode.localName,'AudioOutputs') and 
               not SameText(LCapabilitieNode.ChildNodes[I].ChildNodes[X].DOMNode.localName,'RelayOutputs') and 
               not SameText(LCapabilitieNode.ChildNodes[I].ChildNodes[X].DOMNode.localName,'Recording')
              
            then
              {$REGION 'Log'}
              {TSI:IGNORE ON}
                  DoWriteLog('TONVIFManager.GetCapabilities',Format('Unsupported node name [%s]',[LCapabilitieNode.ChildNodes[I].ChildNodes[X].DOMNode.localName]),tpLivWarning);      
              {TSI:IGNORE OFF}
              {$ENDREGION}                                                
          end;         
        end;

        LNodeTmp1 := TONVIFXMLUtils.RecursiveFindNode(LCapabilitieNode,'Search');  
        if Assigned(LNodeTmp1) then
        begin
          FCapabilities.Extension.Search.XAddr           := TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'XAddr');
          FCapabilities.Extension.Search.MetadataSearch  := StrToBoolDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'MetadataSearch'),False);    
        end;
        
        LNodeTmp1 := TONVIFXMLUtils.RecursiveFindNode(LCapabilitieNode,'Replay');  
        if Assigned(LNodeTmp1) then
          FCapabilities.Extension.Replay.XAddr := TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'XAddr'); 
          
        LNodeTmp1 := TONVIFXMLUtils.RecursiveFindNode(LCapabilitieNode,'DeviceIO');  
        if Assigned(LNodeTmp1) then
          FCapabilities.Extension.DeviceIO.XAddr := TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'XAddr'); 

        LNodeTmp1 := TONVIFXMLUtils.RecursiveFindNode(LCapabilitieNode,'Recording');  
        if Assigned(LNodeTmp1) then
        begin
          FCapabilities.Extension.Recording.XAddr              := TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'XAddr'); 
          FCapabilities.Extension.Recording.ReceiverSource     := StrToBoolDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'ReceiverSource'),False);
          FCapabilities.Extension.Recording.MediaProfileSource := StrToBoolDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'MediaProfileSource'),False); 
          FCapabilities.Extension.Recording.DynamicRecordings  := StrToBoolDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'DynamicRecordings'),False); 
          FCapabilities.Extension.Recording.MaxStringLength    := StrToIntDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'MaxStringLength'),-1); 

        end;
       
        FCapabilities.Extension.VideoSources := StrToIntDef(TONVIFXMLUtils.GetChildNodeValue(LCapabilitieNode,'VideoSources'),-1);         
        FCapabilities.Extension.VideoOutputs := StrToIntDef(TONVIFXMLUtils.GetChildNodeValue(LCapabilitieNode,'VideoOutputs'),-1);         
        FCapabilities.Extension.AudioSources := StrToIntDef(TONVIFXMLUtils.GetChildNodeValue(LCapabilitieNode,'AudioSources'),-1);         
        FCapabilities.Extension.AudioOutputs := StrToIntDef(TONVIFXMLUtils.GetChildNodeValue(LCapabilitieNode,'AudioOutputs'),-1);         
        FCapabilities.Extension.RelayOutputs := StrToIntDef(TONVIFXMLUtils.GetChildNodeValue(LCapabilitieNode,'RelayOutputs'),-1);         
                   
      end;              
    end;
  end
  else
    {$REGION 'Log'}
    {TSI:IGNORE ON}
        DoWriteLog('TONVIFManager.GetCapabilities',Format('Url [%s] Error [%d] LastResponse [%s]',[FUrl,FLastStatusCode,FLastResponse]),tpLivError);      
    {TSI:IGNORE OFF}
    {$ENDREGION}    
end;


function TONVIFManager.GetProfiles: Boolean;
var LResultStr         : String;
    LXMLDoc            : IXMLDocument;
    LSoapBodyNode      : IXMLNode;
    LProfilesNode      : IXMLNode;
    LChildNodeRoot     : IXMLNode; 
    LChildNodeNode     : IXMLNode;
    LChildNodeNode2    : IXMLNode;
    LChildNodeNode3    : IXMLNode;
    I                  : Integer;
    LProfile           : TProfile;
    LCurrentIndex      : integer;
    LSetForDefault     : Boolean;  
    LSetVSourceToken   : Boolean; 
    LTokenPtzSetted    : Boolean; 
    LNewToken          : String;
    LErrorFound        : Boolean;

    Procedure SetVideoSourceToken;
    begin
      if not LProfile.VideoSourceConfiguration.token.IsEmpty then
      begin
        if (LProfile.VideoSourceConfiguration.token.Trim = DEFAULT_TOKEN_IMAGING) or  // How can I identify without using constants?
           (LProfile.VideoSourceConfiguration.token.Trim = DEFAULT_TOKEN_IMAGING_2) 
        then
          FImaging.Token  := Format(AUTO_TOKEN_IMAGING,[LCurrentIndex+1])
        else
          FImaging.Token  := LProfile.VideoSourceConfiguration.token.Trim;

        {$REGION 'Log'}
        {TSI:IGNORE ON}
            DoWriteLog('TONVIFManager.GetProfiles',Format('Set Imaging Token to [%s], VideoSourceConfiguration token [%s] profile token [%s]',[FImaging.Token ,LProfile.VideoSourceConfiguration.token,LProfile.token]),tpLivInfo,true);      
        {TSI:IGNORE OFF}
        {$ENDREGION}                
        LTokenPtzSetted := False;
        LSetVSourceToken:= False;
      end;    
    end;
begin
  Result := False;
  ResetProfiles;
  if not UrlIsValid('TONVIFManager.GetProfiles') then Exit;
  Result := ExecuteRequest(GetUrlByType(atDeviceService), FSOAPBuilder.PrepareGetProfilesRequest, LResultStr);

  if Result then
  begin
    Result := False;
    {$REGION 'Log'}
    {TSI:IGNORE ON}
        DoWriteLog('TONVIFManager.GetProfiles',LResultStr,tpLivXMLResp,true);      
    {TSI:IGNORE OFF}
    {$ENDREGION}  
    LXMLDoc := TXMLDocument.Create(nil);
    LXMLDoc.LoadFromXML(LResultStr);

    if not IsValidSoapXML(LXMLDoc.DocumentElement,LErrorFound) then exit;
    
    LSoapBodyNode := TONVIFXMLUtils.GetSoapBody(LXMLDoc.DocumentElement);
    LProfilesNode := TONVIFXMLUtils.RecursiveFindNode(LSoapBodyNode,'GetProfilesResponse');

    if Assigned(LProfilesNode) then
    begin
      Result := LProfilesNode.ChildNodes.Count > 0;   

      SetLength(FProfiles,LProfilesNode.ChildNodes.Count);

      LCurrentIndex    := 0;  
      LSetForDefault   := not FIsFixedToken;
      LSetVSourceToken := FIsFixedToken;
      LTokenPtzSetted  := False;      
      for I := 0 to LProfilesNode.ChildNodes.Count -1 do
      begin  

        LProfile.token := String(LProfilesNode.ChildNodes[I].Attributes['token']);  
        LProfile.fixed := Boolean(StrToBoolDef(TONVIFXMLUtils.GetAttribute(LProfilesNode.ChildNodes[I],'fixed'), False));
        LProfile.name  := TONVIFXMLUtils.GetChildNodeValue(LProfilesNode.ChildNodes[I],'tt:Name');
        
        // Continue parsing TVideoSourceConfiguration
        LChildNodeRoot := TONVIFXMLUtils.RecursiveFindNode(LProfilesNode.ChildNodes[I],'VideoSourceConfiguration');
        if Assigned(LChildNodeRoot) then
        begin

          LProfile.VideoSourceConfiguration.token       := TONVIFXMLUtils.GetAttribute(LChildNodeRoot,'token');
          if Assigned(FOnPTZTokenFound) then
            FOnPTZTokenFound(LProfile.name,LProfile.VideoSourceConfiguration.token,LSetVSourceToken);
          
          LProfile.VideoSourceConfiguration.name        := TONVIFXMLUtils.GetChildNodeValue(LChildNodeRoot, 'Name'); 
          LProfile.VideoSourceConfiguration.UseCount    := StrToIntDef(TONVIFXMLUtils.GetChildNodeValue(LChildNodeRoot, 'UseCount'), -1);
          LProfile.VideoSourceConfiguration.SourceToken := TONVIFXMLUtils.GetChildNodeValue(LChildNodeRoot, 'SourceToken');
            
          LChildNodeNode := TONVIFXMLUtils.RecursiveFindNode(LChildNodeRoot,'Bounds');          
          if Assigned(LChildNodeNode) then
          begin
            LProfile.VideoSourceConfiguration.Bounds.x      := StrToIntDef(TONVIFXMLUtils.GetAttribute(LChildNodeNode,'x'), -1);
            LProfile.VideoSourceConfiguration.Bounds.y      := StrToIntDef(TONVIFXMLUtils.GetAttribute(LChildNodeNode,'y'), -1);
            LProfile.VideoSourceConfiguration.Bounds.width  := StrToIntDef(TONVIFXMLUtils.GetAttribute(LChildNodeNode,'width'), -1);
            LProfile.VideoSourceConfiguration.Bounds.height := StrToIntDef(TONVIFXMLUtils.GetAttribute(LChildNodeNode,'height'), -1);
          end;
        end;
        
        // Continue parsing TVideoEncoderConfiguration        
        LChildNodeRoot := TONVIFXMLUtils.RecursiveFindNode(LProfilesNode.ChildNodes[I],'VideoEncoderConfiguration');   
        if Assigned(LChildNodeRoot) then
        begin
          LProfile.VideoEncoderConfiguration.token           := TONVIFXMLUtils.GetAttribute(LChildNodeRoot,'token');
          LProfile.VideoEncoderConfiguration.name            := TONVIFXMLUtils.GetChildNodeValue(LChildNodeRoot, 'Name'); 
          LProfile.VideoEncoderConfiguration.UseCount        := StrToIntDef(TONVIFXMLUtils.GetChildNodeValue(LChildNodeRoot, 'UseCount'), -1);
          LProfile.VideoEncoderConfiguration.Encoding        := TONVIFXMLUtils.GetChildNodeValue(LChildNodeRoot, 'Encoding');
          LProfile.VideoEncoderConfiguration.Quality         := StrToFloatLocale(TONVIFXMLUtils.GetChildNodeValue(LChildNodeRoot, 'Quality'),-1);
          LProfile.VideoEncoderConfiguration.SessionTimeout  := TONVIFXMLUtils.GetChildNodeValue(LChildNodeRoot, 'SessionTimeout');
          
          LChildNodeNode := TONVIFXMLUtils.RecursiveFindNode(LChildNodeRoot,'Resolution');
          if Assigned(LChildNodeNode) then
          begin     
            LProfile.VideoEncoderConfiguration.Resolution.width  := StrToIntDef(TONVIFXMLUtils.GetChildNodeValue(LChildNodeNode,'Width'), -1);
            LProfile.VideoEncoderConfiguration.Resolution.height := StrToIntDef(TONVIFXMLUtils.GetChildNodeValue(LChildNodeNode,'Height'), -1);
          end;
          
          LChildNodeNode := TONVIFXMLUtils.RecursiveFindNode(LChildNodeRoot,'RateControl');  
          if Assigned(LChildNodeNode) then
          begin     
            LProfile.VideoEncoderConfiguration.RateControl.FrameRateLimit   := StrToIntDef(TONVIFXMLUtils.GetChildNodeValue(LChildNodeNode,'FrameRateLimit'), -1);
            LProfile.VideoEncoderConfiguration.RateControl.EncodingInterval := StrToIntDef(TONVIFXMLUtils.GetChildNodeValue(LChildNodeNode,'EncodingInterval'), -1);
            LProfile.VideoEncoderConfiguration.RateControl.BitrateLimit     := StrToIntDef(TONVIFXMLUtils.GetChildNodeValue(LChildNodeNode,'BitrateLimit'), -1);            
          end;   

          LChildNodeNode := TONVIFXMLUtils.RecursiveFindNode(LChildNodeRoot,'H264');    
          if Assigned(LChildNodeNode) then
          begin     
            LProfile.VideoEncoderConfiguration.H264.GovLength   := StrToIntDef(TONVIFXMLUtils.GetChildNodeValue(LChildNodeNode,'GovLength'), -1);
            LProfile.VideoEncoderConfiguration.H264.H264Profile := TONVIFXMLUtils.GetChildNodeValue(LChildNodeNode,'H264Profile')
          end;   

          LChildNodeNode := TONVIFXMLUtils.RecursiveFindNode(LChildNodeRoot,'Multicast');    
          if Assigned(LChildNodeNode) then
          begin     
            LProfile.VideoEncoderConfiguration.Multicast.Port      := StrToIntDef(TONVIFXMLUtils.GetChildNodeValue(LChildNodeNode,'Port'), -1);
            LProfile.VideoEncoderConfiguration.Multicast.TTL       := StrToIntDef(TONVIFXMLUtils.GetChildNodeValue(LChildNodeNode,'TTL'), -1);            
            LProfile.VideoEncoderConfiguration.Multicast.AutoStart := StrToBoolDef(TONVIFXMLUtils.GetChildNodeValue(LChildNodeNode,'AutoStart'),false);

            LChildNodeNode := TONVIFXMLUtils.RecursiveFindNode(LChildNodeNode,'Address'); 
            if Assigned(LChildNodeNode) then
              LProfile.VideoEncoderConfiguration.Multicast.Address.TypeAddr := TONVIFXMLUtils.GetChildNodeValue(LChildNodeNode,'Type')
          end;                                                   
        end;
        
        // Continue parsing TPTZConfiguration    
        LChildNodeRoot := TONVIFXMLUtils.RecursiveFindNode(LProfilesNode.ChildNodes[I],'PTZConfiguration');
        if Assigned(LChildNodeRoot) then
        begin
          LProfile.PTZConfiguration.token := TONVIFXMLUtils.GetAttribute(LChildNodeRoot,'token');

          if ( LProfile.PTZConfiguration.token = 'PTZToken') or
             ( LProfile.PTZConfiguration.token = 'PtzConf1')   // How can I identify without using constants?
          then
            LNewToken := LProfile.token
          else
            LNewToken := LProfile.PTZConfiguration.token;
            
          if Assigned(FOnPTZTokenFound) then
            FOnPTZTokenFound(LProfile.name,LNewToken,LSetForDefault);
            
          if LSetForDefault then
          begin
            {$REGION 'Log'}
            {TSI:IGNORE ON}
                DoWriteLog('TONVIFManager.GetProfiles',Format('Set PTZ Token to [%s], PTZConfiguration token [%s] profile token [%s]',[PTZ.Token ,LProfile.PTZConfiguration.token,LProfile.token]),tpLivInfo,true);      
            {TSI:IGNORE OFF}
            {$ENDREGION}            
            PTZ.Token       := LNewToken;
            LTokenPtzSetted := True;
          end;
          
          LSetForDefault := False;         
          LProfile.PTZConfiguration.Name      := TONVIFXMLUtils.GetChildNodeValue(LChildNodeNode,'Name');
          LProfile.PTZConfiguration.UseCount  := StrToIntDef(TONVIFXMLUtils.GetAttribute(LChildNodeRoot,'UseCount'),-1);          
          LProfile.PTZConfiguration.NodeToken := TONVIFXMLUtils.GetAttribute(LChildNodeRoot,'NodeToken');
          
          LChildNodeNode                      := TONVIFXMLUtils.RecursiveFindNode(LChildNodeRoot,'DefaultPTZSpeed');
          if Assigned(LChildNodeNode) then
          begin
            LChildNodeNode2 :=TONVIFXMLUtils.RecursiveFindNode(LChildNodeNode,'PanTilt'); 
            if Assigned(LChildNodeNode2) then            
            begin
              LProfile.PTZConfiguration.DefaultPTZSpeed.PanTilt.x := StrToFloatLocale(TONVIFXMLUtils.GetAttribute(LChildNodeNode2,'X'),-1);
              LProfile.PTZConfiguration.DefaultPTZSpeed.PanTilt.Y := StrToFloatLocale(TONVIFXMLUtils.GetAttribute(LChildNodeNode2,'Y'),-1);              
            end;  
            LChildNodeNode2 :=TONVIFXMLUtils.RecursiveFindNode(LChildNodeNode,'Zoom'); 
            if Assigned(LChildNodeNode2) then            
              LProfile.PTZConfiguration.DefaultPTZSpeed.Zoom := StrToFloatLocale(TONVIFXMLUtils.GetAttribute(LChildNodeNode2,'Zoom'),-1);
          end;
          
          LChildNodeNode := TONVIFXMLUtils.RecursiveFindNode(LChildNodeRoot,'PanTiltLimits');
          if Assigned(LChildNodeNode) then
          begin
            LChildNodeNode2 :=TONVIFXMLUtils.RecursiveFindNode(LChildNodeNode,'Range'); 
            if Assigned(LChildNodeNode2) then  
            begin          
              LProfile.PTZConfiguration.PanTiltLimits.Range.URI := TONVIFXMLUtils.GetChildNodeValue(LChildNodeNode2,'URI');
              LChildNodeNode3                                   := TONVIFXMLUtils.RecursiveFindNode(LChildNodeNode2,'XRange'); 

              if Assigned(LChildNodeNode3) then
              begin
                 LProfile.PTZConfiguration.PanTiltLimits.Range.XRange.Min := StrToIntDef(TONVIFXMLUtils.GetChildNodeValue(LChildNodeNode3,'Min'), -1);
                 LProfile.PTZConfiguration.PanTiltLimits.Range.XRange.Max := StrToIntDef(TONVIFXMLUtils.GetChildNodeValue(LChildNodeNode3,'Max'), -1);                 
              end;
              
              LChildNodeNode3  := TONVIFXMLUtils.RecursiveFindNode(LChildNodeNode2,'YRange'); 
              if Assigned(LChildNodeNode3) then
              begin
                 LProfile.PTZConfiguration.PanTiltLimits.Range.YRange.Min := StrToIntDef(TONVIFXMLUtils.GetChildNodeValue(LChildNodeNode3,'Min'), -1);
                 LProfile.PTZConfiguration.PanTiltLimits.Range.YRange.Max := StrToIntDef(TONVIFXMLUtils.GetChildNodeValue(LChildNodeNode3,'Max'), -1);                 
              end;                            
            end;              
          end;  

          LChildNodeNode := TONVIFXMLUtils.RecursiveFindNode(LChildNodeRoot,'ZoomLimits');
          if Assigned(LChildNodeNode) then
          begin
            LChildNodeNode2 :=TONVIFXMLUtils.RecursiveFindNode(LChildNodeNode,'Range'); 
            if Assigned(LChildNodeNode2) then  
            begin 
              LProfile.PTZConfiguration.ZoomLimits.Range.URI := TONVIFXMLUtils.GetChildNodeValue(LChildNodeNode2,'URI');;
              LChildNodeNode3                                := TONVIFXMLUtils.RecursiveFindNode(LChildNodeNode2,'XRange'); 

              if Assigned(LChildNodeNode3) then
              begin
                 LProfile.PTZConfiguration.ZoomLimits.Range.XRange.Min := StrToIntDef(TONVIFXMLUtils.GetChildNodeValue(LChildNodeNode3,'Min'), -1);
                 LProfile.PTZConfiguration.ZoomLimits.Range.XRange.Max := StrToIntDef(TONVIFXMLUtils.GetChildNodeValue(LChildNodeNode3,'Max'), -1);
              end;              
            end          
          end;        
         
        end;

        // TODO Continue parsing TAudioEncoderConfiguration         

        // TODO Continue parsing TVideoAnalyticsConfiguration        
        
        
        // TODO Continue parsing TExtension                
        
        FProfiles[LCurrentIndex] := LProfile;
        if LTokenPtzSetted or LSetVSourceToken then
          SetVideoSourceToken;
        Inc(LCurrentIndex);
      end;      

      if FIsFixedToken then
        SetTokenImagingByPTZToken;      
    end
    else
      {$REGION 'Log'}
      {TSI:IGNORE ON}
          DoWriteLog('TONVIFManager.GetProfiles','Profiles node not found',tpLivError);
      {TSI:IGNORE OFF}
      {$ENDREGION}      
  end
  else
    {$REGION 'Log'}
    {TSI:IGNORE ON}
        DoWriteLog('TONVIFManager.GetProfiles',Format('Url [%s] Error [%d] LastResponse [%s]',[FUrl,FLastStatusCode,FLastResponse]),tpLivError);      
    {TSI:IGNORE OFF}
    {$ENDREGION}  
end;

function TONVIFManager.GetDeviceInformation: Boolean;
var LResultStr         : String;
    LXMLDoc            : IXMLDocument;
    LSoapBodyNode      : IXMLNode;
    LErrorFound        : Boolean;

    Procedure SaveNodeInfo(const aNodeName:String;var aNodeResult:String);
    var LXMLNode    : IXMLNode;
    begin
      LXMLNode := TONVIFXMLUtils.RecursiveFindNode(LSoapBodyNode,aNodeName);

      if Assigned(LXMLNode) then
        aNodeResult := LXMLNode.Text;    
    end;
    
begin
  Result := false;
  ResetDevice;
  if not UrlIsValid('TONVIFManager.GetDeviceInformation') then Exit;  
  Result := ExecuteRequest(GetUrlByType(atDeviceService), FSOAPBuilder.PrepareGetDeviceInformationRequest, LResultStr);

  if Result then
  begin
    {$REGION 'Log'}
    {TSI:IGNORE ON}
        DoWriteLog('TONVIFManager.GetDeviceInformation',LResultStr,tpLivXMLResp,true);      
    {TSI:IGNORE OFF}
    {$ENDREGION}  
    LXMLDoc := TXMLDocument.Create(nil);
    LXMLDoc.LoadFromXML(LResultStr);

    if not IsValidSoapXML(LXMLDoc.DocumentElement,LErrorFound) then exit;
    
    LSoapBodyNode := TONVIFXMLUtils.GetSoapBody(LXMLDoc.DocumentElement);

    {Init Device information record}
    SaveNodeInfo('Manufacturer',FDevice.Manufacturer);
    SaveNodeInfo('Model',FDevice.Model);
    SaveNodeInfo('FirmwareVersion',FDevice.FirmwareVersion);    
    SaveNodeInfo('SerialNumber',FDevice.SerialNumber);    
    SaveNodeInfo('HardwareId',FDevice.HardwareId);    
    SaveNodeInfo('XAddr',FDevice.XAddr);        
  end
  else
    {$REGION 'Log'}
    {TSI:IGNORE ON}
        DoWriteLog('TONVIFManager.GetDeviceInformation',Format('Url [%s] Error [%d] LastResponse [%s]',[FUrl,FLastStatusCode,FLastResponse]),tpLivError);      
    {TSI:IGNORE OFF}
    {$ENDREGION}    
end;

function TONVIFManager.ExecuteRequest(const aAddr, aRequest: String; Var aAnswer: String): Boolean;
Var LInStream  : TStringStream; 
    LOutStream : TStringStream;
    LXMLDoc    : IXMLDocument;
    LErrorFound: Boolean;
begin
  LInStream  := TStringStream.Create(aRequest);
  Try
    LOutStream := TStringStream.Create;
    try
      Result        := ExecuteRequest(aAddr, LInStream, LOutStream);
      aAnswer       := LOutStream.DataString;  
      FLastResponse := aAnswer; 

      if not Result then
      begin
        Try
          if FLastStatusCode <> ONVIF_ERROR_DELPHI_EXCEPTION then
          begin
            LXMLDoc := TXMLDocument.Create(nil);
            LXMLDoc.LoadFromXML(aAnswer);
            if not IsValidSoapXML(LXMLDoc.DocumentElement,LErrorFound) then 
            begin
              if not LErrorFound then
              begin
                {$REGION 'Log'}
                {TSI:IGNORE ON}
                    DoWriteLog('TONVIFManager.ExecuteRequest',Format('URL [%s] Error [%d] response [%s]',[FUrl,FLastStatusCode,aAnswer]),tpLivError);      
                {TSI:IGNORE OFF}
                {$ENDREGION}
              end;
            end;
          end;
        Except on E : Exception do
          {$REGION 'Log'}
          {TSI:IGNORE ON}
              DoWriteLog('TONVIFManager.ExecuteRequest',Format('ULR [%s] [Parser XML response exception [%s], Execute request Error [%d] response [%s]',[FUrl,e.Message,FLastStatusCode,aAnswer]),tpLivError);      
          {TSI:IGNORE OFF}
          {$ENDREGION}           
        End;
          
      end;
      
      if FSaveResponseOnDisk then
        TFile.AppendAllText(FPathFileResponseOnDisk,aAnswer+sLineBreak,TEncoding.UTF8);
    finally
      FreeAndNil(LOutStream);
    end;
  Finally
    FreeAndNil(LInStream);
  End;
end;

function TONVIFManager.ExecuteRequest(const aAddr: String; const aInStream, aOutStream: TStringStream): Boolean;
Var LHTTPClient: THttpClient;
    LResponse  : IHTTPResponse;
begin  
  Result      := False;
  LHTTPClient := THttpClient.Create;
  Try
    With LHTTPClient do
    begin
      Try
        CustHeaders.Clear;
        AllowCookies          := True;
        HandleRedirects       := True;
        AutomaticDecompression:= [THTTPCompressionMethod.Deflate, THTTPCompressionMethod.GZip, THTTPCompressionMethod.Brotli, THTTPCompressionMethod.Any];
        Accept                := 'gzip, deflate';    
        ContentType           := 'application/soap+xml; charset=utf-8;';          
        ResponseTimeout       := 300000;
        LResponse             := Post(aAddr, aInStream,aOutStream);
        FLastStatusCode       := LResponse.StatusCode;
        Result                := (FLastStatusCode div 100) = 2;
      Except on E:Exception do
        begin
          FLastStatusCode := ONVIF_ERROR_DELPHI_EXCEPTION;
          FLastResponse   := E.Message;
          {$REGION 'Log'}
          {TSI:IGNORE ON}
              DoWriteLog('TONVIFManager.ExecuteRequest',Format(' Addr [%s] exception [%s] ',[aAddr,e.Message]),tpLivException);
          {TSI:IGNORE OFF}
          {$ENDREGION}
        end;
      End;        
    end;
  finally      
    FreeAndNil(LHTTPClient)
  end;
end;

procedure TONVIFManager.Reset;
begin
  ResetDevice;
  ResetProfiles;
  ResetCapabilities;
  FLastStatusCode         := 0;
  FLastResponse           := String.Empty;
    
end;

Procedure TONVIFManager.ResetProfiles;
begin
 SetLength(FProfiles,0);
end;

Procedure TONVIFManager.ResetDevice;
begin
  FDevice.Manufacturer    := String.Empty;
  FDevice.Model           := String.Empty;
  FDevice.FirmwareVersion := String.Empty;
  FDevice.SerialNumber    := String.Empty;
  FDevice.HardwareId      := String.Empty;
  FDevice.XAddr           := String.Empty;
end;

procedure TONVIFManager.ResetCapabilities;
begin
  {device}
  FCapabilities.Device.XAddr                                         := String.Empty;
  FCapabilities.Device.Network.IPFilter                              := False;
  FCapabilities.Device.Network.ZeroConfiguration                     := False;  
  FCapabilities.Device.Network.IPVersion6                            := False;   
  FCapabilities.Device.Network.DynDNS                                := False;  
  FCapabilities.Device.Network.Extension.Dot11Configuration          := False;          
  FCapabilities.Device.System.DiscoveryResolve                       := False;
  FCapabilities.Device.System.DiscoveryBye                           := False;
  FCapabilities.Device.System.RemoteDiscovery                        := False;
  FCapabilities.Device.System.SystemBackup                           := False;
  FCapabilities.Device.System.SystemLogging                          := False;        
  FCapabilities.Device.System.FirmwareUpgrade                        := False;          
  FCapabilities.Device.System.SupportedVersions.Major                := -1;
  FCapabilities.Device.System.SupportedVersions.Minor                := -1;  
  {event}
  FCapabilities.Events.XAddr                                         := String.Empty;
  FCapabilities.Events.WSSubscriptionPolicySupport                   := False;
  FCapabilities.Events.WSPullPointSupport                            := False;
  FCapabilities.Events.WSPausableSubscriptionManagerInterfaceSupport := False; 
  {Media}
  FCapabilities.Media.XAddr                                          := String.Empty;
  FCapabilities.Media.StreamingCapabilities.RTPMulticast             := False;
  FCapabilities.Media.StreamingCapabilities.RTP_TCP                  := False;
  FCapabilities.Media.StreamingCapabilities.RTP_RTSP_TCP             := False;    
  {PTZ}
  FCapabilities.PTZ.XAddr                                            := String.Empty;
  {Extension}
  FCapabilities.Extension.Search.XAddr                               := String.Empty;
  FCapabilities.Extension.Search.MetadataSearch                      := False;
  FCapabilities.Extension.Replay.XAddr                               := String.Empty;
  FCapabilities.Extension.DeviceIO.XAddr                             := String.Empty;  
  FCapabilities.Extension.Recording.XAddr                            := String.Empty;    
  FCapabilities.Extension.Recording.MaxStringLength                  := -1;    
  FCapabilities.Extension.Recording.ReceiverSource                   := false;
  FCapabilities.Extension.Recording.MediaProfileSource               := false;
  FCapabilities.Extension.Recording.DynamicRecordings                := false;
  FCapabilities.Extension.Recording.DynamicTracks                    := false;    
  FCapabilities.Extension.VideoSources                               := -1;  
  FCapabilities.Extension.VideoOutputs                               := -1;
  FCapabilities.Extension.AudioSources                               := -1;
  FCapabilities.Extension.AudioOutputs                               := -1;
  FCapabilities.Extension.RelayOutputs                               := -1;        
end;

Function TONVIFManager.IsValidSoapXML(const aRootNode :IXMLNode;var aErrorFound:Boolean):Boolean;
CONST cNodeSOAPEnvelope  = 'Envelope';
      cNodeSOAPBodyFault = 'Fault';
      cNodeFaultCode     = 'faultcode';
      cNodeFaultString   = 'faultstring';            
var LSoapBodyNode      : IXMLNode;  
    LSoapBodyFaultNode : IXMLNode; 
    LSoapBodyTmpNode   : IXMLNode;
    LFaultCode         : String;
begin
  Result       := false;
  aErrorFound  := False;

  if not Pos(cNodeSOAPEnvelope,aRootNode.NodeName) = 0  then 
  begin
    SetLastStatusCode('TONVIFManager.IsValidSoapXML',ONVIF_ERROR_SOAP_INVALID);
    exit;
  end;

  LSoapBodyNode := TONVIFXMLUtils.GetSoapBody(aRootNode);

  if not Assigned(LSoapBodyNode) then
  begin
    SetLastStatusCode('TONVIFManager.IsValidSoapXML',ONVIF_ERROR_SOAP_NOBODY);
    Exit;
  end;

  LSoapBodyFaultNode := TONVIFXMLUtils.RecursiveFindNode(LSoapBodyNode,cNodeSOAPBodyFault);

  if Assigned(LSoapBodyFaultNode) then
  begin                                                       
    LSoapBodyTmpNode := TONVIFXMLUtils.RecursiveFindNode(LSoapBodyFaultNode,cNodeFaultCode);
    if Assigned(LSoapBodyTmpNode) then    
      LFaultCode := LSoapBodyTmpNode.Text
    else
      LFaultCode := 'not found';
      
    LSoapBodyTmpNode := TONVIFXMLUtils.RecursiveFindNode(LSoapBodyFaultNode,cNodeFaultString);      
    if Assigned(LSoapBodyTmpNode) then    
    begin
      aErrorFound   := True;
      FLastResponse := Format('Faultcode [%s] faultstring [%s]',[LFaultCode,LSoapBodyTmpNode.Text])
    end
    else
    begin
      LSoapBodyTmpNode := TONVIFXMLUtils.RecursiveFindNode(LSoapBodyFaultNode,'Reason');
      if Assigned(LSoapBodyTmpNode) then      
      begin
        aErrorFound   := True;
        FLastResponse := Format('Faultcode [%s] faultstring [%s]',[LFaultCode,TONVIFXMLUtils.GetChildNodeValue(LSoapBodyTmpNode,'Text')])
      end
      else
        FLastResponse := Format('Faultcode [%s] faultstring not found',[LFaultCode])      
    end;

    {$REGION 'Log'}
    {TSI:IGNORE ON}
        DoWriteLog('TONVIFManager.IsValidSoapXML',Format('ULR [%s] SOAP error [%d] description [%s]',[FUrl,FLastStatusCode,FLastResponse]),tpLivError);
    {TSI:IGNORE OFF}
    {$ENDREGION}      
          
    exit;
  end;

  Result := True;
end;

{ TONVIFPTZManager }
constructor TONVIFPTZManager.Create(aONVIFManager: TONVIFManager);
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

function TONVIFPTZManager.StartMoveContinuous(const aDirection: TONVIF_PTZ_CommandType): Boolean;
var LCommandStr: String;  
    LResultStr : String;                                                  
begin
  Result := False;
  if not FONVIFManager.UrlIsValid('TONVIFPTZManager.StartMoveContinuous') then Exit; 
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
  Result := FONVIFManager.ExecuteRequest(FONVIFManager.GetUrlByType(atPtz), FONVIFManager.FSOAPBuilder.PrepareStartMoveContinuousRequest(FToken,LCommandStr), LResultStr);      
end;

function TONVIFPTZManager.Zoom(aInZoom: Boolean): Boolean;
var LCommand   : String;
    LResultStr : String; 
begin
  Result := False;
  if not FONVIFManager.UrlIsValid('TONVIFPTZManager.Zoom') then Exit; 
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
        
  Result := FONVIFManager.ExecuteRequest(FONVIFManager.GetUrlByType(atPtz),FONVIFManager.FSOAPBuilder.PrepareStartZoomRequest(FToken,LCommand), LResultStr);      
end;

function TONVIFPTZManager.Stop: Boolean;
var LResultStr: String;
begin
  Result := false;
  if not FONVIFManager.UrlIsValid('TONVIFPTZManager.Zoom') then Exit; 
  if not isValidToken('TONVIFPTZManager.Stop') then Exit;
  Result := FONVIFManager.ExecuteRequest(FONVIFManager.GetUrlByType(atPtz),FONVIFManager.FSOAPBuilder.PrepareStopMoveRequest(FToken), LResultStr);
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
  if not FONVIFManager.UrlIsValid('TONVIFPTZManager.LoadPresetList') then Exit; 
  if not isValidToken('TONVIFPTZManager.LoadPresetList') then Exit;

  Result := FONVIFManager.ExecuteRequest(FONVIFManager.GetUrlByType(atPTZ),FONVIFManager.FSOAPBuilder.PrepareGetPresetList(FToken),LResponseStr);

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
      LPtzPosNode   := TONVIFXMLUtils.RecursiveFindNode(LPresetListNode.ChildNodes[I],'PTZPosition');
      if Assigned(LPtzPosNode) then
      begin
         LPreset^.PTZPosition.Zoom := StrToFloatLocale(TONVIFXMLUtils.GetAttribute(TONVIFXMLUtils.RecursiveFindNode(LPtzPosNode,'Zoom'),'x'),-1);
         LPanTiltNode             := TONVIFXMLUtils.RecursiveFindNode(LPtzPosNode,'PanTilt');
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
  if not FONVIFManager.UrlIsValid('TONVIFPTZManager.GetNodes') then Exit; 
  if not isValidToken('TONVIFPTZManager.GetNodes') then Exit;

  Result := FONVIFManager.ExecuteRequest(FONVIFManager.GetUrlByType(atPTZ),FONVIFManager.FSOAPBuilder.PrepareGetNodes,LResponseStr);  

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
        FONVIFManager.DoWriteLog('TONVIFManager.GetNodes',Format('Url [%s] Error [%d] LastResponse [%s]',[FONVIFManager.FUrl,FONVIFManager.FLastStatusCode,FONVIFManager.FLastResponse]),tpLivError);      
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
  if not FONVIFManager.UrlIsValid('TONVIFPTZManager.GetStatus') then Exit; 
  if not isValidToken('TONVIFPTZManager.GetStatus') then Exit;

  Result := FONVIFManager.ExecuteRequest(FONVIFManager.GetUrlByType(atPTZ),FONVIFManager.FSOAPBuilder.PrepareGetStatus(FToken),LResponseStr);
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
        FONVIFManager.DoWriteLog('TONVIFManager.GetStatus',Format('Url [%s] Error [%d] LastResponse [%s]',[FONVIFManager.FUrl,FONVIFManager.FLastStatusCode,FONVIFManager.FLastResponse]),tpLivError);      
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
  
  if not FONVIFManager.UrlIsValid('TONVIFPTZManager.GotoHomePosition') then Exit; 
  if not isValidToken('TONVIFPTZManager.GotoHomePosition') then Exit;

  Result := FONVIFManager.ExecuteRequest(FONVIFManager.GetUrlByType(atPTZ),FONVIFManager.FSOAPBuilder.PrepareGotoHome(FToken),LResponseStr);
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

  if not FONVIFManager.UrlIsValid('TONVIFPTZManager.GotoHomePosition') then Exit; 
  if not isValidToken('TONVIFPTZManager.GotoHomePosition') then Exit;
  Result := FONVIFManager.ExecuteRequest(FONVIFManager.GetUrlByType(atPTZ),FONVIFManager.FSOAPBuilder.PrepareSetHomePosition(FToken),LResponseStr);
end;

function TONVIFPTZManager.GoToPreset(const aIndexPreset: Integer) : Boolean;
var LResponseStr : String;
begin
  Result := False;  
  if not FONVIFManager.UrlIsValid('TONVIFPTZManager.GoToPreset') then Exit; 
  if not isValidToken('TONVIFPTZManager.GoToPreset') then Exit;
  
  if not inRange(aIndexPreset,0,FPresentList.Count -1) then
  begin
    FONVIFManager.SetLastStatusCode('TONVIFPTZManager.GoToPreset',ONVIF_ERROR_PTZ_INVALID_PRESET_INDEX);
    Exit;
  end;

  Result := FONVIFManager.ExecuteRequest(FONVIFManager.GetUrlByType(atPTZ),FONVIFManager.FSOAPBuilder.PrepareGotoPreset(FToken,FPresentList[aIndexPreset]^.Token),LResponseStr);
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
  if not FONVIFManager.UrlIsValid('TONVIFPTZManager.SetPreset') then Exit; 
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

  Result := FONVIFManager.ExecuteRequest(FONVIFManager.GetUrlByType(atPTZ),FONVIFManager.FSOAPBuilder.PrepareSetPreset(FToken,LNewSetPresetToken,aPresetName)  ,LResponseStr);

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
      FONVIFManager.SetTokenImagingByPTZToken 
    end;
  end;
end;

function TONVIFPTZManager.RemovePreset(const aIndexPreset: Integer): Boolean;
var LResponseStr : String;
begin
  Result := False;  
  if not FONVIFManager.UrlIsValid('TONVIFPTZManager.RemovePreset') then Exit; 
  if not isValidToken('TONVIFPTZManager.RemovePreset') then Exit;
  
  if not inRange(aIndexPreset,0,FPresentList.Count -1) then
  begin
    FONVIFManager.SetLastStatusCode('TONVIFPTZManager.RemovePreset',ONVIF_ERROR_PTZ_INVALID_PRESET_INDEX);    
    Exit;
  end;

  Result := FONVIFManager.ExecuteRequest(FONVIFManager.GetUrlByType(atPTZ),FONVIFManager.FSOAPBuilder.PrepareRemovePreset(FToken,FPresentList[aIndexPreset].Token),LResponseStr);

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

{ TONVIFImagingManager }
constructor TONVIFImagingManager.Create(aONVIFManager: TONVIFManager);
begin
  FSupportedInfo := TSupportedImagingInfo.Create(self);
  FONVIFManager  := aONVIFManager;
  FToken         := String.Empty;
  ResetImagingSettings;  
end;

function TONVIFImagingManager.GetCapabilities: Boolean;
var LResponseStr : String;
begin

  Result := False;  
  if not FONVIFManager.UrlIsValid('TONVIFImagingManager.GetCapabilities') then Exit; 
  if not isValidToken('TONVIFImagingManager.GetCapabilities') then Exit;

  Result := FONVIFManager.ExecuteRequest(FONVIFManager.GetUrlByType(atImaging),FONVIFManager.FSOAPBuilder.PrepareImagingCapabilities,LResponseStr);

  if Result then 
  begin
    {$REGION 'Log'}
    {TSI:IGNORE ON}
      FONVIFManager.DoWriteLog('TONVIFImagingManager.GetCapabilities',LResponseStr,tpLivXMLResp,true);      
    {TSI:IGNORE OFF}
    {$ENDREGION}
    //TODO Parser
    FSupportImageStabilization                     := False;
    FSupportImagingPresets                         := False;    
  end;

end;

procedure TONVIFImagingManager.ResetImagingSettings;
begin
  {Focus}
  FFocusSettings.Options.Continuous.Speed.Min    := -1;
  FFocusSettings.Options.Continuous.Speed.Max    := -1;
  FFocusSettings.Options.Continuous.Supported    := False; 
  FFocusSettings.Options.Relative.Speed.Min      := -1;
  FFocusSettings.Options.Relative.Speed.Max      := -1;
  FFocusSettings.Options.Relative.Supported      := False; 
  FFocusSettings.Options.Absolute.Speed.Min      := -1;
  FFocusSettings.Options.Absolute.Speed.Max      := -1;
  FFocusSettings.Options.Absolute.Supported      := False; 

  FSupportImageStabilization                     := False;
  FSupportImagingPresets                         := False;
  FImagingSettings.BacklightCompensation         := False;
  FImagingSettings.Brightness                    := -1;
  FImagingSettings.ColorSaturation               := -1;
  FImagingSettings.Contrast                      := -1; 
  FImagingSettings.Exposure.Mode                 := String.Empty;   
  FImagingSettings.Exposure.MinExposureTime      := -1;
  FImagingSettings.Exposure.MaxExposureTime      := -1;
  FImagingSettings.Exposure.MinIris              := -1;  
  FImagingSettings.Exposure.MaxIris              := -1;  
  FImagingSettings.Exposure.MinGain              := -1;
  FImagingSettings.Exposure.MaxGain              := -1;
  FImagingSettings.Exposure.Iris                 := -1;  
  FImagingSettings.Exposure.Gain                 := -1;  
  FImagingSettings.Focus.AutoFocusMode           := ifmUnknown;
  FImagingSettings.Focus.DefaultSpeed            := -1;
  FImagingSettings.IrCutFilter                   := icfmUnknown;
  FImagingSettings.Sharpness                     := -1;
  FImagingSettings.WideDynamicRange.Mode         := String.Empty; 
  FImagingSettings.WhiteBalance.Mode             := String.Empty;   
  FImagingSettings.Extension.Defogging.Mode      := String.Empty;   
  FImagingSettings.Extension.Defogging.Level     := -1;
  FImagingSettings.Extension.NoiseReduction.Level:= -1;  
 
end;


destructor TONVIFImagingManager.Destroy;
begin
  FreeAndNil(FSupportedInfo);
  inherited;
end;

Function TONVIFImagingManager.GetMoveOptions:Boolean;
var LResponseStr       : String;
    LXMLDoc            : IXMLDocument;
    LSoapBodyNode      : IXMLNode;
    LMoveOptionNode    : IXMLNode;
    LErrorFound        : Boolean;
    LNodeSpeed         : IXMLNode;
begin
  Result := FONVIFManager.ExecuteRequest(FONVIFManager.GetUrlByType(atImaging),FONVIFManager.FSOAPBuilder.PrepareImagingMoveOptions(FToken),LResponseStr);  
  if Result then 
  begin
    {$REGION 'Log'}
    {TSI:IGNORE ON}
        FONVIFManager.DoWriteLog('TONVIFImagingManager.GetImagingSettings',LResponseStr,tpLivXMLResp,true);      
    {TSI:IGNORE OFF}
    {$ENDREGION}
    Result  := False;
    LXMLDoc := TXMLDocument.Create(nil);
    LXMLDoc.LoadFromXML(LResponseStr);
    if not FONVIFManager.IsValidSoapXML(LXMLDoc.DocumentElement,LErrorFound) then exit;
     
    LSoapBodyNode    := TONVIFXMLUtils.GetSoapBody(LXMLDoc.DocumentElement);
 
    LMoveOptionNode := TONVIFXMLUtils.RecursiveFindNode(LSoapBodyNode,'Relative');
    if Assigned(LMoveOptionNode) then
    begin
      //FFocusSettings.Options.Relative.Distance     
      FFocusSettings.Options.Relative.Supported := True;
      LNodeSpeed                                := TONVIFXMLUtils.RecursiveFindNode(LMoveOptionNode,'Speed');
      if Assigned(LNodeSpeed) then
      begin
        FFocusSettings.Options.Relative.Speed.Min := StrToFloatLocale(TONVIFXMLUtils.GetChildNodeValue(LNodeSpeed,'Min'),-1);  
        FFocusSettings.Options.Relative.Speed.Max := StrToFloatLocale(TONVIFXMLUtils.GetChildNodeValue(LNodeSpeed,'Max'),-1);
      end;
    end;

    LMoveOptionNode := TONVIFXMLUtils.RecursiveFindNode(LSoapBodyNode,'Absolute');
    if Assigned(LMoveOptionNode) then
    begin
      //TODO FFocusSettings.Options.Absolute.Position 
      FFocusSettings.Options.Absolute.Supported := True;
      LNodeSpeed                                := TONVIFXMLUtils.RecursiveFindNode(LMoveOptionNode,'Speed');
      if Assigned(LNodeSpeed) then
      begin
        FFocusSettings.Options.Absolute.Speed.Min := StrToFloatLocale(TONVIFXMLUtils.GetChildNodeValue(LNodeSpeed,'Min'),-1);  
        FFocusSettings.Options.Absolute.Speed.Max := StrToFloatLocale(TONVIFXMLUtils.GetChildNodeValue(LNodeSpeed,'Max'),-1);
      end;
    end;

    LMoveOptionNode := TONVIFXMLUtils.RecursiveFindNode(LSoapBodyNode,'Continuous');
    if Assigned(LMoveOptionNode) then
    begin
      FFocusSettings.Options.Continuous.Supported := True;
      LNodeSpeed                                  := TONVIFXMLUtils.RecursiveFindNode(LMoveOptionNode,'Speed');
      if Assigned(LNodeSpeed) then
      begin
        FFocusSettings.Options.Continuous.Speed.Min := StrToFloatLocale(TONVIFXMLUtils.GetChildNodeValue(LNodeSpeed,'Min'),-1);  
        FFocusSettings.Options.Continuous.Speed.Max := StrToFloatLocale(TONVIFXMLUtils.GetChildNodeValue(LNodeSpeed,'Max'),-1);
      end;
    end;
    Result := True;  
  end;
end;

function TONVIFImagingManager.GetStatus: Boolean;
var LResponseStr       : String;
    LXMLDoc            : IXMLDocument;
    LSoapBodyNode      : IXMLNode;
    LGestStatus        : IXMLNode;
    LErrorFound        : Boolean;
    I                  : Integer;
begin
  Result := False;  
  
  Result := FONVIFManager.ExecuteRequest(FONVIFManager.GetUrlByType(atPTZ),FONVIFManager.FSOAPBuilder.PrepareImagingGetStatus(FToken),LResponseStr);
  if Result then
  begin
    Result := False;  
    {$REGION 'Log'}
    {TSI:IGNORE ON}
        FONVIFManager.DoWriteLog('TONVIFImagingManager.GetStatus',LResponseStr,tpLivXMLResp,true);      
    {TSI:IGNORE OFF}
    {$ENDREGION}
    LXMLDoc := TXMLDocument.Create(nil);
    LXMLDoc.LoadFromXML(LResponseStr);

    if not FONVIFManager.IsValidSoapXML(LXMLDoc.DocumentElement,LErrorFound) then exit;
    
    LSoapBodyNode   := TONVIFXMLUtils.GetSoapBody(LXMLDoc.DocumentElement);
    Exit;
    LGestStatus  := TONVIFXMLUtils.RecursiveFindNode(LSoapBodyNode,'GetStatusResponse');

    for I := 0 to LGestStatus.ChildNodes.Count -1 do
    begin
        {$REGION 'Log'}
        {TSI:IGNORE ON}
            FONVIFManager.DoWriteLog('TONVIFImagingManager.GetStatus',Format('Unsupported node name [%s]',[LGestStatus.ChildNodes[I].DOMNode.localName]),tpLivWarning);      
        {TSI:IGNORE OFF}
        {$ENDREGION}
    end;      
  end
  else
    {$REGION 'Log'}
    {TSI:IGNORE ON}
        FONVIFManager.DoWriteLog('TONVIFImagingManager.GetStatus',Format('Url [%s] Error [%d] LastResponse [%s]',[FONVIFManager.FUrl,FONVIFManager.FLastStatusCode,FONVIFManager.FLastResponse]),tpLivError);      
    {TSI:IGNORE OFF}
    {$ENDREGION}      
  
end;


function TONVIFImagingManager.GetImagingSettings: Boolean;
var LResponseStr       : String;
    LXMLDoc            : IXMLDocument;
    LSoapBodyNode      : IXMLNode;
    LImgSettingsNode   : IXMLNode;
    LNodeTmp1          : IXMLNode;
    LNodeTmp2          : IXMLNode;   
    LTmpStrValue       : String; 
    LErrorFound        : Boolean;
    I                  : Integer;
begin
  Result := False;  

  ResetImagingSettings;
  if not FONVIFManager.UrlIsValid('TONVIFImagingManager.GetImagingSettings') then Exit; 
  if not isValidToken('TONVIFImagingManager.GetImagingSettings') then Exit;

  if not GetCapabilities then exit;

  if GetMoveOptions then
  begin
    if SupportedInfo.FocusSupported then
      GetStatus;
  end;

  Result := FONVIFManager.ExecuteRequest(FONVIFManager.GetUrlByType(atImaging),FONVIFManager.FSOAPBuilder.PrepareGetImagingSettings(FToken),LResponseStr);

  if Result then 
  begin
    {$REGION 'Log'}
    {TSI:IGNORE ON}
        FONVIFManager.DoWriteLog('TONVIFImagingManager.GetImagingSettings',LResponseStr,tpLivXMLResp,true);      
    {TSI:IGNORE OFF}
    {$ENDREGION}
    Result  := False;
    LXMLDoc := TXMLDocument.Create(nil);
    LXMLDoc.LoadFromXML(LResponseStr);

    if not FONVIFManager.IsValidSoapXML(LXMLDoc.DocumentElement,LErrorFound) then exit;
    
    LSoapBodyNode    := TONVIFXMLUtils.GetSoapBody(LXMLDoc.DocumentElement);
    LImgSettingsNode := TONVIFXMLUtils.RecursiveFindNode(LSoapBodyNode,'ImagingSettings');

    if Assigned(LImgSettingsNode) then
    begin
      LNodeTmp1 := TONVIFXMLUtils.RecursiveFindNode(LImgSettingsNode,'BacklightCompensation');
      if Assigned(LNodeTmp1) then
        FImagingSettings.BacklightCompensation := StrToBoolDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'Mode'),False);
      
      FImagingSettings.Brightness      := StrToIntDef(TONVIFXMLUtils.GetChildNodeValue(LImgSettingsNode,'Brightness'),-1);
      FImagingSettings.ColorSaturation := StrToIntDef(TONVIFXMLUtils.GetChildNodeValue(LImgSettingsNode,'ColorSaturation'),-1);      
      FImagingSettings.Contrast        := StrToIntDef(TONVIFXMLUtils.GetChildNodeValue(LImgSettingsNode,'ColorSaturation'),-1);      
      LNodeTmp1                        := TONVIFXMLUtils.RecursiveFindNode(LImgSettingsNode,'Exposure');
      if Assigned(LNodeTmp1) then
      begin
        FImagingSettings.Exposure.Mode            := TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'Mode');
        FImagingSettings.Exposure.MinExposureTime := StrToFloatLocale(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'MinExposureTime'),-1);
        FImagingSettings.Exposure.MaxExposureTime := StrToFloatLocale(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'MaxExposureTime'),-1);      
        FImagingSettings.Exposure.MinIris         := StrToFloatLocale(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'MinIris'),-1);      
        FImagingSettings.Exposure.MaxIris         := StrToFloatLocale(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'MaxIris'),-1);      
        FImagingSettings.Exposure.MinGain         := StrToFloatLocale(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'MinGain'),-1);      
        FImagingSettings.Exposure.MaxGain         := StrToFloatLocale(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'MaxGain'),-1);      
        FImagingSettings.Exposure.Iris            := StrToFloatLocale(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'Iris'),-1);      
        FImagingSettings.Exposure.Gain            := StrToFloatLocale(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'Gain'),-1);      
      end;
      
      LNodeTmp1 := TONVIFXMLUtils.RecursiveFindNode(LImgSettingsNode,'Focus');
      if Assigned(LNodeTmp1) then
      begin
        LTmpStrValue := TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'AutoFocusMode').Trim;
        if SameText(LTmpStrValue,'MANUAL') then
          FImagingSettings.Focus.AutoFocusMode := ifmManual
        else if SameText(LTmpStrValue,'AUTO') then
          FImagingSettings.Focus.AutoFocusMode := ifmAutoMode
        else if SameText(LTmpStrValue,'DEFAULTSPEED') then
          FImagingSettings.Focus.AutoFocusMode := ifmDefaultSpeed
        else if SameText(LTmpStrValue,'NEARLIMIT') then
          FImagingSettings.Focus.AutoFocusMode := ifmNearLimit
        else if SameText(LTmpStrValue,'FARLIMIT') then
          FImagingSettings.Focus.AutoFocusMode := ifmFarLimit;
      
        FImagingSettings.Focus.DefaultSpeed := StrToIntDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'DefaultSpeed'),-1);      
      end;

      LTmpStrValue := TONVIFXMLUtils.GetChildNodeValue(LImgSettingsNode,'IrCutFilter').Trim;
      if SameText(LTmpStrValue,'AUTO') then
        FImagingSettings.IrCutFilter := icfmAuto
      else if SameText(LTmpStrValue,'ON') then
        FImagingSettings.IrCutFilter := icfmON
      else if SameText(LTmpStrValue,'OFF') then
        FImagingSettings.IrCutFilter := icfmOFF;

      FImagingSettings.Sharpness := StrToIntDef(TONVIFXMLUtils.GetChildNodeValue(LImgSettingsNode,'Sharpness'),-1); 
      
      LNodeTmp1 := TONVIFXMLUtils.RecursiveFindNode(LImgSettingsNode,'WideDynamicRange');
      if Assigned(LNodeTmp1) then
        FImagingSettings.WideDynamicRange.Mode := TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'Mode');

      LNodeTmp1 := TONVIFXMLUtils.RecursiveFindNode(LImgSettingsNode,'WhiteBalance');
      if Assigned(LNodeTmp1) then      
        FImagingSettings.WhiteBalance.Mode := TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'Mode');        

      LNodeTmp1 := TONVIFXMLUtils.RecursiveFindNode(LImgSettingsNode,'Extension',True);
      if Assigned(LNodeTmp1) then      
      begin
        for I := 0 to LNodeTmp1.ChildNodes.Count -1 do
        begin
          if not SameText(LNodeTmp1.ChildNodes[I].DOMNode.localName,'Defogging') and 
             not SameText(LNodeTmp1.ChildNodes[I].DOMNode.localName,'NoiseReduction') 
          then
            {$REGION 'Log'}
            {TSI:IGNORE ON}
                FONVIFManager.DoWriteLog('TONVIFImagingManager.GetImagingSettings',Format('Unsupported node name [%s]',[LNodeTmp1.ChildNodes[I].DOMNode.localName]),tpLivWarning);      
            {TSI:IGNORE OFF}
            {$ENDREGION}                                                
        end;          
        LNodeTmp2 := TONVIFXMLUtils.RecursiveFindNode(LNodeTmp1,'Defogging');
        if Assigned(LNodeTmp2) then
        begin
          FImagingSettings.Extension.Defogging.Mode  := TONVIFXMLUtils.GetChildNodeValue(LNodeTmp2,'Mode');        
          FImagingSettings.Extension.Defogging.Level := StrToFloatLocale( TONVIFXMLUtils.GetChildNodeValue(LNodeTmp2,'Level'),-1);                  
        end;

        LNodeTmp2 := TONVIFXMLUtils.RecursiveFindNode(LNodeTmp1,'NoiseReduction');
        if Assigned(LNodeTmp2) then
          FImagingSettings.Extension.NoiseReduction.Level := StrToFloatLocale( TONVIFXMLUtils.GetChildNodeValue(LNodeTmp2,'Level'),-1);                  
      end;
    end;

    Result := True;
  end;
end;

function TONVIFImagingManager.isValidToken(const aMathodNameRequest: String): Boolean;
begin
  Result := False;
  if FToken.Trim.IsEmpty then
  begin
    FONVIFManager.SetLastStatusCode(aMathodNameRequest,ONVIF_ERROR_PTZ_IMMAGING_IS_EMPTY);    
    Exit;
  end;
  Result := True;  
end;

procedure TONVIFImagingManager.SetToken(const aValue: String);
begin
  if FToken <> aValue then
  begin
    FToken := aValue.Trim;
    {$REGION 'Log'}
    {TSI:IGNORE ON}
        FONVIFManager.DoWriteLog('TONVIFImagingManager.SetToken',Format('Imaging Token  [%s]',[Token]),tpLivInfo,true);      
    {TSI:IGNORE OFF}
    {$ENDREGION}  
    if aValue.Trim.IsEmpty then
      ResetImagingSettings
    else
      GetImagingSettings; 
  end;
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


{ TSupportedImagingInfo }

constructor TSupportedImagingInfo.Create(aOwnerImagingManager: TONVIFImagingManager);
begin
  FOwnerImagingManager := aOwnerImagingManager;
end;

function TSupportedImagingInfo.GetFocusSupported: Boolean;
begin
 Result := FOwnerImagingManager.FFocusSettings.Options.Relative.Supported or 
           FOwnerImagingManager.FFocusSettings.Options.Absolute.Supported or
           FOwnerImagingManager.FFocusSettings.Options.Continuous.Supported;
             
end;

end.

