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
  System.Classes, System.SysUtils, System.SyncObjs, System.Messaging,
  System.IOUtils, IdUDPServer, IdGlobal, Soap.XSBuiltIns, IdSocketHandle,
  IdAuthenticationDigest, Winsock, XmlDoc, XmlIntf, XMLDom,System.Math, System.NetConsts,
  ONVIF.Structure.Device, ONVIF.Structure.Profile,System.Net.HttpClient,System.net.UrlClient,
  ONVIF.Structure.Capabilities,ONVIF.Structure.PTZ;

CONST ONVIF_ERROR_URL_EMPTY                    = -1000;
      ONVIF_ERROR_SOAP_INVALID                 = -1001;
      ONVIF_ERROR_SOAP_NOBODY                  = -1002;
      ONVIF_ERROR_SOAP_FAULTCODE_NOT_FOUND     = -1003;     
      ONVIF_ERROR_PTZ_INVALID_PRESET_INDEX     = -1004;
      ONVIF_ERROR_DELPHI_EXCEPTION             = -1005;
      ONVIF_ERROR_PTZ_TOKEN_IS_EMPTY           = -1006;
      ONVIF_ERROR_PTZ_PRESETNAME_IS_EMPTY      = -1007;
      
Type

  {   
    TODO 
      
      
      - Imaging 
         -- IRCut 
         -- IRIS 
         -- Focus
         others --> https://www.onvif.org/specs/srv/img/ONVIF-Imaging-Service-Spec.pdf?441d4a&441d4a
         
      - PTZ
        -- Preset 
            -- SetPreset

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
  ///   Specifies the type of ONVIF address, including device service, media, and PTZ.
  /// </summary>
  TONVIFAddrType = (atDeviceService, atMedia, atPtz);

  /// <summary>
  ///   Specifies the type of ONVIF PTZ (Pan-Tilt-Zoom) command, including left, top, right,
  ///   bottom, top-right, top-left, bottom-left, and bottom-right.
  /// </summary>
  TONVIF_PTZ_CommandType = (opcNone,opcLeft, opcTop, opcRight, opcBotton, opcTopRight, opcTopLeft, opcBottonLeft, opcBottonRight);

  /// <summary>
  ///   Enumerates the types of PTZ movement supported by ONVIF.
  /// </summary>
  /// <remarks>
  ///   Possible values are: Continuous Move, Relative Move, and Absolute Move.
  /// </remarks>  
  TONVIF_PTZ_MoveType = (opmvContinuousMove,opmvtRelativeMove,opmvtAbsoluteMove);

  /// <summary>
  ///   Enumerates the logging levels for ONVIF events.
  /// </summary>
  /// <remarks>
  ///   Possible values are: Information, Error, Warning, and Exception.
  /// </remarks>
  TPONVIFLivLog = (tpLivInfo,tpLivError,tpLivWarning,tpLiveException);

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
  
  TONVIFManager = class;
    
  TONVIFPTZManager = Class

  private
    FONVIFManager : TONVIFManager;
    FPresentList  : TPTZPresetList;
    /// <summary>
    ///   Prepares the ONVIF PTZ command for retrieving the list of preset positions.
    /// </summary>
    /// <returns>ONVIF PTZ command string for getting the list of preset positions.</returns>    
    function PrepareGetPresetList: String;   

    /// <summary>
    ///   Prepares the ONVIF PTZ command for moving the camera to the specified preset position.
    /// </summary>
    /// <param name="aIndexPreset">Index of the preset position to which the camera should move.</param>
    /// <returns>ONVIF PTZ command string for moving to the specified preset position.</returns>    
    function PrepareGotoPreset(const aTokenPreset : String):String;

    /// <summary>
    ///   Prepares the ONVIF PTZ command for removing the specified preset position.
    /// </summary>
    /// <param name="aIndexPreset">Index of the preset position to be removed.</param>
    /// <returns>ONVIF PTZ command string for removing the specified preset position.</returns>    
    function PrepareRemovePreset(const aTokenPreset: String): String;     

    /// <summary>
    ///   Prepares an ONVIF PTZ (Pan-Tilt) start move request based on the specified command.
    /// </summary>
    /// <param name="aCommand">
    ///   The command to be included in the PTZ start move request.
    /// </param>
    /// <returns>
    ///   An XML-formatted string representing the PTZ start move request.
    /// </returns>
    function PrepareStartMoveRequest(const aDirection: String): String;

    /// <summary>
    ///   Prepares an ONVIF PTZ stop move request.
    /// </summary>
    /// <returns>
    ///   An XML-formatted string representing the PTZ stop move request.
    /// </returns>
    function PrepareStopMoveRequest: String;

    /// <summary>
    ///   Prepares an ONVIF PTZ start zoom request based on the specified command.
    /// </summary>
    /// <param name="aCommand">
    ///   The command to be included in the PTZ start zoom request.
    /// </param>
    /// <returns>
    ///   An XML-formatted string representing the PTZ start zoom request.
    /// </returns>
    function PrepareStartZoomRequest(const aDirection: String): String;
    
    /// <summary>
    ///   Prepares an ONVIF PTZ GoToHomePosition
    /// </summary>
    /// <returns>
    ///   An XML-formatted string representing the PTZ GoToHomePosition request.
    /// </returns>    
    function PrepareGotoHome: String;

    /// <summary>
    ///   Prepares an ONVIF PTZ SetHomePosition
    /// </summary>
    /// <returns>
    ///   An XML-formatted string representing the PTZ SetHomePosition request.
    /// </returns>       
    function PrepareSetHomePosition: String;
    
    /// <summary>
    ///   Prepares an ONVIF PTZ SetPreset
    /// </summary>
    /// <returns>
    ///   An XML-formatted string representing the PTZ SetPreset request.
    /// </returns>        
    function PrepareSetPreset(const aTokenPreset, aPresetName: String): String;
    
    function isValidToken(const aMathodNameRequest:String): Boolean;
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

    /// </summary>
    /// <param name="aInZoom">
    ///   Indicates whether it is an "in" zoom operation (True) or "out" zoom operation (False).
    /// </param>
    /// <param name="aResultStr">
    ///   Returns the result string after executing the PTZ start zoom operation.
    /// <returns>
    ///   True if the PTZ start zoom operation is executed successfully; False otherwise.
    /// </returns>
    function Zoom(aMoveType:TONVIF_PTZ_MoveType;aInZoom: Boolean): Boolean;

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
    /// <param name="aResultStr">
    ///   Returns the result string after executing the PTZ move operation.
    /// </param>
    /// <returns>
    ///   True if the PTZ move operation is executed successfully; False otherwise.
    /// </returns>
    function StartMove(aMoveType:TONVIF_PTZ_MoveType;const aDirection: TONVIF_PTZ_CommandType): Boolean; //TODO Timeout and rename in ConinuosMove

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
    
  End;
    
  /// <summary>
  ///   Represents a manager class for handling ONVIF-related functionalities.
  /// </summary>  
  TONVIFManager = class
  private

    FUrl                   : String;
    FLogin                 : String;
    FPassword              : String;
    FToken                 : String;
    FLastResponse          : String;
    FPathFileResponseOnDisk: String;
    FSaveResponseOnDisk    : Boolean;
    FIsFixedToken          : Boolean;
    FLastStatusCode        : Integer;
    FSpeed                 : Byte;
    FDevice                : TDeviceInformation;

    FProfiles              : TProfiles;
    FCapabilities          : TCapabilitiesONVIF;
    FPTZ                   : TONVIFPTZManager;
    {Event}
    FOnWriteLog            : TEventWriteLog;
    FOnGetAllInfo          : TNotifyEvent; 
    FOnProfileTokenFound   : TEventTokenFound;
    
    /// <summary>
    ///   Calculates the password digest based on the provided parameters.
    /// </summary>
    /// <param name="aPasswordDigest">
    ///   Variable to store the calculated password digest.
    /// </param>
    /// <param name="aNonce">
    ///   String containing the nonce value.
    /// </param>
    /// <param name="aCreated">
    ///   String containing the created timestamp.
    /// </param>
    /// <remarks>
    ///   Ensure that the input parameters are valid and properly formatted.
    ///   The calculated password digest will be stored in the provided variable.
    /// </remarks>
    procedure GetPasswordDigest( Var aPasswordDigest, aNonce, aCreated: String);

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
    ///   Calculates the SHA-1 hash of the provided byte array.
    /// </summary>
    /// <param name="aData">
    ///   The input data for which the SHA-1 hash will be calculated.
    /// </param>
    /// <returns>
    ///   The calculated SHA-1 hash as a byte array.
    /// </returns>
    /// <remarks>
    ///   This function uses the SHA-1 hashing algorithm to generate a hash
    ///   for the provided input data. Ensure that the input data is valid
    ///   and properly formatted.
    /// </remarks>    
    function SHA1(const aData: TBytes): TBytes;
    
    /// <summary>
    ///   Returns an XML string representing a SOAP connection.
    /// </summary>
    /// <returns>
    ///   A string containing the details of the SOAP connection in XML format.
    /// </returns>    
    function GetSoapXMLConnection:String;     

    /// <summary>
    ///   Prepares an XML string representing a request to retrieve profiles.
    /// </summary>
    /// <returns>
    ///   The XML string representing the GetProfiles request.
    /// </returns>
    /// <remarks>
    ///   This function generates an XML string that can be used as a request
    ///   to retrieve profiles. The format and structure of the XML may depend
    ///   on the specific requirements of the system or API you are working with.
    /// </remarks>    
    function PrepareGetProfilesRequest: String;
        
    /// <summary>
    ///   Prepares and returns an XML string representing a request for device information.
    /// </summary>
    /// <returns>
    ///   A string containing the XML-formatted request for device information.
    /// </returns>
    function PrepareGetDeviceInformationRequest: String;

    /// <summary>
    ///   Resets the state or configuration of internal record like TDeviceInformation.
    /// </summary>    
    procedure Reset;

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
    ///   Checks and handles an auxiliary command.
    /// </summary>
    procedure CheckAuxiliaryCommand;    

    /// <summary>
    ///   Sets the URL used for ONVIF communication.
    /// </summary>
    /// <param name="aValue">
    ///   The URL to be set.
    /// </param>
    procedure SetUrl(const aValue: String);

    /// <summary>
    ///   Prepares a GetCapabilities request for ONVIF communication.
    /// </summary>
    /// <returns>
    ///   The prepared GetCapabilities request string.
    /// </returns>
    function PrepareGetCapabilitiesRequest: String;
   

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
    function IsValidSoapXML(const aRootNode: IXMLNode): Boolean;

    /// <summary>
    /// Retrieves the SOAP body node from the given SOAP XML document.
    /// </summary>
    /// <param name="aRootNode">The root node of the SOAP XML document.</param>
    /// <returns>The SOAP body node.</returns>
    function GetSoapBody(const aRootNode: IXMLNode): IXMLNode;

    /// <summary>
    /// Recursively searches for an XML node with the specified name within the given XML node.
    /// </summary>
    /// <param name="ANode">The XML node to start the search from.</param>
    /// <param name="aSearchNodeName">The name of the XML node to search for.</param>
    /// <returns>The found XML node or nil if not found.</returns>
    function RecursiveFindNode(ANode: IXMLNode; const aSearchNodeName: string;const aScanAllNode: Boolean=False): IXMLNode;
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
    ///   Executes the GetAllInfo event, triggering the retrieval of ONVIF information by ReadInfoAsync.
    /// </summary>    
    procedure DoGetAllInfo;

    /// <summary>
    ///   Retrieves the value of a child node within a specified parent node.
    /// </summary>
    /// <param name="ParentNode">
    ///   The parent XML node from which to retrieve the child node value.
    /// </param>
    /// <param name="ChildNodeName">
    ///   The name of the child node whose value is to be retrieved.
    /// </param>
    /// <returns>
    ///   The string value of the specified child node if found; otherwise, an empty string.
    /// </returns>
    /// <remarks>
    ///   Use this function to conveniently obtain the value of a specific child node within
    ///   a given parent node. If the specified child node does not exist, the function
    ///   returns an empty string.
    /// </remarks>    
    function GetChildNodeValue(const ParentNode: IXMLNode;const ChildNodeName: string): string;


    /// <summary>
    ///   Retrieves the value of a specified attribute from an XML node.
    /// </summary>
    /// <param name="Node">
    ///   The XML node from which to retrieve the attribute value.
    /// </param>
    /// <param name="AttributeName">
    ///   The name of the attribute whose value is to be retrieved.
    /// </param>
    /// <returns>
    ///   The string value of the specified attribute if found; otherwise, an empty string.
    /// </returns>
    /// <remarks>
    ///   Use this function to conveniently obtain the value of a specific attribute within
    ///   a given XML node. If the specified attribute does not exist, the function returns
    ///   an empty string.
    /// </remarks>    
    function GetAttribute(const Node: IXMLNode;const AttributeName: string): string;       
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
    ///   Gets or sets the token of the ONVIF camera.
    /// </summary>    
    property Token                   : String              read FToken                   write FToken;
        
    
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
    ///   The <c>OnGetAllInfo</c> event is triggered when the GetAllInfo method is executed, facilitating
    ///   the retrieval of detailed ONVIF information. By assigning a handler to this event, the application
    ///   can respond to the completion of the GetAllInfo operation, allowing for further processing or
    ///   presentation of the obtained ONVIF data.
    /// </remarks>    
    property OnGetAllInfo             : TNotifyEvent       read FOnGetAllInfo            write FOnGetAllInfo;

    /// <summary>
    ///   Property representing an event handler for the token found in profiles.
    /// </summary>
    /// <remarks>
    ///   Use this property to assign a handler for the token found event in profiles.
    ///   The assigned handler should be of type TEventTokenFound, allowing customization
    ///   of the default behavior for ONVIF commands related to profile tokens.
    /// </remarks>    
    property OnProfileTokenFound      : TEventTokenFound   read FOnProfileTokenFound     write FOnProfileTokenFound;    

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
  end;

implementation

Uses System.NetEncoding, IdHashSHA, IdHTTP, IdURI;



constructor TONVIFManager.Create(const aUrl, aLogin, aPassword:String);
begin
  Create(aUrl, aLogin, aPassword,String.Empty);
end;


constructor TONVIFManager.Create(const aUrl,aLogin,aPassword,aToken:String);
begin
  FLogin                  := aLogin;
  FSaveResponseOnDisk     := False;
  FPathFileResponseOnDisk := 'DumpResponse.log';
  FPassword               := aPassword;
  FToken                  := aToken;
  FIsFixedToken           := not FToken.IsEmpty;
  FPTZ                    := TONVIFPTZManager.Create(self);
  Url                     := aUrl;    // execute setUrl;  
  FSpeed                  := 6;
end;

destructor TONVIFManager.Destroy;
begin
  FreeAndNil(FPTZ);
  inherited;
end;

procedure TONVIFManager.DoWriteLog(const aFunction, aDescription: String;aLevel: TPONVIFLivLog; aIsVerboseLog: boolean=false);
begin
  if Assigned(FOnWriteLog) then
    FOnWriteLog(aFunction,aDescription,aLevel,aIsVerboseLog)
end;

procedure TONVIFManager.CheckAuxiliaryCommand;
begin
  
end;


procedure TONVIFManager.GetPasswordDigest(Var aPasswordDigest, aNonce, aCreated: String);
Var i          : Integer;
    LRaw_nonce : TBytes;
    LBnonce    : TBytes; 
    LDigest    : TBytes;
    Lraw_digest: TBytes;
begin
  SetLength(LRaw_nonce, 20);
  for i := 0 to High(LRaw_nonce) do
    LRaw_nonce[i]:= Random(256);
    
  LBnonce         := TNetEncoding.Base64.Encode(LRaw_nonce);
  aNonce          := TEncoding.ANSI.GetString(LBnonce);
  aCreated        := DateTimeToXMLTime(Now,False);
  Lraw_digest     := SHA1(LRaw_nonce + TEncoding.ANSI.GetBytes(aCreated) + TEncoding.ANSI.GetBytes(FPassword));
  LDigest         := TNetEncoding.Base64.Encode(Lraw_digest);
  aPasswordDigest := TEncoding.ANSI.GetString(LDigest);
end;

procedure TONVIFManager.ReadInfo;
begin
  {TODO get system date time to be use for password? }
  GetCapabilities;
  GetDeviceInformation;
  GetProfiles;
  if not Token.Trim.IsEmpty then
  begin
    {TODO Get preset}
    CheckAuxiliaryCommand;
  end;  
end;

procedure TONVIFManager.DoGetAllInfo;
begin
  if Assigned(FOnGetAllInfo) then
    FOnGetAllInfo(Self);
end;

procedure TONVIFManager.ReadInfoAsync;
var LThread: TThread;
begin
  LThread := TThread.CreateAnonymousThread(
    procedure
    begin
      ReadInfo;
      TThread.Queue(nil, DoGetAllInfo);
    end
  );
  LThread.Start;
end;

procedure TONVIFManager.SetUrl(const aValue: String);
begin
  if Furl <> aValue then
  begin
    if aValue.Trim.IsEmpty then
      Reset;
    Furl := aValue;
  end;
end;

function TONVIFManager.SHA1(const aData: TBytes): TBytes;
Var LIdHashSHA1: TIdHashSHA1;
    i, j: TIdBytes;
begin
  LIdHashSHA1 := TIdHashSHA1.Create;
  try
    SetLength(i, Length(aData));
    Move(aData[0], i[0], Length(aData));
    j := LIdHashSHA1.HashBytes(i);
    SetLength(Result, Length(j));
    Move(j[0], Result[0], Length(j));
  finally
    LIdHashSHA1.Free;
  end;
end;

function TONVIFManager.UrlIsValid(const aMathodNameRequest:String): Boolean;
begin
  Result := not FUrl.Trim.IsEmpty;
  if not result then
  begin
    FLastStatusCode := ONVIF_ERROR_URL_EMPTY;
    FLastResponse   := InternalErrorToString(aMathodNameRequest,ONVIF_ERROR_URL_EMPTY);
  end;
    
end;

function TONVIFManager.InternalErrorToString(const aMathodNameRequest:String;const aErrorCode :Integer):String;
begin
  case aErrorCode of
    ONVIF_ERROR_URL_EMPTY                : Result := 'Url is empty'; 
    ONVIF_ERROR_SOAP_INVALID             : result := 'Root node is not SOAP envelope';
    ONVIF_ERROR_SOAP_NOBODY              : result := 'Body SOAP node not found';
    ONVIF_ERROR_SOAP_FAULTCODE_NOT_FOUND : Result := 'SOAP Fault code not found';
    ONVIF_ERROR_PTZ_INVALID_PRESET_INDEX : Result := 'Index of preset is out of range';
    ONVIF_ERROR_PTZ_TOKEN_IS_EMPTY       : Result := 'Token is empty';
    ONVIF_ERROR_PTZ_PRESETNAME_IS_EMPTY  : Result := 'Preset name is empty';
  else
    result := 'Unknow error' 
  end;
  {$REGION 'Log'}
  {TSI:IGNORE ON}
      DoWriteLog(aMathodNameRequest,Format(' Internal error [%s] description [%s]',[aErrorCode,result]),tpLivError);
  {TSI:IGNORE OFF}
  {$ENDREGION}

end;

function TONVIFManager.GetUrlByType(const aUrlType: TONVIFAddrType): string;
CONST   
      URL_DEVICE_SERVICE = 'onvif/device_service';
      URL_PTZ_SERVICE    = 'onvif/ptz_service';  
      URL_MEDIA          = 'onvif/media';

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
    end;
    Result := LUri.Uri;                                  
  finally
    FreeAndNil(LUri);
  end;
end;

function TONVIFManager.GetSoapXMLConnection:String;
CONST XML_SOAP_CONNECTION_OLD: String =
    '<?xml version="1.0" encoding="UTF-8"?> ' + 
    '<soap:Envelope ' +        
    'xmlns:soap="http://www.w3.org/2003/05/soap-envelope" ' + 
    'xmlns:wsdl="http://www.onvif.org/ver10/media/wsdl">' + 
    '<soap:Header>' + 
    '<Security xmlns="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-secext-1.0.xsd" s:mustUnderstand="1"> ' + 
    '<UsernameToken> ' + 
    '<Username>%s</Username> ' + 
    '<Password Type="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-username-token-profile-1.0#PasswordDigest">%s</Password> ' +
    '<Nonce EncodingType="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-soap-message-security-1.0#Base64Binary">%s</Nonce> ' +
    '<Created xmlns="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-utility-1.0.xsd">%s</Created> ' +
    '</UsernameToken> ' + 
    '</Security> ' +   
    '</soap:Header>' + 
    '<soap:Body xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"> ';
CONST XML_SOAP_CONNECTION: String =
    '<?xml version="1.0" encoding="UTF-8"?> ' + 
    '<soap:Envelope ' + 
    ' xmlns:soap="http://www.w3.org/2003/05/soap-envelope"'+ 
    ' xmlns:SOAP-ENC="http://www.w3.org/2003/05/soap-encoding"'+
    ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"'+
    ' xmlns:xsd="http://www.w3.org/2001/XMLSchema"'+ 
    ' xmlns:c14n="http://www.w3.org/2001/10/xml-exc-c14n#"'+
    ' xmlns:xenc="http://www.w3.org/2001/04/xmlenc#"'+
    ' xmlns:wsc="http://schemas.xmlsoap.org/ws/2005/02/sc"'+
    ' xmlns:ds="http://www.w3.org/2000/09/xmldsig#"'+ 
    ' xmlns:wsse="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-secext-1.0.xsd"'+
    ' xmlns:wsa="http://schemas.xmlsoap.org/ws/2004/08/addressing"'+
    ' xmlns:wsdd="http://schemas.xmlsoap.org/ws/2005/04/discovery"'+
    ' xmlns:chan="http://schemas.microsoft.com/ws/2005/02/duplex"'+
    ' xmlns:wsa5="http://www.w3.org/2005/08/addressing"'+
    ' xmlns:xmime="http://tempuri.org/xmime.xsd"'+
    ' xmlns:xop="http://www.w3.org/2004/08/xop/include"'+
    ' xmlns:tt="http://www.onvif.org/ver10/schema"'+ 
    ' xmlns:wsrfbf="http://docs.oasis-open.org/wsrf/bf-2"'+
    ' xmlns:wstop="http://docs.oasis-open.org/wsn/t-1"'+
    ' xmlns:wsrfr="http://docs.oasis-open.org/wsrf/r-2"'+
    ' xmlns:tdn="http://www.onvif.org/ver10/network/wsdl"'+
    ' xmlns:tds="http://www.onvif.org/ver10/device/wsdl"'+
    ' xmlns:tev="http://www.onvif.org/ver10/events/wsdl"'+
    ' xmlns:wsnt="http://docs.oasis-open.org/wsn/b-2"'+
    ' xmlns:timg="http://www.onvif.org/ver20/imaging/wsdl"'+
    ' xmlns:tls="http://www.onvif.org/ver10/display/wsdl"'+
    ' xmlns:tmd="http://www.onvif.org/ver10/deviceIO/wsdl"'+
    ' xmlns:tptz="http://www.onvif.org/ver20/ptz/wsdl"'+
    ' xmlns:trc="http://www.onvif.org/ver10/recording/wsdl"'+
    ' xmlns:wsu="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-utility-1.0.xsd" '+
    ' xmlns:trp="http://www.onvif.org/ver10/replay/wsdl"'+
    ' xmlns:trt="http://www.onvif.org/ver10/media/wsdl" '+
    ' xmlns:trv="http://www.onvif.org/ver10/receiver/wsdl"'+
    ' xmlns:tse="http://www.onvif.org/ver10/search/wsdl"> '+ 
    '<soap:Header>' + 
    '<wsse:Security soap:mustUnderstand="true">' + 
    '<wsse:UsernameToken> ' + 
    '<wsse:Username>%s</wsse:Username> ' + 
    '<wsse:Password Type="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-username-token-profile-1.0#PasswordDigest">%s</wsse:Password> ' +
    '<wsse:Nonce>%s</wsse:Nonce> ' +
    '<wsu:Created>%s</wsu:Created>' +
    '</wsse:UsernameToken> ' + 
    '</wsse:Security> ' +   
    '</soap:Header>' + 
    '<soap:Body xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"> ';
var LPasswordDigest : String; 
    LNonce          : String;
    LCreated        : String;
begin
  GetPasswordDigest(LPasswordDigest, LNonce, LCreated);
  Result := Format(XML_SOAP_CONNECTION, [FLogin, LPasswordDigest, LNonce, LCreated]);    
end;

function TONVIFManager.PrepareGetCapabilitiesRequest: String;
const GET_CAPABILITIES = '<tds:GetCapabilities>'+
                         '<tds:Category>All</tds:Category>'+
                         '</tds:GetCapabilities>'+
                         '</soap:Body>'+
                         '</soap:Envelope>';
begin
  Result := GetSoapXMLConnection+ GET_CAPABILITIES;
end;

function TONVIFManager.GetCapabilities: Boolean;
var LResultStr         : String;
    LXMLDoc            : IXMLDocument;
    LSoapBodyNode      : IXMLNode;
    LCapabilitieNode   : IXMLNode;
    LNodeTmp1          : IXMLNode;
    LNodeTmp2          : IXMLNode;    
begin
  Result := false;

  ResetCapabilities;
  if not UrlIsValid('TONVIFManager.GetCapabilities') then Exit;  
  Result := ExecuteRequest(GetUrlByType(atDeviceService), PrepareGetCapabilitiesRequest, LResultStr);

  if Result then
  begin
    {$REGION 'Log'}
    {TSI:IGNORE ON}
        DoWriteLog('TONVIFManager.GetCapabilities',Format(' XML response [%s]',[LResultStr]),tpLivInfo,true);      
    {TSI:IGNORE OFF}
    {$ENDREGION}
    LXMLDoc := TXMLDocument.Create(nil);
    LXMLDoc.LoadFromXML(LResultStr);

    if not IsValidSoapXML(LXMLDoc.DocumentElement) then exit;
    
    LSoapBodyNode     := GetSoapBody(LXMLDoc.DocumentElement);
    LCapabilitieNode  := RecursiveFindNode(LSoapBodyNode,'Device');

    if Assigned(LCapabilitieNode) then
    begin
      FCapabilities.Device.XAddr := GetChildNodeValue(LCapabilitieNode,'XAddr');

      LNodeTmp1 := RecursiveFindNode(LCapabilitieNode,'Network');

      if Assigned(LNodeTmp1) then
      begin
        FCapabilities.Device.Network.IPFilter          := StrToBoolDef(GetChildNodeValue(LNodeTmp1,'IPFilter'),False);
        FCapabilities.Device.Network.ZeroConfiguration := StrToBoolDef(GetChildNodeValue(LNodeTmp1,'IPFilter'),False);  
        FCapabilities.Device.Network.IPVersion6        := StrToBoolDef(GetChildNodeValue(LNodeTmp1,'IPVersion6'),False);   
        FCapabilities.Device.Network.DynDNS            := StrToBoolDef(GetChildNodeValue(LNodeTmp1,'DynDNS'),False);  

        LNodeTmp2                                      := RecursiveFindNode(LNodeTmp1,'Extension');
        if Assigned(LNodeTmp2) then        
          FCapabilities.Device.Network.Extension.Dot11Configuration := StrToBoolDef(GetChildNodeValue(LNodeTmp2,'Dot11Configuration'),False);
      end;

      LNodeTmp1 := RecursiveFindNode(LCapabilitieNode,'System');

      if Assigned(LNodeTmp1) then
      begin
        FCapabilities.Device.System.DiscoveryResolve        := StrToBoolDef(GetChildNodeValue(LNodeTmp1,'DiscoveryResolve'),False);
        FCapabilities.Device.System.DiscoveryBye            := StrToBoolDef(GetChildNodeValue(LNodeTmp1,'DiscoveryBye'),False);
        FCapabilities.Device.System.RemoteDiscovery         := StrToBoolDef(GetChildNodeValue(LNodeTmp1,'RemoteDiscovery'),False);
        FCapabilities.Device.System.SystemBackup            := StrToBoolDef(GetChildNodeValue(LNodeTmp1,'SystemBackup'),False);
        FCapabilities.Device.System.SystemLogging           := StrToBoolDef(GetChildNodeValue(LNodeTmp1,'SystemLogging'),False);
        FCapabilities.Device.System.FirmwareUpgrade         := StrToBoolDef(GetChildNodeValue(LNodeTmp1,'FirmwareUpgrade'),False);
        
        LNodeTmp2                                           := RecursiveFindNode(LNodeTmp1,'SupportedVersions');
        if Assigned(LNodeTmp2) then
        begin        
          FCapabilities.Device.System.SupportedVersions.Major := StrToIntDef(GetChildNodeValue(LNodeTmp2,'Major'),-1);
          FCapabilities.Device.System.SupportedVersions.Minor := StrToIntDef(GetChildNodeValue(LNodeTmp2,'Minor'),-1);
        end;
      end;    

      {event}
      LCapabilitieNode := RecursiveFindNode(LSoapBodyNode,'Events');

      if Assigned(LCapabilitieNode) then
      begin
        FCapabilities.Events.XAddr                                         := GetChildNodeValue(LCapabilitieNode,'XAddr');
        FCapabilities.Events.WSSubscriptionPolicySupport                   := StrToBoolDef(GetChildNodeValue(LCapabilitieNode,'WSSubscriptionPolicySupport'),False);
        FCapabilities.Events.WSPullPointSupport                            := StrToBoolDef(GetChildNodeValue(LCapabilitieNode,'WSPullPointSupport'),False);
        FCapabilities.Events.WSPausableSubscriptionManagerInterfaceSupport := StrToBoolDef(GetChildNodeValue(LCapabilitieNode,'WSPausableSubscriptionManagerInterfaceSupport'),False);
      end;

      {Media}
      LCapabilitieNode := RecursiveFindNode(LSoapBodyNode,'Media'); 
      if Assigned(LCapabilitieNode) then
      begin            
        FCapabilities.Media.XAddr := GetChildNodeValue(LCapabilitieNode,'XAddr');
        LNodeTmp1                 := RecursiveFindNode(LCapabilitieNode,'StreamingCapabilities');
        if Assigned(LNodeTmp1) then
        begin
          FCapabilities.Media.StreamingCapabilities.RTPMulticast := StrToBoolDef(GetChildNodeValue(LNodeTmp1,'RTPMulticast'),False);
          FCapabilities.Media.StreamingCapabilities.RTP_TCP      := StrToBoolDef(GetChildNodeValue(LNodeTmp1,'RTP_TCP'),False);
          FCapabilities.Media.StreamingCapabilities.RTP_RTSP_TCP := StrToBoolDef(GetChildNodeValue(LNodeTmp1,'RTP_RTSP_TCP'),False);    
        end;
      end;
      {PTZ}
      LCapabilitieNode := RecursiveFindNode(LSoapBodyNode,'PTZ'); 
      if Assigned(LCapabilitieNode) then      
        FCapabilities.PTZ.XAddr := GetChildNodeValue(LCapabilitieNode,'XAddr');
      {Extension}
       LCapabilitieNode := RecursiveFindNode(LSoapBodyNode,'Extension',True); 
      if Assigned(LCapabilitieNode) then
      begin       
        LNodeTmp1 := RecursiveFindNode(LCapabilitieNode,'Search');  
        if Assigned(LNodeTmp1) then
        begin
          FCapabilities.Extension.Search.XAddr           := GetChildNodeValue(LNodeTmp1,'XAddr');
          FCapabilities.Extension.Search.MetadataSearch  := StrToBoolDef(GetChildNodeValue(LNodeTmp1,'MetadataSearch'),False);    
        end;
        LNodeTmp1 := RecursiveFindNode(LCapabilitieNode,'Replay');  
        if Assigned(LNodeTmp1) then
          FCapabilities.Extension.Replay.XAddr := GetChildNodeValue(LNodeTmp1,'XAddr');           
      end;              
    end;
  end
  else
    {$REGION 'Log'}
    {TSI:IGNORE ON}
        DoWriteLog('TONVIFManager.GetCapabilities',Format(' Error [%d] response [%s]',[FLastStatusCode,LResultStr]),tpLivError);      
    {TSI:IGNORE OFF}
    {$ENDREGION}
end;


function TONVIFManager.PrepareGetProfilesRequest: String;
const GET_PROFILES = '<trt:GetProfiles/> ' + 
                      '</soap:Body> ' +
                      '</soap:Envelope>';
begin
  Result := GetSoapXMLConnection + GET_PROFILES
end;

function TONVIFManager.GetChildNodeValue(const ParentNode: IXMLNode; const ChildNodeName: string): string;
var LTmpIndex : Integer;
begin
  Result := String.Empty;
  if Assigned(ParentNode) then
  begin
    // Check if the child node exists before accessing its value
    LTmpIndex := ParentNode.ChildNodes.IndexOf(ChildNodeName,'') ;
    if LTmpIndex > -1 then
      Result := ParentNode.ChildNodes[LTmpIndex].Text
  end;
end; 

function TONVIFManager.GetAttribute(const Node: IXMLNode; const AttributeName: string): string;
begin
  Result := String.empty;
  if Assigned(Node) and Node.HasAttribute(AttributeName) then
    Result := Node.Attributes[AttributeName];
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
begin
  Result := False;
  ResetProfiles;
  if not UrlIsValid('TONVIFManager.GetProfiles') then Exit;
  Result := ExecuteRequest(GetUrlByType(atDeviceService), PrepareGetProfilesRequest, LResultStr);

  if Result then
  begin
    Result := False;
    {$REGION 'Log'}
    {TSI:IGNORE ON}
        DoWriteLog('TONVIFManager.GetProfiles',Format(' XML response [%s]',[LResultStr]),tpLivInfo,true);      
    {TSI:IGNORE OFF}
    {$ENDREGION}  
    LXMLDoc := TXMLDocument.Create(nil);
    LXMLDoc.LoadFromXML(LResultStr);

    if not IsValidSoapXML(LXMLDoc.DocumentElement) then exit;
    
    LSoapBodyNode := GetSoapBody(LXMLDoc.DocumentElement);
    LProfilesNode := RecursiveFindNode(LSoapBodyNode,'GetProfilesResponse');

    if Assigned(LProfilesNode) then
    begin
      Result := LProfilesNode.ChildNodes.Count > 0;   

      SetLength(FProfiles,LProfilesNode.ChildNodes.Count);

      LCurrentIndex  := 0;  
      LSetForDefault := not FIsFixedToken;
      for I := 0 to LProfilesNode.ChildNodes.Count -1 do
      begin  
      
        LProfile.token := String(LProfilesNode.ChildNodes[I].Attributes['token']);  
        LProfile.fixed := Boolean(StrToBoolDef(GetAttribute(LProfilesNode.ChildNodes[I],'fixed'), False));
        LProfile.name  := GetChildNodeValue(LProfilesNode.ChildNodes[I],'tt:Name');

        if Assigned(FOnProfileTokenFound) then
        begin
          FOnProfileTokenFound(LProfile.name,LProfile.token,LSetForDefault);

          if LSetForDefault then
            FToken := LProfile.token;
          LSetForDefault := False;
        end;
        
        // Continue parsing TVideoSourceConfiguration
        LChildNodeRoot := RecursiveFindNode(LProfilesNode.ChildNodes[I],'VideoSourceConfiguration');
        if Assigned(LChildNodeRoot) then
        begin
          LProfile.VideoSourceConfiguration.token       := GetAttribute(LChildNodeRoot,'token');
          LProfile.VideoSourceConfiguration.name        := GetChildNodeValue(LChildNodeRoot, 'Name'); 
          LProfile.VideoSourceConfiguration.UseCount    := StrToIntDef(GetChildNodeValue(LChildNodeRoot, 'UseCount'), 0);
          LProfile.VideoSourceConfiguration.SourceToken := GetChildNodeValue(LChildNodeRoot, 'SourceToken');


          LChildNodeNode := RecursiveFindNode(LChildNodeRoot,'Bounds');          
          if Assigned(LChildNodeNode) then
          begin
            LProfile.VideoSourceConfiguration.Bounds.x      := StrToIntDef(GetAttribute(LChildNodeNode,'x'), 0);
            LProfile.VideoSourceConfiguration.Bounds.y      := StrToIntDef(GetAttribute(LChildNodeNode,'y'), 0);
            LProfile.VideoSourceConfiguration.Bounds.width  := StrToIntDef(GetAttribute(LChildNodeNode,'width'), 0);
            LProfile.VideoSourceConfiguration.Bounds.height := StrToIntDef(GetAttribute(LChildNodeNode,'height'), 0);
          end;
        end;
        
        // Continue parsing TVideoEncoderConfiguration        
        LChildNodeRoot := RecursiveFindNode(LProfilesNode.ChildNodes[I],'VideoEncoderConfiguration');   
        if Assigned(LChildNodeRoot) then
        begin
          LProfile.VideoEncoderConfiguration.token           := GetAttribute(LChildNodeRoot,'token');
          LProfile.VideoEncoderConfiguration.name            := GetChildNodeValue(LChildNodeRoot, 'Name'); 
          LProfile.VideoEncoderConfiguration.UseCount        := StrToIntDef(GetChildNodeValue(LChildNodeRoot, 'UseCount'), 0);
          LProfile.VideoEncoderConfiguration.Encoding        := GetChildNodeValue(LChildNodeRoot, 'Encoding');
          LProfile.VideoEncoderConfiguration.Quality         := StrToFloatDef(GetChildNodeValue(LChildNodeRoot, 'Quality'),0);
          LProfile.VideoEncoderConfiguration.SessionTimeout  := GetChildNodeValue(LChildNodeRoot, 'SessionTimeout');

          LChildNodeNode := RecursiveFindNode(LChildNodeRoot,'Resolution');    
          if Assigned(LChildNodeNode) then
          begin     
            LProfile.VideoEncoderConfiguration.Resolution.width  := StrToIntDef(GetChildNodeValue(LChildNodeNode,'Width'), 0);
            LProfile.VideoEncoderConfiguration.Resolution.height := StrToIntDef(GetChildNodeValue(LChildNodeNode,'Height'), 0);
          end;
          
          LChildNodeNode := RecursiveFindNode(LChildNodeRoot,'RateControl');  
          if Assigned(LChildNodeNode) then
          begin     
            LProfile.VideoEncoderConfiguration.RateControl.FrameRateLimit   := StrToIntDef(GetChildNodeValue(LChildNodeNode,'FrameRateLimit'), 0);
            LProfile.VideoEncoderConfiguration.RateControl.EncodingInterval := StrToIntDef(GetChildNodeValue(LChildNodeNode,'EncodingInterval'), 0);
            LProfile.VideoEncoderConfiguration.RateControl.BitrateLimit     := StrToIntDef(GetChildNodeValue(LChildNodeNode,'BitrateLimit'), 0);            
          end;   

          LChildNodeNode := RecursiveFindNode(LChildNodeRoot,'H264');    
          if Assigned(LChildNodeNode) then
          begin     
            LProfile.VideoEncoderConfiguration.H264.GovLength   := StrToIntDef(GetChildNodeValue(LChildNodeNode,'GovLength'), 0);
            LProfile.VideoEncoderConfiguration.H264.H264Profile := GetChildNodeValue(LChildNodeNode,'H264Profile')
          end;   

          LChildNodeNode := RecursiveFindNode(LChildNodeRoot,'Multicast');    
          if Assigned(LChildNodeNode) then
          begin     
            LProfile.VideoEncoderConfiguration.Multicast.Port      := StrToIntDef(GetChildNodeValue(LChildNodeNode,'Port'), 0);
            LProfile.VideoEncoderConfiguration.Multicast.TTL       := StrToIntDef(GetChildNodeValue(LChildNodeNode,'TTL'), 0);            
            LProfile.VideoEncoderConfiguration.Multicast.AutoStart := StrToBoolDef(GetChildNodeValue(LChildNodeNode,'AutoStart'),false);

            LChildNodeNode := RecursiveFindNode(LChildNodeNode,'Address'); 
            if Assigned(LChildNodeNode) then
              LProfile.VideoEncoderConfiguration.Multicast.Address.TypeAddr := GetChildNodeValue(LChildNodeNode,'Type')
          end;                                                   
        end;
        
        // Continue parsing TPTZConfiguration    
        LChildNodeRoot := RecursiveFindNode(LProfilesNode.ChildNodes[I],'PTZConfiguration');
        if Assigned(LChildNodeRoot) then
        begin
          LProfile.PTZConfiguration.token     := GetAttribute(LChildNodeRoot,'token');
          LProfile.PTZConfiguration.Name      := GetChildNodeValue(LChildNodeNode,'Name');
          LProfile.PTZConfiguration.UseCount  := StrToIntDef(GetAttribute(LChildNodeRoot,'UseCount'),0);          
          LProfile.PTZConfiguration.NodeToken := GetAttribute(LChildNodeRoot,'NodeToken');
          
          LChildNodeNode                      := RecursiveFindNode(LChildNodeRoot,'DefaultPTZSpeed');
          if Assigned(LChildNodeNode) then
          begin
            LChildNodeNode2 :=RecursiveFindNode(LChildNodeNode,'PanTilt'); 
            if Assigned(LChildNodeNode2) then            
            begin
              LProfile.PTZConfiguration.DefaultPTZSpeed.PanTilt.x := StrToFloatDef(GetAttribute(LChildNodeNode2,'X'),0);
              LProfile.PTZConfiguration.DefaultPTZSpeed.PanTilt.Y := StrToFloatDef(GetAttribute(LChildNodeNode2,'Y'),0);              
            end;  
            LChildNodeNode2 :=RecursiveFindNode(LChildNodeNode,'Zoom'); 
            if Assigned(LChildNodeNode2) then            
              LProfile.PTZConfiguration.DefaultPTZSpeed.Zoom := StrToFloatDef(GetAttribute(LChildNodeNode2,'X'),0);
          end;
          
          LChildNodeNode := RecursiveFindNode(LChildNodeRoot,'PanTiltLimits');
          if Assigned(LChildNodeNode) then
          begin
            LChildNodeNode2 :=RecursiveFindNode(LChildNodeNode,'Range'); 
            if Assigned(LChildNodeNode2) then  
            begin          
              LProfile.PTZConfiguration.PanTiltLimits.Range.URI := GetChildNodeValue(LChildNodeNode2,'URI');
              LChildNodeNode3                                   := RecursiveFindNode(LChildNodeNode2,'XRange'); 

              if Assigned(LChildNodeNode3) then
              begin
                 LProfile.PTZConfiguration.PanTiltLimits.Range.XRange.Min := StrToIntDef(GetChildNodeValue(LChildNodeNode3,'Min'), 0);
                 LProfile.PTZConfiguration.PanTiltLimits.Range.XRange.Max := StrToIntDef(GetChildNodeValue(LChildNodeNode3,'Max'), 0);                 
              end;
              
              LChildNodeNode3  := RecursiveFindNode(LChildNodeNode2,'YRange'); 
              if Assigned(LChildNodeNode3) then
              begin
                 LProfile.PTZConfiguration.PanTiltLimits.Range.YRange.Min := StrToIntDef(GetChildNodeValue(LChildNodeNode3,'Min'), 0);
                 LProfile.PTZConfiguration.PanTiltLimits.Range.YRange.Max := StrToIntDef(GetChildNodeValue(LChildNodeNode3,'Max'), 0);                 
              end;                            
            end;              
          end;  

          LChildNodeNode := RecursiveFindNode(LChildNodeRoot,'ZoomLimits');
          if Assigned(LChildNodeNode) then
          begin
            LChildNodeNode2 :=RecursiveFindNode(LChildNodeNode,'Range'); 
            if Assigned(LChildNodeNode2) then  
            begin 
              LProfile.PTZConfiguration.ZoomLimits.Range.URI := GetChildNodeValue(LChildNodeNode2,'URI');;
              LChildNodeNode3                                := RecursiveFindNode(LChildNodeNode2,'XRange'); 

              if Assigned(LChildNodeNode3) then
              begin
                 LProfile.PTZConfiguration.ZoomLimits.Range.XRange.Min := StrToIntDef(GetChildNodeValue(LChildNodeNode3,'Min'), 0);
                 LProfile.PTZConfiguration.ZoomLimits.Range.XRange.Max := StrToIntDef(GetChildNodeValue(LChildNodeNode3,'Max'), 0);                 
              end;              
            end          
          end;        
         
        end;

        // TODO Continue parsing TAudioEncoderConfiguration         

        // TODO Continue parsing TVideoAnalyticsConfiguration        
        
        
        // TODO Continue parsing TExtension                
        
        FProfiles[LCurrentIndex] := LProfile;
        Inc(LCurrentIndex);
      end;      
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
        DoWriteLog('TONVIFManager.GetProfiles',Format(' Error [%d] response [%s]',[FLastStatusCode,LResultStr]),tpLivError);      
    {TSI:IGNORE OFF}
    {$ENDREGION}  
end;


function TONVIFManager.PrepareGetDeviceInformationRequest: String;
const GET_DEVICE_INFO =  '<tds:GetDeviceInformation/>'+
                         '</soap:Body>'+
                         '</soap:Envelope>';
begin
  Result := GetSoapXMLConnection+ GET_DEVICE_INFO;
end;

function TONVIFManager.GetDeviceInformation: Boolean;
var LResultStr         : String;
    LXMLDoc            : IXMLDocument;
    LSoapBodyNode      : IXMLNode;

    Procedure SaveNodeInfo(const aNodeName:String;var aNodeResult:String);
    var LXMLNode    : IXMLNode;
    begin
      LXMLNode := RecursiveFindNode(LSoapBodyNode,aNodeName);

      if Assigned(LXMLNode) then
        aNodeResult := LXMLNode.Text;    
    end;
    
begin
  Result := false;
  ResetDevice;
  if not UrlIsValid('TONVIFManager.GetDeviceInformation') then Exit;  
  Result := ExecuteRequest(GetUrlByType(atDeviceService), PrepareGetDeviceInformationRequest, LResultStr);

  if Result then
  begin
    {$REGION 'Log'}
    {TSI:IGNORE ON}
        DoWriteLog('TONVIFManager.GetDeviceInformation',Format(' XML response [%s]',[LResultStr]),tpLivInfo,true);      
    {TSI:IGNORE OFF}
    {$ENDREGION}  
    LXMLDoc := TXMLDocument.Create(nil);
    LXMLDoc.LoadFromXML(LResultStr);

    if not IsValidSoapXML(LXMLDoc.DocumentElement) then exit;
    
    LSoapBodyNode := GetSoapBody(LXMLDoc.DocumentElement);

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
        DoWriteLog('TONVIFManager.GetDeviceInformation',Format(' Error [%d] response [%s]',[FLastStatusCode,LResultStr]),tpLivError);      
    {TSI:IGNORE OFF}
    {$ENDREGION}
end;

function TONVIFManager.ExecuteRequest(const aAddr, aRequest: String; Var aAnswer: String): Boolean;
Var LInStream : TStringStream; 
    LOutStream: TStringStream;
begin
  LInStream  := TStringStream.Create(aRequest);
  Try
    LOutStream := TStringStream.Create;
    try
      Result        := ExecuteRequest(aAddr, LInStream, LOutStream);
      aAnswer       := LOutStream.DataString;  
      FLastResponse := aAnswer; 
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
        Result                := (FLastStatusCode div 100) = 2
      Except on E:Exception do
        begin
          FLastStatusCode := ONVIF_ERROR_DELPHI_EXCEPTION;
          FLastResponse   := E.Message;
          {$REGION 'Log'}
          {TSI:IGNORE ON}
              DoWriteLog('TONVIFManager.ExecuteRequest',Format(' Addr [%s] exception [%s] ',[aAddr,e.Message]),tpLiveException);
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
end;


Function TONVIFManager.IsValidSoapXML(const aRootNode :IXMLNode):Boolean;
CONST cNodeSOAPEnvelope  = 'Envelope';
      cNodeSOAPBodyFault = 'Fault';
      cNodeFaultCode     = 'faultcode';
      cNodeFaultString   = 'faultstring';   
         
var LSoapBodyNode      : IXMLNode;  
    LSoapBodyFaultNode : IXMLNode;    
begin
  Result := false;

  if not Pos(cNodeSOAPEnvelope,aRootNode.NodeName) = 0  then 
  begin
    FLastStatusCode := ONVIF_ERROR_SOAP_INVALID;
    FLastResponse   := InternalErrorToString('TONVIFManager.IsValidSoapXML',ONVIF_ERROR_SOAP_INVALID);
    exit;
  end;

  LSoapBodyNode := GetSoapBody(aRootNode);

  if not Assigned(LSoapBodyNode) then
  begin
    FLastStatusCode := ONVIF_ERROR_SOAP_NOBODY;
    FLastResponse   := InternalErrorToString('TONVIFManager.IsValidSoapXML',ONVIF_ERROR_SOAP_NOBODY);  
    Exit;
  end;

  LSoapBodyFaultNode := LSoapBodyNode.ChildNodes.FindNode(cNodeSOAPBodyFault);

  if Assigned(LSoapBodyFaultNode) then
  begin
    if Assigned(LSoapBodyFaultNode.ChildNodes.FindNode(cNodeFaultCode,String.Empty)) then    
      FLastStatusCode := StrToIntDef(LSoapBodyFaultNode.ChildNodes.FindNode(cNodeFaultCode,String.Empty).Text,ONVIF_ERROR_SOAP_FAULTCODE_NOT_FOUND)
    else
      FLastStatusCode := ONVIF_ERROR_SOAP_FAULTCODE_NOT_FOUND; 
      
    if assigned(LSoapBodyFaultNode.ChildNodes.FindNode(cNodeFaultString,InternalErrorToString('TONVIFManager.IsValidSoapXML',FLastStatusCode))) then      
      FLastResponse := LSoapBodyFaultNode.ChildNodes.FindNode(cNodeFaultString,InternalErrorToString('TONVIFManager.IsValidSoapXML',FLastStatusCode)).Text
    else
      FLastResponse := InternalErrorToString('TONVIFManager.IsValidSoapXML',FLastStatusCode);
    
    exit;
  end;

  Result := True;
end;

function TONVIFManager.GetSoapBody(const aRootNode :IXMLNode) : IXMLNode;
CONST cNodeSOAPBody = 'Body';
begin
  Result := aRootNode.ChildNodes[cNodeSOAPBody];  
end;

function TONVIFManager.RecursiveFindNode(ANode: IXMLNode; const aSearchNodeName: string;const aScanAllNode: Boolean=False): IXMLNode;
var I: Integer;
    LResult : IXMLNode;
begin
  Result := nil;     
  LResult:= nil;         
  if not Assigned(ANode) then exit;

  if CompareText(ANode.DOMNode.localName , aSearchNodeName) = 0 then
  begin
    LResult := ANode;
    if not aScanAllNode then Exit(LResult);
  end;


  if Assigned(ANode.ChildNodes) then
  begin
    for I := 0 to ANode.ChildNodes.Count - 1 do
    begin
      Result := RecursiveFindNode(ANode.ChildNodes[I], aSearchNodeName,aScanAllNode);
      if (Result <> nil ) then
      begin
         if not aScanAllNode then Exit;

         LResult := Result;
      end;
    end;
  end;


  Result := LResult;
end;

{ TONVIFPTZManager }

constructor TONVIFPTZManager.Create(aONVIFManager: TONVIFManager);
begin
  FONVIFManager := aONVIFManager;
  FPresentList  := TPTZPresetList.Create;
end;

destructor TONVIFPTZManager.Destroy;
begin
  FreeAndNil(FPresentList);
  inherited;
end;

function TONVIFPTZManager.PrepareStartMoveRequest(const aDirection: String): String;
const CALL_PTZ_COMMAND = 	'<tptz:ContinuousMove>'+
                          '<tptz:ProfileToken>%s</tptz:ProfileToken> '+
                          '<tptz:Velocity>'+
                          '<PanTilt %s '+
                          'xmlns="http://www.onvif.org/ver10/schema"/> '+
                          '</tptz:Velocity>'+
                          '</tptz:ContinuousMove>'+
                          '</soap:Body> '+
                          '</soap:Envelope>'; 

	{	<tptz:ContinuousMove>
			<tptz:ProfileToken>def_profile1</tptz:ProfileToken>
			<tptz:Velocity xsi:type="tt:PTZSpeed">
				<PanTilt xmlns="http://www.onvif.org/ver10/schema"
				         y="0"
				         x="0.5"
				         xsi:type="tt:Vector2D"/>
			</tptz:Velocity>
			<tptz:Timeout>PT0H0M0.300S</tptz:Timeout>
		</tptz:ContinuousMove>          }                
begin

  Result := FONVIFManager.GetSoapXMLConnection + Format(CALL_PTZ_COMMAND,[FONVIFManager.Token,aDirection]);
end;

function TONVIFPTZManager.isValidToken(const aMathodNameRequest:String): Boolean;
begin
  Result := False;
  if FONVIFManager.Token.Trim.IsEmpty then
  begin
    FONVIFManager.FLastStatusCode := ONVIF_ERROR_PTZ_TOKEN_IS_EMPTY;
    FONVIFManager.FLastResponse   := FONVIFManager.InternalErrorToString(aMathodNameRequest,ONVIF_ERROR_PTZ_TOKEN_IS_EMPTY);
  
    Exit;
  end;
  Result := True;  
end;


function TONVIFPTZManager.StartMove(aMoveType:TONVIF_PTZ_MoveType;const aDirection: TONVIF_PTZ_CommandType): Boolean;
var LCommandStr: String;  
    LResultStr : String;                                                  
begin
  Result := False;
  if not FONVIFManager.UrlIsValid('TONVIFPTZManager.StartMove') then Exit; 
  if not isValidToken('TONVIFPTZManager.StartMove') then Exit;

  case aMoveType of
    opmvContinuousMove: 
      begin
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
        Result := FONVIFManager.ExecuteRequest(FONVIFManager.GetUrlByType(atPtz), PrepareStartMoveRequest(LCommandStr), LResultStr);      
      end;
      
    opmvtRelativeMove: raise Exception.Create('opmvtRelativeMove non supported');
    opmvtAbsoluteMove: raise Exception.Create('opmvtAbsoluteMove non supported');
  end;

end;

function TONVIFPTZManager.PrepareStartZoomRequest(const aDirection: String): String;
const CALL_PTZ_COMMAND = 	'<tptz:ContinuousMove>'+
                          '<tptz:ProfileToken>%s</tptz:ProfileToken> '+
                          '<tptz:Velocity>'+
                          '<Zoom %s '+
                          'xmlns="http://www.onvif.org/ver10/schema"/> '+
                          '</tptz:Velocity>'+
                          '</tptz:ContinuousMove>'+
                          '</soap:Body> '+
                          '</soap:Envelope>';                          
begin
  Result := FONVIFManager.GetSoapXMLConnection+ Format(CALL_PTZ_COMMAND,[FONVIFManager.Token,aDirection]);
end;

function TONVIFPTZManager.Zoom(aMoveType:TONVIF_PTZ_MoveType;aInZoom: Boolean): Boolean;
var LCommand   : String;
    LResultStr : String; 
begin
  Result := False;
  if not FONVIFManager.UrlIsValid('TONVIFPTZManager.Zoom') then Exit; 
  if not isValidToken('TONVIFPTZManager.Zoom') then Exit;
      
  case aMoveType of
    opmvContinuousMove :
      begin
        if aInZoom then
           LCommand := Format('x="-0.%d"',[FONVIFManager.Speed])
        else
           LCommand := Format('x="0.%d"',[FONVIFManager.Speed]);
        
        Result := FONVIFManager.ExecuteRequest(FONVIFManager.GetUrlByType(atPtz), PrepareStartZoomRequest(LCommand), LResultStr);      
      end;
    opmvtRelativeMove: raise Exception.Create('opmvtRelativeMove non supported');
    opmvtAbsoluteMove: raise Exception.Create('opmvtAbsoluteMove non supported');    
  end;

end;

function TONVIFPTZManager.PrepareStopMoveRequest: String;
const STOP_PTZ_COMMAND =  '<tptz:Stop>'+
                          '<tptz:ProfileToken>%s</tptz:ProfileToken> '+
                          '<tptz:PanTilt>true</tptz:PanTilt> '+
                          '<tptz:Zoom>false</tptz:Zoom> '+
                          '</tptz:Stop> '+
                          '</soap:Body>'+
                          '</soap:Envelope>';
begin
  Result := FONVIFManager.GetSoapXMLConnection+ Format(STOP_PTZ_COMMAND,[FONVIFManager.Token]);
end;

function TONVIFPTZManager.Stop: Boolean;
var LResultStr: String;
begin
  Result := false;
  if not FONVIFManager.UrlIsValid('TONVIFPTZManager.Zoom') then Exit; 
  if not isValidToken('TONVIFPTZManager.Stop') then Exit;
  Result := FONVIFManager.ExecuteRequest(FONVIFManager.GetUrlByType(atPtz), PrepareStopMoveRequest, LResultStr);
end;


function TONVIFPTZManager.PrepareGetPresetList: String;
const GET_PRESET_LIST  ='<tptz:GetPresets> '+
                        '<tptz:ProfileToken>%s</tptz:ProfileToken> '+
                        '</tptz:GetPresets>'+
                        '</soap:Body>'+
                        '</soap:Envelope>';
begin
  Result := FONVIFManager.GetSoapXMLConnection+ Format(GET_PRESET_LIST,[FONVIFManager.Token]);
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
begin
  FPresentList.Clear;
  Result := False;
  if not FONVIFManager.UrlIsValid('TONVIFPTZManager.LoadPresetList') then Exit; 
  if not isValidToken('TONVIFPTZManager.LoadPresetList') then Exit;

  Result := FONVIFManager.ExecuteRequest(FONVIFManager.GetUrlByType(atPTZ),PrepareGetPresetList,LResponseStr);

  if Result then
  begin
    {$REGION 'Log'}
    {TSI:IGNORE ON}
        FONVIFManager.DoWriteLog('TONVIFPTZManager.LoadPresetList',Format(' XML response [%s]',[LResponseStr]),tpLivInfo,true);      
    {TSI:IGNORE OFF}
    {$ENDREGION}
    LXMLDoc := TXMLDocument.Create(nil);
    LXMLDoc.LoadFromXML(LResponseStr);

    if not FONVIFManager.IsValidSoapXML(LXMLDoc.DocumentElement) then exit;
    
    LSoapBodyNode   := FONVIFManager.GetSoapBody(LXMLDoc.DocumentElement);
    LPresetListNode := FONVIFManager.RecursiveFindNode(LSoapBodyNode,'GetPresetsResponse');  

    for I := 0 to LPresetListNode.ChildNodes.Count -1 do
    begin
      New(LPreset);
      LPreset.Token := String(LPresetListNode.ChildNodes[I].Attributes['token']);
      LPreset.Name  := FONVIFManager.GetChildNodeValue(LPresetListNode.ChildNodes[I],'Name');     
      LPtzPosNode   := FONVIFManager.RecursiveFindNode(LPresetListNode.ChildNodes[I],'PTZPosition');
      if Assigned(LPtzPosNode) then
      begin
         LPreset.PTZPosition.Zoom := StrToFloatDef(FONVIFManager.GetAttribute(FONVIFManager.RecursiveFindNode(LPtzPosNode,'Zoom'),'x'),0);
         LPanTiltNode             := FONVIFManager.RecursiveFindNode(LPtzPosNode,'PanTilt');
         if Assigned(LPanTiltNode) then
         begin
            LPreset.PTZPosition.PanTilt.X := StrToFloatDef(FONVIFManager.GetAttribute(LPanTiltNode,'x'),0);
            LPreset.PTZPosition.PanTilt.y := StrToFloatDef(FONVIFManager.GetAttribute(LPanTiltNode,'y'),0);            
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

function TONVIFPTZManager.PrepareGotoHome: String;
const GO_TO_HOME  = '<tptz:GotoHomePosition> '+        
                      '<tptz:ProfileToken>%s</tptz:ProfileToken> '+
                      '</tptz:GotoHomePosition>'+
                      '</soap:Body>'+
                      '</soap:Envelope>';
begin
  Result := FONVIFManager.GetSoapXMLConnection+ Format(GO_TO_HOME,[FONVIFManager.Token]);
end;

function TONVIFPTZManager.GotoHomePosition: Boolean;
var LResponseStr : String;
begin
  Result := False;  
  if not FONVIFManager.UrlIsValid('TONVIFPTZManager.GotoHomePosition') then Exit; 
  if not isValidToken('TONVIFPTZManager.GotoHomePosition') then Exit;

  Result := FONVIFManager.ExecuteRequest(FONVIFManager.GetUrlByType(atPTZ),PrepareGotoHome,LResponseStr);
end;

function TONVIFPTZManager.PrepareSetHomePosition: String;
const SET_TO_HOME  = '<tptz:SetHomePosition> '+        
                      '<tptz:ProfileToken>%s</tptz:ProfileToken> '+
                      '</tptz:SetHomePosition>'+
                      '</soap:Body>'+
                      '</soap:Envelope>';
begin
  Result := FONVIFManager.GetSoapXMLConnection+ Format(SET_TO_HOME,[FONVIFManager.Token]);
end;

function TONVIFPTZManager.SetHomePosition: Boolean;
var LResponseStr : String;
begin
  Result := False;  
  if not FONVIFManager.UrlIsValid('TONVIFPTZManager.GotoHomePosition') then Exit; 
  if not isValidToken('TONVIFPTZManager.GotoHomePosition') then Exit;
  Result := FONVIFManager.ExecuteRequest(FONVIFManager.GetUrlByType(atPTZ),PrepareSetHomePosition,LResponseStr);
end;


function TONVIFPTZManager.PrepareGotoPreset(const aTokenPreset: String): String;
const GO_TO_PRESET  = '<tptz:GotoPreset> '+
                      '<tptz:ProfileToken>%s</tptz:ProfileToken> '+
                      '<tptz:PresetToken>%s</tptz:PresetToken> '+
                      '</tptz:GotoPreset>'+
                      '</soap:Body>'+
                      '</soap:Envelope>';
begin
  Result := FONVIFManager.GetSoapXMLConnection+ Format(GO_TO_PRESET,[FONVIFManager.Token,aTokenPreset]);
end;


function TONVIFPTZManager.GoToPreset(const aIndexPreset: Integer) : Boolean;
var LResponseStr : String;
begin
  Result := False;  
  if not FONVIFManager.UrlIsValid('TONVIFPTZManager.GoToPreset') then Exit; 
  if not isValidToken('TONVIFPTZManager.GoToPreset') then Exit;
  
  if not inRange(aIndexPreset,0,FPresentList.Count -1) then
  begin
    FONVIFManager.FLastStatusCode := ONVIF_ERROR_PTZ_INVALID_PRESET_INDEX;
    FONVIFManager.FLastResponse   := FONVIFManager.InternalErrorToString('TONVIFPTZManager.GoToPreset',ONVIF_ERROR_PTZ_INVALID_PRESET_INDEX);
    Exit;
  end;

  Result := FONVIFManager.ExecuteRequest(FONVIFManager.GetUrlByType(atPTZ),PrepareGotoPreset(FPresentList[aIndexPreset].Token),LResponseStr);
end;

function TONVIFPTZManager.PrepareSetPreset(const aTokenPreset,aPresetName: String): String;
const SET_PRESET  = '<tptz:SetPreset> '+
                      '<tptz:ProfileToken>%s</tptz:ProfileToken> '+
                      '<tptz:PresetToken>%s</tptz:PresetToken> '+
                      '<tptz:PresetName>%s</tptz:PresetToken> '+                      
                      '</tptz:SetPreset>'+
                      '</soap:Body>'+
                      '</soap:Envelope>';
begin  
  Result := FONVIFManager.GetSoapXMLConnection+ Format(SET_PRESET,[FONVIFManager.Token,aTokenPreset,aPresetName]);
end;

function TONVIFPTZManager.SetPreset(const aPresetName:String;var aNewIndexPreset:Integer;aIndexExistsPreset: Integer=-1):Boolean;
var LResponseStr       : String;
    LNewSetPresetToken : String;
    LPreset            : PTPTZPreset;
    LXMLDoc            : IXMLDocument;
    LSoapBodyNode      : IXMLNode;
    LPresetSetResponse : IXMLNode;
begin
  Result            := False;  
  aNewIndexPreset   := -1;
  if not FONVIFManager.UrlIsValid('TONVIFPTZManager.SetPreset') then Exit; 
  if not isValidToken('TONVIFPTZManager.SetPreset') then Exit;

  if aPresetName.Trim.IsEmpty then
  begin
    FONVIFManager.FLastStatusCode := ONVIF_ERROR_PTZ_PRESETNAME_IS_EMPTY;
    FONVIFManager.FLastResponse   := FONVIFManager.InternalErrorToString('TONVIFPTZManager.SetPreset',ONVIF_ERROR_PTZ_PRESETNAME_IS_EMPTY);
    Exit;
  end;

  LNewSetPresetToken := String.Empty;
  if aIndexExistsPreset > -1 then
  begin
    if not inRange(aIndexExistsPreset,0,FPresentList.Count -1) then
    begin
      FONVIFManager.FLastStatusCode := ONVIF_ERROR_PTZ_INVALID_PRESET_INDEX;
      FONVIFManager.FLastResponse   := FONVIFManager.InternalErrorToString('TONVIFPTZManager.SetPreset',ONVIF_ERROR_PTZ_INVALID_PRESET_INDEX);
      Exit;
    end;
  end
  else
  begin
    aNewIndexPreset    := aIndexExistsPreset;
    LNewSetPresetToken := FPresentList[aIndexExistsPreset].Token;
  end;

  Result := FONVIFManager.ExecuteRequest(FONVIFManager.GetUrlByType(atPTZ),PrepareSetPreset(LNewSetPresetToken,aPresetName),LResponseStr);

  if Result then
  begin
    if aIndexExistsPreset > -1 then
      FPresentList[aIndexExistsPreset].Name := aPresetName
    else
    begin
      New(LPreset);
    
      LPreset.Name  := aPresetName;
      LXMLDoc := TXMLDocument.Create(nil);
      LXMLDoc.LoadFromXML(LResponseStr);

      if not FONVIFManager.IsValidSoapXML(LXMLDoc.DocumentElement) then exit;
    
      LSoapBodyNode      := FONVIFManager.GetSoapBody(LXMLDoc.DocumentElement);
      LPresetSetResponse := FONVIFManager.RecursiveFindNode(LSoapBodyNode,'SetPresetsResponse');      
      LPreset.Token      := String(LPresetSetResponse.Attributes['token']);      
      aNewIndexPreset    := FPresentList.Add(LPreset);

    end;
  end;
    
end;

function TONVIFPTZManager.PrepareRemovePreset(const aTokenPreset: String): String;
const REMOVE_PRESET  = '<tptz:RemovePreset> '+
                      '<tptz:ProfileToken>%s</tptz:ProfileToken> '+
                      '<tptz:PresetToken>%s</tptz:PresetToken> '+
                      '</tptz:RemovePreset>'+
                      '</soap:Body>'+
                      '</soap:Envelope>';
begin  
  Result := FONVIFManager.GetSoapXMLConnection+ Format(REMOVE_PRESET,[FONVIFManager.Token,aTokenPreset]);
end;

function TONVIFPTZManager.RemovePreset(const aIndexPreset: Integer): Boolean;
var LResponseStr : String;
begin
  Result := False;  
  if not FONVIFManager.UrlIsValid('TONVIFPTZManager.RemovePreset') then Exit; 
  if not isValidToken('TONVIFPTZManager.RemovePreset') then Exit;
  
  if not inRange(aIndexPreset,0,FPresentList.Count -1) then
  begin
    FONVIFManager.FLastStatusCode := ONVIF_ERROR_PTZ_INVALID_PRESET_INDEX;
    FONVIFManager.FLastResponse   := FONVIFManager.InternalErrorToString('TONVIFPTZManager.RemovePreset',ONVIF_ERROR_PTZ_INVALID_PRESET_INDEX);
    Exit;
  end;

  Result := FONVIFManager.ExecuteRequest(FONVIFManager.GetUrlByType(atPTZ),PrepareRemovePreset(FPresentList[aIndexPreset].Token),LResponseStr);

  if Result then
    FPresentList.Delete(aIndexPreset);
end;



end.

