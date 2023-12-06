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
  System.Classes, System.SysUtils, System.SyncObjs, System.Messaging,ONVIF.Intf,
  ONVIF.SOAP.Builder, System.IOUtils,  Soap.XSBuiltIns,ONVIF.Imaging,ActiveX,
  ONVIF.Constant.Error, ONVIF.Types, IdAuthenticationDigest, Winsock, XmlDoc,
  XmlIntf, XMLDom, System.Math, System.NetConsts, ONVIF.Structure.Device,ONVIF.PTZ,
  ONVIF.Structure.Profile, System.Net.HttpClient, System.net.UrlClient,
  ONVIF.Structure.Capabilities,  ONVIF.XML.Utils;

CONST
      {Imaging}
      DEFAULT_TOKEN_IMAGING   = 'VideoSourceToken';
      DEFAULT_TOKEN_IMAGING_2 = 'VideoSourceConfig';      
      AUTO_TOKEN_IMAGING      = 'VideoSource_%d';
      
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
              -- Relative
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
  

  /// <summary>
  ///   Represents a manager class for handling ONVIF-related functionalities.
  /// </summary>  
  TONVIFManager = class(TComponent,IONVIFManager)
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
    FOnPTZTokenFound         : TEventTokenFound;
    FOnSourceiideoTokenFound : TEventTokenFound;

    function GetSOAPBuilder:TONVIFSOAPBuilder;
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
    function ExecuteRequest(aAddrType: TONVIFAddrType;const aMethodName, aRequest: String; var aAnswer: String): Boolean; overload;

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

    /// <summary>
    ///   Procedure to set the imaging token based on the PTZ (Pan-Tilt-Zoom) token.
    /// </summary>    
    procedure SetTokenImagingByPTZToken;
    function GetSpeed: Byte;
    procedure SetSpeed(const Value: Byte);
    function GetBodyNode(const aResponseText:String): IXMLNode;
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
    constructor Create(const aUrl, aLogin, aPassword:String);reintroduce;overload;
  
    /// <summary>
    ///   Initializes a new instance of the IONVIFManager interface with the specified ONVIF service details.
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
    constructor Create(const aUrl, aLogin, aPassword, aToken: String);reintroduce;overload;
    
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
    procedure WriteLastErrorCodeLog(const aMethodName: String);
    
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
    property Speed                   : Byte                read GetSpeed                 write SetSpeed;

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

Uses IdURI;

constructor TONVIFManager.Create(const aUrl, aLogin, aPassword:String);
begin
  Create(aUrl, aLogin, aPassword,String.Empty);
end;

constructor TONVIFManager.Create(const aUrl,aLogin,aPassword,aToken:String);
begin
  inherited Create(nil);
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
  inherited Destroy;
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
      {MSXML uses COM objects, the error message means the MSXML COM objects failed to instantiate.}
      CoInitialize(nil);
      Try
        ReadInfo;
      Finally
       CoUninitialize; 
      End;
    end
  );
  LThread.Start;
end;

procedure TONVIFManager.SetLastStatusCode(const aMethodName:String;const aErrorCode: Integer);
begin
  FLastStatusCode := aErrorCode;
  FLastResponse   := InternalErrorToString(aMethodName,aErrorCode);
end;

procedure TONVIFManager.SetSpeed(const Value: Byte);
begin
  FSpeed := Value;
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

procedure TONVIFManager.WriteLastErrorCodeLog(const aMethodName: String);
begin
  {$REGION 'Log'}
  {TSI:IGNORE ON}
      DoWriteLog(aMethodName,Format('Url [%s] Error [%d] LastResponse [%s]',[FUrl,FLastStatusCode,FLastResponse]),tpLivError);      
  {TSI:IGNORE OFF}
  {$ENDREGION}        
end;

function TONVIFManager.InternalErrorToString(const aMathodNameRequest:String;const aErrorCode :Integer):String;
begin

  case aErrorCode of
    ONVIF_ERROR_URL_EMPTY                          : Result := 'Url is empty'; 
    ONVIF_ERROR_SOAP_INVALID                       : result := 'Root node is not SOAP envelope';
    ONVIF_ERROR_SOAP_NOBODY                        : result := 'Body SOAP node not found';
    ONVIF_ERROR_SOAP_FAULTCODE_NOT_FOUND           : Result := 'SOAP Fault code not found';
    {PTZ}
    ONVIF_ERROR_PTZ_INVALID_PRESET_INDEX           : Result := 'Index of preset is out of range';
    ONVIF_ERROR_PTZ_TOKEN_IS_EMPTY                 : Result := 'PTZ token is empty';
    ONVIF_ERROR_PTZ_PRESETNAME_IS_EMPTY            : Result := 'Preset name is empty';
    ONVIF_ERROR_PTZ_HOME_COMMAND_NOT_SUPPORTED     : Result := 'Home command is not supported by camera';
    ONVIF_ERROR_PTZ_MAX_PRESET                     : Result := 'Maximum number of presets reached';
    ONVIF_ERROR_PTZ_MOVE_CONTINUOUS_NOT_SUPPORTED  : Result := 'Continuous mode not supported by camera';
    ONVIF_ERROR_PTZ_MOVE_ABSOLUTE_NOT_SUPPORTED    : Result := 'Absolute mode not supported by camera';
    ONVIF_ERROR_PTZ_MOVE_RELATIVE_NOT_SUPPORTED    : Result := 'Relativ mode not supported by camera';
    ONVIF_ERROR_PTZ_AUX_COMMAND_NOT_SUPPORTED      : Result := 'Auxiliary command not supported by camera';
    ONVIF_ERROR_PTZ_AUX_COMMAND_NOT_FOUND          : Result := 'Auxiliary command not found';
    ONVIF_ERROR_PTZ_AUX_COMMAND_VALUE_NOT_FOUND    : Result := 'Auxiliary command value not found';
    {Imaging}
    ONVIF_ERROR_IMG_IMMAGING_IS_EMPTY              : Result := 'VideoSource token is empty';
    ONVIF_ERROR_IMG_FOCUS_NOT_SUPPORTED            : Result := 'Focus command is not supported by camera';
    ONVIF_ERROR_IMG_FOCUS_CONTINUOUS_NOT_SUPPORTED : Result := 'Continuous mode for focus command is not supported by camera'; 
    ONVIF_ERROR_IMG_FOCUS_ABSOLUTE_NOT_SUPPORTED   : Result := 'Relativ mode for focus command is not supported by camera'; 
    ONVIF_ERROR_IMG_FOCUS_RELATIVE_NOT_SUPPORTED   : Result := 'Absolute mode for focus command is not supported by camera'; 
    ONVIF_ERROR_IMG_FOCUS_ABSOLUTE_OUT_OF_RANGE    : Result := 'Value of absolute position is out of range'; 
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

function TONVIFManager.GetBodyNode(const aResponseText: String): IXMLNode;
var LXMLDoc       : IXMLDocument;
    LErrorFound   : Boolean;
begin
  Result          := nil;
  LXMLDoc         := TXMLDocument.Create(nil);
  LXMLDoc.LoadFromXML(aResponseText);
  LXMLDoc.Active := True;
  if not IsValidSoapXML(LXMLDoc.DocumentElement,LErrorFound) then exit;
  Result := TONVIFXMLUtils.GetSoapBody(LXMLDoc.DocumentElement);  
end;

function TONVIFManager.GetCapabilities: Boolean;
var LResultStr         : String;
    LCapabilitieNode   : IXMLNode;
    LNodeTmp1          : IXMLNode;
    LNodeTmp2          : IXMLNode;  
    LBodyNode          : IXMLNode;  
    I                  : Integer;
    X                  : Integer;    
begin
  Result := false;

  ResetCapabilities;
  Result := ExecuteRequest(atDeviceService,'TONVIFManager.GetCapabilities',FSOAPBuilder.PrepareGetCapabilitiesRequest, LResultStr);

  if Result then
  begin
    {$REGION 'Log'}
    {TSI:IGNORE ON}
        DoWriteLog('TONVIFManager.GetCapabilities',LResultStr,tpLivXMLResp,true);      
    {TSI:IGNORE OFF}
    {$ENDREGION}
    
    LBodyNode := GetBodyNode(LResultStr);
    if not Assigned(LBodyNode) then Exit;
    
    LCapabilitieNode  := TONVIFXMLUtils.RecursiveFindNode(LBodyNode,'Device');

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
                  DoWriteLog('TONVIFManager.GetCapabilities',Format('Unsupported node name [%s]',[LNodeTmp2.ChildNodes[I].DOMNode.localName]),tpLivWarning);      
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
      LCapabilitieNode := TONVIFXMLUtils.RecursiveFindNode(LBodyNode,'Events');

      if Assigned(LCapabilitieNode) then
      begin
        FCapabilities.Events.XAddr                                         := TONVIFXMLUtils.GetChildNodeValue(LCapabilitieNode,'XAddr');
        FCapabilities.Events.WSSubscriptionPolicySupport                   := StrToBoolDef(TONVIFXMLUtils.GetChildNodeValue(LCapabilitieNode,'WSSubscriptionPolicySupport'),False);
        FCapabilities.Events.WSPullPointSupport                            := StrToBoolDef(TONVIFXMLUtils.GetChildNodeValue(LCapabilitieNode,'WSPullPointSupport'),False);
        FCapabilities.Events.WSPausableSubscriptionManagerInterfaceSupport := StrToBoolDef(TONVIFXMLUtils.GetChildNodeValue(LCapabilitieNode,'WSPausableSubscriptionManagerInterfaceSupport'),False);
      end;

      {Media}
      LCapabilitieNode := TONVIFXMLUtils.RecursiveFindNode(LBodyNode,'Media'); 
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
      LCapabilitieNode := TONVIFXMLUtils.RecursiveFindNode(LBodyNode,'PTZ'); 
      if Assigned(LCapabilitieNode) then      
        FCapabilities.PTZ.XAddr := TONVIFXMLUtils.GetChildNodeValue(LCapabilitieNode,'XAddr');
        
      {Extension}
      LCapabilitieNode := TONVIFXMLUtils.RecursiveFindNode(LBodyNode,'Extension',True); 

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
    LBodyNode          : IXMLNode;
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
  ResetProfiles;
  Result := ExecuteRequest(atPtz,'TONVIFManager.GetProfiles',FSOAPBuilder.PrepareGetProfilesRequest, LResultStr);

  if Result then
  begin
    Result := False;
    {$REGION 'Log'}
    {TSI:IGNORE ON}
        DoWriteLog('TONVIFManager.GetProfiles',LResultStr,tpLivXMLResp,true);      
    {TSI:IGNORE OFF}
    {$ENDREGION}  
    LBodyNode := GetBodyNode(LResultStr);
    if not Assigned(LBodyNode) then Exit;
    
    LProfilesNode := TONVIFXMLUtils.RecursiveFindNode(LBodyNode,'GetProfilesResponse');

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
          if Assigned(FOnSourceiideoTokenFound) then
            FOnSourceiideoTokenFound(LProfile.name,LProfile.VideoSourceConfiguration.token,LSetVSourceToken);
          
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

function TONVIFManager.GetSOAPBuilder: TONVIFSOAPBuilder;
begin
  Result := FSOAPBuilder;
end;

function TONVIFManager.GetSpeed: Byte;
begin
  Result := FSpeed;
end;

function TONVIFManager.GetDeviceInformation: Boolean;
var LResultStr         : String;
    LBodyNode          : IXMLNode;

    Procedure SaveNodeInfo(const aNodeName:String;var aNodeResult:String);
    var LXMLNode    : IXMLNode;
    begin
      LXMLNode := TONVIFXMLUtils.RecursiveFindNode(LBodyNode,aNodeName);

      if Assigned(LXMLNode) then
        aNodeResult := LXMLNode.Text;    
    end;
    
begin
  ResetDevice;
  Result := ExecuteRequest(atDeviceService,'TONVIFManager.GetDeviceInformation',FSOAPBuilder.PrepareGetDeviceInformationRequest, LResultStr);

  if Result then
  begin
    {$REGION 'Log'}
    {TSI:IGNORE ON}
        DoWriteLog('TONVIFManager.GetDeviceInformation',LResultStr,tpLivXMLResp,true);      
    {TSI:IGNORE OFF}
    {$ENDREGION}  
    LBodyNode := GetBodyNode(LResultStr);
    if not Assigned(LBodyNode) then Exit;

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

function TONVIFManager.ExecuteRequest(aAddrType: TONVIFAddrType;const aMethodName, aRequest: String; Var aAnswer: String): Boolean;
Var LInStream  : TStringStream; 
    LOutStream : TStringStream;
    LXMLDoc    : IXMLDocument;
    LErrorFound: Boolean;
    LUrl       : String;
begin
  Result := False;

  if not UrlIsValid(aMethodName) then Exit;
  
  LUrl       := GetUrlByType(aAddrType);
  LInStream  := TStringStream.Create(aRequest);
  Try
    LOutStream := TStringStream.Create;
    try
      Result        := ExecuteRequest(LUrl, LInStream, LOutStream);
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
  FLastStatusCode := 0;
  FLastResponse   := String.Empty;    
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


end.

