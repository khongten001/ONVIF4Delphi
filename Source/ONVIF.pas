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
{specs: https://www.onvif.org/specs/core/ONVIF-Core-Specification.pdf}
uses
  System.Classes, System.SysUtils, System.SyncObjs, System.Messaging, ONVIF.Intf,
  ONVIF.Structure.Common, ONVIF.SOAP.Builder, System.IOUtils, Soap.XSBuiltIns,
  ONVIF.Imaging, ActiveX, System.DateUtils, ONVIF.Constant.Error, ONVIF.Types,
  IdAuthenticationDigest, Winsock, XmlDoc, XmlIntf, XMLDom, System.Math,
  System.NetConsts, ONVIF.Structure.Device, ONVIF.PTZ, ONVIF.Structure.Profile,
  System.Net.HttpClient, System.net.UrlClient, ONVIF.Structure.Capabilities,
  ONVIF.XML.Utils;

CONST
      {Imaging}
      DEFAULT_TOKEN_IMAGING   = 'VideoSourceToken';
      DEFAULT_TOKEN_IMAGING_2 = 'VideoSourceConfig';      
      AUTO_TOKEN_IMAGING      = 'VideoSource_%d';
      
Type

  {   
    TODO LIST 
      
      - Recoding
        
      - Events A device supporting PTZ service dispatchs events listed in this chapter through the event 

      - PRofile parser in record
        -- AudioEncoderConfiguration  : TAudioEncoderConfiguration;
        -= VideoAnalyticsConfiguration: TVideoAnalyticsConfiguration;
        -- Extension                  : TExtension;          
          --- Need example            
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
  ///   Represents a class for excluding specific information in a request.
  /// </summary>
  /// <remarks>
  ///   TExcludeRequest is used to control the exclusion of certain information in a request.
  ///   It allows excluding device information and network interface details.
  /// </remarks>
  TExcludeRequest = Class(TPersistent)
  strict private
    FDeveficeInformation : Boolean;
    FNetworkInterface    : Boolean;
    FRecordingList       : Boolean;
  public
    /// <summary>
    /// Gets or sets a value indicating whether to exclude device information.
    /// </summary>
    /// <value>Default value is False.</value>
    property DeveficeInformation : Boolean read FDeveficeInformation write FDeveficeInformation default false;

    /// <summary>
    /// Gets or sets a value indicating whether to exclude network interface details.
    /// </summary>
    /// <value>Default value is False.</value>
    property NetworkInterface    : Boolean read FNetworkInterface    write FNetworkInterface    default false;
    property RecordingList       : Boolean read FRecordingList       write FRecordingList       default true;
    
  End;


  /// <summary>
  ///   Represents a manager class for handling ONVIF-related functionalities.
  /// </summary>  
  TONVIFManager = class(TComponent,IONVIFManager)
  private
    FUrl                     : String;
    FLastResponse            : String;
    FPathFileResponseOnDisk  : String;
    FDateTimeDevice          : TDateTime;
    FSaveResponseOnDisk      : Boolean;
    FIsFixedToken            : Boolean;
    FLastStatusCode          : Integer;
    FSpeed                   : Byte;
    FDevice                  : TDeviceInformation;
    FSOAPBuilder             : TONVIFSOAPBuilder;  
    FExcludeReuqest          : TExcludeRequest;
    FProfiles                : TProfiles;    
    FCapabilities            : TCapabilitiesONVIF;
    FPTZ                     : TONVIFPTZManager;
    FImaging                 : TONVIFImagingManager;
    FNetworkInterface        : TNetworkInterface;
    FSystemDateTime          : TONVIFSystemDateAndTime;
    FONVIFProxySettings      : TONVIFProxySettings;
    {Event}
    FOnWriteLog              : TEventWriteLog;
    FOnReadInfoComplete      : TNotifyEvent; 
    FOnPTZTokenFound         : TEventTokenFound;
    FOnSourceiideoTokenFound : TEventTokenFound;


    /// <summary>
    /// Retrieves the SOAP builder for ONVIF requests.
    /// </summary>
    /// <returns>
    ///   Returns an instance of TONVIFSOAPBuilder used for constructing ONVIF SOAP requests.
    /// </returns>
    /// <remarks>
    ///   Call this function to obtain the SOAP builder instance, allowing the construction
    ///   of ONVIF SOAP requests for communication with the device.
    /// </remarks>    
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

    /// <summary>
    /// Sets the last status code and logs the associated method name and error code.
    /// </summary>
    /// <param name="aMethodName">The name of the method associated with the status code.</param>
    /// <param name="aErrorCode">The error code to be set as the last status code.</param>
    /// <remarks>
    ///   Call this procedure to set the last status code and log information about the associated method name
    ///   and error code. This information is useful for tracking the status of the last operation.
    /// </remarks>    
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
    /// Sets the imaging token by retrieving it from the PTZ token.
    /// </summary>
    /// <remarks>
    ///   Call this procedure to set the imaging token by obtaining it from the associated PTZ token.
    /// </remarks>
    procedure SetTokenImagingByPTZToken;

    /// <summary>
    /// Retrieves the speed value.
    /// </summary>
    /// <returns>
    ///   Returns the speed value as a Byte.
    /// </returns>
    /// <remarks>
    ///   Call this function to get the speed value.
    /// </remarks>
    function GetSpeed: Byte;

    /// <summary>
    /// Sets the speed value.
    /// </summary>
    /// <param name="Value">The speed value to be set.</param>
    /// <remarks>
    ///   Call this procedure to set the speed value.
    /// </remarks>
    procedure SetSpeed(const Value: Byte);

    /// <summary>
    /// Retrieves the body node from the specified response text.
    /// </summary>
    /// <param name="aResponseText">The response text containing XML data.</param>
    /// <returns>
    ///   Returns the body node as an IXMLNode.
    /// </returns>
    /// <remarks>
    ///   Call this function to extract the body node from the provided response text.
    /// </remarks>
    function GetBodyNode(const aResponseText: String): IXMLNode;

    /// <summary>
    /// Resets the network interface.
    /// </summary>
    /// <remarks>
    ///   Call this procedure to reset the network interface.
    /// </remarks>
    procedure ResetNetworkInterface;

    /// <summary>
    /// Retrieves the system date and time information.
    /// </summary>
    /// <returns>
    ///   Returns True if the retrieval of system date and time is successful; otherwise, returns False.
    /// </returns>
    /// <remarks>
    ///   Call this function to obtain the system date and time information.
    /// </remarks>
    function GetSystemDateTime: Boolean;
    procedure ResetSystemDateAndTime;

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
    ///   This operation gets basic device information from the device.
    /// </summary>
    /// <returns>
    ///   True if the device information is successfully retrieved; False otherwise, compile TDeviceInformation record.
    /// </returns>    
    function GetDeviceInformation: Boolean;

    /// <summary>
    ///   This operation gets the network interface configuration from a device. 
    ///   The device shall support return of network interface configuration settings as defined by the NetworkInterface type through the GetNetworkInterfaces command.
    /// </summary>
    /// <returns>
    ///   True if the network interfaces is successfully retrieved; False otherwise, compile TDeviceInformation record.
    /// </returns>       
    function GetNetworkInterfaces: Boolean;
    
    /// <summary>
    ///   This method has been replaced by the more generic GetServices method. 
    //    For capabilities of individual services refer to the GetServiceCapabilities methods.
    /// </summary>
    /// <returns>
    ///   Returns True if the capabilities are successfully retrieved and processed; otherwise, returns False.
    /// </returns>   
    function GetCapabilities: Boolean; 

    function GetRecording: Boolean;    
    
    /// <summary>
    /// Writes the last error code log for a specific method.
    /// </summary>
    /// <param name="aMethodName">The name of the method associated with the last error code.</param>
    /// <remarks>
    ///   Call this procedure to write the last error code log for a specific method.
    ///   The log includes information about the associated method name and the last error code.
    /// </remarks>    
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
    property ProxySettings           : TONVIFProxySettings     read FONVIFProxySettings                  write FONVIFProxySettings;
    /// <summary>
    ///   Gets or sets the speed parameter for PTZ operations.
    /// </summary>                                          
    property Speed                   : Byte                    read GetSpeed                             write SetSpeed;

    /// <summary>
    ///   Gets or sets the URL of the ONVIF service.
    /// </summary>
    property Url                     : String                  read Furl                                 write SetUrl;
    
    /// <summary>
    ///   Event handler for writing logs with specific parameters.
    /// </summary>
    /// <remarks>
    ///   Use this event to handle log writing with detailed information based on the specified parameters.
    /// </remarks>
    property OnWriteLog               : TEventWriteLog         read FOnWriteLog                          write FOnWriteLog;
    
    /// <summary>
    ///   Event triggered when retrieving comprehensive ONVIF information through the GetAllInfo method.
    /// </summary>
    /// <remarks>
    ///   The <c>FOnReadInfoComplete</c> event is triggered when the GetAllInfo method is executed, facilitating
    ///   the retrieval of detailed ONVIF information. By assigning a handler to this event, the application
    ///   can respond to the completion of the GetAllInfo operation, allowing for further processing or
    ///   presentation of the obtained ONVIF data.
    /// </remarks>    
    property OnReadInfoComplete       : TNotifyEvent           read FOnReadInfoComplete                  write FOnReadInfoComplete;

    /// <summary>
    ///   Property representing an event handler for the token found in profiles.
    /// </summary>
    /// <remarks>
    ///   Use this property to assign a handler for the token found event in PTZConfigurazion.
    ///   The assigned handler should be of type TEventTokenFound, allowing customization
    ///   of the default behavior for ONVIF commands related to PTZ tokens.
    /// </remarks>    
    property OnPTZTokenFound          : TEventTokenFound        read FOnPTZTokenFound                    write FOnPTZTokenFound;   

    /// <summary>
    ///   Property representing an event handler for the token found in SourceVideo.
    /// </summary>
    /// <remarks>
    ///   Use this property to assign a handler for the token found event in VideoSource.
    ///   The assigned handler should be of type TEventTokenFound, allowing customization
    ///   of the default behavior for ONVIF commands related to VideoSource tokens.
    /// </remarks>    
    property OnSourceiideoTokenFound   : TEventTokenFound       read FOnSourceiideoTokenFound            write FOnSourceiideoTokenFound;       

    /// <summary>
    ///   Gets or sets whether to save the last HTTP response on disk.
    /// </summary>
    /// <remarks>
    ///   Set this property to True if you want to save the last HTTP response on disk.
    /// </remarks>
    property SaveResponseOnDisk       : Boolean                 read FSaveResponseOnDisk                 write FSaveResponseOnDisk;  

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
    property PathFileResponseOnDisk   : String                   read FPathFileResponseOnDisk             write FPathFileResponseOnDisk;

    /// <summary>
    ///   Gets the last HTTP status code received.
    /// </summary>
    /// <remarks>
    ///   Use this property to retrieve the last HTTP status code received during communication.
    /// </remarks>    
    property LastStatusCode           : Integer                  read FLastStatusCode;
    
    /// <summary>
    ///   Gets the last HTTP response received.
    /// </summary>
    /// <remarks>
    ///   Use this property to retrieve the last HTTP response received during communication.
    /// </remarks>    
    property LastResponse             : String                   read FLastResponse; 

    
    /// <summary>
    /// Gets or sets the exclusion request configuration.
    /// </summary>
    /// <value>
    ///   An instance of TExcludeRequest indicating the exclusion preferences for specific information in a request.
    /// </value>
    /// <remarks>
    ///   Use this property to configure the exclusion preferences for specific information in a request.
    /// </remarks>     
    property ExcludeReuqest           : TExcludeRequest          read FExcludeReuqest;

    /// <summary>
    /// Gets the date and time information from the device.
    /// </summary>
    /// <value>
    ///   A TDateTime value representing the date and time information from the device.
    /// </value>
    /// <remarks>
    ///   Use this property to retrieve the date and time information from the device.
    /// </remarks>    
    property DateTimeDevice           : TDateTime                read FDateTimeDevice;
    
    /// <summary>
    ///   Gets information about the ONVIF device.
    /// </summary>
    property Device                   : TDeviceInformation       read FDevice;

    /// <summary>
    ///   Gets information about the ONVIF NetworkInterface.
    /// </summary>
    property NetworkInterface         : TNetworkInterface        read FNetworkInterface;    

    /// <summary>
    ///   Gets information about the ONVIF SystemDateAndTime.
    /// </summary>
    property SystemDateTime           : TONVIFSystemDateAndTime  read FSystemDateTime;    

    
    /// <summary>
    ///   Gets the profiles associated with the ONVIF communication.
    /// </summary>
    /// <remarks>
    ///   Use this property to retrieve the profiles associated with the ONVIF communication.
    /// </remarks>    
    property Profiles                 : TProfiles                read FProfiles;  
    
    /// <summary>
    ///   Represents the ONVIF capabilities of the device.
    /// </summary>
    /// <remarks>
    ///   The ONVIF capabilities, including device, events, PTZ, and extension capabilities.
    /// </remarks>     
    property Capabilities             : TCapabilitiesONVIF       read FCapabilities; 
    
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
  FONVIFProxySettings     := TONVIFProxySettings.Create;
  FDateTimeDevice         := 0;
  FPathFileResponseOnDisk := 'DumpResponse.log';
  FIsFixedToken           := not aToken.IsEmpty;
  FPTZ                    := TONVIFPTZManager.Create(self);
  FExcludeReuqest         := TExcludeRequest.Create;
  FPTZ.Token              := aToken;
  FImaging                := TONVIFImagingManager.Create(self);
  Url                     := aUrl;    // execute setUrl;  
  FSpeed                  := 6;
end;

destructor TONVIFManager.Destroy;
begin
  Reset;
  FreeAndNil(FONVIFProxySettings);
  FreeAndNil(FExcludeReuqest);
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

procedure TONVIFManager.ResetSystemDateAndTime;
begin
  FSystemDateTime := Default(TONVIFSystemDateAndTime);
end;

function TONVIFManager.GetRecording:Boolean;
var LResultStr : String;
begin

  {TODO -parser XML and structure}
  Result := ExecuteRequest(atMedia,'TONVIFManager.GetRecording',FSOAPBuilder.PrepareGetRecording, LResultStr);
  if Result then
  begin
    {$REGION 'Log'}
    {TSI:IGNORE ON}
        DoWriteLog('TONVIFManager.GetRecording',LResultStr,tpLivXMLResp,true);
    {TSI:IGNORE OFF}
    {$ENDREGION}  
  end;
end;

function TONVIFManager.GetSystemDateTime:Boolean;
CONST MAX_SEC_TOLERANCE = 60;
var LResultStr : String;
    LBodyNode  : IXMLNode;
    LNodeTmp1  : IXMLNode;
    LNodeTmp2  : IXMLNode;    
begin
  ResetSystemDateAndTime;
  Result := ExecuteRequest(atDeviceService,'TONVIFManager.GetSystemDateTime',FSOAPBuilder.PrepareGetSystemDateTimeRequest, LResultStr);

  if Result then
  begin
    {$REGION 'Log'}
    {TSI:IGNORE ON}
        DoWriteLog('TONVIFManager.GetSystemDateTime',LResultStr,tpLivXMLResp,true);      
    {TSI:IGNORE OFF}
    {$ENDREGION}
    
    LBodyNode := GetBodyNode(LResultStr);
    if not Assigned(LBodyNode) then Exit;
    
    LBodyNode                       := TONVIFXMLUtils.RecursiveFindNode(LBodyNode, 'SystemDateAndTime');
    FSystemDateTime.DateTimeType    := TONVIFXMLUtils.GetChildNodeValue(LBodyNode, 'DateTimeType');
    FSystemDateTime.DaylightSavings := StrToBoolDef(TONVIFXMLUtils.GetChildNodeValue(LBodyNode, 'DaylightSavings'),False); 

    // TimeZone
    LNodeTmp1 := TONVIFXMLUtils.RecursiveFindNode(LBodyNode, 'TimeZone');
    if Assigned(LNodeTmp1) then
      FSystemDateTime.TimeZone.TZ := TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1, 'TZ');

    // UTCDateTime
    LNodeTmp1 := TONVIFXMLUtils.RecursiveFindNode(LBodyNode, 'UTCDateTime');
    if Assigned(LNodeTmp1) then
    begin

      LNodeTmp2 := TONVIFXMLUtils.RecursiveFindNode(LNodeTmp1, 'Time');
      if Assigned(LNodeTmp2) then
      begin
        FSystemDateTime.UTCDateTime.Time.Hour   := StrToIntDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp2, 'Hour'), 0);
        FSystemDateTime.UTCDateTime.Time.Minute := StrToIntDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp2, 'Minute'), 0);
        FSystemDateTime.UTCDateTime.Time.Second := StrToIntDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp2, 'Second'), 0);
      end;
      
      LNodeTmp2 := TONVIFXMLUtils.RecursiveFindNode(LNodeTmp1, 'Date');      
      if Assigned(LNodeTmp2) then
      begin
        FSystemDateTime.UTCDateTime.Date.Year  := StrToIntDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp2, 'Year'), 0);
        FSystemDateTime.UTCDateTime.Date.Month := StrToIntDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp2, 'Month'), 0);
        FSystemDateTime.UTCDateTime.Date.Day   := StrToIntDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp2, 'Day'), 0);
      end;
    end;

    // LocalDateTime
    LNodeTmp1 := TONVIFXMLUtils.RecursiveFindNode(LBodyNode, 'LocalDateTime');
    if Assigned(LNodeTmp1) then
    begin

      LNodeTmp2 := TONVIFXMLUtils.RecursiveFindNode(LNodeTmp1, 'Time');    
      if Assigned(LNodeTmp2) then
      begin

        FSystemDateTime.LocalDateTime.Time.Hour   := StrToIntDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp2, 'Hour'), 0);
        FSystemDateTime.LocalDateTime.Time.Minute := StrToIntDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp2, 'Minute'), 0);
        FSystemDateTime.LocalDateTime.Time.Second := StrToIntDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp2, 'Second'), 0);
      end;
      
      LNodeTmp2 := TONVIFXMLUtils.RecursiveFindNode(LNodeTmp1, 'Date');            
      if Assigned(LNodeTmp2) then
      begin
        FSystemDateTime.LocalDateTime.Date.Year   := StrToIntDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp2, 'Year'), 0);
        FSystemDateTime.LocalDateTime.Date.Month  := StrToIntDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp2, 'Month'), 0);
        FSystemDateTime.LocalDateTime.Date.Day    := StrToIntDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp2, 'Day'), 0);
      end;

      FDateTimeDevice := EncodeDate(FSystemDateTime.LocalDateTime.Date.Year, FSystemDateTime.LocalDateTime.Date.Month, FSystemDateTime.LocalDateTime.Date.Day) +
                         EncodeTime(FSystemDateTime.LocalDateTime.Time.Hour, FSystemDateTime.LocalDateTime.Time.Minute, FSystemDateTime.LocalDateTime.Time.Second, 0);

      FSOAPBuilder.DateTimeDevice         := FDateTimeDevice;
      FSOAPBuilder.DeviceDifferenceSec    := SecondsBetween(Now,FDateTimeDevice);
                              
      {$REGION 'Log'}
      {TSI:IGNORE ON}
          DoWriteLog('TONVIFManager.GetSystemDateTime',Format('Date time device [%s]',[DateTimeToStr(FDateTimeDevice)]),tpLivInfo);      
      {TSI:IGNORE OFF}
      {$ENDREGION}   
      if FSOAPBuilder.DeviceDifferenceSec > MAX_SEC_TOLERANCE then
      begin
        {$REGION 'Log'}
        {TSI:IGNORE ON}      
          DoWriteLog('TONVIFManager.GetSystemDateTime', Format('Misalignment between date time of device and date time of PC. Device datetime: %s, PC date time: %s', [DateTimeToStr(dateTimeDevice), DateTimeToStr(Now)]), tpLivWarning);                      
        {TSI:IGNORE OFF}
        {$ENDREGION}   
      end;
    end;
  end;
end;

procedure TONVIFManager.ReadInfo;
begin
  Try
    FSOAPBuilder.ResetDateTimeDevice;
    GetSystemDateTime;
    if GetCapabilities then
    begin    
      if not FExcludeReuqest.DeveficeInformation then      
        GetDeviceInformation;
      if not FExcludeReuqest.NetworkInterface then      
        GetNetworkInterfaces;      
      if not FExcludeReuqest.RecordingList then              
        GetRecording;
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
  FSOAPBuilder.ResetDateTimeDevice;
  GetSystemDateTime;
  if FCapabilities.Imaging.XAddr.Trim.IsEmpty then
  begin
    {$REGION 'Log'}
    {TSI:IGNORE ON}
        DoWriteLog('TONVIFManager.SetTokenImagingByPTZToken','Imaging not supported by camera',tpLivInfo);      
    {TSI:IGNORE OFF}
    {$ENDREGION}
     Exit;
  end;
  
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
    ONVIF_ERROR_PTZ_PRESET_RESPONSE_EMPTY          : Result := 'New preset name: response is empty';
    ONVIF_ERROR_PTZ_PRESETNAME_IS_EMPTY            : Result := 'Preset name is empty';
    ONVIF_ERROR_PTZ_PRESETNAME_IS_DUPLICATE        : Result := 'Preset name is duplicate';
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

  case aUrlType of
      atMedia        : 
        begin
          if not FCapabilities.Media.XAddr.Trim.IsEmpty then          
            Result := FCapabilities.Media.XAddr.Trim;
        end;
      atPtz          : 
        begin
          if not FCapabilities.PTZ.XAddr.Trim.IsEmpty then          
            Result := FCapabilities.PTZ.XAddr.Trim;
        end;

      atImaging      : 
        begin
          if not FCapabilities.Imaging.XAddr.Trim.IsEmpty then          
            Result := FCapabilities.Imaging.XAddr.Trim;
        end;

  else
    Result := String.Empty;
  end;

  if Result.Trim.IsEmpty then
  begin
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

function TONVIFManager.GetNetworkInterfaces: Boolean;
var LResultStr         : String;
    LNetworkNode       : IXMLNode;
    LNodeTmp1          : IXMLNode;
    LNodeTmp2          : IXMLNode;  
    LNodeTmp3          : IXMLNode;  
    LBodyNode          : IXMLNode;  
begin
  ResetNetworkInterface;
  Result := ExecuteRequest(atDeviceService,'TONVIFManager.GetNetworkInterfaces',FSOAPBuilder.PrepareGetNetworkInterfaceRequest, LResultStr);

  if Result then
  begin
    {$REGION 'Log'}
    {TSI:IGNORE ON}
        DoWriteLog('TONVIFManager.GetNetworkInterfaces',LResultStr,tpLivXMLResp,true);      
    {TSI:IGNORE OFF}
    {$ENDREGION}
    
    LBodyNode := GetBodyNode(LResultStr);
    if not Assigned(LBodyNode) then Exit;
    
    LNetworkNode  := TONVIFXMLUtils.RecursiveFindNode(LBodyNode,'NetworkInterfaces');

    if Assigned(LNetworkNode) then
    begin
      FNetworkInterface.Token   := TONVIFXMLUtils.GetAttribute(LNetworkNode,'token');
      FNetworkInterface.Enabled := StrToBoolDef(TONVIFXMLUtils.GetChildNodeValue(LNetworkNode,'Enabled'),False);

      {Info}
      LNodeTmp1                 := TONVIFXMLUtils.RecursiveFindNode(LNetworkNode,'Info');
      if Assigned(LNodeTmp1) then
      begin
        FNetworkInterface.Info.Name     := TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'Name');
        FNetworkInterface.Info.HwAddress:= TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'HwAddress');
        FNetworkInterface.Info.MTU      := StrToIntDef( TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'MTU'),-1);
      end;

      {Link}
      LNodeTmp1 := TONVIFXMLUtils.RecursiveFindNode(LNetworkNode,'Link');
      if Assigned(LNodeTmp1) then
      begin
        FNetworkInterface.Link.InterfaceType := StrToIntDef( TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'InterfaceType'),-1);

        LNodeTmp2 := TONVIFXMLUtils.RecursiveFindNode(LNodeTmp1,'AdminSettings');
        if Assigned(LNodeTmp2) then
        begin
          FNetworkInterface.Link.AdminSettings.AutoNegotiation  := StrToBoolDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp2,'AutoNegotiation'),False);
          FNetworkInterface.Link.AdminSettings.Duplex           := TONVIFXMLUtils.GetChildNodeValue(LNodeTmp2,'Duplex');
          FNetworkInterface.Link.AdminSettings.Speed            := StrToIntDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp2,'Speed'),-1);
        end;
        
        LNodeTmp2  := TONVIFXMLUtils.RecursiveFindNode(LNodeTmp1,'OperSettings');
        if Assigned(LNodeTmp2) then
        begin
          FNetworkInterface.Link.OperSettings.AutoNegotiation   := StrToBoolDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp2,'AutoNegotiation'),False);
          FNetworkInterface.Link.OperSettings.Duplex            := TONVIFXMLUtils.GetChildNodeValue(LNodeTmp2,'Duplex');
          FNetworkInterface.Link.OperSettings.Speed             := StrToIntDef( TONVIFXMLUtils.GetChildNodeValue(LNodeTmp2,'Speed'),-1);
        end;
      end;    

      {IP4}
      LNodeTmp1 := TONVIFXMLUtils.RecursiveFindNode(LNetworkNode,'IPv4');

      if Assigned(LNodeTmp1) then
      begin
        FNetworkInterface.IPv4.Enabled := StrToBoolDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'Enabled'),False);
        
        LNodeTmp2                      := TONVIFXMLUtils.RecursiveFindNode(LNodeTmp1,'Config');
        if Assigned(LNodeTmp2) then
        begin
          LNodeTmp3 := TONVIFXMLUtils.RecursiveFindNode(LNodeTmp2,'Manual');
          if Assigned(LNodeTmp3) then
          begin
            FNetworkInterface.IPv4.Config.Manual.Address      := TONVIFXMLUtils.GetChildNodeValue(LNodeTmp3,'Address');
            FNetworkInterface.IPv4.Config.Manual.PrefixLength := StrToIntDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp3,'PrefixLength'),-1);
          end;
          FNetworkInterface.IPv4.Config.DHCP  := StrToBoolDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp2,'DHCP'),False);
        end;
      end;

      {IP6}
      LNodeTmp1 := TONVIFXMLUtils.RecursiveFindNode(LNetworkNode,'IPv6');

      if Assigned(LNodeTmp1) then
      begin
        FNetworkInterface.IPv6.Enabled := StrToBoolDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'Enabled'),False);
        
        LNodeTmp2                      := TONVIFXMLUtils.RecursiveFindNode(LNodeTmp1,'Config');
        if Assigned(LNodeTmp2) then
        begin
          LNodeTmp3 := TONVIFXMLUtils.RecursiveFindNode(LNodeTmp2,'LinkLocal');
          if Assigned(LNodeTmp3) then
          begin
            FNetworkInterface.IPv6.Config.LinkLocal.Address      := TONVIFXMLUtils.GetChildNodeValue(LNodeTmp3,'Address');
            FNetworkInterface.IPv6.Config.LinkLocal.PrefixLength := StrToIntDef( TONVIFXMLUtils.GetChildNodeValue(LNodeTmp3,'PrefixLength'),-1);
          end;
          
          LNodeTmp3 := TONVIFXMLUtils.RecursiveFindNode(LNodeTmp2,'FromDHCP');
          if Assigned(LNodeTmp3) then
          begin
            FNetworkInterface.IPv6.Config.FromDHCP.Address      := TONVIFXMLUtils.GetChildNodeValue(LNodeTmp3,'Address');
            FNetworkInterface.IPv6.Config.FromDHCP.PrefixLength := StrToIntDef( TONVIFXMLUtils.GetChildNodeValue(LNodeTmp3,'PrefixLength'),-1);
          end;
          FNetworkInterface.IPv6.Config.AcceptRouterAdvert  := StrToBoolDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp2,'AcceptRouterAdvert'),False);
          FNetworkInterface.IPv6.Config.DHCP                := TONVIFXMLUtils.GetChildNodeValue(LNodeTmp2,'DHCP');          
        end;
      end;      
    end;
  end
  else
    WriteLastErrorCodeLog('TONVIFManager.GetNetworkInterfaces');
end;

function TONVIFManager.GetCapabilities: Boolean;
var LResultStr         : String;
    LCapabilitieNode   : IXMLNode;
    LNodeTmp1          : IXMLNode;
    LNodeTmp2          : IXMLNode;  
    LBodyNode          : IXMLNode;  
    I                  : Integer;
    X                  : Integer; 
    Z                  : Integer;
    LCountAux          : Integer;
    LCountAuxValue     : Integer;
    LFoundAux          : Boolean;
    LTmpStr            : string;  
    LTmpStrSplit       : string;     
begin
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
    LCapabilitieNode  := TONVIFXMLUtils.RecursiveFindNode(LBodyNode,'Analytics');

    if Assigned(LCapabilitieNode) then
    begin
      FCapabilities.Analytics.XAddr                  := TONVIFXMLUtils.GetChildNodeValue(LCapabilitieNode,'XAddr');
      FCapabilities.Analytics.AnalyticsModuleSupport := StrToBoolDef(TONVIFXMLUtils.GetChildNodeValue(LCapabilitieNode,'AnalyticsModuleSupport'),False);
      FCapabilities.Analytics.RuleSupport            := StrToBoolDef(TONVIFXMLUtils.GetChildNodeValue(LCapabilitieNode,'RuleSupport'),False);    
    end;

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
          FCapabilities.Device.Network.Extension.Dot11Configuration   := StrToBoolDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp2,'Dot11Configuration'),False);
          FCapabilities.Device.Network.Extension.DHCPv6               := StrToBoolDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp2,'DHCPv6'),False);
          FCapabilities.Device.Network.Extension.Dot1XConfigurations  := StrToIntDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp2,'Dot1XConfigurations'),-1);

          for I := 0 to LNodeTmp2.ChildNodes.Count -1 do
          begin
            if not SameText(LNodeTmp2.ChildNodes[I].DOMNode.localName,'Dot11Configuration') and
               not SameText(LNodeTmp2.ChildNodes[I].DOMNode.localName,'DHCPv6')  and
               not SameText(LNodeTmp2.ChildNodes[I].DOMNode.localName,'Dot1XConfigurations') 
            then
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
        LNodeTmp2 := TONVIFXMLUtils.RecursiveFindNode(LNodeTmp1,'Extension');      
        if Assigned(LNodeTmp2) then
        begin            
          FCapabilities.Device.System.Extension.HttpFirmwareUpgrade    := StrToBoolDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp2,'HttpFirmwareUpgrade'),False);         
          FCapabilities.Device.System.Extension.HttpSystemBackup       := StrToBoolDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp2,'HttpSystemBackup'),False);          
          FCapabilities.Device.System.Extension.HttpSystemLogging      := StrToBoolDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp2,'HttpSystemLogging'),False);          
          FCapabilities.Device.System.Extension.HttpSupportInformation := StrToBoolDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp2,'HttpSupportInformation'),False);
        end;

        LNodeTmp2 := TONVIFXMLUtils.RecursiveFindNode(LNodeTmp1,'SupportedVersions');
        if Assigned(LNodeTmp2) then
        begin  
          SetLength(FCapabilities.Device.System.SupportedVersions,LNodeTmp2.ChildNodes.Count div 2);
          I := 0;
          while I < LNodeTmp2.ChildNodes.Count-1 do
          begin
                
            FCapabilities.Device.System.SupportedVersions[I].Major := StrToIntDef(LNodeTmp2.ChildNodes[I].Text,-1);
            FCapabilities.Device.System.SupportedVersions[I].Minor := StrToIntDef(LNodeTmp2.ChildNodes[I+1].Text,-1);
            Inc(I,2);
          end;
        end;

        LNodeTmp1 := TONVIFXMLUtils.RecursiveFindNode(LCapabilitieNode,'IO');      

        if Assigned(LNodeTmp1) then
        begin            
          FCapabilities.Device.IO.InputConnectors    := StrToIntDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'InputConnectors'),-1);         
          FCapabilities.Device.IO.RelayOutputs       := StrToIntDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'RelayOutputs'),-1);          
          LNodeTmp2 := TONVIFXMLUtils.RecursiveFindNode(LNodeTmp1,'Extension');      
          if Assigned(LNodeTmp2) then
          begin                        
            for X := 0 to LNodeTmp2.ChildNodes.Count -1 do
            begin  
              if SameText(LNodeTmp2.ChildNodes[X].DOMNode.localName,'AuxiliaryCommands') then
              begin
                LFoundAux    := False;        
                LTmpStr      := LNodeTmp2.ChildNodes[X].Text.Replace('tt:',String.Empty,[rfIgnoreCase]);
                if not LTmpStr.Trim.IsEmpty then
                begin
                  LTmpStrSplit := LTmpStr;
                  LCountAux    := Length(FCapabilities.Device.IO.Extension.AuxiliaryCommands);

                  if LTmpStr.Contains('|') then
                    LTmpStrSplit := LTmpStr.Split(['|'])[0];
            
                  for Z := 0 to LCountAux -1 do
                  begin
                    if SameText(FCapabilities.Device.IO.Extension.AuxiliaryCommands[Z].Name,LTmpStrSplit) then
                    begin
                      LFoundAux      := True;
                      LCountAuxValue := Length(FCapabilities.Device.IO.Extension.AuxiliaryCommands[Z].Values);
                      SetLength(FCapabilities.Device.IO.Extension.AuxiliaryCommands[Z].Values,LCountAuxValue+1);
                      if LTmpStr.Contains('|') then            
                        FCapabilities.Device.IO.Extension.AuxiliaryCommands[Z].Values[LCountAuxValue] := LTmpStr.Split(['|'])[1]
                      else
                        FCapabilities.Device.IO.Extension.AuxiliaryCommands[Z].Values[LCountAuxValue] := LTmpStrSplit;
                      Break;
                    end
                  end;
          
                  if not LFoundAux then
                  begin
                    SetLength(FCapabilities.Device.IO.Extension.AuxiliaryCommands,LCountAux+1); 
                    FCapabilities.Device.IO.Extension.AuxiliaryCommands[LCountAux].Name := LTmpStrSplit;
                         
                    SetLength(FCapabilities.Device.IO.Extension.AuxiliaryCommands[LCountAux].Values,1);        
                    if LTmpStr.Contains('|') then            
                      FCapabilities.Device.IO.Extension.AuxiliaryCommands[LCountAux].Values[0] :=  LTmpStr.Split(['|'])[1]              
                    else
                      FCapabilities.Device.IO.Extension.AuxiliaryCommands[LCountAux].Values[0]:= LTmpStr;        
                  end;
                end
              end;
            end;

          end;
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
      {Imaging}
      LCapabilitieNode := TONVIFXMLUtils.RecursiveFindNode(LBodyNode,'Imaging'); 
      if Assigned(LCapabilitieNode) then      
        FCapabilities.Imaging.XAddr := TONVIFXMLUtils.GetChildNodeValue(LCapabilitieNode,'XAddr');

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
  Result := ExecuteRequest(atDeviceService,'TONVIFManager.GetProfiles',FSOAPBuilder.PrepareGetProfilesRequest, LResultStr);

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
          LNewToken := LProfile.token;
       {   if ( LProfile.PTZConfiguration.token = 'PTZToken') or
             ( LProfile.PTZConfiguration.token = 'PtzConf1')   // How can I identify without using constants?
          then
            
          else
            LNewToken := LProfile.PTZConfiguration.token;}
            
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
      Try
        Result := ExecuteRequest(LUrl, LInStream, LOutStream);        
      Except on E : Exception do
        begin
          FLastStatusCode := ONVIF_ERROR_DELPHI_EXCEPTION;
          FLastResponse   := e.Message;
          {$REGION 'Log'}
          {TSI:IGNORE ON}
              DoWriteLog(aMethodName,Format('ExecuteRequest: ULR [%s] Generic exception [%s] response [%s]',[LUrl,e.Message,aAnswer]),tpLivException);      
          {TSI:IGNORE OFF}
          {$ENDREGION}           
        end;
      End;    
      
      aAnswer := LOutStream.DataString; 
      
      if FLastStatusCode <> ONVIF_ERROR_DELPHI_EXCEPTION then 
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
                    DoWriteLog(aMethodName,Format('ExecuteRequest: URL [%s] Error [%d] response [%s]',[LUrl,FLastStatusCode,aAnswer]),tpLivError);      
                {TSI:IGNORE OFF}
                {$ENDREGION}
              end;
            end;
          end;
        Except on E : Exception do
          {$REGION 'Log'}
          {TSI:IGNORE ON}
              DoWriteLog(aMethodName,Format('ExecuteRequest: ULR [%s] [Parser XML response exception [%s], Execute request Error [%d] response [%s]',[LUrl,e.Message,FLastStatusCode,aAnswer]),tpLivError);      
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
  LHTTPClient := THttpClient.Create;
  Try
    With LHTTPClient do
    begin
      // Set proxy details if provided
      // Set proxy details if provided
      if (not FONVIFProxySettings.Address.IsEmpty) and (FONVIFProxySettings.Port > 0) then
      begin
        ProxySettings := TProxySettings.Create(FONVIFProxySettings.Address, FONVIFProxySettings.Port,
          FONVIFProxySettings.Username, FONVIFProxySettings.Password);
      end;
    
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
    end;
  finally      
    FreeAndNil(LHTTPClient)
  end;
end;

procedure TONVIFManager.Reset;
begin

  ResetSystemDateAndTime;
  ResetDevice;
  ResetProfiles;
  ResetCapabilities;
  ResetNetworkInterface;
  FLastStatusCode := 0;
  FLastResponse   := String.Empty;    
end;

Procedure TONVIFManager.ResetNetworkInterface;
begin
  FNetworkInterface                                    := Default(TNetworkInterface);

  FNetworkInterface.Info.MTU                           := -1;
  FNetworkInterface.Link.AdminSettings.Speed           := -1;
  FNetworkInterface.Link.OperSettings.Speed            := -1;
  FNetworkInterface.Link.InterfaceType                 := -1;
  FNetworkInterface.IPv4.Config.Manual.PrefixLength    := -1;
  FNetworkInterface.IPv6.Config.LinkLocal.PrefixLength := -1;
  FNetworkInterface.IPv6.Config.FromDHCP.PrefixLength  := -1;
end;

Procedure TONVIFManager.ResetProfiles;
begin
  SetLength(FProfiles,0);
end;

Procedure TONVIFManager.ResetDevice;
begin
  FDevice := Default(TDeviceInformation);
end;

procedure TONVIFManager.ResetCapabilities;
begin
  FCapabilities                                                      := Default(TCapabilitiesONVIF);
  FCapabilities.Device.Network.Extension.Dot1XConfigurations         := -1;              
  SetLength(FCapabilities.Device.System.SupportedVersions,0);

  FCapabilities.Device.IO.InputConnectors                            := -1;
  FCapabilities.Device.IO.RelayOutputs                               := -1;
  SetLength(FCapabilities.Device.IO.Extension.AuxiliaryCommands,0);

  FCapabilities.Extension.Recording.MaxStringLength                  := -1;    
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

