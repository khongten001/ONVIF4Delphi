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

unit ONVIF.SOAP.Builder;

interface

uses IdHashSHA,System.SysUtils,System.NetEncoding,Soap.XSBuiltIns,IdGlobal; 

CONST END_SOAP_XML =  '</soap:Body>'+
                      '</soap:Envelope>'; 

Type

  // 
  /// <summary>
  ///   Utility class for handling SOAP operations in the ONVIF context.
  /// </summary>
  /// <remarks>
  ///   This class provides various methods and functions to assist with the
  ///   manipulation, parsing, and generation of XML documents in compliance
  ///   with the ONVIF standard.
  /// </remarks>
  TONVIFSOAPBuilder = class
  private
     FLogin     : String;
     FPassword  : String;
    
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

  public
     constructor Create(const aLogin, aPassword:String);

    /// <summary>
    ///   Prepares a GetCapabilities request for ONVIF communication.
    /// </summary>
    /// <returns>
    ///   The prepared GetCapabilities request string.
    /// </returns>
    function PrepareGetCapabilitiesRequest: String;

    /// <summary>
    ///   Prepares an XML SOAP request for moving the focus in imaging with various parameters.
    /// </summary>
    /// <param name="aToken">Token for referencing the VideoSource in the move operation.</param>
    /// <param name="aAbsolutePos">Position parameter for the absolute focus control.</param>
    /// <param name="aRelativeDistance">Distance parameter for the relative focus control.</param>
    /// <param name="aSpeed">Speed parameter for the focus control (absolute or relative).</param>
    /// <returns>String representation of the SOAP request.</returns>
    function PrepareImagingMoveFocus(const aToken: String; aAbsolutePos, aRelativeDistance, aSpeed: String): String;

    
    /// <summary>
    ///   Prepares an XML SOAP request for stopping the focus movement in imaging.
    /// </summary>
    /// <param name="aToken">Token for referencing the VideoSource in the move operation.</param>
    /// <returns>String representation of the SOAP request.</returns>
    function PrepareImagingStopMoveFocus(const aToken: String): String;
    
    /// <summary>
    ///   Prepares a GetMoveOptions request for ONVIF communication.
    /// </summary>
    /// <returns>
    ///   The prepared GetCapabilities request string.
    /// </returns>
    function PrepareImagingMoveOptions(const aToken: String): String;
    
    /// <summary>
    ///   Prepares a GetStatus request for ONVIF communication.
    /// </summary>
    /// <returns>
    ///   The prepared GetCapabilities request string.
    /// </returns>
    function PrepareImagingGetStatus(const aToken: String): String;
      
    /// <summary>
    ///   Prepares an ONVIF Imaginig setttings  request based on the specified command.
    /// </summary>
    /// <returns>
    ///   An XML-formatted string representing the Imaginig setttings move request.
    /// </returns>      
    function PrepareGetImagingSettings(const aToken: String): String;

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
        
    {PTZ}
    
    /// <summary>
    ///   Prepares an ONVIF PTZ (Pan-Tilt) start move request based on the specified command.
    /// </summary>
    /// <param name="aToken">
    ///   Token for authetication 
    /// </param>
    /// <param name="aCommand">
    ///   The command to be included in the PTZ start move request.
    /// </param>
    /// <returns>
    ///   An XML-formatted string representing the PTZ start move request.
    /// </returns>
    function PrepareStartMoveContinuousRequest(const aToken,aDirection: String): String; 

    /// <summary>
    ///   Prepares an ONVIF PTZ (Pan-Tilt) start move request based on the specified command.
    /// </summary>
    /// <param name="aToken">
    ///   Token for authetication 
    /// </param>
    /// <param name="aCommand">
    ///   The command to be included in the PTZ start move request.
    /// </param>
    /// <returns>
    ///   An XML-formatted string representing the PTZ start move request.
    /// </returns>    
    function PrepareStartMoveRelativeRequest(const aToken,aDirection: String): String;              
    
    /// <summary>
    ///   Prepares the ONVIF PTZ command for retrieving the list of preset positions.
    /// </summary>
    /// <param name="aToken">
    ///   Token for authetication 
    /// </param>
    /// <returns>ONVIF PTZ command string for getting the list of preset positions.</returns>    
    function PrepareGetPresetList(const aToken:String): String;   

    /// <summary>
    ///   Prepares the ONVIF PTZ command for moving the camera to the specified preset position.
    /// </summary>
    /// <param name="aToken">
    ///   Token for authetication 
    /// </param>
    /// <param name="aTokenPreset">Token of preset.</param>
    /// <returns>ONVIF PTZ command string for moving to the specified preset position.</returns>    
    function PrepareGotoPreset(const aToken,aTokenPreset : String):String;

    /// <summary>
    ///   Prepares the ONVIF PTZ command for removing the specified preset position.
    /// </summary>
    /// <param name="aToken">
    ///   Token for authetication 
    /// </param>
    /// <param name="aTokenPreset">Token of preset.</param>
    /// <returns>ONVIF PTZ command string for removing the specified preset position.</returns>    
    function PrepareRemovePreset(const aToken,aTokenPreset: String): String; 
    
    /// <summary>
    ///   Prepares the ONVIF PTZ command for get status of device
    /// </summary>
    /// <param name="aToken">
    ///   Token for authetication 
    /// </param>
    /// <returns> An XML-formatted string representing the PTZ GetStatus request.</returns> 
    function PrepareGetStatus(const aToken:String):String;    

    /// <summary>
    ///   Prepares an ONVIF PTZ stop move request.
    /// </summary>
    /// <param name="aToken">
    ///   Token for authetication 
    /// </param>
    /// <returns>
    ///   An XML-formatted string representing the PTZ start zoom request.
    /// </returns>
    function PrepareStopMoveRequest(const aToken:String): String;

    /// <summary>
    ///   Prepares an ONVIF PTZ start zoom request based on the specified command.
    /// </summary>
    /// <param name="aToken">
    ///   Token for authetication 
    /// </param>
    /// <param name="aZoomIN">
    ///   The command to be included in the PTZ start zoom request.
    /// </param>
    /// <returns>
    ///   An XML-formatted string representing the PTZ start zoom request.
    /// </returns>
    function PrepareStartZoomRequest(const aToken,aZoomIN: String): String;
    
    /// <summary>
    ///   Prepares an ONVIF PTZ GoToHomePosition
    /// </summary>
    /// <param name="aToken">
    ///   Token for authetication 
    /// </param>
    /// <returns>
    ///   An XML-formatted string representing the PTZ GoToHomePosition request.
    /// </returns>    
    function PrepareGotoHome(const aToken:String): String;
       
    /// <summary>
    ///   Prepares an ONVIF PTZ SetHomePosition
    /// </summary>
    /// <param name="aToken">
    ///   Token for authetication 
    /// </param>
    /// <returns>
    ///   An XML-formatted string representing the PTZ SetHomePosition request.
    /// </returns>       
    function PrepareSetHomePosition(const aToken:String): String;

    /// <summary>
    ///   Prepares an ONVIF PTZ GetNodes
    /// </summary>
    /// <returns>
    ///   An XML-formatted string representing the PTZ GetNodes request.
    /// </returns>       
    function PrepareGetNodes: String;    
    
    /// <summary>
    ///   Prepares an ONVIF PTZ SetPreset
    /// </summary>
    /// <param name="aToken">
    ///   Token for authetication 
    /// </param>
    /// <param name="aTokenPreset">Token of preset.</param>
    /// <returns>
    ///   An XML-formatted string representing the PTZ SetPreset request.
    /// </returns>        
    function PrepareSetPreset(const aToken,aTokenPreset, aPresetName: String): String;
            
    /// <summary>
    ///   Prepares and returns an XML string representing a request for device information.
    /// </summary>
    /// <returns>
    ///   A string containing the XML-formatted request for device information.
    /// </returns>
    function PrepareGetDeviceInformationRequest: String;
    
    /// <summary>
    ///   Prepares an ONVIF Imaginig Capabilities  request based on the specified command.
    /// </summary>
    /// <returns>
    ///   An XML-formatted string representing the Imaginig Capabilities move request.
    /// </returns>        
    function PrepareImagingCapabilities: String;    
  end;

implementation



constructor TONVIFSOAPBuilder.Create(const aLogin,aPassword:String);
begin
  FLogin     := aLogin;
  FPassword  := aPassword;
end;

procedure TONVIFSOAPBuilder.GetPasswordDigest(Var aPasswordDigest, aNonce, aCreated: String);
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

function TONVIFSOAPBuilder.SHA1(const aData: TBytes): TBytes;
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

function TONVIFSOAPBuilder.GetSoapXMLConnection:String;
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

function TONVIFSOAPBuilder.PrepareGetDeviceInformationRequest: String;
const GET_DEVICE_INFO =  '<tds:GetDeviceInformation/>' + END_SOAP_XML;
begin
  Result := GetSoapXMLConnection+ GET_DEVICE_INFO;
end;

function TONVIFSOAPBuilder.PrepareGetCapabilitiesRequest: String;
const GET_CAPABILITIES = '<tds:GetCapabilities>'+
                         '<tds:Category>All</tds:Category>'+
                         '</tds:GetCapabilities>' + END_SOAP_XML;
begin
  Result := GetSoapXMLConnection+ GET_CAPABILITIES;
end;

{Immaginig}

function TONVIFSOAPBuilder.PrepareGetProfilesRequest: String;
const GET_PROFILES = '<trt:GetProfiles/> ' + END_SOAP_XML;
begin
  Result := GetSoapXMLConnection + GET_PROFILES
end;

function TONVIFSOAPBuilder.PrepareImagingMoveOptions(const aToken:String): String;
const GET_MOVE_OPTIONS  = '<timg:GetMoveOptions> '+
                          '<timg:VideoSourceToken>%s</timg:VideoSourceToken> '+
                          '</timg:GetMoveOptions>'+ END_SOAP_XML;
begin  
  Result := GetSoapXMLConnection+ Format(GET_MOVE_OPTIONS,[aToken]);;
end;

function TONVIFSOAPBuilder.PrepareImagingCapabilities: String;
const GET_CAPABILITIES  = '<timg:GetServiceCapabilities> '+
                          '</timg:GetServiceCapabilities>'+ END_SOAP_XML;
begin  
  Result := GetSoapXMLConnection+ GET_CAPABILITIES;
end;



function TONVIFSOAPBuilder.PrepareImagingGetStatus(const aToken: String): String;
const GET_SETTINGS  = '<timg:GetStatus> '+
                      '<timg:VideoSourceToken>%s</timg:VideoSourceToken> '+
                      '</timg:GetStatus>'+ END_SOAP_XML;
begin  
  Result := GetSoapXMLConnection+ Format(GET_SETTINGS,[aToken]);
end;

function TONVIFSOAPBuilder.PrepareImagingStopMoveFocus(const aToken:String): String;
const STOP_FOCUS_COMMAND =  '<timg:Stop>'+
                            '<timg:VideoSourceToken >%s</timg:VideoSourceToken > '+
                            '</timg:Stop> '+ END_SOAP_XML;
begin
  Result := GetSoapXMLConnection+ Format(STOP_FOCUS_COMMAND,[aToken]);
end;


function TONVIFSOAPBuilder.PrepareImagingMoveFocus(const aToken: String;aAbsolutePos,aRelativeDistance,aSpeed:String): String;
const SET_MOVE_START    =   '<timg:Move>' +
                            '<timg:VideoSourceToken>%s</timg:VideoSourceToken>' +
                            '<timg:Focus> ';
                        
      SET_MOVE_ABSOLUTE =   '<Absolute>' +
                            '<Position>%s</Position>' +
                            '</Absolute> ';
                            
      SET_MOVE_RELATIVE  =  '<Relative>' +      
                            '<Distance>%s</Distance>' +
                            '</Relative> ' ;

      SET_MOVE_CONTINUOUS = '<timg:Continuous>' +
                            '<timg:Speed>%s</timg:Speed>' +
                            '</timg:Continuous> ';
                            
      SET_MOVE_END =        '</timg:Focus>' +
                            '</timg:Move> ';
var LXML :String;                  
begin  
  LXML := Format(SET_MOVE_START,[aToken]);
  if not aAbsolutePos.Trim.IsEmpty then
    LXML := LXML + Format(SET_MOVE_ABSOLUTE,[aAbsolutePos]);

  if not aRelativeDistance.Trim.IsEmpty then
    LXML := LXML + Format(SET_MOVE_RELATIVE,[aRelativeDistance]);
    
  if not aSpeed.Trim.IsEmpty then
    LXML := LXML + Format(SET_MOVE_CONTINUOUS,[aSpeed]);
    
  LXML := LXML  + SET_MOVE_END  + END_SOAP_XML;
  Result := GetSoapXMLConnection+ LXML;
end;

function TONVIFSOAPBuilder.PrepareGetImagingSettings(const aToken:String): String;
const GET_SETTINGS  = '<timg:GetImagingSettings> '+
                      '<timg:VideoSourceToken>%s</timg:VideoSourceToken> '+
                      '</timg:GetImagingSettings>'+ END_SOAP_XML;
begin  
  Result := GetSoapXMLConnection+ Format(GET_SETTINGS,[aToken]);
end;

{PTZ}

Function TONVIFSOAPBuilder.PrepareStartMoveRelativeRequest(const aToken,aDirection: String): String;
const CALL_PTZ_COMMAND ='<tptz:RelativeMove> '+
                        '<tptz:ProfileToken>%s</tptz:ProfileToken> '+
                        '<tptz:Translation xsi:type="tt:PTZVector"> '+
                        '<PanTilt xmlns="http://www.onvif.org/ver10/schema" %s'+
                       // 'y="0" '+
                       // 'x="-0.00999999978" '+
                        'xsi:type="tt:Vector2D"/> '+
                        'Zoom xmlns="http://www.onvif.org/ver10/schema" '+
                        'x="0" '+
                        'xsi:type="tt:Vector1D"/> '+
                        '</tptz:Translation> '+
                        '<tptz:Speed xsi:type="tt:PTZSpeed"> '+
                        '<PanTilt xmlns="http://www.onvif.org/ver10/schema" '+
                        'y="0.560000002" '+
                        'x="0.560000002" '+
                        'xsi:type="tt:Vector2D"/> '+
                        '<Zoom xmlns="http://www.onvif.org/ver10/schema" '+
                        'x="0.560000002" '+
                        'xsi:type="tt:Vector1D"/> '+
                        '</tptz:Speed> '+
                        '</tptz:RelativeMove> '+ END_SOAP_XML;                       
begin                                         
    Result := GetSoapXMLConnection + Format(CALL_PTZ_COMMAND,[aToken,aDirection]);  
end;

function TONVIFSOAPBuilder.PrepareStartMoveContinuousRequest(const aToken,aDirection: String): String;
const CALL_PTZ_COMMAND = 	'<tptz:ContinuousMove>'+
                          '<tptz:ProfileToken>%s</tptz:ProfileToken> '+
                          '<tptz:Velocity>'+
                          '<PanTilt %s '+
                          'xmlns="http://www.onvif.org/ver10/schema"/> '+
                          '</tptz:Velocity>'+
                          '</tptz:ContinuousMove>'+ END_SOAP_XML;

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

  Result := GetSoapXMLConnection + Format(CALL_PTZ_COMMAND,[aToken,aDirection]);
end;

function TONVIFSOAPBuilder.PrepareStartZoomRequest(const aToken,aZoomIN: String): String;
const CALL_PTZ_COMMAND = 	'<tptz:ContinuousMove>'+
                          '<tptz:ProfileToken>%s</tptz:ProfileToken> '+
                          '<tptz:Velocity>'+
                          '<Zoom %s '+
                          'xmlns="http://www.onvif.org/ver10/schema"/> '+
                          '</tptz:Velocity>'+
                          '</tptz:ContinuousMove>'+ END_SOAP_XML;
begin
  Result := GetSoapXMLConnection+ Format(CALL_PTZ_COMMAND,[aToken,aZoomIN]);
end;

function TONVIFSOAPBuilder.PrepareGetPresetList(const aToken:String): String;
const GET_PRESET_LIST  ='<tptz:GetPresets> '+
                        '<tptz:ProfileToken>%s</tptz:ProfileToken> '+
                        '</tptz:GetPresets>'+ END_SOAP_XML;
begin
  Result := GetSoapXMLConnection+ Format(GET_PRESET_LIST,[aToken]);
end;

function TONVIFSOAPBuilder.PrepareGotoHome(const aToken:String): String;
const GO_TO_HOME  = '<tptz:GotoHomePosition> '+        
                      '<tptz:ProfileToken>%s</tptz:ProfileToken> '+
                      '</tptz:GotoHomePosition>'+ END_SOAP_XML;
begin
  Result := GetSoapXMLConnection+ Format(GO_TO_HOME,[aToken]);
end;

function TONVIFSOAPBuilder.PrepareGetNodes: String;
const GET_NODES  = '<tptz:GetNodes> '+        
                    '</tptz:GetNodes>'+ END_SOAP_XML;
begin
  Result := GetSoapXMLConnection+ GET_NODES;
end;

function TONVIFSOAPBuilder.PrepareSetHomePosition(const aToken:String): String;
const SET_TO_HOME  = '<tptz:SetHomePosition> '+        
                      '<tptz:ProfileToken>%s</tptz:ProfileToken> '+
                      '</tptz:SetHomePosition>'+ END_SOAP_XML;
begin
  Result := GetSoapXMLConnection+ Format(SET_TO_HOME,[aToken]);
end;

function TONVIFSOAPBuilder.PrepareGetStatus(const aToken: String): String;
const GET_STATUS  = '<tptz:GetStatus> '+        
                      '<tptz:ProfileToken>%s</tptz:ProfileToken> '+
                      '</tptz:GetStatus>'+ END_SOAP_XML;
begin  
  Result := GetSoapXMLConnection+ Format(GET_STATUS,[aToken]);
end;

function TONVIFSOAPBuilder.PrepareGotoPreset(const aToken,aTokenPreset: String): String;
const GO_TO_PRESET  = '<tptz:GotoPreset> '+
                      '<tptz:ProfileToken>%s</tptz:ProfileToken> '+
                      '<tptz:PresetToken>%s</tptz:PresetToken> '+
                      '</tptz:GotoPreset>'+ END_SOAP_XML;
begin
  Result := GetSoapXMLConnection+ Format(GO_TO_PRESET,[aToken,aTokenPreset]);
end;

function TONVIFSOAPBuilder.PrepareSetPreset(const aToken,aTokenPreset,aPresetName: String): String;
const SET_PRESET  = ' <tptz:SetPreset> '+
                      '<tptz:ProfileToken>%s</tptz:ProfileToken> '+
                      '<tptz:PresetName>%s</tptz:PresetName> '+                      
                      '<tptz:PresetToken>%s</tptz:PresetToken> '+
                      '</tptz:SetPreset>'+ END_SOAP_XML;
begin  
  Result := GetSoapXMLConnection+ Format(SET_PRESET,[aToken,aPresetName,aTokenPreset]);
end;

function TONVIFSOAPBuilder.PrepareRemovePreset(const aToken,aTokenPreset: String): String;
const REMOVE_PRESET  = '<tptz:RemovePreset> '+
                      '<tptz:ProfileToken>%s</tptz:ProfileToken> '+
                      '<tptz:PresetToken>%s</tptz:PresetToken> '+
                      '</tptz:RemovePreset>'+ END_SOAP_XML;
begin  
  Result := GetSoapXMLConnection+ Format(REMOVE_PRESET,[aToken,aTokenPreset]);
end;

function TONVIFSOAPBuilder.PrepareStopMoveRequest(const aToken:String): String;
const STOP_PTZ_COMMAND =  '<tptz:Stop>'+
                          '<tptz:ProfileToken>%s</tptz:ProfileToken> '+
                          '<tptz:PanTilt>true</tptz:PanTilt> '+
                          '<tptz:Zoom>false</tptz:Zoom> '+
                          '</tptz:Stop> '+ END_SOAP_XML;
begin
  Result := GetSoapXMLConnection+ Format(STOP_PTZ_COMMAND,[aToken]);
end;


end.
