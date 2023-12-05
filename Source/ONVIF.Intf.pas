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

unit ONVIF.Intf;

interface
                          
uses ONVIF.Types,ONVIF.SOAP.Builder,XmlDoc, XmlIntf, XMLDom;

  Type

  IONVIFManager = interface 
    ['{A0935A5A-F0FD-45E0-881F-2B04574A2E94}']
    
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
    function ExecuteRequest(aAddrType: TONVIFAddrType;const aMethodName, aRequest: String; Var aAnswer: String): Boolean;

    procedure SetLastStatusCode(const aMethodName:String;const aErrorCode:Integer);
    
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
    procedure DoWriteLog(const aFunction, aDescription: String;aLevel: TPONVIFLivLog; aIsVerboseLog: boolean=false);
    function GetSpeed: Byte;
    procedure SetSpeed(const Value: Byte);
    function GetSOAPBuilder: TONVIFSOAPBuilder;
    procedure WriteLastErrorCodeLog(const aMethodName: String);
    procedure SetTokenImagingByPTZToken;
    Function IsValidSoapXML(const aRootNode :IXMLNode;var aErrorFound:Boolean):Boolean;
    /// <summary>
    ///   Gets or sets the speed parameter for PTZ operations.
    /// </summary>
    property Speed                   : Byte                read GetSpeed                   write SetSpeed;
    
  end;

implementation

end.
