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
  
  /// <summary>
  /// Interface for managing ONVIF functionality.
  /// </summary>
  /// <remarks>
  ///   This interface provides methods to interact with ONVIF devices, including
  ///   retrieving speed, constructing SOAP messages, logging error codes, setting
  ///   imaging tokens based on PTZ tokens, and more.
  /// </remarks>
  IONVIFManager = interface 
    ['{A0935A5A-F0FD-45E0-881F-2B04574A2E94}']
    
    /// <summary>
    /// Executes an ONVIF request with the specified address type, method name, and request.
    /// </summary>
    /// <param name="aAddrType">The type of ONVIF address to use for the request.</param>
    /// <param name="aMethodName">The name of the method that called ExecuteRequest for logging purposes.</param>
    /// <param name="aRequest">The ONVIF request to be sent.</param>
    /// <param name="aAnswer">The variable to store the ONVIF response.</param>
    /// <returns>
    ///   <c>True</c> if the request is successfully executed; otherwise, <c>False</c>.
    /// </returns>
    function ExecuteRequest(aAddrType: TONVIFAddrType;const aMethodName, aRequest: String; Var aAnswer: String): Boolean;

    /// <summary>
    /// Sets the last status code for a specific method and logs the method name for reference.
    /// </summary>
    /// <param name="aMethodName">The name of the method, used for logging purposes.</param>
    /// <param name="aErrorCode">The error code to be set.</param>   
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
    
    /// <summary>
    /// Gets the speed value of PTZ.
    /// </summary>
    /// <returns>The speed value as a byte.</returns>    
    function GetSpeed: Byte;
    
    /// <summary>
    /// Sets the speed value of PTZ.
    /// </summary>
    /// <param name="Value">The byte value to set as the speed.</param>    ///
    procedure SetSpeed(const Value: Byte);
    
    /// <summary>
    /// Gets the ONVIF SOAP builder for constructing XML requests.
    /// </summary>
    /// <returns>The ONVIF SOAP builder instance.</returns>    
    function GetSOAPBuilder: TONVIFSOAPBuilder;

    /// <summary>
    /// Writes the last error code to the log for a specific method.
    /// </summary>
    /// <param name="aMethodName">The name of the method for logging purposes.</param>    
    procedure WriteLastErrorCodeLog(const aMethodName: String);

    /// <summary>
    /// Sets the imaging token by searching for the corresponding profile based on the PTZ token.
    /// </summary>
    procedure SetTokenImagingByPTZToken;

    /// <summary>
    /// Gets the XML body node from the response text.
    /// </summary>
    /// <param name="aResponseText">The response text containing XML.</param>
    /// <returns>The XML body node.</returns>    
    function GetBodyNode(const aResponseText:String): IXMLNode;

    /// <summary>
    ///   Gets or sets the speed parameter for PTZ operations.
    /// </summary>
    property Speed                   : Byte                read GetSpeed                   write SetSpeed;
    
  end;

implementation

end.
