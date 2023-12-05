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

unit ONVIF.Types;
                                
interface
 Type
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
  

implementation

end.
