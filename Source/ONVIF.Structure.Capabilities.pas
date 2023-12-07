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
unit ONVIF.Structure.Capabilities;

interface

uses ONVIF.Structure.Common;

  Type

  /// <summary>
  ///   Represents extension settings for network configuration in ONVIF.
  /// </summary>
  /// <record name="TExtensionNetworkONVIF">
  ///   <field name="Dot11Configuration" type="Boolean">
  ///     Indicates whether Dot11 configuration is supported.
  ///   </field>
  /// </record>  
  TExtensionNetworkONVIF = record
    Dot11Configuration : Boolean;
    DHCPv6             : Boolean;
    Dot1XConfigurations: Integer;
  end;

  /// <summary>
  ///   Represents network configuration settings in ONVIF.
  /// </summary>
  /// <record name="TNetworkONVIF">
  ///   <field name="IPFilter" type="Boolean">
  ///     Indicates whether IP filtering is supported.
  ///   </field>
  ///   <field name="ZeroConfiguration" type="Boolean">
  ///     Indicates whether zero configuration is supported.
  ///   </field>
  ///   <field name="IPVersion6" type="Boolean">
  ///     Indicates whether IP version 6 is supported.
  ///   </field>
  ///   <field name="DynDNS" type="Boolean">
  ///     Indicates whether DynDNS is supported.
  ///   </field>
  ///   <field name="Extension" type="TExtensionNetworkONVIF">
  ///     Extension settings for network configuration.
  ///   </field>
  /// </record>  
  TNetworkONVIF = Record
    IPFilter         : Boolean;
    ZeroConfiguration: Boolean;
    IPVersion6       : Boolean;
    DynDNS           : Boolean;
    Extension        : TExtensionNetworkONVIF;
  end;

  /// <summary>
  ///   Represents supported versions for system configuration in ONVIF.
  /// </summary>
  /// <record name="TSupportedVersionsSytemONVIF">
  ///   <field name="Major" type="Integer">
  ///     The major version number.
  ///   </field>
  ///   <field name="Minor" type="Integer">
  ///     The minor version number.
  ///   </field>
  /// </record>  
  TSupportedVersionsSytemONVIF = record
    Major : Integer;
    Minor : integer;
  end;

  /// <summary>
  /// Represents ONVIF system extension information.
  /// </summary>
  /// <record name="TExtensionSystem">
  ///   <field name="HttpFirmwareUpgrade" type="Boolean">
  ///     Indicates whether HTTP firmware upgrade is supported.
  ///   </field>
  ///   <field name="HttpSystemBackup" type="Boolean">
  ///     Indicates whether HTTP system backup is supported.
  ///   </field>
  ///   <field name="HttpSystemLogging" type="Boolean">
  ///     Indicates whether HTTP system logging is supported.
  ///   </field>
  ///   <field name="HttpSupportInformation" type="Boolean">
  ///     Indicates whether HTTP support information is supported.
  ///   </field>
  /// </record>  
  TExtensionSystem = record
    HttpFirmwareUpgrade     : Boolean;
    HttpSystemBackup        : Boolean;
    HttpSystemLogging       : Boolean;
    HttpSupportInformation  : Boolean;
  end;
  

  /// <summary>
  ///   Represents system configuration settings in ONVIF.
  /// </summary>
  /// <record name="TSystemONVIF">
  ///   <field name="DiscoveryResolve" type="Boolean">
  ///     Indicates whether discovery resolve is supported.
  ///   </field>
  ///   <field name="DiscoveryBye" type="Boolean">
  ///     Indicates whether discovery bye is supported.
  ///   </field>
  ///   <field name="RemoteDiscovery" type="Boolean">
  ///     Indicates whether remote discovery is supported.
  ///   </field>
  ///   <field name="SystemBackup" type="Boolean">
  ///     Indicates whether system backup is supported.
  ///   </field>
  ///   <field name="SystemLogging" type="Boolean">
  ///     Indicates whether system logging is supported.
  ///   </field>
  ///   <field name="FirmwareUpgrade" type="Boolean">
  ///     Indicates whether firmware upgrade is supported.
  ///   </field>
  ///   <field name="SupportedVersions" type="TSupportedVersionsSytemONVIF">
  ///     Supported versions for system configuration.
  ///   </field>
  ///   <field name="Extension" type="TExtensionSystem">
  ///     ONVIF system extension information.
  ///   </field>
  /// </record>  
  TSystemONVIF = Record
     DiscoveryResolve : Boolean;
     DiscoveryBye     : Boolean;
     RemoteDiscovery  : Boolean;
     SystemBackup     : Boolean;
     SystemLogging    : Boolean;
     FirmwareUpgrade  : Boolean;  
     SupportedVersions: TArray<TSupportedVersionsSytemONVIF>; 
     Extension        : TExtensionSystem;            
  end;

  /// <summary>
  /// Represents an IO extension with an array of auxiliary commands.
  /// </summary>
  /// <record name="TExtensionIO">
  ///   <field name="AuxiliaryCommands" type="TArray<TAuxiliaryCommand>">
  ///     An array of auxiliary commands associated with the IO extension.
  ///   </field>
  /// </record>  
  TExtensionIO = record
    AuxiliaryCommands : TArray<TAuxiliaryCommand>;
  end;
  
  /// <summary>
  /// Represents ONVIF IO information, including the number of input connectors, relay outputs, and IO extensions.
  /// </summary>
  /// <record name="TIOOnvif">
  ///   <field name="InputConnectors" type="Integer">
  ///     The number of input connectors for ONVIF IO.
  ///   </field>
  ///   <field name="RelayOutputs" type="Integer">
  ///     The number of relay outputs for ONVIF IO.
  ///   </field>
  ///   <field name="Extension" type="TExtensionIO">
  ///     IO extension information, including an array of auxiliary commands.
  ///   </field>
  /// </record>  
  TIOOnvif = record
    InputConnectors : Integer;
    RelayOutputs    : Integer;
    Extension       : TExtensionIO;
  end;  

  /// <summary>
  ///   Represents ONVIF device information.
  /// </summary>
  /// <record name="TDeviceONVIF">
  ///   <field name="XAddr" type="String">
  ///     The XAddr (service endpoint) of the ONVIF device.
  ///   </field>
  ///   <field name="Network" type="TNetworkONVIF">
  ///     Network configuration settings for the ONVIF device.
  ///   </field>
  ///   <field name="System" type="TSystemONVIF">
  ///     System configuration settings for the ONVIF device.
  ///   </field>
  ///   <field name="System" type="TSystemONVIF">
  ///     System IO settings for the ONVIF device.
  ///   </field>
  /// </record>  
  TDeviceONVIF = Record
    XAddr     : String;
    Network   : TNetworkONVIF;
    System    : TSystemONVIF;
    IO        : TIOOnvif;
//    Security  : 
  end;

  /// <summary>
  ///   Represents ONVIF events information.
  /// </summary>
  /// <record name="TEventsONVIF">
  ///   <field name="XAddr" type="String">
  ///     The XAddr (service endpoint) for ONVIF events.
  ///   </field>
  ///   <field name="WSSubscriptionPolicySupport" type="Boolean">
  ///     Indicates whether WS Subscription Policy is supported.
  ///   </field>
  ///   <field name="WSPullPointSupport" type="Boolean">
  ///     Indicates whether WS Pull Point is supported.
  ///   </field>
  ///   <field name="WSPausableSubscriptionManagerInterfaceSupport" type="Boolean">
  ///     Indicates whether WS Pausable Subscription Manager is supported.
  ///   </field>
  /// </record>  
  TEventsONVIF = Record
     XAddr                                         : String;
     WSSubscriptionPolicySupport                   : Boolean;
     WSPullPointSupport                            : Boolean;     
     WSPausableSubscriptionManagerInterfaceSupport : Boolean;          
  end;
  
  /// <summary>
  ///   Represents streaming capabilities information for ONVIF.
  /// </summary>
  /// <record name="TStreamingCapabilitiesONVIF">
  ///   <field name="RTPMulticast" type="Boolean">
  ///     Indicates whether RTP Multicast is supported.
  ///   </field>
  ///   <field name="RTP_TCP" type="Boolean">
  ///     Indicates whether RTP over TCP is supported.
  ///   </field>
  ///   <field name="RTP_RTSP_TCP" type="Boolean">
  ///     Indicates whether RTP over RTSP over TCP is supported.
  ///   </field>
  /// </record>  
  TStreamingCapabilitiesONVIF = record
     RTPMulticast : Boolean;
     RTP_TCP      : Boolean;
     RTP_RTSP_TCP : Boolean;
  end;
  
  /// <summary>
  ///   Represents ONVIF media information.
  /// </summary>
  /// <record name="TMediaONVIF">
  ///   <field name="XAddr" type="String">
  ///     The XAddr (service endpoint) for ONVIF media.
  ///   </field>
  ///   <field name="StreamingCapabilities" type="TStreamingCapabilitiesONVIF">
  ///     Streaming capabilities for ONVIF media.
  ///   </field>
  /// </record>  
  TMediaONVIF = Record
    XAddr                 : String;
    StreamingCapabilities : TStreamingCapabilitiesONVIF;
  end;

  /// <summary>
  ///   Represents ONVIF PTZ (Pan-Tilt-Zoom) information.
  /// </summary>
  /// <record name="TPTZONVIF">
  ///   <field name="XAddr" type="String">
  ///     The XAddr (service endpoint) for ONVIF PTZ.
  ///   </field>
  /// </record>  
  TPTZONVIF = Record
     XAddr : String;
  end;  

  /// <summary>
  ///   Represents ONVIF search extension information.
  /// </summary>
  /// <record name="TSearchExtensionONVIF">
  ///   <field name="XAddr" type="String">
  ///     The XAddr (service endpoint) for ONVIF search extension.
  ///   </field>
  ///   <field name="MetadataSearch" type="Boolean">
  ///     Indicates whether metadata search is supported.
  ///   </field>
  /// </record>  
  TSearchExtensionONVIF = record
    XAddr          : String;
    MetadataSearch : Boolean;
  end;

  /// <summary>
  ///   Represents ONVIF extension information XAddr.
  /// </summary>
  /// <record name="TReplayExtensionONVIF">
  ///   <field name="XAddr" type="String">
  ///     The XAddr (service endpoint) for ONVIF replay extension.
  ///   </field>
  /// </record>  
  TXAddrExtensionONVIF = record
    XAddr : String;
  end;

  /// <summary>
  ///   Record representing ONVIF recording extension settings.
  ///   • XAddr: String representing the address.
  ///   • ReceiverSource: Boolean indicating support for receiver as a source.
  ///   • MediaProfileSource: Boolean indicating support for media profile as a source.
  ///   • DynamicRecordings: Boolean indicating support for dynamic recordings.
  ///   • DynamicTracks: Boolean indicating support for dynamic tracks.
  ///   • MaxStringLength: Integer representing the maximum string length.
  /// </summary>  
  TRecordingExtensionONVIF = record
    XAddr              : String;
    ReceiverSource     : Boolean;
    MediaProfileSource : Boolean;
    DynamicRecordings  : Boolean;
    DynamicTracks      : Boolean;
    MaxStringLength    : Integer;
  end;

  /// <summary>
  ///   Represents ONVIF extension information.
  /// </summary>
  /// <record name="TExtensionONVIF">
  ///   <field name="Search" type="TSearchExtensionONVIF">
  ///     ONVIF search extension information.
  ///   </field>
  ///   <field name="Replay" type="TReplayExtensionONVIF">
  ///     ONVIF replay extension information.
  ///   </field>
  /// </record>
  TExtensionONVIF = Record
     Search         : TSearchExtensionONVIF;
     Replay         : TXAddrExtensionONVIF;
     DeviceIO       : TXAddrExtensionONVIF;
     VideoSources   : Integer;
     VideoOutputs   : Integer;
     AudioSources   : Integer;
     AudioOutputs   : Integer; 
     RelayOutputs   : integer; 
     Recording      : TRecordingExtensionONVIF;     
  end;

  /// <summary>
  /// Represents ONVIF analytics information, including the XAddr, rule support, and analytics module support.
  /// </summary>
  /// <remarks>
  ///   This record is used to store information related to ONVIF analytics, providing details about the XAddr, rule support,
  ///   and analytics module support.
  /// </remarks>  
  TAnalyticsONVIF = Record
    XAddr                   : String;
    RuleSupport             : Boolean;
    AnalyticsModuleSupport  : Boolean;
  End;
  
  /// <summary>
  ///   Represents ONVIF capabilities information.
  /// </summary>
  /// <record name="TCapabilitiesOVIF">
  ///   <field name="Device" type="TDeviceONVIF">
  ///     ONVIF device capabilities.
  ///   </field>
  ///   <field name="Events" type="TEventsONVIF">
  ///     ONVIF events capabilities.
  ///   </field>
  ///   <field name="Media" type="TPTZONVIF">
  ///     ONVIF PTZ capabilities.
  ///   </field>
  ///   <field name="Extension" type="TExtensionONVIF">
  ///     ONVIF extension capabilities.
  ///   </field>
  /// </record>  
  TCapabilitiesONVIF = Record
    Analytics  : TAnalyticsONVIF;
    Device     : TDeviceONVIF;
    Imaging    : TXAddrExtensionONVIF;
    Events     : TEventsONVIF;
    Media      : TMediaONVIF;
    PTZ        : TPTZONVIF;
    Extension  : TExtensionONVIF;
  end;

implementation


end.
