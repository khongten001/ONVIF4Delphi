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

unit ONVIF.Structure.Device;

interface
  Type
  /// <summary>
  ///   Represents information about a device, including manufacturer, model, firmware version,
  ///   serial number, hardware ID, and XAddr.
  /// </summary>
  TDeviceInformation = record
    Manufacturer   : string;
    Model          : string;
    FirmwareVersion: String;
    SerialNumber   : String;
    HardwareId     : String;
    XAddr          : String;
  end;  

  /// <summary>
  /// Represents information about a network interface.
  /// </summary>
  /// <remarks>
  ///   This record contains details such as the name, hardware address, and Maximum Transmission Unit (MTU) of the network interface.
  /// </remarks>
  TInfoNetworkInterface = record
    Name      : String;
    HwAddress : String;
    MTU       : Integer;
  end;

  /// <summary>
  /// Represents administrative settings for a network link.
  /// </summary>
  /// <remarks>
  ///   This record includes information about auto-negotiation, speed, and duplex settings.
  /// </remarks>
  TLinkAdminSettings = record
    AutoNegotiation : Boolean;
    Speed           : Integer;
    Duplex          : String;  
  end;

  /// <summary>
  /// Represents IP address representation settings.
  /// </summary>
  TIPRappresentation = record
    Address      : String;
    PrefixLength : Integer;
  end;

  /// <summary>
  /// Represents IPv4 configuration settings.
  /// </summary>
  TIpV4Config = record
    Manual : TIPRappresentation;
    DHCP   : Boolean; 
  end;
  
  /// <summary>
  /// Represents IPv4 network interface settings.
  /// </summary>
  TIpV4NetworkInterface = record
    Enabled   : Boolean;  
    Config    : TIpV4Config;
  end;
  
  /// <summary>
  /// Represents IPv6 configuration settings.
  /// </summary>
  TIpV6Config = record
    AcceptRouterAdvert : Boolean;    
    DHCP               : String;
    LinkLocal          : TIPRappresentation; 
    FromDHCP           : TIPRappresentation;
  end;  

  /// <summary>
  /// Represents IPv6 network interface settings.
  /// </summary>
  TIpV6NetworkInterface = Record
    Enabled   : Boolean;   
    Config    : TIpV6Config;     
  End;
  
  /// <summary>
  /// Represents settings for a network link.
  /// </summary>
  TLinkNetworkInterface = record
    AdminSettings  : TLinkAdminSettings;
    OperSettings   : TLinkAdminSettings;
    InterfaceType  : Integer;    
  end;    

  /// <summary>
  /// Represents a network interface.
  /// </summary>
  TNetworkInterface = record
    Token          : string;
    Enabled        : Boolean;
    Info           : TInfoNetworkInterface;
    Link           : TLinkNetworkInterface;
    IPv4           : TIpV4NetworkInterface;
    Ipv6           : TIpV6NetworkInterface;
  end;


  /// <summary>
  ///   Represents timezone information for ONVIF SystemDateAndTime.
  /// </summary>
  /// <record name="TONVIFTimeZone">
  ///   <field name="TZ" type="String">
  ///     Timezone identifier.
  ///   </field>
  ///   <field name="DaylightSavings" type="TONVIFDaylightSavings">
  ///     Daylight savings information.
  ///   </field>
  /// </record>
  TONVIFTimeZone = record
    TZ   : String;                     
  end;

  /// <summary>
  ///   Represents the time information for ONVIF SystemDateAndTime.
  /// </summary>
  /// <record name="TONVIFTime">
  ///   <field name="Hour" type="Word">
  ///     Hours.
  ///   </field>
  ///   <field name="Minute" type="Word">
  ///     Minutes.
  ///   </field>
  ///   <field name="Second" type="Word">
  ///     Seconds.
  ///   </field>
  /// </record>
  TONVIFTime = record
    Hour  : Word; 
    Minute: Word; 
    Second: Word; 
  end;

  /// <summary>
  ///   Represents the date information for ONVIF SystemDateAndTime.
  /// </summary>
  /// <record name="TONVIFDate">
  ///   <field name="Year" type="Word">
  ///     Year.
  ///   </field>
  ///   <field name="Month" type="Word">
  ///     Month.
  ///   </field>
  ///   <field name="Day" type="Word">
  ///     Day.
  ///   </field>
  /// </record>
  TONVIFDate = record
    Year : Word;  
    Month: Word; 
    Day  : Word;   
  end;

  
  /// <summary>
  ///   Represents UTC o Local date and time information for ONVIF SystemDateAndTime.
  /// </summary>
  /// <record name="TONVIFUTCDateTime">
  ///   <field name="Time" type="TTime">
  ///     UTC time.
  ///   </field>
  ///   <field name="Date" type="TDate">
  ///     UTC date.
  ///   </field>
  /// </record>
  TONVIFDateTime = record
    Time: TONVIFTime;
    Date: TONVIFDate;
  end;

  /// <summary>
  ///   Represents the full date and time information for ONVIF SystemDateAndTime.
  /// </summary>
  /// <record name="TONVIFSystemDateAndTime">
  ///   <field name="DateTimeType" type="TONVIFDateTimeType">
  ///     Type of the date and time.
  ///   </field>
  ///   <field name="DaylightSavings" type="TONVIFDaylightSavings">
  ///     Daylight savings information.
  ///   </field>
  ///   <field name="TimeZone" type="TONVIFTimeZone">
  ///     Timezone information.
  ///   </field>
  ///   <field name="UTCDateTime" type="TONVIFUTCDateTime">
  ///     UTC date and time.
  ///   </field>
  ///   <field name="LocalDateTime" type="TLocalDateTime">
  ///     Local date and time.
  ///   </field>
  /// </record>
  TONVIFSystemDateAndTime = record
    DateTimeType    : String;    
    DaylightSavings : Boolean; 
    TimeZone        : TONVIFTimeZone;        
    UTCDateTime     : TONVIFDateTime;     
    LocalDateTime   : TONVIFDateTime;        
  end;
  


implementation

end.
