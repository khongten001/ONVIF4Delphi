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

unit ONVIF.Structure.PTZ;

interface

uses System.Generics.Collections,ONVIF.Structure.Common;


Type

  /// <summary>
  /// Represents supported preset tour information.
  /// </summary>
  /// <record name="TSupportedPresetTour">
  ///   <field name="MaximumNumberOfPresetTours" type="Integer">
  ///     The maximum number of supported preset tours.
  ///   </field>
  ///   <field name="PTZPresetTourOperation" type="TArray<String>">
  ///     The array of supported PTZ preset tour operations.
  ///   </field>
  /// </record>
  TSupportedPresetTour = record
    MaximumNumberOfPresetTours : Integer;
    PTZPresetTourOperation     : TArray<String>;
  end;

  /// <summary>
  /// Represents an extension containing supported preset tour information.
  /// </summary>
  /// <record name="TExtension">
  ///   <field name="SupportedPresetTour" type="TSupportedPresetTour">
  ///     The supported preset tour information within the extension.
  ///   </field>
  /// </record>
  TExtension = Record
    SupportedPresetTour : TSupportedPresetTour;
  end;

  /// <summary>
  /// Represents an array of XY PTZ ONVIF ranges.
  /// </summary>
  /// <remarks>
  ///   This type definition represents an array of XY PTZ ONVIF ranges.
  /// </remarks>
  TArrayXY = TArray<TRangeXYPTZONVIF>;

  /// <summary>
  /// Represents an array of X PTZ ONVIF ranges.
  /// </summary>
  /// <remarks>
  ///   This type definition represents an array of X PTZ ONVIF ranges.
  /// </remarks>
  TArrayX  = TArray<TRangeXPTZONVIF>;

  
  /// <summary>
  /// Represents supported PTZ spaces.
  /// </summary>
  /// <record name="TSupportedPTZSpaces">
  ///   <field name="AbsolutePanTiltPositionSpace" type="TArrayXY">
  ///     The array of absolute Pan/Tilt position spaces.
  ///   </field>
  ///   <field name="AbsoluteZoomPositionSpace" type="TArrayX">
  ///     The array of absolute Zoom position spaces.
  ///   </field>
  ///   <field name="RelativePanTiltTranslationSpace" type="TArrayXY">
  ///     The array of relative Pan/Tilt translation spaces.
  ///   </field>
  ///   <field name="RelativeZoomTranslationSpace" type="TArrayX">
  ///     The array of relative Zoom translation spaces.
  ///   </field>
  ///   <field name="ContinuousPanTiltVelocitySpace" type="TArrayXY">
  ///     The array of continuous Pan/Tilt velocity spaces.
  ///   </field>
  ///   <field name="ContinuousZoomVelocitySpace" type="TArrayX">
  ///     The array of continuous Zoom velocity spaces.
  ///   </field>
  ///   <field name="PanTiltSpeedSpace" type="TRangeXPTZONVIF">
  ///     The range of Pan/Tilt speed spaces.
  ///   </field>
  ///   <field name="ZoomSpeedSpace" type="TRangeXPTZONVIF">
  ///     The range of Zoom speed spaces.
  ///   </field>
  /// </record>  
  TSupportedPTZSpaces = record
    AbsolutePanTiltPositionSpace     : TArrayXY;
    AbsoluteZoomPositionSpace        : TArrayX;
    RelativePanTiltTranslationSpace  : TArrayXY;
    RelativeZoomTranslationSpace     : TArrayX;
    ContinuousPanTiltVelocitySpace   : TArrayXY;
    ContinuousZoomVelocitySpace      : TArrayX;
    PanTiltSpeedSpace                : TRangeXPTZONVIF;
    ZoomSpeedSpace                   : TRangeXPTZONVIF;
  end;
  
  /// <summary>
  ///  Represents the Node in PTZ (Pan-Tilt-Zoom) system.
  ///
  ///	A PTZ-capable device can have multiple PTZ nodes. The PTZ nodes may represent mechanical 
  ///	PTZ drivers, uploaded PTZ drivers or digital PTZ drivers. PTZ nodes are the lowest level entities 
  ///	in the PTZ control API and reflect the supported PTZ capabilities. The PTZ node is referenced 
  ///	either by its name or by its reference token. The PTZ Service does not provide operations to 
  ///	create or manipulate PTZ nodes.
  ///	The following properties shall be provided for all PTZ nodes:
  ///	  • Token – A unique identifier that is used to reference PTZ nodes.
  ///	  • Name – A name given by the installer.
  ///	  • SupportedPTZSpaces – A list of coordinate systems available for the PTZ node. For 
  ///	each Coordinate System, the PTZ node shall specify its allowed range.
  ///	  • MaximumNumberOfPresets – All preset operations shall be available for this PTZ node 
  ///	if one preset is supported.
  ///	  • HomeSupported – A boolean operator specifying the availability of a home position. If 
  ///	set to true, the Home Position Operations shall be available for this PTZ node.
  ///	  • AuxiliaryCommands – A list of supported Auxiliary commands. If the list is not empty, 
  ///	the Auxiliary Operations shall be available for this PTZ node. A device may use 
  ///	auxiliary commands that are described in Core Specification.
  ///	  • MaximumNumberOfPresetTours – Indicates number of preset tours that can be created. 
  ///	Required preset tour operations shall be available for this PTZ Node if one or more 
  ///	preset tour is supported
  /// </summary>  
  TPTZNode = record
    FixedHomePosition      : Boolean;
    GeoMove                : Boolean;
    Token                  : String;
    SupportedPTZSpaces     : TSupportedPTZSpaces;
    MaximumNumberOfPresets : Integer;
    HomeSupported          : Boolean;
    AuxiliaryCommands      : TArray<TAuxiliaryCommand>;
    Extension              : TExtension;
  end;
  
  /// <summary>
  ///   Represents the pan and tilt coordinates in a PTZ (Pan-Tilt-Zoom) system.
  /// </summary>
  TPTZPanTilt = Record
    X : double; 
    Y : double; 
  end;

  /// <summary>
  ///   Represents the position in a PTZ (Pan-Tilt-Zoom) system, including pan-tilt
  ///   coordinates and zoom level.
  /// </summary>  
  TPTZPosition = record
    PanTilt: TPTZPanTilt;
    Zoom   : Double;
  end;

  /// <summary>
  ///   Represents a PTZ preset with a token, name, and associated position.
  /// </summary>  
  TPTZPreset = record
    Token       : String;
    Name        : String;
    PTZPosition : TPTZPosition
  end;
  PTPTZPreset = ^TPTZPreset;
  
  /// <summary>
  /// Represents the status of PTZ modes, including Pan/Tilt and Zoom.
  /// </summary>
  /// <remarks>
  ///   This record provides information about the Pan/Tilt and Zoom modes for PTZ.
  /// </remarks>  
  TPTZModeStatus = Record
    PanTilt : String;
    Zoom    : String;
  End;
  
  /// <summary>
  ///	<tptz:PTZStatus>
  ///		<tt:Position>
  ///			<tt:PanTilt x="0.000000"
  ///			            y="0.636364"
  ///			            space="http://www.onvif.org/ver10/tptz/PanTiltSpaces/PositionGenericSpace"/>
  ///			<tt:Zoom x="0.000000"
  ///			         space="http://www.onvif.org/ver10/tptz/ZoomSpaces/PositionGenericSpace"/>
  ///		</tt:Position>
  ///		<tt:MoveStatus>
  ///			<tt:PanTilt>IDLE</tt:PanTilt>
  ///			<tt:Zoom>IDLE</tt:Zoom>
  ///		</tt:MoveStatus>
  ///		<tt:Error>NO error</tt:Error>
  ///		<tt:UtcTime>2023-12-03T23:46:30Z</tt:UtcTime>
  ///	</tptz:PTZStatus>
  /// </summary>  
  TPTZStatus = Record
    PTZPosition : TPTZPosition;
    MoveStatus  : TPTZModeStatus;
    Error       : String;
    UtcTime     : TDateTime;
  End;
    
  /// <summary>
  /// Represents a list of PTZ presets.
  /// </summary>
  /// <remarks>
  ///   This class extends TList<PTPTZPreset> to manage a collection of PTZ presets.
  /// </remarks>  
  TPTZPresetList = Class(TList<PTPTZPreset>)
  private
    /// <summary>
    /// Event handler for item removal from the PTZ preset list.
    /// </summary>
    /// <param name="Sender">The object triggering the event.</param>
    /// <param name="Item">The PTZ preset being removed.</param>
    /// <param name="Action">The action indicating how the item was removed.</param>  
    procedure DoItemRemoved(Sender: TObject; const Item: PTPTZPreset;Action: TCollectionNotification);
  public       
    /// <summary>
    /// Constructs a new instance of TPTZPresetList.
    /// </summary>     
    constructor Create; reintroduce;
  end;

implementation

{ TPTZPresetList }
procedure TPTZPresetList.DoItemRemoved(Sender: TObject; const Item: PTPTZPreset; Action: TCollectionNotification);
begin
  if Action in [cnDeleting, cnRemoved] then  
    Dispose(item)
end;

constructor TPTZPresetList.Create;
begin
  inherited Create;
  OnNotify := DoItemRemoved;
end;

end.
