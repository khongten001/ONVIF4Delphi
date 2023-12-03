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

unit ONVIF.Structure.Common;

interface

type
  /// <summary>
  ///   Represents a simple item for ONVIF configuration.
  /// </summary>
  /// <record name="TSimpleItem">
  ///   <field name="Name" type="String">
  ///     The name of the simple item.
  ///   </field>
  ///   <field name="Value" type="String">
  ///     The value of the simple item.
  ///   </field>
  /// </record>
  TSimpleItem = record
    Name : String;
    Value: String;
  end;
  
  /// <summary>
  ///   Represents a point X and Y for ONVIF configuration.
  /// </summary>
  /// <record name="TRealPoint">
  ///   <field name="x" type="Real">
  ///     The x value.
  ///   </field>
  ///   <field name="y" type="Real">
  ///     The y value
  ///   </field>
  /// </record>
  TRealPoint = record
    x: Real;
    y: Real;
  end;

  /// <summary>
  ///   Represents a point with real coordinates for ONVIF configuration (alias for TRealPoint).
  /// </summary>
  /// <record name="TElementItemXY">
  ///   Same structure as TRealPoint.
  /// </record>  
  TElementItemXY = TRealPoint;


  /// <summary>
  ///   Represents the minimum and maximum values for a range.
  /// </summary>
  /// <record name="TMinMaxValue">
  /// <field name="Min" type="Real">
  ///   The minimum value in the range.
  /// </field>
  /// <field name="Max" type="Real">
  ///   The maximum value in the range.
  /// </field>
  /// </record>  
  TMinMaxValue = Record
     Min : Real;
     Max : Real;
  end;  

  /// <summary>
  ///   Represents a range for Pan-Tilt movement in ONVIF PTZ (Pan-Tilt-Zoom) configuration.
  /// </summary>
  /// <record name="TRangePTZONVIF">
  /// <field name="URI" type="String">
  ///   URI associated with the range.
  /// </field>
  /// <field name="XRange" type="TMinMaxValue">
  ///   X-axis range for Pan movement.
  /// </field>
  /// <field name="YRange" type="TMinMaxValue">
  ///   Y-axis range for Tilt movement.
  /// </field>
  /// </record>  
  TRangeXYPTZONVIF = Record
    URI    : String;
    XRange : TMinMaxValue;
    YRange : TMinMaxValue; 
  end;  

  /// <summary>
  ///   Represents the range of Zoom for ONVIF PTZ (Pan-Tilt-Zoom) configuration.
  /// </summary>
  /// <record name="TRangeZoomPTZONVIF">
  /// <field name="URI" type="String">
  ///   URI associated with the Zoom range.
  /// </field>
  /// <field name="XRange" type="TMinMaxValue">
  ///   X-axis range for Zoom.
  /// </field>
  /// </record>  
  TRangeXPTZONVIF = Record
    URI    : String;
    XRange : TMinMaxValue; 
  End;    

implementation

end.
