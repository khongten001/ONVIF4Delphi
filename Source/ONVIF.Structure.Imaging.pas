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


unit ONVIF.Structure.Imaging;

interface

uses ONVIF.Structure.Common;

Type

  /// <summary>
  ///
  ///   AutoFocusMode
  ///   DefaultSpeed
  ///   NearLimit
  ///   Parameter to set autofocus near limit (unit: meter). 
  ///   FarLimit
  ///   Parameter to set autofocus far limit (unit: meter). If set to 0.0, infinity will be used.
  /// </summary>  
  TImagingFocusMode = ( ifmUnknown,ifmAutoMode,ifmDefaultSpeed,ifmNearLimit,ifmFarLimit,ifmManual);

  /// <summary>
  ///  Enumeration representing the mode of the IR cut filter.
  ///  The IR cut filter is a device used in imaging systems to block infrared light during the day for better color reproduction
  ///  and allow infrared light at night for improved low-light visibility.
  ///    • icfmUnknown: epresents an unknown or undefined state of the IR cut filter.
  ///    • icfmOFF: Enable IR cut fiter. Typically day mode.
  ///    • icfmON: Disable IR cut filter. Typically night mode.
  ///    • icfmAuto: Ir cut filter is automatically activated by the device.
  /// </summary>    
  TIrCutFilterMode  = ( icfmUnknown,icfmAuto,icfmON,icfmOFF);
  
  /// <summary>
  /// xs:complexType name="Exposure"/> 
  /// xs:element name="Mode" type="tt:ExposureMode"/> 
  /// xs:element name="Priority" type="tt:ExposurePriority"/> 
  /// xs:element name="Window" type= "tt:Rectangle"/> 
  /// xs:element name="MinExposureTime" type="xs:float"/> 
  /// xs:element name="MaxExposureTime" type="xs:float"/> 
  /// xs:element name="MinGain" type="xs:float"/> 
  /// xs:element name="MaxGain" type="xs:float"/> 
  /// xs:element name="MinIris" type="xs:float"/> 
  /// xs:element name="MaxIris" type="xs:float"/> 
  /// xs:element name="ExposureTime" type="xs:float"/> 
  /// xs:element name="Gain" type="xs:float"/> 
  /// xs:element name="Iris" type="xs:float"/> 
  /// /xs:complexType>   
  ///	 Mode
  ///	Exposure Mode 
  ///	 Auto – Enabled the exposure algorithm on the device. 
  ///	 Manual – Disabled exposure algorithm on the device. 
  ///	 Priority
  ///	The exposure priority mode (low noise/framerate). 
  ///	 Window
  ///	Rectangular exposure mask. 
  ///	 MinExposureTime
  ///	Minimum value of exposure time range allowed to be used by the algorithm. 
  ///	 MaxExposureTime
  ///	Maximum value of exposure time range allowed to be used by the algorithm. 
  ///	 MinGain
  ///	Minimum value of the sensor gain range that is allowed to be used by the algorithm. 
  ///	 MaxGain
  ///	Maximum value of the sensor gain range that is allowed to be used by the algorithm. 
  ///	 MinIris
  ///	Minimum value of the iris range allowed to be used by the algorithm. 
  ///	 MaxIris
  ///	Maximum value of the iris range allowed to be used by the algorithm. 
  ///	 ExposureTime
  ///	The fixed exposure time used by the image sensor (μs). 
  ///	 Gain
  ///	The fixed gain used by the image sensor (dB). 
  ///	 Iris
  ///	The fixed attenuation of input light affected by the iris (dB). 0dB maps to a fully opened iris
  /// </summary>
  TImagingExposure = record
     Mode            : String;
     Priority        : String;
     MinExposureTime : Double;
     MaxExposureTime : Double;
     MinGain         : Double;
     MaxGain         : Double;
     MinIris         : Double;
     MaxIris         : Double;
     ExposureTime    : Double;
     Gain            : Double;
     Iris            : Double
  end;

  /// <summary>
  ///   Record representing options for continuous focus in an imaging system.
  ///   •  Speed of continuous focus, specified as a range using TMinMaxValue.
  ///   •  Indicates whether continuous focus is supported or not.
  /// </summary>                                 
  TContinuousFocusOptions = Record
    Speed    : TMinMaxValue;
    Supported: Boolean;
  End;
  
  /// <summary>
  ///   Record representing options for Absolute focus in an imaging system.
  ///   •  Speed of continuous focus, specified as a range using TMinMaxValue.
  ///   •  Indicates whether continuous focus is supported or not.
  ///   •  Position ??
  /// </summary>   
  TAbsoluteFocusOptions = Record
    Speed    : TMinMaxValue;
    Position : TMinMaxValue;    
    Supported: Boolean;
  End;

  /// <summary>
  ///   Record representing options for Relative focus in an imaging system.
  ///   • Speed: Speed of continuous focus, specified as a range using TMinMaxValue.
  ///   • Distance: Represents the distance of focus adjustment, specified as a range using TMinMaxValue.
  ///   • Supported: Indicates whether continuous focus is supported or not.
  /// </summary>  
  TRelativeFocusOptions = Record
    Speed    : TMinMaxValue;
    Distance : TMinMaxValue;
    Supported: Boolean;
  End;  

  /// <summary>
  ///   Record representing focus options in an imaging system.
  /// </summary>  
  TFocusOptions = record
    Relative   : TRelativeFocusOptions;
    Continuous : TContinuousFocusOptions;
    Absolute   : TAbsoluteFocusOptions;
  end;
  
  /// <summary>
  ///   Record representing the status of focus.
  ///   • Position: Double representing the current focus position.
  ///   • MoveStatus: String indicating the movement status of the focus.
  ///   • Error: String providing information about any focus-related errors.
  /// </summary>
  TFocusStatus = Record
    Position   : Double;
    MoveStatus : String;
    Error      : String;
  End;

  /// <summary>
  ///   Record representing focus settings.
  ///   • Status: Status information indicating the current focus state.
  ///   • Options: Record representing focus options in an imaging system.
  /// </summary>
  TImagingFocusSettings = record
    Options : TFocusOptions;
    Status  : TFocusStatus;
  end;
  
  /// <summary>
  ///   Record representing focus settings.
  ///   • AutoFocusMode: Mode indicating the autofocus behavior.
  ///   • DefaultSpeed: Integer value representing the default autofocus speed.
  /// </summary>  
  TImagingFocus = record
    AutoFocusMode : TImagingFocusMode;
    DefaultSpeed  : Integer;
  end;

  /// <summary>
  ///   Record representing wide dynamic range settings.
  ///   • Mode: String representing the mode of wide dynamic range.
  /// </summary>  
  TWideDynamicRange = record
    Mode   : String;
    YrGain : Double;
    YbGain : Double;
  end;

  /// <summary>
  ///   Record representing white balance settings.
  ///   • Mode: String representing the mode of white balance.
  /// </summary>  
  TWhiteBalance = record
    Mode   : String;
    CrGain : Double;
    CbGain : Double;
  end;

  /// <summary>
  ///   Record representing defogging settings.
  ///   • Mode: String representing the mode of defogging.
  ///   • Level: Double representing the defogging level.
  /// </summary>  
  TDefogging = record
    Mode  : String;
    Level : Double;
  end;

  /// <summary>
  ///   Record representing noise reduction settings.
  ///   • Level: Double representing the level of noise reduction.
  /// </summary>  
  TNoiseReduction = record
    Level : Double;
  end;
  
  /// <summary>
  ///   Record representing imaging extensions.
  ///   • Defogging: Record representing defogging settings.
  ///   • NoiseReduction: Record representing noise reduction settings.
  /// </summary>  
  TImagingExtension = record
    Defogging      : TDefogging;
    NoiseReduction : TNoiseReduction;
  end;

  /// <summary>
  ///   Record representing imaging settings.
  ///   • BacklightCompensation: Boolean indicating whether backlight compensation is enabled.
  ///   • Brightness: Integer representing the brightness level.
  ///   • ColorSaturation: Integer representing the color saturation level.
  ///   • Contrast: Integer representing the contrast level.
  ///   • Exposure: Record representing exposure settings.
  ///   • Focus: Record representing focus settings.
  ///   • IrCutFilter: Enumeration representing the mode of the IR cut filter.
  ///   • Sharpness: Integer representing the sharpness level.
  ///   • WideDynamicRange: Record representing wide dynamic range settings.
  ///   • WhiteBalance: Record representing white balance settings.
  ///   • Extension: Record representing imaging extensions.
  /// </summary>  
  TImagingSettings = record
     BacklightCompensation : Boolean;
     Brightness            : Integer;
     ColorSaturation       : Integer;
     Contrast              : Integer; 
     Exposure              : TImagingExposure;
     Focus                 : TImagingFocus;
     IrCutFilter           : TIrCutFilterMode;     
     Sharpness             : Integer; 
     WideDynamicRange      : TWideDynamicRange;
     WhiteBalance          : TWhiteBalance;
     Extension             : TImagingExtension;     
  end;

  TFocusOptions20     = TImagingFocus;
  TImagingExposure20  = TImagingExposure;
  TWideDynamicRange20 = TWideDynamicRange;
  TWhiteBalance20     = TWhiteBalance;
  

  TImagingOptions20 = record
     BacklightCompensation : Boolean;
     Brightness            : Integer;
     ColorSaturation       : Integer;
     Contrast              : Integer; 
     Exposure              : TImagingExposure20;
     Focus                 : TFocusOptions20;
     IrCutFilter           : TIrCutFilterMode;     
     Sharpness             : Integer; 
     WideDynamicRange      : TWideDynamicRange20;
     WhiteBalance          : TWhiteBalance20;     
  end;


  TMinMaxValue = Record
     Min : Real;
     Max : Real;
  end;  
  
  TDefoggingOptions = record
    Mode : TArray<String>; 
    Level: Boolean; 
  end;

  TExtensionOptions = record
    DefoggingOptions     : TDefoggingOptions;
    NoiseReductionOptions: Boolean;
  end;

  TWideDynamicRangeOptions = record
    Mode : TArray<String>;
    Level: TMinMaxValue;
  end;

  TFocusImagingOptions = record
    AutoFocusModes: TArray<String>;
    DefaultSpeed  : TMinMaxValue;
    NearLimit     : TMinMaxValue;
    FarLimit      : TMinMaxValue;
  end;
  
  TExposureImagingOptions = record
    Mode           : TArray<String>;
    MinExposureTime: TMinMaxValue;
    MaxExposureTime: TMinMaxValue;
    MaxIris        : TMinMaxValue; 
    ExposureTime   : TMinMaxValue;
    Iris           : TMinMaxValue;
  end;


  /// <summary>
  ///   Record representing white balance options.
  ///   • Mode: String representing the mode of white balance.
  /// </summary>  
  TWhiteBalanceOptions = record
    Mode   : TArray<string>;
    CrGain : Double;
    CbGain : Double;
  end;  

  TImagingOptions = record
    BacklightCompensation: TArray<String>;
    Brightness           : TMinMaxValue;
    ColorSaturation      : TMinMaxValue;
    Contrast             : TMinMaxValue;
    Exposure             : TExposureImagingOptions;
    Focus                : TFocusImagingOptions;
    IrCutFilterModes     : TArray<String>;
    Sharpness            : TMinMaxValue;
    WideDynamicRange     : TWideDynamicRangeOptions;
    WhiteBalance         : TWhiteBalanceOptions;
    Extension            : TExtensionOptions;
  end;


  

implementation


end.
