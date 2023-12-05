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
                               
  
  TContinuousFocusOptions = Record
    Speed    : TMinMaxValue;
    Supported: Boolean;
  End;

  TAbsoluteFocusOptions = Record
    Speed    : TMinMaxValue;
    Position : TMinMaxValue;    
    Supported: Boolean;
  End;

  TRelativeFocusOptions = Record
    Speed    : TMinMaxValue;
    Distance : TMinMaxValue;
    Supported: Boolean;
  End;  

  TFocusOptions = record
    Relative   : TRelativeFocusOptions;
    Continuous : TContinuousFocusOptions;
    Absolute   : TAbsoluteFocusOptions;
  end;

  TFocusStatus = Record
    AM_TODO : String;
  End;

  TImagingFocusSettings = record
    Options : TFocusOptions;
    Status  : TFocusStatus;
  end;
  
  TImagingFocus = record
    AutoFocusMode : TImagingFocusMode;
    DefaultSpeed  : Integer;
  end;

  TWideDynamicRange = record
    Mode : String;
  end;

  TWhiteBalance = record
    Mode : String;
  end;

  TDefogging = record
    Mode  : String;
    Level : Double;
  end;

  TNoiseReduction = record
    Level : Double;
  end;
  
  TImagingExtension = record
    Defogging      : TDefogging;
    NoiseReduction : TNoiseReduction;
  end;

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
  

implementation


end.
