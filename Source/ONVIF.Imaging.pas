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

unit ONVIF.Imaging;
{specs 
https://www.onvif.org/specs/srv/img/ONVIF-Imaging-Service-Spec.pdf
}  

interface

uses
  ONVIF.Types, ONVIF.XML.Utils, XmlDoc, XmlIntf, XMLDom, ONVIF.Constant.Error,
  System.Math, ONVIF.Structure.Imaging, System.Classes, ONVIF.Intf, System.SysUtils,
  Soap.XSBuiltIns;
  
  {TODO LIST

         -- Saturation, contrast ecc
         -- IRCut 
         -- IRIS 
         -- Focus
              -- Relative
         -- GetServiceCapabilities or in Device ? 
            --- Flip: Indicates whether or not E-Flip is supported.
            --- Reverse: Indicates whether or not reversing of PT control direction is supported.
            --- GetCompatibleConfigurations: Indicates the support for GetCompatibleConfigurations command.
            --- MoveStatus Indicates that the PTZStatus includes MoveStatus information.
            --- StatusPosition Indicates that the PTZStatus includes Position information.         
         others -->             
  }

type
  TONVIFImagingManager        = class;

  TOnGetFocusSettings         = procedure (const aFocusMode:TImagingFocusMode;aDefaultSpeed:Integer) of object;    
  TOnGetIrCutSettings         = procedure (const aIrCutMode:TIrCutFilterMode) of object;      
  TOnGetFocusAbsoluteLimit    = procedure (const Min,Max,Position:Double;const aStatus,Error:String) of object;      

  
  /// <summary>
  ///   Class representing supported imaging information.
  ///   • FocusSupported: Property indicating whether focus is supported.
  /// </summary>
  TSupportedImagingInfo = class(TPersistent)
  private
    FOwnerImagingManager : TONVIFImagingManager;

    /// <summary>
    ///   Function to get the supported focus information.
    /// </summary>    
    function GetFocusSupported: Boolean;
  public
  
    /// <summary>
    ///   Constructor for creating an instance of TSupportedImagingInfo.
    /// </summary>  
    constructor Create(aOwnerImagingManager : TONVIFImagingManager); 

    /// <summary>
    ///   Property indicating whether focus is supported.
    /// </summary>     
    property FocusSupported : Boolean read GetFocusSupported;
  end;
  

  /// <summary>
  ///
  ///   https://www.onvif.org/specs/srv/img/ONVIF-Imaging-Service-Spec-v1606.pdf
  ///
  ///   The imaging service provides configuration and control data for imaging specific properties. 
  ///   WSDL is part of the framework and provided in the Imaging WSDL file. 
  ///   The service includes the following operations: 
  ///    Get and set imaging configurations (exposure time, gain and white balance, for 
  ///   example). 
  ///    Get imaging configuration options (valid ranges for imaging parameters). 
  ///    Move focus lens. 
  ///    Stop ongoing focus movement. 
  ///    Get current position and move status for focus. 
  /// </summary>  
  TONVIFImagingManager = Class
  private
    FONVIFManager              : IONVIFManager;
    FSupportImagingPresets     : Boolean;
    FSupportImageStabilization : Boolean;
    FToken                     : String;
    FImagingSettings           : TImagingSettings;
    FImagingOptions            : TImagingOptions;
    FSupportedInfo             : TSupportedImagingInfo;
    FFocusSettings             : TImagingFocusSettings;
    FOnGetFocusSettings        : TOnGetFocusSettings;
    FOnGetFocusAbsoluteLimit   : TOnGetFocusAbsoluteLimit;
    FOnGetIrCutSettings        : TOnGetIrCutSettings;
    
    
    /// <summary>    
    //  The capabilities reflect optional functions and functionality of a service. The information is 
    /// static and does not change during device operation. The following capabilites are available: 
    //    ImageStabilization: Indicates whether or not Image Stabilization feature is supported. 
    //    ImagingPresets    : Indicates whether or not Imaging Presets feature is supported  
    /// </summary>           
    function GetCapabilities: Boolean;
    
    /// <summary>
    ///   Function to check the validity of a token for a given method name request.
    /// </summary>    
    function isValidToken(const aMathodNameRequest:String): Boolean;
    
    /// <summary>
    ///   Procedure to set a token value.
    /// </summary>    
    procedure SetToken(const aValue: String);

    /// <summary>
    ///   Procedure to reset imaging settings.
    /// </summary>    
    procedure ResetImagingSettings;

    /// <summary>
    ///  This operation requests the imaging setting for a video source on the device. A device 
    ///  implementing the imaging service shall support this command. 
    ///  If the Video Source supports any of the imaging settings as defined by the ImagingSettings 
    ///  type in the [ONVIF Schema], then it should be possible to retrieve the imaging settings from 
     /// the device through the GetImagingSettings command.  
    /// </summary>      
    function GetImagingSettings: Boolean;
    
    /// <summary>
    ///  The GetMoveOptions command retrieves the focus lens move options to be used in the move 
    ///  command as defined in Section 5.1.4. A device that supports the imaging service shall 
    ///  support the GetMoveOptions command. The response to the command shall include all 
    ///  supported Move Operations. If focus move is not supported at all, the reponse shall be empty
    /// </summary>
    function GetMoveOptions: Boolean;
    
    /// <summary>
    ///  Via this command the current status of the Move operation can be requested. 
    ///  Supported for this command is available if the support for the Move operation is signalled via GetMoveOptions.    
    /// </summary>
    function GetStatus(aFireEvent:Boolean): Boolean;    

    /// <summary>    
    /// This operation gets the valid ranges for the imaging parameters that have device specific 
    /// ranges. A device implementing the imaging service shall support this command. The 
    /// command shall return all supported parameters and their ranges such that these can be 
    /// applied to the SetImagingSettings command.
    /// For read-only parameters which cannot be modified via the SetImagingSettings command only 
    /// a single option or identical Min and Max values shall be provided.
    /// </summary>
    /// <returns>
    ///   True if the operation successfully , False otherwise.</returns>  
    /// </returns>    
    function GetOptions: Boolean;
    procedure ResetOptions;
  public
    /// <summary>
    ///   Constructor for initializing a new instance of the Imaging Manager.
    /// </summary>
    /// <param name="aONVIFManager">Reference to the parent ONVIF manager instance.</param>  
    constructor Create(aONVIFManager : IONVIFManager);
    
    
    /// <summary>
    ///   Destructor for cleaning up resources.
    /// </summary>     
    destructor Destroy;override; 

    /// <summary>
    ///	The Move command moves the focus lens in an absolute manner from its current position. 
    ///	The speed argument is optional for absolute control. 
    ///	If no speed argument is used, the default speed is used. 
    ///	Focus adjustments through this operation will turn off the autofocus. 
    ///	A device with support for remote focus control should support absolute control through the Move operation.
    ///	The supported MoveOpions are signalled via the GetMoveOptions command. 
    //  At least one focus control capability is required for this operation to be functional.
    ///	The move operation contains the following commands:
    ///	
    ///	Absolute – Requires position parameter and optionally takes a speed argument. 
    ///            A unitless type is used by default for focus positioning and speed. 
    ///            Optionally, if supported, the position may be requested in m-1 units.
    /// </summary> 
    function FocusMoveAbsolute(const aNewPosition:Double): Boolean;
    
    /// <summary>
    ///	The Move command moves the focus lens in an continuous manner from its current position. 
    ///	The speed argument is  required for continuous. 
    ///	Focus adjustments through this operation will turn off the autofocus. 
    ///	A device with support for remote focus control should support absolute control through the Move operation.
    ///	The supported MoveOpions are signalled via the GetMoveOptions command. 
    //  At least one focus control capability is required for this operation to be functional.  
    /// </summary> 
    function FocusMoveContinuous(const isNear:Boolean): Boolean;
    
    /// <summary>
    /// The Stop command stops all ongoing focus movements of the lense. 
    /// A device with support for remote focus control as signalled via the GetMoveOptions supports this command.
    /// The operation will not affect ongoing autofocus operation
    /// </summary> 
    function FocusMoveStop : Boolean;

    /// <summary>
    ///   Property providing access to imaging focus settings.
    /// </summary>    
    property FocusSettings             : TImagingFocusSettings         read FFocusSettings;
    
    /// <summary>
    ///   Property providing access to imaging settings.
    /// </summary>    
    property ImagingSettings           : TImagingSettings              read FImagingSettings;

    
    /// <summary>
    ///   Property providing access to imaging Options.
    /// </summary>    
    property ImagingOptions            : TImagingOptions               read FImagingOptions;    
    

    /// <summary>
    ///   Property providing access to supported imaging information.
    /// </summary>    
    property SupportedInfo             : TSupportedImagingInfo         read FSupportedInfo; 

    /// <summary>
    ///   Property indicating whether image stabilization is supported.
    /// </summary>    
    property SupportImageStabilization : Boolean                       read FSupportImageStabilization;

    /// <summary>
    ///   Property indicating whether imaging presets are supported.
    /// </summary>    
    property SupportImagingPresets     : Boolean                       read FSupportImagingPresets;    
    
    /// <summary>
    ///   Gets or sets the token of the VideoSource.
    /// </summary>    
    property Token                     : String                        read FToken                          write SetToken; 
    property OnGetFocusSettings        : TOnGetFocusSettings           read FOnGetFocusSettings             write FOnGetFocusSettings;
    property OnGetFocusAbsoluteLimit   : TOnGetFocusAbsoluteLimit      read FOnGetFocusAbsoluteLimit        write FOnGetFocusAbsoluteLimit;
    property OnGetIrCutSettings        : TOnGetIrCutSettings           read FOnGetIrCutSettings             write FOnGetIrCutSettings;    

  End;



implementation



{ TONVIFImagingManager }
constructor TONVIFImagingManager.Create(aONVIFManager: IONVIFManager);
begin
  FSupportedInfo := TSupportedImagingInfo.Create(self);
  FONVIFManager  := aONVIFManager;
  FToken         := String.Empty;
  ResetImagingSettings;  
end;

function TONVIFImagingManager.GetCapabilities: Boolean;
var LResponseStr : String;
    LSoapBodyNode: IXMLNode;
    LNodeTmp     : IXMLNode; 
begin
  Result := False;  
  if not isValidToken('TONVIFImagingManager.GetCapabilities') then Exit;

  Result := FONVIFManager.ExecuteRequest(atImaging,'TONVIFImagingManager.GetCapabilities',FONVIFManager.GetSOAPBuilder.PrepareImagingCapabilities,LResponseStr);

  if Result then 
  begin
    {$REGION 'Log'}
    {TSI:IGNORE ON}
      FONVIFManager.DoWriteLog('TONVIFImagingManager.GetCapabilities',LResponseStr,tpLivXMLResp,true);      
    {TSI:IGNORE OFF}
    {$ENDREGION}
    LSoapBodyNode  := FONVIFManager.GetBodyNode(LResponseStr);
    if not Assigned(LSoapBodyNode) then Exit;
  
    LNodeTmp := TONVIFXMLUtils.RecursiveFindNode(LSoapBodyNode,'ImageStabilization');
    if Assigned(LNodeTmp) then    
      FSupportImageStabilization := StrToBoolDef(LNodeTmp.Text,False);
    LNodeTmp := TONVIFXMLUtils.RecursiveFindNode(LSoapBodyNode,'AdaptablePreset ');
    if Assigned(LNodeTmp) then    
      FSupportImagingPresets := StrToBoolDef(LNodeTmp.Text,False);
  end;
end;

procedure TONVIFImagingManager.ResetImagingSettings;
begin
  {Focus}
  FFocusSettings                                 := Default(TImagingFocusSettings);  
  FFocusSettings.Options.Continuous.Speed.Min    := -1;
  FFocusSettings.Options.Continuous.Speed.Max    := -1;
  FFocusSettings.Options.Relative.Speed.Min      := -1;
  FFocusSettings.Options.Relative.Speed.Max      := -1;
  FFocusSettings.Options.Relative.Distance.Min   := -1;
  FFocusSettings.Options.Relative.Distance.Max   := -1;  
  FFocusSettings.Options.Absolute.Speed.Min      := -1;
  FFocusSettings.Options.Absolute.Speed.Max      := -1;
  FFocusSettings.Options.Absolute.Position.Min   := -1; 
  FFocusSettings.Options.Absolute.Position.Max   := -1;   
  FFocusSettings.Status.Position                 := -1;
  
  FImagingSettings                               := Default(TImagingSettings);
  FImagingSettings.Brightness                    := -1;
  FImagingSettings.ColorSaturation               := -1;
  FImagingSettings.Contrast                      := -1; 
  FImagingSettings.Exposure.MinExposureTime      := -1;
  FImagingSettings.Exposure.MaxExposureTime      := -1;
  FImagingSettings.Exposure.MinIris              := -1;  
  FImagingSettings.Exposure.MaxIris              := -1;  
  FImagingSettings.Exposure.MinGain              := -1;
  FImagingSettings.Exposure.MaxGain              := -1;
  FImagingSettings.Exposure.Iris                 := -1;  
  FImagingSettings.Exposure.Gain                 := -1;  
  FImagingSettings.Focus.DefaultSpeed            := -1;
  FImagingSettings.Sharpness                     := -1;
  FImagingSettings.WideDynamicRange.YrGain       := -1;
  FImagingSettings.WideDynamicRange.YbGain       := -1;  
  FImagingSettings.WhiteBalance.CrGain           := -1;
  FImagingSettings.WhiteBalance.CbGain           := -1;
  FImagingSettings.Extension.Defogging.Level     := -1;
  FImagingSettings.Extension.NoiseReduction.Level:= -1;   
  ResetOptions;
  
end;

Procedure TONVIFImagingManager.ResetOptions;
begin
  // Reset Backlight Compensation options
  SetLength(FImagingOptions.BacklightCompensation, 0);

  // Reset Brightness options
  FImagingOptions.Brightness.Min      := -1;
  FImagingOptions.Brightness.Max      := -1;

  // Reset Color Saturation options
  FImagingOptions.ColorSaturation.Min := -1;
  FImagingOptions.ColorSaturation.Max := -1;

  // Reset Contrast options
  FImagingOptions.Contrast.Min        := -1;
  FImagingOptions.Contrast.Max        := -1;

  // Reset Exposure options
  SetLength(FImagingOptions.Exposure.Mode, 0);
  FImagingOptions.Exposure.MinExposureTime.Min := -1;
  FImagingOptions.Exposure.MinExposureTime.Max := -1;
  FImagingOptions.Exposure.MaxExposureTime.Min := -1;
  FImagingOptions.Exposure.MaxExposureTime.Max := -1;
  FImagingOptions.Exposure.MaxIris.Min         := -1;
  FImagingOptions.Exposure.MaxIris.Max         := -1;
  FImagingOptions.Exposure.ExposureTime.Min    := -1;
  FImagingOptions.Exposure.ExposureTime.Max    := -1;
  FImagingOptions.Exposure.Iris.Min            := -1;
  FImagingOptions.Exposure.Iris.Max            := -1;

  // Reset Focus options
  SetLength(FImagingOptions.Focus.AutoFocusModes, 0);
  FImagingOptions.Focus.DefaultSpeed.Min := -1;
  FImagingOptions.Focus.DefaultSpeed.Max := -1;
  FImagingOptions.Focus.NearLimit.Min    := -1;
  FImagingOptions.Focus.NearLimit.Max    := -1;
  FImagingOptions.Focus.FarLimit.Min     := -1;
  FImagingOptions.Focus.FarLimit.Max     := -1;

  // Reset IR Cut Filter Modes
  SetLength(FImagingOptions.IrCutFilterModes, 0);

  // Reset Sharpness options
  FImagingOptions.Sharpness.Min := -1;
  FImagingOptions.Sharpness.Max := -1;

  // Reset Wide Dynamic Range options
  SetLength(FImagingOptions.WideDynamicRange.Mode, 0);
  FImagingOptions.WideDynamicRange.Level.Min := -1;
  FImagingOptions.WideDynamicRange.Level.Max := -1;

  // Reset White Balance options
  SetLength(FImagingOptions.WhiteBalance.Mode, 0);
  FImagingOptions.WhiteBalance.CrGain := -1;
  FImagingOptions.WhiteBalance.CbGain := -1;

  // Reset Extension options
  SetLength(FImagingOptions.Extension.DefoggingOptions.Mode, 0);
  FImagingOptions.Extension.DefoggingOptions.Level := False;
  FImagingOptions.Extension.NoiseReductionOptions  := False;
end;

destructor TONVIFImagingManager.Destroy;
begin
  ResetImagingSettings;
  FreeAndNil(FSupportedInfo);
  inherited;
end;

function TONVIFImagingManager.FocusMoveAbsolute(const aNewPosition: Double): Boolean;
var LCommand   : String;
    LResultStr : String; 
begin
  Result := False;
  if not isValidToken('TONVIFPTZManager.FocusMoveAbsolute') then Exit;

  if not SupportedInfo.FocusSupported then
  begin
    FONVIFManager.SetLastStatusCode('TONVIFPTZManager.FocusMoveAbsolute',ONVIF_ERROR_IMG_FOCUS_NOT_SUPPORTED);
    Exit;
  end;  

  if not FFocusSettings.Options.Absolute.Supported then
  begin
    FONVIFManager.SetLastStatusCode('TONVIFPTZManager.FocusMoveAbsolute',ONVIF_ERROR_IMG_FOCUS_ABSOLUTE_NOT_SUPPORTED);
    Exit;
  end;    

  LCommand := FloatToStr(aNewPosition).Replace(',','.');   

  if not InRange(aNewPosition,FFocusSettings.Options.Absolute.Speed.Min,FFocusSettings.Options.Absolute.Speed.Max) then
  begin
    FONVIFManager.SetLastStatusCode('TONVIFPTZManager.FocusMoveAbsolute',ONVIF_ERROR_IMG_FOCUS_ABSOLUTE_OUT_OF_RANGE);  
    Exit;
  end;
           
  Result := FONVIFManager.ExecuteRequest(atPtz,'TONVIFImagingManager.FocusMoveAbsolute',FONVIFManager.GetSOAPBuilder.PrepareImagingMoveFocus(FToken,LCommand,String.Empty,String.Empty), LResultStr);      
end;

function TONVIFImagingManager.FocusMoveContinuous(const isNear:Boolean): Boolean;
var LCommand   : String;
    LResultStr : String; 
begin
  Result := False;
  if not isValidToken('TONVIFPTZManager.FocusMoveContinuous') then Exit;

  if not SupportedInfo.FocusSupported then
  begin
    FONVIFManager.SetLastStatusCode('TONVIFPTZManager.FocusMoveContinuous',ONVIF_ERROR_IMG_FOCUS_NOT_SUPPORTED);
    Exit;
  end;  

  if not FFocusSettings.Options.Continuous.Supported then
  begin
    FONVIFManager.SetLastStatusCode('TONVIFPTZManager.FocusMoveContinuous',ONVIF_ERROR_IMG_FOCUS_CONTINUOUS_NOT_SUPPORTED);
    Exit;
  end;    

  // Cosa devo passare ??
  if isNear then
     LCommand := FloatToStr( FONVIFManager.Speed * -1).Replace(',','.')    
  else
     LCommand := FloatToStr( FONVIFManager.Speed ).Replace(',','.');
        
  Result := FONVIFManager.ExecuteRequest(atPtz,'TONVIFImagingManager.FocusMoveContinuous',FONVIFManager.GetSOAPBuilder.PrepareImagingMoveFocus(FToken,String.Empty,String.Empty,LCommand), LResultStr);      
end;

function TONVIFImagingManager.FocusMoveStop: Boolean;
var LResultStr : String;
begin
  Result := False;
  if not isValidToken('TONVIFPTZManager.FocusMoveStop') then Exit;

  if not SupportedInfo.FocusSupported then
  begin
    FONVIFManager.SetLastStatusCode('TONVIFPTZManager.FocusMoveStop',ONVIF_ERROR_IMG_FOCUS_NOT_SUPPORTED);
    Exit;
  end;  

  if not FFocusSettings.Options.Continuous.Supported then
  begin
    FONVIFManager.SetLastStatusCode('TONVIFPTZManager.FocusMoveStop',ONVIF_ERROR_IMG_FOCUS_CONTINUOUS_NOT_SUPPORTED);
    Exit;
  end;      

  Result := FONVIFManager.ExecuteRequest(atPtz,'TONVIFPTZManager.FocusMoveStop',FONVIFManager.GetSOAPBuilder.PrepareImagingStopMoveFocus(FToken), LResultStr);        
  if Result then  
    GetStatus(False);  
end;

Function TONVIFImagingManager.GetMoveOptions:Boolean;
var LResponseStr       : String;
    LSoapBodyNode      : IXMLNode;
    LMoveOptionNode    : IXMLNode;
    LNodeTmp         : IXMLNode;
begin
  Result := FONVIFManager.ExecuteRequest(atImaging,'TONVIFImagingManager.GetImagingSettings',FONVIFManager.GetSOAPBuilder.PrepareImagingMoveOptions(FToken),LResponseStr);  
  if Result then 
  begin
    {$REGION 'Log'}
    {TSI:IGNORE ON}
        FONVIFManager.DoWriteLog('TONVIFImagingManager.GetImagingSettings',LResponseStr,tpLivXMLResp,true);      
    {TSI:IGNORE OFF}
    {$ENDREGION}
    Result  := False;
    LSoapBodyNode  := FONVIFManager.GetBodyNode(LResponseStr);
    if not Assigned(LSoapBodyNode) then Exit;
 
    LMoveOptionNode := TONVIFXMLUtils.RecursiveFindNode(LSoapBodyNode,'Relative');
    if Assigned(LMoveOptionNode) then
    begin

      FFocusSettings.Options.Relative.Supported := True;
      LNodeTmp                                  := TONVIFXMLUtils.RecursiveFindNode(LMoveOptionNode,'Speed');
      if Assigned(LNodeTmp) then
      begin
        FFocusSettings.Options.Relative.Speed.Min := StrToFloatLocale(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp,'Min'),-1);  
        FFocusSettings.Options.Relative.Speed.Max := StrToFloatLocale(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp,'Max'),-1);
      end;
      
      LNodeTmp := TONVIFXMLUtils.RecursiveFindNode(LMoveOptionNode,'Distance');
      if Assigned(LNodeTmp) then
      begin
        FFocusSettings.Options.Relative.Distance.Min := StrToFloatLocale(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp,'Min'),-1);  
        FFocusSettings.Options.Relative.Distance.Max := StrToFloatLocale(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp,'Max'),-1);
      end;      
    end;

    LMoveOptionNode := TONVIFXMLUtils.RecursiveFindNode(LSoapBodyNode,'Absolute');
    if Assigned(LMoveOptionNode) then
    begin
   
      FFocusSettings.Options.Absolute.Supported := True;
      LNodeTmp                                  := TONVIFXMLUtils.RecursiveFindNode(LMoveOptionNode,'Speed');
      if Assigned(LNodeTmp) then
      begin
        FFocusSettings.Options.Absolute.Speed.Min := StrToFloatLocale(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp,'Min'),-1);  
        FFocusSettings.Options.Absolute.Speed.Max := StrToFloatLocale(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp,'Max'),-1);
      end;

      LNodeTmp := TONVIFXMLUtils.RecursiveFindNode(LMoveOptionNode,'Position');
      if Assigned(LNodeTmp) then
      begin
        FFocusSettings.Options.Absolute.Position.Min := StrToFloatLocale(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp,'Min'),-1);  
        FFocusSettings.Options.Absolute.Position.Max := StrToFloatLocale(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp,'Max'),-1);
      end;      
    end;

    LMoveOptionNode := TONVIFXMLUtils.RecursiveFindNode(LSoapBodyNode,'Continuous');
    if Assigned(LMoveOptionNode) then
    begin
      FFocusSettings.Options.Continuous.Supported := True;
      LNodeTmp                                  := TONVIFXMLUtils.RecursiveFindNode(LMoveOptionNode,'Speed');
      if Assigned(LNodeTmp) then
      begin
        FFocusSettings.Options.Continuous.Speed.Min := StrToFloatLocale(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp,'Min'),-1);  
        FFocusSettings.Options.Continuous.Speed.Max := StrToFloatLocale(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp,'Max'),-1);
      end;
    end;
    Result := True;  
  end;
end;

function TONVIFImagingManager.GetOptions: Boolean;
var LResponseStr       : String;
    LSoapBodyNode      : IXMLNode;    
    LNodeTmp1          : IXMLNode;
    LNodeTmp2          : IXMLNode;
    LImgSettingsNode   : IXMLNode;

    Procedure GetMinMaxValue(aParentNode:IXMLNode;aNodeName:String;var aValueResult : TMinMaxValue);
    var LNodeMinMax : IXMLNode;
    begin
      LNodeMinMax := TONVIFXMLUtils.RecursiveFindNode(aParentNode, aNodeName);
      if Assigned(LNodeMinMax) then
      begin
        aValueResult.Min  := StrToFloatDef(TONVIFXMLUtils.GetChildNodeValue(LNodeMinMax, 'Min'), -1);
        aValueResult.Max  := StrToFloatDef(TONVIFXMLUtils.GetChildNodeValue(LNodeMinMax, 'Max'), -1);
      end;    
    end;
begin
  Result := False;  

  
 // ResetPTZStatus;
  if not isValidToken('TONVIFImagingManager.GetOptions') then Exit;
  Result := FONVIFManager.ExecuteRequest(atImaging,'TONVIFPTZManager.GetOptions',FONVIFManager.GetSOAPBuilder.PrepareGetImagingOptions(FToken),LResponseStr);
  if Result then
  begin
    Result := False;  
    {$REGION 'Log'}
    {TSI:IGNORE ON}
        FONVIFManager.DoWriteLog('TONVIFImagingManager.GetOptions',LResponseStr,tpLivXMLResp,true);      
    {TSI:IGNORE OFF}
    {$ENDREGION}
    LSoapBodyNode := FONVIFManager.GetBodyNode(LResponseStr);
    if not Assigned(LSoapBodyNode) then Exit;
  
    LImgSettingsNode := TONVIFXMLUtils.RecursiveFindNode(LSoapBodyNode, 'ImagingOptions');

    if not Assigned(LImgSettingsNode) then Exit;
   
    FImagingOptions.BacklightCompensation := TONVIFXMLUtils.GetChildNodeValues(LImgSettingsNode, 'BacklightCompensation','Mode');
    
    GetMinMaxValue(LImgSettingsNode,'Brightness',FImagingOptions.Brightness); 
    GetMinMaxValue(LImgSettingsNode,'ColorSaturation',FImagingOptions.ColorSaturation);    
    GetMinMaxValue(LImgSettingsNode,'Contrast',FImagingOptions.Contrast);    


    FImagingOptions.Exposure.Mode  := TONVIFXMLUtils.GetChildNodeValues(LImgSettingsNode,'Exposure', 'Mode');
    LNodeTmp1                      := TONVIFXMLUtils.RecursiveFindNode(LImgSettingsNode, 'Exposure');
    if Assigned(LNodeTmp1) then
    begin
      GetMinMaxValue(LNodeTmp1,'MinExposureTime',FImagingOptions.Exposure.MinExposureTime);    
      GetMinMaxValue(LNodeTmp1,'MaxExposureTime',FImagingOptions.Exposure.MaxExposureTime);    
      GetMinMaxValue(LNodeTmp1,'MaxIris',FImagingOptions.Exposure.MaxIris);    
      GetMinMaxValue(LNodeTmp1,'ExposureTime',FImagingOptions.Exposure.ExposureTime);    
      GetMinMaxValue(LNodeTmp1,'Iris',FImagingOptions.Exposure.Iris);    
    end;

    FImagingOptions.Focus.AutoFocusModes := TONVIFXMLUtils.GetChildNodeValues(LImgSettingsNode,'Focus', 'AutoFocusModes');    
    LNodeTmp1                            := TONVIFXMLUtils.RecursiveFindNode(LImgSettingsNode, 'Focus');
    if Assigned(LNodeTmp1) then
    begin    
      GetMinMaxValue(LNodeTmp1,'Iris',FImagingOptions.Focus.DefaultSpeed);    
      GetMinMaxValue(LNodeTmp1,'FarLimit',FImagingOptions.Focus.FarLimit);    
      GetMinMaxValue(LNodeTmp1,'NearLimit',FImagingOptions.Focus.NearLimit);    
    end;

    FImagingOptions.IrCutFilterModes := TONVIFXMLUtils.GetChildNodeValues(LSoapBodyNode,'ImagingOptions', 'IrCutFilterModes');
    GetMinMaxValue(LImgSettingsNode,'Sharpness',FImagingOptions.Sharpness);        

    FImagingOptions.WideDynamicRange.Mode  := TONVIFXMLUtils.GetChildNodeValues(LImgSettingsNode,'WideDynamicRange', 'Mode');    
    LNodeTmp1                              := TONVIFXMLUtils.RecursiveFindNode(LImgSettingsNode, 'WideDynamicRange');
    if Assigned(LNodeTmp1) then
      GetMinMaxValue(LNodeTmp1,'Level',FImagingOptions.WideDynamicRange.Level); 
      
    FImagingOptions.WhiteBalance.Mode      := TONVIFXMLUtils.GetChildNodeValues(LImgSettingsNode,'WhiteBalance', 'Mode');
    LNodeTmp1                              := TONVIFXMLUtils.RecursiveFindNode(LImgSettingsNode, 'WhiteBalance');
    if Assigned(LNodeTmp1) then
    begin
      FImagingOptions.WhiteBalance.CrGain := StrToIntDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1, 'CrGain'), -1);
      FImagingOptions.WhiteBalance.CbGain := StrToIntDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1, 'CbGain'), -1);
    end;

    LNodeTmp1 := TONVIFXMLUtils.RecursiveFindNode(LImgSettingsNode, 'Extension');
    if Assigned(LNodeTmp1) then
    begin
      FImagingOptions.Extension.DefoggingOptions.Mode  := TONVIFXMLUtils.GetChildNodeValues(LNodeTmp1,'DefoggingOptions','Mode');
      
      LNodeTmp2 := TONVIFXMLUtils.RecursiveFindNode(LNodeTmp1, 'DefoggingOptions');
      if Assigned(LNodeTmp2) then
        FImagingOptions.Extension.DefoggingOptions.Level := StrToBoolDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp2, 'Level'), False);
      
      FImagingOptions.Extension.NoiseReductionOptions  := StrToBoolDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1, 'NoiseReductionOptions'), False);
    end;

    Result := True;
  
  end;
end;

function TONVIFImagingManager.GetStatus(aFireEvent:Boolean): Boolean;
var LResponseStr       : String;
    LSoapBodyNode      : IXMLNode;
    LGestStatus        : IXMLNode;
    I                  : Integer;  
begin
  Result := FONVIFManager.ExecuteRequest(atImaging,'TONVIFImagingManager.GetStatus',FONVIFManager.GetSOAPBuilder.PrepareImagingGetStatus(FToken),LResponseStr);
  if Result then
  begin
    Result := False;  
    {$REGION 'Log'}
    {TSI:IGNORE ON}
        FONVIFManager.DoWriteLog('TONVIFImagingManager.GetStatus',LResponseStr,tpLivXMLResp,true);      
    {TSI:IGNORE OFF}
    {$ENDREGION}
    LSoapBodyNode  := FONVIFManager.GetBodyNode(LResponseStr);
    if not Assigned(LSoapBodyNode) then Exit;    

    LGestStatus     := TONVIFXMLUtils.RecursiveFindNode(LSoapBodyNode,'Position');
    if Assigned(LGestStatus) then
      LGestStatus := LGestStatus.ParentNode;
    for I := 0 to LGestStatus.ChildNodes.Count -1 do
    begin
      if SameText(LGestStatus.ChildNodes[I].DOMNode.localName,'Position') then
        FFocusSettings.Status.Position := StrToFloatLocale(LGestStatus.ChildNodes[I].Text,-1) 
      else if SameText(LGestStatus.ChildNodes[I].DOMNode.localName,'MoveStatus') then
        FFocusSettings.Status.MoveStatus := LGestStatus.ChildNodes[I].Text
      else if SameText(LGestStatus.ChildNodes[I].DOMNode.localName,'Error') then
        FFocusSettings.Status.Error := LGestStatus.ChildNodes[I].Text 
      else
        {$REGION 'Log'}
        {TSI:IGNORE ON}
            FONVIFManager.DoWriteLog('TONVIFImagingManager.GetStatus',Format('Unsupported node name [%s]',[LGestStatus.ChildNodes[I].DOMNode.localName]),tpLivWarning);      
        {TSI:IGNORE OFF}
        {$ENDREGION}                                                

    end;      
    if aFireEvent then
    begin
      if Assigned(FOnGetFocusAbsoluteLimit) and FFocusSettings.Options.Absolute.Supported then
        FOnGetFocusAbsoluteLimit(FFocusSettings.Options.Absolute.Position.Min,FFocusSettings.Options.Absolute.Position.Max,FFocusSettings.Status.Position,FFocusSettings.Status.MoveStatus,FFocusSettings.Status.Error)      
    end;

  end
  else
    {$REGION 'Log'}
    {TSI:IGNORE ON}
        FONVIFManager.WriteLastErrorCodeLog('TONVIFManager.GetStatus');
    {TSI:IGNORE OFF}
    {$ENDREGION}       
end;

function TONVIFImagingManager.GetImagingSettings: Boolean;
var LResponseStr       : String;
    LSoapBodyNode      : IXMLNode;
    LImgSettingsNode   : IXMLNode;
    LNodeTmp1          : IXMLNode;
    LNodeTmp2          : IXMLNode;   
    LTmpStrValue       : String; 
    I                  : Integer;
begin
  Result := False;  

  ResetImagingSettings;
  if not isValidToken('TONVIFImagingManager.GetImagingSettings') then Exit;

  if not GetCapabilities then exit;

  GetOptions;
  
  if GetMoveOptions then
  begin
    if SupportedInfo.FocusSupported then
      GetStatus(True);
  end;

  Result := FONVIFManager.ExecuteRequest(atImaging,'TONVIFImagingManager.GetImagingSettings',FONVIFManager.GetSOAPBuilder.PrepareGetImagingSettings(FToken),LResponseStr);

  if Result then 
  begin
    {$REGION 'Log'}
    {TSI:IGNORE ON}
        FONVIFManager.DoWriteLog('TONVIFImagingManager.GetImagingSettings',LResponseStr,tpLivXMLResp,true);      
    {TSI:IGNORE OFF}
    {$ENDREGION}
    Result  := False;
    LSoapBodyNode  := FONVIFManager.GetBodyNode(LResponseStr);
    if not Assigned(LSoapBodyNode) then Exit;

    LImgSettingsNode := TONVIFXMLUtils.RecursiveFindNode(LSoapBodyNode,'ImagingSettings');

    if Assigned(LImgSettingsNode) then
    begin
      LNodeTmp1 := TONVIFXMLUtils.RecursiveFindNode(LImgSettingsNode,'BacklightCompensation');
      if Assigned(LNodeTmp1) then
        FImagingSettings.BacklightCompensation := StrToBoolDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'Mode'),False);
      
      FImagingSettings.Brightness      := StrToIntDef(TONVIFXMLUtils.GetChildNodeValue(LImgSettingsNode,'Brightness'),-1);
      FImagingSettings.ColorSaturation := StrToIntDef(TONVIFXMLUtils.GetChildNodeValue(LImgSettingsNode,'ColorSaturation'),-1);      
      FImagingSettings.Contrast        := StrToIntDef(TONVIFXMLUtils.GetChildNodeValue(LImgSettingsNode,'ColorSaturation'),-1);      
      LNodeTmp1                        := TONVIFXMLUtils.RecursiveFindNode(LImgSettingsNode,'Exposure');
      if Assigned(LNodeTmp1) then
      begin
        FImagingSettings.Exposure.Mode            := TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'Mode');
        FImagingSettings.Exposure.Priority        := TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'Priority');        
        FImagingSettings.Exposure.MinExposureTime := StrToFloatLocale(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'MinExposureTime'),-1);
        FImagingSettings.Exposure.MaxExposureTime := StrToFloatLocale(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'MaxExposureTime'),-1);      
        FImagingSettings.Exposure.MinIris         := StrToFloatLocale(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'MinIris'),-1);      
        FImagingSettings.Exposure.MaxIris         := StrToFloatLocale(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'MaxIris'),-1);      
        FImagingSettings.Exposure.MinGain         := StrToFloatLocale(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'MinGain'),-1);      
        FImagingSettings.Exposure.MaxGain         := StrToFloatLocale(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'MaxGain'),-1);      
        FImagingSettings.Exposure.Iris            := StrToFloatLocale(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'Iris'),-1);      
        FImagingSettings.Exposure.Gain            := StrToFloatLocale(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'Gain'),-1);      
      end;
      
      LNodeTmp1 := TONVIFXMLUtils.RecursiveFindNode(LImgSettingsNode,'Focus');
      if Assigned(LNodeTmp1) then
      begin
        LTmpStrValue := TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'AutoFocusMode').Trim;
        if SameText(LTmpStrValue,'MANUAL') then
          FImagingSettings.Focus.AutoFocusMode := ifmManual
        else if SameText(LTmpStrValue,'AUTO') then
          FImagingSettings.Focus.AutoFocusMode := ifmAutoMode
        else if SameText(LTmpStrValue,'DEFAULTSPEED') then
          FImagingSettings.Focus.AutoFocusMode := ifmDefaultSpeed
        else if SameText(LTmpStrValue,'NEARLIMIT') then
          FImagingSettings.Focus.AutoFocusMode := ifmNearLimit
        else if SameText(LTmpStrValue,'FARLIMIT') then
          FImagingSettings.Focus.AutoFocusMode := ifmFarLimit;
      
        FImagingSettings.Focus.DefaultSpeed := StrToIntDef(TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'DefaultSpeed'),-1);   

        if Assigned(FOnGetFocusSettings) then
          FOnGetFocusSettings(FImagingSettings.Focus.AutoFocusMode,FImagingSettings.Focus.DefaultSpeed);           
      end;

      LTmpStrValue := TONVIFXMLUtils.GetChildNodeValue(LImgSettingsNode,'IrCutFilter').Trim;
      if SameText(LTmpStrValue,'AUTO') then
        FImagingSettings.IrCutFilter := icfmAuto
      else if SameText(LTmpStrValue,'ON') then
        FImagingSettings.IrCutFilter := icfmON
      else if SameText(LTmpStrValue,'OFF') then
        FImagingSettings.IrCutFilter := icfmOFF;
        
      if Assigned(FOnGetIrCutSettings) then
        FOnGetIrCutSettings(FImagingSettings.IrCutFilter);
        
      FImagingSettings.Sharpness := StrToIntDef(TONVIFXMLUtils.GetChildNodeValue(LImgSettingsNode,'Sharpness'),-1); 
      
      LNodeTmp1 := TONVIFXMLUtils.RecursiveFindNode(LImgSettingsNode,'WideDynamicRange');
      if Assigned(LNodeTmp1) then
      begin
        FImagingSettings.WideDynamicRange.Mode   := TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'Mode');
        FImagingSettings.WideDynamicRange.YrGain := StrToFloatLocale( TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'YrGain'),-1);
        FImagingSettings.WideDynamicRange.YbGain := StrToFloatLocale( TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'YbGain'),-1);        

      end;

      LNodeTmp1 := TONVIFXMLUtils.RecursiveFindNode(LImgSettingsNode,'WhiteBalance');
      if Assigned(LNodeTmp1) then     
      begin 
        FImagingSettings.WhiteBalance.Mode   := TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'Mode');        
        FImagingSettings.WhiteBalance.CrGain := StrToFloatLocale( TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'CrGain'),-1);        
        FImagingSettings.WhiteBalance.CbGain := StrToFloatLocale( TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'CbGain'),-1);                
      end;

      LNodeTmp1 := TONVIFXMLUtils.RecursiveFindNode(LImgSettingsNode,'Extension',True);
      if Assigned(LNodeTmp1) then      
      begin

        for I := 0 to LNodeTmp1.ChildNodes.Count -1 do
        begin
          if not SameText(LNodeTmp1.ChildNodes[I].DOMNode.localName,'Defogging') and 
             not SameText(LNodeTmp1.ChildNodes[I].DOMNode.localName,'NoiseReduction') 
          then
            {$REGION 'Log'}
            {TSI:IGNORE ON}
                FONVIFManager.DoWriteLog('TONVIFImagingManager.GetImagingSettings',Format('Unsupported node name [%s]',[LNodeTmp1.ChildNodes[I].DOMNode.localName]),tpLivWarning);      
            {TSI:IGNORE OFF}
            {$ENDREGION}                                                
        end;         
         
        LNodeTmp2 := TONVIFXMLUtils.RecursiveFindNode(LNodeTmp1,'Defogging');
        if Assigned(LNodeTmp2) then
        begin
          FImagingSettings.Extension.Defogging.Mode  := TONVIFXMLUtils.GetChildNodeValue(LNodeTmp2,'Mode');        
          FImagingSettings.Extension.Defogging.Level := StrToFloatLocale( TONVIFXMLUtils.GetChildNodeValue(LNodeTmp2,'Level'),-1);                  
        end;

        LNodeTmp2 := TONVIFXMLUtils.RecursiveFindNode(LNodeTmp1,'NoiseReduction');
        if Assigned(LNodeTmp2) then
          FImagingSettings.Extension.NoiseReduction.Level := StrToFloatLocale( TONVIFXMLUtils.GetChildNodeValue(LNodeTmp2,'Level'),-1);                  
      end;
    end;

    Result := True;
  end;
end;

function TONVIFImagingManager.isValidToken(const aMathodNameRequest: String): Boolean;
begin
  Result := False;
  if FToken.Trim.IsEmpty then
  begin
    FONVIFManager.SetLastStatusCode(aMathodNameRequest,ONVIF_ERROR_IMG_IMMAGING_IS_EMPTY);    
    Exit;
  end;
  Result := True;  
end;

procedure TONVIFImagingManager.SetToken(const aValue: String);
begin
  if FToken <> aValue then
  begin
    FToken := aValue.Trim;
    {$REGION 'Log'}
    {TSI:IGNORE ON}
        FONVIFManager.DoWriteLog('TONVIFImagingManager.SetToken',Format('Imaging Token  [%s]',[Token]),tpLivInfo,true);      
    {TSI:IGNORE OFF}
    {$ENDREGION}  
    if aValue.Trim.IsEmpty then
      ResetImagingSettings
    else
      GetImagingSettings; 
  end;
end;


{ TSupportedImagingInfo }
constructor TSupportedImagingInfo.Create(aOwnerImagingManager: TONVIFImagingManager);
begin
  FOwnerImagingManager := aOwnerImagingManager;
end;

function TSupportedImagingInfo.GetFocusSupported: Boolean;
begin
  Result := FOwnerImagingManager.FFocusSettings.Options.Relative.Supported or 
            FOwnerImagingManager.FFocusSettings.Options.Absolute.Supported or
            FOwnerImagingManager.FFocusSettings.Options.Continuous.Supported;
             
end;

end.
