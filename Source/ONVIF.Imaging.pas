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

interface

uses
  ONVIF.Types, ONVIF.XML.Utils, XmlDoc, XmlIntf, XMLDom, ONVIF.Constant.Error,
  System.Math, ONVIF.Structure.Imaging, System.Classes, ONVIF.Intf, System.SysUtils,
  Soap.XSBuiltIns;
  
type
  TONVIFImagingManager = class;

  
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
    FSupportedInfo             : TSupportedImagingInfo;
    FFocusSettings             : TImagingFocusSettings;
    
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
    function GetStatus: Boolean;
    
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
    property FocusSettings             : TImagingFocusSettings read FFocusSettings;
    
    /// <summary>
    ///   Property providing access to imaging settings.
    /// </summary>    
    property ImagingSettings           : TImagingSettings      read FImagingSettings;

    /// <summary>
    ///   Property providing access to supported imaging information.
    /// </summary>    
    property SupportedInfo             : TSupportedImagingInfo read FSupportedInfo; 

    /// <summary>
    ///   Property indicating whether image stabilization is supported.
    /// </summary>    
    property SupportImageStabilization : Boolean               read FSupportImageStabilization;

    /// <summary>
    ///   Property indicating whether imaging presets are supported.
    /// </summary>    
    property SupportImagingPresets     : Boolean               read FSupportImagingPresets;    
    
    /// <summary>
    ///   Gets or sets the token of the VideoSource.
    /// </summary>    
    property Token                    : String                 read FToken                         write SetToken;        
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
    //TODO Parser
    FSupportImageStabilization := False;
    FSupportImagingPresets     := False;    
  end;
end;

procedure TONVIFImagingManager.ResetImagingSettings;
begin
  {Focus}
  FFocusSettings.Options.Continuous.Speed.Min    := -1;
  FFocusSettings.Options.Continuous.Speed.Max    := -1;
  FFocusSettings.Options.Continuous.Supported    := False; 
  FFocusSettings.Options.Relative.Speed.Min      := -1;
  FFocusSettings.Options.Relative.Speed.Max      := -1;
  FFocusSettings.Options.Relative.Supported      := False; 
  FFocusSettings.Options.Absolute.Speed.Min      := -1;
  FFocusSettings.Options.Absolute.Speed.Max      := -1;
  FFocusSettings.Options.Absolute.Supported      := False; 
  FFocusSettings.Status.Position                 := -1;
  FFocusSettings.Status.MoveStatus               := String.Empty; 
  FFocusSettings.Status.Error                    := String.Empty; 
  FSupportImageStabilization                     := False;
  FSupportImagingPresets                         := False;
  FImagingSettings.BacklightCompensation         := False;
  FImagingSettings.Brightness                    := -1;
  FImagingSettings.ColorSaturation               := -1;
  FImagingSettings.Contrast                      := -1; 
  FImagingSettings.Exposure.Mode                 := String.Empty;   
  FImagingSettings.Exposure.MinExposureTime      := -1;
  FImagingSettings.Exposure.MaxExposureTime      := -1;
  FImagingSettings.Exposure.MinIris              := -1;  
  FImagingSettings.Exposure.MaxIris              := -1;  
  FImagingSettings.Exposure.MinGain              := -1;
  FImagingSettings.Exposure.MaxGain              := -1;
  FImagingSettings.Exposure.Iris                 := -1;  
  FImagingSettings.Exposure.Gain                 := -1;  
  FImagingSettings.Focus.AutoFocusMode           := ifmUnknown;
  FImagingSettings.Focus.DefaultSpeed            := -1;
  FImagingSettings.IrCutFilter                   := icfmUnknown;
  FImagingSettings.Sharpness                     := -1;
  FImagingSettings.WideDynamicRange.Mode         := String.Empty; 
  FImagingSettings.WhiteBalance.Mode             := String.Empty;   
  FImagingSettings.Extension.Defogging.Mode      := String.Empty;   
  FImagingSettings.Extension.Defogging.Level     := -1;
  FImagingSettings.Extension.NoiseReduction.Level:= -1;   
end;

destructor TONVIFImagingManager.Destroy;
begin
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
    GetStatus;  
end;

Function TONVIFImagingManager.GetMoveOptions:Boolean;
var LResponseStr       : String;
    LSoapBodyNode      : IXMLNode;
    LMoveOptionNode    : IXMLNode;
    LNodeSpeed         : IXMLNode;
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
      //FFocusSettings.Options.Relative.Distance     
      FFocusSettings.Options.Relative.Supported := True;
      LNodeSpeed                                := TONVIFXMLUtils.RecursiveFindNode(LMoveOptionNode,'Speed');
      if Assigned(LNodeSpeed) then
      begin
        FFocusSettings.Options.Relative.Speed.Min := StrToFloatLocale(TONVIFXMLUtils.GetChildNodeValue(LNodeSpeed,'Min'),-1);  
        FFocusSettings.Options.Relative.Speed.Max := StrToFloatLocale(TONVIFXMLUtils.GetChildNodeValue(LNodeSpeed,'Max'),-1);
      end;
    end;

    LMoveOptionNode := TONVIFXMLUtils.RecursiveFindNode(LSoapBodyNode,'Absolute');
    if Assigned(LMoveOptionNode) then
    begin
      //TODO FFocusSettings.Options.Absolute.Position 
      FFocusSettings.Options.Absolute.Supported := True;
      LNodeSpeed                                := TONVIFXMLUtils.RecursiveFindNode(LMoveOptionNode,'Speed');
      if Assigned(LNodeSpeed) then
      begin
        FFocusSettings.Options.Absolute.Speed.Min := StrToFloatLocale(TONVIFXMLUtils.GetChildNodeValue(LNodeSpeed,'Min'),-1);  
        FFocusSettings.Options.Absolute.Speed.Max := StrToFloatLocale(TONVIFXMLUtils.GetChildNodeValue(LNodeSpeed,'Max'),-1);
      end;
    end;

    LMoveOptionNode := TONVIFXMLUtils.RecursiveFindNode(LSoapBodyNode,'Continuous');
    if Assigned(LMoveOptionNode) then
    begin
      FFocusSettings.Options.Continuous.Supported := True;
      LNodeSpeed                                  := TONVIFXMLUtils.RecursiveFindNode(LMoveOptionNode,'Speed');
      if Assigned(LNodeSpeed) then
      begin
        FFocusSettings.Options.Continuous.Speed.Min := StrToFloatLocale(TONVIFXMLUtils.GetChildNodeValue(LNodeSpeed,'Min'),-1);  
        FFocusSettings.Options.Continuous.Speed.Max := StrToFloatLocale(TONVIFXMLUtils.GetChildNodeValue(LNodeSpeed,'Max'),-1);
      end;
    end;
    Result := True;  
  end;
end;

function TONVIFImagingManager.GetStatus: Boolean;
var LResponseStr       : String;
    LSoapBodyNode      : IXMLNode;
    LGestStatus        : IXMLNode;
    I                  : Integer;  
begin
  Result := FONVIFManager.ExecuteRequest(atImaging,'TONVIFImagingManager.GetImagingSettings',FONVIFManager.GetSOAPBuilder.PrepareImagingGetStatus(FToken),LResponseStr);
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
      begin
        FFocusSettings.Status.Position := StrToFloatLocale(LGestStatus.ChildNodes[I].Text,-1) 

        {TODO Add event with current position e Max e Min range of focus }
      end
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

  if GetMoveOptions then
  begin
    if SupportedInfo.FocusSupported then
      GetStatus;
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
      end;

      LTmpStrValue := TONVIFXMLUtils.GetChildNodeValue(LImgSettingsNode,'IrCutFilter').Trim;
      if SameText(LTmpStrValue,'AUTO') then
        FImagingSettings.IrCutFilter := icfmAuto
      else if SameText(LTmpStrValue,'ON') then
        FImagingSettings.IrCutFilter := icfmON
      else if SameText(LTmpStrValue,'OFF') then
        FImagingSettings.IrCutFilter := icfmOFF;

      FImagingSettings.Sharpness := StrToIntDef(TONVIFXMLUtils.GetChildNodeValue(LImgSettingsNode,'Sharpness'),-1); 
      
      LNodeTmp1 := TONVIFXMLUtils.RecursiveFindNode(LImgSettingsNode,'WideDynamicRange');
      if Assigned(LNodeTmp1) then
        FImagingSettings.WideDynamicRange.Mode := TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'Mode');

      LNodeTmp1 := TONVIFXMLUtils.RecursiveFindNode(LImgSettingsNode,'WhiteBalance');
      if Assigned(LNodeTmp1) then      
        FImagingSettings.WhiteBalance.Mode := TONVIFXMLUtils.GetChildNodeValue(LNodeTmp1,'Mode');        

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
