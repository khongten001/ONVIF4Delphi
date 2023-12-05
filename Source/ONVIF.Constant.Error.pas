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

unit ONVIF.Constant.Error;

interface
CONST {Error Code}
      ONVIF_ERROR_URL_EMPTY                          = -1000;
      ONVIF_ERROR_SOAP_INVALID                       = -1001;
      ONVIF_ERROR_SOAP_NOBODY                        = -1002;
      ONVIF_ERROR_SOAP_FAULTCODE_NOT_FOUND           = -1003;     
      ONVIF_ERROR_PTZ_INVALID_PRESET_INDEX           = -1004;
      ONVIF_ERROR_DELPHI_EXCEPTION                   = -1005;
      
      {PTZ}
      ONVIF_ERROR_PTZ_TOKEN_IS_EMPTY                 = -1206;
      ONVIF_ERROR_PTZ_PRESETNAME_IS_EMPTY            = -1207;
      ONVIF_ERROR_PTZ_HOME_COMMAND_NOT_SUPPORTED     = -1208;
      ONVIF_ERROR_PTZ_MAX_PRESET                     = -1209; 
      ONVIF_ERROR_PTZ_MOVE_CONTINUOUS_NOT_SUPPORTED  = -1210;  
      ONVIF_ERROR_PTZ_MOVE_ABSOLUTE_NOT_SUPPORTED    = -1211; 
      ONVIF_ERROR_PTZ_MOVE_RELATIVE_NOT_SUPPORTED    = -1212;             

      {Imaging}
      ONVIF_ERROR_IMG_IMMAGING_IS_EMPTY              = -1313;
      ONVIF_ERROR_IMG_FOCUS_NOT_SUPPORTED            = -1314;  
      ONVIF_ERROR_IMG_FOCUS_CONTINUOUS_NOT_SUPPORTED = -1315;        
      ONVIF_ERROR_IMG_FOCUS_ABSOLUTE_NOT_SUPPORTED   = -1316;   
      ONVIF_ERROR_IMG_FOCUS_RELATIVE_NOT_SUPPORTED   = -1317;     
      ONVIF_ERROR_IMG_FOCUS_ABSOLUTE_OUT_OF_RANGE    = -1318;  

implementation

end.
