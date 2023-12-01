unit ONVIF.Structure.PTZ;

interface

uses System.Generics.Collections;


Type
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
  
  TPTZPresetList = TList<TPTZPreset>;

implementation

end.
