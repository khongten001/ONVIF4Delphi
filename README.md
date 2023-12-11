# ONVIF4Delphi Project
Welcome to the ONVIF_WSDL project! This repository is dedicated to providing a comprehensive solution for handling ONVIF 

# Project Overview
The ONVIF_WSDL project focuses on managing essential aspects of ONVIF protocols, offering functionality for:

**Device Information**: Effortlessly retrieve and manage information about connected devices.

**Profiles**: Seamlessly handle ONVIF profiles, ensuring compatibility and easy integration.

**Capabilities**: Access device capabilities.

**NetworkInterface**: Access to network interface information.

**StreamURI**: Access URI RTSP info

The PTZ (Pan-Tilt-Zoom): functionality in this project provides support for controlling camera movement.

- **Continuous Move:** The current implementation supports continuous movement. You can start, stop, and control the speed of continuous panning and tilting.

- **Zoom Control:** Basic zoom control is available. For example, you can zoom in or out.

- **Preset Management:** Supports preset management, allowing users to save and recall specific camera configurations.
  
- **PTZ Command supported (PTZ.SupportedInfo):** This feature allows users to retrieve information about the supported PTZ commands of the camera . It provides details on available commands, helping users understand the camera's capabilities.

- **GoHome Position:** The library includes a GoHome position feature, allowing the camera to return to a predefined home position.

- **SetHomePosition:** Users can set a custom home position for the camera, providing flexibility in defining a default reference point.

- **Auxiliary commands:** Users can set auxiliary command if supported by camera , like Wirer and IRLamp ecc ecc.

**Note:** As of now, only continuous movement (ContinuousMove) is supported.

The Imaging: functionality in this project is focused on video adjustments 

- **Imaging Settings:** The feature provides users with the ability to retrieve information about the configuration of the imaging feature.
- **Imaging options:** The feature provides users with the ability to retrieve information about then range (Min/Max) of the imaging settings feature.
- **Focus:** supported in continuous and absolute mode.

# How to Use
Example Delphi code for using the ONVIF_WSDL project:
```delphi
// Create an instance of TONVIFManager with login credentials and set up URL
LONVIFManager := TONVIFManager.Create(String.Empty, <Login>, <Password>);
LONVIFManager.SaveResponseOnDisk := True;
LONVIFManager.Url := <URL onvif example http://xxx.xxx.xxx.xxx:580/>;
LONVIFManager.ReadInfo or ReadInfoAsync
```
Now you can interact with ONVIF features using LONVIFManager
For instance, you can retrieve device information, handle profiles, and more.

Don't forget to replace <Login>, <Password>, and <URL onvif example http://xxx.xxx.xxx.xxx:580/>
with your actual credentials and ONVIF device URL.

# Documentation
comprehensive English documentation set is embedded directly in the source code in XML format, providing an easily accessible reference for developers

You can find an executable in the Demo\Bin folder with the implemented functions ready for testing.
![image](https://github.com/amancini/ONVIF_WDSL/assets/11525545/e3825f4c-7f43-4637-bf57-7e0dae7b48ff)

# Camera Library Testing

The library has been successfully tested with different cameras from various brands.

## Test Results

| CAMERA MODEL              | RESULT |
|---------------------------|--------|
| HIKVISION DS-2DF8442IXS   |   OK   |
| PANASONIC IPRO WV-S6131   |   OK   |
| AXIS v5915                |   OK   |
| DAHUA DH-PTZ              |   OK   |
| IPRO WV-U65302            |   OK   |



# Contributions
Contributions are welcome! If you encounter any issues, have suggestions for improvements, or want to contribute new features, please check our Contribution Guidelines.

# License
This project is licensed under the MIT License, making it open and accessible for a wide range of applications.

Thank you for choosing ONVIF_WSDL for your project. Happy coding! 🚀
