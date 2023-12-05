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

unit ONVIF.XML.Utils;

interface

uses XmlDoc, XmlIntf, XMLDom,System.SysUtils; 


Type

  /// <summary>
  ///   Utility class for handling XML-related operations in the ONVIF context.
  /// </summary>
  /// <remarks>
  ///   This class provides various methods and functions to assist with the
  ///   manipulation, parsing, and generation of XML documents in compliance
  ///   with the ONVIF standard.
  /// </remarks>
  TONVIFXMLUtils = class    
  public
    /// <summary>
    ///   Retrieves the value of a child node within a specified parent node.
    /// </summary>
    /// <param name="ParentNode">
    ///   The parent XML node from which to retrieve the child node value.
    /// </param>
    /// <param name="ChildNodeName">
    ///   The name of the child node whose value is to be retrieved.
    /// </param>
    /// <returns>
    ///   The string value of the specified child node if found; otherwise, an empty string.
    /// </returns>
    /// <remarks>
    ///   Use this function to conveniently obtain the value of a specific child node within
    ///   a given parent node. If the specified child node does not exist, the function
    ///   returns an empty string.
    /// </remarks>    
    class function GetChildNodeValue(const ParentNode: IXMLNode;const ChildNodeName: string): string;static;

    /// <summary>
    ///   Retrieves the value of a specified attribute from an XML node.
    /// </summary>
    /// <param name="Node">
    ///   The XML node from which to retrieve the attribute value.
    /// </param>
    /// <param name="AttributeName">
    ///   The name of the attribute whose value is to be retrieved.
    /// </param>
    /// <returns>
    ///   The string value of the specified attribute if found; otherwise, an empty string.
    /// </returns>
    /// <remarks>
    ///   Use this function to conveniently obtain the value of a specific attribute within
    ///   a given XML node. If the specified attribute does not exist, the function returns
    ///   an empty string.
    /// </remarks>    
    class function GetAttribute(const Node: IXMLNode;const AttributeName: string): string;static;

    /// <summary>
    /// Retrieves the SOAP body node from the given SOAP XML document.
    /// </summary>
    /// <param name="aRootNode">The root node of the SOAP XML document.</param>
    /// <returns>The SOAP body node.</returns>
    class function GetSoapBody(const aRootNode: IXMLNode): IXMLNode;static;

    /// <summary>
    /// Recursively searches for an XML node with the specified name within the given XML node.
    /// </summary>
    /// <param name="ANode">The XML node to start the search from.</param>
    /// <param name="aSearchNodeName">The name of the XML node to search for.</param>
    /// <returns>The found XML node or nil if not found.</returns>
    class function RecursiveFindNode(ANode: IXMLNode; const aSearchNodeName: string;const aScanAllNode: Boolean=False): IXMLNode;static;    

  end;
  
Function StrToFloatLocale(const S: string; const Default: Extended):Extended;

implementation

Function StrToFloatLocale(const S: string; const Default: Extended):Extended;
begin
  if FormatSettings.DecimalSeparator = '.' then
    Result := StrToFloatDef(s.Replace(',','.'),Default)
  else    
    Result := StrToFloatDef(s.Replace('.',','),Default)      
end;


{ TONVIFXMLUtils }
class function TONVIFXMLUtils.GetChildNodeValue(const ParentNode: IXMLNode; const ChildNodeName: string): string;
var LTmpIndex : Integer;
begin
  Result := String.Empty;
  if Assigned(ParentNode) then
  begin
    // Check if the child node exists before accessing its value
    LTmpIndex := ParentNode.ChildNodes.IndexOf(ChildNodeName,'') ;
    if LTmpIndex > -1 then
      Result := ParentNode.ChildNodes[LTmpIndex].Text
  end;
end; 

class function TONVIFXMLUtils.GetAttribute(const Node: IXMLNode; const AttributeName: string): string;
begin
  Result := String.empty;
  if Assigned(Node) and Node.HasAttribute(AttributeName) then
    Result := Node.Attributes[AttributeName];
end;

class function TONVIFXMLUtils.GetSoapBody(const aRootNode :IXMLNode) : IXMLNode;
CONST cNodeSOAPBody = 'Body';
begin
  Result := aRootNode.ChildNodes[cNodeSOAPBody];  
end;

class function TONVIFXMLUtils.RecursiveFindNode(ANode: IXMLNode; const aSearchNodeName: string;const aScanAllNode: Boolean=False): IXMLNode;
var I       : Integer;
    LResult : IXMLNode;
begin
  Result := nil;     
  LResult:= nil;         
  if not Assigned(ANode) then exit;

  if CompareText(ANode.DOMNode.localName , aSearchNodeName) = 0 then
  begin
    LResult := ANode;
    if not aScanAllNode then Exit(LResult);
  end;

  if Assigned(ANode.ChildNodes) then
  begin
    for I := 0 to ANode.ChildNodes.Count - 1 do
    begin
      Result := RecursiveFindNode(ANode.ChildNodes[I], aSearchNodeName,aScanAllNode);
      if (Result <> nil ) then
      begin
         if not aScanAllNode then Exit;

         LResult := Result;
      end;
    end;
  end;
  Result := LResult;
end;




end.
