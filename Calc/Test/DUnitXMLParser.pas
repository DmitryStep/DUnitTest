unit DUnitXMLParser;

interface

uses
  System.SysUtils, System.Variants,  Xml.xmldom, Xml.XMLIntf, Xml.Win.msxmldom, Xml.XMLDoc, TestStructureUnit, RTTI,
  System.Generics.Collections;

type
  TTestsXMLParser = class
  private
    FXMLFileName: string;
    FXMLDocument: IXMLDocument;
    function GetRootNode: IXMLNode;
    function GetChildNodeByIndex(Node: IXMLNode; aNodeIndex: Integer): IXMLNode;
    function GetAttribute(Node: IXMLNode; aAttributeName: string): String;
    function CheckNodeName(Node: IXMLNode; ExpectedName: string): Boolean;
    function IsChildExists(Node: IXMLNode): Boolean;
    function GetChildCount(Node: IXMLNode): integer;
    function LoadXML: Boolean;
    procedure UnLoadXML;
  public
    constructor Create(aXMLFileName: string);
    procedure RunXMLParsing(var aTestCaseList: TTestCaseList; var aSuiteList: TSuiteList);
  end;

  procedure LoadTestsFromXML(aXMLFileName: string; var aSuiteList: TSuiteList; var aTestCaseList: TTestCaseList);

implementation

function TTestsXMLParser.CheckNodeName(Node: IXMLNode; ExpectedName: string): Boolean;
begin
  Result := AnsiLowerCase(Node.NodeName) = AnsiLowerCase(ExpectedName);
end;

constructor TTestsXMLParser.Create(aXMLFileName: string);
begin
  FXMLDocument := TXMLDocument.Create(nil);
  FXMLFileName := aXMLFileName;
end;


function TTestsXMLParser.GetRootNode: IXMLNode;
begin
  Result := FXMLDocument.ChildNodes[0];
end;


function TTestsXMLParser.GetChildNodeByIndex(Node: IXMLNode; aNodeIndex: Integer): IXMLNode;
begin
  Result := Node.ChildNodes[aNodeIndex];
end;


function TTestsXMLParser.GetAttribute(Node: IXMLNode; aAttributeName: string): String;
begin
  Result := Node.Attributes[aAttributeName];
end;



function TTestsXMLParser.IsChildExists(Node: IXMLNode): Boolean;
begin
  Result := Node.HasChildNodes;
end;


function TTestsXMLParser.GetChildCount(Node: IXMLNode): integer;
begin
  Result := Node.ChildNodes.Count;
end;


function TTestsXMLParser.LoadXML: Boolean;
begin
  Result := false;
  try
    if FileExists(FXMLFileName) then
    begin
      FXMLDocument.LoadFromFile(FXMLFileName);
      if not FXMLDocument.IsEmptyDoc then
        Result := true;
    end
    else
      Result := False;
  except
    Result := False;
  end;
  FXMLDocument.Active := Result;
end;


procedure TTestsXMLParser.UnLoadXML;
begin
  FXMLDocument.Active := false;
end;


procedure TTestsXMLParser.RunXMLParsing(var aTestCaseList: TTestCaseList; var aSuiteList: TSuiteList);


// Мама - рекурсия, папа - стакан портвейна!
procedure GetDataFromNode(RootNode: IXMLNode; var aSuitesCounter: Integer; var aTestsCounter: Integer;
                          var aSuiteList: TSuiteList; var aTestCaseList: TTestCaseList);
var
  iNodesCounter: Integer;
  iParamsCounter: Integer;
  sSuiteName: string;
  sTestCaseClass: string;
  ParamsArray: TInputDataArray;
begin
  // Корень - уровень Zero
  if CheckNodeName(RootNode, 'Tests') then
  begin
    for iNodesCounter := 0 to GetChildCount(RootNode) - 1 do
    begin
      // Падаем на 1
      GetDataFromNode(GetChildNodeByIndex(RootNode, iNodesCounter), aSuitesCounter, aTestsCounter, aSuiteList, aTestCaseList)
    end;
  end;

  // TestSuites - уровень 1
  if CheckNodeName(RootNode, 'TestSuite') and IsChildExists(RootNode) then
  begin
    SetLength(aSuiteList, aSuitesCounter + 1);
    sSuiteName := GetAttribute(RootNode, 'Name') + ': ' + GetAttribute(RootNode, 'Description');
    sTestCaseClass := GetAttribute(RootNode, 'ClassName');
    aSuiteList[aSuitesCounter].SuiteName := sSuiteName;
    aSuiteList[aSuitesCounter].SuiteClassName := sTestCaseClass;
    for iNodesCounter := 0 to GetChildCount(RootNode) - 1 do
    begin
      SetLength(aTestCaseList, aTestsCounter + 1);
      aTestCaseList[aTestsCounter].SuiteName := sSuiteName;
      aTestCaseList[aTestsCounter].TestCaseClass := sTestCaseClass;
      // Опускаемся на 2
      GetDataFromNode(GetChildNodeByIndex(RootNode, iNodesCounter), aSuitesCounter, aTestsCounter, aSuiteList, aTestCaseList);
    end;
    Inc(aSuitesCounter);
  end;

  // TestCases - уровень 2
  if CheckNodeName(RootNode, 'TestCase') and IsChildExists(RootNode) then
  begin
    aTestCaseList[aTestsCounter].TestCaseName := GetAttribute(RootNode, 'Name') + ': ' + GetAttribute(RootNode, 'Description');
    aTestCaseList[aTestsCounter].MethodName := GetAttribute(RootNode, 'MethodName');
    for iNodesCounter := 0 to GetChildCount(RootNode) - 1 do
    begin
      // Последний уровень 3, ниже некуда
      GetDataFromNode(GetChildNodeByIndex(RootNode, iNodesCounter), aSuitesCounter, aTestsCounter, aSuiteList, aTestCaseList);
    end;
    Inc(aTestsCounter);
  end;

  // InputData - уровень 3
  if CheckNodeName(RootNode, 'InputData') then
  begin
    iParamsCounter := 0;
    While RootNode.HasAttribute('Param' + IntToStr(iParamsCounter)) do
    begin
      SetLength(ParamsArray, iParamsCounter + 1);
      ParamsArray[iParamsCounter] := TValue.From<String>(GetAttribute(RootNode, 'Param' + IntToStr(iParamsCounter)));
      Inc(iParamsCounter);
    end;
    DataArray.Add(aTestCaseList[aTestsCounter].TestCaseName, ParamsArray);
    ParamsArray := nil;
  end;

  // ExpectedResult - уровень 3
  if CheckNodeName(RootNode, 'ExpectedResult') then
  begin
    SetLength(ParamsArray, Length(DataArray.Items[aTestCaseList[aTestsCounter].TestCaseName]));
    ParamsArray := DataArray.Items[aTestCaseList[aTestsCounter].TestCaseName];
    SetLength(ParamsArray, Length(ParamsArray) + 3);
    ParamsArray[Length(ParamsArray) - 3] := TValue.From<String>(GetAttribute(RootNode, 'Value'));
    ParamsArray[Length(ParamsArray) - 2] := TValue.From<String>(GetAttribute(RootNode, 'FailMessageText'));
    ParamsArray[Length(ParamsArray) - 1] := TValue.From<String>(GetAttribute(RootNode, 'Operation'));
    DataArray.Items[aTestCaseList[aTestsCounter].TestCaseName] := ParamsArray;
    ParamsArray := nil;
  end;
end;
// Конец рекурсивного угробища и начало процедуры

var
  iSuitesCounter: integer;
  iTestsCounter: Integer;

begin  // RunXMLParsing
  iSuitesCounter := 0;
  iTestsCounter := 0;
  LoadXML;
  GetDataFromNode(GetRootNode, iSuitesCounter, iTestsCounter, aSuiteList, aTestCaseList);
  UnLoadXML;
end;  // RunXMLParsing


procedure LoadTestsFromXML(aXMLFileName: string; var aSuiteList: TSuiteList; var aTestCaseList: TTestCaseList);
var
  XMLParser: TTestsXMLParser;
begin
  XMLParser := TTestsXMLParser.Create(aXMLFileName);
  try
    XMLParser.RunXMLParsing(aTestCaseList, aSuiteList);
  finally
    FreeAndNil(XMLParser);
  end;
end;

end.
