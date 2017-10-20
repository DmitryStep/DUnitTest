unit DUnitTestingCore;

interface

uses
  System.Math, System.SysUtils, System.Variants, System.Generics.Collections, System.Generics.Defaults,
  TestFramework, TestStructureUnit, RTTI, Classes, DUnitXMLParser;

type

  TVarArray = array of Variant;
  TCoreTestCaseClass = class of TCoreTestCase;

  TTestClassesList = TList<TCoreTestCaseClass>;

  // TestSuites
  TCoreTestSuite = class (TTestSuite)
  private
    FSuiteName: string;
    FSuitePath: string;
  public
    constructor Create(aSuitePath: string; aSuiteName: string; Tests: TTestCaseList; aTestClass: TCoreTestCaseClass); overload;
    procedure AddTests(Tests: TTestCaseList; aTestClass: TCoreTestCaseClass); virtual;
  end;

  //TestCases
  TCoreTestCase = class (TTestCase)
  protected
    FFolderName: string;
    FSuitePath: string;
    FSuiteName: string;
    FTestClass: TCoreTestCaseClass;
    FMethodName: string;
    ExpectedResult: Variant;
    FailMessage: string;
    Operation: string;
    function VarToBool(Arg: Variant): Boolean;
  public
    constructor Create(TestCase: TTestCaseRec); reintroduce; overload;
    procedure AssertResults<T>(aExpectedResult: Variant; aActualResult: T; aOperation: string; aFailMessageTemplate: string);
    procedure AssertResult<T>(aActualResult: T);
    procedure AssertDoubleResults(ExpectedResult: Variant; ActualResult: Double; Operation: string; FailMessageTemplate: string);
    procedure AssertBooleanResults(ExpectedResult: Variant; ActualResult: Boolean; Operation: string; FailMessageTemplate: string);
    function IsArray(Arg: Variant): Boolean;
    function IsStructure(Arg: Variant): Boolean;
    function GetParameters: TVarArray;
    function GetStructure(RecStr: Variant): TArray<Variant>;
    function GetArray(ArrStr: Variant): TArray<Variant>;
    function ArrayLength(ArrStr: Variant): integer;
  end;

  procedure PrepareToTest(TestsFileName: string);
  procedure RegisterTestClass(aClass: TCoreTestCaseClass);
  procedure CreateDUnitTests(Suites: TSuiteList; Tests: TTestCaseList; aTestClass: TCoreTestCaseClass);
  procedure CreateTests;

var
  TestClassesList: TTestClassesList;

implementation


constructor TCoreTestCase.Create(TestCase: TTestCaseRec);
begin
  inherited Create(TestCase.MethodName);

  FFolderName := TestCase.TestCaseClass;
  FSuitePath := TestCase.SuiteName;
  FSuiteName := TestCase.SuiteName;
  FTestName := TestCase.TestCaseName;
  FMethodName := TestCase.MethodName;
end;


function TCoreTestCase.VarToBool(Arg: Variant): Boolean;
begin
  Result := LowerCase(VarToStr(Arg)) = 'true';
end;


function TCoreTestCase.GetParameters: TVarArray;
var
  ParamValue: TInputDataArray;
  DataLen: integer;
  Index: integer;
begin
  ParamValue := DataArray.Items[Self.FTestName];
  DataLen := Length(ParamValue);
  SetLength(Result, DataLen - 3);
  for Index := 0 to DataLen - 4 do
  begin
    Result[Index] := ParamValue[Index].AsVariant;
  end;
  Self.ExpectedResult := ParamValue[DataLen - 3].AsVariant;
  Self.FailMessage := ParamValue[DataLen - 2].AsString;
  Self.Operation := ParamValue[DataLen - 1].AsString;
end;


function TCoreTestCase.IsStructure(Arg: Variant): Boolean;
begin
  Result := Pos('struct', VarToStr(Arg)) > 0;
end;


function TCoreTestCase.GetStructure(RecStr: Variant): TArray<Variant>;
const
  Delimiter = ';';
var
  DelimiterPos: Integer;
  Index: integer;
  TempStr: string;
begin
  Index := 0;
  TempStr := Trim(RecStr);
  TempStr := StringReplace(TempStr, 'struct(', '', [rfReplaceAll]);
  TempStr := StringReplace(TempStr, ')', '', [rfReplaceAll]);
  DelimiterPos := Pos(Delimiter, TempStr);
  while DelimiterPos > 0 do
  begin
    SetLength(Result, Index + 1);
    Result[Index] := Copy(TempStr, 1, DelimiterPos - 1);
    Delete(TempStr, 1, DelimiterPos);
    DelimiterPos := Pos(Delimiter, TempStr);
    Inc(Index);
  end;
  SetLength(Result, Index + 1);
  Result[Index] := TempStr;
end;


function TCoreTestCase.IsArray(Arg: Variant): Boolean;
begin
  Result := Pos('array', VarToStr(Arg)) > 0;
end;


function TCoreTestCase.GetArray(ArrStr: Variant): TArray<Variant>;
const
  Delimiter = ';';

var
  CharIndex: integer;
  ArrayIndex: Integer;
  TempStr: string;
  ResultStr: string;
  IsStruct: Boolean;
begin
  ResultStr := '';
  ArrayIndex := 0;
  IsStruct := False;
  TempStr := Trim(ArrStr);
  TempStr := StringReplace(TempStr, 'array[', '', [rfReplaceAll]);
  TempStr := StringReplace(TempStr, ']', '', [rfReplaceAll]);

  for CharIndex := 1 to Length(TempStr) do
  begin
    if TempStr[CharIndex] = '(' then
      IsStruct := True;
    if TempStr[CharIndex] = ')' then
      IsStruct := False;

    if (not IsStruct and (TempStr[CharIndex] = Delimiter)) or
       (CharIndex = Length(TempStr))
    then
    begin
      SetLength(Result, ArrayIndex + 1);
      Result[ArrayIndex] := ResultStr;
      inc(ArrayIndex);
      ResultStr := '';
    end
    else
    begin
      ResultStr := ResultStr + TempStr[CharIndex];
    end;

  end;
end;


function TCoreTestCase.ArrayLength(ArrStr: Variant): integer;
const
  Delimiter = ';';
var
  TempStr: string;
  CharIndex: Integer;
  IsStruct: Boolean;
begin
  TempStr := VarToStr(ArrStr);
  Result := 0;
  IsStruct := False;
  if TempStr <> 'array[]' then
  begin
    for CharIndex := 1 to Length(TempStr) do
    begin
      if TempStr[CharIndex] = '(' then
        IsStruct := True;
      if TempStr[CharIndex] = ')' then
        IsStruct := False;
      if not IsStruct and (TempStr[CharIndex] = Delimiter) then
        inc(Result);
    end;
    Result := Result + 1;
  end;
end;


procedure TCoreTestCase.AssertResult<T>(aActualResult: T);
begin
  AssertResults<T>(ExpectedResult, aActualResult, Operation, FailMessage);
end;


procedure TCoreTestCase.AssertResults<T>(aExpectedResult: Variant; aActualResult: T; aOperation: string; aFailMessageTemplate: string);
var
  AssertionResult: Boolean;
  ActualResultValue: TValue;
  ExpectedResultValue: TValue;
  FailMessageValue: String;
begin
  if aOperation = 'except' then
    CheckException(fMethod, Exception, '')
  else
  begin
    ActualResultValue := TValue.From<T>(aActualResult);
    ExpectedResultValue := TValue.From<Variant>(aExpectedResult);
    if aOperation = 'equals' then
      AssertionResult := ActualResultValue.AsVariant = ExpectedResultValue.AsVariant;
    if aOperation = 'not equals' then
      AssertionResult := ActualResultValue.AsVariant <> ExpectedResultValue.AsVariant;
    if aOperation = 'larger than' then
      AssertionResult := ActualResultValue.AsVariant > ExpectedResultValue.AsVariant;
    if aOperation = 'equals or larger than' then
      AssertionResult := ActualResultValue.AsVariant >= ExpectedResultValue.AsVariant;
    if aOperation = 'less than' then
      AssertionResult := ActualResultValue.AsVariant < ExpectedResultValue.AsVariant;
    if aOperation = 'equals or less than' then
      AssertionResult := ActualResultValue.AsVariant <= ExpectedResultValue.AsVariant;
    if aOperation = 'contains' then
      AssertionResult := Pos(VarToStr(ActualResultValue.AsVariant), VarToStr(ExpectedResultValue.AsVariant)) > 0;
    if aOperation = 'not contains' then
      AssertionResult := Pos(VarToStr(ActualResultValue.AsVariant), VarToStr(ExpectedResultValue.AsVariant)) = 0;

    FailMessageValue := StringReplace(aFailMessageTemplate, '%r', VarToStr(ActualResultValue.AsVariant), [rfReplaceAll]);
    FailMessageValue := StringReplace(FailMessageValue, '%o', Operation, [rfReplaceAll]);
    FailMessageValue := StringReplace(FailMessageValue, '%e', VarToStr(ExpectedResultValue.AsVariant), [rfReplaceAll]);
    if Pos(' not not', FailMessageValue) > 0 then
      FailMessageValue := StringReplace(FailMessageValue, ' not not', '', [rfReplaceAll]);

    Check(AssertionResult, FailMessageValue);
  end;
end;


procedure TCoreTestCase.AssertDoubleResults(ExpectedResult: Variant; ActualResult: Double; Operation: string; FailMessageTemplate: string);
var
  ExpectedResultValue: string;
  ActualResultValue: string;
  TrimValue: Integer;
begin
  ExpectedResultValue := StringReplace(VarToStr(ExpectedResult), ',', '.', [rfReplaceAll]);
  TrimValue := Length(ExpectedResultValue) - Pos('.', ExpectedResultValue);
  ActualResultValue := StringReplace(FloatToStr(RoundTo(ActualResult, -TrimValue)), ',', '.', [rfReplaceAll]);
  AssertResults<String>(ExpectedResultValue, ActualResultValue, Operation, FailMessage);
end;


procedure TCoreTestCase.AssertBooleanResults(ExpectedResult: Variant; ActualResult: Boolean; Operation: string; FailMessageTemplate: string);
var
  ExpectedResultValue: string;
  ActualResultValue: string;
  TrimValue: Integer;
begin
  ExpectedResultValue := LowerCase(VarToStr(ExpectedResult));
  ActualResultValue := LowerCase(BoolToStr(ActualResult));
  AssertResults<String>(ExpectedResultValue, ActualResultValue, Operation, FailMessage);
end;


constructor TCoreTestSuite.Create(aSuitePath: string; aSuiteName: string; Tests: TTestCaseList; aTestClass: TCoreTestCaseClass);
begin
  inherited Create(aSuiteName);
  FSuitePath := aSuitePath;
  FSuiteName := aSuiteName;
  AddTests(Tests, aTestClass);
end; // Create


procedure TCoreTestSuite.AddTests(Tests: TTestCaseList; aTestClass: TCoreTestCaseClass);
var
  TestCaseIndex: integer;
  MethodName: string;
  TestName: string;
  SuiteName: string;

begin
  // For all tests from file
  for TestCaseIndex := 0 to Length(Tests) - 1 do
  begin
    // Get method for current test
    MethodName := Tests[TestCaseIndex].MethodName;

    // Get suite name for current test
    SuiteName := Tests[TestCaseIndex].SuiteName;
    if SuiteName = '' then
       SuiteName := aTestClass.ClassName;

    // Get test case name for current test
    TestName := Tests[TestCaseIndex].TestCaseName;
    if TestName = '' then
      TestName := MethodName;

    // If is current suite and test class then add test to suite
    if (Tests[TestCaseIndex].SuiteName = Self.FSuiteName) and
       (Tests[TestCaseIndex].TestCaseClass = aTestClass.ClassName)
    then
    begin
      Self.AddTest(aTestClass.Create(Tests[TestCaseIndex]) as ITest);
    end;
  end;
end; // AddTests


procedure PrepareToTest(TestsFileName: string);
var
  FileName: string;
begin
  // Имя файла передаётся в этом случае первым параметром запуска
  if ParamStr(1) <> '' then
    FileName := ExtractFilePath(ParamStr(0)) + ParamStr(1)
  else
    FileName := ExtractFilePath(ParamStr(0)) + TestsFileName;

  LoadTestsFromXML(FileName, SuiteList, TestCaseList);

//  if IsConsole then
//  begin
//    {$APPTYPE CONSOLE}
//  end;
end; // PrepareToTest


procedure RegisterTestClass(aClass: TCoreTestCaseClass);
begin
  if not Assigned(TestClassesList) then
    TestClassesList :=TList<TCoreTestCaseClass>.Create;
  TestClassesList.Add(aClass);
end;


procedure CreateDUnitTests(Suites: TSuiteList; Tests: TTestCaseList; aTestClass: TCoreTestCaseClass);
var
  iSuiteIndex: integer;
  Suite: TCoreTestSuite;
begin
  for iSuiteIndex := 0 to  Length(Suites) - 1 do
  begin
    if Suites [iSuiteIndex].SuiteClassName = aTestClass.ClassName then
    begin
//      Suite := TCoreTestSuite.Create(aTestClass.ClassName, Suites[iSuiteIndex].SuiteName, Tests, aTestClass);
      Suite := TCoreTestSuite.Create('', Suites[iSuiteIndex].SuiteName, Tests, aTestClass);
//      RegisterTest(aTestClass.ClassName, Suite);
      RegisterTest('', Suite);
    end;
  end;
end;


procedure CreateTests;
var
  TestClass: TCoreTestCaseClass;
begin
  for TestClass in TestClassesList do
  begin
    CreateDUnitTests(SuiteList, TestCaseList, TestClass);
  end;
end;


end.
