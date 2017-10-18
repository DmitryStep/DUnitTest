unit DUnitTestingCore;

interface

uses
  SysUtils, Variants, System.Generics.Collections, System.Generics.Defaults, TestFramework, TestStructureUnit,
  RTTI, Classes, DUnitXMLParser;

const
  MT_FUNCTION = true;
  MT_PROCEDURE = false;

type

  TCoreTestCaseClass = class of TCoreTestCase;

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
    ExpectedResult: TValue;
    FailMessage: string;
    Operation: string;
    ParamValue: TInputDataArray;
  public
    constructor Create(TestCase: TTestCaseRec); reintroduce; overload;
    procedure AssertResults(ExpectedResult: TValue; ActualResult: TValue; Operation: string; FailMessageTemplate: string);
    function RunFunction(aClass: TClass; aMethodName: string; aArgs: TInputDataArray): TValue;
    procedure RunProcedure(aClass: TClass; aMethodName: string; aArgs: TInputDataArray);
    procedure RunMethod(aClass: TClass; aMethodName: string; aArgs: TInputDataArray;
                           MethodType: boolean; ExpectedResult: TValue; Operation: string; FailMessageTemplate: string);  end;

  procedure PrepareToTest(TestsFileName: string);
  procedure CreateDUnitTests(Suites: TSuiteList; Tests: TTestCaseList; aTestClass: TCoreTestCaseClass);

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



function TCoreTestCase.RunFunction(aClass: TClass; aMethodName: string; aArgs: TInputDataArray): TValue;
var
  RTTIContext: TRTTIContext;
  RTTIType: TRTTIType;
  RTTIMethod: TRTTIMethod;
  RTTIParam: TRTTIParameter;
  fArgs: TInputDataArray;
  Index: integer;
  Res: integer;
begin
  RTTIContext := TRTTIContext.Create;
  try
    RTTIType := RTTIContext.GetType(aClass);
    RTTIMethod := RTTIType.GetMethod(aMethodName);
    SetLength(fArgs, length(aArgs) - 3);
    for Index := 0 to length(fArgs) - 1 do
    begin
      fArgs[Index] := TValue.From<Variant>(aArgs[Index].AsVariant);
    end;
    Result := RTTIMethod.Invoke(aClass.Create, fArgs);
  finally
    RTTIContext.Free();
  end;
end;



procedure TCoreTestCase.RunProcedure(aClass: TClass; aMethodName: string; aArgs: TInputDataArray);
var
  RTTIContext: TRTTIContext;
  fArgs: TInputDataArray;
  Index: integer;
begin
  RTTIContext := TRTTIContext.Create;
  try
    SetLength(fArgs, length(aArgs) - 3);
    for Index := 0 to length(fArgs) do
      fArgs[Index] := aArgs[Index];
    RTTIContext.GetType(aClass).GetMethod(aMethodName).Invoke(aClass.Create, fArgs);
  finally
    RTTIContext.Free();
  end;
end;


procedure TCoreTestCase.RunMethod(aClass: TClass; aMethodName: string; aArgs: TInputDataArray;
                                     MethodType: boolean; ExpectedResult: TValue; Operation: string; FailMessageTemplate: string);
var
  ReturnValue: TValue;
begin
  if MethodType = MT_FUNCTION then
  begin
    if Operation = 'except' then
    begin
      StartExpectingException(Exception);
      ReturnValue := RunFunction(aClass, aMethodName, aArgs);
      StopExpectingException('');
      AssertResults(ExpectedResult, ReturnValue, Operation, FailMessageTemplate);
    end
    else
    begin
      try
        ReturnValue := RunFunction(aClass, aMethodName, aArgs);
      except
        Fail('Actual result is exception! Expected result = ' + ExpectedResult.AsString);
      end;
      AssertResults(ExpectedResult, ReturnValue, Operation, FailMessageTemplate);
    end;
  end;
end;

procedure TCoreTestCase.AssertResults(ExpectedResult: TValue; ActualResult: TValue; Operation: string; FailMessageTemplate: string);
var
  AssertionResult: Boolean;
  FailMessageValue: String;
begin
  if Operation = 'except' then
    CheckException(fMethod, Exception, '')
  else
  begin
    if Operation = 'equals' then
      AssertionResult := ActualResult.AsVariant = ExpectedResult.AsVariant;
    if Operation = 'not equals' then
      AssertionResult := ActualResult.AsVariant <> ExpectedResult.AsVariant;
    if Operation = 'larger than' then
      AssertionResult := ActualResult.AsVariant > ExpectedResult.AsVariant;
    if Operation = 'equals or larger than' then
      AssertionResult := ActualResult.AsVariant >= ExpectedResult.AsVariant;
    if Operation = 'less than' then
      AssertionResult := ActualResult.AsVariant < ExpectedResult.AsVariant;
    if Operation = 'equals or less than' then
      AssertionResult := ActualResult.AsVariant <= ExpectedResult.AsVariant;
    if Operation = 'contains' then
      AssertionResult := Pos(VarToStr(ActualResult.AsVariant), VarToStr(ExpectedResult.AsVariant)) > 0;
    if Operation = 'not contains' then
      AssertionResult := Pos(VarToStr(ActualResult.AsVariant), VarToStr(ExpectedResult.AsVariant)) = 0;

    FailMessageValue := StringReplace(FailMessageTemplate, '%r', VarToStr(ActualResult.AsVariant), [rfReplaceAll]);
    FailMessageValue := StringReplace(FailMessageValue, '%o', Operation, [rfReplaceAll]);
    FailMessageValue := StringReplace(FailMessageValue, '%e', VarToStr(ExpectedResult.AsVariant), [rfReplaceAll]);
    if Pos(' not not', FailMessageValue) > 0 then
      FailMessageValue := StringReplace(FailMessageValue, ' not not', '', [rfReplaceAll]);

    Check(AssertionResult, FailMessageValue);
  end;
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

  if IsConsole then
  begin
    {$APPTYPE CONSOLE}
  end;
end; // PrepareToTest


procedure CreateDUnitTests(Suites: TSuiteList; Tests: TTestCaseList; aTestClass: TCoreTestCaseClass);
var
  iSuiteIndex: integer;
  Suite: TCoreTestSuite;
begin
  for iSuiteIndex := 0 to  Length(Suites) - 1 do
  begin
    if Suites [iSuiteIndex].SuiteClassName = aTestClass.ClassName then
    begin
      Suite := TCoreTestSuite.Create(aTestClass.ClassName, Suites[iSuiteIndex].SuiteName, Tests, aTestClass);
      RegisterTest(aTestClass.ClassName, Suite);
    end;
  end;
end;


end.
