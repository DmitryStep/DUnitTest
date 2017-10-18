unit DUnitTestingCore;

interface

uses
  SysUtils, Variants, System.Generics.Collections, System.Generics.Defaults, TestFramework, TestStructureUnit,
  RTTI, Classes, DUnitXMLParser;

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
  public
    constructor Create(TestCase: TTestCaseRec); reintroduce; overload;
    procedure AssertResults<T>(ExpectedResult: T; ActualResult: T; Operation: string; FailMessageTemplate: string);
  end;

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



procedure TCoreTestCase.AssertResults<T>(ExpectedResult: T; ActualResult: T; Operation: string; FailMessageTemplate: string);
var
  AssertionResult: Boolean;
  ActualResultValue: TValue;
  ExpectedResultValue: TValue;
  FailMessageValue: String;
begin
  if Operation = 'except' then
    CheckException(fMethod, Exception, '')
  else
  begin
    ActualResultValue := TValue.From<T>(ActualResult);
    ExpectedResultValue := TValue.From<T>(ExpectedResult);
    if Operation = 'equals' then
      AssertionResult := ActualResultValue.AsVariant = ExpectedResultValue.AsVariant;
    if Operation = 'not equals' then
      AssertionResult := ActualResultValue.AsVariant <> ExpectedResultValue.AsVariant;
    if Operation = 'larger than' then
      AssertionResult := ActualResultValue.AsVariant > ExpectedResultValue.AsVariant;
    if Operation = 'equals or larger than' then
      AssertionResult := ActualResultValue.AsVariant >= ExpectedResultValue.AsVariant;
    if Operation = 'less than' then
      AssertionResult := ActualResultValue.AsVariant < ExpectedResultValue.AsVariant;
    if Operation = 'equals or less than' then
      AssertionResult := ActualResultValue.AsVariant <= ExpectedResultValue.AsVariant;
    if Operation = 'contains' then
      AssertionResult := Pos(VarToStr(ActualResultValue.AsVariant), VarToStr(ExpectedResultValue.AsVariant)) > 0;
    if Operation = 'not contains' then
      AssertionResult := Pos(VarToStr(ActualResultValue.AsVariant), VarToStr(ExpectedResultValue.AsVariant)) = 0;

    FailMessageValue := StringReplace(FailMessageTemplate, '%r', VarToStr(ActualResultValue.AsVariant), [rfReplaceAll]);
    FailMessageValue := StringReplace(FailMessageValue, '%o', Operation, [rfReplaceAll]);
    FailMessageValue := StringReplace(FailMessageValue, '%e', VarToStr(ExpectedResultValue.AsVariant), [rfReplaceAll]);
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
