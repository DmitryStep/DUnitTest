unit DUnitTestingCore;

interface

uses
  SysUtils, Variants, System.Generics.Collections, System.Generics.Defaults, TestFramework, TestStructureUnit, RTTI, Classes;

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
    FTestInstance: TObject;
  public
    constructor Create(TestCase: TTestCaseRec); reintroduce; overload;
    procedure AssertResults<T>(ExpectedResult: T; FactResult: T; Operation: string; FailMessageTemplate: string);
  end;

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


procedure TCoreTestCase.AssertResults<T>(ExpectedResult: T; FactResult: T; Operation: string; FailMessageTemplate: string);
var
  AssertionResult: Boolean;
  FactResultValue: TValue;
  ExpectedResultValue: TValue;
  FailMessageValue: String;
begin
  if Operation = 'except' then
    CheckException(fMethod, Exception, '')
  else
  begin
    FactResultValue := TValue.From<T>(FactResult);
    ExpectedResultValue := TValue.From<T>(ExpectedResult);
    if Operation = 'equals' then
      AssertionResult := FactResultValue.AsVariant = ExpectedResultValue.AsVariant;
    if Operation = 'not equals' then
      AssertionResult := FactResultValue.AsVariant <> ExpectedResultValue.AsVariant;
    if Operation = 'larger than' then
      AssertionResult := FactResultValue.AsVariant > ExpectedResultValue.AsVariant;
    if Operation = 'equals or larger than' then
      AssertionResult := FactResultValue.AsVariant >= ExpectedResultValue.AsVariant;
    if Operation = 'less than' then
      AssertionResult := FactResultValue.AsVariant < ExpectedResultValue.AsVariant;
    if Operation = 'equals or less than' then
      AssertionResult := FactResultValue.AsVariant <= ExpectedResultValue.AsVariant;
    if Operation = 'contains' then
      AssertionResult := Pos(VarToStr(FactResultValue.AsVariant), VarToStr(ExpectedResultValue.AsVariant)) > 0;
    if Operation = 'not contains' then
      AssertionResult := Pos(VarToStr(FactResultValue.AsVariant), VarToStr(ExpectedResultValue.AsVariant)) = 0;

    FailMessageValue := StringReplace(FailMessageTemplate, '%r', VarToStr(FactResultValue.AsVariant), [rfReplaceAll]);
    FailMessageValue := StringReplace(FailMessageValue, '%o', Operation, [rfReplaceAll]);
    FailMessageValue := StringReplace(FailMessageValue, '%e', VarToStr(ExpectedResultValue.AsVariant), [rfReplaceAll]);
    if Pos(' not not', FailMessageValue) > 0 then
      FailMessageValue := StringReplace(FailMessageValue, ' not not', '', [rfReplaceAll]);

    Check(AssertionResult = true, FailMessageValue);
  end;
end;


constructor TCoreTestSuite.Create(aSuitePath: string; aSuiteName: string; Tests: TTestCaseList; aTestClass: TCoreTestCaseClass);
begin
  inherited Create(aSuiteName);
  FSuitePath := aSuitePath;
  FSuiteName := aSuiteName;
  AddTests(Tests, aTestClass);
end;


procedure TCoreTestSuite.AddTests(Tests: TTestCaseList; aTestClass: TCoreTestCaseClass);
var
  TestCaseIndex: integer;
  MethodName: string;
  TestName: string;
  SuiteName: string;

begin
  for TestCaseIndex := 0 to Length(Tests) - 1 do
  begin
    MethodName := Tests[TestCaseIndex].MethodName;
    SuiteName := Tests[TestCaseIndex].SuiteName;
    if SuiteName = '' then
       SuiteName := aTestClass.ClassName;
    TestName := Tests[TestCaseIndex].TestCaseName;
    if TestName = '' then
      TestName := MethodName;
    if (Tests[TestCaseIndex].SuiteName = Self.FSuiteName) and
       (Tests[TestCaseIndex].TestCaseClass = aTestClass.ClassName)
    then
    begin
      Self.AddTest(aTestClass.Create(Tests[TestCaseIndex]) as ITest);
    end;
  end;
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
      Suite := TCoreTestSuite.Create(aTestClass.ClassName, Suites[iSuiteIndex].SuiteName, Tests, aTestClass);
      RegisterTest(aTestClass.ClassName, Suite);
    end;
  end;
end;


end.
