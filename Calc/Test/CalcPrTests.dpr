program CalcPrTests;
{

  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options
  to use the console test runner.  Otherwise the GUI test runner will be used by
  default.

}

//{$DEFINE CONSOLE_TESTRUNNER}
{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  System.SysUtils,
  DUnitTestRunner,
  DUnitXMLParser in 'DUnitXMLParser.pas',
  TestStructureUnit in 'TestStructureUnit.pas',
  DUnitTestingCore in 'DUnitTestingCore.pas',
  TestCalcLogic in 'TestCalcLogic.pas',
  CalcLogic in '..\CalcLogic.pas';

{$R *.RES}

var
  FileName: string;

begin
  if IsConsole then
    FileName := ParamStr(1)
  else
    FileName := ExtractFilePath(ParamStr(0)) + 'tests.xml';

  LoadTestsFromXML(FileName, SuiteList, TestCaseList);

  CreateDUnitTests(SuiteList, TestCaseList, TestTCalcLogic);
  CreateDUnitTests(SuiteList, TestCaseList, TestTCalcLogic1);

  DUnitTestRunner.RunRegisteredTests;

  if IsConsole then ReadLn;
end.

