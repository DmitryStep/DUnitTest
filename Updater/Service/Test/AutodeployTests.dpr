program AutodeployTests;
{

  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options
  to use the console test runner.  Otherwise the GUI test runner will be used by
  default.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  DUnitTestRunner,
  TestuAutodeployService in 'TestuAutodeployService.pas',
  uAutodeployService in '..\uAutodeployService.pas',
  uConfigManager in '..\..\_Include\uConfigManager.pas',
  uDeployManager in '..\..\_Include\uDeployManager.pas',
  uJenkinsAPI in '..\..\_Include\uJenkinsAPI.pas',
  uServicesManager in '..\..\_Include\uServicesManager.pas',
  uShellAPI in '..\..\_Include\uShellAPI.pas',
  uLogManager in '..\..\_Include\uLogManager.pas';

{$R *.RES}

begin
  DUnitTestRunner.RunRegisteredTests;
end.

