program Tests;
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
  TestuJenkinsAPI in 'TestuJenkinsAPI.pas',
  uJenkinsAPI in '..\_Include\uJenkinsAPI.pas' {/  uDeployManager in '..\_Include\uDeployManager.pas',},
  TestuServicesManager in 'TestuServicesManager.pas',
  uServicesManager in '..\_Include\uServicesManager.pas',
  TestuConfigManager in 'TestuConfigManager.pas',
  uConfigManager in '..\_Include\uConfigManager.pas',
  TestuDeployManager in 'TestuDeployManager.pas',
  uDeployManager in '..\_Include\uDeployManager.pas',
  uShellAPI in '..\_Include\uShellAPI.pas';

{R *.RES}

begin
  DUnitTestRunner.RunRegisteredTests;
end.

