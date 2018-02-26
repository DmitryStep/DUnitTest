program Project1;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  uJenkinsAPI in 'uJenkinsAPI.pas',
  uServicesManager in 'uServicesManager.pas',
  uSettingsManager in 'uSettingsManager.pas',
  uSettingsStructure in 'uSettingsStructure.pas',
  uShellAPI in 'uShellAPI.pas',
  uController in 'uController.pas',
  uBackupManager in 'uBackupManager.pas',
  uUpdateManager in 'uUpdateManager.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
