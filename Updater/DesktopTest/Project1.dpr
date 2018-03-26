program Project1;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  uConfigManager in '..\_Include\uConfigManager.pas',
  uDeployManager in '..\_Include\uDeployManager.pas',
  uJenkinsAPI in '..\_Include\uJenkinsAPI.pas',
  uServicesManager in '..\_Include\uServicesManager.pas',
  uShellAPI in '..\_Include\uShellAPI.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
