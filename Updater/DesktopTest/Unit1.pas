unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, uConfigManager, uDeployManager, uLogManager,
  uServicesManager;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    FConfigManager: TConfigManager;
    FDelployManager: TDeployManager;
    FLogManager: TLogManager;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  FConfigManager := TConfigManager.Create(ExtractFilePath(Application.ExeName) +
                                          'testconfig.ini');
  if FConfigManager.LoadSettings then
  begin
    FLogManager := TLogManager.Create(FConfigManager.GlobalSettings.LoggerSettings.s_LogDir,
                                      FConfigManager.GlobalSettings.LoggerSettings.s_LogFileName,
                                      FConfigManager.GlobalSettings.LoggerSettings.b_IsUseDataInLogFilename,
                                      FConfigManager.GlobalSettings.LoggerSettings.s_LogDetalization,
                                      FConfigManager.GlobalSettings.LoggerSettings.s_IsUseErrorFile);
    FDelployManager := TDeployManager.Create(FConfigManager, FLogManager);
    FDelployManager.Deploy;
    FreeAndNil(FDelployManager);
    FreeAndNil(FLogManager);
  end;
  FreeAndNil(FConfigManager);
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  ServMan: TServiceManager;
begin
  ServMan := TServiceManager.Create;
  ServMan.RunService('ILSAutodeployService');
//  ServMan.RunService('MDM');
  FreeAndNil(ServMan);
end;

end.
