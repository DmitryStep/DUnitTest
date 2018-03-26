unit uAutodeployService;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.SvcMgr, Vcl.Dialogs,
  uConfigManager, uDeployManager;

type
  TAutodeployService = class(TService)
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
  private
    { Private declarations }
    FConfigManager: TConfigManager;
    FDeployManager: TDeployManager;
  public
    function GetServiceController: TServiceController; override;
    { Public declarations }
  end;

var
  AutodeployService: TAutodeployService;

implementation

{$R *.DFM}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  AutodeployService.Controller(CtrlCode);
end;

function TAutodeployService.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TAutodeployService.ServiceStart(Sender: TService;
  var Started: Boolean);
begin
  FConfigManager := TConfigManager.Create(ExtractFilePath(ParamStr(0)) +
                                          'Autodeploy.ini');
  if FConfigManager.LoadSettings then
    FDeployManager := TDeployManager.Create(FConfigManager)
  else
  begin
    FConfigManager := nil;
    FDeployManager := nil;
  end;
end;

procedure TAutodeployService.ServiceStop(Sender: TService;
  var Stopped: Boolean);
begin
  FreeAndNil(FDeployManager);
  FreeAndNil(FConfigManager);
end;

end.
