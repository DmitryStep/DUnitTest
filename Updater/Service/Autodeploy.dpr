program Autodeploy;

uses
  Vcl.SvcMgr,
  uAutodeployService in 'uAutodeployService.pas' {ILSAutodeployService: TService},
  uConfigManager in '..\_Include\uConfigManager.pas',
  uDeployManager in '..\_Include\uDeployManager.pas',
  uJenkinsAPI in '..\_Include\uJenkinsAPI.pas',
  uServicesManager in '..\_Include\uServicesManager.pas',
  uShellAPI in '..\_Include\uShellAPI.pas',
  uLogManager in '..\_Include\uLogManager.pas',
  SysUtils;

{$R *.RES}


var
  WorkParam: string;

begin
  // Windows 2003 Server requires StartServiceCtrlDispatcher to be
  // called before CoRegisterClassObject, which can be called indirectly
  // by Application.Initialize. TServiceApplication.DelayInitialize allows
  // Application.Initialize to be called from TService.Main (after
  // StartServiceCtrlDispatcher has been called).
  //
  // Delayed initialization of the Application object may affect
  // events which then occur prior to initialization, such as
  // TService.OnCreate. It is only recommended if the ServiceApplication
  // registers a class object with OLE and is intended for use with
  // Windows 2003 Server.
  //
  // Application.DelayInitialize := True;
  //
  if not Application.DelayInitialize or Application.Installing then
    Application.Initialize;
  Application.CreateForm(TILSAutodeployService, ILSAutodeployService);
  WorkParam := ILSAutodeployService.GetServiceInstance;
  if ( WorkParam <> '') then
  begin
    ILSAutodeployService.Name := 'ILSAutodeployService_' + WorkParam;
    ILSAutodeployService.DisplayName := 'ILS Autodeploy Service ' + WorkParam;
  end
  else
  begin
    ILSAutodeployService.Name := 'ILSAutodeployService';
    ILSAutodeployService.DisplayName := 'ILS Autodeploy Service ';
  end;
  Application.Run;
end.
