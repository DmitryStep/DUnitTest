unit uServicesManager;

interface

uses
  System.Classes, System.SysUtils, Windows, WinSvc, uShellApi;

type
  TServiceManager = class(TObject)
  private
    FMachineName: string;
  public
    constructor Create;
    destructor Destroy; override;
    function GetLocalName: string;
    function GetServiceState(const aServiceName: string): DWord;
    function StopService(const aServiceName: string): boolean;
    function RunService(const aServiceName: string): boolean;
    function ForseStopService(const aServiceName: string): boolean;
    function ForseStartService(const aServiceName: string): boolean;
  end;


implementation

constructor TServiceManager.Create;
begin
  FMachineName := GetLocalName();
  inherited Create();
end;


destructor TServiceManager.Destroy;
begin
  inherited Destroy;
end;


function TServiceManager.GetLocalName: string;
var
   buf: array[0..MAX_COMPUTERNAME_LENGTH] of char;
   sizebuf: dword;
begin
   GetComputerName(buf,sizebuf);
   Result:=StrPas(buf);
end;


function TServiceManager.GetServiceState(const aServiceName: string): DWord;
var
  h_manager, h_service: SC_Handle;
  service_status: TServiceStatus;
  hStat: DWord;
  h_svc: SC_Handle;
begin
  hStat := 1;
  h_manager := OpenSCManager(PChar(FMachineName) ,nil, SC_MANAGER_CONNECT);
  if h_manager > 0 then
  begin
    h_svc := OpenService(h_manager,PChar(aServiceName), SERVICE_QUERY_STATUS);
    if h_svc > 0 then
    begin
      if(QueryServiceStatus(h_svc, service_status)) then
        hStat := service_status.dwCurrentState;
      CloseServiceHandle(h_svc);
    end;
    CloseServiceHandle(h_manager);
  end;
  Result := hStat;
end;


function TServiceManager.RunService(const aServiceName: string): boolean;
var
  h_manager, h_svc: SC_Handle;
  svc_status: TServiceStatus;
  Temp: PChar;
  dwCheckPoint: DWord;
begin
  svc_status.dwCurrentState := 1;
  h_manager := OpenSCManager(PChar(FMachineName), nil, SC_MANAGER_CONNECT);
  if h_manager > 0 then
  begin
    h_svc := OpenService(h_manager, PChar(aServiceName), SERVICE_START or SERVICE_QUERY_STATUS);
    if h_svc > 0 then
    begin
      temp := nil;
      if (StartService(h_svc, 0, temp)) then
        if (QueryServiceStatus(h_svc, svc_status)) then
          while (SERVICE_RUNNING <> svc_status.dwCurrentState) do
          begin
            dwCheckPoint := svc_status.dwCheckPoint;
            Sleep(svc_status.dwWaitHint);
            if (not QueryServiceStatus(h_svc, svc_status)) or
               (svc_status.dwCheckPoint < dwCheckPoint)
            then
              Break;
          end;
      CloseServiceHandle(h_svc);
    end;
    CloseServiceHandle(h_manager);
  end;
  Result := SERVICE_RUNNING = svc_status.dwCurrentState;
end;


function TServiceManager.StopService(const aServiceName: string): boolean;
var
  h_manager, h_svc: SC_Handle;
  svc_status: TServiceStatus;
  dwCheckPoint: DWord;
begin
  h_manager:=OpenSCManager(PChar(FMachineName), nil, SC_MANAGER_CONNECT);
  if h_manager > 0 then
  begin
    h_svc := OpenService(h_manager, PChar(aServiceName), SERVICE_STOP or SERVICE_QUERY_STATUS);
    if h_svc > 0 then
    begin
      if(ControlService(h_svc, SERVICE_CONTROL_STOP, svc_status))then
        if(QueryServiceStatus(h_svc, svc_status))then
          while(SERVICE_STOPPED <> svc_status.dwCurrentState)do
          begin
            dwCheckPoint := svc_status.dwCheckPoint;
            Sleep(svc_status.dwWaitHint);
            if(not QueryServiceStatus(h_svc, svc_status)) or
              (svc_status.dwCheckPoint < dwCheckPoint)
            then
              Break;
          end;
      CloseServiceHandle(h_svc);
    end;
    CloseServiceHandle(h_manager);
  end;
  Result := SERVICE_STOPPED = svc_status.dwCurrentState;
end;//StopService


function TServiceManager.ForseStopService(const aServiceName: string): boolean;
begin
  ShellExecute(0, '', 'TASKKILL', '/F /IM ' + aServiceName + ' /T', '', SW_HIDE);
  Result := GetLastError = 0;
end;

function TServiceManager.ForseStartService(const aServiceName: string): boolean;
begin
  ShellExecute(0, '', '', aServiceName, '', SW_HIDE);
  Result := GetLastError = 0;
end;


end.
