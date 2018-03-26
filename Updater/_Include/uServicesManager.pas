unit uServicesManager;

interface

uses
  System.Classes, System.SysUtils, Windows, WinSvc, uShellApi, Tlhelp32;

type
  TServiceManager = class(TObject)
  private
    FMachineName: string;
  public
    constructor Create;
    function GetLocalName: string;

    // Возвращает список служб, содержащих в имени заданное значение
    function GetServicesList(var AResultList: TStringList;
                              const AServiceNamePart: string): integer;

    // Возвращает состояние заданной службы
    function GetServiceState(const aServiceName: string): DWord;

    // Останавливает службу
    function StopService(const aServiceName: string): boolean;

    // Запускает службу
    function RunService(const aServiceName: string): boolean;

    // Убивает таску в памяти
    function KillTask(const aTaskFileName: string): Integer;

    // Запускает файл
    function StartTask(const aTaskFileName: string): boolean;
  end;


implementation

constructor TServiceManager.Create;
begin
  FMachineName := GetLocalName();
  inherited Create();
end; // Create


function TServiceManager.GetLocalName: string;
var
   buf: array[0..MAX_COMPUTERNAME_LENGTH] of char;
   sizebuf: dword;

begin
   GetComputerName(buf,sizebuf);
   Result:=StrPas(buf);
end; // GetLocalName


function TServiceManager.GetServicesList(var AResultList: TStringList;
                                         const AServiceNamePart: string): integer;
var
  SCManagerHandle: THandle;
  ServiceMode: integer;
  ServiceStatus: integer;
  pcbBytesNeeded: DWORD;
  lpServicesReturned: DWORD;
  lpResumeHandle: DWORD;
  i: DWORD;
  lpServices: array of TEnumServiceStatus;
begin
  Result := 0;
  SCManagerHandle := OpenSCManager(PChar(FMachineName), Nil, GENERIC_READ);
  if SCManagerHandle = 0 then Exit;
  ServiceMode := SERVICE_WIN32;
  ServiceStatus := SERVICE_STATE_ALL;
  lpResumeHandle := 0;
  EnumServicesStatus(SCManagerHandle,
                     ServiceMode,
                     ServiceStatus,
                     lpServices[0],
                     0,
                     pcbBytesNeeded,
                     lpServicesReturned,
                     lpResumeHandle);
  SetLength(lpServices, pcbBytesNeeded div SizeOf(TEnumServiceStatus) + 1);
  lpResumeHandle := 0;
  EnumServicesStatus(SCManagerHandle,
                     ServiceMode,
                     ServiceStatus,
                     lpServices[0],
                     Length(lpServices) * SizeOf(TEnumServiceStatus),
                     pcbBytesNeeded,
                     lpServicesReturned,
                     lpResumeHandle);
  if lpServicesReturned > 0 then
    for i := 0 to lpServicesReturned - 1 do
      if pos(AServiceNamePart, lpServices[i].lpServiceName) > 0 then
        AResultList.Add(lpServices[i].lpServiceName);
  CloseServiceHandle(SCManagerHandle);
  Result := AResultList.Count;
end; // GetServicesList


function TServiceManager.GetServiceState(const aServiceName: string): DWord;
var
  h_manager, h_service: SC_Handle;
  service_status: TServiceStatus;
  hStat: DWord;
  h_svc: SC_Handle;
begin
  if aServiceName = '' then Exit;
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
end; // GetServiceState


function TServiceManager.RunService(const aServiceName: string): boolean;
var
  h_manager, h_svc: SC_Handle;
  svc_status: TServiceStatus;
  Temp: PChar;
  dwCheckPoint: DWord;
begin
  if aServiceName = '' then Exit;
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
end; // RunService


function TServiceManager.StopService(const aServiceName: string): boolean;
var
  h_manager, h_svc: SC_Handle;
  svc_status: TServiceStatus;
  dwCheckPoint: DWord;
begin
  if aServiceName = '' then Exit;
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
end; // StopService


function TServiceManager.KillTask(const aTaskFileName: string): Integer;
const
  PROCESS_TERMINATE = $0001;
var
  ContinueLoop: BOOL;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
  FExeName: string;
  ExePos: integer;
begin
  ExePos := pos('.exe', aTaskFileName);
  if ExePos > 0 then
    FExeName := Copy(aTaskFileName, 1, ExePos + 3);
  Result := 0;
  if aTaskFileName <> '' then
  begin
    FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    FProcessEntry32.dwSize := SizeOf(FProcessEntry32);
    ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);
    while Integer(ContinueLoop) <> 0 do
    begin
      if (UpperCase(ExtractFileName(FProcessEntry32.szExeFile)) = UpperCase(FExeName))
          or (UpperCase(FProcessEntry32.szExeFile) = UpperCase(FExeName))
      then
        Result := Integer(TerminateProcess(
      OpenProcess(PROCESS_TERMINATE, BOOL(0), FProcessEntry32.th32ProcessID), 0));
      ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
    end;
    CloseHandle(FSnapshotHandle);
  end;
end; // KillProcess


function TServiceManager.StartTask(const aTaskFileName: string): boolean;
begin
  if ATaskFileName <> '' then
    WinExec(aTaskFileName, SW_HIDE);
  Result := GetLastError = 0;
end; // StartProcess


end.
