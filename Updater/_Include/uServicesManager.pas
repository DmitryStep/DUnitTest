unit uServicesManager;

interface

uses
  System.Classes, System.SysUtils, Windows, WinSvc, uShellApi, Tlhelp32, uLogManager;

type
  TServiceManager = class(TObject)
  private
    FMachineName: string;
    FLogger: TLogManager;
  public
    constructor Create; overload;
    constructor Create(const ALogger: TLogManager); overload;
    destructor Destroy; override;
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

const
  {$EXTERNALSYM SERVICE_STOPPED}
  SERVICE_STOPPED                = $00000001;
  {$EXTERNALSYM SERVICE_START_PENDING}
  SERVICE_START_PENDING          = $00000002;
  {$EXTERNALSYM SERVICE_STOP_PENDING}
  SERVICE_STOP_PENDING           = $00000003;
  {$EXTERNALSYM SERVICE_RUNNING}
  SERVICE_RUNNING                = $00000004;
  {$EXTERNALSYM SERVICE_CONTINUE_PENDING}
  SERVICE_CONTINUE_PENDING       = $00000005;
  {$EXTERNALSYM SERVICE_PAUSE_PENDING}
  SERVICE_PAUSE_PENDING          = $00000006;
  {$EXTERNALSYM SERVICE_PAUSED}
  SERVICE_PAUSED                 = $00000007;


implementation

constructor TServiceManager.Create;
begin
  FLogger := nil;
  FMachineName := GetLocalName();
  inherited Create();
end; // Create


constructor TServiceManager.Create(const ALogger: TLogManager);
begin
  FLogger := ALogger;
  FMachineName := GetLocalName();
  inherited Create();
end; // Create


destructor TServiceManager.Destroy;
begin
  FLogger := nil;
end; // Destroy


function TServiceManager.GetLocalName: string;
var
   buf: array[0..MAX_COMPUTERNAME_LENGTH] of char;
   sizebuf: dword;

begin
   GetComputerName(buf,sizebuf);
   Result:=StrPas(buf);
   if Assigned(FLogger) then
     FLogger.WriteDebugMessageToLog('Определено имя компьютера: ' + Result);
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
  if Assigned(FLogger) then
    FLogger.WriteDebugMessageToLog('Определяем список сервисов.');
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
      begin
        AResultList.Add(lpServices[i].lpServiceName);
        if Assigned(FLogger) then
          FLogger.WriteDebugMessageToLog('Добавлен сервис' + lpServices[i].lpServiceName);
      end;
  CloseServiceHandle(SCManagerHandle);
  Result := AResultList.Count;
  if Assigned(FLogger) then
    FLogger.WriteDebugMessageToLog('GetServicesList - количество сервисов = ' + IntToStr(Result));
end; // GetServicesList


function TServiceManager.GetServiceState(const aServiceName: string): DWord;
var
  h_manager: SC_Handle;
  service_status: TServiceStatus;
  hStat: DWord;
  h_svc: SC_Handle;
begin
  if Assigned(FLogger) then
    FLogger.WriteDebugMessageToLog('GetServiceState - определяем статус сервиса ' +
                                    aServiceName);
  if aServiceName = '' then
  begin
    Result := 0;
    Exit;
  end;
  hStat := 1;
  try
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
  except
    on E: Exception do
      if Assigned(FLogger) then
        FLogger.WriteErrorMessageToLog('Ошибка получения состояния сервиса ' +
                                       aServiceName, E.Message, E.StackTrace);
  end;
  Result := hStat;
  if Assigned(FLogger) then
    FLogger.WriteDebugMessageToLog('GetServiceState - статус сервиса ' +
                                   aServiceName + ' = ' + IntToStr(Result));
end; // GetServiceState


function TServiceManager.RunService(const aServiceName: string): boolean;
var
  h_manager, h_svc: SC_Handle;
  svc_status: TServiceStatus;
  Temp: PChar;
  dwCheckPoint: DWord;
begin
  if Assigned(FLogger) then
    FLogger.WriteDebugMessageToLog('RunService - запуск сервиса ' +
                                   aServiceName);
  if aServiceName = '' then
  begin
    Result := false;
    Exit;
  end;
  try
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
  except
    on E: Exception do
      if Assigned(FLogger) then
        FLogger.WriteErrorMessageToLog('Ошибка запуска сервиса ' + aServiceName,
                                       E.Message, E.StackTrace);
  end;
  Result := SERVICE_RUNNING = svc_status.dwCurrentState;
  if Assigned(FLogger) then
    FLogger.WriteDebugMessageToLog('RunService - результат запуска сервиса ' +
                                   aServiceName + ' = ' + BoolToStr(Result));
end; // RunService


function TServiceManager.StopService(const aServiceName: string): boolean;
var
  h_manager, h_svc: SC_Handle;
  svc_status: TServiceStatus;
  dwCheckPoint: DWord;
begin
  if Assigned(FLogger) then
    FLogger.WriteDebugMessageToLog('StopService - остановка сервиса ' +
                                   aServiceName);
  if aServiceName = '' then
  begin
    Result := false;
    if Assigned(FLogger) then
      FLogger.WriteDebugMessageToLog('Пустое имя сервиса! Выход из функции.');
    Exit;
  end;
  try
    h_manager:=OpenSCManager(PChar(FMachineName), nil, SC_MANAGER_CONNECT);
    if h_manager > 0 then
    begin
      h_svc := OpenService(h_manager, PChar(aServiceName), SERVICE_STOP or SERVICE_QUERY_STATUS);
      if h_svc > 0 then
      begin
        if (ControlService(h_svc, SERVICE_CONTROL_STOP, svc_status)) then
          if QueryServiceStatus(h_svc, svc_status) then
            while (SERVICE_STOPPED <> svc_status.dwCurrentState) do
            begin
              dwCheckPoint := svc_status.dwCheckPoint;
              Sleep(svc_status.dwWaitHint);
              if not QueryServiceStatus(h_svc, svc_status) or
                (svc_status.dwCheckPoint < dwCheckPoint)
              then
                Break;
            end;
        CloseServiceHandle(h_svc);
      end;
      CloseServiceHandle(h_manager);
    end;
  except
    on E: Exception do
      if Assigned(FLogger) then
        FLogger.WriteErrorMessageToLog('Ошибка остановки сервиса ' + aServiceName,
                                       E.Message, E.StackTrace);
  end;
  Result := SERVICE_STOPPED = svc_status.dwCurrentState;
  if Assigned(FLogger) then
    FLogger.WriteDebugMessageToLog('StopService - результат остановки сервиса ' +
                                   aServiceName + ' = ' + BoolToStr(Result));
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
  if Assigned(FLogger) then
    FLogger.WriteDebugMessageToLog('KillTask - остановка процесса ' +
                                   aTaskFileName);
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
    if Assigned(FLogger) then
      FLogger.WriteDebugMessageToLog('KillTask - результат остановки процесса ' +
                                     aTaskFileName + ' = ' + IntToStr(Result));
  end
  else
    if Assigned(FLogger) then
      FLogger.WriteDebugMessageToLog('KillTask - пустое имя процесса! Выход. ');
end; // KillProcess


function TServiceManager.StartTask(const aTaskFileName: string): boolean;
begin
  Result := true;
  if Assigned(FLogger) then
    FLogger.WriteDebugMessageToLog('StartTask - запуск процесса ' +
                                   aTaskFileName);
  if ATaskFileName <> '' then
    WinExec(aTaskFileName, SW_HIDE, false)
  else
  begin
    if Assigned(FLogger) then
      FLogger.WriteDebugMessageToLog('StartTask - пустое имя процесса! Выход.');
    Exit;
  end;
  Result := GetLastError <> 0;
  if Assigned(FLogger) then
    FLogger.WriteDebugMessageToLog('StartTask - результат запуска процесса ' +
                                   aTaskFileName + ' = ' + BoolToStr(Result) +
                                   ' (GetLastError = ' + IntToStr(GetLastError) +
                                   ').');
end; // StartProcess


end.
