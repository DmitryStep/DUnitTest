unit uDeployManager;

interface

uses
  uConfigManager, uJenkinsAPI, SysUtils, VCL.ExtCtrls, Classes, uShellAPI,
  uServicesManager, Forms, IOUtils, IniFiles, uLogManager;

const
  cConnectionStringTemplate = 'Provider=SQLOLEDB.1;' +
                              'Password=<PASSWORD>;' +
                              'Persist Security Info=True;' +
                              'User ID=<USER>;' +
                              'Initial Catalog=<DBNAME>;' +
                              'Data Source=<SERVER>';

type
  TDeployManager = class
  private
    FConfigManager: TConfigManager;
    FJenkins: array of TJenkinsAPI;
    FBuildState: TBuildState;
    FProgramsCount: integer;
    FCheckTimer: TTimer;
    FTempReleaseDir: string;
    FTempBackupDir: string;
    FLoggerDir: string;
    FArchiveDir: string;
    FLogger: TLogManager;

    procedure CreateDirectories(const AProgramIndex: integer);
    procedure DeleteDirectories;
    function DownloadArtifactFiles(const AProgramIndex: integer): string;
    procedure PackProgram(const AProgramIndex: integer;
                                     const ACurrentBuild: string);
    procedure UnpackProgram(const AProgramIndex: integer;
                           const AArchFileName: string);
    function StopAllServicesAndTasks(const AProgramIndex: integer): boolean;
    function StartAllServicesAndTasks(const AProgramIndex: integer): boolean;
    function BackupProgram(const AProgramIndex: integer): boolean;
    function RestoreProgram(const AProgramIndex: integer): boolean;
    function CopyProgram(const AProgramIndex: integer): boolean;
    function UpdateDatabase(const AProgramIndex: integer): boolean;

    procedure OnTimer(Sender: TObject);

    // Формируем строку соединения с базой по данным из конфига
    function GetConnectionString(const AProgramIndex: integer): string;

    // Создание ini-файла для DBUpdater
    procedure SetIniFileForDBUpdater(const AProgramIndex: integer);

    procedure DeployProgram(const AProgramIndex: integer);

  public
    constructor Create(const AConfigManager: TConfigManager;
                       const ALogger: TLogManager);
    destructor Destroy; override;

    function GetLastBuildNumber(const AProgramIndex: integer): string;
    function CheckNewRelease(const AProgramIndex: integer): string;
    function CheckNewBuild(const AProgramIndex: integer): string;
    function CheckAllReleases: boolean;
    function CheckAllBuilds: boolean;

    procedure Deploy;
  end;

implementation


procedure TDeployManager.CreateDirectories(const AProgramIndex: integer);
begin
  try
    FLogger.WriteInfoMessageToLog('Создание директорий для проекта ' +
                                  FConfigManager.Programs[AProgramIndex].s_ProgramName + ':');
    TDirectory.CreateDirectory(FConfigManager.GlobalSettings.s_TempDir);
    FLogger.WriteMessageToLog(FConfigManager.GlobalSettings.s_TempDir);
    TDirectory.CreateDirectory(FTempReleaseDir);
    FLogger.WriteMessageToLog(FTempReleaseDir);
    TDirectory.CreateDirectory(FTempBackupDir);
    FLogger.WriteMessageToLog(FTempBackupDir);
    TDirectory.CreateDirectory(FTempReleaseDir +
                               FConfigManager.Programs[AProgramIndex].s_ProgramName);
    FLogger.WriteMessageToLog(FTempReleaseDir +
                               FConfigManager.Programs[AProgramIndex].s_ProgramName);
    TDirectory.CreateDirectory(FTempBackupDir +
                               FConfigManager.Programs[AProgramIndex].s_ProgramName);
    FLogger.WriteMessageToLog(FTempBackupDir +
                              FConfigManager.Programs[AProgramIndex].s_ProgramName);
  except
    on E: Exception do
      FLogger.WriteErrorMessageToLog('Ошибка при создании директорий!',
                                     E.Message, E.StackTrace);
  end;
end; // CreateDirectories

// ----------------------------------------------------------------------------------------------------------------------------------

procedure TDeployManager.DeleteDirectories;
begin
  try
    TDirectory.Delete(FConfigManager.GlobalSettings.s_TempDir ,true);
    FLogger.WriteDebugMessageToLog('Удаление директорий.');
  except
    on E: Exception do
      FLogger.WriteErrorMessageToLog('Ошибка при удалении директорий!',
                                     E.Message, E.StackTrace);
  end;
end; // DeleteDirectories

// ----------------------------------------------------------------------------------------------------------------------------------

function TDeployManager.DownloadArtifactFiles(const AProgramIndex: integer): string;
var
  sl_ArtifactsList: TStringList;
  s_FileURL: string;
  s_FileName: string;
  i: integer;
begin
  FLogger.WriteInfoMessageToLog('Скачивание артефактов.');
  sl_ArtifactsList := TStringList.Create();
  try
    try
      if FJenkins[AProgramIndex].GetArtifactsList(sl_ArtifactsList) > 0 then
      begin
        if FConfigManager.Programs[AProgramIndex].b_IsZipPacked then
        begin
          FLogger.WriteInfoMessageToLog('Обнаружен признак архива. Скачиваем только первый артефакт из списка!');
          FLogger.WriteInfoMessageToLog('Количество артефактов: ' + IntToStr(sl_ArtifactsList.Count));
          s_FileURL := sl_ArtifactsList.Strings[0];
          FLogger.WriteMessageToLog('URL артефакта: ' + s_FileURL);
          s_FileName := FJenkins[AProgramIndex].GetFileNameFromURL(s_FileURL);
          FLogger.WriteMessageToLog('Имя файла артефакта: ' + s_FileName);
          Result := FTempReleaseDir + s_FileName;
          FJenkins[AProgramIndex].GetFile(s_FileURL, Result);
          FLogger.WriteInfoMessageToLog('Закачка завершена!');
        end
        else
        begin
          FLogger.WriteInfoMessageToLog('Признак архива не обнаружен. Скачиваем все артефакты из списка!');
          FLogger.WriteInfoMessageToLog('Количество артефактов: ' + IntToStr(sl_ArtifactsList.Count));
          for i := 0 to sl_ArtifactsList.Count - 1 do
          begin
            s_FileURL := sl_ArtifactsList.Strings[i];
            FLogger.WriteMessageToLog('URL артефакта: ' + s_FileURL);
            s_FileName := FJenkins[AProgramIndex].GetFileNameFromURL(s_FileURL);
            FLogger.WriteMessageToLog('Имя файла артефакта: ' + s_FileName);
            FJenkins[AProgramIndex].GetFile(s_FileURL, FTempReleaseDir +
                                            '\' + s_FileName);
            Result := '';
          end;
          FLogger.WriteInfoMessageToLog('Закачка завершена!');
        end;
      end;
    except
      on E: Exception do
      begin
        FLogger.WriteErrorMessageToLog('Ошибка при скачивании артефактов!',
                                       E.Message, E.StackTrace);
        Result := '';
      end;
    end;
  finally
    FreeAndNil(sl_ArtifactsList);
  end;
end; // DownloadArtifactFiles

// ----------------------------------------------------------------------------------------------------------------------------------

procedure TDeployManager.PackProgram(const AProgramIndex: integer;
                                     const ACurrentBuild: string);
var
  s_CurrentRelease: string;
begin
  s_CurrentRelease := StringReplace(FConfigManager.GlobalSettings.s_CurrentRelease, '.', '', [rfReplaceAll]);
  FLogger.WriteInfoMessageToLog('Упаковка предыдущей версии ' +
                                 FConfigManager.Programs[AProgramIndex].s_ProgramName +
                                 ' в ' + FConfigManager.GlobalSettings.s_ArchiveDir + '\' +
                                 FConfigManager.Programs[AProgramIndex].s_ProgramName + '_' +
                                 s_CurrentRelease + '_' + ACurrentBuild + '.zip');
  ArchiveFiles(FTempBackupDir +
               FConfigManager.Programs[AProgramIndex].s_ProgramName,
               FConfigManager.GlobalSettings.s_ArchiveDir + '\' +
               FConfigManager.Programs[AProgramIndex].s_ProgramName + '_' +
               s_CurrentRelease + '_' + ACurrentBuild + '.zip'
              );
end; // PackProgram

// ----------------------------------------------------------------------------------------------------------------------------------

procedure TDeployManager.UnpackProgram(const AProgramIndex: integer;
                                      const AArchFileName: string);
begin
  FLogger.WriteInfoMessageToLog('Распаковка ' + AArchFileName + ' в ' +
                                FTempReleaseDir +
                                FConfigManager.Programs[AProgramIndex].s_ProgramName);
  UnarchiveFiles(AArchFileName,
                 FTempReleaseDir +
                 FConfigManager.Programs[AProgramIndex].s_ProgramName
                );
end; // UnpackProgram

// ----------------------------------------------------------------------------------------------------------------------------------

function TDeployManager.StopAllServicesAndTasks(const AProgramIndex: integer): boolean;
var
  i, j: integer;
  ServiceManager: TServiceManager;
  ServicesList: TStringList;
begin
  Result := true;
  ServicesList := TStringList.Create;
  ServiceManager := TServiceManager.Create(FLogger);
  FLogger.WriteDebugMessageToLog('Создан объект TServiceManager.');
  FLogger.WriteInfoMessageToLog('Остановка сервисов и процессов.');
  for i := 0 to FConfigManager.Programs[AProgramIndex].sl_ProgramServices.Count - 1 do
  begin
    ServiceManager.GetServicesList(ServicesList,
                                   FConfigManager.Programs[AProgramIndex]
                                                 .sl_ProgramServices
                                                 .Strings[i]);
    for j := 0 to ServicesList.Count - 1 do
    begin
      ServiceManager.StopService(ServicesList.Strings[j]);
      Result := ServiceManager.GetServiceState(ServicesList.Strings[j]) = SERVICE_STOPPED;
      if not Result then
      begin
        FLogger.WriteMessageToLog(ServicesList.Strings[j] + ' - NOT STOPPED!' );
        Exit
      end
      else
        FLogger.WriteMessageToLog(ServicesList.Strings[j] + ' - STOPPED!' );
    end;
  end;
  for i := 0 to FConfigManager.Programs[AProgramIndex].sl_ProgramTasks.Count - 1 do
  begin
    Result := ServiceManager.KillTask(FConfigManager.Programs[AProgramIndex]
                                                    .sl_ProgramTasks
                                                    .Strings[i]) = 0;
    if not Result then
    begin
      FLogger.WriteMessageToLog(FConfigManager.Programs[AProgramIndex]
                                              .sl_ProgramTasks
                                              .Strings[i] + ' - NOT STOPPED!');
      Exit;
    end
    else
      FLogger.WriteMessageToLog(FConfigManager.Programs[AProgramIndex]
                                              .sl_ProgramTasks
                                              .Strings[i] + ' - STOPPED!');
  end;
  FreeAndNil(ServiceManager);
  FreeAndNil(ServicesList);
  FLogger.WriteInfoMessageToLog('Завершение остановки сервисов и процессов.');
  FLogger.WriteDebugMessageToLog('Удалён объект TServiceManager.');
end; // StopAllServicesAndTasks

// ----------------------------------------------------------------------------------------------------------------------------------

function TDeployManager.StartAllServicesAndTasks(const AProgramIndex: integer): boolean;
var
  i, j: integer;
  ServiceManager: TServiceManager;
  ServicesList: TStringList;
begin
  Result := true;
  ServicesList := TStringList.Create;
  ServiceManager := TServiceManager.Create(FLogger);
  FLogger.WriteDebugMessageToLog('Создан объект TServiceManager.');
  FLogger.WriteInfoMessageToLog('Запуск сервисов и процессов');
  for i := 0 to FConfigManager.Programs[AProgramIndex].sl_ProgramServices.Count - 1 do
  begin
    ServiceManager.GetServicesList(ServicesList,
                                   FConfigManager.Programs[AProgramIndex]
                                                 .sl_ProgramServices
                                                 .Strings[i]);
    for j := 0 to ServicesList.Count - 1 do
    begin
      ServiceManager.RunService(ServicesList.Strings[j]);
      Result := ServiceManager.GetServiceState(ServicesList.Strings[j]) = SERVICE_RUNNING;
      if not Result then
      begin
        FLogger.WriteMessageToLog(ServicesList.Strings[j] + ' - NOT RUNNING!' );
        Exit
      end
      else
        FLogger.WriteMessageToLog(ServicesList.Strings[j] + ' - RUNNING!' );
    end;
  end;
  for i := 0 to FConfigManager.Programs[AProgramIndex].sl_ProgramTasks.Count - 1 do
  begin
    Result := ServiceManager.StartTask(FConfigManager.Programs[AProgramIndex]
                                                     .s_DestinationDir + '\' +
                                       FConfigManager.Programs[AProgramIndex]
                                                     .sl_ProgramTasks
                                                     .Strings[i]);
    if not Result then
    begin
      FLogger.WriteMessageToLog(FConfigManager.Programs[AProgramIndex]
                                              .sl_ProgramTasks
                                              .Strings[i] + ' - NOT STOPPED!');
      Exit;
    end
    else
      FLogger.WriteMessageToLog(FConfigManager.Programs[AProgramIndex]
                                              .sl_ProgramTasks
                                              .Strings[i] + ' - STOPPED!');
  end;
  FreeAndNil(ServiceManager);
  FreeAndNil(ServicesList);
  FLogger.WriteInfoMessageToLog('Завершение запуска сервисов и процессов.');
  FLogger.WriteDebugMessageToLog('Удалён объект TServiceManager.');
end; // StartAllServicesAndTasks

// ----------------------------------------------------------------------------------------------------------------------------------

function TDeployManager.BackupProgram(const AProgramIndex: integer): boolean;
begin
  FLogger.WriteInfoMessageToLog('Бэкап программы ' + FConfigManager.Programs[AProgramIndex].s_DestinationDir +
                                 '\*.* в ' +
                                 FTempBackupDir +
                                 FConfigManager.Programs[AProgramIndex].s_ProgramName +
                                 '\*.*');
  CopyFiles(FConfigManager.Programs[AProgramIndex].s_DestinationDir + '\*.*',
            FTempBackupDir +
            FConfigManager.Programs[AProgramIndex].s_ProgramName + '\*.*');
  Result := GetLastError <> 0;
end; // BackupProgram

// ----------------------------------------------------------------------------------------------------------------------------------

function TDeployManager.RestoreProgram(const AProgramIndex: integer): boolean;
begin
  FLogger.WriteInfoMessageToLog('Восстановление программы ' + FTempBackupDir +
                                 FConfigManager.Programs[AProgramIndex].s_ProgramName +
                                 '\*.* в ' +
                                 FConfigManager.Programs[AProgramIndex].s_DestinationDir + '*.*');
  CopyFiles(FTempBackupDir +
            FConfigManager.Programs[AProgramIndex].s_ProgramName + '\*.*',
            FConfigManager.Programs[AProgramIndex].s_DestinationDir + '*.*');
  Result := GetLastError <> 0;
end; // RestoreProgram

// ----------------------------------------------------------------------------------------------------------------------------------

function TDeployManager.CopyProgram(const AProgramIndex: integer): boolean;
var
  i: integer;
  s_FileMask: string;
  s_Source: string;
  p: integer;
begin
  Result := true;
  for i := 0 to FConfigManager.Programs[AProgramIndex].sl_ProgramFiles.Count - 1 do
  begin
    s_Source := FConfigManager.Programs[AProgramIndex].sl_ProgramFiles.Strings[i];
    p := pos('..\', s_Source);
    s_FileMask := '';
    while p > 0 do
    begin
      s_FileMask := s_FileMask + '..\';
      Delete(s_Source, p, 3);
      p := pos('..\', s_Source);
    end;
    s_FileMask := '\' + s_FileMask + '*.*';
    FLogger.WriteInfoMessageToLog('Копирование программы ' + FTempReleaseDir +
                                   FConfigManager.Programs[AProgramIndex].s_ProgramName +
                                   '\' + s_Source + ' в ' +
                                   FConfigManager.Programs[AProgramIndex].s_DestinationDir +
                                   s_FileMask);
    CopyFiles(FTempReleaseDir +
              FConfigManager.Programs[AProgramIndex].s_ProgramName + '\' +
              s_Source,
              FConfigManager.Programs[AProgramIndex].s_DestinationDir + s_FileMask);
    Result := GetLastError <> 0;
    if not Result then
      Exit;
  end;
end; // CopyProgram

// ----------------------------------------------------------------------------------------------------------------------------------

function TDeployManager.GetConnectionString(const AProgramIndex: integer): string;
begin
  Result := StringReplace(cConnectionStringTemplate,
                          '<USER>',
                          FConfigManager.Programs[AProgramIndex].Database.s_DatabaseUser,
                          [rfReplaceAll]);
  Result := StringReplace(Result,
                          '<PASSWORD>',
                          FConfigManager.Programs[AProgramIndex].Database.s_DatabasePass,
                          [rfReplaceAll]);
  Result := StringReplace(Result,
                          '<DBNAME>',
                          FConfigManager.Programs[AProgramIndex].Database.s_DatabaseName,
                          [rfReplaceAll]);
  Result := StringReplace(Result,
                          '<SERVER>',
                          FConfigManager.Programs[AProgramIndex].Database.s_DatabaseServer,
                          [rfReplaceAll]);
end; // GetConnectionString

// ----------------------------------------------------------------------------------------------------------------------------------

procedure TDeployManager.SetIniFileForDBUpdater(const AProgramIndex: integer);
var
  DBUpdaterIni: TIniFile;
  s_ConnectionString: string;
begin
  FLogger.WriteInfoMessageToLog('Создаём ini для апдейтера БД с параметрами: ');
  s_ConnectionString := GetConnectionString(AProgramIndex);
  FLogger.WriteMessageToLog('ConnectionString = ' + s_ConnectionString);
  FLogger.WriteMessageToLog('ScriptsDir = ' + FTempReleaseDir + 'Scripts');
  DBUpdaterIni := TIniFile.Create(ExtractFilePath(FConfigManager
                                                  .GlobalSettings
                                                  .s_DBUpdaterDir) +
                                  '\Updater.ini');
  DBUpdaterIni.WriteString('ConnectionSettings',
                           'ConnectionString',
                           s_ConnectionString);
  DBUpdaterIni.WriteInteger('ConnectionSettings',
                            'ConnectionTimeout',
                            30);
  DBUpdaterIni.WriteString('Scripts',
                           'ScriptsFolder',
                           FTempReleaseDir + 'Scripts');
  FLogger.WriteInfoMessageToLog('Путь до ini: ' +
                                ExtractFilePath(FConfigManager.GlobalSettings.s_DBUpdaterDir) +
                                '\Updater.ini');
  FreeAndNil(DBUpdaterIni);
end; // SetIniFileForDBUpdater

// ----------------------------------------------------------------------------------------------------------------------------------

function TDeployManager.UpdateDatabase(const AProgramIndex: integer): boolean;
var
  s_CmdLine: string;
begin
  FLogger.WriteInfoMessageToLog('Апдейт БД');
  SetIniFileForDBUpdater(AProgramIndex);
  s_CmdLine := FConfigManager.GlobalSettings.s_DBUpdaterDir + ' -c i -i "' +
               FConfigManager.GlobalSettings.s_DBUpdaterDir + '\Updater.ini"';
  FLogger.WriteDebugMessageToLog('s_CmdLine = ' + s_CmdLine);
  WinExec(s_CmdLine, SW_HIDE);
  Result := GetLastError <> 0;
end; // UpdateDatabase

// ----------------------------------------------------------------------------------------------------------------------------------

constructor TDeployManager.Create(const AConfigManager: TConfigManager;
                                  const ALogger: TLogManager);
var
  i: integer;
begin
  FLogger := ALogger;
  FConfigManager := AConfigManager;
  FTempReleaseDir := FConfigManager.GlobalSettings.s_TempDir + '\Release\';
  FTempBackupDir := FConfigManager.GlobalSettings.s_TempDir + '\BackUp\';
  FLoggerDir := FConfigManager.GlobalSettings.LoggerSettings.s_LogDir;
  FArchiveDir := FConfigManager.GlobalSettings.s_ArchiveDir;
  if FConfigManager.GlobalSettings.b_IsArchiveOld then
    TDirectory.CreateDirectory(FArchiveDir);
  if Assigned(AConfigManager) then
  begin
    FProgramsCount := length(FConfigManager.Programs);
    FLogger.WriteDebugMessageToLog('Определено количество проектов: ' +
                                   IntToStr(FProgramsCount));
    SetLength(FJenkins, FProgramsCount);
    if FConfigManager.GlobalSettings.b_UseStableBuilds then
      FBuildState := bsStable
    else
      FBuildState := bsAll;
    if FConfigManager.GlobalSettings.i_CheckPeriod > 0 then
    begin
      FCheckTimer := TTimer.Create(nil);
      FCheckTimer.Interval := FConfigManager.GlobalSettings.i_CheckPeriod * 1000;
      FCheckTimer.OnTimer := OnTimer;
      FCheckTimer.Enabled := true;
      FLogger.WriteDebugMessageToLog('Создан объект TTimer');
      FLogger.WriteInfoMessageToLog('Запуск таймера с интервалом проверки ' +
                                     IntToStr(FConfigManager.GlobalSettings.i_CheckPeriod) +
                                     ' секунд');
    end
    else
    begin
      FLogger.WriteInfoMessageToLog('Настройки таймера не установлены. Таймер не запущен!');
      FCheckTimer := nil;
    end;
    for i := 0 to FProgramsCount - 1 do
    begin
      FLogger.WriteDebugMessageToLog('Создан объект TJenkinsAPI для проекта ' + IntToStr(i));
      FJenkins[i] := TJenkinsAPI.Create(FConfigManager.Programs[i].s_SourceURL,
                                        FConfigManager.Programs[i].s_SourceReleaseDir,
                                        FConfigManager.Programs[i].s_SourceUser,
                                        FConfigManager.Programs[i].s_SourcePass,
                                        FConfigManager.GlobalSettings.ProxySettings.s_ProxyHost,
                                        FConfigManager.GlobalSettings.ProxySettings.i_ProxyPort,
                                        FConfigManager.GlobalSettings.ProxySettings.s_ProxyUser,
                                        FConfigManager.GlobalSettings.ProxySettings.s_ProxyPass,
                                        FBuildState,
                                        FLogger);
      CreateDirectories(i);
    end;
  end;
end; // Create

// ----------------------------------------------------------------------------------------------------------------------------------

destructor TDeployManager.Destroy;
var
  i: integer;
begin
  FreeAndNil(FCheckTimer);
  FLogger.WriteDebugMessageToLog('Уничтожен объект TTimer');
  for i := 0 to FProgramsCount - 1 do
  begin
    FreeAndNil(FJenkins[i]);
    FLogger.WriteDebugMessageToLog('Уничтожен объект TJenkinsAPI для проекта ' + IntToStr(i));
  end;
  SetLength(FJenkins, 0);
  DeleteDirectories;
  FConfigManager := nil;
  FLogger := nil;
end; // Destroy

// ----------------------------------------------------------------------------------------------------------------------------------

procedure TDeployManager.DeployProgram(const AProgramIndex: integer);
var
  s_FileName: string;
  s_CurrentBuild: string;
begin
  FLogger.WriteInfoMessageToLog('------ Деплой проекта ' +
                                 FConfigManager.Programs[AProgramIndex].s_ProgramName +
                                 ' начат! ------');
  s_FileName := DownloadArtifactFiles(AProgramIndex);
  s_CurrentBuild := FConfigManager.Programs[AProgramIndex].s_CurrentBuild;
  if FConfigManager.Programs[AProgramIndex].b_IsZipPacked then
    UnpackProgram(AProgramIndex, s_FileName);
  StopAllServicesAndTasks(AProgramIndex);
  BackupProgram(AProgramIndex);
  CopyProgram(AProgramIndex);
  if FConfigManager.Programs[AProgramIndex].Database.s_DatabaseName <> '' then
    UpdateDatabase(AProgramIndex);
  StartAllServicesAndTasks(AProgramIndex);
  if FConfigManager.GlobalSettings.b_IsArchiveOld then
    PackProgram(AProgramIndex, s_CurrentBuild);
  FLogger.WriteInfoMessageToLog('----- Деплой проекта ' +
                                 FConfigManager.Programs[AProgramIndex].s_ProgramName +
                                 ' завершён! ------');
end; // DeployProgram

// ----------------------------------------------------------------------------------------------------------------------------------

procedure TDeployManager.Deploy;
var
  i: integer;
  s_LastReleaseNumber: string;
  s_LastBuildNumber: string;
begin
  FLogger.WriteInfoMessageToLog('......... AUTODEPLOY STARTED! .........');
  if not FConfigManager.GlobalSettings.b_IsMaster then
  begin
    FLogger.WriteInfoMessageToLog('Сервис работает в режиме отслеживания релизных веток!');
    if CheckAllReleases then
    begin
      s_LastReleaseNumber := FJenkins[0].GetLastReleaseNumber;
      FLogger.WriteInfoMessageToLog('Обнаружены изменения релизов во всех отслеживаемых проектах: ' +
                                     FConfigManager.GlobalSettings.s_CurrentRelease +
                                     ' -> ' + s_LastReleaseNumber);
      for i := 0 to FProgramsCount - 1 do
      begin
        s_LastBuildNumber := FJenkins[i].GetLastBuildNumber;
        FLogger.WriteInfoMessageToLog('Определён номер последнего билда для проекта ' +
                                       FConfigManager.Programs[i].s_ProgramName + ': ' +
                                       FConfigManager.Programs[i].s_CurrentBuild +
                                       ' -> ' + s_LastBuildNumber);
        DeployProgram(i);
        FConfigManager.SetBuild(FConfigManager.Programs[i], s_LastBuildNumber);
        FLogger.WriteDebugMessageToLog('Сохранён номер последнего билда (' +
                                       s_LastBuildNumber + ') для ' +
                                       FConfigManager.Programs[i].s_ProgramName);
      end;
      FConfigManager.SetRelease(s_LastReleaseNumber);
      FLogger.WriteDebugMessageToLog('Сохранён номер последнего релиза (' +
                                      s_LastReleaseNumber + ')');
    end
    else
    begin
      FLogger.WriteInfoMessageToLog('Изменения релизов во ВСЕХ отслеживаемых проектах не обнаружены!');
      FLogger.WriteInfoMessageToLog('Проверка изменения билда для каждого проекта.');
      for i := 0 to FProgramsCount - 1 do
      begin
        s_LastReleaseNumber := FConfigManager.GlobalSettings.s_CurrentRelease;
        s_LastBuildNumber := FJenkins[i].GetLastBuildNumber(s_LastReleaseNumber,
                                                            FBuildState);
        if s_LastBuildNumber > FConfigManager.Programs[i].s_CurrentBuild then
        begin
          FLogger.WriteInfoMessageToLog('Обнаружено изменение билда проекта ' +
                                         FConfigManager.Programs[i].s_ProgramName + ': ' +
                                         FConfigManager.Programs[i].s_CurrentBuild + ' -> ' +
                                         s_LastBuildNumber);
          DeployProgram(i);
          FConfigManager.SetBuild(FConfigManager.Programs[i], s_LastBuildNumber);
          FLogger.WriteDebugMessageToLog('Сохранён номер последнего билда (' +
                                         s_LastBuildNumber + ') для ' +
                                         FConfigManager.Programs[i].s_ProgramName);
        end
        else
          FLogger.WriteInfoMessageToLog('Для проекта ' +
                                         FConfigManager.Programs[i].s_ProgramName +
                                         ' изменений билда не обнаружено.');
      end;
    end;
  end
  else
  begin
    FLogger.WriteInfoMessageToLog('Сервис работает в режиме отслеживания мастер-веток!');
    if CheckAllBuilds then
    begin
      FLogger.WriteInfoMessageToLog('Обнаружены изменения билдов во всех отслеживаемых проектах:');
      for i := 0 to FProgramsCount - 1 do
      begin
        s_LastReleaseNumber := FConfigManager.GlobalSettings.s_CurrentRelease;
        s_LastBuildNumber := FJenkins[i].GetLastBuildNumber(s_LastReleaseNumber,
                                                            FBuildState);
        FLogger.WriteInfoMessageToLog('Проект ' +
                                        FConfigManager.Programs[i].s_ProgramName + ': ' +
                                        FConfigManager.Programs[i].s_CurrentBuild + ' -> ' +
                                        s_LastBuildNumber);
        DeployProgram(i);
        FConfigManager.SetBuild(FConfigManager.Programs[i], s_LastBuildNumber);
        FLogger.WriteDebugMessageToLog('Сохранён номер последнего билда (' +
                                       s_LastBuildNumber + ') для ' +
                                       FConfigManager.Programs[i].s_ProgramName);
      end;
    end;
  end;
  FLogger.WriteInfoMessageToLog('......... AUTODEPLOY FINISHED! .........');
end; // Deploy

// ----------------------------------------------------------------------------------------------------------------------------------

function TDeployManager.GetLastBuildNumber(const AProgramIndex: integer): string;
var
  s_LastBuildNumber: string;
  s_LastFailedBuildNumber: string;
  s_LastStableBuildNumber: string;
  s_LastUnstableBuildNumber: string;
begin
  if Assigned(FJenkins[AProgramIndex]) then
  begin
    s_LastBuildNumber := FJenkins[AProgramIndex].GetLastBuildNumber(bsAll);
    s_LastFailedBuildNumber := FJenkins[AProgramIndex].GetLastBuildNumber(bsFailed);
    s_LastStableBuildNumber := FJenkins[AProgramIndex].GetLastBuildNumber(bsStable);
    s_LastUnstableBuildNumber := FJenkins[AProgramIndex].GetLastBuildNumber(bsUnstable);
    if FBuildState = bsStable then
    begin
      Result := s_LastStableBuildNumber;
      FLogger.WriteDebugMessageToLog('Для проекта №' + IntToStr(AProgramIndex) +
                                     ' определён s_LastStableBuildNumber = ' +
                                     Result);
    end
    else
    begin
      if s_LastBuildNumber = s_LastFailedBuildNumber then
      begin
        if s_LastStableBuildNumber > s_LastUnstableBuildNumber then
        begin
          Result := s_LastStableBuildNumber;
          FLogger.WriteDebugMessageToLog('Для проекта №' + IntToStr(AProgramIndex) +
                                         ' определён s_LastStableBuildNumber = ' +
                                         Result);
        end
        else
        begin
          Result := s_LastUnstableBuildNumber;
          FLogger.WriteDebugMessageToLog('Для проекта №' + IntToStr(AProgramIndex) +
                                         ' определён s_LastUnstableBuildNumber = ' +
                                         Result);
        end;
      end
      else
      begin
        Result := s_LastBuildNumber;
        FLogger.WriteDebugMessageToLog('Для проекта №' + IntToStr(AProgramIndex) +
                                       ' определён s_LastBuildNumber = ' +
                                       Result);
      end;
    end;
  end
  else
    FLogger.WriteDebugMessageToLog('Для проекта №' + IntToStr(AProgramIndex) +
                                   ' не инициализирован объект TJenkinsAPI!');
end; // GetLastBuildNumber

// ----------------------------------------------------------------------------------------------------------------------------------

procedure TDeployManager.OnTimer(Sender: TObject);
begin
  FLogger.WriteDebugMessageToLog('Таймер остановлен!');
  FCheckTimer.Enabled := false;
  Deploy;
  FCheckTimer.Enabled := true;
  FLogger.WriteDebugMessageToLog('Таймер запущен!');
end; // OnTimer

// ----------------------------------------------------------------------------------------------------------------------------------

function TDeployManager.CheckNewRelease(const AProgramIndex: integer): string;
var
  s_NewRelease: string;
begin
  Result := '';
  if Assigned(FConfigManager) then
  begin
    try
      s_NewRelease := FJenkins[AProgramIndex].GetLastReleaseNumber;
      FLogger.WriteDebugMessageToLog('Для проекта №' + IntToStr(AProgramIndex) +
                                     ' определён s_NewRelease = ' + s_NewRelease);
      if s_NewRelease <> FConfigManager.GlobalSettings.s_CurrentRelease then
      begin
        Result := s_NewRelease;
        FLogger.WriteDebugMessageToLog('Для проекта №' + IntToStr(AProgramIndex) +
                                       ' определён Result = s_NewRelease = ' + Result);
      end
      else
      begin
        Result := '';
        FLogger.WriteDebugMessageToLog('Для проекта №' + IntToStr(AProgramIndex) +
                                       ' определён Result = ''');
      end;
    except
      on E: Exception do
      begin
        FLogger.WriteErrorMessageToLog('Ошибка при проверке нового релиза!',
                                       E.Message, E.StackTrace);
        Result := '';
      end;
    end;
  end
  else
    FLogger.WriteDebugMessageToLog('Не инициализирован объект TConfigManager!');
end; // CheckNewRelease

// ----------------------------------------------------------------------------------------------------------------------------------

function TDeployManager.CheckNewBuild(const AProgramIndex: integer): string;
var
  s_NewBuild: string;
begin
  Result := '';
  if Assigned(FConfigManager) then
  begin
    try
      s_NewBuild := GetLastBuildNumber(AProgramIndex);
      FLogger.WriteDebugMessageToLog('Для проекта №' + IntToStr(AProgramIndex) +
                                     ' определён s_NewBuild = ' + s_NewBuild);
      if s_NewBuild <> FConfigManager.Programs[AProgramIndex].s_CurrentBuild then
      begin
        Result := s_NewBuild;
        FLogger.WriteDebugMessageToLog('Для проекта №' + IntToStr(AProgramIndex) +
                                       ' определён Result = s_NewBuild = ' + s_NewBuild);
      end
      else
      begin
        Result := '';
        FLogger.WriteDebugMessageToLog('Для проекта №' + IntToStr(AProgramIndex) +
                                       ' определён Result = ''');
      end;
    except
      on E: Exception do
      begin
        FLogger.WriteErrorMessageToLog('Ошибка при проверке нового билда!',
                                       E.Message, E.StackTrace);
        Result := '';
      end;
    end;
  end
  else
    FLogger.WriteDebugMessageToLog('Не инициализирован объект TConfigManager!');
end; // CheckNewBuild

// ----------------------------------------------------------------------------------------------------------------------------------

function TDeployManager.CheckAllReleases: boolean;
var
  i: integer;
  s_PrevRelease: string;
  s_CurrRelease: string;
begin
  Result := true;
  if FProgramsCount = 1 then
  begin
    s_CurrRelease := CheckNewRelease(0);
    s_PrevRelease := FConfigManager.GlobalSettings.s_CurrentRelease;
    FLogger.WriteDebugMessageToLog('FProgramsCount = 1');
    FLogger.WriteDebugMessageToLog('s_CurrRelease = ' + s_CurrRelease) ;
    FLogger.WriteDebugMessageToLog('s_PrevRelease = ' + s_PrevRelease);
  end
  else
  begin
    FLogger.WriteDebugMessageToLog('FProgramsCount = ' + intToStr(FProgramsCount));
    s_PrevRelease := CheckNewRelease(0);
    for i := 1 to FProgramsCount - 1 do
    begin
      s_CurrRelease := CheckNewRelease(i);
      FLogger.WriteDebugMessageToLog('Итерация ' + IntToStr(i)) ;
      FLogger.WriteDebugMessageToLog('s_CurrRelease = ' + s_CurrRelease) ;
      FLogger.WriteDebugMessageToLog('s_PrevRelease = ' + s_PrevRelease);
      if s_PrevRelease <> s_CurrRelease then
      begin
        FLogger.WriteDebugMessageToLog('s_PrevRelease <> s_CurrRelease - Выход!');
        Result := false;
        Exit;
      end
      else
        s_PrevRelease := s_CurrRelease;
    end;
  end;
  Result := Result and
            (s_CurrRelease <> '') and
            (s_CurrRelease > FConfigManager.GlobalSettings.s_CurrentRelease);
  FLogger.WriteDebugMessageToLog('Результат выполнения функции CheckAllReleases: ' + BoolToStr(Result));
end; // CheckAllReleases

// ----------------------------------------------------------------------------------------------------------------------------------

function TDeployManager.CheckAllBuilds: boolean;
var
  i: integer;
  s_NextBuild: string;
  s_CurrBuild: string;
begin
  Result := true;
  for i := 0 to FProgramsCount - 1 do
  begin
    s_CurrBuild := FConfigManager.Programs[i].s_CurrentBuild;
    s_NextBuild := CheckNewBuild(i);
    FLogger.WriteDebugMessageToLog('Итерация ' + IntToStr(i)) ;
    FLogger.WriteDebugMessageToLog('s_CurrBuild = ' + s_CurrBuild) ;
    FLogger.WriteDebugMessageToLog('s_NextBuild = ' + s_NextBuild);
    Result := Result and (s_NextBuild > s_CurrBuild);
    FLogger.WriteDebugMessageToLog('Результат итерации: ' + BoolToStr(Result));
    if not Result then
    begin
      FLogger.WriteDebugMessageToLog('Result = false - Выход!');
      Exit;
    end;
  end;
  FLogger.WriteDebugMessageToLog('Результат выполнения функции CheckAllBuilds: ' + BoolToStr(Result));
end; // CheckAllBuilds

end. // uDeployManager
