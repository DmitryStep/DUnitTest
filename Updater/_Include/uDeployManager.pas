unit uDeployManager;

interface

uses
  uConfigManager, uJenkinsAPI, SysUtils, VCL.ExtCtrls, Classes, uShellAPI,
  uServicesManager, Forms, IOUtils, IniFiles;

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

    procedure CreateDirectories(const AProgramIndex: integer);
    procedure DeleteDirectories;
    function DownloadArtifactFiles(const AProgramIndex: integer): string;
    procedure PackProgram(const AProgramIndex: integer);
    procedure UnpackProgram(const AProgramIndex: integer;
                           const AArchFileName: string);
    function StopAllServicesAndTasks(const AProgramIndex: integer): boolean;
    function StartAllServicesAndTasks(const AProgramIndex: integer): boolean;
    function CopyProgram(const AProgramIndex: integer): boolean;
    function UpdateDatabase(const AProgramIndex: integer): boolean;

    procedure OnTimer(Sender: TObject);
    // Формируем строку соединения с базой по данным из конфига
    function GetConnectionString(const AProgramIndex: integer): string;

    // Создание ini-файла для DBUpdater
    procedure SetIniFileForDBUpdater(const AProgramIndex: integer);

    procedure DeployProgram(const AProgramIndex: integer);

  public
    constructor Create(const AConfigManager: TConfigManager);
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
  TDirectory.CreateDirectory(FConfigManager.GlobalSettings.s_TempDir);
  TDirectory.CreateDirectory(FTempReleaseDir);
  TDirectory.CreateDirectory(FTempBackupDir);
  TDirectory.CreateDirectory(FTempReleaseDir +
                             FConfigManager.Programs[AProgramIndex].s_ProgramName);
  TDirectory.CreateDirectory(FTempBackupDir +
                             FConfigManager.Programs[AProgramIndex].s_ProgramName);
end; // CreateDirectories


procedure TDeployManager.DeleteDirectories;
begin
  TDirectory.Delete(FConfigManager.GlobalSettings.s_TempDir ,true);
end; // DeleteDirectories


function TDeployManager.DownloadArtifactFiles(const AProgramIndex: integer): string;
var
  sl_ArtifactsList: TStringList;
  s_FileURL: string;
  s_FileName: string;
  i: integer;
begin
  sl_ArtifactsList := TStringList.Create();
  try
    try
      if FJenkins[AProgramIndex].GetArtifactsList(sl_ArtifactsList) > 0 then
      begin
        if FConfigManager.Programs[AProgramIndex].b_IsZipPacked then
        begin
          s_FileURL := sl_ArtifactsList.Strings[0];
          s_FileName := FJenkins[AProgramIndex].GetFileNameFromURL(s_FileURL);
          Result := FTempReleaseDir + s_FileName;
          FJenkins[AProgramIndex].GetFile(s_FileURL, Result);
        end
        else
        for i := 0 to sl_ArtifactsList.Count - 1 do
        begin
          s_FileURL := sl_ArtifactsList.Strings[i];
          s_FileName := FJenkins[AProgramIndex].GetFileNameFromURL(s_FileURL);
          FJenkins[AProgramIndex].GetFile(s_FileURL, FTempReleaseDir +
                                          '\' + s_FileName);
          Result := '';
        end;
      end;
    except
      Result := '';
    end;
  finally
    FreeAndNil(sl_ArtifactsList);
  end;
end; // DownloadArtifactFiles


procedure TDeployManager.PackProgram(const AProgramIndex: integer);
begin
  ArchiveFiles(FTempBackupDir +
               FConfigManager.Programs[AProgramIndex].s_ProgramName,
               ExtractFilePath(Application.ExeName) +
               FConfigManager.Programs[AProgramIndex].s_ProgramName +
               FConfigManager.GlobalSettings.s_CurrentRelease + '.zip'
              );
end; // PackProgram


procedure TDeployManager.UnpackProgram(const AProgramIndex: integer;
                                      const AArchFileName: string);
begin
  UnarchiveFiles(AArchFileName,
                 FTempReleaseDir +
                 FConfigManager.Programs[AProgramIndex].s_ProgramName
                );
end; // UnpackProgram


function TDeployManager.StopAllServicesAndTasks(const AProgramIndex: integer): boolean;
var
  i, j: integer;
  ServiceManager: TServiceManager;
  ServicesList: TStringList;
begin
  ServicesList := TStringList.Create;
  ServiceManager := TServiceManager.Create;
  for i := 0 to FConfigManager.Programs[AProgramIndex].sl_ProgramServices.Count - 1 do
  begin
    ServiceManager.GetServicesList(ServicesList,
                                   FConfigManager.Programs[AProgramIndex]
                                                 .sl_ProgramServices
                                                 .Strings[i]);
    for j := 0 to ServicesList.Count - 1 do
      ServiceManager.StopService(ServicesList.Strings[j]);
  end;
  for i := 0 to FConfigManager.Programs[AProgramIndex].sl_ProgramTasks.Count - 1 do
  begin
    ServiceManager.KillTask(FConfigManager.Programs[AProgramIndex]
                                           .sl_ProgramTasks
                                           .Strings[i]);
  end;
  FreeAndNil(ServiceManager);
  FreeAndNil(ServicesList);
end; // StopAllServicesAndTasks


function TDeployManager.StartAllServicesAndTasks(const AProgramIndex: integer): boolean;
var
  i, j: integer;
  ServiceManager: TServiceManager;
  ServicesList: TStringList;
begin
  ServicesList := TStringList.Create;
  ServiceManager := TServiceManager.Create;
  for i := 0 to FConfigManager.Programs[AProgramIndex].sl_ProgramServices.Count - 1 do
  begin
    ServiceManager.GetServicesList(ServicesList,
                                   FConfigManager.Programs[AProgramIndex]
                                                 .sl_ProgramServices
                                                 .Strings[i]);
    for j := 0 to ServicesList.Count - 1 do
      ServiceManager.RunService(ServicesList.Strings[j]);
  end;
  for i := 0 to FConfigManager.Programs[AProgramIndex].sl_ProgramTasks.Count - 1 do
  begin
    ServiceManager.StartTask( FConfigManager.Programs[AProgramIndex]
                                            .s_DestinationDir + '\' +
                              FConfigManager.Programs[AProgramIndex]
                                           .sl_ProgramTasks
                                           .Strings[i]);
  end;
  FreeAndNil(ServiceManager);
  FreeAndNil(ServicesList);
end; // StartAllServicesAndTasks


function TDeployManager.CopyProgram(const AProgramIndex: integer): boolean;
var
  i: integer;
  s_FileMask: string;
  s_Source: string;
  p: integer;
begin
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
    CopyFiles(FTempReleaseDir +
              FConfigManager.Programs[AProgramIndex].s_ProgramName + '\' +
              s_Source,
              FConfigManager.Programs[AProgramIndex].s_DestinationDir + s_FileMask);
  end;
end; // CopyProgram


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


procedure TDeployManager.SetIniFileForDBUpdater(const AProgramIndex: integer);
var
  DBUpdaterIni: TIniFile;
  s_ConnectionString: string;
begin
  s_ConnectionString := GetConnectionString(AProgramIndex);
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
  FreeAndNil(DBUpdaterIni);
end; // SetIniFileForDBUpdater


function TDeployManager.UpdateDatabase(const AProgramIndex: integer): boolean;
var
  s_CmdLine: string;
begin
  SetIniFileForDBUpdater(AProgramIndex);
  s_CmdLine := FConfigManager.GlobalSettings.s_DBUpdaterDir + ' -c i -i "' +
               FConfigManager.GlobalSettings.s_DBUpdaterDir + '\Updater.ini"';
  WinExec(s_CmdLine, SW_HIDE);
end; // UpdateDatabase


constructor TDeployManager.Create(const AConfigManager: TConfigManager);
var
  i: integer;
begin
  FConfigManager := AConfigManager;
  FTempReleaseDir := FConfigManager.GlobalSettings.s_TempDir + '\Release\';
  FTempBackupDir := FConfigManager.GlobalSettings.s_TempDir + '\BackUp\';
  if Assigned(AConfigManager) then
  begin
    FProgramsCount := length(FConfigManager.Programs);
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
    end
    else
      FCheckTimer := nil;
    for i := 0 to FProgramsCount - 1 do
    begin
      FJenkins[i] := TJenkinsAPI.Create(FConfigManager.Programs[i].s_SourceURL,
                                        FConfigManager.Programs[i].s_SourceReleaseDir,
                                        FConfigManager.Programs[i].s_SourceUser,
                                        FConfigManager.Programs[i].s_SourcePass,
                                        FBuildState);
      CreateDirectories(i);
    end;
  end;
end; // Create


destructor TDeployManager.Destroy;
var
  i: integer;
begin
  FreeAndNil(FCheckTimer);
  for i := 0 to FProgramsCount - 1 do
    FreeAndNil(FJenkins[i]);
  SetLength(FJenkins, 0);
  DeleteDirectories;
  FConfigManager := nil;
end; // Destroy


procedure TDeployManager.DeployProgram(const AProgramIndex: integer);
var
  s_FileName: string;
begin
  s_FileName := DownloadArtifactFiles(AProgramIndex);
  if FConfigManager.Programs[AProgramIndex].b_IsZipPacked then
    UnpackProgram(AProgramIndex, s_FileName);
  StopAllServicesAndTasks(AProgramIndex);
  CopyProgram(AProgramIndex);
  if FConfigManager.Programs[AProgramIndex].Database.s_DatabaseName <> '' then
    UpdateDatabase(AProgramIndex);
  StartAllServicesAndTasks(AProgramIndex);
  PackProgram(AProgramIndex);
end; // DeployProgram


procedure TDeployManager.Deploy;
var
  i: integer;
  s_LastReleaseNumber: string;
  s_LastBuildNumber: string;
begin
  if not FConfigManager.GlobalSettings.b_IsMaster then
  begin
    if CheckAllReleases then
    begin
      for i := 0 to FProgramsCount - 1 do
      begin
//        s_LastReleaseNumber := FConfigManager.GlobalSettings.s_CurrentRelease;
        s_LastBuildNumber := FJenkins[i].GetLastBuildNumber;
        DeployProgram(i);
        FConfigManager.SetBuild(FConfigManager.Programs[i], s_LastBuildNumber);
      end;
      FConfigManager.SetRelease(s_LastReleaseNumber);
    end
    else
      for i := 0 to FProgramsCount - 1 do
      begin
        s_LastReleaseNumber := FConfigManager.GlobalSettings.s_CurrentRelease;
        s_LastBuildNumber := FJenkins[i].GetLastBuildNumber(s_LastReleaseNumber,
                                                            FBuildState);
        if s_LastBuildNumber > FConfigManager.Programs[i].s_CurrentBuild then
          DeployProgram(i);
        FConfigManager.SetBuild(FConfigManager.Programs[i], s_LastBuildNumber);
      end;
  end
  else
  begin
    if CheckAllBuilds then
      for i := 0 to FProgramsCount - 1 do
      begin
        s_LastReleaseNumber := FConfigManager.GlobalSettings.s_CurrentRelease;
        s_LastBuildNumber := FJenkins[i].GetLastBuildNumber(s_LastReleaseNumber,
                                                            FBuildState);
        DeployProgram(i);
        FConfigManager.SetBuild(FConfigManager.Programs[i], s_LastBuildNumber);
      end;
  end;
end; // Deploy


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
      Result := s_LastStableBuildNumber
    else
    begin
      if s_LastBuildNumber = s_LastFailedBuildNumber then
      begin
        if s_LastStableBuildNumber > s_LastUnstableBuildNumber then
          Result := s_LastStableBuildNumber
        else
          Result := s_LastUnstableBuildNumber;
      end
      else
        Result := s_LastBuildNumber;
    end;
  end;
end; // GetLastBuildNumber


procedure TDeployManager.OnTimer(Sender: TObject);
begin
  FCheckTimer.Enabled := false;
  Deploy;
  FCheckTimer.Enabled := true;
end; // OnTimer


function TDeployManager.CheckNewRelease(const AProgramIndex: integer): string;
var
  s_NewRelease: string;
begin
  Result := '';
  if Assigned(FConfigManager) then
  begin
    try
      s_NewRelease := FJenkins[AProgramIndex].GetLastReleaseNumber;
      if s_NewRelease <> FConfigManager.GlobalSettings.s_CurrentRelease then
        Result := s_NewRelease
      else
        Result := '';
    except
      Result := '';
    end;
  end;
end; // CheckNewRelease


function TDeployManager.CheckNewBuild(const AProgramIndex: integer): string;
var
  s_NewBuild: string;
begin
  Result := '';
  if Assigned(FConfigManager) then
  begin
    try
      s_NewBuild := GetLastBuildNumber(AProgramIndex);
      if s_NewBuild <> FConfigManager.Programs[AProgramIndex].s_CurrentBuild then
        Result := s_NewBuild
      else
        Result := '';
    except
      Result := '';
    end;
  end;
end; // CheckNewBuild


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
  end
  else
  begin
    s_PrevRelease := CheckNewRelease(0);
    for i := 1 to FProgramsCount - 1 do
    begin
      s_CurrRelease := CheckNewRelease(i);
      if s_PrevRelease <> s_CurrRelease then
      begin
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
end; // CheckAllReleases


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
    Result := Result and (s_NextBuild > s_CurrBuild);
    if not Result then
      Exit;
  end;
end; // CheckAllBuilds


end.
