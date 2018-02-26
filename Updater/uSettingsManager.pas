unit uSettingsManager;

interface

uses
  System.SysUtils, System.Classes, System.IniFiles, uSettingsStructure, Forms;

type
  TSettingsManager = class (TIniFile)
  private
    FIniFile: string;
    FDatabases: TDatabases;
    FPrograms: TPrograms;
    FServerSettings: TServerSettings;
    FRelease: TReleaseSettings;
    FDBUpdater: TDBUpdaterSettings;
    FJenkins: TJenkinsSettings;

    function GetServerSettings: TServerSettings;
    function GetReleaseSettings: TReleaseSettings;
    function GetDBUpdaterSettings: TDBUpdaterSettings;
    function GetJenkinsSettings: TJenkinsSettings;
    function GetServicesList(const AProgramSection: string): TServices;
    function GetFilesList(const AProgramSection: string): TFiles;
    function GetTasksList(const AProgramSection: string): TTasks;
  public
    constructor Create(const aIniFile: String);
    destructor Destroy; override;
    procedure LoadSettingsFromFile;
    procedure SetReleaseSettings(const AReleaseNumber: string;
                                 const ABuildNumber: string;
                                 const ABuildSection: string);

    property DBList: TDatabases read FDatabases;
    property ProgramsList: TPrograms read FPrograms;
    property Release: TReleaseSettings read FRelease;
    property DBUpdater: TDBUpdaterSettings read FDBUpdater;
    property Jenkins: TJenkinsSettings read FJenkins;
  end;

const
  cDelimiter = ';';

implementation

constructor TSettingsManager.Create(const aIniFile: String);
begin
  FIniFile := aIniFile;
  SetLength(FDatabases, 0);
  SetLength(FPrograms, 0);
  inherited Create(aIniFile);
end;


destructor TSettingsManager.Destroy;
var
  i: integer;
begin
  for i := 0 to length(FPrograms) - 1 do
  begin
    if Assigned(FPrograms[i].sl_Services) then
      FreeAndNil(FPrograms[i].sl_Services);
    if Assigned(FPrograms[i].sl_Files) then
      FreeAndNil(FPrograms[i].sl_Files);
    if Assigned(FPrograms[i].sl_Tasks) then
      FreeAndNil(FPrograms[i].sl_Tasks);
  end;
  SetLength(FDatabases, 0);
  SetLength(FPrograms, 0);
  inherited Destroy;
end;


function TSettingsManager.GetServerSettings: TServerSettings;
begin
  Result.s_ServerName := ReadString('SERVER', 'SERVERNAME', '');
  Result.s_UserName :=  ReadString('SERVER', 'USERNAME', 'sa');
  Result.s_Password :=  ReadString('SERVER', 'PASSWORD', 'SAsql123');
end;


function TSettingsManager.GetReleaseSettings: TReleaseSettings;
begin
  Result.s_ReleaseNumber := ReadString('RELEASE', 'RELEASENUMBER', '7.0.0');
  Result.s_BuildNumber := ReadString('RELEASE', 'BUILDNUMBER', '0');
end;


function TSettingsManager.GetDBUpdaterSettings: TDBUpdaterSettings;
begin
  Result.s_DBUpdaterPath := ReadString('DBUPDATER', 'PATH', '');
  Result.s_ScriptsPath := ReadString('DBUPDATER', 'SCRIPTS', '');
  Result.s_IniFilePath := ReadString('DBUPDATER', 'INIFILE', '');
end;

function TSettingsManager.GetJenkinsSettings: TJenkinsSettings;
begin
  Result.s_URL := ReadString('JENKINS', 'URL', '');
  Result.s_User := ReadString('JENKINS', 'USERNAME', '');
  Result.s_Password := ReadString('JENKINS', 'PASSWORD', '');
end;

procedure TSettingsManager.SetReleaseSettings(const AReleaseNumber: string;
                                              const ABuildNumber: string;
                                              const ABuildSection: string);
begin
  WriteString('RELEASE', 'RELEASENUMBER', AReleaseNumber);
  WriteString(ABuildSection, 'BUILDNUMBER', ABuildNumber);
  FRelease.s_ReleaseNumber := AReleaseNumber;
  FRelease.s_BuildNumber := ABuildNumber;
end;


function TSettingsManager.GetServicesList(const AProgramSection: string): TServices;
var
  s_Services: string;
  s_Service: string;
  i_PosDelimiter: integer;
begin
  s_Services := ReadString(AProgramSection, 'SERVICES', '');
  i_PosDelimiter := pos(cDelimiter, s_Services);
  while i_PosDelimiter > 0 do
  begin
    s_Service := Copy(s_Services, 1, i_PosDelimiter - 1);
    Delete(s_Services, 1, i_PosDelimiter);
    Result.Add(s_Service);
    i_PosDelimiter := pos(cDelimiter, s_Services);
  end;
  if s_Services <> '' then
    Result.Add(s_Services);
end;


function TSettingsManager.GetFilesList(const AProgramSection: string): TFiles;
var
  s_Files: string;
  s_File: string;
  i_PosDelimiter: integer;
begin
  s_Files := ReadString(AProgramSection, 'FILES', '');
  i_PosDelimiter := pos(cDelimiter, s_Files);
  while i_PosDelimiter > 0 do
  begin
    s_File := Copy(s_Files, 1, i_PosDelimiter - 1);
    Delete(s_Files, 1, i_PosDelimiter);
    Result.Add(s_File);
    i_PosDelimiter := pos(cDelimiter, s_Files);
  end;
  if s_Files <> '' then
    Result.Add(s_Files);
end;


function TSettingsManager.GetTasksList(const AProgramSection: string): TTasks;
var
  s_Tasks: string;
  s_Task: string;
  i_PosDelimiter: integer;
begin
  s_Tasks := ReadString(AProgramSection, 'TASKS', '');
  i_PosDelimiter := pos(cDelimiter, s_Tasks);
  while i_PosDelimiter > 0 do
  begin
    s_Task := Copy(s_Tasks, 1, i_PosDelimiter - 1);
    Delete(s_Tasks, 1, i_PosDelimiter);
    Result.Add(s_Task);
    i_PosDelimiter := pos(cDelimiter, s_Tasks);
  end;
  if s_Tasks <> '' then
    Result.Add(s_Tasks);
end;


procedure TSettingsManager.LoadSettingsFromFile;
var
  i_SectionsCounter: integer;
  s_Sections: TStrings;
begin
  ReadSections(s_Sections);
  FServerSettings := GetServerSettings();
  FRelease := GetReleaseSettings();
  FDBUpdater := GetDBUpdaterSettings();
  FJenkins := GetJenkinsSettings();
  for i_SectionsCounter := 0 to s_Sections.Count - 1 do
  begin
    if pos('PROGRAM', s_Sections.Strings[i_SectionsCounter]) > 0 then
      with FPrograms[i_SectionsCounter] do
      begin
        SetLength(FPrograms, Length(FPrograms) + 1);
        s_ProgramName := ReadString(s_Sections.Strings[i_SectionsCounter], 'NAME', '');
        s_SourcePath := ReadString(s_Sections.Strings[i_SectionsCounter], 'SRC',
                                   ExtractFilePath(Application.ExeName) + '\TEMP');
        s_ReleaseTemplate :=  ReadString(s_Sections.Strings[i_SectionsCounter],
                                         'RTEMPLATE', 'release\');
        s_DestinationDir := ReadString(s_Sections.Strings[i_SectionsCounter],
                                       'DEST', '');
        s_LastBuild := ReadString(s_Sections.Strings[i_SectionsCounter], 'BUILDNUMBER',
                                  FRelease.s_BuildNumber);
        s_IniSectionName := s_Sections.Strings[i_SectionsCounter];
        sl_Services := TStringList.Create;
        sl_Services := GetServicesList(s_Sections.Strings[i_SectionsCounter]);
        sl_Tasks := TStringList.Create;
        sl_Tasks := GetTasksList(s_Sections.Strings[i_SectionsCounter]);
        sl_Files := TStringList.Create;
        sl_Files := GetFilesList(s_Sections.Strings[i_SectionsCounter]);
    end;
    if pos('DATABASE', s_Sections.Strings[i_SectionsCounter]) > 0  then
      with FDatabases[i_SectionsCounter] do
      begin
        SetLength(FDatabases, Length(FDatabases) + 1);
        r_ServerSettings.s_ServerName := ReadString(s_Sections.Strings[i_SectionsCounter],
                                                    'SERVER',
                                                    FServerSettings.s_ServerName );
        r_ServerSettings.s_UserName := ReadString(s_Sections.Strings[i_SectionsCounter],
                                                  'USER',
                                                  FServerSettings.s_UserName );
        r_ServerSettings.s_Password := ReadString(s_Sections.Strings[i_SectionsCounter],
                                                  'PASSWORD',
                                                  FServerSettings.s_Password );
        s_DatabaseName := ReadString(s_Sections.Strings[i_SectionsCounter], 'DBNAME', '');
      end;
  end;
end;

end.
