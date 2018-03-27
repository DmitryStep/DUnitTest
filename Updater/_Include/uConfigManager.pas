unit uConfigManager;

interface

uses
  System.SysUtils, System.Classes, System.IniFiles, Forms;

type
  TGlobalSettings = packed record
    s_JenkinsUser: string;
    s_JenkinsPass: string;
    s_DatabaseServer: string;
    s_DatabaseUser: string;
    s_DatabasePass: string;
    s_DBUpdaterDir: string;
    s_CurrentRelease: string;
    b_UseStableBuilds: boolean;
    i_CheckPeriod: integer;
    s_TempDir: string;
    b_IsMaster: boolean;
  end;

  TDatabaseSettings = packed record
    s_DatabaseServer: string;
    s_DatabaseUser: string;
    s_DatabasePass: string;
    s_DatabaseName: string;
  end;

  TProgramSettings = packed record
    s_SectionName: string;
    s_ProgramName: string;
    s_SourceURL: string;
    s_SourceUser: string;
    s_SourcePass: string;
    s_SourceReleaseDir: string;
    s_DestinationDir: string;
    Database: TDatabaseSettings;
    sl_ProgramFiles: TStringList;
    sl_ProgramServices: TStringList;
    sl_ProgramTasks: TStringList;
    s_CurrentBuild: string;
    b_IsDeploy: boolean;
    b_IsZipPacked: boolean;
  end;

  TProgramsList = array of TProgramSettings;


  TConfigManager = class (TIniFile)
  private
    FIniFile: string;
    FGlobalSettings: TGlobalSettings;
    FPrograms: TProgramsList;

    // преобразует строку значений в StringList
    procedure StrToList(var AResultList: TStringList;
                        const ASourceStr: string);
    // Загрузка секции GLOBAL
    procedure LoadGlobalSettings;

    // Загрузка секций PROGRAM
    procedure LoadProgramsSettings;

  public
    constructor Create(const AIniFile: String);
    destructor Destroy; override;

    // Загрузка всех настроек
    function LoadSettings: boolean;

    // Сохранение заданной версии релиза в конфиге
    procedure SetRelease(const AReleaseNumber: string);

    // Сохранение заданной версии билда в конфиге
    procedure SetBuild(var AProgram: TProgramSettings;
                       const ABuildNumber: string);

    property GlobalSettings: TGlobalSettings read FGlobalSettings;
    property Programs: TProgramsList read FPrograms;
  end;


implementation

constructor TConfigManager.Create(const AIniFile: String);
begin
  FIniFile := AIniFile;
  SetLength(FPrograms, 0);
  inherited Create(FIniFile);
end; // Create


destructor TConfigManager.Destroy;
var
  i: integer;

begin
  for i := 0 to length(FPrograms) - 1 do
  begin
    FPrograms[i].sl_ProgramFiles.Free;
    FPrograms[i].sl_ProgramServices.Free;
    FPrograms[i].sl_ProgramTasks.Free;
  end;
  SetLength(FPrograms, 0);
  inherited Destroy;
end; // Destroy


procedure TConfigManager.StrToList(var AResultList: TStringList;
                                   const ASourceStr: string);
const
  cDelimiter = ';';

var
  i_PosDelimiter: integer;
  s_TempStr: string;
  s_TempSrcStr: string;
begin
  AResultList.Clear;
  if ASourceStr <> '' then
  begin
    s_TempSrcStr := ASourceStr;
    i_PosDelimiter := pos(cDelimiter, ASourceStr);
    while i_PosDelimiter > 0 do
    begin
      s_TempStr := copy(s_TempSrcStr, 1, i_PosDelimiter - 1);
      AResultList.Add(s_TempStr);
      delete(s_TempSrcStr, 1, i_PosDelimiter);
      i_PosDelimiter := pos(cDelimiter, s_TempSrcStr);
    end;
    AResultList.Add(s_TempSrcStr);
  end;
end; // StrToList


procedure TConfigManager.LoadGlobalSettings;
begin
  with FGlobalSettings do
  begin
    s_JenkinsUser := ReadString('GLOBAL', 'JENKINSUSER', '');
    s_JenkinsPass := ReadString('GLOBAL', 'JENKINSPASS', '');
    s_DatabaseServer := ReadString('GLOBAL', 'DBSERVER', '');
    s_DatabaseUser := ReadString('GLOBAL', 'DBUSER', 'sa');
    s_DatabasePass := ReadString('GLOBAL', 'DBPASS', 'SAsql123');
    s_DBUpdaterDir := ReadString('GLOBAL', 'DBUPDATER', '');
    s_CurrentRelease := ReadString('GLOBAL', 'RELEASE', '7.0.0');
    b_UseStableBuilds := ReadBool('GLOBAL', 'USESTABLEBUILDS', true);
    i_CheckPeriod := ReadInteger('GLOBAL', 'CHECKPERIOD', 0);
    s_TempDir := ReadString('GLOBAL',
                            'TEMPDIR',
                            ExtractFilePath(Application.ExeName) + 'TEMP');
    b_IsMaster := ReadBool('GLOBAL', 'MASTER', true);
  end;
end; // LoadGlobalSettings


procedure TConfigManager.LoadProgramsSettings;
var
  s_Sections: TStringList;
  i: integer;
  s_TempStr: string;
begin
  s_Sections := TStringList.Create;
  ReadSections(s_Sections);
  for i := 0 to s_Sections.Count - 1 do
    if pos('PROGRAM', UpperCase(s_Sections.Strings[i])) = 1 then
    begin
      SetLength(FPrograms, length(FPrograms) + 1);
      with FPrograms[length(FPrograms) - 1] do
      begin
        s_SectionName := s_Sections.Strings[i];
        s_ProgramName := ReadString(s_Sections.Strings[i], 'NAME', '');
        s_SourceURL := ReadString(s_Sections.Strings[i], 'URL', '');
        s_SourceUser := ReadString(s_Sections.Strings[i],
                                   'USER',
                                   FGlobalSettings.s_JenkinsUser);
        s_SourcePass := ReadString(s_Sections.Strings[i],
                                   'PASS',
                                   FGlobalSettings.s_JenkinsPass);
        s_SourceReleaseDir := ReadString(s_Sections.Strings[i],
                                         'RELEASESRC',
                                         '');
        s_DestinationDir := ReadString(s_Sections.Strings[i], 'DESTDIR', '');
        Database.s_DatabaseServer := ReadString(s_Sections.Strings[i],
                                       'DBSERVER',
                                       FGlobalSettings.s_DatabaseServer);
        Database.s_DatabaseUser := ReadString(s_Sections.Strings[i],
                                     'DBUSER',
                                     FGlobalSettings.s_DatabaseUser);
        Database.s_DatabasePass := ReadString(s_Sections.Strings[i],
                                     'DBPASS',
                                     FGlobalSettings.s_DatabasePass);
        Database.s_DatabaseName := ReadString(s_Sections.Strings[i],
                                              'DBNAME', '');
        s_TempStr := ReadString(s_Sections.Strings[i], 'FILES', '');
        sl_ProgramFiles := TStringList.Create;
        StrToList(sl_ProgramFiles, s_TempStr);
        s_TempStr := ReadString(s_Sections.Strings[i], 'SERVICES', '');
        sl_ProgramServices := TStringList.Create;
        StrToList(sl_ProgramServices, s_TempStr);
        s_TempStr := ReadString(s_Sections.Strings[i], 'TASKS', '');
        sl_ProgramTasks := TStringList.Create;
        StrToList(sl_ProgramTasks, s_TempStr);
        s_CurrentBuild := ReadString(s_Sections.Strings[i], 'BUILD', '0');
        b_IsDeploy := ReadBool(s_Sections.Strings[i], 'DEPLOY', true);
        b_IsZipPacked := ReadBool(s_Sections.Strings[i], 'PACKED', true);
      end;
    end;
  FreeAndNil(s_Sections);
end; // LoadProgramsSettings


function TConfigManager.LoadSettings: boolean;
begin
  Result := true;
  try
    LoadGlobalSettings;
    LoadProgramsSettings;
  except
    Result := false;
  end;
end; // LoadSettings


procedure TConfigManager.SetRelease(const AReleaseNumber: string);
begin
  WriteString('GLOBAL', 'RELEASE', AReleaseNumber);
  FGlobalSettings.s_CurrentRelease := AReleaseNumber;
end; // SetRelease


procedure TConfigManager.SetBuild(var AProgram: TProgramSettings;
                                  const ABuildNumber: string);
begin
  WriteString(AProgram.s_SectionName, 'BUILD', ABuildNumber);
  AProgram.s_CurrentBuild := ABuildNumber;
end; // SetBuild


end.
