unit uSettingsManager;

interface

uses
  System.SysUtils, System.Classes, System.IniFiles, uSettingsStructure;

type
  TSettingsManager = class (TIniFile)
  private
    FIniFile: string;
    FDatabases: TDatabases;
    FPrograms: TPrograms;
    FServices: TServices;
    FServerSettings: TServerSettings;
    FRelease: TReleaseSettings;
    FDBUpdater: TDBUpdaterSettings;

    function GetServerSettings: TServerSettings;
    function GetReleaseSettings: TReleaseSettings;
    function GetDBUpdaterSettings: TDBUpdaterSettings;
    procedure GetServicesList;
  public
    constructor Create(const aIniFile: String);
    destructor Destroy; override;
    procedure LoadSettingsFromFile;
    procedure SetReleaseSettings(const AReleaseNumber: string;
                                const ABuildNumber: string);

    property DBList: TDatabases read FDatabases;
    property ProgramsList: TPrograms read FPrograms;
    property ServicesList: TServices read FServices;
    property Release: TReleaseSettings read FRelease;
    property DBUpdater: TDBUpdaterSettings read FDBUpdater;
  end;

implementation

constructor TSettingsManager.Create(const aIniFile: String);
begin
  FIniFile := aIniFile;
  SetLength(FDatabases, 0);
  SetLength(FPrograms, 0);
  SetLength(FServices, 0);
  inherited Create(aIniFile);
end;


destructor TSettingsManager.Destroy;
begin
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
  Result.s_ReleaseNumber := self.ReadString('RELEASE', 'RELEASENUMBER', '7.0.0');
  Result.s_BuildNumber := self.ReadString('RELEASE', 'BUILDNUMBER', '0');
end;


function TSettingsManager.GetDBUpdaterSettings: TDBUpdaterSettings;
begin
  Result.s_DBUpdaterPath := self.ReadString('DBUPDATER', 'PATH', '');
  Result.s_ScriptsPath := self.ReadString('DBUPDATER', 'SCRIPTS', '');
  Result.s_IniFilePath := self.ReadString('DBUPDATER', 'INIFILE', '');
end;


procedure TSettingsManager.SetReleaseSettings(const AReleaseNumber: string;
                                              const ABuildNumber: string);
begin
  WriteString('RELEASE', 'RELEASENUMBER', AReleaseNumber);
  WriteString('RELEASE', 'BUILDNUMBER', ABuildNumber);
  FRelease.s_ReleaseNumber := AReleaseNumber;
  FRelease.s_BuildNumber := ABuildNumber;
end;


procedure TSettingsManager.GetServicesList;
var
  i_ServicesCounter: integer;
  s_Services: TStrings;
begin
  self.ReadSectionValues('SERVICES', s_Services);
  for i_ServicesCounter := 0 to s_Services.Count - 1 do
  begin
    SetLength(FServices, length(FServices) + 1);
    FServices[i_ServicesCounter] := s_Services.Strings[i_ServicesCounter];
  end;
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
  GetServicesList();
  for i_SectionsCounter := 0 to s_Sections.Count - 1 do
  begin
    if pos('PROGRAM', s_Sections.Strings[i_SectionsCounter]) > 0 then
    begin
      SetLength(FPrograms, Length(FPrograms) + 1);
      FPrograms[i_SectionsCounter].s_ProgramName := ReadString(s_Sections.Strings[i_SectionsCounter], 'NAME', '');
      FPrograms[i_SectionsCounter].s_SourceURL := ReadString(s_Sections.Strings[i_SectionsCounter], 'URL', '');
      FPrograms[i_SectionsCounter].s_SourcePath := ReadString(s_Sections.Strings[i_SectionsCounter], 'SRC', '');
      FPrograms[i_SectionsCounter].s_ReleaseTemplate :=  ReadString(s_Sections.Strings[i_SectionsCounter], 'RTEMPLATE', '');
      FPrograms[i_SectionsCounter].s_DestinationDir := ReadString(s_Sections.Strings[i_SectionsCounter], 'DEST', '');
    end;
    if pos('DATABASE', s_Sections.Strings[i_SectionsCounter]) > 0  then
    begin
      SetLength(FDatabases, Length(FDatabases) + 1);
      FDatabases[i_SectionsCounter].r_ServerSettings.s_ServerName := ReadString(s_Sections.Strings[i_SectionsCounter],
                                                                                'SERVER',
                                                                                FServerSettings.s_ServerName );
      FDatabases[i_SectionsCounter].r_ServerSettings.s_UserName := ReadString(s_Sections.Strings[i_SectionsCounter],
                                                                              'USER',
                                                                              FServerSettings.s_UserName );
      FDatabases[i_SectionsCounter].r_ServerSettings.s_Password := ReadString(s_Sections.Strings[i_SectionsCounter],
                                                                              'PASSWORD',
                                                                              FServerSettings.s_Password );
      FDatabases[i_SectionsCounter].s_DatabaseName := ReadString(s_Sections.Strings[i_SectionsCounter], 'DBNAME', '');
    end;
  end;

end;

end.
