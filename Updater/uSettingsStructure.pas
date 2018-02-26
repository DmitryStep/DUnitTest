unit uSettingsStructure;

interface

uses
  Classes;

type
  TFiles = TStringList;
  TServices  = TStringList;
  TTasks = TStringList;

  TServerSettings = packed record
    s_ServerName: string;
    s_UserName: string;
    s_Password: string;
  end;

  TJenkinsSettings = packed record
    s_URL: string;
    s_User: string;
    s_Password: string;
  end;

  TDatabaseSettings = packed record
    r_ServerSettings: TServerSettings;
    s_DatabaseName: string;
  end;

  TProgramSettings = packed record
    s_ProgramName: string;
    s_IniSectionName: string;
    s_SourcePath: string;
    s_ReleaseTemplate: string;
    s_DestinationDir: string;
    s_LastBuild: string;
    sl_Files: TFiles;
    sl_Services: TServices;
    sl_Tasks: TTasks;
  end;

  TReleaseSettings = packed record
    s_ReleaseNumber: string;
    s_BuildNumber: string;
  end;

  TDBUpdaterSettings = packed record
    s_DBUpdaterPath: string;
    s_ScriptsPath: string;
    s_IniFilePath: string;
  end;

  TDatabases = array of TDatabaseSettings;
  TPrograms  = array of TProgramSettings;

implementation

end.
