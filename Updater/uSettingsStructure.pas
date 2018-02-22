unit uSettingsStructure;

interface

type
  TServerSettings = packed record
    s_ServerName: string;
    s_UserName: string;
    s_Password: string;
  end;

  TDatabaseSettings = packed record
    r_ServerSettings: TServerSettings;
    s_DatabaseName: string;
  end;

  TProgramSettings = packed record
    s_ProgramName: string;
    s_SourceURL: string;
    s_SourcePath: string;
    s_ReleaseTemplate: string;
    s_DestinationDir: string;
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

  TServicesNames = string;

  TDatabases = array of TDatabaseSettings;
  TPrograms  = array of TProgramSettings;
  TServices  = array of TServicesNames;

implementation

end.
