unit TestSettings;

interface

uses
  System.SysUtils, System.Classes, System.IniFiles;

const
  // ��������� �������� ��� ���������� � �� ������ - ���� ���� �� ������
  DefaultTestServer = 'TEST-PC';
  DefaultTestBase = 'ILSMonitoring';
  DefaultTestUser = 'sa';
  DefaultTestPassword = 'SAsql123';

  // ��������� �������� ��� ���������� � �� ������ - ���� ���� �� ������
  DefaultTestServerRelease = 'TEST-PC';
  DefaultTestBaseRelease = 'ILSMonitoringRelease';
  DefaultTestUserRelease = 'sa';
  DefaultTestPasswordRelease = 'SAsql123';

type
  // ��������� ��� �������� �������� ���������� � �����
  TDatabaseSettingsRec = record
    TestServer: string;
    TestBase: string;
    TestUser: string;
    TestPassword: string;
  end;

// ������� ��� ������ � Ini
function GetAutoStartSetting: Boolean;
function GetConfigSettings: string;
function GetFullPathForIniFile(FileName: string): string;
function ReadDatabaseSettings(FileName: string): TDatabaseSettingsRec;
function ReadSettingsFromIniFile(FileName: string): TDatabaseSettingsRec;

var
  DBSettingsRec: TDatabaseSettingsRec;

implementation

// �������� ������ ���� �� �����
function GetFullPathForIniFile(FileName: string): string;
begin

  Result := ExtractFilePath(ParamStr(0)) + FileName;

end; // GetFullPathForiniFile


function GetAutoStartSetting: Boolean;
begin

  Result := (UpperCase(ParamStr(1)) = '/AUTO') or (UpperCase(ParamStr(1)) = '/A');

end; // GetAutoStartSetting


// ���������� ��� ����� ������� ������
function GetConfigSettings: string;
begin

  Result := ParamStr(2);

end; // GetConfigSettings


// ��������� ��������� �� Ini
function ReadDatabaseSettings(FileName: string): TDatabaseSettingsRec;
var
  FullFileName: string;
  IniFile: TIniFile;

begin

  IniFile := nil;
  FullFileName := GetFullPathForIniFile(FileName);

  IniFile := TIniFile.Create(FullFileName);

  try

    {$IFDEF DEBUG}
      Result.TestServer := IniFile.ReadString('DBSETTINGS', 'TestServer', DefaultTestServer);
      Result.TestBase := IniFile.ReadString('DBSETTINGS', 'TestBase', DefaultTestBase);
      Result.TestUser := IniFile.ReadString('DBSETTINGS', 'TestUser', DefaultTestUser);
      Result.TestPassword := IniFile.ReadString('DBSETTINGS', 'TestPassword', DefaultTestPassword);
    {$ELSE}
      Result.TestServer := IniFile.ReadString('DBSETTINGS_RELEASE', 'TestServer', DefaultTestServerRelease);
      Result.TestBase := IniFile.ReadString('DBSETTINGS_RELEASE', 'TestBase', DefaultTestBaseRelease);
      Result.TestUser := IniFile.ReadString('DBSETTINGS_RELEASE', 'TestUser', DefaultTestUserRelease);
      Result.TestPassword := IniFile.ReadString('DBSETTINGS_RELEASE', 'TestPassword', DefaultTestPasswordRelease);
    {$ENDIF}

  finally

    FreeAndNil(IniFile);

  end;

end; // ReadSettings


// ������ ��������� ���� ��������, ���� �� ������, � ��������� ���������
function ReadSettingsFromIniFile(FileName: string): TDatabaseSettingsRec;
var
  FullFileName: string;
  IniFile: TIniFile;

begin

  IniFile := nil;
  FullFileName := GetFullPathForIniFile(FileName);

  if not FileExists(FullFileName) then
  begin

    IniFile := TIniFile.Create(FullFileName);

    try
      IniFile.WriteString('DBSETTINGS', 'TestServer', DefaultTestServer);
      IniFile.WriteString('DBSETTINGS', 'TestBase', DefaultTestBase);
      IniFile.WriteString('DBSETTINGS', 'TestUser', DefaultTestUser);
      IniFile.WriteString('DBSETTINGS', 'TestPassword', DefaultTestPassword);
      IniFile.WriteString('DBSETTINGS_RELEASE', 'TestServer', DefaultTestServerRelease);
      IniFile.WriteString('DBSETTINGS_RELEASE', 'TestBase', DefaultTestBaseRelease);
      IniFile.WriteString('DBSETTINGS_RELEASE', 'TestUser', DefaultTestUserRelease);
      IniFile.WriteString('DBSETTINGS_RELEASE', 'TestPassword', DefaultTestPasswordRelease);
    finally
      FreeAndNil(IniFile);
    end;

  end;

  Result := ReadDatabaseSettings(FileName);

end; // ReadSettingsFromIniFile

initialization

  if FileExists(GetFullPathForIniFile('MServerTest.ini')) then
    DBSettingsRec := ReadSettingsFromIniFile('MServerTest.ini')
  else
    DBSettingsRec := ReadSettingsFromIniFile('BuildServerTest.ini');

end.
