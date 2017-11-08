unit TestSettings;

interface

uses
  System.SysUtils, System.Classes, System.IniFiles;

const
  // Дефолтные значения для соединения с БД дебага - пока файл не создан
  DefaultTestServer = 'TEST-PC';
  DefaultTestBase = 'ILSMonitoring';
  DefaultTestUser = 'sa';
  DefaultTestPassword = 'SAsql123';

  // Дефолтные значения для соединения с БД релиза - пока файл не создан
  DefaultTestServerRelease = 'TEST-PC';
  DefaultTestBaseRelease = 'ILSMonitoringRelease';
  DefaultTestUserRelease = 'sa';
  DefaultTestPasswordRelease = 'SAsql123';

type
  // Структура для хранения настроек соединения с базой
  TDatabaseSettingsRec = record
    TestServer: string;
    TestBase: string;
    TestUser: string;
    TestPassword: string;
  end;

// Функции для работы с Ini
function GetAutoStartSetting: Boolean;
function GetConfigSettings: string;
function GetFullPathForIniFile(FileName: string): string;
function ReadDatabaseSettings(FileName: string): TDatabaseSettingsRec;
function ReadSettingsFromIniFile(FileName: string): TDatabaseSettingsRec;
function GetFileFromParameter(Param: string): string;
procedure GetFileNames(var inpFile, outFile: string; defaultInpFile, defaultOutFile: string);

var
  DBSettingsRec: TDatabaseSettingsRec;

implementation

// Получаем полный путь до файла
function GetFullPathForIniFile(FileName: string): string;
begin

  Result := ExtractFilePath(ParamStr(0)) + FileName;

end; // GetFullPathForiniFile


function GetAutoStartSetting: Boolean;
begin

  Result := (UpperCase(ParamStr(1)) = '/AUTO') or (UpperCase(ParamStr(1)) = '/A');

end; // GetAutoStartSetting


// Возвращает имя файла шаблона тестов
function GetConfigSettings: string;
begin

  Result := ParamStr(2);

end; // GetConfigSettings


// Считываем настройки из Ini
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


// Создаём дефолтный файл настроек, если не создан, и считываем настройки
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


function GetFileFromParameter(Param: string): string;
begin
  Result := '';
  if Pos('-in:', Param) = 1 then
    Result := Copy(Param, 5, Length(Param) - 4);
  if Pos('-out:', Param) = 1 then
    Result := Copy(Param, 6, Length(Param) - 5);
end;


procedure GetFileNames(var inpFile, outFile: string; defaultInpFile, defaultOutFile: string);
begin
  inpFile := '';
  outFile := '';
  if (ParamStr(1) <> '') and (ParamStr(2) <> '') then
  begin
    inpFile := GetFileFromParameter(ParamStr(1));
    outFile := GetFileFromParameter(ParamStr(2));
  end
  else
  if ParamStr(1) <> '' then
  begin
    inpFile := GetFileFromParameter(ParamStr(1));
    outFile := GetFileFromParameter(ParamStr(1));
  end;

  if inpFile = '' then
    inpFile := defaultInpFile;
  if outFile = '' then
    outFile := defaultOutFile;
end;


initialization

  if FileExists(GetFullPathForIniFile('MServerTest.ini')) then
    DBSettingsRec := ReadSettingsFromIniFile('MServerTest.ini')
  else
    DBSettingsRec := ReadSettingsFromIniFile('BuildServerTest.ini');

end.
