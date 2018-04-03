unit uJenkinsAPI;

interface

uses
  IdTCPConnection, IdTCPClient, IdHTTP, System.SysUtils, System.Classes,
  uLogManager,
  {$IF CompilerVersion >= 27}
    System.JSON
  {$ELSE}
    DBXJSON
  {$IFEND};

type
  TBuildState = (bsAll, bsStable, bsUnstable, bsFailed);

  TJenkinsAPI = class
  private
    FIdHTTP: TIdHTTP;
    FJenkinsProjectURL: string;
    FReleasesDir: string;
    FUsername: string;
    FPassword: string;
    FBuildState: TBuildState;
    FLogger: TLogManager;


    // Возвращает наименование поля для получения последнего билда
    // ABuildState - значение определяет "успешность" запрашиваемого билда
    // (см. тип TBuildState и константу cBuildFieldName)
    function GetLastBuildFieldName(const ABuildState: TBuildState): string;
  public
    constructor Create(const AJenkinsURL: string;
                       const AReleasesDir: string;
                       const AUsername: string;
                       const APassword: string;
                       const ABuildState: TBuildState;
                       const ALogger: TLogManager); overload;
    constructor Create(const AJenkinsURL: string;
                       const AReleasesDir: string;
                       const AUsername: string;
                       const APassword: string;
                       const AProxyHost: string;
                       const AProxyPort: integer;
                       const AProxyUser: string;
                       const AProxyPass: string;
                       const ABuildState: TBuildState;
                       const ALogger: TLogManager); overload;
    destructor Destroy; override;

    // Возвращает список релизов в проекте
    function GetReleasesList(var AResult: TStringList): integer;

    // Возвращает номер последнего релиза в проекте
    function GetLastReleaseNumber: string;

    // Возвращает номер последнего билда в последнем релизе
    function GetLastBuildNumber: string; overload;

    // Возвращает номер последнего билда, соответствующего заданному состоянию, в последнем релизе
    function GetLastBuildNumber(const ABuildState: TBuildState): string; overload;

    // Возвращает номер последнего билда, соответствующего заданному состоянию, в заданном релизе
    function GetLastBuildNumber(const AReleaseNumber: string;
                                const ABuildState: TBuildState): string; overload;

    // Возвращает список артефактов в последнем билде последнего релиза
    function GetArtifactsList(var AResult: TStringList): integer; overload;

    // Возвращает список артефактов в заданном билде заданного релиза
    function GetArtifactsList(var AResult: TStringList;
                              const AReleaseNumber: string;
                              const ABuildNumber: string): integer; overload;

    // Скачивание файла
    function GetFile(const AFileURL: string;
                     const ASavedFileName: string): boolean;

    // Возвращает имя файла из URL
    function GetFileNameFromURL(const AFileURL: string): string;
  end;


const
  cBuildFieldName: array [TBuildState] of string = ('lastBuild',
                                                    'lastStableBuild',
                                                    'lastUnstableBuild',
                                                    'lastFailedBuild');

implementation

constructor TJenkinsAPI.Create(const AJenkinsURL: string;
                               const AReleasesDir: string;
                               const AUsername: string;
                               const APassword: string;
                               const ABuildState: TBuildState;
                               const ALogger: TLogManager);
begin
  FLogger := ALogger;
  FJenkinsProjectURL := StringReplace(AJenkinsURL, ' ', '%20', [rfReplaceAll]);
  FReleasesDir := StringReplace(AReleasesDir, '/', '%2F', [rfReplaceAll]);
  FUsername := AUsername;
  FPassword := APassword;
  FBuildState := ABuildState;
  FIdHTTP := TIdHTTP.Create(nil);
  FIdHTTP.Request.BasicAuthentication := true;
  FIdHTTP.Request.Username := FUsername;
  FIdHTTP.Request.Password := FPassword;
  FLogger.WriteDebugMessageToLog('Создан объект TJenkinsAPI с параметрами:');
  FLogger.WriteDebugMessageToLog('FJenkinsProjectURL = ' + FJenkinsProjectURL);
  FLogger.WriteDebugMessageToLog('FReleasesDir = ' + FReleasesDir);
  FLogger.WriteDebugMessageToLog('FUsername = ' + FUserName);
  FLogger.WriteDebugMessageToLog('FPassword = ' + FPassword);
  FLogger.WriteDebugMessageToLog('FBuildState = ' + GetLastBuildFieldName(FBuildState));
end; // Create

// ----------------------------------------------------------------------------------------------------------------------------------

constructor TJenkinsAPI.Create(const AJenkinsURL: string;
                               const AReleasesDir: string;
                               const AUsername: string;
                               const APassword: string;
                               const AProxyHost: string;
                               const AProxyPort: integer;
                               const AProxyUser: string;
                               const AProxyPass: string;
                               const ABuildState: TBuildState;
                               const ALogger: TLogManager);
begin
  Create(AJenkinsURL, AReleasesDir, AUsername, APassword, ABuildState, ALogger);
  if (AProxyHost <> '') then
  begin
    FIdHTTP.ProxyParams.ProxyServer := AProxyHost;
    FIdHTTP.ProxyParams.ProxyPort := AProxyPort;
    FIdHTTP.ProxyParams.ProxyUsername := AProxyUser;
    FIdHTTP.ProxyParams.ProxyPassword := AProxyPass;
    FLogger.WriteDebugMessageToLog('Обнаружены настройки прокси:');
    FLogger.WriteDebugMessageToLog('ProxyServer = ' + AProxyHost);
    FLogger.WriteDebugMessageToLog('ProxyPort = ' + IntToStr(AProxyPort));
    FLogger.WriteDebugMessageToLog('ProxyUsername = ' + AProxyUser);
    FLogger.WriteDebugMessageToLog('ProxyPassword = ' + AProxyPass);
  end;
end; // Create

// ----------------------------------------------------------------------------------------------------------------------------------

destructor TJenkinsAPI.Destroy;
begin
  FLogger.WriteDebugMessageToLog('Удалён объект TJenkins.');
  FLogger := nil;
  FreeAndNil(FIdHTTP);
end; // Destroy

// ----------------------------------------------------------------------------------------------------------------------------------

function TJenkinsAPI.GetLastBuildFieldName(const ABuildState: TBuildState): string;
begin
  Result := cBuildFieldName[ABuildState];
end; // GetLastBuildFieldName

// ----------------------------------------------------------------------------------------------------------------------------------

function TJenkinsAPI.GetLastBuildNumber(const AReleaseNumber: string;
                                        const ABuildState: TBuildState): string;
var
  s_Response: string;
  s_BuildState: string;
  s_FullURL: string;
begin
  s_BuildState := GetLastBuildFieldName(ABuildState);
  if (FReleasesDir = '') or (AReleaseNumber = '') then
    s_FullURL := FJenkinsProjectURL + '/api/json'
  else
    s_FullURL := FJenkinsProjectURL + '/job/' +
                 FReleasesDir +
                 AReleaseNumber + '/api/json';
    FLogger.WriteDebugMessageToLog('Определён URL апи для получения номера билда: ' +
                                   s_FullURL);
  try
    s_Response := FIdHTTP.Get(s_FullURL);
    FLogger.WriteDebugMessageToLog('Получен ответ от сервера: ' +
                                   s_Response);
    Result := ((TJSONObject.ParseJSONValue(s_Response) as TJSONObject)
              .Get(s_BuildState).JsonValue as TJSONObject)
              .Get('number')
              .JsonValue
              .Value;
    FLogger.WriteDebugMessageToLog('Получен номер билда: ' +
                                   Result);
  except
    on E: Exception do
    begin
      FLogger.WriteDebugMessageToLog('Ошибка при соединении с сервером: ' +
                                      E.Message + ' Номер билда = 0.');
      Result := '0';
    end;
  end;
end; // GetLastBuildNumber

// ----------------------------------------------------------------------------------------------------------------------------------

function TJenkinsAPI.GetLastBuildNumber(const ABuildState: TBuildState): string;
begin
  Result := GetLastBuildNumber(GetLastReleaseNumber, ABuildState);
end; // GetLastBuildNumber

// ----------------------------------------------------------------------------------------------------------------------------------

function TJenkinsAPI.GetLastBuildNumber: string;
begin
  Result := GetLastBuildNumber(FBuildState);
end; // GetLastBuildNumber

// ----------------------------------------------------------------------------------------------------------------------------------

function TJenkinsAPI.GetReleasesList(var AResult: TStringList): integer;
var
  s_Response: string;
  s_FullJobsPath: string;
  s_JobName: string;
  JSONArray: TJSONArray;
  i: integer;
begin
  AResult.Clear;
  JSONArray := nil;
  s_FullJobsPath := FJenkinsProjectURL + '/api/json';
  FLogger.WriteDebugMessageToLog('Определён URL апи для получения списка релизов: ' +
                                 s_FullJobsPath);
  try
    try
      s_Response := FIdHTTP.Get(s_FullJobsPath);
      FLogger.WriteDebugMessageToLog('Получен ответ от сервера: ' + s_Response);
      if s_Response <> '' then
      begin
        JSONArray := (TJSONObject.ParseJSONValue(s_Response) as TJSONObject)
                     .Get('jobs')
                     .JSONValue as TJSONArray;
        for i := 0 to JSONArray.Size - 1 do
        begin
          s_JobName := (JsonArray.Get(i) as TJSONObject)
                       .Get('name')
                       .JsonValue
                       .Value;
          s_JobName := StringReplace(s_JobName,
                                     FReleasesDir,
                                     '',
                                     [rfReplaceAll]);
          AResult.Add(s_JobName);
          FLogger.WriteDebugMessageToLog('Найден релиз: ' + s_JobName);
        end;
      end;
    except
      on E: Exception do
      begin
        FLogger.WriteErrorMessageToLog('Ошибка при получении списка релизов!',
                                       E.Message, E.StackTrace);
        AResult.Clear;
      end;
    end;
  finally
    Result := AResult.Count;
    FLogger.WriteDebugMessageToLog('Количество релизов: ' + IntToStr(Result));
    FreeAndNil(JSONArray);
  end;
end; // GetReleasesList

// ----------------------------------------------------------------------------------------------------------------------------------

function TJenkinsAPI.GetLastReleaseNumber: string;
var
  sl_ReleasesList: TStringList;
begin
  if FReleasesDir = '' then
    Result := ''
  else
  begin
    sl_ReleasesList := TStringList.Create;
    Result := '';
    try
      try
        GetReleasesList(sl_ReleasesList);
        if not sl_ReleasesList.Sorted then
          sl_ReleasesList.Sort;
        Result := StringReplace(sl_ReleasesList.Strings[sl_ReleasesList.Count - 1],
                                FReleasesDir,
                                '',
                                [rfReplaceAll]);
        FLogger.WriteDebugMessageToLog('Номер последнего релиза: ' + Result);
      except
        on E: Exception do
          FLogger.WriteErrorMessageToLog('Ошибка при получении номера последнего релиза!',
                                         E.Message, E.StackTrace);
      end;
    finally
      FreeAndNil(sl_ReleasesList);
    end;
  end;
end; // GetLastReleaseNumber

// ----------------------------------------------------------------------------------------------------------------------------------

function TJenkinsAPI.GetArtifactsList(var AResult: TStringList;
                                      const AReleaseNumber: string;
                                      const ABuildNumber: string): integer;
var
  s_Response: string;
  s_ArtifactsURL: string;
  s_FileName: string;
  JSONArray: TJSONArray;
  i: integer;
begin
  AResult.Clear;
  JSONArray := nil;
  if (FReleasesDir = '') or (AReleaseNumber = '') then
    s_ArtifactsURL := FJenkinsProjectURL + '/' + ABuildNumber
  else
    s_ArtifactsURL := FJenkinsProjectURL + '/job/' +
                      FReleasesDir +
                      AReleaseNumber + '/' +
                      ABuildNumber;
  FLogger.WriteDebugMessageToLog('Получен URL апи для определения списка артефактов: ' +
                                 s_ArtifactsURL);
  try
    try
      s_Response := FIdHTTP.Get(s_ArtifactsURL + '/api/json');
      FLogger.WriteDebugMessageToLog('Получен ответ от сервера: ' + s_Response);
      JSONArray := (TJSONObject.ParseJSONValue(s_Response) as TJSONObject)
                   .Get('artifacts')
                   .JsonValue as TJSONArray;
      for i := 0 to JSONArray.Size - 1 do
      begin
        s_FileName := (JSONArray.Get(i) as TJSONObject)
                      .Get('fileName')
                      .JsonValue
                      .Value;
        s_FileName := s_ArtifactsURL + '/artifact/' + s_FileName;
        AResult.Add(s_FileName);
        FLogger.WriteDebugMessageToLog('В список добавлен артефакт: ' + s_FileName);
      end;
      Result := JSONArray.Size;
    except
      on E: Exception do
      begin
        FLogger.WriteDebugMessageToLog('Ошибка при соединении с сервером: ' +
                                       E.Message + ' Номер билда = 0.');
        Result := 0;
      end;
    end;
  finally
    FreeAndNil(JSONArray);
  end;
end; // GetArtifactsList

// ----------------------------------------------------------------------------------------------------------------------------------

function TJenkinsAPI.GetArtifactsList(var AResult: TStringList): integer;
begin
  Result := GetArtifactsList(AResult, GetLastReleaseNumber, GetLastBuildNumber);
end; // GetArtifactsList

// ----------------------------------------------------------------------------------------------------------------------------------

function TJenkinsAPI.GetFile(const AFileURL: string;
                             const ASavedFileName: string): boolean;
var
  ms_MemoryStream: TMemoryStream;
begin
  ms_MemoryStream := TMemoryStream.Create;
  try
    try
      FIdHTTP.Get(AFileURL, ms_MemoryStream);
      if FileExists(ASavedFileName) then
      begin
        DeleteFile(ASavedFileName);
        FLogger.WriteDebugMessageToLog('Файл ' + ASavedFileName + ' найден и удалён!');
      end;
      ms_MemoryStream.SaveToFile(ASavedFileName);
      FLogger.WriteDebugMessageToLog('Файл ' + AFileURL + ' скачан и сохранён как ' +
                                     ASavedFileName + '!');
      Result := true;
    except
      on E: Exception do
      begin
        FLogger.WriteErrorMessageToLog('Ошибка при скачивании файла!',
                                       E.Message, E.StackTrace);
        Result := false;
      end;
    end;
  finally
    ms_MemoryStream.Free;
  end;
end; // GetFile

// ----------------------------------------------------------------------------------------------------------------------------------

function TJenkinsAPI.GetFileNameFromURL(const AFileURL: string): string;
var
  i: integer;
begin
  Result := '';
  i := length(AFileURL);
  while AFileURL[i] <> '/' do
  begin
    Result := AFileURL[i] + Result;
    dec(i)
  end;
  FLogger.WriteDebugMessageToLog('Из URL ' + AFileURL + ' получено имя файла ' +
                                  Result + '!');
end; // GetFileNameFromURL

end. // uJenkinsAPI
