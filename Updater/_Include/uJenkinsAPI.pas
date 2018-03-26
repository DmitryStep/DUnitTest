unit uJenkinsAPI;

interface

uses
  IdTCPConnection, IdTCPClient, IdHTTP, System.SysUtils, System.Classes,
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

    // Возвращает наименование поля для получения последнего билда
    // ABuildState - значение определяет "успешность" запрашиваемого билда
    // (см. тип TBuildState и константу cBuildFieldName)
    function GetLastBuildFieldName(const ABuildState: TBuildState): string;
  public
    constructor Create(const AJenkinsURL: string;
                       const AReleasesDir: string;
                       const AUsername: string;
                       const APassword: string;
                       const ABuildState: TBuildState);
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
                               const ABuildState: TBuildState);
begin
  FJenkinsProjectURL := StringReplace(AJenkinsURL, ' ', '%20', [rfReplaceAll]);
  FReleasesDir := StringReplace(AReleasesDir, '/', '%2F', [rfReplaceAll]);
  FUsername := AUsername;
  FPassword := APassword;
  FBuildState := ABuildState;
  FIdHTTP := TIdHTTP.Create(nil);
  FIdHTTP.Request.BasicAuthentication := true;
  FIdHTTP.Request.Username := FUsername;
  FIdHTTP.Request.Password := FPassword;
end; // Create


destructor TJenkinsAPI.Destroy;
begin
  FreeAndNil(FIdHTTP);
end; // Destroy


function TJenkinsAPI.GetLastBuildFieldName(const ABuildState: TBuildState): string;
begin
  Result := cBuildFieldName[ABuildState];
end; // GetLastBuildFieldName


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
  try
    s_Response := FIdHTTP.Get(s_FullURL);
    Result := ((TJSONObject.ParseJSONValue(s_Response) as TJSONObject)
              .Get(s_BuildState).JsonValue as TJSONObject)
              .Get('number')
              .JsonValue
              .Value;
  except
    Result := '0';
  end;
end; // GetLastBuildNumber


function TJenkinsAPI.GetLastBuildNumber(const ABuildState: TBuildState): string;
var
  s_Response: string;
  s_BuildState: string;
  s_FullURL: string;
begin
  Result := GetLastBuildNumber(GetLastReleaseNumber, ABuildState);
end; // GetLastBuildNumber


function TJenkinsAPI.GetLastBuildNumber: string;
begin
  Result := GetLastBuildNumber(FBuildState);
end; // GetLastBuildNumber


function TJenkinsAPI.GetReleasesList(var AResult: TStringList): integer;
var
  s_Response: string;
  s_FullJobsPath: string;
  s_JobName: string;
  JSONArray: TJSONArray;
  i: integer;
begin
  AResult.Clear;
  Result := 0;
  JSONArray := nil;
  s_FullJobsPath := FJenkinsProjectURL + '/api/json';
  try
    try
      s_Response := FIdHTTP.Get(s_FullJobsPath);
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
        end;
      end;
    except
      AResult.Clear;
    end;
  finally
    Result := AResult.Count;
    FreeAndNil(JSONArray);
  end;
end; // GetReleasesList


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
      GetReleasesList(sl_ReleasesList);
      if not sl_ReleasesList.Sorted then
        sl_ReleasesList.Sort;
      Result := StringReplace(sl_ReleasesList.Strings[sl_ReleasesList.Count - 1],
                              FReleasesDir,
                              '',
                              [rfReplaceAll]);
    finally
      FreeAndNil(sl_ReleasesList);
    end;
  end;
end; // GetLastReleaseNumber


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
  Result := 0;
  AResult.Clear;
  JSONArray := nil;
  if (FReleasesDir = '') or (AReleaseNumber = '') then
    s_ArtifactsURL := FJenkinsProjectURL + '/' + ABuildNumber
  else
    s_ArtifactsURL := FJenkinsProjectURL + '/job/' +
                      FReleasesDir +
                      AReleaseNumber + '/' +
                      ABuildNumber;
  try
    try
      s_Response := FIdHTTP.Get(s_ArtifactsURL + '/api/json');
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
      end;
      Result := JSONArray.Size;
    except
      Result := 0;
    end;
  finally
    FreeAndNil(JSONArray);
  end;
end; // GetArtifactsList


function TJenkinsAPI.GetArtifactsList(var AResult: TStringList): integer;
begin
  Result := GetArtifactsList(AResult, GetLastReleaseNumber, GetLastBuildNumber);
end; // GetArtifactsList


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
        DeleteFile(ASavedFileName);
      ms_MemoryStream.SaveToFile(ASavedFileName);
      Result := true;
    except
      Result := false;
    end;
  finally
    ms_MemoryStream.Free;
  end;
end; // GetFile


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
end; // GetFileNameFromURL

end.
