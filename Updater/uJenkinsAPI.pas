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
    FJenkinsURL: string;
    FUsername: string;
    FPassword: string;
    function SetAuthParamsForRequest: boolean;
    function GetFullJobsPath(const AJobsPath: string): string;
    procedure EnumJSONPairs(var AResult: TStringList; const AJSONObject: TJSONObject);
    function GetBuildFieldName(const ABuildState: TBuildState): string;
  public
    constructor Create(const AJenkinsURL, AUsername, APassword: string);
    destructor Destroy;
    function GetJobsList(var AResult: string;
                         const AJobsPath: string): integer; overload;
    function GetJobsList(var AResult: TJSONObject;
                         const AJobsPath: string): integer; overload;
    function GetJobsList(var AResult: TStringList;
                         const AJobsPath: string): integer; overload;
    function GetLastBuildNumberInt(const ABuildState: TBuildState;
                                   const AReleasePath: string;
                                   const ABuildsPath: string): integer;
    function GetLastBuildNumberStr(const ABuildState: TBuildState;
                                   const AReleasePath: string;
                                   const ABuildsPath: string): string;
    function GetLastReleaseNumber(const AReleaseJobsPath: string;
                                  const ANotReleaseNumberTemplate: string): string;
    function GetLastReleaseName(const AReleaseJobsPath: string): string;
    function GetArtifactFileName(const AReleasePath: string;
                                 const ABuildsPath: string;
                                 const ABuildNumber: string): string;
    function GetFile(const AReleasePath: string;
                     const ABuildsPath: string;
                     const ABuildNumber: string;
                     const AFileName: string;
                     const ASavedFileName: string): boolean;
  end;

implementation


constructor TJenkinsAPI.Create(const AJenkinsURL, AUsername, APassword: string);
begin
  self.FJenkinsURL := AJenkinsURL;
  self.FUsername := AUsername;
  self.FPassword := APassword;
  self.FIdHTTP := TIdHTTP.Create(nil);
end;


function TJenkinsAPI.SetAuthParamsForRequest: boolean;
begin
  FIdHTTP.Request.BasicAuthentication := true;
  FIdHTTP.Request.Username := FUsername;
  FIdHTTP.Request.Password := FPassword;
end;


procedure TJenkinsAPI.EnumJSONPairs(var AResult: TStringList; const AJSONObject: TJSONObject);
var
  i: integer;
  ResStr: string;
begin
  for I := 0 to AJSONObject.Size - 1 do
  begin
    ResStr := StringReplace(AJSONObject.Get(i).ToString, '"','', [rfReplaceAll]);
    ResStr := StringReplace(ResStr, ':', '=', [rfReplaceAll]);
    AResult.Add(ResStr);
  end;
end;


function TJenkinsAPI.GetBuildFieldName(const ABuildState: TBuildState): string;
begin
  case ABuildState of
    bsStable: Result := 'lastStableBuild';
    bsUnstable: Result := 'lastUnstableBuild';
    bsFailed: Result := 'lastFailedBuild';
  else
    Result := 'lastBuild';
  end;
end;


function TJenkinsAPI.GetFullJobsPath(const AJobsPath: string): string;
begin
  Result := StringReplace(AJobsPath, '/', '/job/', [rfReplaceAll]);
  Result := '/job/' + Result;
  Result := StringReplace(Result, ' ', '%20', [rfReplaceAll]);
end;


function TJenkinsAPI.GetJobsList(var AResult: string;
                                 const AJobsPath: string): integer;
var
  s_Response: string;
  s_FullJobsPath: string;
begin
  AResult := '';
  Result := 0;
  SetAuthParamsForRequest;
  s_FUllJobsPath := FJenkinsURL + GetFullJobsPath(AJobsPath) + '/api/json';
  try
    try
      s_Response := FIdHTTP.Get(s_FullJobsPath);
      AResult := (TJSONObject.ParseJSONValue(s_Response) as TJSONObject).Get('jobs').JsonValue.ToString;
    except
      on E: Exception do
        AResult := E.Message;
    end;
  finally
    Result := FIdHTTP.ResponseCode;
    if AResult = '' then
      AResult := FIdHTTP.ResponseText;
  end;
end;


function TJenkinsAPI.GetJobsList(var AResult: TJSONObject;
                                 const AJobsPath: string): integer;
var
  s_Response: string;
  s_FullJobsPath: string;
begin
  AResult := nil;
  Result := 0;
  SetAuthParamsForRequest;
  s_FUllJobsPath := FJenkinsURL + GetFullJobsPath(AJobsPath) + '/api/json';
  try
    try
      s_Response := FIdHTTP.Get(s_FullJobsPath);
    finally
      if s_Response <> '' then
        AResult := (TJSONObject.ParseJSONValue(s_Response) as TJSONObject).Get('jobs').JSONValue as TJSONObject;
    end;
  finally
    Result := FIdHTTP.ResponseCode;
  end;
end;


function TJenkinsAPI.GetJobsList(var AResult: TStringList;
                                 const AJobsPath: string): integer;
var
  s_Response: string;
  s_FullJobsPath: string;
  s_JobName: string;
  JSONArray: TJSONArray;
  i: integer;
begin
  AResult.Clear;
  Result := 0;
  SetAuthParamsForRequest;
  s_FUllJobsPath := FJenkinsURL + GetFullJobsPath(AJobsPath) + '/api/json';
  try
    try
      s_Response := FIdHTTP.Get(s_FullJobsPath);
    finally
      if s_Response <> '' then
      begin
        JSONArray := (TJSONObject.ParseJSONValue(s_Response) as TJSONObject).Get('jobs').JSONValue as TJSONArray;
        for i := 0 to JSONArray.Size - 1 do
        begin
          s_JobName := (JsonArray.Get(i) as TJSONObject).Get('name').JsonValue.Value;
          s_JobName := StringReplace(s_JobName, '%2F', '/', [rfReplaceAll]);
          AResult.Add(s_JobName);
        end;
      end;
    end;
  finally
    Result := FIdHTTP.ResponseCode;
    if Assigned(JSONArray) then
      JSONArray.Free;
  end;
end;


function TJenkinsAPI.GetLastBuildNumberStr(const ABuildState: TBuildState;
                                           const AReleasePath: string;
                                           const ABuildsPath: string): string;
var
  s_Response: string;
  s_BuildState: string;
  s_FullBuildsPath: string;
begin
  Result := '0';
  s_BuildState := GetBuildFieldName(ABuildState);
  s_FullBuildsPath := StringReplace(ABuildsPath, '/', '%2F', [rfReplaceAll]);
  s_FullBuildsPath := FJenkinsURL + GetFullJobsPath(AReleasePath + s_FullBuildsPath) + '/api/json';
  s_Response := FIdHTTP.Get(s_FullBuildsPath);
  Result := ((TJSONObject.ParseJSONValue(s_Response) as TJSONObject).Get(s_BuildState).JsonValue as TJSONObject).Get('number').JsonValue.Value;
end;


function TJenkinsAPI.GetLastBuildNumberInt(const ABuildState: TBuildState; const AReleasePath: string; const ABuildsPath: string): integer;
begin
  Result := StrToInt(GetLastBuildNumberStr(ABuildState, AReleasePath, ABuildsPath));
end;


function TJenkinsAPI.GetLastReleaseNumber(const AReleaseJobsPath: string;
                                          const ANotReleaseNumberTemplate: string): string;
var
  sl_ReleasesList: TStringList;
begin
  sl_ReleasesList := TStringList.Create;
  Result := '';
  try
    GetJobsList(sl_ReleasesList, AReleaseJobsPath);
    if not sl_ReleasesList.Sorted then
      sl_ReleasesList.Sort;
    Result := sl_ReleasesList.Strings[sl_ReleasesList.Count - 1];
    Result := Trim(StringReplace(Result, ANotReleaseNumberTemplate, '', [rfReplaceAll]));
  finally
    FreeAndNil(sl_ReleasesList);
  end;
end;

function TJenkinsAPI.GetLastReleaseName(const AReleaseJobsPath: string): string;
var
  sl_ReleasesList: TStringList;
begin
  sl_ReleasesList := TStringList.Create;
  Result := '';
  try
    GetJobsList(sl_ReleasesList, AReleaseJobsPath);
    if not sl_ReleasesList.Sorted then
      sl_ReleasesList.Sort;
    Result := sl_ReleasesList.Strings[sl_ReleasesList.Count - 1];
  finally
    FreeAndNil(sl_ReleasesList);
  end;
end;


function TJenkinsAPI.GetArtifactFileName(const AReleasePath: string;
                                         const ABuildsPath: string;
                                         const ABuildNumber: string): string;
var
  s_Response: string;
  s_FullBuildsPath: string;
begin
  Result := '';
  s_FullBuildsPath := StringReplace(ABuildsPath, '/', '%2F', [rfReplaceAll]);
  s_FullBuildsPath := FJenkinsURL + GetFullJobsPath(AReleasePath) + s_FullBuildsPath + '/' + ABuildNumber + '/api/json';
  try
    s_Response := FIdHTTP.Get(s_FullBuildsPath);
    Result := (((TJSONObject.ParseJSONValue(s_Response) as TJSONObject).Get('artifacts').JsonValue as TJSONArray).Get(0) as TJSONObject).Get('fileName').JsonValue.Value
  except
    Result := '';
  end;
end;


function TJenkinsAPI.GetFile(const AReleasePath: string;
                             const ABuildsPath: string;
                             const ABuildNumber: string;
                             const AFileName: string;
                             const ASavedFileName: string): boolean;
var
  s_FullFilePath: string;
  ms_MemoryStream: TMemoryStream;
begin
  SetAuthParamsForRequest;
  s_FullFilePath := StringReplace(ABuildsPath, '/', '%2F', [rfReplaceAll]);
  s_FullFilePath := FJenkinsURL + GetFullJobsPath(AReleasePath) + s_FullFilePath + '/' + ABuildNumber + '/artifact/' + AFileName;
  ms_MemoryStream := TMemoryStream.Create;
  try
    try
      FIdHTTP.Get(s_FullFilePath, ms_MemoryStream);
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
end;


destructor TJenkinsAPI.Destroy;
begin
  FreeAndNil(FIdHTTP);
end;

end.
