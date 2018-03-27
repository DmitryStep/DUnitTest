unit TestuJenkinsAPI;
{

  Delphi DUnit Test Case
  ----------------------
  This unit contains a skeleton test case class generated by the Test Case Wizard.
  Modify the generated code to correctly setup and call the methods from the unit 
  being tested.

}

interface

uses
  TestFramework, System.SysUtils, DBXJSON, IdHTTP, IdTCPClient, uJenkinsAPI,
  System.Classes, IdTCPConnection, Forms;

type
  // Test methods for class TJenkinsAPI

  TestTJenkinsAPI = class(TTestCase)
  strict private
    FJenkinsAPI: TJenkinsAPI;
    FJenkinsAPI1: TJenkinsAPI;
    FResult: TStringList;
    FFileName: string;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGetReleasesList;
    procedure TestGetLastReleaseNumber;
    procedure TestGetLastBuildNumber;
    procedure TestGetLastBuildNumber1;
    procedure TestGetLastBuildNumber2;
    procedure TestGetLastBuildNumber3;
    procedure TestGetArtifactsList;
    procedure TestGetArtifactsList1;
    procedure TestGetArtifactsList2;
    procedure TestGetFileNameFromURL;
    procedure TestGetFile;
    procedure TestGetFile1;
  end;

implementation

procedure TestTJenkinsAPI.SetUp;
begin
  FFileName := '';
  FJenkinsAPI := TJenkinsAPI.Create('http://test.ils-glonass.ru:8080/job/ILS Web Project/job/ILSWebLogistic_Release',
                                    'release/',
                                    'DStepanov',
                                    'DS123',
                                    bsAll);
  FJenkinsAPI1 := TJenkinsAPI.Create('http://test.ils-glonass.ru:8080/job/ILS%20Web%20Project/job/ILSWebLogistic_Master',
                                    '',
                                    'DStepanov',
                                    'DS123',
                                    bsAll);
  FResult := TStringList.Create;
end;

procedure TestTJenkinsAPI.TearDown;
begin
  if (FFileName <> '') and FileExists(FFileName) then
    DeleteFile(FFileName);
  FreeAndNil(FResult);
  FreeAndNil(FJenkinsAPI);
  FreeAndNil(FJenkinsAPI1);
end;

procedure TestTJenkinsAPI.TestGetReleasesList;
var
  ReturnValue: integer;
begin
  ReturnValue := FJenkinsAPI.GetReleasesList(FResult);
  CheckEquals(1, ReturnValue, '');
end;

procedure TestTJenkinsAPI.TestGetLastReleaseNumber;
var
  ReturnValue: string;
begin
  ReturnValue := FJenkinsAPI.GetLastReleaseNumber;
  CheckEquals('7.1.4', ReturnValue, '');
end;

procedure TestTJenkinsAPI.TestGetLastBuildNumber;
var
  ReturnValue: string;
begin
  ReturnValue := FJenkinsAPI.GetLastBuildNumber;
  CheckEquals('15', ReturnValue, '');
end;

procedure TestTJenkinsAPI.TestGetArtifactsList;
var
  ReturnValue: integer;
begin
  ReturnValue := FJenkinsAPI.GetArtifactsList(FResult);
  CheckEquals('http://test.ils-glonass.ru:8080/job/ILS%20Web%20Project/job/ILSWebLogistic_Release/job/release%2F7.1.4/15/artifact/Release.zip', FResult.Strings[0], '');
end;

procedure TestTJenkinsAPI.TestGetFile;
var
  ReturnValue: Boolean;
  Result: boolean;
begin
  FJenkinsAPI.GetArtifactsList(FResult);
  FFileName := ExtractFilePath(Application.ExeName) +
               FJenkinsAPI.GetFileNameFromURL(FResult.Strings[0]);
  ReturnValue := FJenkinsAPI.GetFile(FResult.Strings[0], FFileName);
  Result := ReturnValue and FileExists(FFileName);
  CheckTrue(Result);
end;

procedure TestTJenkinsAPI.TestGetFile1;
var
  ReturnValue: Boolean;
  Result: boolean;
begin
  FJenkinsAPI1.GetArtifactsList(FResult);
  FFileName := ExtractFilePath(Application.ExeName) +
               FJenkinsAPI1.GetFileNameFromURL(FResult.Strings[0]);
  ReturnValue := FJenkinsAPI1.GetFile(FResult.Strings[0], FFileName);
  Result := ReturnValue and FileExists(FFileName);
  CheckTrue(Result);
end;

procedure TestTJenkinsAPI.TestGetArtifactsList1;
var
  ReturnValue: Integer;
begin
  ReturnValue := FJenkinsAPI.GetArtifactsList(FResult, '7.1.4', '6');
  CheckEquals(1, ReturnValue, '');
  CheckEquals('http://test.ils-glonass.ru:8080/job/ILS%20Web%20Project/job/ILSWebLogistic_Release/job/release%2F7.1.4/6/artifact/Release.zip', FResult.Strings[0], '');
end;


procedure TestTJenkinsAPI.TestGetArtifactsList2;
var
  ReturnValue: Integer;
begin
  ReturnValue := FJenkinsAPI1.GetArtifactsList(FResult, '', '6');
  CheckEquals(1, ReturnValue, '');
  CheckEquals('http://test.ils-glonass.ru:8080/job/ILS%20Web%20Project/job/ILSWebLogistic_Master/6/artifact/Release.zip', FResult.Strings[0], '');
end;


procedure TestTJenkinsAPI.TestGetLastBuildNumber1;
var
  ReturnValue: string;
begin
  ReturnValue := FJenkinsAPI.GetLastBuildNumber(bsFailed);
  CheckEquals('0', ReturnValue, '');
end;

procedure TestTJenkinsAPI.TestGetLastBuildNumber2;
var
  ReturnValue: string;
begin
  ReturnValue := FJenkinsAPI.GetLastBuildNumber('7.1.4', bsStable);
  CheckEquals('15', ReturnValue, '');
end;

procedure TestTJenkinsAPI.TestGetLastBuildNumber3;
var
  ReturnValue: string;
begin
  ReturnValue := FJenkinsAPI1.GetLastBuildNumber('', bsStable);
  CheckEquals('11', ReturnValue, '');
end;

procedure TestTJenkinsAPI.TestGetFileNameFromURL;
var
  ReturnValue: string;
begin
  FJenkinsAPI.GetArtifactsList(FResult);
  ReturnValue := FJenkinsAPI.GetFileNameFromURL(FResult.Strings[0]);
  CheckEquals('Release.zip', ReturnValue, '');
end;

initialization
  RegisterTest(TestTJenkinsAPI.Suite);
end.
