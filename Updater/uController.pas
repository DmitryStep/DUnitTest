unit uController;

interface

uses
  uSettingsStructure, uSettingsManager, uJenkinsAPI, SysUtils;

function CheckNewRelease(const ASetManager: TSettingsManager;
                         const AProgram: TProgramSettings): string;
function CheckNewBuild(const ASetManager: TSettingsManager;
                       const AProgram: TProgramSettings;
                       const ARelease: string): string;

implementation

function CheckNewRelease(const ASetManager: TSettingsManager;
                         const AProgram: TProgramSettings): string;
var
  Jenk: TJenkinsAPI;
  s_NewRelease: string;
begin
  Result := '';
  if Assigned(ASetManager) then
  begin
    Jenk.Create(ASetManager.Jenkins.s_URL, ASetManager.Jenkins.s_User, ASetManager.Jenkins.s_Password);
    try
      s_NewRelease := Jenk.GetLastReleaseNumber(AProgram.s_SourcePath, AProgram.s_ReleaseTemplate);
      if s_NewRelease <> ASetManager.Release.s_ReleaseNumber then
        Result := s_NewRelease;
    finally
      FreeAndNil(Jenk);
    end;
  end;
end;

function CheckNewBuild(const ASetManager: TSettingsManager;
                       const AProgram: TProgramSettings;
                       const ARelease: string): string;
var
  Jenk: TJenkinsAPI;
  s_NewBuild: string;
begin
  Result := '';
  if Assigned(ASetManager) then
  begin
    Jenk.Create(ASetManager.Jenkins.s_URL, ASetManager.Jenkins.s_User, ASetManager.Jenkins.s_Password);
    try
      s_NewBuild := Jenk.GetLastBuildNumberStr(bsStable, AProgram.s_SourcePath, AProgram.s_ReleaseTemplate + ARelease);
      if s_NewBuild <> AProgram.s_LastBuild then
        Result := s_NewBuild;
    finally
      FreeAndNil(Jenk);
    end;
  end;
end;


end.
