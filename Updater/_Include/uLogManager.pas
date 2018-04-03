unit uLogManager;

interface

uses
  System.SysUtils, System.Classes, Winapi.Windows, IOUtils;

type
  TMessageType = (lmtDebug, lmtInfo, lmtError);

  TLogDetalization = byte;

  TLogManager = class
  private
    FTemplateLogFileName: string;
    FLogFileName: string;
    FErrorLogFileName: string;
    FLogDir: string;
    FLogDetalization: TLogDetalization;
    FIsUseErrorFile: boolean;
    FIsUseData: boolean;
    FLastDate: string;

    procedure CreateLogFile;
    function GetErrorLogFileName: string;
    function CheckCurrentDate: boolean;
    procedure AddDataToFilename;
  public
    constructor Create(const ALogDir: string;
                       const ALogFileName: string;
                       const AIsAddDataToFilename: boolean;
                       const ALogDetalization: string;
                       const AIsUseErrorFile: boolean);

    destructor Destroy; override;

    procedure WriteMessageToLog(const AMessageType: TMessageType;
                                const AMessageText: string); overload;
    procedure WriteMessageToLog(const AMessageText: string); overload;
    procedure WriteInfoMessageToLog(const AMessageText: string);
    procedure WriteDebugMessageToLog(const AMessageText: string);
    procedure WriteErrorMessageToLog(const AMessageText: string;
                                     const AErrorMessage: string;
                                     const AStackTraceMessage: string);
    procedure WriteEmptyStringToLog;
  end;

const
  cErrors: TLogDetalization = 0;
  cInfo: TLogDetalization = 1;
  cDebug: TLogDetalization = 2;
  cLogDetalization: array [0..2] of string = ('ERROR', 'INFO', 'DEBUG');

implementation

// ----------------------------------------------------------------------------------------------------------------------------------

constructor TLogManager.Create(const ALogDir: string;
                               const ALogFileName: string;
                               const AIsAddDataToFilename: boolean;
                               const ALogDetalization: string;
                               const AIsUseErrorFile: boolean);
var
  i: integer;
begin
  FLogDir := ALogDir;
  FTemplateLogFileName := ALogFileName;
  FIsUseErrorFile := AIsUseErrorFile;
  FIsUseData := AIsAddDataToFilename;
  FLastDate := DateToStr(Now());
  i := 0;
  while (cLogDetalization[i] <> UpperCase(ALogDetalization)) and (i < 3) do
    inc(i);
  if i = 3 then
    FLogDetalization := 1
  else
    FLogDetalization := i;
  TDirectory.CreateDirectory(FLogDir);
  AddDataToFileName();
  CreateLogFile();
end; // Create

// ----------------------------------------------------------------------------------------------------------------------------------

destructor TLogManager.Destroy;
begin
  inherited Destroy;
end; // Destroy

// ----------------------------------------------------------------------------------------------------------------------------------

function TLogManager.CheckCurrentDate: boolean;
begin
  Result := DateToStr(Now()) = FLastDate;
end; // CheckCurrentDate

// ----------------------------------------------------------------------------------------------------------------------------------

procedure TLogManager.AddDataToFilename;
var
  i: integer;
begin
  FLogFileName := FTemplateLogFileName;
  FErrorLogFileName := GetErrorLogFileName;
  if FIsUseData then
  begin
    i := length(FLogFileName);
    while FLogFileName[i] <> '.' do
      dec(i);
    insert('_' + FLastDate, FLogFileName, i);
    i := length(FErrorLogFileName);
    while FErrorLogFileName[i] <> '.' do
      dec(i);
    insert('_' + FLastDate, FErrorLogFileName, i);
  end;
  FLogFileName := FLogDir + '\' + FLogFileName;
  FErrorLogFileName := FLogDir + '\' + FErrorLogFileName;
end; // AddDataToFilename

// ----------------------------------------------------------------------------------------------------------------------------------

function TLogManager.GetErrorLogFileName: string;
begin
  Result := StringReplace(FTemplateLogFileName, '.log', '.err', [rfReplaceAll]);
end; // GetErrorLogFileName

// ----------------------------------------------------------------------------------------------------------------------------------

procedure TLogManager.CreateLogFile;
var
  LogFile: TextFile;
begin
  if not FileExists(FLogFileName) then
  begin
    AssignFile(LogFile, FLogFileName);
    Rewrite(LogFile);
    CloseFile(LogFile);
  end;
  if FIsUseErrorFile and not FileExists(FErrorLogFileName) then
  begin
    AssignFile(LogFile, FErrorLogFileName);
    Rewrite(LogFile);
    CloseFile(LogFile);
  end;
end; // CreateLogFile

// ----------------------------------------------------------------------------------------------------------------------------------

procedure TLogManager.WriteMessageToLog(const AMessageType: TMessageType;
                                        const AMessageText: string);
var
  LogFile: TextFile;
  s_MesType: string;
  i_LenText: integer;
begin
  if FIsUseData and not CheckCurrentDate then
  begin
    AddDataToFilename;
    CreateLogFile;
  end;
  case AMessageType of
    lmtDebug: s_MesType := 'DEBUG:';
    lmtInfo: s_MesType := 'INFO:';
    lmtError:  s_MesType := 'ERROR:';
  end;
  i_LenText := length(AMessageText);
  if i_LenText > 0 then
  begin
    AssignFile(LogFile, FLogFileName);
    Append(LogFile);
    Writeln(LogFile, DateTimeToStr(Now()):25, s_MesType:10, AMessageText:i_LenText + 10);
    Flush(LogFile);
    CloseFile(LogFile);
    if (AMessageType = lmtError) and FIsUseErrorFile then
    begin
      AssignFile(LogFile, FErrorLogFileName);
      Append(LogFile);
      Writeln(LogFile, DateTimeToStr(Now()):25, s_MesType:10, AMessageText:i_LenText + 10);
      Flush(LogFile);
      CloseFile(LogFile);
    end;
  end;
end; // WriteMessageToLog

// ----------------------------------------------------------------------------------------------------------------------------------

procedure TLogManager.WriteMessageToLog(const AMessageText: string);
var
  LogFile: TextFile;
begin
  AssignFile(LogFile, FLogFileName);
  Append(LogFile);
  Write(LogFile, #9, #9, AMessageText);
  WriteLn(LogFile);
  Flush(LogFile);
  CloseFile(LogFile);
end; // WriteMessageToLog

// ----------------------------------------------------------------------------------------------------------------------------------

procedure TLogManager.WriteInfoMessageToLog(const AMessageText: string);
begin
  if FLogDetalization >= 1 then
    WriteMessageToLog(lmtInfo, AMessageText);
end; // WriteDebugMessageToLog

// ----------------------------------------------------------------------------------------------------------------------------------

procedure TLogManager.WriteDebugMessageToLog(const AMessageText: string);
begin
  if FLogDetalization = 2 then
    WriteMessageToLog(lmtDebug, AMessageText);
end; // WriteDebugMessageToLog

// ----------------------------------------------------------------------------------------------------------------------------------

procedure TLogManager.WriteErrorMessageToLog(const AMessageText: string;
                                             const AErrorMessage: string;
                                             const AStackTraceMessage: string);
begin
  WriteMessageToLog(lmtError, AMessageText);
  WriteMessageToLog(AErrorMessage);
  WriteMessageToLog(AStackTraceMessage);
  WriteEmptyStringToLog();
end; // WriteErrorToLog

// ----------------------------------------------------------------------------------------------------------------------------------

procedure TLogManager.WriteEmptyStringToLog();
var
  LogFile: TextFile;
begin
  AssignFile(LogFile, FLogFileName);
  Append(LogFile);
  Writeln(LogFile);
  Flush(LogFile);
  CloseFile(LogFile);
end; // WriteEmptyStringToLog


end. // uLogManager
