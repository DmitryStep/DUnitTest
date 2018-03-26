unit uShellAPI;

interface

uses
 ShellApi, ActiveX, Windows, SysUtils;

const
  { Дублируем ShowWindow() Commands, чтоб не подключать каждый раз ShellApi вместе с uShellAPI}
  SW_HIDE = 0;
  SW_SHOW = 5;


procedure ShellExecute(const AWnd: HWND;
                       const AOperation, AFileName: String;
                       const AParameters: String = '';
                       const ADirectory: String = '';
                       const AShowCmd: Integer = SW_SHOWNORMAL);

procedure WinExec(const ACmdLine: String;
                  const ACmdShow: UINT = SW_SHOWNORMAL);

procedure CopyFiles(const ASourceDir: string; const ADestDir: string);
procedure ArchiveFiles(const ASourceFiles: string; const ADestArchive: string);
procedure UnarchiveFiles(const ASourceArchive: string; const ADestPath: string);


implementation


// Copyright from http://www.gunsmoker.ru/2015/01/never-use-ShellExecute.html
procedure ShellExecute(const AWnd: HWND;
                       const AOperation, AFileName: String;
                       const AParameters: String = '';
                       const ADirectory: String = '';
                       const AShowCmd: Integer = SW_SHOWNORMAL);
var
  ExecInfo: TShellExecuteInfo;
  NeedUnitialize: Boolean;
begin
  Assert(AFileName <> '');
  NeedUnitialize := Succeeded(CoInitializeEx(nil, COINIT_APARTMENTTHREADED or
                                                  COINIT_DISABLE_OLE1DDE));
  try
    FillChar(ExecInfo, SizeOf(ExecInfo), 0);
    ExecInfo.cbSize := SizeOf(ExecInfo);

    ExecInfo.Wnd := AWnd;
    ExecInfo.lpVerb := Pointer(AOperation);
    ExecInfo.lpFile := PChar(AFileName);
    ExecInfo.lpParameters := Pointer(AParameters);
    ExecInfo.lpDirectory := Pointer(ADirectory);
    ExecInfo.nShow := AShowCmd;
    ExecInfo.fMask := SEE_MASK_NOASYNC or SEE_MASK_FLAG_NO_UI;
    {$IFDEF UNICODE}
    ExecInfo.fMask := ExecInfo.fMask or SEE_MASK_UNICODE;
    {$ENDIF}
    {$WARN SYMBOL_PLATFORM OFF}
    Win32Check(ShellExecuteEx(@ExecInfo));
    {$WARN SYMBOL_PLATFORM ON}
  finally
    if NeedUnitialize then
      CoUninitialize;
  end;
end; // ShellExecute


// Copyright from http://www.gunsmoker.ru/2015/01/never-use-ShellExecute.html
procedure WinExec(const ACmdLine: String;
                  const ACmdShow: UINT = SW_SHOWNORMAL);
var
  SI: TStartupInfo;
  PI: TProcessInformation;
  CmdLine: String;
begin
  Assert(ACmdLine <> '');

  CmdLine := ACmdLine;
  UniqueString(CmdLine);

  FillChar(SI, SizeOf(SI), 0);
  FillChar(PI, SizeOf(PI), 0);
  SI.cb := SizeOf(SI);
  SI.dwFlags := STARTF_USESHOWWINDOW;
  SI.wShowWindow := ACmdShow;

  SetLastError(ERROR_INVALID_PARAMETER);
  {$WARN SYMBOL_PLATFORM OFF}
  Win32Check(CreateProcess(nil, PChar(CmdLine), nil, nil, False,
                           CREATE_DEFAULT_ERROR_MODE
                           {$IFDEF UNICODE}
                           or CREATE_UNICODE_ENVIRONMENT
                           {$ENDIF},
                           nil, nil, SI, PI));
  {$WARN SYMBOL_PLATFORM ON}
  CloseHandle(PI.hThread);
  CloseHandle(PI.hProcess);
end; // WinExec


procedure CopyFiles(const ASourceDir: string; const ADestDir: string);
const
  cCmdCopyTemplate = 'xcopy "<SOURCE>" "<DESTINATION>" /E /Y';
var
  s_CmdLine: string;
begin
  s_CmdLine := StringReplace(cCmdCopyTemplate, '<SOURCE>',
                             ASourceDir, [rfReplaceAll]);
  s_CmdLine := StringReplace(s_CmdLine, '<DESTINATION>',
                             ADestDir, [rfReplaceAll]);
  if pos('*.*', ASourceDir) = 0 then
    s_CmdLine := StringReplace(s_CmdLine, '/E', '', [rfReplaceAll]);
  WinExec(s_CmdLine, SW_HIDE);
end; // CopyFiles


procedure ArchiveFiles(const ASourceFiles: string; const ADestArchive: string);
const
  cCmdArchTemplate = '"c:\Program Files\7-zip\7z.exe" a -sdel -tzip -ssw -mx7 "<DESTINATION>" "<SOURCE>"';
var
  s_CmdLine: string;
begin
  s_CmdLine := StringReplace(cCmdArchTemplate, '<SOURCE>',
                             ASourceFiles, [rfReplaceAll]);
  s_CmdLine := StringReplace(s_CmdLine, '<DESTINATION>',
                             ADestArchive, [rfReplaceAll]);
  WinExec(s_CmdLine, SW_HIDE);
end; // ArchiveFiles


procedure UnarchiveFiles(const ASourceArchive: string; const ADestPath: string);
const
  cCmdUnArchTemplate = '"c:\Program Files\7-zip\7z.exe" x -o"<DESTINATION>" -y -aoa "<SOURCE>"';
var
  s_CmdLine: string;
begin
  s_CmdLine := StringReplace(cCmdUnArchTemplate, '<SOURCE>',
                             ASourceArchive, [rfReplaceAll]);
  s_CmdLine := StringReplace(s_CmdLine, '<DESTINATION>',
                             ADestPath, [rfReplaceAll]);
  WinExec(s_CmdLine, SW_HIDE);
end; // UnarchiveFiles

end.
