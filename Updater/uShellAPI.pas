unit uShellAPI;

interface

uses
 ShellApi, ActiveX, Windows, SysUtils;

procedure ShellExecute(const AWnd: HWND;
                       const AOperation, AFileName: String;
                       const AParameters: String = '';
                       const ADirectory: String = '';
                       const AShowCmd: Integer = SW_SHOWNORMAL);
procedure WinExec(const ACmdLine: String;
                  const ACmdShow: UINT = SW_SHOWNORMAL);
procedure UnpackFiles(const ASourceZip: string;
                      const ADestinationFolder: string);
procedure CopyFiles(const ASource: string; const ADestination: string);


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
end;


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
end;


procedure UnpackFiles(const ASourceZip: string;
                      const ADestinationFolder: string);
begin

end;


procedure CopyFiles(const ASource: string; const ADestination: string);
begin

end;


end.
