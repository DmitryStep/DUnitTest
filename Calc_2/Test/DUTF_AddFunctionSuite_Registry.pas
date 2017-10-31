// DUnit Test Framework Additional Functional Suite (DUTF_AddFunctionSuite)
//
// ������ ��������������� ����������� ��� DUnit
// ��������� ����� �������, ����������� ����������� DUnit �� ������ � ��������
//
// �����: �.�. ��������
// 09.02.2017
//
// ������� ���������:
//
// 12.02.2017 - ��������� ��������� � ������� ��� ������ � �������� �� ����-������ "� ���� �������"
//
// 21.03.2017 - ��������� ������� ������� � �������� ������ ������� ��� �������������� �� ����� ���������� ���������

//======================================Begin DUTF_AddFunctionSuite_Registry ===================================
unit DUTF_AddFunctionSuite_Registry;

//==============================================���������==============================================
interface

uses
  Winapi.Windows, Winapi.ShellApi, System.SysUtils, System.Win.Registry;

const
  // ������ ��������� �������
  BufSize = 1000;

type

  // ���, ����������� ����� �������� ������ ��� ������ � �������� (��� BINARY)
  TBinaryBuffer = array [0..BufSize-1] of Byte;

  // ����� �������� � �������, ����������� ������ � �������� �� ����-������.
  // ������������ ��� ���������� �������� � �������� �� ����-����� � ���� �������.
  // ������ ��������� � ������� ��������� �������� ������� TRgistry, ���������� �������� �
  // ����������� ���������� �������.
  // ����� �����������, ����� �� �������� � ������������� �������� ��������� � ������� ������/������
  // ���������� ������� ������ ����� (������ ����������� ������ binary, integer � string).
  //
  // ������� ���������:
  // Root - �������� ������� �������;
  // BranchPath - ������ ���� �� �����, ������� ��� ������ �����;
  // ParamName - ��� ��������� � �������;
  // ParamValue - �������� ���������.
  //
  // �������� ��������� �������:
  // boolean - ��������� ���������� �������;
  // integer, string - �������� ������������ ���������.

  function IsRegistryBranchExists (Root: HKEY; BranchPath: string): Boolean;
  function IsRegistryParamExists (Root: HKEY; BranchPath: string; ParamName: string): Boolean;
  function CreateRegistryBranch (Root: HKEY; BranchPath: string): boolean;
  function DeleteRegistryBranch (Root: HKEY; BranchPath: string): boolean;
  procedure RenameRegistryParam  (Root: HKEY; BranchPath: string; ParamName: string; NewParamName: string);
  function ReadStringParamFromRegistry (Root: HKEY; BranchPath: string; ParamName: string): string;
  procedure WriteStringParamToRegistry (Root: HKEY; BranchPath: string; ParamName: string;
                                        ParamValue: string);
  function ReadIntParamFromRegistry (Root: HKEY; BranchPath: string; ParamName: string): integer;
  procedure WriteIntParamToRegistry (Root: HKEY; BranchPath: string; ParamName: string;
                                     ParamValue: Integer);
  procedure ReadBinaryParamFromRegistry (Root: HKEY; BranchPath: string;
                                         ParamName: string; var ParamValue: TBinaryBuffer);
  procedure WriteBinaryParamToRegistry (Root: HKEY; BranchPath: string; ParamName: string;
                                        ParamValue: TBinaryBuffer);
  function ExportRegistryBranchToFile (Root: HKEY; BranchPath: string; FileName: string): boolean;
  function ImportRegistryBranchFromFile (Root: HKEY; BranchPath: string; FileName: string): boolean;

// End of RegistryWork procedures


//==============================================�������� �������� � �������============================
implementation

//====================================Begin Registry Work==============================================

// �������� ������������� ����� �������
function IsRegistryBranchExists (Root: HKEY; BranchPath: string): Boolean;
var
  Reg: TRegistry;

begin

  Result := false;

  Reg := nil;
  Reg := TRegistry.Create();

  try

    Reg.RootKey := Root;
    Result := Reg.KeyExists(BranchPath);

  finally

    FreeAndNil(Reg);

  end;

end; // IsRegistryBranchExists


// �������� ������������� ��������� � �������
function IsRegistryParamExists (Root: HKEY; BranchPath: string; ParamName: string): Boolean;
var
  Reg: TRegistry;

begin

  Result := false;

  if IsRegistryBranchExists (Root, BranchPath) then
  begin

    Reg := nil;
    Reg := TRegistry.Create();

    try

      Reg.RootKey := Root;
      Reg.OpenKey(BranchPath, false);
      Result := Reg.ValueExists(ParamName);

    finally

      FreeAndNil(Reg);

    end;

  end;


end; // IsRegistryParamExists


// �������� ����� �������
function CreateRegistryBranch (Root: HKEY; BranchPath: string): boolean;
var
  Reg: TRegistry;

begin

  Result := false;

  if not IsRegistryBranchExists (Root, BranchPath) then
  begin

    Reg := nil;
    Reg := TRegistry.Create();

    try

      Reg.RootKey := Root;
      Result := Reg.CreateKey(BranchPath);

    finally

      FreeAndNil(Reg);

    end;

  end;

end; // CreateRegistryBranch


// �������� ����� �������
function DeleteRegistryBranch (Root: HKEY; BranchPath: string): Boolean;
var
  Reg: TRegistry;

begin

  Result := false;

  if IsRegistryBranchExists (Root, BranchPath) then
  begin

    Reg := nil;
    Reg := TRegistry.Create();

    try

      Reg.RootKey := Root;
      Result := Reg.DeleteKey(BranchPath);

    finally

      FreeAndNil(Reg);

    end;

  end;

end; // DeleteRegistryBranch


// �������������� ��������� ������� - ��� ���������� ������� ������ ����� ������� �����
procedure RenameRegistryParam  (Root: HKEY; BranchPath: string; ParamName: string; NewPAramName: string);
var
  Reg: TRegistry;

begin

  if IsRegistryParamExists (Root, BranchPath, ParamName) then
  begin

    Reg := nil;
    Reg := TRegistry.Create(KEY_READ);

    try

      Reg.RootKey := Root;
      Reg.OpenKey(BranchPath,false);
      Reg.RenameValue(ParamName, NewParamName);

    finally

      Reg.CloseKey();
      FreeAndNil(Reg);

    end;

  end;

end;


// ������ ���������� �������� ��������� �������
function ReadStringParamFromRegistry (Root: HKEY; BranchPath: string; ParamName: string): string;
var
  Reg: TRegistry;

begin

  Result := '';

  if IsRegistryParamExists (Root, BranchPath, ParamName) then
  begin

    Reg := nil;
    Reg := TRegistry.Create(KEY_READ);

    try

      Reg.RootKey := Root;
      Reg.OpenKey(BranchPath,false);
      Result := Reg.ReadString(ParamName);

    finally

      Reg.CloseKey();
      FreeAndNil(Reg);

    end;

  end;

end; // ReadStringParamFromRegistry


// ������ ���������� �������� � �������� �������
procedure WriteStringParamToRegistry (Root: HKEY;
                                      BranchPath: string;
                                      ParamName: string;
                                      ParamValue: string);
var
  Reg: TRegistry;

begin

  if IsRegistryParamExists (Root, BranchPath, ParamName) then
  begin

    Reg := nil;
    Reg := TRegistry.Create();

    try

      Reg.RootKey := Root;
      Reg.OpenKey(BranchPath,false);
      Reg.WriteString(ParamName, ParamValue);

    finally

      Reg.CloseKey();
      FreeAndNil(Reg);

    end;

  end;

end; // WriteStringParamToRegistry


// ������ �������������� �������� �� ��������� �������
function ReadIntParamFromRegistry (Root: HKEY; BranchPath: string; ParamName: string): integer;
var
  Reg: TRegistry;

begin

  Result := -999999;

  if IsRegistryParamExists (Root, BranchPath, ParamName) then
  begin

    Reg := nil;
    Reg := TRegistry.Create(KEY_READ);

    try

      Reg.RootKey := Root;
      Reg.OpenKey(BranchPath,false);
      Result := Reg.ReadInteger(ParamName);

    finally

      Reg.CloseKey();
      FreeAndNil(Reg);

    end;

  end;

end; // ReadIntParamFromRegistry


// ������ �������������� �������� � �������� �������
procedure WriteIntParamToRegistry (Root: HKEY; BranchPath: string; ParamName: string;
                                   ParamValue: Integer);
var
  Reg: TRegistry;

begin

  if IsRegistryParamExists (Root, BranchPath, ParamName) then
  begin

    Reg := nil;
    Reg := TRegistry.Create();

    try

      Reg.RootKey := Root;
      Reg.OpenKey(BranchPath, false);
      Reg.WriteInteger(ParamName, ParamValue);

    finally

      Reg.CloseKey();
      FreeAndNil(Reg);

    end;

  end;

end; // WriteIntParamToRegistry


// ������ �������� ��������� ��������� �� �������
procedure ReadBinaryParamFromRegistry (Root: HKEY; BranchPath: string; ParamName: string;
                                       var ParamValue: TBinaryBuffer);
var
  Reg: TRegistry;

begin

  if IsRegistryParamExists (Root, BranchPath, ParamName) then
  begin

    Reg := nil;
    Reg := TRegistry.Create(KEY_READ);

    try

      Reg.RootKey := Root;
      Reg.OpenKey(BranchPath, false);
      Reg.ReadBinaryData(ParamName, ParamValue, BufSize);

    finally

      Reg.CloseKey();
      FreeAndNil(Reg);

    end;

  end;

end; // ReadBinaryParamFromRegistry


// ������ �������� ��������� ��������� � ������
procedure WriteBinaryParamToRegistry (Root: HKEY; BranchPath: string; ParamName: string;
                                      ParamValue: TBinaryBuffer);
var
  Reg: TRegistry;

begin

  if IsRegistryParamExists (Root, BranchPath, ParamName) then
  begin

    Reg := nil;
    Reg := TRegistry.Create();

    try

      Reg.RootKey := Root;
      Reg.OpenKey(BranchPath, false);
      Reg.WriteBinaryData(ParamName, ParamValue, BufSize);

    finally

      Reg.CloseKey();
      FreeAndNil(Reg);

    end;

  end;

end; // WriteBinaryParamToRegistry


// ������� �������� ����� ������� � ����
function ExportRegistryBranchToFile (Root: HKEY; BranchPath: string; FileName: string): boolean;
var
  Reg: TRegistry;
  FullBranchPath: string;
  Parameters: string;
  Handle: THandle;

begin

  DeleteFile(FileName);

  if IsRegistryBranchExists(Root, BranchPath) then
  begin

    Reg := nil;
    Reg := TRegistry.Create();
    Reg.RootKey := Root;
    if BranchPath[1] = '\' then
      Delete(BranchPath, 1, 1);
    FullBranchPath := Reg.RootKeyName + '\' + BranchPath;
    Parameters := '/e "'+FileName+'" "'+FullBranchPath+'"';

    Result := ShellExecute(Handle, 'open', PChar('regedit.exe'), PChar(parameters), '', SW_SHOWNORMAL) > 32;

  end;

  Result := FileExists(FileName);

end; // ExportRegistryBranchToFile


// ������ ������ ������� �� �����
function ImportRegistryBranchFromFile (Root: HKEY; BranchPath: string; FileName: string): boolean;
var
  Parameters: string;
  Res: Boolean;
  Handle: THandle;

begin

  DeleteRegistryBranch(Root, BranchPath);

  if FileExists(FileName) then
  begin

    Parameters := '/s "'+FileName+'"';

    Result := ShellExecute(Handle, 'open', PChar('regedit.exe'), PChar(parameters), '', SW_SHOWNORMAL) > 32;

  end;

  Result := IsRegistryBranchExists(Root, BranchPath);

end; // ImportRegistryBranchFromFile


//======================================End TRegistryWork==============================================

end.
//====================================End DUTF_AddFunctionSuite_Registry========================================
