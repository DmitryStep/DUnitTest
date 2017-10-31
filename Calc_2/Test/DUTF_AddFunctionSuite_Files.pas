// DUnit Test Framework Additional Functional Suite (DUTF_AddFunctionSuite)
//
// ������ ��������������� ����������� ��� DUnit
// ��������� ����� �������, ����������� ����������� DUnit �� ������ � �������
//
// �����: �.�. ��������
// 23.03.2017
//
// ������� ���������:
//
// 23.03.2017 - �������� �������, ������������ ��������� ������ ���������� �����
//
// 27.03.2017 - �������� ������� ��������� ����� �� ������ �����

//======================================Begin DUTF_AddFunctionSuite_Files ===================================

unit DUTF_AddFunctionSuite_Files;

//==============================================���������==============================================
interface

uses
  System.Classes, System.SysUtils;

// ����� �������� � �������, ����������� ������ � �������

function GetFullFileName(FilePath, FileName: string): string;
function GetFullErrorFileName(FilePath, FileName: string; ErrorSuffix: string): string;

function ReadLastStringFromTextFile (FileName: string): string;
function ReadStringByIndex (FileName: string; Index: integer): string;
function GetStringsCount (FileName: string): Integer;

// End of Files Work

//==============================================�������� �������� � �������============================
implementation

//====================================Begin Files Work==============================================

// ��������������� ������� - ����������� ������� ���� � ����� ������ ����
// ���������� ���
function GetFullFileName(FilePath, FileName: string): string;
begin

  Result := FilePath + FileName;

end; // GetFullFileName


// Error - ���
function GetFullErrorFileName(FilePath, FileName: string; ErrorSuffix: string): string;
begin

  Result := FilePath + ChangeFileExt(ExtractFileName(FileName), '') + ErrorSuffix + ExtractFileExt(FileName);

end; // GetFullErrorFileName


// ��������� ��������� ������ �� ���������� �����
function ReadLastStringFromTextFile (FileName: string): string;
var
  StringList: TStringList;

begin
  Result := '';

  if FileExists(FileName) then
  begin
    // ������ � ��������� StringList �� ���������� �����
    StringList := nil;
    StringList := TStringList.Create();

    try

      StringList.LoadFromFile(FileName);

      // �������� �������� ��������� ������ � ������, ������� ������-���� � ���������� ���������� ��������
      Result := StringList.Strings[StringList.Count-1];

    finally

      FreeAndNil(StringList);

    end;

  end;

end; // ReadLastStringFromTextFile


// ���������� �������� �������� ������ ����� �� � ������ (������� �������� Index), ��������� ����� � �������
function ReadStringByIndex (FileName: string; Index: integer): string;
var
  StringList: TStringList;

begin

  Result := '';

  if FileExists(FileName) then
  begin

    // ������ � ��������� StringList �� ���������� �����
    StringList := nil;
    StringList := TStringList.Create();

    try

      StringList.LoadFromFile(FileName);

      // �������� �������� i-��� ������ � ������, ������� ������-���� � ���������� ���������� ��������
      if Index <= (StringList.Count) then
      begin

        Result := StringList.Strings[Index - 1]

      end
      else
      begin

        Result := '';

      end;
    finally

      FreeAndNil(StringList);

    end;

  end;

end; // ReadStringByIndex


//���������� ���������� ����� ���������� �����
function GetStringsCount (FileName: string): Integer;
var
  StringList: TStringList;

begin

  Result := 0;

  if FileExists(FileName) then
  begin

    StringList := nil;
    StringList := TStringList.Create();

    try

      StringList.LoadFromFile(FileName);
      Result := StringList.Count;

    finally

      FreeAndNil(StringList);

    end;

  end;

end; // GetStringsCount

//====================================End Files Work==============================================

end.
//====================================End DUTF_AddFunctionSuite_Files========================================
