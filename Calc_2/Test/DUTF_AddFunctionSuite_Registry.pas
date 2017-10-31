// DUnit Test Framework Additional Functional Suite (DUTF_AddFunctionSuite)
//
// Модуль дополнительного функционала для DUnit
// Реализует набор функций, расширяющий возможности DUnit по работе с реестром
//
// Автор: Д.В. Степанов
// 09.02.2017
//
// История изменений:
//
// 12.02.2017 - добавлены процедуры и функции для работы с реестром из юнит-тестов "в одну команду"
//
// 21.03.2017 - добавлены функции импорта и экспорта данных реестра для резервирования во время выполнения автотеста

//======================================Begin DUTF_AddFunctionSuite_Registry ===================================
unit DUTF_AddFunctionSuite_Registry;

//==============================================Интерфейс==============================================
interface

uses
  Winapi.Windows, Winapi.ShellApi, System.SysUtils, System.Win.Registry;

const
  // Размер буферного массива
  BufSize = 1000;

type

  // Тип, реализующий буфер бинарных данных для работы с реестром (тип BINARY)
  TBinaryBuffer = array [0..BufSize-1] of Byte;

  // Набор процедур и функций, реализующий работу с реестром из юнит-тестов.
  // Предназначен для выполнения операций с реестром из юнит-теста в одну команду.
  // Каждая процедура и функция реализует создание объекта TRgistry, выполнение операции и
  // уничтожение созданного объекта.
  // Класс расширяемый, можно по аналогии с существующими дописать процедуры и функции чтения/записи
  // параметров реестра других типов (сейчас реализованы только binary, integer и string).
  //
  // Входные параметры:
  // Root - корневой элемент реестра;
  // BranchPath - полный путь до ключа, включая имя самого ключа;
  // ParamName - имя параметра в реестре;
  // ParamValue - значение параметра.
  //
  // Выходные параметры функций:
  // boolean - результат выполнения функции;
  // integer, string - значение считываемого параметра.

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


//==============================================Описание процедур и функций============================
implementation

//====================================Begin Registry Work==============================================

// Проверка существования ветки реестра
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


// Проверка существования параметра в реестре
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


// Создание ветки реестра
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


// Удаление ветки реестра
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


// Переименование параметра реестра - для сохранения текущих данных перед началом теста
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


// Чтение строкового значения параметра реестра
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


// Запись строкового значения в параметр реестра
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


// Чтение целочисленного значения из параметра реестра
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


// Запись целочисленного значения в параметр реестра
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


// Чтение значения бинарного параметра из реестра
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


// Запись значения бинарного параметра в реестр
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


// Экспорт заданной ветки реестра в файл
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


// Импорт данных реестра из файла
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
