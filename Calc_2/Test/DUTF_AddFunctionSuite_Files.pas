// DUnit Test Framework Additional Functional Suite (DUTF_AddFunctionSuite)
//
// Модуль дополнительного функционала для DUnit
// Реализует набор функций, расширяющий возможности DUnit по работе с файлами
//
// Автор: Д.В. Степанов
// 23.03.2017
//
// История изменений:
//
// 23.03.2017 - написана функция, возвращающая последнюю строку текстового файла
//
// 27.03.2017 - написаны функции получения путей до файлов логов

//======================================Begin DUTF_AddFunctionSuite_Files ===================================

unit DUTF_AddFunctionSuite_Files;

//==============================================Интерфейс==============================================
interface

uses
  System.Classes, System.SysUtils;

// Набор процедур и функций, реализующий работу с файлами

function GetFullFileName(FilePath, FileName: string): string;
function GetFullErrorFileName(FilePath, FileName: string; ErrorSuffix: string): string;

function ReadLastStringFromTextFile (FileName: string): string;
function ReadStringByIndex (FileName: string; Index: integer): string;
function GetStringsCount (FileName: string): Integer;

// End of Files Work

//==============================================Описание процедур и функций============================
implementation

//====================================Begin Files Work==============================================

// Вспомогательные функции - определение полного пути и имени файлов лога
// Нормальный лог
function GetFullFileName(FilePath, FileName: string): string;
begin

  Result := FilePath + FileName;

end; // GetFullFileName


// Error - лог
function GetFullErrorFileName(FilePath, FileName: string; ErrorSuffix: string): string;
begin

  Result := FilePath + ChangeFileExt(ExtractFileName(FileName), '') + ErrorSuffix + ExtractFileExt(FileName);

end; // GetFullErrorFileName


// Считывает последнюю строку из текстового файла
function ReadLastStringFromTextFile (FileName: string): string;
var
  StringList: TStringList;

begin
  Result := '';

  if FileExists(FileName) then
  begin
    // создаём и заполняем StringList из указанного файла
    StringList := nil;
    StringList := TStringList.Create();

    try

      StringList.LoadFromFile(FileName);

      // получаем значение последней строки в списке, убиваем стринг-лист и возвращаем полученное значение
      Result := StringList.Strings[StringList.Count-1];

    finally

      FreeAndNil(StringList);

    end;

  end;

end; // ReadLastStringFromTextFile


// Возвращает значение заданной строки файла по её номеру (входной параметр Index), нумерация строк с единицы
function ReadStringByIndex (FileName: string; Index: integer): string;
var
  StringList: TStringList;

begin

  Result := '';

  if FileExists(FileName) then
  begin

    // создаём и заполняем StringList из указанного файла
    StringList := nil;
    StringList := TStringList.Create();

    try

      StringList.LoadFromFile(FileName);

      // получаем значение i-той строки в списке, убиваем стринг-лист и возвращаем полученное значение
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


//Возвращает количество строк текстового файла
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
