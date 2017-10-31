// DUnit Test Framework Additional Functional Suite (DUTF_AddFunctionSuite)
//
// Модуль дополнительного функционала для DUnit
// Реализует набор функций, расширяющий возможности DUnit по работе с базой данных
//
// Автор: Д.В. Степанов
// 21.03.2017
//
// История изменений:
//
// 21.03.2017 - сделан базовый набор функций: соединение с БД/отсоединение от БД, резервное копирование и восстановление таблиц
//
// 23.03.2017 - добавил парочку констант
//
// 27.03.2017 - дописал функции формирования строк подключения и процедуру переименования таблицы
//
// 13.04.2017 - добавлены функции получения значения поля из запроса
//
// 24.04.2017 - Объединены функции InsertWithIdentity и InsertWithoutIdentity. Теперь это одна функция
// с параметром Identity: boolean. Также доведена до ума работа с ХП
//
// 28.04.2017 - Добавлены CreateNewTable и ExecSQLCommand. Изменён механизм создания фековых таблиц - ранее копии оригинала
// создавались без сохранения констрайнтов и индексов. То же самое - с восстановлением резервной таблицы
//
// 02.05.2017 - Добавлена функция проверки наличия свойства IDENTITY у таблиц: IsIdentityExists.
// TableIsExists переименована в IsExists и переделана на проверку существования любого объекта в БД
// Добавлен вариант функции InsertIntoTable с автоопределением присутствия свойства IDENTITY
// Добавлена процедура очистки таблицы ClearTable
//
// 05.05.2017 - Добавлена работа с результатами выполнения StoredProc как с датасетом

//======================================Begin DUTF_AddFunctionSuite_DB ===================================

unit DUTF_AddFunctionSuite_DB;

//==============================================Интерфейс==============================================
interface

uses
  Winapi.ShellAPI, System.SysUtils, System.Rtti, Data.DB, Data.Win.ADODB;

type
  // Структура - описание параметра для ХП
  // ParamName - имя параметра
  // ParamValue - значение параметра
  // ParamDirection - определяет, входной или выходной. По умаолчанию - pdInput.
  TStoredProcParam = record
                       ParamName: string;
                       ParamValue: Variant;
                       ParamDirection: TParameterDirection;
                     end;

  // Тип - массив записей параметров для ХП
  TStoredProcParamsArray = array of TStoredProcParam;


  // Структура, описывающая одно поле таблицы для процедуры CreateTable
  // FieldName - имя поля
  // FieldType - тип поля
  // NULLABLE - обязательность заполнения
  TTableField = record
                   FieldName: string;
                   FieldType: string;
                   Nullable: boolean;
                 end;

  // Массив записей с описанием полей для создаваемой таблицы
  TTableFieldsArray = array of TTableField;


const
  // константы для параметра CreateEmpty процедуры CreateFakeTable
  TBL_EMPTY = True;
  TBL_NOTEMPTY = False;

  // константы для параметра Identity процедуры InsertIntoTable
  IDENTITY_ON = True;
  IDENTITY_OFF = False;

// Набор процедур и функций, реализующий работу с БД через ADO

// Коннекты и дисконнекты
function GetConnectionStringSQL(Server: string; Database: string; Login: string; Password: string): string;
function GetConnectionStringWindows(Server: string; Database: string): string;
function ConnectToDatabase(var ConnectID: TADOConnection; ConnectionString: string): boolean;
function DisconnectFromDatabase(ConnectID: TADOConnection): boolean;

// Для таблиц
function IsExists(ConnectID: TADOConnection; ObjectName: string): Boolean;
function IsIdentityExists(ConnectID: TADOConnection; TableName: string): Boolean;
procedure CreateNewTable(ConnectID: TADOConnection; TableName: string; TableFields: TTableFieldsArray);
procedure CreateFakeTable(ConnectID: TADOConnection; OriginalTableName, ReserveTableName: string; CreateEmpty: boolean);
procedure DeleteTable(ConnectID: TADOConnection; TableName: string);
procedure RenameTable(ConnectID: TADOConnection; SrcTableName, DestTableName: string);
procedure ClearTable(ConnectID: TADOConnection; TableName: string);
procedure RestoreOriginalTable(ConnectID: TADOConnection; ReserveTableName, OriginalTableName: string);

// Селекты
procedure OpenSelectQuery(ConnectID: TADOConnection; var QueryID: TADOQuery; SQLText: string);
procedure CloseSelectQuery(var QueryID: TADOQuery);

// Команды
procedure UpdateTable(ConnectID: TADOConnection; TableName: string; Field: string; Value: string; Conditions: string);
procedure InsertIntoTable(ConnectID: TADOConnection; TableName: string; FieldsList: string; ValuesList: string;
                          Identity: Boolean); overload;
procedure InsertIntoTable(ConnectID: TADOConnection; TableName: string; FieldsList: string; ValuesList: string); overload;
procedure DeleteFromTable(ConnectID: TADOConnection; TableName: string; ConditionsList: string);
procedure ExecSQLCommand(ConnectID: TADOConnection; Command: string);

// Получение значений
function GetIntegerValueFromQuery(QueryID: TADOQuery; FieldName: string): integer;
function GetFloatValueFromQuery(QueryID: TADOQuery; FieldName: string): double;
function GetBoolValueFromQuery(QueryID: TADOQuery; FieldName: string): boolean;
function GetStringValueFromQuery(QueryID: TADOQuery; FieldName: string): string;
function GetDateTimeValueFromQuery(QueryID: TADOQuery; FieldName: string): TDateTime;

// Для хранимок
procedure ExecStoredProcedure(ConnectID: TADOConnection; ProcedureName: string; var Params: TStoredProcParamsArray);
procedure SetParamNamesAndDirections(ConnectID: TADOConnection; ProcedureName: string; var Params: TStoredProcParamsArray);
procedure OpenStoredProcAsDataSet(ConnectID: TADOConnection; var DataSetID: TADODataSet; ProcedureName: string;
                                  Params: string);
procedure CloseStoredProcDataSet(var DataSetID: TADODataSet);
function GetParamCount(ConnectID: TADOConnection; ProcedureName: string): integer;
function OutMSSQLTableTOCSV(ServerName: string; BaseName: string; User: string; Password: string;
                             TableName: string; FullCSVFileName: string; FieldsDelimiter: string): boolean;

// End of Database Work

//==============================================Описание процедур и функций============================
implementation

//====================================Begin Database Work==============================================

// Создаёт строку подключения с авторизацией SQL
// Входные параметры:
// Server - адрес сервера БД
// Database - имя базы данных на сервере
// Login - логин пользователя БД
// Password - пароль пользователя БД
function GetConnectionStringSQL(Server: string; Database: string; Login: string; Password: string): string;
const
  TestConnectionString = 'Provider=SQLOLEDB.1; ' +
                         'Password=<PSW>; ' +
                         'Persist Security Info=True; ' +
                         'User ID=<User>; ' +
                         'Initial Catalog=<DB>; ' +
                         'Data Source=<SRV>';

begin
  Result := StringReplace(TestConnectionString, '<PSW>', Password, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '<User>', Login, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '<DB>', DataBase, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '<SRV>', Server, [rfReplaceAll, rfIgnoreCase]);
end; // GetConnectionStringSQL


// Создаёт строку подключения с авторизацией Windows
// Входные параметры:
// Server - адрес сервера БД
// Database - имя базы данных на сервере
function GetConnectionStringWindows(Server: string; Database: string): string;
const
  TestConnectionString = 'Provider=SQLOLEDB.1; ' +
                         'Integrated Security=SSPI; ' +
                         'Persist Security Info=False; ' +
                         'Initial Catalog=<DB>; ' +
                         'Data Source=<SRV>';

begin
  Result := StringReplace(TestConnectionString, '<DB>', DataBase, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '<SRV>', Server, [rfReplaceAll, rfIgnoreCase]);
end; // GetConnectionStringWindows


// Соединение с БД по заданной ConnectionString
// Входные параметры:
// ConnectID - переменная, в которую будет возвращен идентификатор соединения после его создания
// ConnectionString - строка подключения (полностью)
function ConnectToDatabase (var ConnectID: TADOConnection; ConnectionString: string): boolean;
begin
  ConnectID := nil;
  ConnectID := TADOConnection.Create(nil);
  ConnectID.ConnectionString := ConnectionString;
  ConnectID.LoginPrompt := false;
  try
    ConnectID.Connected := True;
  finally
    Result := ConnectID.Connected;
  end;
end; // ConnectToDatabase


// Отсоединение от БД
// Входные параметры:
// ConnectID - переменная, в которую будет возвращен идентификатор соединения после его создания
function DisconnectFromDatabase(ConnectID: TADOConnection): boolean;
begin
  try
    ConnectID.Connected := false;
  finally
    Result := ConnectID.Connected;
    FreeAndNil(ConnectID);
  end;
end; // DisconnectFromDatabase


// Проверка существования объекта в БД
// Входные параметры:
// ConnectID - идентификатор коннекшена
// TableName - имя проверяемой таблицы
function IsExists(ConnectID: TADOConnection; ObjectName: string): Boolean;
var
  OperQuery: TADOQuery;
begin
  Result := false;
  if ConnectID.Connected then
  begin
    OperQuery := nil;
    OperQuery := TADOQuery.Create(nil);
    try
      OperQuery.Connection := ConnectID;
      OperQuery.SQL.Text := 'SELECT Count(*) AS CNT FROM dbo.sysobjects WHERE id = OBJECT_ID(''' + ObjectName + ''')';
      OperQuery.Open();
      Result := OperQuery.FieldByName('CNT').AsInteger = 1;
      OperQuery.Close();
    finally
      FreeAndNil(OperQuery);
    end;
  end;
end; // IsExists


// Проверяет, присутствует ли у таблицы свойство IDENTITY
// Входные параметры:
// ConnectID - идентификатор коннекшена
// TableName - имя проверяемой таблицы
function IsIdentityExists(ConnectID: TADOConnection; TableName: string): Boolean;
var
  QueryID: TADOQuery;
begin
  Result := IDENTITY_ON;
  if ConnectID.Connected then
  begin
    QueryID := nil;
    QueryID := TADOQuery.Create(nil);
    try
      try
        QueryID.Connection := ConnectID;
        // Пытаемся изменить значение свойства.
        QueryID.SQL.Text := 'SET IDENTITY_INSERT ' + TableName + ' OFF';
        QueryID.ExecSQL;
      except
        // Если свойства не существует у таблицы - падает исключение, которое и обрабатываем как результат выполнения = False
        Result := IDENTITY_OFF;
      end;
      // Возвращаем значение свойства в предыдущее состояние, если результат = True (свойство присутствует у таблицы)
      if Result = IDENTITY_ON then
      begin
        QueryID.SQL.Text := 'SET IDENTITY_INSERT ' + TableName + ' ON';
        QueryID.ExecSQL;
      end;
    finally
      FreeAndNil(QueryID);
    end;
  end;
end; // IsIdentityExists


// Создание новой таблицы
// Входные параметры:
// ConnectID - идентификатор соединения
// TableName - имя таблицы
// TableFields - массив записей, описывающих каждое поле таблицы (TTableFieldsArray)
procedure CreateNewTable(ConnectID: TADOConnection; TableName: string; TableFields: TTableFieldsArray);
var
  CreateTableQuery: TADOQuery;
  SQLText: string;
  i: Integer;
begin
  if ConnectID.Connected then
  begin
    SQLText := 'CREATE TABLE ' + TableName + '(';
    for i := 0 to Length(TableFields) - 1 do
    begin
      SQLText := SQLText + TableFields[i].FieldName + ' ' + TableFields[i].FieldType;
      if TableFields[i].Nullable then
      begin
        SQLText := SQLText + ' NULL'
      end
      else
      begin
        SQLText := SQLText + ' NOT NULL';
      end;
      if (i < Length(TableFields) - 1) then
      begin
        SQLText := SQLText + ', '
      end;
    end;
    SQLText := SQLText + ');';
    CreateTableQuery := nil;
    CreateTableQuery := TADOQuery.Create(nil);
    try
      CreateTableQuery.Connection := ConnectID;
      CreateTableQuery.SQL.Text := SQLText;
      CreateTableQuery.ExecSQL();
    finally
      FreeAndNil(CreateTableQuery);
    end;
  end;
end; // CreateNewTable


// Создаём копию таблицы для тестирования
// Входные параметры:
// ConnectID - идентификатор соединения
// OriginalTableName - оригинальное имя таблицы-источника
// ReserveTableName - новое имя, под которым будет сохранена исходная таблица (резервная копия)
// CreateEmpty - признак переноса данных: true - создаётся пустая копия таблицы, false - создаётся копия со всеми данными
procedure CreateFakeTable(ConnectID: TADOConnection; OriginalTableName, ReserveTableName: string; CreateEmpty: boolean);
var
  OperQuery: TADOQuery;
  SqlCondition: string;
begin
  SqlCondition := '';
  if ConnectID.Connected then
  begin
    OperQuery := nil;
    OperQuery := TADOQuery.Create(nil);
    try
      OperQuery.Connection := ConnectID;
      // Сохраняем оригинальную таблицу
      OperQuery.SQL.Text := 'EXEC sp_rename ''' + OriginalTableName + ''', ''' + ReserveTableName + '''';
      OperQuery.ExecSQL;
      //Проверяем значение CreateEmpty и при true выставляем невыполнимое условие, чтобы перенести только структуру
      if CreateEmpty then
      begin
        SqlCondition := ' WHERE 0 = 1'
      end;
      // Создаём новую таблицу селектом из созданной ранее копии
      OperQuery.SQL.Text := 'SELECT * INTO ' + OriginalTableName + ' FROM ' + ReserveTableName + SqlCondition;
      OperQuery.ExecSQL;
    finally
      FreeAndNil(OperQuery);
    end;
  end;
end; // CreateFakeTable


// Удаление таблицы из БД
// Входные параметры:
// ConnectID - идентификатор соединения
// TableName - имя таблицы
procedure DeleteTable(ConnectID: TADOConnection; TableName: string);
var
  OperQuery: TADOQuery;
begin
  if ConnectID.Connected then
  begin
    OperQuery := nil;
    OperQuery := TADOQuery.Create(nil);
    try
      OperQuery.Connection := ConnectID;
      OperQuery.SQL.Text := 'DROP TABLE '+ TableName;
      OperQuery.ExecSQL;
    finally
      FreeAndNil(OperQuery);
    end;
  end;
end; // DeleteTable


// Переименование таблицы
// Входные параметры:
// ConnectID - идентификатор соединения
// SrcTableName - исходное имя таблицы
// DestTableName - новое имя таблицы
procedure RenameTable(ConnectID: TADOConnection; SrcTableName, DestTableName: string);
var
  OperQuery: TADOQuery;
begin
  if ConnectID.Connected then
  begin
    OperQuery := nil;
    OperQuery := TADOQuery.Create(nil);
    try
      OperQuery.Connection := ConnectID;
      OperQuery.SQL.Text := 'EXEC sp_rename ''' + SrcTableName +''', ''' + DestTableName +'''';
      OperQuery.ExecSQL;
    finally
      FreeAndNil(OperQuery);
    end;
  end;
end; // RenameTable


// Выполняет очистку таблицы
// Входные параметры:
// ConnectID - идентификатор соединения
// TableName - имя таблицы
procedure ClearTable(ConnectID: TADOConnection; TableName: string);
var
  OperQuery: TADOQuery;
begin
  if ConnectID.Connected then
  begin
    OperQuery := nil;
    OperQuery := TADOQuery.Create(nil);
    try
      OperQuery.Connection := ConnectID;
      OperQuery.SQL.Text := 'TRUNCATE TABLE ' + TableName;
      OperQuery.ExecSQL;
    finally
      FreeAndNil(OperQuery);
    end;
  end;
end; // ClearTable


// Восстановление таблицы из резерва
// Входные параметры:
// ConnectID - идентификатор соединения
// ReserveTableName - имя резервной таблицы
// OriginalTableName - оригинальное имя восстанавливаемой таблицы
procedure RestoreOriginalTable(ConnectID: TADOConnection; ReserveTableName, OriginalTableName: string);
var
  OperQuery: TADOQuery;
begin
  if ConnectID.Connected then
  begin
    OperQuery := nil;
    OperQuery := TADOQuery.Create(nil);
    try
      OperQuery.Connection := ConnectID;
      // Удаляем фейковую таблицу
      OperQuery.SQL.Text := 'DROP TABLE '+ OriginalTableName;
      OperQuery.ExecSQL;
      // Переименовываем резервную таблицу в таблицу с исходным именем
      OperQuery.SQL.Text := 'EXEC sp_rename ''' + ReserveTableName + ''', ''' + OriginalTableName + '''';
      OperQuery.ExecSQL;
    finally
      FreeAndNil(OperQuery);
    end;
  end;
end; // RestoreOriginalTable


// Выполнение запроса SELECT
// Входные параметры:
// ConnectID - идентификатор коннекшена
// QueryID - в эту переменную возвращается выборка
// SQLText - текст запроса
procedure OpenSelectQuery(ConnectID: TADOConnection; var QueryID: TADOQuery; SQLText: string);
begin
  if ConnectID.Connected then
  begin
    QueryID := nil;
    QueryID := TADOQuery.Create(nil);
    try
      QueryID.Connection := ConnectID;
      QueryID.SQL.Text := SQLText;
      QueryID.Open;
    except
      FreeAndNil(QueryID);
    end;
  end;
end; // OpenSelectQuery


// Закрытие SELECT
// Входные параметры:
// QueryID - закрываемGetIntegerValueFromQuery(TestQuery, 'CNT');ая квери
procedure CloseSelectQuery(var QueryID: TADOQuery);
begin
  if Assigned(QueryID) then
  begin
    try
      QueryID.Close();
    finally
      FreeAndNil(QueryID);
    end;
  end;
end; // CloseSelectQuery


// UPDATE заданной таблицы
// Входные параметры:
// ConnectID - идентификатор соединения
// TableName - имя таблицы
// Field - имя поля
// Value - усанавливаемое значение поля
// Conditions - список условий удаления в формате SQL
procedure UpdateTable(ConnectID: TADOConnection; TableName: string; Field: string; Value: string; Conditions: string);
var
  QueryID: TADOQuery;
begin
  if ConnectID.Connected then
  begin
    QueryID := nil;
    QueryID := TADOQuery.Create(nil);
    try
      ConnectID.BeginTrans;
      QueryID.Connection := ConnectID;
      QueryID.SQL.Text := 'UPDATE ' + TableName + ' SET ' + Field + ' = ' + Value;
      if Trim(Conditions)<>'' then
      begin
        QueryID.SQL.Text := QueryID.SQL.Text + ' WHERE '+ Conditions;
      end;
      QueryID.ExecSQL;
      try
        ConnectID.CommitTrans;
      except
        ConnectID.RollbackTrans;
      end;
    finally
      FreeAndNil(QueryID);
    end;
  end;
end; // UpdateTable


// Добавление записи в таблицу с ручным управлением свойством IDENTITY
// Входные параметры:
// ConnectID - идентификатор соединения
// TableName - имя таблицы
// FieldsList - список полей через запятую в формате строки
// ValuesList - список значений полей через запятую в формате строки
// Identity - признак наличия свойства IDENTITY у таблицы
procedure InsertIntoTable(ConnectID: TADOConnection; TableName: string; FieldsList: string; ValuesList: string; Identity: Boolean);
var
  QueryID: TADOQuery;

begin
  if ConnectID.Connected then
  begin
    QueryID := nil;
    QueryID := TADOQuery.Create(nil);
    try
      ConnectID.BeginTrans;
      QueryID.Connection := ConnectID;
      if Identity then
      begin
        QueryID.SQL.Text := 'SET IDENTITY_INSERT ' + TableName + ' ON';
        QueryID.ExecSQL;
      end;
      QueryID.SQL.Text := 'INSERT INTO ' + TableName;
      if FieldsList <> '' then
        QueryID.SQL.Text := QueryID.SQL.Text + ' ('+FieldsList+')';
      QueryID.SQL.Text := QueryID.SQL.Text + ' VALUES ('+ValuesList+')';
      QueryID.ExecSQL;
      if Identity then
      begin
        QueryID.SQL.Text := 'SET IDENTITY_INSERT ' + TableName + ' OFF';
        QueryID.ExecSQL;
      end;
      try
        ConnectID.CommitTrans;
      except
        ConnectID.RollbackTrans;
      end;
    finally
      FreeAndNil(QueryID);
    end;
  end;
end; // InsertIntoTable


// Добавление записи в таблицу с автоматическим управлением свойством IDENTITY
// Входные параметры:
// ConnectID - идентификатор соединения
// TableName - имя таблицы
// FieldsList - список полей через запятую в формате строки
// ValuesList - список значений полей через запятую в формате строки
procedure InsertIntoTable(ConnectID: TADOConnection; TableName: string; FieldsList: string; ValuesList: string);
var
  QueryID: TADOQuery;
  Identity: Boolean;

begin
  if ConnectID.Connected then
  begin
    Identity := IsIdentityExists(ConnectID, TableName);
    QueryID := nil;
    QueryID := TADOQuery.Create(nil);
    try
      ConnectID.BeginTrans;

      QueryID.Connection := ConnectID;
      if Identity then
      begin
        QueryID.SQL.Text := 'SET IDENTITY_INSERT ' + TableName + ' ON';
        QueryID.ExecSQL;
      end;

      QueryID.SQL.Text := 'INSERT INTO '+ TableName +' (' + FieldsList + ') VALUES (' + ValuesList + ')';
      QueryID.ExecSQL;

      if Identity then
      begin
        QueryID.SQL.Text := 'SET IDENTITY_INSERT ' + TableName + ' OFF';
        QueryID.ExecSQL;
      end;

      try
        ConnectID.CommitTrans;
      except
        ConnectID.RollbackTrans;
      end;
    finally
      FreeAndNil(QueryID);
    end;
  end;
end;// InsertIntoTable

// Удаление записей из заданной таблицы
// Входные параметры:
// ConnectID - идентификатор соединения
// TableName - имя таблицы
// ConditionsList - список условий удаления в формате SQL
procedure DeleteFromTable(ConnectID: TADOConnection; TableName: string; ConditionsList: string);
var
  QueryID: TADOQuery;

begin
  if ConnectID.Connected then
  begin
    QueryID := nil;
    QueryID := TADOQuery.Create(nil);

    try
      ConnectID.BeginTrans;
      QueryID.Connection := ConnectID;
      QueryID.SQL.Text := 'DELETE FROM '+TableName;
      if ConditionsList <> '' then
      begin
        QueryID.SQL.Text := QueryID.SQL.Text + ' WHERE ' + ConditionsList;
      end;
      QueryID.ExecSQL;
      try
        ConnectID.CommitTrans;
      except
        ConnectID.RollbackTrans;
      end;
    finally
      FreeAndNil(QueryID);
    end;
  end;
end; // DeleteFromTable


// Выполняет производьную команду SQL
procedure ExecSQLCommand(ConnectID: TADOConnection; Command: string);
var
  SQLCommandQuery: TADOQuery;

begin
  if ConnectID.Connected then
  begin
    SQLCommandQuery := nil;
    SQLCommandQuery := TADOQuery.Create(nil);
    try
      SQLCommandQuery.Connection := ConnectID;
      SQLCommandQuery.SQL.Text := Command;
      SQLCommandQuery.ExecSQL;
    finally
      FreeAndNil(SQLCommandQuery);
    end;
  end;
end; // ExecSQLCommand


// Функция получает значение целочисленного поля запроса
function GetIntegerValueFromQuery(QueryID: TADOQuery; FieldName: string): integer;
begin
  Result := QueryID.FieldByName(FieldName).AsInteger;
end; // GetIntegerValueFromQuery


// Функция получает значение вещественного поля запроса
function GetFloatValueFromQuery(QueryID: TADOQuery; FieldName: string): double;
begin
  Result := QueryID.FieldByName(FieldName).AsFloat;
end; // GetFloatValueFromQuery


// Функция получает значение логического поля запроса
function GetBoolValueFromQuery(QueryID: TADOQuery; FieldName: string): boolean;
begin
  Result := QueryID.FieldByName(FieldName).AsBoolean;
end; // GetBoolValueFromQuery


// Функция получает значение строкового поля запроса
function GetStringValueFromQuery(QueryID: TADOQuery; FieldName: string): string;
begin
  Result := QueryID.FieldByName(FieldName).AsString;
end; // GetStringValueFromQuery


// Функция получает значение строкового поля запроса
function GetDateTimeValueFromQuery(QueryID: TADOQuery; FieldName: string): TDateTime;
begin
  Result := QueryID.FieldByName(FieldName).AsDateTime;
end; // GetDateTimeValueFromQuery


// Запуск хранимой процедуры
procedure ExecStoredProcedure(ConnectID: TADOConnection; ProcedureName: string; var Params: TStoredProcParamsArray);
var
  StoredProc: TADOStoredProc;
  i: Integer;
begin
  if ConnectID.Connected then
  begin
    StoredProc := nil;
    StoredProc := TADOStoredProc.Create(nil);
    try
      StoredProc.ProcedureName := ProcedureName;
      StoredProc.Connection := ConnectID;
      StoredProc.Prepared := True;
      StoredProc.Parameters.Refresh;

      // Заполняем значения параметров INPUT
      // По умолчанию параметр №0 - @RETURN_VALUE
      for i := 0 to Length(Params) - 1 do
      begin
        StoredProc.Parameters.ParamByName(Params[i].ParamName).Value := Params[i].ParamValue;
      end;

      StoredProc.ExecProc();

      // Получаем значения параметров после выполнения процедуры, чтобы получить OUTPUT
      for i := 0 to Length(Params) - 1 do
      begin
        if Params[i].ParamDirection = pdOutput then
        begin
          Params[i].ParamValue := StoredProc.Parameters.ParamByName(Params[i].ParamName).Value;
        end;
      end;
    finally
      FreeAndNil(StoredProc);
    end;
  end;
end; // ExecStoredProcedure


// Заполнение именами параметров массива TStoredProcParamsArray
procedure SetParamNamesAndDirections(ConnectID: TADOConnection; ProcedureName: string; var Params: TStoredProcParamsArray);
var
  StoredProc: TADOStoredProc;
  i: Integer;
begin
  if ConnectID.Connected then
  begin
    StoredProc := nil;
    StoredProc := TADOStoredProc.Create(nil);
    try
      StoredProc.ProcedureName := ProcedureName;
      StoredProc.Connection := ConnectID;
      StoredProc.Parameters.Refresh;
      SetLength(Params, GetParamCount(ConnectID, ProcedureName));
      for i := 0 to GetParamCount(ConnectID, ProcedureName) - 1 do
      begin
        Params[i].ParamName := StoredProc.Parameters[i].Name;

        // Все параметры с директивой pdInputOutput приравниваем к pdOutput
        // Непонятно, в чём причина, но в pdInputOutput значения из хранимки не возвращаются
        if StoredProc.Parameters[i].Direction = pdInputOutput then
        begin
          Params[i].ParamDirection := pdOutput
        end
        else
        begin
          Params[i].ParamDirection := StoredProc.Parameters[i].Direction;
        end;
      end;
    finally
      FreeAndNil(StoredProc);
    end;
  end;
end; // SetParamNamesAndDirections


// Процедура возвращает DataSet, сфоормированный в результате работы заданной ХП
procedure OpenStoredProcAsDataSet(ConnectID: TADOConnection; var DataSetID: TADODataSet; ProcedureName: string;
                                  Params: string);
var
  i: Integer;
begin
  if ConnectID.Connected then
  begin
    DataSetID := nil;
    DataSetID := TADODataSet.Create(nil);
    try
      DataSetID.Connection := ConnectID;
      DataSetID.CommandText := 'EXEC ' + ProcedureName + ' ' + Params;
      DataSetID.Open();
    except
      FreeAndNil(DataSetID);
    end;
  end;
end; // OpenStoredProcAsDataSet


// Закрывает DataSet от SP
procedure CloseStoredProcDataSet(var DataSetID: TADODataSet);
begin
  if Assigned(DataSetID) then
  begin
    try
      DataSetID.Close();
    finally
      FreeAndNil(DataSetID);
    end;
  end;
end; // CloseStoredProcDataSet


// Получение количества параметров хранимой процедуры
function GetParamCount(ConnectID: TADOConnection; ProcedureName: string): integer;
var
  StoredProc: TADOStoredProc;

begin
  if ConnectID.Connected then
  begin
    Result := -1;
    StoredProc := nil;
    StoredProc := TADOStoredProc.Create(nil);
    try
      StoredProc.ProcedureName := ProcedureName;
      StoredProc.Connection := ConnectID;
      StoredProc.Parameters.Refresh;

      Result := StoredProc.Parameters.Count;
    finally
      FreeAndNil(StoredProc);
    end;
  end;
end; // GetParamCount


// Вывод таблицы MS SQL в csv
function OutMSSQLTableTOCSV(ServerName: string; BaseName: string; User: string; Password: string;
                             TableName: string; FullCSVFileName: string; FieldsDelimiter: string): boolean;
const
  ParamsTemplate = '[%BASE%].dbo.[%TABLE%] out %FILENAME% -S%SERVER% -U%USER% -P%PASSWORD% -T -t%DELIMITER% -r\r\n -c';
var
  ExecParams: string;
begin
  if FileExists(FullCSVFileName) then
    DeleteFile(FullCSVFileName);
  ExecParams := StringReplace(ParamsTemplate, '%BASE%', BaseName, [rfReplaceAll]);
  ExecParams := StringReplace(ExecParams, '%TABLE%', TableName, [rfReplaceAll]);
  ExecParams := StringReplace(ExecParams, '%FILENAME%', FullCSVFileName, [rfReplaceAll]);
  ExecParams := StringReplace(ExecParams, '%SERVER%', ServerName, [rfReplaceAll]);
  ExecParams := StringReplace(ExecParams, '%USER%', User, [rfReplaceAll]);
  ExecParams := StringReplace(ExecParams, '%PASSWORD%', Password, [rfReplaceAll]);
  ExecParams := StringReplace(ExecParams, '%DELIMITER%', FieldsDelimiter, [rfReplaceAll]);
  Result := ShellExecute(0, 'open', 'bcp.exe', PChar(ExecParams), '', 0) > 32;
end; // OutMSSQLTableTOCSV

//====================================End Database Work==============================================

end.
//====================================End DUTF_AddFunctionSuite_DB========================================
