// DUnit Test Framework Additional Functional Suite (DUTF_AddFunctionSuite)
//
// ������ ��������������� ����������� ��� DUnit
// ��������� ����� �������, ����������� ����������� DUnit �� ������ � ����� ������
//
// �����: �.�. ��������
// 21.03.2017
//
// ������� ���������:
//
// 21.03.2017 - ������ ������� ����� �������: ���������� � ��/������������ �� ��, ��������� ����������� � �������������� ������
//
// 23.03.2017 - ������� ������� ��������
//
// 27.03.2017 - ������� ������� ������������ ����� ����������� � ��������� �������������� �������
//
// 13.04.2017 - ��������� ������� ��������� �������� ���� �� �������
//
// 24.04.2017 - ���������� ������� InsertWithIdentity � InsertWithoutIdentity. ������ ��� ���� �������
// � ���������� Identity: boolean. ����� �������� �� ��� ������ � ��
//
// 28.04.2017 - ��������� CreateNewTable � ExecSQLCommand. ������ �������� �������� ������� ������ - ����� ����� ���������
// ����������� ��� ���������� ������������ � ��������. �� �� ����� - � ��������������� ��������� �������
//
// 02.05.2017 - ��������� ������� �������� ������� �������� IDENTITY � ������: IsIdentityExists.
// TableIsExists ������������� � IsExists � ���������� �� �������� ������������� ������ ������� � ��
// �������� ������� ������� InsertIntoTable � ���������������� ����������� �������� IDENTITY
// ��������� ��������� ������� ������� ClearTable
//
// 05.05.2017 - ��������� ������ � ������������ ���������� StoredProc ��� � ���������

//======================================Begin DUTF_AddFunctionSuite_DB ===================================

unit DUTF_AddFunctionSuite_DB;

//==============================================���������==============================================
interface

uses
  Winapi.ShellAPI, System.SysUtils, System.Rtti, Data.DB, Data.Win.ADODB;

type
  // ��������� - �������� ��������� ��� ��
  // ParamName - ��� ���������
  // ParamValue - �������� ���������
  // ParamDirection - ����������, ������� ��� ��������. �� ���������� - pdInput.
  TStoredProcParam = record
                       ParamName: string;
                       ParamValue: Variant;
                       ParamDirection: TParameterDirection;
                     end;

  // ��� - ������ ������� ���������� ��� ��
  TStoredProcParamsArray = array of TStoredProcParam;


  // ���������, ����������� ���� ���� ������� ��� ��������� CreateTable
  // FieldName - ��� ����
  // FieldType - ��� ����
  // NULLABLE - �������������� ����������
  TTableField = record
                   FieldName: string;
                   FieldType: string;
                   Nullable: boolean;
                 end;

  // ������ ������� � ��������� ����� ��� ����������� �������
  TTableFieldsArray = array of TTableField;


const
  // ��������� ��� ��������� CreateEmpty ��������� CreateFakeTable
  TBL_EMPTY = True;
  TBL_NOTEMPTY = False;

  // ��������� ��� ��������� Identity ��������� InsertIntoTable
  IDENTITY_ON = True;
  IDENTITY_OFF = False;

// ����� �������� � �������, ����������� ������ � �� ����� ADO

// �������� � �����������
function GetConnectionStringSQL(Server: string; Database: string; Login: string; Password: string): string;
function GetConnectionStringWindows(Server: string; Database: string): string;
function ConnectToDatabase(var ConnectID: TADOConnection; ConnectionString: string): boolean;
function DisconnectFromDatabase(ConnectID: TADOConnection): boolean;

// ��� ������
function IsExists(ConnectID: TADOConnection; ObjectName: string): Boolean;
function IsIdentityExists(ConnectID: TADOConnection; TableName: string): Boolean;
procedure CreateNewTable(ConnectID: TADOConnection; TableName: string; TableFields: TTableFieldsArray);
procedure CreateFakeTable(ConnectID: TADOConnection; OriginalTableName, ReserveTableName: string; CreateEmpty: boolean);
procedure DeleteTable(ConnectID: TADOConnection; TableName: string);
procedure RenameTable(ConnectID: TADOConnection; SrcTableName, DestTableName: string);
procedure ClearTable(ConnectID: TADOConnection; TableName: string);
procedure RestoreOriginalTable(ConnectID: TADOConnection; ReserveTableName, OriginalTableName: string);

// �������
procedure OpenSelectQuery(ConnectID: TADOConnection; var QueryID: TADOQuery; SQLText: string);
procedure CloseSelectQuery(var QueryID: TADOQuery);

// �������
procedure UpdateTable(ConnectID: TADOConnection; TableName: string; Field: string; Value: string; Conditions: string);
procedure InsertIntoTable(ConnectID: TADOConnection; TableName: string; FieldsList: string; ValuesList: string;
                          Identity: Boolean); overload;
procedure InsertIntoTable(ConnectID: TADOConnection; TableName: string; FieldsList: string; ValuesList: string); overload;
procedure DeleteFromTable(ConnectID: TADOConnection; TableName: string; ConditionsList: string);
procedure ExecSQLCommand(ConnectID: TADOConnection; Command: string);

// ��������� ��������
function GetIntegerValueFromQuery(QueryID: TADOQuery; FieldName: string): integer;
function GetFloatValueFromQuery(QueryID: TADOQuery; FieldName: string): double;
function GetBoolValueFromQuery(QueryID: TADOQuery; FieldName: string): boolean;
function GetStringValueFromQuery(QueryID: TADOQuery; FieldName: string): string;
function GetDateTimeValueFromQuery(QueryID: TADOQuery; FieldName: string): TDateTime;

// ��� ��������
procedure ExecStoredProcedure(ConnectID: TADOConnection; ProcedureName: string; var Params: TStoredProcParamsArray);
procedure SetParamNamesAndDirections(ConnectID: TADOConnection; ProcedureName: string; var Params: TStoredProcParamsArray);
procedure OpenStoredProcAsDataSet(ConnectID: TADOConnection; var DataSetID: TADODataSet; ProcedureName: string;
                                  Params: string);
procedure CloseStoredProcDataSet(var DataSetID: TADODataSet);
function GetParamCount(ConnectID: TADOConnection; ProcedureName: string): integer;
function OutMSSQLTableTOCSV(ServerName: string; BaseName: string; User: string; Password: string;
                             TableName: string; FullCSVFileName: string; FieldsDelimiter: string): boolean;

// End of Database Work

//==============================================�������� �������� � �������============================
implementation

//====================================Begin Database Work==============================================

// ������ ������ ����������� � ������������ SQL
// ������� ���������:
// Server - ����� ������� ��
// Database - ��� ���� ������ �� �������
// Login - ����� ������������ ��
// Password - ������ ������������ ��
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


// ������ ������ ����������� � ������������ Windows
// ������� ���������:
// Server - ����� ������� ��
// Database - ��� ���� ������ �� �������
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


// ���������� � �� �� �������� ConnectionString
// ������� ���������:
// ConnectID - ����������, � ������� ����� ��������� ������������� ���������� ����� ��� ��������
// ConnectionString - ������ ����������� (���������)
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


// ������������ �� ��
// ������� ���������:
// ConnectID - ����������, � ������� ����� ��������� ������������� ���������� ����� ��� ��������
function DisconnectFromDatabase(ConnectID: TADOConnection): boolean;
begin
  try
    ConnectID.Connected := false;
  finally
    Result := ConnectID.Connected;
    FreeAndNil(ConnectID);
  end;
end; // DisconnectFromDatabase


// �������� ������������� ������� � ��
// ������� ���������:
// ConnectID - ������������� ����������
// TableName - ��� ����������� �������
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


// ���������, ������������ �� � ������� �������� IDENTITY
// ������� ���������:
// ConnectID - ������������� ����������
// TableName - ��� ����������� �������
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
        // �������� �������� �������� ��������.
        QueryID.SQL.Text := 'SET IDENTITY_INSERT ' + TableName + ' OFF';
        QueryID.ExecSQL;
      except
        // ���� �������� �� ���������� � ������� - ������ ����������, ������� � ������������ ��� ��������� ���������� = False
        Result := IDENTITY_OFF;
      end;
      // ���������� �������� �������� � ���������� ���������, ���� ��������� = True (�������� ������������ � �������)
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


// �������� ����� �������
// ������� ���������:
// ConnectID - ������������� ����������
// TableName - ��� �������
// TableFields - ������ �������, ����������� ������ ���� ������� (TTableFieldsArray)
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


// ������ ����� ������� ��� ������������
// ������� ���������:
// ConnectID - ������������� ����������
// OriginalTableName - ������������ ��� �������-���������
// ReserveTableName - ����� ���, ��� ������� ����� ��������� �������� ������� (��������� �����)
// CreateEmpty - ������� �������� ������: true - �������� ������ ����� �������, false - �������� ����� �� ����� �������
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
      // ��������� ������������ �������
      OperQuery.SQL.Text := 'EXEC sp_rename ''' + OriginalTableName + ''', ''' + ReserveTableName + '''';
      OperQuery.ExecSQL;
      //��������� �������� CreateEmpty � ��� true ���������� ������������ �������, ����� ��������� ������ ���������
      if CreateEmpty then
      begin
        SqlCondition := ' WHERE 0 = 1'
      end;
      // ������ ����� ������� �������� �� ��������� ����� �����
      OperQuery.SQL.Text := 'SELECT * INTO ' + OriginalTableName + ' FROM ' + ReserveTableName + SqlCondition;
      OperQuery.ExecSQL;
    finally
      FreeAndNil(OperQuery);
    end;
  end;
end; // CreateFakeTable


// �������� ������� �� ��
// ������� ���������:
// ConnectID - ������������� ����������
// TableName - ��� �������
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


// �������������� �������
// ������� ���������:
// ConnectID - ������������� ����������
// SrcTableName - �������� ��� �������
// DestTableName - ����� ��� �������
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


// ��������� ������� �������
// ������� ���������:
// ConnectID - ������������� ����������
// TableName - ��� �������
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


// �������������� ������� �� �������
// ������� ���������:
// ConnectID - ������������� ����������
// ReserveTableName - ��� ��������� �������
// OriginalTableName - ������������ ��� ����������������� �������
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
      // ������� �������� �������
      OperQuery.SQL.Text := 'DROP TABLE '+ OriginalTableName;
      OperQuery.ExecSQL;
      // ��������������� ��������� ������� � ������� � �������� ������
      OperQuery.SQL.Text := 'EXEC sp_rename ''' + ReserveTableName + ''', ''' + OriginalTableName + '''';
      OperQuery.ExecSQL;
    finally
      FreeAndNil(OperQuery);
    end;
  end;
end; // RestoreOriginalTable


// ���������� ������� SELECT
// ������� ���������:
// ConnectID - ������������� ����������
// QueryID - � ��� ���������� ������������ �������
// SQLText - ����� �������
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


// �������� SELECT
// ������� ���������:
// QueryID - ���������GetIntegerValueFromQuery(TestQuery, 'CNT');�� �����
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


// UPDATE �������� �������
// ������� ���������:
// ConnectID - ������������� ����������
// TableName - ��� �������
// Field - ��� ����
// Value - �������������� �������� ����
// Conditions - ������ ������� �������� � ������� SQL
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


// ���������� ������ � ������� � ������ ����������� ��������� IDENTITY
// ������� ���������:
// ConnectID - ������������� ����������
// TableName - ��� �������
// FieldsList - ������ ����� ����� ������� � ������� ������
// ValuesList - ������ �������� ����� ����� ������� � ������� ������
// Identity - ������� ������� �������� IDENTITY � �������
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


// ���������� ������ � ������� � �������������� ����������� ��������� IDENTITY
// ������� ���������:
// ConnectID - ������������� ����������
// TableName - ��� �������
// FieldsList - ������ ����� ����� ������� � ������� ������
// ValuesList - ������ �������� ����� ����� ������� � ������� ������
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

// �������� ������� �� �������� �������
// ������� ���������:
// ConnectID - ������������� ����������
// TableName - ��� �������
// ConditionsList - ������ ������� �������� � ������� SQL
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


// ��������� ������������ ������� SQL
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


// ������� �������� �������� �������������� ���� �������
function GetIntegerValueFromQuery(QueryID: TADOQuery; FieldName: string): integer;
begin
  Result := QueryID.FieldByName(FieldName).AsInteger;
end; // GetIntegerValueFromQuery


// ������� �������� �������� ������������� ���� �������
function GetFloatValueFromQuery(QueryID: TADOQuery; FieldName: string): double;
begin
  Result := QueryID.FieldByName(FieldName).AsFloat;
end; // GetFloatValueFromQuery


// ������� �������� �������� ����������� ���� �������
function GetBoolValueFromQuery(QueryID: TADOQuery; FieldName: string): boolean;
begin
  Result := QueryID.FieldByName(FieldName).AsBoolean;
end; // GetBoolValueFromQuery


// ������� �������� �������� ���������� ���� �������
function GetStringValueFromQuery(QueryID: TADOQuery; FieldName: string): string;
begin
  Result := QueryID.FieldByName(FieldName).AsString;
end; // GetStringValueFromQuery


// ������� �������� �������� ���������� ���� �������
function GetDateTimeValueFromQuery(QueryID: TADOQuery; FieldName: string): TDateTime;
begin
  Result := QueryID.FieldByName(FieldName).AsDateTime;
end; // GetDateTimeValueFromQuery


// ������ �������� ���������
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

      // ��������� �������� ���������� INPUT
      // �� ��������� �������� �0 - @RETURN_VALUE
      for i := 0 to Length(Params) - 1 do
      begin
        StoredProc.Parameters.ParamByName(Params[i].ParamName).Value := Params[i].ParamValue;
      end;

      StoredProc.ExecProc();

      // �������� �������� ���������� ����� ���������� ���������, ����� �������� OUTPUT
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


// ���������� ������� ���������� ������� TStoredProcParamsArray
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

        // ��� ��������� � ���������� pdInputOutput ������������ � pdOutput
        // ���������, � ��� �������, �� � pdInputOutput �������� �� �������� �� ������������
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


// ��������� ���������� DataSet, ��������������� � ���������� ������ �������� ��
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


// ��������� DataSet �� SP
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


// ��������� ���������� ���������� �������� ���������
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


// ����� ������� MS SQL � csv
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
