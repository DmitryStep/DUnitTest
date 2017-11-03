// DUnit Test Framework Additional Functional Suite (DUTF_AddFunctionSuite)
//
// ������ ��������������� ����������� ��� DUnit
// ��������� ����� �������, ����������� ����������� DUnit �� ������ � ���������� �������� � ���������� ������� ����� RTTIContext
//
// �����: �.�. ��������
// 11.04.2017
//
// ������� ���������:
//
// 11.04.2017 - ������ ������� ����� ������� ��� ��������� �������� ��������� ������ � ���������� ��������� �������
//
// 20.04.2017 - ���������� ��������� ������� (����������� � ���������� Pointer ������ TObject, ��-�� ����� ���������
// ������ �� ��� Invoke)
//
// 01.11.2017 - ��������� �� �����


//======================================Begin DUTF_AddFunctionSuite_RTTI ===================================

unit DUTF_AddFunctionSuite_RTTI;

//==============================================���������==============================================
interface

uses
  System.Rtti, System.TypInfo, System.Variants, System.SysUtils, System.Generics.Collections;

  // ����� �������� � �������, ����������� ������ � Private ����� Rtti
type
  TValArray = array of TValue;

  TRTTIClass = class
  private
    RTTIContext: TRttiContext;
    function VarToBool(aVarValue: Variant): Boolean;
    function VarToPropValue(VarValue: Variant; FieldType: TRttiParameter): TValue;
  public
    constructor Create;
    destructor Destroy;

    function SetArgumentsForMethod(ParamValues: array of Variant; ClassType: TClass; MethodName: string): TValArray;
    function GetPrivatePropertyValueBool(ClassType: TClass; PropertyName: string; Instance: TObject): Boolean;
    function GetPrivatePropertyValueString(ClassType: TClass; PropertyName: string; Instance: TObject): String;
    function GetPrivatePropertyValueInteger(ClassType: TClass; PropertyName: string; Instance: TObject): Integer;
    function GetPrivatePropertyValueFloat(ClassType: TClass; PropertyName: string; Instance: TObject): Double;
    function GetPrivatePropertyValue<T>(ClassType: TClass; PropertyName: string; Instance: TObject): T;

    // ��������� ����������� ������ �������
    function GetPrivateFunctionResultBoolean(ClassType: TClass; MethodName: string; Instance: TObject;
                                             const Args: TValArray): Boolean;
    function GetPrivateFunctionResultString(ClassType: TClass; MethodName: string; Instance: TObject;
                                            const Args: TValArray): string;
    function GetPrivateFunctionResultInteger(ClassType: TClass; MethodName: string; Instance: TObject;
                                             const Args: TValArray): Integer;
    function GetPrivateFunctionResultFloat(ClassType: TClass; MethodName: string; Instance: TObject;
                                           const Args: TValArray): Double;
    function GetPrivateFunctionResult<T>(ClassType: TClass; MethodName: string; Instance: TObject;
                                           const Args: TValArray): T;

    // ������ ��������

    procedure ExecPrivateProcedure(ClassType: TClass; MethodName: string; Instance: TObject;
                                             const Args: TValArray);
    procedure SetPrivateValue(ClassType: TClass; PropertyName: string; Instance: Pointer; Value: TValue);
  end;
// End of Rtti Work


//==============================================�������� �������� � �������============================
implementation

//====================================Begin Private Work==============================================

constructor TRTTIClass.Create;
begin
  RTTIContext := TRttiContext.Create();
end;


destructor TRTTIClass.Destroy;
begin
  RTTIContext.Free();
end;

// ����������� Variant � boolean
function TRTTIClass.VarToBool(aVarValue: Variant): Boolean;
begin
  Result := LowerCase( Trim( VarToStr(aVarValue) ) ) = 'true';
end;


// ������� ����������� �������� �� ���� Variant � ��� TValue
// � ����������� ���������� � "������" ����
function TRTTIClass.VarToPropValue(VarValue: Variant; FieldType: TRttiParameter): TValue;
var
  TypeStr: string;
begin
  TypeStr := LowerCase( FieldType.ParamType.ToString );
  if VarToStr(VarValue) = '' then
  begin
    if FieldType.ParamType.IsInstance then
      Result := nil
    else
    begin
      Result := TValue.From<Variant>(VarValue);
    end;
  end
  else
  if FieldType.ParamType.IsInstance then
  begin
    Result := TValue.From<Variant>(VarValue);
  end
  else
  if TypeStr = 'byte' then
    Result := TValue.From<Byte>(Byte(VarValue))
  else
  if TypeStr = 'word' then
    Result := TValue.From<Word>(Word(VarValue))
  else
  if TypeStr = 'longword' then
    Result := TValue.From<LongWord>(LongWord(VarValue))
  else
  if TypeStr = 'integer' then
    Result := TValue.From<Integer>(Integer(VarValue))
  else
  if TypeStr = 'longint' then
    Result := TValue.From<LongInt>(LongInt(VarValue))
  else
  if TypeStr = 'uint64' then
    Result := TValue.From<UInt64>(UInt64(VarValue))
  else
  if TypeStr = 'boolean' then
    Result := TValue.From<Boolean>(VarToBool(VarValue))
  else
  if TypeStr = 'char' then
    Result := TValue.From<Char>(VarToStr(VarValue)[1])
  else
  if TypeStr = 'string' then
    Result := TValue.From<String>(VarToStr(VarValue))
  else
  if TypeStr = 'single' then
    Result := TValue.From<Single>(Single(VarValue))
  else
  if TypeStr = 'double' then
    Result := TValue.From<Double>(Double(VarValue))
  else
  if TypeStr = 'extended' then
    Result := TValue.From<Extended>(Extended(VarValue))
  else
  if (TypeStr = 'tdatetime') or
     (TypeStr = 'tdate')     or
     (TypeStr = 'ttime')
  then
    Result := TValue.From<TDateTime>(VarToDateTime(VarValue))
  else
  if TypeStr = 'variant' then
    Result := TValue.From<Variant>(VarValue);
end;


// ���������� ������� ���������� ��� ������� �������� � �������.
// ������� ���������:
// ClassType - �����
// PropertyName - ��� ���������� �������� ������
// ParamValues - �������� ����������
function TRTTIClass.SetArgumentsForMethod(ParamValues: array of Variant; ClassType: TClass; MethodName: string): TValArray;
var
  Parameter: TRttiParameter;
  ParamIndex: integer;
begin
  ParamIndex := 0;
  for Parameter in RTTIContext.GetType(ClassType).GetMethod(MethodName).GetParameters do
  begin
    SetLength(Result, ParamIndex + 1);
    Result[ParamIndex] := VarToPropValue(ParamValues[ParamIndex], Parameter);
    Inc(ParamIndex);
  end;
end;

// ��������� �������� �������� ���� Boolean.
// ������� ���������:
// ClassType - �����
// PropertyName - ��� ���������� �������� ������
// ptrObject - ��������� �� ��������� ������, � ������� �������� �� ��������� ����

function TRTTIClass.GetPrivatePropertyValueBool(ClassType: TClass; PropertyName: string; Instance: TObject): Boolean;
begin
  Result := RttiContext.GetType(ClassType).GetField(PropertyName).GetValue(Instance).AsBoolean;
end; // GetPrivatePropertyValueBool


// ��������� �������� �������� ���� String.
// ������� ���������:
// ClassType - �����
// PropertyName - ��� ���������� �������� ������
// ptrObject - ��������� �� ��������� ������, � ������� �������� �� ��������� ����

function TRTTIClass.GetPrivatePropertyValueString(ClassType: TClass; PropertyName: string; Instance: TObject): string;
begin
  Result := RttiContext.GetType(ClassType).GetField(PropertyName).GetValue(Instance).AsString;
end; // GetPrivatePropertyValueString


// ��������� �������� �������� ���� Integer.
// ������� ���������:
// ClassType - �����
// PropertyName - ��� ���������� �������� ������
// Instance - ��������� �� ��������� ������, � ������� �������� �� ��������� ����

function TRTTIClass.GetPrivatePropertyValueInteger(ClassType: TClass; PropertyName: string; Instance: TObject): integer;
begin
  Result := RttiContext.GetType(ClassType).GetField(PropertyName).GetValue(Instance).AsInteger;
end; // GetPrivatePropertyValueInteger


// ��������� �������� �������� ���� Float.
// ������� ���������:
// ClassType - �����
// PropertyName - ��� ���������� �������� ������
// Instance - ��������� �� ��������� ������, � ������� �������� �� ��������� ����

function TRTTIClass.GetPrivatePropertyValueFloat(ClassType: TClass; PropertyName: string; Instance: TObject): Double;
begin
  Result := RttiContext.GetType(ClassType).GetField(PropertyName).GetValue(Instance).AsExtended;
end; // GetPrivatePropertyValueFloat


// ��������� �������� �������� ������������� ����.
// ������� ���������:
// ClassType - �����
// PropertyName - ��� ���������� �������� ������
// Instance - ��������� �� ��������� ������, � ������� �������� �� ��������� ����
function TRTTIClass.GetPrivatePropertyValue<T>(ClassType: TClass; PropertyName: string; Instance: TObject): T;
begin
  Result := RttiContext.GetType(ClassType).GetField(PropertyName).GetValue(Instance).AsType<T>;
end; // GetPrivatePropertyValue


// ��������� ���������� ������ ��������� ������� ���� Boolean.
// ������� ���������:
// ClassType - �����
// MethodName - ��� ���������� ������ ������ (�������)
// Instance - ��������� �� ��������� ������, � ������� �������� �� ��������� ����
// Args - ���������, ������������ � �������

function TRTTIClass.GetPrivateFunctionResultBoolean(ClassType: TClass; MethodName: string; Instance: TObject;
                                         const Args: TValArray): Boolean;
begin
  Result := RTTIContext.GetType(ClassType).GetMethod(MethodName).Invoke(Instance, Args).AsBoolean;
end; // GetPrivateFunctionResultBoolean


// ��������� ���������� ������ ��������� ������� ���� String.
// ������� ���������:
// ClassType - �����
// MethodName - ��� ���������� ������ ������ (�������)
// Instance - ��������� �� ��������� ������, � ������� �������� �� ��������� ����
// Args - ���������, ������������ � �������

function TRTTIClass.GetPrivateFunctionResultString(ClassType: TClass; MethodName: string; Instance: TObject;
                                        const Args: TValArray): String;
begin
  Result := RTTIContext.GetType(ClassType).GetMethod(MethodName).Invoke(Instance, args).AsString;
end; // GetPrivateFunctionResultString


// ��������� ���������� ������ ��������� ������� ���� Integer.
// ������� ���������:
// ClassType - �����
// MethodName - ��� ���������� ������ ������ (�������)
// Instance - ��������� �� ��������� ������, � ������� �������� �� ��������� ����
// Args - ���������, ������������ � �������

function TRTTIClass.GetPrivateFunctionResultInteger(ClassType: TClass; MethodName: string; Instance: TObject;
                                         const Args: TValArray): Integer;
begin
  Result := RTTIContext.GetType(ClassType).GetMethod(MethodName).Invoke(Instance, args).AsInteger;
end; // GetPrivateFunctionResultInteger


// ��������� ���������� ������ ��������� ������� ���� Float.
// ������� ���������:
// ClassType - �����
// MethodName - ��� ���������� ������ ������ (�������)
// Instance - ��������� �� ��������� ������, � ������� �������� �� ��������� ����
// Args - ���������, ������������ � �������

function TRTTIClass.GetPrivateFunctionResultFloat(ClassType: TClass; MethodName: string; Instance: TObject;
                                       const Args: TValArray): Double;
begin
  Result := RTTIContext.GetType(ClassType).GetMethod(MethodName).Invoke(Instance, args).AsExtended;
end; // GetPrivateFunctionResultFloat


// ��������� ���������� ������ ��������� ������� ������������� ����.
// ������� ���������:
// ClassType - �����
// MethodName - ��� ���������� ������ ������ (�������)
// Instance - ��������� �� ��������� ������, � ������� �������� �� ��������� ����
// Args - ���������, ������������ � �������
function TRTTIClass.GetPrivateFunctionResult<T>(ClassType: TClass; MethodName: string; Instance: TObject;
                                     const Args: TValArray): T;
begin
  Result := RTTIContext.GetType(ClassType).GetMethod(MethodName).Invoke(Instance, args).AsType<T>;
end;

// ������ ��������� ���������
// ������� ���������:
// ClassType - �����
// MethodName - ��� ���������� ������ ������ (�������)
// Instance - ��������� �� ��������� ������, � ������� �������� �� ��������� ����
// Args - ���������, ������������ � ���������
procedure TRTTIClass.ExecPrivateProcedure(ClassType: TClass; MethodName: string; Instance: TObject;
                               const Args: TValArray);
begin
  RTTIContext.GetType(ClassType).GetMethod(MethodName).Invoke(Instance, args).AsExtended;
end; // ExecPrivateProcedure

procedure TRTTIClass.SetPrivateValue(ClassType: TClass; PropertyName: string; Instance: Pointer; Value: TValue);
begin
  RTTIContext.GetType(ClassType).GetField(PropertyName).SetValue(Instance, Value);
end;

//====================================End Private Work==============================================

end.

//======================================End DUTF_AddFunctionSuite_RTTI ===================================
