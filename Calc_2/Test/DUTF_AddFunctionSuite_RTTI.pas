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


//======================================Begin DUTF_AddFunctionSuite_RTTI ===================================

unit DUTF_AddFunctionSuite_RTTI;

//==============================================���������==============================================
interface

uses
  System.Rtti, System.SysUtils;

  // ����� �������� � �������, ����������� ������ � Private ����� Rtti

  // ��������� �������� �������
  function GetPrivatePropertyValueBool(ClassType: TClass; PropertyName: string; Instance: TObject): Boolean;
  function GetPrivatePropertyValueString(ClassType: TClass; PropertyName: string; Instance: TObject): String;
  function GetPrivatePropertyValueInteger(ClassType: TClass; PropertyName: string; Instance: TObject): Integer;
  function GetPrivatePropertyValueFloat(ClassType: TClass; PropertyName: string; Instance: TObject): Double;

  // ��������� ����������� ������ �������
  function GetPrivateFunctionResultBoolean(ClassType: TClass; MethodName: string; Instance: TObject;
                                           const Args: array of TValue): Boolean;
  function GetPrivateFunctionResultString(ClassType: TClass; MethodName: string; Instance: TObject;
                                          const Args: array of TValue): string;
  function GetPrivateFunctionResultInteger(ClassType: TClass; MethodName: string; Instance: TObject;
                                           const Args: array of TValue): Integer;
  function GetPrivateFunctionResultFloat(ClassType: TClass; MethodName: string; Instance: TObject;
                                         const Args: array of TValue): Double;

  // ������ ��������

  procedure ExecPrivateProcedure(ClassType: TClass; MethodName: string; Instance: TObject;
                                           const Args: array of TValue);
// End of Rtti Work


//==============================================�������� �������� � �������============================
implementation

//====================================Begin Private Work==============================================

// ��������� �������� �������� ���� Boolean.
// ������� ���������:
// ClassType - �����
// PropertyName - ��� ���������� �������� ������
// ptrObject - ��������� �� ��������� ������, � ������� �������� �� ��������� ����

function GetPrivatePropertyValueBool(ClassType: TClass; PropertyName: string; Instance: TObject): Boolean;
var
  RTTIContext: TRttiContext;

begin

  RTTIContext := TRttiContext.Create();

  try

    Result := RttiContext.GetType(ClassType).GetField(PropertyName).GetValue(Instance).AsBoolean;

  finally

    RTTIContext.Free();

  end;

end; // GetPrivatePropertyValueBool


// ��������� �������� �������� ���� String.
// ������� ���������:
// ClassType - �����
// PropertyName - ��� ���������� �������� ������
// ptrObject - ��������� �� ��������� ������, � ������� �������� �� ��������� ����

function GetPrivatePropertyValueString(ClassType: TClass; PropertyName: string; Instance: TObject): string;
var
  RTTIContext: TRttiContext;

begin

  RTTIContext := TRttiContext.Create();

  try

    Result := RttiContext.GetType(ClassType).GetField(PropertyName).GetValue(Instance).AsString;

  finally

    RTTIContext.Free();

  end;

end; // GetPrivatePropertyValueString


// ��������� �������� �������� ���� Integer.
// ������� ���������:
// ClassType - �����
// PropertyName - ��� ���������� �������� ������
// Instance - ��������� �� ��������� ������, � ������� �������� �� ��������� ����

function GetPrivatePropertyValueInteger(ClassType: TClass; PropertyName: string; Instance: TObject): integer;
var
  RTTIContext: TRttiContext;

begin

  RTTIContext := TRttiContext.Create();

  try

    Result := RttiContext.GetType(ClassType).GetField(PropertyName).GetValue(Instance).AsInteger;

  finally

    RTTIContext.Free();

  end;

end; // GetPrivatePropertyValueInteger


// ��������� �������� �������� ���� Float.
// ������� ���������:
// ClassType - �����
// PropertyName - ��� ���������� �������� ������
// Instance - ��������� �� ��������� ������, � ������� �������� �� ��������� ����

function GetPrivatePropertyValueFloat(ClassType: TClass; PropertyName: string; Instance: TObject): Double;
var
  RTTIContext: TRttiContext;

begin

  RTTIContext := TRttiContext.Create();

  try

    Result := RttiContext.GetType(ClassType).GetField(PropertyName).GetValue(Instance).AsExtended;

  finally

    RTTIContext.Free();

  end;

end; // GetPrivatePropertyValueFloat


// ��������� ���������� ������ ��������� ������� ���� Boolean.
// ������� ���������:
// ClassType - �����
// MethodName - ��� ���������� ������ ������ (�������)
// Instance - ��������� �� ��������� ������, � ������� �������� �� ��������� ����
// Args - ���������, ������������ � �������

function GetPrivateFunctionResultBoolean(ClassType: TClass; MethodName: string; Instance: TObject;
                                         const Args: array of TValue): Boolean;
var
  RTTIContext: TRttiContext;

begin

  RTTIContext := TRttiContext.Create();

  try

    Result := RTTIContext.GetType(ClassType).GetMethod(MethodName).Invoke(Instance, Args).AsBoolean;

  finally

    RTTIContext.Free();

  end;

end; // GetPrivateFunctionResultBoolean


// ��������� ���������� ������ ��������� ������� ���� String.
// ������� ���������:
// ClassType - �����
// MethodName - ��� ���������� ������ ������ (�������)
// Instance - ��������� �� ��������� ������, � ������� �������� �� ��������� ����
// Args - ���������, ������������ � �������

function GetPrivateFunctionResultString(ClassType: TClass; MethodName: string; Instance: TObject;
                                        const Args: array of TValue): String;
var
  RTTIContext: TRttiContext;

begin

  RTTIContext := TRttiContext.Create();

  try

    Result := RTTIContext.GetType(ClassType).GetMethod(MethodName).Invoke(Instance, args).AsString;

  finally

    RTTIContext.Free();

  end;

end; // GetPrivateFunctionResultString


// ��������� ���������� ������ ��������� ������� ���� Integer.
// ������� ���������:
// ClassType - �����
// MethodName - ��� ���������� ������ ������ (�������)
// Instance - ��������� �� ��������� ������, � ������� �������� �� ��������� ����
// Args - ���������, ������������ � �������

function GetPrivateFunctionResultInteger(ClassType: TClass; MethodName: string; Instance: TObject;
                                         const Args: array of TValue): Integer;
var
  RTTIContext: TRttiContext;

begin

  RTTIContext := TRttiContext.Create();

  try

    Result := RTTIContext.GetType(ClassType).GetMethod(MethodName).Invoke(Instance, args).AsInteger;

  finally

    RTTIContext.Free();

  end;

end; // GetPrivateFunctionResultInteger


// ��������� ���������� ������ ��������� ������� ���� Float.
// ������� ���������:
// ClassType - �����
// MethodName - ��� ���������� ������ ������ (�������)
// Instance - ��������� �� ��������� ������, � ������� �������� �� ��������� ����
// Args - ���������, ������������ � �������

function GetPrivateFunctionResultFloat(ClassType: TClass; MethodName: string; Instance: TObject;
                                       const Args: array of TValue): Double;
var
  RTTIContext: TRttiContext;

begin

  RTTIContext := TRttiContext.Create();

  try

    Result := RTTIContext.GetType(ClassType).GetMethod(MethodName).Invoke(Instance, args).AsExtended;

  finally

    RTTIContext.Free();

  end;

end; // GetPrivateFunctionResultFloat


// ������ ��������� ���������
// ������� ���������:
// ClassType - �����
// MethodName - ��� ���������� ������ ������ (�������)
// Instance - ��������� �� ��������� ������, � ������� �������� �� ��������� ����
// Args - ���������, ������������ � ���������
procedure ExecPrivateProcedure(ClassType: TClass; MethodName: string; Instance: TObject;
                               const Args: array of TValue);
var
  RTTIContext: TRttiContext;

begin

  RTTIContext := TRttiContext.Create();

  try

    RTTIContext.GetType(ClassType).GetMethod(MethodName).Invoke(Instance, args).AsExtended;

  finally

    RTTIContext.Free();

  end;

end; // ExecPrivateProcedure

//====================================End Private Work==============================================

end.

//======================================End DUTF_AddFunctionSuite_RTTI ===================================
