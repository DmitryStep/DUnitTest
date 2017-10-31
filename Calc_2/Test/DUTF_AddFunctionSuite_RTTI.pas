// DUnit Test Framework Additional Functional Suite (DUTF_AddFunctionSuite)
//
// Модуль дополнительного функционала для DUnit
// Реализует набор функций, расширяющий возможности DUnit по работе с приватными методами и свойствами классов через RTTIContext
//
// Автор: Д.В. Степанов
// 11.04.2017
//
// История изменений:
//
// 11.04.2017 - Создан базовый набор функций для получения значений приватных свойст и выполнения приватных функций
//
// 20.04.2017 - Переписаны параметры функций (использовал в параметрах Pointer вместо TObject, из-за этого вызывался
// совсем не тот Invoke)


//======================================Begin DUTF_AddFunctionSuite_RTTI ===================================

unit DUTF_AddFunctionSuite_RTTI;

//==============================================Интерфейс==============================================
interface

uses
  System.Rtti, System.SysUtils;

  // Набор процедур и функций, реализующий работу с Private через Rtti

  // Получение значений свойств
  function GetPrivatePropertyValueBool(ClassType: TClass; PropertyName: string; Instance: TObject): Boolean;
  function GetPrivatePropertyValueString(ClassType: TClass; PropertyName: string; Instance: TObject): String;
  function GetPrivatePropertyValueInteger(ClassType: TClass; PropertyName: string; Instance: TObject): Integer;
  function GetPrivatePropertyValueFloat(ClassType: TClass; PropertyName: string; Instance: TObject): Double;

  // Получение результатов работы функций
  function GetPrivateFunctionResultBoolean(ClassType: TClass; MethodName: string; Instance: TObject;
                                           const Args: array of TValue): Boolean;
  function GetPrivateFunctionResultString(ClassType: TClass; MethodName: string; Instance: TObject;
                                          const Args: array of TValue): string;
  function GetPrivateFunctionResultInteger(ClassType: TClass; MethodName: string; Instance: TObject;
                                           const Args: array of TValue): Integer;
  function GetPrivateFunctionResultFloat(ClassType: TClass; MethodName: string; Instance: TObject;
                                         const Args: array of TValue): Double;

  // Запуск процедур

  procedure ExecPrivateProcedure(ClassType: TClass; MethodName: string; Instance: TObject;
                                           const Args: array of TValue);
// End of Rtti Work


//==============================================Описание процедур и функций============================
implementation

//====================================Begin Private Work==============================================

// Получение значения свойства типа Boolean.
// Входные параметры:
// ClassType - класс
// PropertyName - имя приватного свойства класса
// ptrObject - указатель на экземпляр класса, с которым работаем из основного кода

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


// Получение значения свойства типа String.
// Входные параметры:
// ClassType - класс
// PropertyName - имя приватного свойства класса
// ptrObject - указатель на экземпляр класса, с которым работаем из основного кода

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


// Получение значения свойства типа Integer.
// Входные параметры:
// ClassType - класс
// PropertyName - имя приватного свойства класса
// Instance - указатель на экземпляр класса, с которым работаем из основного кода

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


// Получение значения свойства типа Float.
// Входные параметры:
// ClassType - класс
// PropertyName - имя приватного свойства класса
// Instance - указатель на экземпляр класса, с которым работаем из основного кода

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


// Получение результата работы приватной функции типа Boolean.
// Входные параметры:
// ClassType - класс
// MethodName - имя приватного метода класса (функции)
// Instance - указатель на экземпляр класса, с которым работаем из основного кода
// Args - параметры, передаваемые в функцию

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


// Получение результата работы приватной функции типа String.
// Входные параметры:
// ClassType - класс
// MethodName - имя приватного метода класса (функции)
// Instance - указатель на экземпляр класса, с которым работаем из основного кода
// Args - параметры, передаваемые в функцию

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


// Получение результата работы приватной функции типа Integer.
// Входные параметры:
// ClassType - класс
// MethodName - имя приватного метода класса (функции)
// Instance - указатель на экземпляр класса, с которым работаем из основного кода
// Args - параметры, передаваемые в функцию

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


// Получение результата работы приватной функции типа Float.
// Входные параметры:
// ClassType - класс
// MethodName - имя приватного метода класса (функции)
// Instance - указатель на экземпляр класса, с которым работаем из основного кода
// Args - параметры, передаваемые в функцию

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


// Запуск приватной процедуры
// Входные параметры:
// ClassType - класс
// MethodName - имя приватного метода класса (функции)
// Instance - указатель на экземпляр класса, с которым работаем из основного кода
// Args - параметры, передаваемые в процедуру
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
