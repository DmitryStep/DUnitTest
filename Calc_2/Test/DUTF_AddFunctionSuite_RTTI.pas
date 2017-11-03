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
//
// 01.11.2017 - переписал на класс


//======================================Begin DUTF_AddFunctionSuite_RTTI ===================================

unit DUTF_AddFunctionSuite_RTTI;

//==============================================Интерфейс==============================================
interface

uses
  System.Rtti, System.TypInfo, System.Variants, System.SysUtils, System.Generics.Collections;

  // Набор процедур и функций, реализующий работу с Private через Rtti
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

    // Получение результатов работы функций
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

    // Запуск процедур

    procedure ExecPrivateProcedure(ClassType: TClass; MethodName: string; Instance: TObject;
                                             const Args: TValArray);
    procedure SetPrivateValue(ClassType: TClass; PropertyName: string; Instance: Pointer; Value: TValue);
  end;
// End of Rtti Work


//==============================================Описание процедур и функций============================
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

// Преобразует Variant в boolean
function TRTTIClass.VarToBool(aVarValue: Variant): Boolean;
begin
  Result := LowerCase( Trim( VarToStr(aVarValue) ) ) = 'true';
end;


// Функция преобразует значение из типа Variant в тип TValue
// с сохранением информации о "родном" типе
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


// Заполнение массива параметров для запуска процедур и функций.
// Входные параметры:
// ClassType - класс
// PropertyName - имя приватного свойства класса
// ParamValues - значения параметров
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

// Получение значения свойства типа Boolean.
// Входные параметры:
// ClassType - класс
// PropertyName - имя приватного свойства класса
// ptrObject - указатель на экземпляр класса, с которым работаем из основного кода

function TRTTIClass.GetPrivatePropertyValueBool(ClassType: TClass; PropertyName: string; Instance: TObject): Boolean;
begin
  Result := RttiContext.GetType(ClassType).GetField(PropertyName).GetValue(Instance).AsBoolean;
end; // GetPrivatePropertyValueBool


// Получение значения свойства типа String.
// Входные параметры:
// ClassType - класс
// PropertyName - имя приватного свойства класса
// ptrObject - указатель на экземпляр класса, с которым работаем из основного кода

function TRTTIClass.GetPrivatePropertyValueString(ClassType: TClass; PropertyName: string; Instance: TObject): string;
begin
  Result := RttiContext.GetType(ClassType).GetField(PropertyName).GetValue(Instance).AsString;
end; // GetPrivatePropertyValueString


// Получение значения свойства типа Integer.
// Входные параметры:
// ClassType - класс
// PropertyName - имя приватного свойства класса
// Instance - указатель на экземпляр класса, с которым работаем из основного кода

function TRTTIClass.GetPrivatePropertyValueInteger(ClassType: TClass; PropertyName: string; Instance: TObject): integer;
begin
  Result := RttiContext.GetType(ClassType).GetField(PropertyName).GetValue(Instance).AsInteger;
end; // GetPrivatePropertyValueInteger


// Получение значения свойства типа Float.
// Входные параметры:
// ClassType - класс
// PropertyName - имя приватного свойства класса
// Instance - указатель на экземпляр класса, с которым работаем из основного кода

function TRTTIClass.GetPrivatePropertyValueFloat(ClassType: TClass; PropertyName: string; Instance: TObject): Double;
begin
  Result := RttiContext.GetType(ClassType).GetField(PropertyName).GetValue(Instance).AsExtended;
end; // GetPrivatePropertyValueFloat


// Получение значения свойства произвольного типа.
// Входные параметры:
// ClassType - класс
// PropertyName - имя приватного свойства класса
// Instance - указатель на экземпляр класса, с которым работаем из основного кода
function TRTTIClass.GetPrivatePropertyValue<T>(ClassType: TClass; PropertyName: string; Instance: TObject): T;
begin
  Result := RttiContext.GetType(ClassType).GetField(PropertyName).GetValue(Instance).AsType<T>;
end; // GetPrivatePropertyValue


// Получение результата работы приватной функции типа Boolean.
// Входные параметры:
// ClassType - класс
// MethodName - имя приватного метода класса (функции)
// Instance - указатель на экземпляр класса, с которым работаем из основного кода
// Args - параметры, передаваемые в функцию

function TRTTIClass.GetPrivateFunctionResultBoolean(ClassType: TClass; MethodName: string; Instance: TObject;
                                         const Args: TValArray): Boolean;
begin
  Result := RTTIContext.GetType(ClassType).GetMethod(MethodName).Invoke(Instance, Args).AsBoolean;
end; // GetPrivateFunctionResultBoolean


// Получение результата работы приватной функции типа String.
// Входные параметры:
// ClassType - класс
// MethodName - имя приватного метода класса (функции)
// Instance - указатель на экземпляр класса, с которым работаем из основного кода
// Args - параметры, передаваемые в функцию

function TRTTIClass.GetPrivateFunctionResultString(ClassType: TClass; MethodName: string; Instance: TObject;
                                        const Args: TValArray): String;
begin
  Result := RTTIContext.GetType(ClassType).GetMethod(MethodName).Invoke(Instance, args).AsString;
end; // GetPrivateFunctionResultString


// Получение результата работы приватной функции типа Integer.
// Входные параметры:
// ClassType - класс
// MethodName - имя приватного метода класса (функции)
// Instance - указатель на экземпляр класса, с которым работаем из основного кода
// Args - параметры, передаваемые в функцию

function TRTTIClass.GetPrivateFunctionResultInteger(ClassType: TClass; MethodName: string; Instance: TObject;
                                         const Args: TValArray): Integer;
begin
  Result := RTTIContext.GetType(ClassType).GetMethod(MethodName).Invoke(Instance, args).AsInteger;
end; // GetPrivateFunctionResultInteger


// Получение результата работы приватной функции типа Float.
// Входные параметры:
// ClassType - класс
// MethodName - имя приватного метода класса (функции)
// Instance - указатель на экземпляр класса, с которым работаем из основного кода
// Args - параметры, передаваемые в функцию

function TRTTIClass.GetPrivateFunctionResultFloat(ClassType: TClass; MethodName: string; Instance: TObject;
                                       const Args: TValArray): Double;
begin
  Result := RTTIContext.GetType(ClassType).GetMethod(MethodName).Invoke(Instance, args).AsExtended;
end; // GetPrivateFunctionResultFloat


// Получение результата работы приватной функции произвольного типа.
// Входные параметры:
// ClassType - класс
// MethodName - имя приватного метода класса (функции)
// Instance - указатель на экземпляр класса, с которым работаем из основного кода
// Args - параметры, передаваемые в функцию
function TRTTIClass.GetPrivateFunctionResult<T>(ClassType: TClass; MethodName: string; Instance: TObject;
                                     const Args: TValArray): T;
begin
  Result := RTTIContext.GetType(ClassType).GetMethod(MethodName).Invoke(Instance, args).AsType<T>;
end;

// Запуск приватной процедуры
// Входные параметры:
// ClassType - класс
// MethodName - имя приватного метода класса (функции)
// Instance - указатель на экземпляр класса, с которым работаем из основного кода
// Args - параметры, передаваемые в процедуру
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
