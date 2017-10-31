unit TestStructureUnit;

interface

uses
  System.SysUtils, Variants, System.Generics.Collections, RTTI;

type
  TInputDataArray = array of TValue;

  TTestDataDictionary = TDictionary<string, TInputDataArray>;

  TSuiteRec = record
    SuitePath: string;
    SuiteName: string;
    SuiteClassName: string;
  end;

  TTestCaseRec = record
    SuiteName: string;
    TestCaseClass: string;
    MethodName: string;
    TestCaseName: string;
  end;

  TSuiteList = array of TSuiteRec;

  TTestCaseList = array of TTestCaseRec;

  procedure CreateTestDataDictionary;
  procedure DestroyTestDataDictionary;

var
  TestCaseList: TTestCaseList;
  SuiteList: TSuiteList;
  DataArray: TTestDataDictionary;

implementation

procedure CreateTestDataDictionary;
begin
  DataArray := TDictionary<string, TInputDataArray>.Create;
  DataArray.Clear;
end;


procedure DestroyTestDataDictionary;
begin
  DataArray.Clear;
  FreeAndNil(DataArray);
end;

initialization

  CreateTestDataDictionary;

finalization

  DestroyTestDataDictionary;

end.
