unit CalcLogic;

interface

uses SysUtils;

type
  TCalcLogic = class
  public
    function Add(a, b: integer): integer;
    function Sub(a, b: integer): integer;
    function Mul(a, b: integer): integer;
    function Division(a, b: integer): integer;
  end;

implementation


function TCalcLogic.Add(a, b: integer): integer;
begin
  Result := a + b;
end;

function TCalcLogic.Sub(a, b: integer): integer;
begin
  Result := a - b;
end;

function TCalcLogic.Mul(a, b: integer): integer;
begin
  Result := a * b;
end;

function TCalcLogic.Division(a, b: integer): integer;
begin
  try
    Result := a div b;
  except
    on E: Exception do
      raise;
  end;
end;

end.
