program CalcPr;

uses
  Vcl.Forms,
  Calc in 'Calc.pas' {Form1},
  CalcLogic in 'CalcLogic.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
