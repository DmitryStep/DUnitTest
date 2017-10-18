unit Calc;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, CalcLogic, Xml.xmldom,
  Xml.XMLIntf, Xml.Win.msxmldom, Xml.XMLDoc;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    CalcLogic: TCalcLogic;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  Label1.Caption := IntToStr(CalcLogic.Add(StrToInt(Edit1.Text), StrToInt(Edit2.Text)));
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Label1.Caption := IntToStr(CalcLogic.Sub(StrToInt(Edit1.Text), StrToInt(Edit2.Text)));
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Label1.Caption := IntToStr(CalcLogic.Mul(StrToInt(Edit1.Text), StrToInt(Edit2.Text)));
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  Label1.Caption := IntToStr(CalcLogic.Division(StrToInt(Edit1.Text), StrToInt(Edit2.Text)));
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FreeAndNil(CalcLogic);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  CalcLogic := TCalcLogic.Create;
end;

end.
