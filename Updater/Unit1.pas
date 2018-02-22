unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, IdBaseComponent, IdComponent, uJenkinsAPI,
  Vcl.StdCtrls, Vcl.Grids, Vcl.ValEdit, uServicesManager;

type
  TForm1 = class(TForm)
    Button2: TButton;
    Memo1: TMemo;
    ListBox1: TListBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Button1: TButton;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;


var
  Jenk: TJenkinsAPI;
  ServManager: TServiceManager;

implementation

{$R *.dfm}


procedure TForm1.Button1Click(Sender: TObject);
begin
  if Assigned(ServManager) then
  begin
    if Button1.Caption = 'Start' then
    begin
      ServManager.RunService('ose');
      Button1.Caption := 'Stop';
    end
    else
    begin
      ServManager.StopService('ose');
      Button1.Caption := 'Start';
    end;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  Res: string;
  Code: integer;
  Res1: TStringList;
  i: integer;
  LastReleaseName: string;
begin
  Memo1.Clear;
  ListBox1.Clear;
  Label1.Caption := '';
  Label2.Caption := '';
  Label3.Caption := '';
  Label4.Caption := '';
  Label5.Caption := '';
  Jenk := TJenkinsAPI.Create('http://test.ils-glonass.ru:8080', 'DStepanov', 'DS123');
  ServManager := TServiceManager.Create();
  Code := Jenk.GetJobsList(Res, 'ILS Monitoring Project/ILS Monitoring Release');
  Memo1.Text := IntToStr(Code) + ' ' + Res;
  Res1 := TStringList.Create;
  Code := Jenk.GetJobsList(Res1, 'ILS Monitoring Project/ILS Monitoring Release');
  for i := 0 to Res1.Count - 1 do
  begin
    ListBox1.Items.Add(Res1.Strings[i]);
  end;
  LastReleaseName := Jenk.GetLastReleaseName('ILS Monitoring Project/ILS Monitoring Release');
  Label1.Caption := Jenk.GetLastReleaseNumber('ILS Monitoring Project/ILS Monitoring Release', 'release/') + #10#13 + LastReleaseName;

  Label2.Caption := 'Failed: ' + Jenk.GetLastBuildNumberStr(bsFailed, 'ILS Monitoring Project/ILS Monitoring Release/', LastReleaseName) + #10#13 +
                    'Stable: ' +  Jenk.GetLastBuildNumberStr(bsStable, 'ILS Monitoring Project/ILS Monitoring Release/', LastReleaseName) + #10#13 +
                    'Unstable: ' +  Jenk.GetLastBuildNumberStr(bsUnstable, 'ILS Monitoring Project/ILS Monitoring Release/', LastReleaseName) + #10#13 +
                    'Last: ' +  Jenk.GetLastBuildNumberStr(bsAll, 'ILS Monitoring Project/ILS Monitoring Release/', LastReleaseName);
  Label3.Caption := Jenk.GetArtifactFileName('ILS Monitoring Project/ILS Monitoring Release/', LastReleaseName, Jenk.GetLastBuildNumberStr(bsStable, 'ILS Monitoring Project/ILS Monitoring Release/', LastReleaseName));
{  Label4.Caption := BoolToStr(Jenk.GetFile('ILS Monitoring Project/ILS Monitoring Release/',
                              LastReleaseName,
                              Jenk.GetLastBuildNumberStr(bsStable, 'ILS Monitoring Project/ILS Monitoring Release/', LastReleaseName),
                              Label3.Caption,
                              ExtractFilePath(Application.ExeName) + '\release' + Jenk.GetLastReleaseNumber('ILS Monitoring Project/ILS Monitoring Release', 'release/') + '.zip'));  Label5.Caption := ServManager.GetLocalName;}
  Res1.Free;
  Jenk.Free;
  ServManager.Free;
end;

end.
