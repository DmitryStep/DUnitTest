unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, uConfigManager, uDeployManager;

type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    FConfigManager: TConfigManager;
    FDelployManager: TDeployManager;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  FConfigManager := TConfigManager.Create(ExtractFilePath(Application.ExeName) +
                                          'testconfig.ini');
  if FConfigManager.LoadSettings then
  begin
    FDelployManager := TDeployManager.Create(FConfigManager);
    FDelployManager.Deploy;
    FreeAndNil(FDelployManager);
  end;
  FreeAndNil(FConfigManager);
end;

end.
