unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, uConfigManager, uDeployManager, uLogManager;

type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    FConfigManager: TConfigManager;
    FDelployManager: TDeployManager;
    FLogManager: TLogManager;
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
    FLogManager := TLogManager.Create(ExtractFilePath(ParamStr(0)), 'Test.log', true);
    FDelployManager := TDeployManager.Create(FConfigManager, FLogManager);
    FDelployManager.Deploy;
    FreeAndNil(FDelployManager);
    FreeAndNil(FLogManager);
  end;
  FreeAndNil(FConfigManager);
end;

end.
