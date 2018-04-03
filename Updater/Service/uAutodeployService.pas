unit uAutodeployService;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.SvcMgr, Vcl.Dialogs,
  uConfigManager, uDeployManager, uLogManager, WinSvc;

type
  TILSAutodeployService = class(TService)
    procedure ServiceAfterInstall(Sender: TService);
    procedure ServiceBeforeInstallUninstall(Sender: TService);
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
  private
    { Private declarations }
    FServiceName: string;
    FIniFileName: string;
    FLogFileName: string;
    FConfigManager: TConfigManager;
    FDeployManager: TDeployManager;
    FLogManager: TLogManager;
  public
    { Public declarations }
    function GetServiceInstance: string;
    function GetServiceController: TServiceController; override;
  end;

var
  ILSAutodeployService: TILSAutodeployService;

implementation

{$R *.DFM}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  ILSAutodeployService.Controller(CtrlCode);
end; // ServiceController

// ----------------------------------------------------------------------------------------------------------------------------------

function TILSAutodeployService.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end; // GetServiceController

// ----------------------------------------------------------------------------------------------------------------------------------

procedure TILSAutodeployService.ServiceBeforeInstallUninstall(Sender: TService);
var
  ConfigParam: string;
begin
  ConfigParam := ParamStr(1);
  if (ConfigParam <> '') and (pos('/work:', ConfigParam) = 1) then
  begin
    Delete(ConfigParam, 1, 6);
    Name := 'ILSAutodeployService_' + ConfigParam;
    DisplayName := 'ILS Autodeploy Service ' + ConfigParam;
  end
  else
  begin
    Name := 'ILSAutodeployService';
    DisplayName := 'ILS Autodeploy Service';
  end;
end; // ServiceBeforeInstallUninstall

// ----------------------------------------------------------------------------------------------------------------------------------

procedure TILSAutodeployService.ServiceStart(Sender: TService;
  var Started: Boolean);
var
  i: integer;
begin
  try
    Started := false;
    FServiceName := GetServiceInstance();
    FIniFileName := ExtractFilePath(ParamStr(0)) + 'Autodeploy' +
                    FServiceName + '.ini';
    FConfigManager := TConfigManager.Create(FIniFileName);
    try
      if FConfigManager.LoadSettings then
      begin
        FLogFileName := FConfigManager.GlobalSettings.LoggerSettings.s_LogFileName;
        i := length(FLogFileName);
        while FLogFileName[i] <> '.' do
          dec(i);
        Insert(FServiceName, FLogFileName, i);
        FLogManager := TLogManager.Create(FConfigManager.GlobalSettings.LoggerSettings.s_LogDir,
                                          FLogFileName,
                                          FConfigManager.GlobalSettings.LoggerSettings.b_IsUseDataInLogFilename,
                                          FConfigManager.GlobalSettings.LoggerSettings.s_LogDetalization,
                                          FConfigManager.GlobalSettings.LoggerSettings.s_IsUseErrorFile);
        FLogManager.WriteInfoMessageToLog('==================== SERVICE ' +
                                           DisplayName +
                                           ' STARTED ====================');
        FDeployManager := TDeployManager.Create(FConfigManager, FLogManager);
        Started := true;
      end
      else
      begin
        FConfigManager := nil;
        FLogManager := nil;
        FDeployManager := nil;
        FLogManager.WriteErrorMessageToLog('SERVICE ' + DisplayName + ' NOT STARTED: ' +
                                           'отсутствует файл настроек ' + FIniFileName, '', '');
      end;
    except
      on E: Exception do
        FLogManager.WriteErrorMessageToLog('Ошибка при старте сервиса!',
                                           E.Message,
                                           E.StackTrace);
    end;
  except
    on E: Exception do
      FLogManager.WriteErrorMessageToLog('Ошибка при старте сервиса!',
                                         E.Message,
                                         E.StackTrace);
  end;
end; procedure TILSAutodeployService.ServiceStop(Sender: TService;
  var Stopped: Boolean);
begin
  FreeAndNil(FDeployManager);
  if Assigned(FLogManager) then
    FLogManager.WriteDebugMessageToLog('==================== SERVICE '+
                                       DisplayName +
                                       ' STOPPED! ====================');
  FreeAndNil(FLogManager);
  FreeAndNil(FConfigManager);
  Stopped := true;
end;

// ServiceStart

// ----------------------------------------------------------------------------------------------------------------------------------

function TILSAutodeployService.GetServiceInstance: string;
var
  ConfigParam: string;
begin
  Result := '';
  ConfigParam := ParamStr(1);
  if (Length(ConfigParam) > 6) and (pos('/work:', ConfigParam) = 1) then
    Result := Copy(ConfigParam, 7, length(ConfigParam) - 6);
end; // GetServiceInstance

// ----------------------------------------------------------------------------------------------------------------------------------

// Copiright by Anton Omelchenko
procedure TILSAutodeployService.ServiceAfterInstall(Sender: TService);
var
  ConfigParam: string;
  SCManager: SC_HANDLE;
  OurService: SC_HANDLE;
begin
  ConfigParam := GetServiceInstance();
  if ( ConfigParam <> '' ) then
  begin
    SCManager := OpenSCManager( nil, nil, SC_MANAGER_ALL_ACCESS );
    if ( SCManager <> 0 ) then
    begin
      OurService := OpenService( SCManager, PChar( 'ILSAutodeployService_' + ConfigParam ), SERVICE_ALL_ACCESS );
      if ( OurService <> 0 ) then
      begin
        if not ChangeServiceConfig(
          OurService,
          SERVICE_WIN32_OWN_PROCESS,
          SERVICE_AUTO_START,
          SERVICE_ERROR_NORMAL,
          PChar(ParamStr(0) + ' /work:' + ConfigParam),
          nil,
          nil,
          nil,
          nil,
          nil,
          nil
        )
        then
          raise Exception.Create( 'Не удалось изменение статуса службы' );
      end
      else
        raise Exception.Create( 'Не удалось подключение к службе' );
    end
    else
      raise Exception.Create( 'Не удалось подключение к менеджеру служб' );
  end;
end; // ServiceAfterInstall


end. // uAutodeployService
