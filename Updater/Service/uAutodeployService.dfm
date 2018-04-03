object ILSAutodeployService: TILSAutodeployService
  OldCreateOrder = False
  AllowPause = False
  DisplayName = 'ILS Autodeploy Service '
  BeforeInstall = ServiceBeforeInstallUninstall
  AfterInstall = ServiceAfterInstall
  BeforeUninstall = ServiceBeforeInstallUninstall
  OnStart = ServiceStart
  OnStop = ServiceStop
  Height = 150
  Width = 215
end
