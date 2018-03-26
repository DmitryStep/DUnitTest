object AutodeployService: TAutodeployService
  OldCreateOrder = False
  DisplayName = 'AutodeployService'
  OnStart = ServiceStart
  OnStop = ServiceStop
  Height = 150
  Width = 215
end
