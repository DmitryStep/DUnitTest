unit TestRunner;

interface

uses
  Winapi.Windows, Vcl.Dialogs, System.SysUtils, System.Classes, GuiTestRunner, TestFramework, TestSettings;

const
  NOT_REWRITE_FILE = True;
  REWRITE_FILE = False;

  DIRECTION_LEFT = 0;
  DIRECTION_CENTER = 1;
  DIRECTION_RIGHT = 2;

function GetTemplateDir: string;
function GetDUnitTemplateConfigName: string;
function FormatString(SourceString: string; Direction: byte; LengthString: Byte): string;
procedure PrepareConfigFile(TemplateFile: string);
procedure RunMyTest(test: ITest);
procedure RunMyRegisteredTests;
function GetLogFileSize(LogFileName: string): integer;
procedure CheckLogFile(LogFileName: string);

implementation


// Определение пути хранения шаблона настроечного ini-файла для тестов
function GetTemplateDir: string;
begin
  Result := ExtractFilePath(ParamStr(0)) + 'Templates\';
end; // GetTemplateDir


// Определение полного пути и имени конфиг-файла, переданного в качестве настроечного в ParamStr(2)
function GetDUnitTemplateConfigName: string;
begin
  Result := GetTemplateDir() + GetConfigSettings();
end; //GetDUnitConfigName


// Копирование шаблона ini для теста
procedure PrepareConfigFile(TemplateFile: string);
var
  CurrentConfigFile: string;

begin
  if  TemplateFile <> '' then
  begin
    CurrentConfigFile := ExtractFilePath(ParamStr(0)) + 'dunit.ini';
    if FileExists(CurrentConfigFile) then
    begin
      DeleteFile(CurrentConfigFile);
    end;
    CopyFile(PChar(TemplateFile), PChar(CurrentConfigFile), REWRITE_FILE);
  end;
end; // PrepareConfigFile


// Форматирование строки вывода
function FormatString(SourceString: string; Direction: byte; LengthString: Byte): string;
begin
  Result := SourceString;

  while Length(Result) <= LengthString do
  begin

    if Direction = DIRECTION_LEFT then
      Result := Result + ' '
    else
    if Direction = DIRECTION_RIGHT then
      Result := ' ' + Result
    else
    if Direction = DIRECTION_CENTER then
    begin
      Result := Result + ' ';
      if Length(Result) < LengthString then
        Result := ' ' + Result;
    end;
  end;
end; // FormatString


// Вывод результатов тестирования в файл
procedure OutTestResultsToFile(TestRunnerContext: TGUITestRunner);
var
  i, TreeViewIndex: integer;
  LogFile: TextFile;
  TestName: string;
  LogStr: string;
  ResultImageIndex: integer;
  TestResultString: string;
begin
  with TestRunnerContext do
  begin
    AssignFile(LogFile, ExtractFilePath(ParamStr(0)) + ExtractFileName(ParamStr(0)) + '.log');
    Rewrite(LogFile);

    for TreeViewIndex := 0 to TestTree.Items.Count - 1 do
    begin

      if TestTree.Items[TreeViewIndex].getFirstChild() = nil then
      begin

        ResultImageIndex := TestTree.Items[TreeViewIndex].ImageIndex;

        case ResultImageIndex of
          imgNONE:    TestResultString := 'DISABLED';
          imgRUN:     TestResultString := 'PASSED';
          imgFAILED:  TestResultString := 'FAILED';
          imgERROR:   TestResultString := 'ERROR';
        end;

        if (ResultImageIndex = imgFAILED) or
           (ResultImageIndex = imgERROR)
        then
        begin
          for I := 0 to FailureListView.Items.Count - 1 do
          begin
            TestName := FailureListView.Items[i].Caption;
            if TestName = TestTree.Items[TreeViewIndex].Text then
            begin
              logStr := FailureListView.Items[i].SubItems[1];
              Writeln(LogFile, FormatString(TestName, DIRECTION_LEFT, 100), FormatString(TestResultString, DIRECTION_LEFT, 10),
                      LogStr);
            end;
          end;
        end
        else
        begin
          TestName := TestTree.Items[TreeViewIndex].Text;
          Writeln(LogFile, FormatString(TestName, DIRECTION_LEFT, 100), FormatString(TestResultString, DIRECTION_LEFT, 10));
        end;
      end;
    end;

    CloseFile(LogFile);
  end;
end; // OutTestResultsToFile


// Создание формы и запуск теста
procedure RunMyTest(test: ITest);
var
  TestRunnerContext: TGUITestRunner;
begin
  TestRunnerContext := TGUITestRunner.Create(nil);
  with TestRunnerContext  do
  begin
    try
      Suite := test;
      if not GetAutoStartSetting() then
      begin
        ShowModal();     // Ручной режим запуска
      end
      else
      begin
        PrepareConfigFile(GetDUnitTemplateConfigName());
        Show();          //Автоматический режим запуска
        RunActionExecute(nil); // автонажатие кнопки запуска тестов.
      end;
      OutTestResultsToFile(TestRunnerContext);
    finally
      Free();
    end;
  end;
end; // RunMyTest


// Получение размера файла лога
function GetLogFileSize(LogFileName: string): integer;
var
  MemoryStream: TMemoryStream;

begin
  if FileExists(LogFileName) then
  begin
    MemoryStream := nil;
    MemoryStream := TMemoryStream.Create();
    try
      MemoryStream.LoadFromFile(LogFileName);
      Result := MemoryStream.Size;
    finally
      FreeAndNil(MemoryStream);
    end;
  end
  else
  begin
    Result := 0;
  end;
end; // GetLogFileSize


// Проверяет размер файла лога и если он нулевой - удаляет файл
procedure CheckLogFile(LogFileName: string);
begin
  if FileExists(LogFileName) and
     (GetLogFileSize(LogFileName) = 0)
  then
  begin
      DeleteFile(LogFileName);
  end;
end; // CheckLogFile


// Запуск тестов
procedure RunMyRegisteredTests;
begin
   RunMyTest(registeredTests)
end; // RunMyRegisteredTests

end.
