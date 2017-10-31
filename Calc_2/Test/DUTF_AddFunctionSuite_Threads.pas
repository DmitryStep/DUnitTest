// DUnit Test Framework Additional Functional Suite (DUTF_AddFunctionSuite)
//
// Модуль дополнительного функционала для DUnit
// Реализует набор функций, расширяющий возможности DUnit по работе с потоками
//
// Автор: Д.В. Степанов
// 09.02.2017
//
// История изменений:
// 09.02.2017 - создан класс для отслеживания в потоке появления диалоговых окон MessageBox, MessageDlg
// и передачи в них задаваемых из кода теста системных команд посредством SendMessage
// (класс TMessageThread)
//
// 10.02.2017 - в модуль добавлены внеклассовые методы для управления потоком TMessageThread
// из кода Unit-теста одной командой (процедуры StartMessageThread и StopMessageThread)
//
// 11.02.2017 - добавлен внеклассовый метод для имитации клика по произвольному компоненту формы
// (функция ComponentClick)

//======================================Begin DUTF_AddFunctionSuite_Threads ===================================
unit DUTF_AddFunctionSuite_Threads;

//==============================================Интерфейс==============================================
interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Controls;

const
  // Размер временной задержки, чтобы окно Message успевало отобразиться на экране, микросекунды
  MessageViewTime = 10;

type

  // Класс, реализующий поток для автонажатия кнопок на формах диалоговых окон
  TMessageThread = class (TThread)
  private
  // Приватные свойства класса:
  //
  // messageText - Заголовок диалогового окна
  // messageButtonText - Надпись на кнопке, которую нужно нажать
  // WindowsMessageCommandLevel - Сообщение WinAPI (Winapi.Messages)
  // WindowsMessageCommand - Команда WinAPI (Winapi.Messages Button Notification Codes)
  // MessagesCount - количество выведенных на экран сообщений

    messageText: PWideChar;
    messageButtonText: PWideChar;
    WindowsMessageCommandLevel: Integer;
    WindowsMessageCommand: integer;

    MessagesCount: Byte;

    procedure DoWork;
    procedure Execute(); override;

  public

    // Конструктор класса - переопределён с дополнительными параметрами,
    // чтобы передавать данные в приватные свойства перед инициализацией потока.
    //
    // Передаваемые значения:
    // Mess - заголовок окна;
    // BtnMess - надпись на кнопке;
    // CommandLevel - сообщение;
    // Command - команда.

    constructor Create (Mess: PWideChar;
                        BtnMess: PWideChar;
                        CommandLevel: Integer;
                        Command: integer); overload;

    function GetMessagesCount: byte;

  end; //TMessageThread

//-----------------------------------------------------------------------------------------------------
  // Внеклассовые процедуры модуля
  //
  // Процедуры для управления потоками TMessageThread (старт и остановка)
  //
  // Значения параметров те же, что и у конструктора класса
  // Добавляется в качестве переменного параметра
  // DialogThread - переменная, в которую возвращается указатель на конкретный поток в юнит-тесте

  procedure StartMessageThread (var DialogThread: TMessageThread;
                                Mess: PWideChar;
                                BtnMess: PWideChar;
                                CommandLevel: Integer;
                                Command: integer);

  // Остановка потока. DialogThread - идентификатор нужного потока
  procedure StopMessageThread (var DialogThread: TMessageThread);

  // Процедура реализации клика мышкой по элементу - необходима, чтобы вызвать возникновение
  // события и инициализировать его обработку по OnClick. Простой вызов OnClick
  // из кода тест-кейса выполняет только код обработчика, игнорируя API Windows.
  // Такая проблема наблюдается с автоотключением радиобоксов в пределах одного парента при выборе
  // одного из них.
  //
  // В качестве параметра ComponentName передаётся наименование компонента на форме.
  // Чтобы можно было применять к любому элементу - использован базовый класс TWinControl.
  // Результат - целое число: 0 - функция выполнена успешно, не 0 - ошибка при выполнении

  function ComponentClick (ComponentName: TWinControl): integer;


//==============================================Описание процедур и функций============================
implementation

//==============================================Класс TMessageThread===================================

procedure TMessageThread.DoWork;
var
  // Переменные для поиска хэндлов диалогового окна и кнопки в нём
  hWindow: HWND;
  hButton: HWND;
begin
  // Пока диалоговое окно и кнопка не найдены - их обработчики не определены.
  hWindow := 0;
  hButton := 0;

  // Задержка для отладки модуля. Без неё диалог не успевает отображаться.
  // Можно закомментировать, когда не нужна. Или уменьшить значение в константе MessageViewTime
  Sleep(MessageViewTime);

  hWindow := FindWindow(nil, messageText); // Находим хэндл диалога по заголовку окна

  if hWindow<>0 then // Если нашли диалоговое окно, то
  begin
    if messageButtonText<>'' then // Если текст на кнопке = непустая строка
    begin

      //Находим хэндл кнопки по надписи на ней и хэндлу родительского окна
      hButton := FindWindowEx(hWindow,
                              0,
                              'TButton',
                              messageButtonText
                             );
    end;

    // Посылаем сообщение на выполнение нужной нам команды
    if hButton <> 0 then
    begin
      inc(MessagesCount);
      SendMessage(hWindow,
                  WindowsMessageCommandLevel,
                  MakeWParam(WindowsMessageCommand,0),
                  hButton);
    end;
  end;
end;

//----------------------------------------------Execute для потока-------------------------------------
procedure TMessageThread.Execute();
begin
  while not Terminated do
  begin
    DoWork;
  end; // while not Terminated
end;
//------------------------------------End Execute------------------------------------------------------


//------------------------------------Конструктор Create-----------------------------------------------
constructor TMessageThread.Create(Mess: PWideChar;
                                  BtnMess: PWideChar;
                                  CommandLevel: Integer;
                                  Command: integer);
begin

  // Присваиваем свойствам класса значения, переданные в качестве параметров в конструктор класса
  self.messageText := Mess;
  self.messageButtonText := BtnMess;
  self.WindowsMessageCommandLevel := CommandLevel;
  self.WindowsMessageCommand := Command;

  self.MessagesCount := 0;

  // Вызываем конструктор базового класса TThread
  inherited Create(true);

end;
//------------------------------------End Create-------------------------------------------------------


//------------------------------------Begin GetMessagesCount-----------------------------------------------
function TMessageThread.GetMessagesCount: byte;
begin
  Result := MessagesCount;
end;
//--------------------------------------End GetMessagesCount-----------------------------------------------


//==============================================End TMessageThread=====================================

//====================================Внеклассовые процедуры и функции=================================

//------------------------------------Создание и старт потока------------------------------------------
procedure StartMessageThread(Var DialogThread: TMessageThread;
                             Mess: PWideChar;
                             BtnMess: PWideChar;
                             CommandLevel: Integer;
                             Command: integer);
begin
  // Создаём поток собственного класса с заданными параметрами и устанавливаем ему приоритет Normal
  DialogThread := TMessageThread.Create(Mess, BtnMess, CommandLevel, Command);
  DialogThread.Priority := tpNormal;
  DialogThread.FreeOnTerminate := true;
  //Стартуем поток
  DialogThread.Resume();
end;
//------------------------------------End StartMessageThread-------------------------------------------

//------------------------------------Остановка потока-------------------------------------------------
procedure StopMessageThread (var DialogThread: TMessageThread);
begin
  // Убиваем заданный поток.
  if not DialogThread.Terminated then
    DialogThread.Terminate();
end;
//------------------------------------End StopMessageThread--------------------------------------------

//------------------------------------Клик по комопоненту----------------------------------------------
function ComponentClick (ComponentName: TWinControl): integer;

var
  // Переменные, в которые будем получать ID обработчиков для родительского элемента и самой кнопки
  hComponent: HWND;
  hComponentParent: HWND;
  comID: integer;
  ClassName: string;

begin
  // Получаем ID обработчиков и имя класса компонента
  hComponent := ComponentName.Handle;
  hComponentParent := ComponentName.Parent.Handle;
  ClassName := LowerCase(ComponentName.ClassName);

  // Для клика по некоторым компонентам не нужен их ID, WParam формируется из команды и нуля
  // Вероятно, в будущем нужно будет придумать более простой способ разделения групп объектов,
  // для которых параметры SendMessage формируются по разному.
  // Пока оставлю так.
  if (ClassName='tbutton') or (ClassName='tradiobutton') then
  begin
    comID := 0
  end
  else
  begin
    comID := GetDlgCtrlID(hComponent);
  end;

  // Посылаем событие клика мышкой по нужному компоненту
  Result := SendMessage(hComponentParent,
                        WM_COMMAND,
                        MakeWParam(BN_CLICKED, comID),
                        hComponent
                       );
end;
//------------------------------------End ComponentClick---------------------------------------------

end.
//====================================End DUTF_AddFunctionSuite_Threads========================================
