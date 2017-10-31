// DUnit Test Framework Additional Functional Suite (DUTF_AddFunctionSuite)
//
// ������ ��������������� ����������� ��� DUnit
// ��������� ����� �������, ����������� ����������� DUnit �� ������ � ��������
//
// �����: �.�. ��������
// 09.02.2017
//
// ������� ���������:
// 09.02.2017 - ������ ����� ��� ������������ � ������ ��������� ���������� ���� MessageBox, MessageDlg
// � �������� � ��� ���������� �� ���� ����� ��������� ������ ����������� SendMessage
// (����� TMessageThread)
//
// 10.02.2017 - � ������ ��������� ������������ ������ ��� ���������� ������� TMessageThread
// �� ���� Unit-����� ����� �������� (��������� StartMessageThread � StopMessageThread)
//
// 11.02.2017 - �������� ������������ ����� ��� �������� ����� �� ������������� ���������� �����
// (������� ComponentClick)

//======================================Begin DUTF_AddFunctionSuite_Threads ===================================
unit DUTF_AddFunctionSuite_Threads;

//==============================================���������==============================================
interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Controls;

const
  // ������ ��������� ��������, ����� ���� Message �������� ������������ �� ������, ������������
  MessageViewTime = 10;

type

  // �����, ����������� ����� ��� ����������� ������ �� ������ ���������� ����
  TMessageThread = class (TThread)
  private
  // ��������� �������� ������:
  //
  // messageText - ��������� ����������� ����
  // messageButtonText - ������� �� ������, ������� ����� ������
  // WindowsMessageCommandLevel - ��������� WinAPI (Winapi.Messages)
  // WindowsMessageCommand - ������� WinAPI (Winapi.Messages Button Notification Codes)
  // MessagesCount - ���������� ���������� �� ����� ���������

    messageText: PWideChar;
    messageButtonText: PWideChar;
    WindowsMessageCommandLevel: Integer;
    WindowsMessageCommand: integer;

    MessagesCount: Byte;

    procedure DoWork;
    procedure Execute(); override;

  public

    // ����������� ������ - ������������ � ��������������� �����������,
    // ����� ���������� ������ � ��������� �������� ����� �������������� ������.
    //
    // ������������ ��������:
    // Mess - ��������� ����;
    // BtnMess - ������� �� ������;
    // CommandLevel - ���������;
    // Command - �������.

    constructor Create (Mess: PWideChar;
                        BtnMess: PWideChar;
                        CommandLevel: Integer;
                        Command: integer); overload;

    function GetMessagesCount: byte;

  end; //TMessageThread

//-----------------------------------------------------------------------------------------------------
  // ������������ ��������� ������
  //
  // ��������� ��� ���������� �������� TMessageThread (����� � ���������)
  //
  // �������� ���������� �� ��, ��� � � ������������ ������
  // ����������� � �������� ����������� ���������
  // DialogThread - ����������, � ������� ������������ ��������� �� ���������� ����� � ����-�����

  procedure StartMessageThread (var DialogThread: TMessageThread;
                                Mess: PWideChar;
                                BtnMess: PWideChar;
                                CommandLevel: Integer;
                                Command: integer);

  // ��������� ������. DialogThread - ������������� ������� ������
  procedure StopMessageThread (var DialogThread: TMessageThread);

  // ��������� ���������� ����� ������ �� �������� - ����������, ����� ������� �������������
  // ������� � ���������������� ��� ��������� �� OnClick. ������� ����� OnClick
  // �� ���� ����-����� ��������� ������ ��� �����������, ��������� API Windows.
  // ����� �������� ����������� � ��������������� ����������� � �������� ������ ������� ��� ������
  // ������ �� ���.
  //
  // � �������� ��������� ComponentName ��������� ������������ ���������� �� �����.
  // ����� ����� ���� ��������� � ������ �������� - ����������� ������� ����� TWinControl.
  // ��������� - ����� �����: 0 - ������� ��������� �������, �� 0 - ������ ��� ����������

  function ComponentClick (ComponentName: TWinControl): integer;


//==============================================�������� �������� � �������============================
implementation

//==============================================����� TMessageThread===================================

procedure TMessageThread.DoWork;
var
  // ���������� ��� ������ ������� ����������� ���� � ������ � ��
  hWindow: HWND;
  hButton: HWND;
begin
  // ���� ���������� ���� � ������ �� ������� - �� ����������� �� ����������.
  hWindow := 0;
  hButton := 0;

  // �������� ��� ������� ������. ��� �� ������ �� �������� ������������.
  // ����� ����������������, ����� �� �����. ��� ��������� �������� � ��������� MessageViewTime
  Sleep(MessageViewTime);

  hWindow := FindWindow(nil, messageText); // ������� ����� ������� �� ��������� ����

  if hWindow<>0 then // ���� ����� ���������� ����, ��
  begin
    if messageButtonText<>'' then // ���� ����� �� ������ = �������� ������
    begin

      //������� ����� ������ �� ������� �� ��� � ������ ������������� ����
      hButton := FindWindowEx(hWindow,
                              0,
                              'TButton',
                              messageButtonText
                             );
    end;

    // �������� ��������� �� ���������� ������ ��� �������
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

//----------------------------------------------Execute ��� ������-------------------------------------
procedure TMessageThread.Execute();
begin
  while not Terminated do
  begin
    DoWork;
  end; // while not Terminated
end;
//------------------------------------End Execute------------------------------------------------------


//------------------------------------����������� Create-----------------------------------------------
constructor TMessageThread.Create(Mess: PWideChar;
                                  BtnMess: PWideChar;
                                  CommandLevel: Integer;
                                  Command: integer);
begin

  // ����������� ��������� ������ ��������, ���������� � �������� ���������� � ����������� ������
  self.messageText := Mess;
  self.messageButtonText := BtnMess;
  self.WindowsMessageCommandLevel := CommandLevel;
  self.WindowsMessageCommand := Command;

  self.MessagesCount := 0;

  // �������� ����������� �������� ������ TThread
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

//====================================������������ ��������� � �������=================================

//------------------------------------�������� � ����� ������------------------------------------------
procedure StartMessageThread(Var DialogThread: TMessageThread;
                             Mess: PWideChar;
                             BtnMess: PWideChar;
                             CommandLevel: Integer;
                             Command: integer);
begin
  // ������ ����� ������������ ������ � ��������� ����������� � ������������� ��� ��������� Normal
  DialogThread := TMessageThread.Create(Mess, BtnMess, CommandLevel, Command);
  DialogThread.Priority := tpNormal;
  DialogThread.FreeOnTerminate := true;
  //�������� �����
  DialogThread.Resume();
end;
//------------------------------------End StartMessageThread-------------------------------------------

//------------------------------------��������� ������-------------------------------------------------
procedure StopMessageThread (var DialogThread: TMessageThread);
begin
  // ������� �������� �����.
  if not DialogThread.Terminated then
    DialogThread.Terminate();
end;
//------------------------------------End StopMessageThread--------------------------------------------

//------------------------------------���� �� �����������----------------------------------------------
function ComponentClick (ComponentName: TWinControl): integer;

var
  // ����������, � ������� ����� �������� ID ������������ ��� ������������� �������� � ����� ������
  hComponent: HWND;
  hComponentParent: HWND;
  comID: integer;
  ClassName: string;

begin
  // �������� ID ������������ � ��� ������ ����������
  hComponent := ComponentName.Handle;
  hComponentParent := ComponentName.Parent.Handle;
  ClassName := LowerCase(ComponentName.ClassName);

  // ��� ����� �� ��������� ����������� �� ����� �� ID, WParam ����������� �� ������� � ����
  // ��������, � ������� ����� ����� ��������� ����� ������� ������ ���������� ����� ��������,
  // ��� ������� ��������� SendMessage ����������� �� �������.
  // ���� ������� ���.
  if (ClassName='tbutton') or (ClassName='tradiobutton') then
  begin
    comID := 0
  end
  else
  begin
    comID := GetDlgCtrlID(hComponent);
  end;

  // �������� ������� ����� ������ �� ������� ����������
  Result := SendMessage(hComponentParent,
                        WM_COMMAND,
                        MakeWParam(BN_CLICKED, comID),
                        hComponent
                       );
end;
//------------------------------------End ComponentClick---------------------------------------------

end.
//====================================End DUTF_AddFunctionSuite_Threads========================================
