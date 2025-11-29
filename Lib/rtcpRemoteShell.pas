{ Copyright 2004-2017 (c) RealThinClient.com (http://www.realthinclient.com) }

unit rtcpRemoteShell;

interface

{$INCLUDE rtcDefs.inc}

uses
  Windows, Classes, SysUtils,
  rtcLog, rtcInfo, rtcPortalMod;

type
  TRtcRemoteShellMode = (rsmControl, rsmHost);

  TRtcRemoteShellCommandEvent = procedure(Sender: TObject;
    const UserName, Command: String) of object;
  TRtcRemoteShellResultEvent = procedure(Sender: TObject;
    const UserName, ResultText: String) of object;
  TRtcRemoteShellErrorEvent = procedure(Sender: TObject;
    const UserName, ErrorText: String) of object;

  TRtcPRemoteShell = class(TRtcPModule)
  private
    FMode: TRtcRemoteShellMode;
    FUserName: String;

    FOnCommandReceived: TRtcRemoteShellCommandEvent;
    FOnResultReceived: TRtcRemoteShellResultEvent;
    FOnError: TRtcRemoteShellErrorEvent;

    procedure SetMode(const Value: TRtcRemoteShellMode);
    procedure SetUserName(const Value: String);

    procedure SendShellResult(const UserName, Command, Output: String;
      Sender: TObject);
    procedure SendShellError(const UserName, ErrorText: String;
      Sender: TObject);

    function ExecuteCommand(const Command: String): String;

    procedure TriggerCommandReceived(Sender: TObject; const UserName, Command: String);
    procedure TriggerResultReceived(Sender: TObject; const UserName, Output: String);
    procedure TriggerError(Sender: TObject; const UserName, ErrorText: String);

    procedure xOnCommandReceived(Sender, Obj: TObject; Data: TRtcValue);
    procedure xOnResultReceived(Sender, Obj: TObject; Data: TRtcValue);
    procedure xOnError(Sender, Obj: TObject; Data: TRtcValue);

  protected
    procedure Call_DataFromUser(Sender: TObject; const uname: string;
      Data: TRtcFunctionInfo); override;
    procedure Call_LogOut(Sender: TObject); override;

  public
    constructor Create(AOwner: TComponent); override;

    procedure Connect(const UserName: String; Sender: TObject = nil);
    procedure Listen(Sender: TObject = nil);
    procedure SendCommand(const Command: String; Sender: TObject = nil);

  published
    property Mode: TRtcRemoteShellMode read FMode write SetMode
      default rsmControl;
    property UserName: String read FUserName write SetUserName;

    property OnCommandReceived: TRtcRemoteShellCommandEvent read FOnCommandReceived
      write FOnCommandReceived;
    property OnResultReceived: TRtcRemoteShellResultEvent read FOnResultReceived
      write FOnResultReceived;
    property OnError: TRtcRemoteShellErrorEvent read FOnError write FOnError;
  end;

implementation

{ TRtcPRemoteShell }

constructor TRtcPRemoteShell.Create(AOwner: TComponent);
begin
  inherited;
  FMode := rsmControl;
  FUserName := '';
end;

procedure TRtcPRemoteShell.Connect(const UserName: String; Sender: TObject);
begin
  SetUserName(UserName);
  setSubscriber(UserName, True);
end;

procedure TRtcPRemoteShell.Listen(Sender: TObject);
begin
  Mode := rsmHost;
end;

procedure TRtcPRemoteShell.SendCommand(const Command: String; Sender: TObject);
var
  fn: TRtcFunctionInfo;
begin
  if Mode <> rsmControl then
    raise Exception.Create('SendCommand can only be used when Mode=Control');

  if not assigned(Client) then
    raise Exception.Create('Portal Client not assigned');

  if FUserName = '' then
    raise Exception.Create('UserName not set');

  fn := TRtcFunctionInfo.Create;
  try
    fn.FunctionName := 'remote-shell-command';
    fn.asText['cmd'] := Command;
    Client.SendToUser(Sender, FUserName, fn);
  except
    fn.Free;
    raise;
  end;
end;

procedure TRtcPRemoteShell.SetMode(const Value: TRtcRemoteShellMode);
begin
  FMode := Value;
end;

procedure TRtcPRemoteShell.SetUserName(const Value: String);
begin
  FUserName := Value;
end;

procedure TRtcPRemoteShell.TriggerCommandReceived(Sender: TObject;
  const UserName, Command: String);
var
  v: TRtcValue;
begin
  if assigned(FOnCommandReceived) then
  begin
    v := TRtcValue.Create;
    try
      with v.NewRecord do
      begin
        asText['user'] := UserName;
        asText['cmd'] := Command;
      end;
      CallEvent(Sender, xOnCommandReceived, v);
    finally
      v.Free;
    end;
  end;
end;

procedure TRtcPRemoteShell.TriggerError(Sender: TObject;
  const UserName, ErrorText: String);
var
  v: TRtcValue;
begin
  if assigned(FOnError) then
  begin
    v := TRtcValue.Create;
    try
      with v.NewRecord do
      begin
        asText['user'] := UserName;
        asText['err'] := ErrorText;
      end;
      CallEvent(Sender, xOnError, v);
    finally
      v.Free;
    end;
  end;
end;

procedure TRtcPRemoteShell.TriggerResultReceived(Sender: TObject;
  const UserName, Output: String);
var
  v: TRtcValue;
begin
  if assigned(FOnResultReceived) then
  begin
    v := TRtcValue.Create;
    try
      with v.NewRecord do
      begin
        asText['user'] := UserName;
        asText['out'] := Output;
      end;
      CallEvent(Sender, xOnResultReceived, v);
    finally
      v.Free;
    end;
  end;
end;

procedure TRtcPRemoteShell.Call_DataFromUser(Sender: TObject;
  const uname: string; Data: TRtcFunctionInfo);
var
  cmd, output: String;
begin
  if Data.FunctionName = 'remote-shell-command' then
  begin
    if Mode = rsmHost then
    begin
      cmd := Data.asText['cmd'];
      TriggerCommandReceived(Sender, uname, cmd);

      TThread.CreateAnonymousThread(
        procedure
        var
          resText: String;
        begin
          try
            resText := ExecuteCommand(cmd);
            SendShellResult(uname, cmd, resText, Sender);
            TriggerResultReceived(Sender, uname, resText);
          except
            on E: Exception do
            begin
              SendShellError(uname, E.Message, Sender);
              TriggerError(Sender, uname, E.Message);
            end;
          end;
        end).Start;
    end;
  end
  else if Data.FunctionName = 'remote-shell-result' then
  begin
    if Mode = rsmControl then
    begin
      output := Data.asText['out'];
      TriggerResultReceived(Sender, uname, output);
    end;
  end
  else if Data.FunctionName = 'remote-shell-error' then
  begin
    TriggerError(Sender, uname, Data.asText['err']);
  end;
end;

procedure TRtcPRemoteShell.Call_LogOut(Sender: TObject);
begin
  inherited;
  FUserName := '';
end;

function TRtcPRemoteShell.ExecuteCommand(const Command: String): String;
const
  READ_BUFFER_SIZE = 4096;
var
  Security: TSecurityAttributes;
  ReadPipe, WritePipe: THandle;
  StartInfo: TStartupInfo;
  ProcInfo: TProcessInformation;
  Buffer: AnsiString;
  Chunk: AnsiString;
  BytesRead: DWORD;
  WaitRes: DWORD;
  TempBuffer: array [0 .. READ_BUFFER_SIZE - 1] of AnsiChar;
  CmdLine: String;
begin
  Result := '';

  FillChar(Security, SizeOf(Security), 0);
  Security.nLength := SizeOf(Security);
  Security.bInheritHandle := True;

  if not CreatePipe(ReadPipe, WritePipe, @Security, 0) then
    raise Exception.Create('Unable to create pipe for shell command');
  try
    FillChar(StartInfo, SizeOf(StartInfo), 0);
    StartInfo.cb := SizeOf(StartInfo);
    StartInfo.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
    StartInfo.wShowWindow := SW_HIDE;
    StartInfo.hStdInput := 0;
    StartInfo.hStdOutput := WritePipe;
    StartInfo.hStdError := WritePipe;

    CmdLine := 'cmd.exe /C ' + Command;

    if not CreateProcess(nil, PChar(CmdLine), nil, nil, True, CREATE_NO_WINDOW,
      nil, nil, StartInfo, ProcInfo) then
      raise Exception.Create('Unable to start shell process');
    try
      CloseHandle(WritePipe);
      WritePipe := 0;

      WaitRes := WaitForSingleObject(ProcInfo.hProcess, INFINITE);
      if WaitRes = WAIT_FAILED then
        raise Exception.Create('Shell process wait failed');

      repeat
        if not ReadFile(ReadPipe, TempBuffer, READ_BUFFER_SIZE, BytesRead, nil) then
          Break;
        if BytesRead > 0 then
        begin
          SetString(Chunk, PAnsiChar(@TempBuffer[0]), BytesRead);
          Buffer := Buffer + Chunk;
        end;
      until BytesRead = 0;

      Result := String(Buffer);
    finally
      CloseHandle(ProcInfo.hProcess);
      CloseHandle(ProcInfo.hThread);
    end;
  finally
    if WritePipe <> 0 then
      CloseHandle(WritePipe);
    CloseHandle(ReadPipe);
  end;
end;

procedure TRtcPRemoteShell.SendShellError(const UserName, ErrorText: String;
  Sender: TObject);
var
  fn: TRtcFunctionInfo;
begin
  if not assigned(Client) then Exit;

  fn := TRtcFunctionInfo.Create;
  try
    fn.FunctionName := 'remote-shell-error';
    fn.asText['err'] := ErrorText;
    Client.SendToUser(Sender, UserName, fn);
  except
    fn.Free;
    raise;
  end;
end;

procedure TRtcPRemoteShell.SendShellResult(const UserName, Command,
  Output: String; Sender: TObject);
var
  fn: TRtcFunctionInfo;
begin
  if not assigned(Client) then Exit;

  fn := TRtcFunctionInfo.Create;
  try
    fn.FunctionName := 'remote-shell-result';
    fn.asText['cmd'] := Command;
    fn.asText['out'] := Output;
    Client.SendToUser(Sender, UserName, fn);
  except
    fn.Free;
    raise;
  end;
end;

procedure TRtcPRemoteShell.xOnCommandReceived(Sender, Obj: TObject;
  Data: TRtcValue);
begin
  if assigned(FOnCommandReceived) then
    FOnCommandReceived(Obj, Data.asText['user'], Data.asText['cmd']);
end;

procedure TRtcPRemoteShell.xOnError(Sender, Obj: TObject; Data: TRtcValue);
begin
  if assigned(FOnError) then
    FOnError(Obj, Data.asText['user'], Data.asText['err']);
end;

procedure TRtcPRemoteShell.xOnResultReceived(Sender, Obj: TObject;
  Data: TRtcValue);
begin
  if assigned(FOnResultReceived) then
    FOnResultReceived(Obj, Data.asText['user'], Data.asText['out']);
end;

end.
