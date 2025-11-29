{ Copyright 2004-2017 (c) RealThinClient.com (http://www.realthinclient.com) }

unit rtcpRemoteCamVCL;

interface

{$INCLUDE rtcDefs.inc}

uses
  Windows, Messages, Classes, SysUtils, Graphics, Controls, ExtCtrls, JPEG,
  rtcLog, rtcInfo, rtcPortalMod;

type
  TRtcRemoteCamMode = (rmCapture, rmViewer);

  TRtcRemoteCamErrorEvent = procedure(Sender: TObject; const UserName,
    ErrorText: String) of object;
  TRtcRemoteCamFrameEvent = procedure(Sender: TObject; const UserName: String;
    Frame: TBitmap) of object;

  TRemoteCamVCL = class(TRtcPModule)
  private
    FCaptureHandle: HWND;
    FMode: TRtcRemoteCamMode;
    FUserName: String;
    FTargetDisplay: TControl;
    FFrameInterval: Cardinal;
    FJpegQuality: Integer;

    FCaptureThread: TThread;

    FOnError: TRtcRemoteCamErrorEvent;
    FOnFrameReceived: TRtcRemoteCamFrameEvent;

    procedure SetMode(const Value: TRtcRemoteCamMode);
    procedure SetUserName(const Value: String);
    procedure SetTargetDisplay(const Value: TControl);
    procedure SetFrameInterval(const Value: Cardinal);
    procedure SetJpegQuality(const Value: Integer);

    procedure EnsureCaptureWindow;
    procedure DestroyCaptureWindow;

    function CaptureFrame: TBitmap;
    procedure SendFrameFrame(const Bitmap: TBitmap; Sender: TObject);

    procedure SendCommand(const Cmd: String; Sender: TObject);
    procedure HandleCommand(const Cmd, UserName: String; Sender: TObject);

    procedure NotifyError(Sender: TObject; const UserName, ErrorText: String);
    procedure DeliverFrame(Sender: TObject; const UserName: String;
      Frame: TBitmap);

    procedure xOnError(Sender, Obj: TObject; Data: TRtcValue);
    procedure xOnFrame(Sender, Obj: TObject; Data: TRtcValue);

  protected
    procedure Call_DataFromUser(Sender: TObject; const uname: string;
      Data: TRtcFunctionInfo); override;
    procedure Call_LogOut(Sender: TObject); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Connect(const UserName: String; Sender: TObject = nil);
    procedure Listen(Sender: TObject = nil);

    procedure Start(Sender: TObject = nil);
    procedure Stop(Sender: TObject = nil);

  published
    property Mode: TRtcRemoteCamMode read FMode write SetMode default rmCapture;
    property UserName: String read FUserName write SetUserName;
    property TargetDisplay: TControl read FTargetDisplay write SetTargetDisplay;
    property FrameInterval: Cardinal read FFrameInterval write SetFrameInterval
      default 200;
    property JpegQuality: Integer read FJpegQuality write SetJpegQuality
      default 70;

    property OnError: TRtcRemoteCamErrorEvent read FOnError write FOnError;
    property OnFrameReceived: TRtcRemoteCamFrameEvent read FOnFrameReceived
      write FOnFrameReceived;
  end;

implementation

const
  WM_CAP_START = $0400;
  WM_CAP_DRIVER_CONNECT = WM_CAP_START + 10;
  WM_CAP_DRIVER_DISCONNECT = WM_CAP_START + 11;
  WM_CAP_SET_PREVIEW = WM_CAP_START + 50;

function capCreateCaptureWindowA(lpszWindowName: PAnsiChar; dwStyle: DWord;
  x, y, nWidth, nHeight: Integer; ParentWin: HWND; nId: Integer): HWND; stdcall;
  external 'avicap32.dll';

function CreateHiddenCaptureWindow: HWND;
begin
  Result := capCreateCaptureWindowA('RTC_REMOTE_CAM', WS_POPUP or WS_DISABLED,
    0, 0, 320, 240, 0, 0);
end;

type
  TRtcCamSendThread = class(TThread)
  private
    FCam: TRemoteCamVCL;
    FOwnerSender: TObject;
  protected
    procedure Execute; override;
  public
    constructor Create(ACam: TRemoteCamVCL; ASender: TObject);
  end;

  TRtcCamFrameThread = class(TThread)
  private
    FCam: TRemoteCamVCL;
    FOwnerSender: TObject;
    FUser: String;
    FData: TMemoryStream;
    FBmp: TBitmap;
  protected
    procedure Execute; override;
  public
    constructor Create(ACam: TRemoteCamVCL; const AUser: String;
      AData: TStream; ASender: TObject);
    destructor Destroy; override;
  end;

{ TRtcCamSendThread }

constructor TRtcCamSendThread.Create(ACam: TRemoteCamVCL; ASender: TObject);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FCam := ACam;
  FOwnerSender := ASender;
  Resume;
end;

procedure TRtcCamSendThread.Execute;
var
  bmp: TBitmap;
begin
  while not Terminated do
  begin
    bmp := nil;
    try
      bmp := FCam.CaptureFrame;
      if assigned(bmp) then
        FCam.SendFrameFrame(bmp, FOwnerSender);
    except
      on E: Exception do
        FCam.NotifyError(FOwnerSender, FCam.UserName, E.Message);
    end;
    bmp.Free;
    Sleep(FCam.FrameInterval);
  end;
end;

{ TRtcCamFrameThread }

constructor TRtcCamFrameThread.Create(ACam: TRemoteCamVCL; const AUser: String;
  AData: TStream; ASender: TObject);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FCam := ACam;
  FUser := AUser;
  FOwnerSender := ASender;
  FData := TMemoryStream.Create;
  FData.CopyFrom(AData, 0);
  Resume;
end;

  destructor TRtcCamFrameThread.Destroy;
  begin
    FreeAndNil(FBmp);
    FData.Free;
    inherited;
  end;

procedure TRtcCamFrameThread.Execute;
var
  jpg: TJPEGImage;
begin
  jpg := TJPEGImage.Create;
  try
    FData.Position := 0;
    jpg.LoadFromStream(FData);
    FBmp := TBitmap.Create;
    FBmp.Assign(jpg);
    Synchronize(
      procedure
      begin
        FCam.DeliverFrame(FOwnerSender, FUser, FBmp);
      end);
  except
    on E: Exception do
      FCam.NotifyError(FOwnerSender, FUser, E.Message);
  end;
  FreeAndNil(FBmp);
  jpg.Free;
end;

{ TRemoteCamVCL }

constructor TRemoteCamVCL.Create(AOwner: TComponent);
begin
  inherited;
  FMode := rmCapture;
  FFrameInterval := 200;
  FJpegQuality := 70;
end;

destructor TRemoteCamVCL.Destroy;
begin
  Stop;
  DestroyCaptureWindow;
  inherited;
end;

procedure TRemoteCamVCL.Call_DataFromUser(Sender: TObject; const uname: string;
  Data: TRtcFunctionInfo);
var
  cmd: String;
begin
  if Data.FunctionName = 'remote-cam' then
  begin
    cmd := UpperCase(Data.asText['op']);

    if cmd = 'FRAME' then
    begin
      if Mode = rmViewer then
      begin
        if Data.isType['img'] = rtc_ByteStream then
          TRtcCamFrameThread.Create(Self, uname, Data.asByteStream['img'], Sender);
      end;
    end
    else
      HandleCommand(cmd, uname, Sender);
  end;
end;

procedure TRemoteCamVCL.Call_LogOut(Sender: TObject);
begin
  inherited;
  Stop;
  FUserName := '';
end;

procedure TRemoteCamVCL.Connect(const UserName: String; Sender: TObject);
begin
  SetUserName(UserName);
  setSubscriber(UserName, True);
end;

procedure TRemoteCamVCL.Listen(Sender: TObject);
begin
  if Mode = rmCapture then
    setHost(True);
end;

procedure TRemoteCamVCL.Start(Sender: TObject);
begin
  if Mode = rmViewer then
    SendCommand('START', Sender)
  else
    HandleCommand('START', FUserName, Sender);
end;

procedure TRemoteCamVCL.Stop(Sender: TObject);
begin
  if Mode = rmViewer then
    SendCommand('STOP', Sender)
  else if Mode = rmCapture then
    HandleCommand('STOP', FUserName, Sender);

  if assigned(FCaptureThread) then
  begin
    FCaptureThread.Terminate;
    FCaptureThread.WaitFor;
    FreeAndNil(FCaptureThread);
  end;

  if Mode = rmCapture then
    DestroyCaptureWindow;
end;

procedure TRemoteCamVCL.DeliverFrame(Sender: TObject; const UserName: String;
  Frame: TBitmap);
var
  v: TRtcValue;
  tgtImg: TImage;
  tgtWin: TWinControl;
  r: TRect;
begin
  if assigned(FOnFrameReceived) then
  begin
    v := TRtcValue.Create;
    try
      with v.NewRecord do
      begin
        asText['user'] := UserName;
        asObject['bmp'] := Frame;
      end;
      CallEvent(Sender, xOnFrame, v);
    finally
      v.Free;
    end;
  end;

  if not assigned(FTargetDisplay) then
    Exit;

  if FTargetDisplay is TImage then
  begin
    tgtImg := TImage(FTargetDisplay);
    if not assigned(tgtImg.Picture) then
      tgtImg.Picture := TPicture.Create;
    tgtImg.Picture.Bitmap.Assign(Frame);
    Exit;
  end;

  if FTargetDisplay is TWinControl then
  begin
    tgtWin := TWinControl(FTargetDisplay);
    r := tgtWin.ClientRect;
    tgtWin.Canvas.StretchDraw(r, Frame);
  end;
end;

procedure TRemoteCamVCL.DestroyCaptureWindow;
begin
  if FCaptureHandle <> 0 then
  begin
    SendMessage(FCaptureHandle, WM_CAP_DRIVER_DISCONNECT, 0, 0);
    DestroyWindow(FCaptureHandle);
    FCaptureHandle := 0;
  end;
end;

procedure TRemoteCamVCL.EnsureCaptureWindow;
begin
  if FCaptureHandle = 0 then
  begin
    FCaptureHandle := CreateHiddenCaptureWindow;
    if FCaptureHandle = 0 then
      raise Exception.Create('Unable to create capture window');

    if SendMessage(FCaptureHandle, WM_CAP_DRIVER_CONNECT, 0, 0) = 0 then
      raise Exception.Create('Unable to connect capture driver');

    SendMessage(FCaptureHandle, WM_CAP_SET_PREVIEW, 1, 0);
  end;
end;

function TRemoteCamVCL.CaptureFrame: TBitmap;
var
  dcSrc, dcMem: HDC;
  r: TRect;
  bmpHandle, oldObj: HGDIOBJ;
  width, height: Integer;
begin
  EnsureCaptureWindow;

  GetClientRect(FCaptureHandle, r);
  width := r.Right - r.Left;
  height := r.Bottom - r.Top;
  if width <= 0 then width := 320;
  if height <= 0 then height := 240;

  Result := TBitmap.Create;
  Result.PixelFormat := pf24bit;
  Result.Width := width;
  Result.Height := height;

  dcSrc := GetDC(FCaptureHandle);
  dcMem := CreateCompatibleDC(dcSrc);
  bmpHandle := CreateCompatibleBitmap(dcSrc, width, height);
  oldObj := SelectObject(dcMem, bmpHandle);
  BitBlt(dcMem, 0, 0, width, height, dcSrc, 0, 0, SRCCOPY);
  SelectObject(dcMem, oldObj);
  DeleteDC(dcMem);
  ReleaseDC(FCaptureHandle, dcSrc);

  Result.Handle := bmpHandle;
end;

procedure TRemoteCamVCL.SendFrameFrame(const Bitmap: TBitmap; Sender: TObject);
var
  fn: TRtcFunctionInfo;
  ms: TMemoryStream;
  jpg: TJPEGImage;
begin
  if not assigned(Client) then Exit;
  if FUserName = '' then Exit;

  fn := TRtcFunctionInfo.Create;
  try
    fn.FunctionName := 'remote-cam';
    fn.asText['op'] := 'FRAME';

    ms := TMemoryStream.Create;
    jpg := TJPEGImage.Create;
    try
      jpg.CompressionQuality := FJpegQuality;
      jpg.Assign(Bitmap);
      jpg.SaveToStream(ms);
      ms.Position := 0;
      fn.asByteStream['img'] := ms;
    finally
      jpg.Free;
      ms.Free;
    end;

    Client.SendToUser(Sender, FUserName, fn);
  except
    fn.Free;
    raise;
  end;
end;

procedure TRemoteCamVCL.SendCommand(const Cmd: String; Sender: TObject);
var
  fn: TRtcFunctionInfo;
begin
  if not assigned(Client) then Exit;
  if FUserName = '' then Exit;

  fn := TRtcFunctionInfo.Create;
  try
    fn.FunctionName := 'remote-cam';
    fn.asText['op'] := Cmd;
    Client.SendToUser(Sender, FUserName, fn);
  except
    fn.Free;
    raise;
  end;
end;

procedure TRemoteCamVCL.HandleCommand(const Cmd, UserName: String;
  Sender: TObject);
begin
  if Cmd = 'START' then
  begin
    if Mode = rmCapture then
    begin
      FUserName := UserName;
      if assigned(FCaptureThread) then
        Exit;
      FCaptureThread := TRtcCamSendThread.Create(Self, Sender);
    end;
  end
  else if Cmd = 'STOP' then
  begin
    if assigned(FCaptureThread) then
    begin
      FCaptureThread.Terminate;
      FCaptureThread.WaitFor;
      FreeAndNil(FCaptureThread);
    end;
  end;
end;

procedure TRemoteCamVCL.NotifyError(Sender: TObject; const UserName,
  ErrorText: String);
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

procedure TRemoteCamVCL.SetFrameInterval(const Value: Cardinal);
begin
  FFrameInterval := Value;
end;

procedure TRemoteCamVCL.SetJpegQuality(const Value: Integer);
begin
  if Value < 1 then
    FJpegQuality := 1
  else if Value > 100 then
    FJpegQuality := 100
  else
    FJpegQuality := Value;
end;

procedure TRemoteCamVCL.SetMode(const Value: TRtcRemoteCamMode);
begin
  FMode := Value;
end;

procedure TRemoteCamVCL.SetTargetDisplay(const Value: TControl);
begin
  FTargetDisplay := Value;
end;

procedure TRemoteCamVCL.SetUserName(const Value: String);
begin
  FUserName := Value;
end;

procedure TRemoteCamVCL.xOnError(Sender, Obj: TObject; Data: TRtcValue);
begin
  if assigned(FOnError) then
    FOnError(Obj, Data.asText['user'], Data.asText['err']);
end;

procedure TRemoteCamVCL.xOnFrame(Sender, Obj: TObject; Data: TRtcValue);
var
  bmp: TBitmap;
begin
  if assigned(FOnFrameReceived) then
  begin
    bmp := TBitmap(Data.asRecord.asObject['bmp']);
    try
      FOnFrameReceived(Obj, Data.asRecord.asText['user'], bmp);
    finally
      // Caller owns Frame instance life-cycle in DeliverFrame; no Free here
    end;
  end;
end;

end.
