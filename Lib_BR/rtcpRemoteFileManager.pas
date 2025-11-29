{ Copyright 2004-2017 (c) RealThinClient.com (http://www.realthinclient.com) }

unit rtcpRemoteFileManager;

interface

{$INCLUDE rtcDefs.inc}

uses
  Windows, Classes, SysUtils, Math, SyncObjs,
  VirtualTrees, VirtualTrees.Types,
  rtcTypes, rtcInfo, rtcLog, rtcPortalMod;

type
  TRtcRemoteFileMode = (rfServer, rfClient);

  TNodeData = record
    Name: String;
    Size: Int64;
    Date: TDateTime;
    IsDirectory: Boolean;
  end;
  PNodeData = ^TNodeData;

  TRemoteFileProgressEvent = procedure(Sender: TObject; const FileName: String;
    Percent: Integer) of object;
  TRemoteFileErrorEvent = procedure(Sender: TObject; const UserName, ErrorText: String)
    of object;

  TRtcPRemoteFileManager = class;

  TTransferDirection = (tdUpload, tdDownload);

  TTransferSession = class
  public
    FileName: String;
    LocalPath: String;
    Stream: TFileStream;
    TotalSize: Int64;
    BytesProcessed: Int64;
    Direction: TTransferDirection;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TRtcPRemoteFileManager = class(TRtcPModule)
  private
    const
      CMD_LIST = 1;
      CMD_LIST_RESPONSE = 2;
      CMD_CREATE_FOLDER = 3;
      CMD_DELETE_FILE = 4;
      CMD_DELETE_FOLDER = 5;
      CMD_UPLOAD_REQUEST = 6;
      CMD_UPLOAD_DATA = 7;
      CMD_UPLOAD_COMPLETE = 8;
      CMD_DOWNLOAD_REQUEST = 9;
      CMD_DOWNLOAD_START = 10;
      CMD_DOWNLOAD_DATA = 11;
      CMD_DOWNLOAD_COMPLETE = 12;
      CMD_ERROR = 255;
    var
      FMode: TRtcRemoteFileMode;
      FUserName: String;
      FTargetTree: TVirtualStringTree;
      FOnFileProgress: TRemoteFileProgressEvent;
      FOnError: TRemoteFileErrorEvent;
      FListCache: TStringList;
      FSessionLock: TCriticalSection;
      FSessions: TStringList;

      FSyncUser: String;
      FSyncErrorText: String;
      FSyncFileName: String;
      FSyncPercent: Integer;

      FOldGetText: TVSTGetTextEvent;
      FOldGetImageIndex: TVTGetImageEvent;
      FOldFreeNode: TVTFreeNodeEvent;

      procedure SetMode(const Value: TRtcRemoteFileMode);
      procedure SetTargetTree(const Value: TVirtualStringTree);
      procedure SetUserName(const Value: String);

      procedure AttachTreeEvents;
      procedure DetachTreeEvents;
      procedure TreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
        Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
      procedure TreeGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
        Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
        var ImageIndex: TImageIndex);
      procedure TreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);

      procedure RunInMainThread(AMethod: TThreadMethod);
      procedure DoError;
      procedure DoProgress;
      procedure PopulateTree;
      procedure ClearListCache;

      procedure SendPacket(const UserName: String; Command: Byte;
        const Payload: RawByteString; Sender: TObject = nil);
      function EncodePacket(Command: Byte; const Payload: RawByteString): RtcByteArray;
      function ParsePacket(const Packet: RtcByteArray; out Command: Byte;
        out Payload: RawByteString): Boolean;

      procedure HandleServerCommand(const UserName: String; Command: Byte;
        const Payload: RawByteString);
      procedure HandleClientCommand(const UserName: String; Command: Byte;
        const Payload: RawByteString);

      procedure DispatchError(const UserName, Msg: String);
      procedure DispatchProgress(const FileName: String; Percent: Integer);

      procedure StartListing(const UserName, Path: String);
      procedure PerformCreateFolder(const UserName, Path: String);
      procedure PerformDeleteFile(const UserName, Path: String);
      procedure PerformDeleteFolder(const UserName, Path: String);

      procedure BeginUpload(const UserName, RemotePath: String; Size: Int64);
      procedure ReceiveUploadData(const UserName: String; const Payload: RawByteString);
      procedure FinishUpload(const UserName, RemotePath: String);

      procedure BeginDownload(const UserName, RemoteFile: String);
      procedure ReceiveDownloadStart(const UserName: String;
        const Payload: RawByteString);
      procedure ReceiveDownloadData(const UserName: String;
        const Payload: RawByteString);
      procedure ReceiveDownloadComplete(const UserName: String;
        const Payload: RawByteString);

      function GetSessionKey(const UserName, FileName: String): String;
      function FindSession(const Key: String): TTransferSession;
      procedure AddSession(const Key: String; Session: TTransferSession);
      procedure RemoveSession(const Key: String);
      procedure ClearSessions;
    protected
      procedure Call_DataFromUser(Sender: TObject; const uname: string;
        Data: TRtcFunctionInfo); override;
      procedure Call_LogOut(Sender: TObject); override;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;

      procedure Connect(const UserName: String; Sender: TObject = nil);
      procedure Listen(Sender: TObject = nil);

      procedure ListPath(const Path: String; Sender: TObject = nil);
      procedure CreateFolder(const Path: String; Sender: TObject = nil);
      procedure DeleteFile(const Path: String; Sender: TObject = nil);
      procedure DeleteFolder(const Path: String; Sender: TObject = nil);

      procedure Upload(const LocalFile, RemotePath: String; Sender: TObject = nil);
      procedure Download(const RemoteFile, LocalPath: String; Sender: TObject = nil);
    published
      property Mode: TRtcRemoteFileMode read FMode write SetMode default rfClient;
      property UserName: String read FUserName write SetUserName;
      property TargetTree: TVirtualStringTree read FTargetTree write SetTargetTree;
      property OnFileProgress: TRemoteFileProgressEvent read FOnFileProgress
        write FOnFileProgress;
      property OnError: TRemoteFileErrorEvent read FOnError write FOnError;
  end;

implementation

{ TTransferSession }

constructor TTransferSession.Create;
begin
  inherited Create;
  Stream := nil;
  TotalSize := 0;
  BytesProcessed := 0;
  Direction := tdUpload;
end;

destructor TTransferSession.Destroy;
begin
  if Assigned(Stream) then
    Stream.Free;
  inherited;
end;

{ TRtcPRemoteFileManager }

constructor TRtcPRemoteFileManager.Create(AOwner: TComponent);
begin
  inherited;
  FMode := rfClient;
  FUserName := '';
  FListCache := TStringList.Create;
  FSessionLock := TCriticalSection.Create;
  FSessions := TStringList.Create;
  FSessions.CaseSensitive := True;
  FSessions.Sorted := False;
  FSessions.Duplicates := dupIgnore;
end;

destructor TRtcPRemoteFileManager.Destroy;
begin
  DetachTreeEvents;
  ClearListCache;
  ClearSessions;
  FSessions.Free;
  FSessionLock.Free;
  FListCache.Free;
  inherited;
end;

procedure TRtcPRemoteFileManager.AttachTreeEvents;
begin
  if not Assigned(FTargetTree) then
    Exit;

  FOldGetText := FTargetTree.OnGetText;
  FOldGetImageIndex := FTargetTree.OnGetImageIndex;
  FOldFreeNode := FTargetTree.OnFreeNode;

  FTargetTree.NodeDataSize := SizeOf(TNodeData);
  FTargetTree.OnGetText := TreeGetText;
  FTargetTree.OnGetImageIndex := TreeGetImageIndex;
  FTargetTree.OnFreeNode := TreeFreeNode;
end;

procedure TRtcPRemoteFileManager.DetachTreeEvents;
begin
  if not Assigned(FTargetTree) then
    Exit;

  FTargetTree.OnGetText := FOldGetText;
  FTargetTree.OnGetImageIndex := FOldGetImageIndex;
  FTargetTree.OnFreeNode := FOldFreeNode;
end;

  procedure TRtcPRemoteFileManager.TreeFreeNode(Sender: TBaseVirtualTree;
    Node: PVirtualNode);
  var
    Data: PNodeData;
  begin
    if Assigned(FOldFreeNode) then
      FOldFreeNode(Sender, Node);
    Data := PNodeData(Sender.GetNodeData(Node));
    if Assigned(Data) then
      Finalize(Data^);
  end;

  procedure TRtcPRemoteFileManager.TreeGetImageIndex(Sender: TBaseVirtualTree;
    Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
    var Ghosted: Boolean; var ImageIndex: TImageIndex);
  var
    Data: PNodeData;
  begin
    if Assigned(FOldGetImageIndex) then
      FOldGetImageIndex(Sender, Node, Kind, Column, Ghosted, ImageIndex);

    if (Kind in [ikNormal, ikSelected]) then
    begin
      Data := PNodeData(Sender.GetNodeData(Node));
      if Assigned(Data) then
      begin
        if Data^.IsDirectory then
          ImageIndex := 0
      else
        ImageIndex := 1;
    end;
  end;
end;

function FormatSizeToText(const Size: Int64): String;
const
  KB = 1024;
  MB = KB * 1024;
  GB = MB * 1024;
begin
  if Size >= GB then
    Result := FormatFloat('0.00 GB', Size / GB)
  else if Size >= MB then
    Result := FormatFloat('0.00 MB', Size / MB)
  else if Size >= KB then
    Result := FormatFloat('0.00 KB', Size / KB)
  else
    Result := IntToStr(Size) + ' B';
end;

  procedure TRtcPRemoteFileManager.TreeGetText(Sender: TBaseVirtualTree;
    Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
    var CellText: String);
  var
    Data: PNodeData;
  begin
    if Assigned(FOldGetText) then
      FOldGetText(Sender, Node, Column, TextType, CellText);

    Data := PNodeData(Sender.GetNodeData(Node));
    if not Assigned(Data) then
      Exit;

  case Column of
    0: CellText := Data^.Name;
    1: CellText := FormatSizeToText(Data^.Size);
    2: CellText := DateTimeToStr(Data^.Date);
  end;
end;

procedure TRtcPRemoteFileManager.SetMode(const Value: TRtcRemoteFileMode);
begin
  if FMode <> Value then
    FMode := Value;
end;

procedure TRtcPRemoteFileManager.SetTargetTree(const Value: TVirtualStringTree);
begin
  if FTargetTree = Value then
    Exit;
  DetachTreeEvents;
  FTargetTree := Value;
  AttachTreeEvents;
end;

procedure TRtcPRemoteFileManager.SetUserName(const Value: String);
begin
  FUserName := Value;
end;

procedure TRtcPRemoteFileManager.Connect(const UserName: String;
  Sender: TObject);
begin
  SetUserName(UserName);
  setSubscriber(UserName, True);
end;

procedure TRtcPRemoteFileManager.Listen(Sender: TObject);
begin
  Mode := rfServer;
end;

procedure TRtcPRemoteFileManager.ClearListCache;
begin
  FListCache.Clear;
end;

procedure TRtcPRemoteFileManager.PopulateTree;
var
  I: Integer;
  Node: PVirtualNode;
  Data: PNodeData;
  Line, DirFlag, SizeStr, DateStr: String;
  Parts: TStringList;
begin
  if not Assigned(FTargetTree) then
    Exit;

  FTargetTree.BeginUpdate;
  try
    FTargetTree.Clear;
    Parts := TStringList.Create;
    try
      Parts.Delimiter := #9;
      Parts.StrictDelimiter := True;
      for I := 0 to FListCache.Count - 1 do
      begin
        Line := FListCache[I];
        Parts.DelimitedText := Line;
        if Parts.Count < 4 then
          Continue;
        Node := FTargetTree.AddChild(nil);
        Data := FTargetTree.GetNodeData(Node);
        if Assigned(Data) then
        begin
          Data^.Name := Parts[0];
          SizeStr := Parts[1];
          DateStr := Parts[2];
          DirFlag := Parts[3];
          Data^.Size := StrToInt64Def(SizeStr, 0);
          Data^.Date := StrToFloatDef(DateStr, 0);
          Data^.IsDirectory := DirFlag = '1';
        end;
      end;
    finally
      Parts.Free;
    end;
  finally
    FTargetTree.EndUpdate;
  end;
end;

procedure TRtcPRemoteFileManager.RunInMainThread(AMethod: TThreadMethod);
begin
  if GetCurrentThreadId = MainThreadID then
    AMethod
  else
    TThread.Synchronize(nil, AMethod);
end;

procedure TRtcPRemoteFileManager.DoError;
begin
  if Assigned(FOnError) then
    FOnError(Self, FSyncUser, FSyncErrorText);
end;

procedure TRtcPRemoteFileManager.DoProgress;
begin
  if Assigned(FOnFileProgress) then
    FOnFileProgress(Self, FSyncFileName, FSyncPercent);
end;

  function TRtcPRemoteFileManager.EncodePacket(Command: Byte;
    const Payload: RawByteString): RtcByteArray;
  var
    L: Cardinal;
  begin
    L := Length(Payload);
    SetLength(Result, 1 + SizeOf(Cardinal) + L);
  Result[0] := Command;
  PCardinal(@Result[1])^ := L;
  if L > 0 then
    Move(Payload[1], Result[1 + SizeOf(Cardinal)], L);
end;

  function TRtcPRemoteFileManager.ParsePacket(const Packet: RtcByteArray;
    out Command: Byte; out Payload: RawByteString): Boolean;
var
  L: Cardinal;
  PayloadLen: Integer;
begin
  Result := False;
  if Length(Packet) < 1 + SizeOf(Cardinal) then
    Exit;

  Command := Packet[0];
  L := PCardinal(@Packet[1])^;
  PayloadLen := Length(Packet) - 1 - SizeOf(Cardinal);
  if PayloadLen < Integer(L) then
    Exit;

  SetLength(Payload, L);
  if L > 0 then
    Move(Packet[1 + SizeOf(Cardinal)], Payload[1], L);
  Result := True;
end;

procedure TRtcPRemoteFileManager.SendPacket(const UserName: String; Command: Byte;
  const Payload: RawByteString; Sender: TObject);
var
  fn: TRtcFunctionInfo;
  begin
    if not Assigned(Client) then
      raise Exception.Create('Portal Client not assigned');
    if UserName = '' then
      raise Exception.Create('UserName not specified for SendPacket');

    fn := TRtcFunctionInfo.Create;
    try
      fn.FunctionName := 'remote-file-manager';
      fn.asByteArray['pkt'] := EncodePacket(Command, Payload);
      Client.SendToUser(Sender, UserName, fn);
    except
      fn.Free;
      raise;
    end;
  end;

procedure TRtcPRemoteFileManager.Call_DataFromUser(Sender: TObject;
  const uname: string; Data: TRtcFunctionInfo);
var
  Packet: RtcByteArray;
  Cmd: Byte;
  Payload: RawByteString;
begin
  if Data.FunctionName <> 'remote-file-manager' then
    Exit;

  Packet := Data.asByteArray['pkt'];
  if not ParsePacket(Packet, Cmd, Payload) then
    Exit;

  if Mode = rfServer then
    HandleServerCommand(uname, Cmd, Payload)
  else
    HandleClientCommand(uname, Cmd, Payload);
end;

procedure TRtcPRemoteFileManager.Call_LogOut(Sender: TObject);
begin
  inherited;
  ClearListCache;
  ClearSessions;
end;

procedure TRtcPRemoteFileManager.DispatchError(const UserName, Msg: String);
begin
  if Assigned(FOnError) then
  begin
    FSyncUser := UserName;
    FSyncErrorText := Msg;
    RunInMainThread(DoError);
  end;
  if (Mode = rfServer) and (UserName <> '') then
    SendPacket(UserName, CMD_ERROR, RawByteString(AnsiString(Msg)));
end;

procedure TRtcPRemoteFileManager.DispatchProgress(const FileName: String;
  Percent: Integer);
begin
  if Assigned(FOnFileProgress) then
  begin
    FSyncFileName := FileName;
    FSyncPercent := Percent;
    RunInMainThread(DoProgress);
  end;
end;

procedure TRtcPRemoteFileManager.StartListing(const UserName, Path: String);
var
  sr: TSearchRec;
  Lines: TStringList;
  DirFlag: String;
  Payload: RawByteString;
begin
  Lines := TStringList.Create;
  try
    if FindFirst(IncludeTrailingPathDelimiter(Path) + '*.*', faAnyFile, sr) = 0 then
    begin
      repeat
        if (sr.Name = '.') or (sr.Name = '..') then
          Continue;
        if (sr.Attr and faDirectory) <> 0 then
          DirFlag := '1'
        else
          DirFlag := '0';
        Lines.Add(sr.Name + #9 + IntToStr(sr.Size) + #9 +
          FloatToStr(FileDateToDateTime(sr.Time)) + #9 + DirFlag);
      until FindNext(sr) <> 0;
    end;
    FindClose(sr);
    Payload := RawByteString(AnsiString(Lines.Text));
    SendPacket(UserName, CMD_LIST_RESPONSE, Payload);
  finally
    Lines.Free;
  end;
end;

procedure TRtcPRemoteFileManager.PerformCreateFolder(const UserName, Path: String);
begin
  if not ForceDirectories(Path) then
    DispatchError(UserName, 'Unable to create folder: ' + Path);
end;

procedure TRtcPRemoteFileManager.PerformDeleteFile(const UserName, Path: String);
begin
  try
    if not SysUtils.DeleteFile(Path) then
      DispatchError(UserName, 'Unable to delete file: ' + Path);
  except
    on E: Exception do
      DispatchError(UserName, E.Message);
  end;
end;

procedure TRtcPRemoteFileManager.PerformDeleteFolder(const UserName, Path: String);
begin
  try
    if not RemoveDir(Path) then
      DispatchError(UserName, 'Unable to delete folder: ' + Path);
  except
    on E: Exception do
      DispatchError(UserName, E.Message);
  end;
end;

procedure TRtcPRemoteFileManager.BeginUpload(const UserName, RemotePath: String;
  Size: Int64);
var
  Key: String;
  Session: TTransferSession;
begin
  Session := TTransferSession.Create;
  Session.FileName := RemotePath;
  Session.Direction := tdUpload;
  Session.TotalSize := Size;
  try
    Session.Stream := TFileStream.Create(RemotePath, fmCreate);
  except
    Session.Free;
    DispatchError(UserName, 'Cannot open file for upload: ' + RemotePath);
    Exit;
  end;
  Key := GetSessionKey(UserName, RemotePath);
  AddSession(Key, Session);
end;

procedure TRtcPRemoteFileManager.ReceiveUploadData(const UserName: String;
  const Payload: RawByteString);
var
  Sep: Integer;
  RemotePath: String;
  Data: RawByteString;
  Key: String;
  Session: TTransferSession;
  Percent: Integer;
begin
  Sep := Pos(#0, String(Payload));
  if Sep = 0 then
    Exit;
  RemotePath := Copy(String(Payload), 1, Sep - 1);
  Data := Copy(Payload, Sep + 1, Length(Payload));
  Key := GetSessionKey(UserName, RemotePath);
  Session := FindSession(Key);
  if not Assigned(Session) then
    Exit;

  if (Length(Data) > 0) and Assigned(Session.Stream) then
    Session.Stream.WriteBuffer(Data[1], Length(Data));

  Inc(Session.BytesProcessed, Length(Data));
  if Session.TotalSize > 0 then
    Percent := Round((Session.BytesProcessed / Session.TotalSize) * 100)
  else
    Percent := 0;
  DispatchProgress(RemotePath, Percent);
end;

procedure TRtcPRemoteFileManager.FinishUpload(const UserName, RemotePath: String);
var
  Key: String;
begin
  Key := GetSessionKey(UserName, RemotePath);
  RemoveSession(Key);
  DispatchProgress(RemotePath, 100);
  SendPacket(UserName, CMD_UPLOAD_COMPLETE, RawByteString(AnsiString(RemotePath)));
end;

procedure TRtcPRemoteFileManager.BeginDownload(const UserName, RemoteFile: String);
var
  Chunk: RawByteString;
  Stream: TFileStream;
  Size: Int64;
  Buffer: array [0 .. 8191] of Byte;
  ReadCnt: Integer;
  Payload: RawByteString;
begin
  try
    Stream := TFileStream.Create(RemoteFile, fmOpenRead or fmShareDenyNone);
  except
    DispatchError(UserName, 'Cannot open file: ' + RemoteFile);
    Exit;
  end;
  try
    Size := Stream.Size;
    Payload := RawByteString(AnsiString(RemoteFile + #0 + IntToStr(Size)));
    SendPacket(UserName, CMD_DOWNLOAD_START, Payload);

    while True do
    begin
      ReadCnt := Stream.Read(Buffer, SizeOf(Buffer));
      if ReadCnt <= 0 then
        Break;
      SetString(Chunk, PAnsiChar(@Buffer[0]), ReadCnt);
      Payload := RawByteString(AnsiString(RemoteFile + #0)) + RawByteString(Chunk);
      SendPacket(UserName, CMD_DOWNLOAD_DATA, Payload);
      DispatchProgress(RemoteFile, Round((Stream.Position / Size) * 100));
    end;
    SendPacket(UserName, CMD_DOWNLOAD_COMPLETE, RawByteString(AnsiString(RemoteFile)));
  finally
    Stream.Free;
  end;

end;

procedure TRtcPRemoteFileManager.ReceiveDownloadStart(const UserName: String;
  const Payload: RawByteString);
var
  Sep: Integer;
  RemoteFile, SizeStr: String;
  Size: Int64;
  Key: String;
  Session: TTransferSession;
begin
  Sep := Pos(#0, String(Payload));
  if Sep = 0 then
    Exit;
  RemoteFile := Copy(String(Payload), 1, Sep - 1);
  SizeStr := Copy(String(Payload), Sep + 1, Length(Payload));
  Size := StrToInt64Def(SizeStr, 0);

  Key := GetSessionKey(UserName, RemoteFile);
  Session := FindSession(Key);
  if not Assigned(Session) then
  begin
    Session := TTransferSession.Create;
    Session.FileName := RemoteFile;
    Session.Direction := tdDownload;
    Session.LocalPath := RemoteFile;
    AddSession(Key, Session);
  end;
  Session.TotalSize := Size;
  try
    if not Assigned(Session.Stream) then
      Session.Stream := TFileStream.Create(Session.LocalPath, fmCreate);
  except
    DispatchError(UserName, 'Cannot create local file: ' + Session.LocalPath);
  end;
end;

procedure TRtcPRemoteFileManager.ReceiveDownloadData(const UserName: String;
  const Payload: RawByteString);
var
  Sep: Integer;
  RemoteFile: String;
  Data: RawByteString;
  Key: String;
  Session: TTransferSession;
  Percent: Integer;
begin
  Sep := Pos(#0, String(Payload));
  if Sep = 0 then
    Exit;
  RemoteFile := Copy(String(Payload), 1, Sep - 1);
  Data := Copy(Payload, Sep + 1, Length(Payload));
  Key := GetSessionKey(UserName, RemoteFile);
  Session := FindSession(Key);
  if not Assigned(Session) then
    Exit;

  if (Length(Data) > 0) and Assigned(Session.Stream) then
    Session.Stream.WriteBuffer(Data[1], Length(Data));

  Inc(Session.BytesProcessed, Length(Data));
  if Session.TotalSize > 0 then
    Percent := Round((Session.BytesProcessed / Session.TotalSize) * 100)
  else
    Percent := 0;
  DispatchProgress(RemoteFile, Percent);
end;

procedure TRtcPRemoteFileManager.ReceiveDownloadComplete(const UserName: String;
  const Payload: RawByteString);
var
  RemoteFile: String;
  Key: String;
begin
  RemoteFile := String(Payload);
  Key := GetSessionKey(UserName, RemoteFile);
  RemoveSession(Key);
  DispatchProgress(RemoteFile, 100);
end;

function TRtcPRemoteFileManager.GetSessionKey(const UserName, FileName: String): String;
begin
  Result := UserName + ':' + FileName;
end;

function TRtcPRemoteFileManager.FindSession(const Key: String): TTransferSession;
var
  idx: Integer;
begin
  Result := nil;
  FSessionLock.Acquire;
  try
    idx := FSessions.IndexOf(Key);
    if idx >= 0 then
      Result := TTransferSession(FSessions.Objects[idx]);
  finally
    FSessionLock.Release;
  end;
end;

procedure TRtcPRemoteFileManager.AddSession(const Key: String;
  Session: TTransferSession);
var
  idx: Integer;
begin
  FSessionLock.Acquire;
  try
    idx := FSessions.IndexOf(Key);
    if idx >= 0 then
    begin
      FSessions.Objects[idx].Free;
      FSessions.Delete(idx);
    end;
    FSessions.AddObject(Key, Session);
  finally
    FSessionLock.Release;
  end;
end;

procedure TRtcPRemoteFileManager.RemoveSession(const Key: String);
var
  idx: Integer;
  Obj: TObject;
begin
  FSessionLock.Acquire;
  try
    idx := FSessions.IndexOf(Key);
    if idx >= 0 then
    begin
      Obj := FSessions.Objects[idx];
      FSessions.Delete(idx);
    end
    else
      Obj := nil;
  finally
    FSessionLock.Release;
  end;
  if Assigned(Obj) then
    Obj.Free;
end;

procedure TRtcPRemoteFileManager.ClearSessions;
var
  i: Integer;
begin
  FSessionLock.Acquire;
  try
    for i := 0 to FSessions.Count - 1 do
      FSessions.Objects[i].Free;
    FSessions.Clear;
  finally
    FSessionLock.Release;
  end;
end;

procedure TRtcPRemoteFileManager.HandleServerCommand(const UserName: String;
  Command: Byte; const Payload: RawByteString);
var
  Path: String;
  Sep: Integer;
  SizeStr: String;
  Size: Int64;
begin
  case Command of
    CMD_LIST:
      StartListing(UserName, String(Payload));
    CMD_CREATE_FOLDER:
      PerformCreateFolder(UserName, String(Payload));
    CMD_DELETE_FILE:
      PerformDeleteFile(UserName, String(Payload));
    CMD_DELETE_FOLDER:
      PerformDeleteFolder(UserName, String(Payload));
    CMD_UPLOAD_REQUEST:
      begin
        Sep := Pos(#0, String(Payload));
        if Sep > 0 then
        begin
          Path := Copy(String(Payload), 1, Sep - 1);
          SizeStr := Copy(String(Payload), Sep + 1, Length(Payload));
          Size := StrToInt64Def(SizeStr, 0);
          BeginUpload(UserName, Path, Size);
        end;
      end;
    CMD_UPLOAD_DATA:
      ReceiveUploadData(UserName, Payload);
    CMD_UPLOAD_COMPLETE:
      FinishUpload(UserName, String(Payload));
    CMD_DOWNLOAD_REQUEST:
      begin
        Sep := Pos(#0, String(Payload));
        if Sep > 0 then
        begin
          Path := Copy(String(Payload), 1, Sep - 1);
          SizeStr := Copy(String(Payload), Sep + 1, Length(Payload));
          BeginDownload(UserName, Path);
        end;
      end;
  end;
end;

procedure TRtcPRemoteFileManager.HandleClientCommand(const UserName: String;
  Command: Byte; const Payload: RawByteString);
begin
  case Command of
    CMD_LIST_RESPONSE:
      begin
        ClearListCache;
        FListCache.Text := String(Payload);
        RunInMainThread(PopulateTree);
      end;
    CMD_UPLOAD_COMPLETE:
      DispatchProgress(String(Payload), 100);
    CMD_DOWNLOAD_START:
      ReceiveDownloadStart(UserName, Payload);
    CMD_DOWNLOAD_DATA:
      ReceiveDownloadData(UserName, Payload);
    CMD_DOWNLOAD_COMPLETE:
      ReceiveDownloadComplete(UserName, Payload);
    CMD_ERROR:
      DispatchError(UserName, String(Payload));
  end;
end;

procedure TRtcPRemoteFileManager.ListPath(const Path: String; Sender: TObject);
begin
  if Mode <> rfClient then
    raise Exception.Create('ListPath can only be used in Client mode');
  SendPacket(FUserName, CMD_LIST, RawByteString(AnsiString(Path)), Sender);
end;

procedure TRtcPRemoteFileManager.CreateFolder(const Path: String;
  Sender: TObject);
begin
  if Mode <> rfClient then
    raise Exception.Create('CreateFolder can only be used in Client mode');
  SendPacket(FUserName, CMD_CREATE_FOLDER, RawByteString(AnsiString(Path)), Sender);
end;

procedure TRtcPRemoteFileManager.DeleteFile(const Path: String; Sender: TObject);
begin
  if Mode <> rfClient then
    raise Exception.Create('DeleteFile can only be used in Client mode');
  SendPacket(FUserName, CMD_DELETE_FILE, RawByteString(AnsiString(Path)), Sender);
end;

procedure TRtcPRemoteFileManager.DeleteFolder(const Path: String;
  Sender: TObject);
begin
  if Mode <> rfClient then
    raise Exception.Create('DeleteFolder can only be used in Client mode');
  SendPacket(FUserName, CMD_DELETE_FOLDER, RawByteString(AnsiString(Path)), Sender);
end;

procedure TRtcPRemoteFileManager.Upload(const LocalFile, RemotePath: String;
  Sender: TObject);
var
  Stream: TFileStream;
  Size: Int64;
  Buffer: array [0 .. 8191] of Byte;
  ReadCnt: Integer;
  Chunk: RawByteString;
  Payload: RawByteString;
begin
  if Mode <> rfClient then
    raise Exception.Create('Upload can only be used in Client mode');

  try
    Stream := TFileStream.Create(LocalFile, fmOpenRead or fmShareDenyNone);
  except
    on E: Exception do
    begin
      DispatchError(FUserName, E.Message);
      Exit;
    end;
  end;
  try
    Size := Stream.Size;
    Payload := RawByteString(AnsiString(RemotePath + #0 + IntToStr(Size)));
    SendPacket(FUserName, CMD_UPLOAD_REQUEST, Payload, Sender);
    while True do
    begin
      ReadCnt := Stream.Read(Buffer, SizeOf(Buffer));
      if ReadCnt <= 0 then
        Break;
      SetString(Chunk, PAnsiChar(@Buffer[0]), ReadCnt);
      Payload := RawByteString(AnsiString(RemotePath + #0)) + RawByteString(Chunk);
      SendPacket(FUserName, CMD_UPLOAD_DATA, Payload, Sender);
      DispatchProgress(LocalFile, Round((Stream.Position / Size) * 100));
    end;
    SendPacket(FUserName, CMD_UPLOAD_COMPLETE, RawByteString(AnsiString(RemotePath)),
      Sender);
  finally
    Stream.Free;
  end;
end;

procedure TRtcPRemoteFileManager.Download(const RemoteFile, LocalPath: String;
  Sender: TObject);
var
  Key: String;
  Session: TTransferSession;
  Payload: RawByteString;
begin
  if Mode <> rfClient then
    raise Exception.Create('Download can only be used in Client mode');

  Session := TTransferSession.Create;
  Session.FileName := RemoteFile;
  Session.LocalPath := LocalPath;
  Session.Direction := tdDownload;
  Key := GetSessionKey(FUserName, RemoteFile);
  AddSession(Key, Session);

  Payload := RawByteString(AnsiString(RemoteFile + #0 + LocalPath));
  SendPacket(FUserName, CMD_DOWNLOAD_REQUEST, Payload, Sender);
end;

end.
