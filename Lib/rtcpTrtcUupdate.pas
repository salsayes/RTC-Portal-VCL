{ Bidirectional auto-update component using RTC Portal messaging }

unit rtcpTrtcUupdate;

interface

{$INCLUDE rtcPortalDefs.inc}
{$INCLUDE rtcDefs.inc}

uses
  Windows, Classes, SysUtils,
  rtcInfo, rtcTypes, rtcPortalMod,
  rtcpAutoUpdater;

type
  TTrtcUpdateMode = (rumControl, rumHost);

  TTrtcUpdateAnnouncement = record
    Version: String;
    Location: String;
    Silent: Boolean;
    Forced: Boolean;
    AutoRun: Boolean;
    FromUser: String;
  end;

  TTrtcUpdateEvent = procedure(Sender: TObject;
    const Info: TUpdateCheckResult) of object;
  TTrtcAnnouncementEvent = procedure(Sender: TObject;
    const Announcement: TTrtcUpdateAnnouncement) of object;
  TTrtcUpdateErrorEvent = procedure(Sender: TObject; const Msg: String) of object;

  TRtcUpdateWorker = class(TThread)
  private
    FUpdater: TAutoUpdater;
    FSilent: Boolean;
    FOnFinished: TNotifyEvent;
    FOnError: TTrtcUpdateErrorEvent;
    FOwner: TObject;
    FOnCheckFinished: TTrtcUpdateEvent;
    FResult: TUpdateCheckResult;
    FError: String;

    procedure DoFinished;
    procedure DoError;
    procedure DoCheckFinished;
  protected
    procedure Execute; override;
  public
    constructor Create(const Updater: TAutoUpdater; const Silent: Boolean;
      const OwnerObj: TObject; const OnFinished: TNotifyEvent;
      const OnError: TTrtcUpdateErrorEvent; const OnCheck: TTrtcUpdateEvent);
  end;

  { Component that can act as Control (provider) or Host (consumer) }
  TRtcUupdate = class(TRtcPModule)
  private
    FMode: TTrtcUpdateMode;
    FUpdateLocation: String;
    FUpdateVersion: String;
    FSilentUpdate: Boolean;
    FForcedUpdate: Boolean;
    FAutoRunAfterUpdate: Boolean;
    FControlUser: String;
    FAutoUpdater: TAutoUpdater;
    FLastAnnouncement: TTrtcUpdateAnnouncement;

    FOnAnnouncement: TTrtcAnnouncementEvent;
    FOnCheckFinished: TTrtcUpdateEvent;
    FOnUpdateFinished: TNotifyEvent;
    FOnUpdateError: TTrtcUpdateErrorEvent;

    procedure ApplyAnnouncement(const User: String; const Data: TRtcRecord);
    procedure HandleUpdateQuery(const User: String);
    procedure HandleUpdateInfo(const User: String; const Data: TRtcRecord);
    procedure PushAnnouncement(const User: String);

    function GetExecutableName: String;
    procedure SetExecutableName(const Value: String);
    function GetTemporaryFileName: String;
    procedure SetTemporaryFileName(const Value: String);

    function BuildAnnouncement(const User: String): TTrtcUpdateAnnouncement;
  protected
    procedure Call_DataFromUser(Sender: TObject; const uname: String;
      Data: TRtcFunctionInfo); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure RequestUpdate(const TargetUser: String = '');
    procedure PublishUpdate;
    procedure StartUpdateAsync(const SilentOverride: Boolean = False);

    property AutoUpdater: TAutoUpdater read FAutoUpdater;
    property LastAnnouncement: TTrtcUpdateAnnouncement read FLastAnnouncement;
  published
    property Mode: TTrtcUpdateMode read FMode write FMode default rumControl;
    property UpdateLocation: String read FUpdateLocation write FUpdateLocation;
    property UpdateVersion: String read FUpdateVersion write FUpdateVersion;
    property SilentUpdate: Boolean read FSilentUpdate write FSilentUpdate default False;
    property ForcedUpdate: Boolean read FForcedUpdate write FForcedUpdate default False;
    property AutoRunAfterUpdate: Boolean read FAutoRunAfterUpdate
      write FAutoRunAfterUpdate default True;
    property ControlUser: String read FControlUser write FControlUser;

    property ExecutableName: String read GetExecutableName write SetExecutableName;
    property TemporaryFileName: String read GetTemporaryFileName
      write SetTemporaryFileName;

    property OnAnnouncement: TTrtcAnnouncementEvent read FOnAnnouncement
      write FOnAnnouncement;
    property OnCheckFinished: TTrtcUpdateEvent read FOnCheckFinished
      write FOnCheckFinished;
    property OnUpdateFinished: TNotifyEvent read FOnUpdateFinished
      write FOnUpdateFinished;
    property OnUpdateError: TTrtcUpdateErrorEvent read FOnUpdateError
      write FOnUpdateError;
  end;

implementation

{ TRtcUpdateWorker }

  constructor TRtcUpdateWorker.Create(const Updater: TAutoUpdater;
    const Silent: Boolean; const OwnerObj: TObject; const OnFinished: TNotifyEvent;
    const OnError: TTrtcUpdateErrorEvent; const OnCheck: TTrtcUpdateEvent);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FUpdater := Updater;
  FSilent := Silent;
  FOnFinished := OnFinished;
  FOnError := OnError;
  FOwner := OwnerObj;
  FOnCheckFinished := OnCheck;
  Resume;
end;

procedure TRtcUpdateWorker.DoFinished;
begin
  if Assigned(FOnFinished) then
    FOnFinished(FOwner);
end;

procedure TRtcUpdateWorker.DoCheckFinished;
begin
  if Assigned(FOnCheckFinished) then
    FOnCheckFinished(FOwner, FResult);
end;

procedure TRtcUpdateWorker.DoError;
begin
  if Assigned(FOnError) then
    FOnError(FOwner, FError);
end;

procedure TRtcUpdateWorker.Execute;
begin
  try
    if (FUpdater.LastCheck.SourceFile = '') or
      (not FileExists(FUpdater.LastCheck.SourceFile)) then
      FUpdater.CheckUpdate;

    FResult := FUpdater.LastCheck;
    if Assigned(FOnCheckFinished) then
      Synchronize(DoCheckFinished);

    if FResult.HasUpdate then
      FUpdater.StartUpdate(FSilent);
  except
    on E: Exception do
    begin
      FError := E.Message;
      if Assigned(FOnError) then
        Synchronize(DoError);
    end;
  end;

  if Assigned(FOnFinished) then
    Synchronize(DoFinished);
end;

{ TRtcUupdate }

function TRtcUupdate.BuildAnnouncement(const User: String): TTrtcUpdateAnnouncement;
begin
  Result.Version := FUpdateVersion;
  Result.Location := FUpdateLocation;
  Result.Silent := FSilentUpdate;
  Result.Forced := FForcedUpdate;
  Result.AutoRun := FAutoRunAfterUpdate;
  Result.FromUser := User;
end;

procedure TRtcUupdate.ApplyAnnouncement(const User: String; const Data: TRtcRecord);
begin
  FLastAnnouncement.Version := Data.asText['version'];
  FLastAnnouncement.Location := Data.asText['location'];
  FLastAnnouncement.Silent := Data.asBoolean['silent'];
  FLastAnnouncement.Forced := Data.asBoolean['forced'];
  FLastAnnouncement.AutoRun := Data.asBoolean['autorun'];
  FLastAnnouncement.FromUser := User;

  FAutoUpdater.GatewayUpdatePath := FLastAnnouncement.Location;
  if FLastAnnouncement.Silent then
    FAutoUpdater.SilentForce := True
  else
    FAutoUpdater.SilentForce := False;
end;

procedure TRtcUupdate.Call_DataFromUser(Sender: TObject; const uname: String;
  Data: TRtcFunctionInfo);
begin
  if (Data = nil) then
    Exit;

  if (Data.FunctionName = 'trtc-update:query') and (FMode = rumControl) then
    HandleUpdateQuery(uname)
  else if (Data.FunctionName = 'trtc-update:info') and (FMode = rumHost) then
    if Data.isType['update', rtc_Record] then
      HandleUpdateInfo(uname, Data.asRecord['update']);
end;

constructor TRtcUupdate.Create(AOwner: TComponent);
begin
  inherited;
  FMode := rumControl;
  FAutoRunAfterUpdate := True;
  FAutoUpdater := TAutoUpdater.Create(Self);
end;

destructor TRtcUupdate.Destroy;
begin
  FreeAndNil(FAutoUpdater);
  inherited;
end;

function TRtcUupdate.GetExecutableName: String;
begin
  Result := FAutoUpdater.ExecutableName;
end;

function TRtcUupdate.GetTemporaryFileName: String;
begin
  Result := FAutoUpdater.TemporaryFileName;
end;

procedure TRtcUupdate.HandleUpdateInfo(const User: String; const Data: TRtcRecord);
begin
  ApplyAnnouncement(User, Data);

  if Assigned(FOnAnnouncement) then
    FOnAnnouncement(Self, FLastAnnouncement);

  if (FLastAnnouncement.Forced) or (FLastAnnouncement.Silent) then
    StartUpdateAsync(FLastAnnouncement.Silent);
end;

procedure TRtcUupdate.HandleUpdateQuery(const User: String);
begin
  FAutoUpdater.GatewayUpdatePath := FUpdateLocation;
  FAutoUpdater.CheckUpdate;
  if FUpdateVersion = '' then
    FUpdateVersion := FAutoUpdater.LastCheck.AvailableVersion.AsString;

  PushAnnouncement(User);
end;

procedure TRtcUupdate.PublishUpdate;
begin
  PushAnnouncement('');
end;

procedure TRtcUupdate.PushAnnouncement(const User: String);
var
  rec: TRtcFunctionInfo;
  info: TRtcRecord;
  announcement: TTrtcUpdateAnnouncement;
  target: String;
begin
  rec := TRtcFunctionInfo.Create;
  try
    rec.FunctionName := 'trtc-update:info';
    info := rec.NewRecord('update');
    info.asText['version'] := FUpdateVersion;
    info.asText['location'] := FUpdateLocation;
    info.asBoolean['silent'] := FSilentUpdate;
    info.asBoolean['forced'] := FForcedUpdate;
    info.asBoolean['autorun'] := FAutoRunAfterUpdate;

    if User <> '' then
      target := User
    else if FControlUser <> '' then
      target := FControlUser
    else
      target := '';

    if target <> '' then
      SendToUser(Self, target, rec);

    announcement := BuildAnnouncement(User);
    if Assigned(FOnAnnouncement) then
      FOnAnnouncement(Self, announcement);
  finally
    rec.Free;
  end;
end;

procedure TRtcUupdate.RequestUpdate(const TargetUser: String);
var
  rec: TRtcFunctionInfo;
  target: String;
begin
  if FMode <> rumHost then
    Exit;

  if TargetUser <> '' then
    target := TargetUser
  else
    target := FControlUser;

  if target = '' then
    Exit;

  rec := TRtcFunctionInfo.Create;
  try
    rec.FunctionName := 'trtc-update:query';
    rec.asText['current_version'] := FAutoUpdater.LastCheck.CurrentVersion.AsString;
    SendToUser(Self, target, rec);
  finally
    rec.Free;
  end;
end;

procedure TRtcUupdate.SetExecutableName(const Value: String);
begin
  FAutoUpdater.ExecutableName := Value;
end;

procedure TRtcUupdate.SetTemporaryFileName(const Value: String);
begin
  FAutoUpdater.TemporaryFileName := Value;
end;

procedure TRtcUupdate.StartUpdateAsync(const SilentOverride: Boolean);
var
  silentMode: Boolean;
begin
  if FMode <> rumHost then
    Exit;

  silentMode := SilentOverride or FSilentUpdate;
  TRtcUpdateWorker.Create(FAutoUpdater, silentMode, Self,
    FOnUpdateFinished, FOnUpdateError, FOnCheckFinished);
end;

end.

