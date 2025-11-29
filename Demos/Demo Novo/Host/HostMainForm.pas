unit HostMainForm;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.Threading,
  Vcl.Forms,
  Vcl.ExtCtrls,
  PortalTypes;

type
  THostForm = class(TForm)
    TimerHeartbeat: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure TimerHeartbeatTimer(Sender: TObject);
  private
    FIdentity: TClientIdentity;
    procedure SendHeartbeat;
    procedure OnCommandReceived(const Packet: TRoutedPacket);
    procedure StreamCameraFrame;
  public
  end;

var
  HostForm: THostForm;

implementation

{$R *.dfm}

procedure THostForm.FormCreate(Sender: TObject);
begin
  Visible := False;
  FIdentity.Id := 'HOST-001';
  FIdentity.Name := 'Demo Host';
  FIdentity.Ip := '127.0.0.1';
  FIdentity.ClientType := 'Host';
  FIdentity.Status := 'Online';
  FIdentity.Guid := '{DEMO-GUID}';
  FIdentity.HWID := 'ABC123';

  TimerHeartbeat.Interval := 15000;
  TimerHeartbeat.Enabled := True;
end;

procedure THostForm.OnCommandReceived(const Packet: TRoutedPacket);
var
  Guard: TRoutingGuard;
begin
  Guard := TRoutingGuard.Create;
  Guard.EnsureValidChannel(Packet);
  try
    case Packet.Channel of
      TGatewayChannel.gcControl:
        ; // Handle update or admin command
      TGatewayChannel.gcShell:
        ; // Execute shell command via background thread
      TGatewayChannel.gcCamera:
        ; // Incoming camera configuration
      TGatewayChannel.gcFile:
        ; // Incoming file transfer request
      TGatewayChannel.gcHeartbeat:
        ;
    end;
  finally
    if Assigned(Packet.Payload) then
      Packet.Payload.Free;
  end;
end;

procedure THostForm.SendHeartbeat;
var
  Payload: TStringStream;
begin
  Payload := TStringStream.Create('PING:' + FIdentity.Id, TEncoding.UTF8);
  try
    // enviaria para o Gateway
    Payload.Free;
  except
    Payload.Free;
    raise;
  end;
end;

procedure THostForm.StreamCameraFrame;
var
  Frame: TMemoryStream;
begin
  Frame := TMemoryStream.Create;
  try
    Frame.SetSize(256 * 1024);
    // enviar frame usando o canal gcCamera
    Frame.Free;
  except
    Frame.Free;
    raise;
  end;
end;

procedure THostForm.TimerHeartbeatTimer(Sender: TObject);
begin
  TTask.Run(
    procedure
    begin
      SendHeartbeat;
      StreamCameraFrame;
    end);
end;

end.
