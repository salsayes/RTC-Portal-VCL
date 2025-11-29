unit PortalTypes;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections;

{$SCOPEDENUMS ON}

type
  TGatewayChannel = (
    gcControl,
    gcShell,
    gcCamera,
    gcFile,
    gcHeartbeat
  );

  TRoutedPacket = record
    ClientId: string;
    Channel: TGatewayChannel;
    PayloadSize: Integer;
    Payload: TStream;
    class function Create(const AClientId: string; AChannel: TGatewayChannel; AStream: TStream): TRoutedPacket; static;
  end;

  TClientIdentity = record
    Id: string;
    Name: string;
    Ip: string;
    ClientType: string;
    LatencyMs: Cardinal;
    Status: string;
    Guid: string;
    HWID: string;
  end;

  TRoutingGuard = record
  private
    FMaxSize: Integer;
  public
    constructor Create(AMaxSize: Integer = 5 * 1024 * 1024); // 5MB default safety
    procedure EnsureValidChannel(const Packet: TRoutedPacket);
  end;

  TGatewayTelemetry = class
  private
    FHeartbeatInterval: Cardinal;
    FOnMetrics: TProc<string>;
  public
    constructor Create(AIntervalMs: Cardinal = 15000);
    procedure PublishHeartbeat(const Client: TClientIdentity);
    property HeartbeatInterval: Cardinal read FHeartbeatInterval;
    property OnMetrics: TProc<string> read FOnMetrics write FOnMetrics;
  end;

implementation

{ TRoutedPacket }

class function TRoutedPacket.Create(const AClientId: string; AChannel: TGatewayChannel; AStream: TStream): TRoutedPacket;
begin
  Result.ClientId := AClientId;
  Result.Channel := AChannel;
  Result.Payload := AStream;
  if Assigned(AStream) then
    Result.PayloadSize := AStream.Size
  else
    Result.PayloadSize := 0;
end;

{ TRoutingGuard }

constructor TRoutingGuard.Create(AMaxSize: Integer);
begin
  FMaxSize := AMaxSize;
end;

procedure TRoutingGuard.EnsureValidChannel(const Packet: TRoutedPacket);
begin
  if Packet.PayloadSize > FMaxSize then
    raise Exception.CreateFmt('Payload acima do limite para %s (%d bytes)', [Packet.ClientId, Packet.PayloadSize]);

  case Packet.Channel of
    TGatewayChannel.gcControl,
    TGatewayChannel.gcShell,
    TGatewayChannel.gcCamera,
    TGatewayChannel.gcFile,
    TGatewayChannel.gcHeartbeat:
      Exit;
  else
    raise Exception.Create('Canal desconhecido recebido no Gateway');
  end;
end;

{ TGatewayTelemetry }

constructor TGatewayTelemetry.Create(AIntervalMs: Cardinal);
begin
  FHeartbeatInterval := AIntervalMs;
end;

procedure TGatewayTelemetry.PublishHeartbeat(const Client: TClientIdentity);
var
  LMsg: string;
begin
  LMsg := Format('Ping %s (%s) - Latência %d ms', [Client.Id, Client.Ip, Client.LatencyMs]);
  if Assigned(FOnMetrics) then
    FOnMetrics(LMsg);
end;

end.
