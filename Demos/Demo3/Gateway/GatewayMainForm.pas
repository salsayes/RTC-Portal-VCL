unit GatewayMainForm;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  VirtualTrees,
  PortalTypes,
  ThreadSafeLists;

type
  PClientNode = ^TClientIdentity;

  TGatewayForm = class(TForm)
    MemoLog: TMemo;
    ClientTree: TVirtualStringTree;
    TimerHeartbeat: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure TimerHeartbeatTimer(Sender: TObject);
    procedure ClientTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
  private
    FClients: TThreadSafeClientList;
    FTelemetry: TGatewayTelemetry;
    procedure AddLog(const Msg: string);
    procedure RefreshTree;
    procedure RoutePacket(const Packet: TRoutedPacket);
    procedure OnClientConnected(const Client: TClientIdentity);
    procedure OnClientDisconnected(const ClientId: string);
  public
    { Public declarations }
  end;

var
  GatewayForm: TGatewayForm;

implementation

{$R *.dfm}

procedure TGatewayForm.AddLog(const Msg: string);
begin
  MemoLog.Lines.Add(FormatDateTime('hh:nn:ss.zzz', Now) + ' ' + Msg);
end;

procedure TGatewayForm.ClientTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: PClientNode;
begin
  Data := Sender.GetNodeData(Node);
  if not Assigned(Data) then
    Exit;

  case Column of
    0: CellText := Data^.Name;
    1: CellText := Data^.Id;
    2: CellText := Data^.Ip;
    3: CellText := Data^.ClientType;
    4: CellText := Data^.LatencyMs.ToString;
    5: CellText := Data^.Status;
    6: CellText := Data^.Guid;
    7: CellText := Data^.HWID;
  end;
end;

procedure TGatewayForm.FormCreate(Sender: TObject);
begin
  Caption := 'Gateway Demo';
  Width := 1000;
  Height := 600;

  MemoLog.Align := alBottom;
  MemoLog.Height := 150;

  ClientTree.Align := alClient;
  ClientTree.Header.Options := ClientTree.Header.Options + [hoVisible];
  ClientTree.TreeOptions.MiscOptions := ClientTree.TreeOptions.MiscOptions + [toFullRowDrag, toGridExtensions];
  ClientTree.NodeDataSize := SizeOf(TClientIdentity);
  ClientTree.Header.Columns.Add.Text := 'Nome de Usuário';
  ClientTree.Header.Columns.Add.Text := 'ID do Cliente';
  ClientTree.Header.Columns.Add.Text := 'IP';
  ClientTree.Header.Columns.Add.Text := 'Tipo (Host/Admin)';
  ClientTree.Header.Columns.Add.Text := 'Latência';
  ClientTree.Header.Columns.Add.Text := 'Status';
  ClientTree.Header.Columns.Add.Text := 'GUID';
  ClientTree.Header.Columns.Add.Text := 'HWID';

  FClients := TThreadSafeClientList.Create;
  FTelemetry := TGatewayTelemetry.Create;
  FTelemetry.OnMetrics := AddLog;

  TimerHeartbeat.Interval := FTelemetry.HeartbeatInterval;
  TimerHeartbeat.Enabled := True;

  AddLog('Gateway iniciado e aguardando conexões ...');
end;

procedure TGatewayForm.OnClientConnected(const Client: TClientIdentity);
begin
  FClients.AddClient(Client);
  AddLog(Format('Cliente %s conectado (%s)', [Client.Id, Client.Ip]));
  RefreshTree;
end;

procedure TGatewayForm.OnClientDisconnected(const ClientId: string);
begin
  FClients.RemoveClient(ClientId);
  AddLog(Format('Cliente %s desconectado', [ClientId]));
  RefreshTree;
end;

procedure TGatewayForm.RefreshTree;
var
  Snapshot: TArray<TClientIdentity>;
  Node: PVirtualNode;
  Data: PClientNode;
  Item: TClientIdentity;
begin
  ClientTree.Clear;
  Snapshot := FClients.Snapshot;
  ClientTree.RootNodeCount := Length(Snapshot);
  Node := ClientTree.GetFirst;
  for Item in Snapshot do
  begin
    Data := ClientTree.GetNodeData(Node);
    Data^ := Item;
    Node := ClientTree.GetNext(Node);
  end;
  ClientTree.ReinitChildren(nil, True);
  ClientTree.Refresh;
end;

procedure TGatewayForm.RoutePacket(const Packet: TRoutedPacket);
var
  Guard: TRoutingGuard;
begin
  Guard := TRoutingGuard.Create;
  Guard.EnsureValidChannel(Packet);
  try
    case Packet.Channel of
      TGatewayChannel.gcControl:
        AddLog(Format('Controle recebido de %s (%d bytes)', [Packet.ClientId, Packet.PayloadSize]));
      TGatewayChannel.gcShell:
        AddLog(Format('Shell: %s', [Packet.ClientId]));
      TGatewayChannel.gcCamera:
        AddLog(Format('Frame de câmera roteado (%d bytes)', [Packet.PayloadSize]));
      TGatewayChannel.gcFile:
        AddLog(Format('Pacote de arquivo roteado (%d bytes)', [Packet.PayloadSize]));
      TGatewayChannel.gcHeartbeat:
        AddLog('Heartbeat recebido');
    end;
  finally
    if Assigned(Packet.Payload) then
      Packet.Payload.Free;
  end;
end;

procedure TGatewayForm.TimerHeartbeatTimer(Sender: TObject);
var
  Clients: TArray<TClientIdentity>;
  Client: TClientIdentity;
begin
  Clients := FClients.Snapshot;
  for Client in Clients do
    FTelemetry.PublishHeartbeat(Client);
end;

end.
