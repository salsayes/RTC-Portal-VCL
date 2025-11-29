unit ControlMainForm;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  System.Threading,
  System.Generics.Collections,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  VirtualTrees,
  PortalTypes,
  ThreadSafeLists;

type
  PHostNode = ^TClientIdentity;

  TControlForm = class(TForm)
    pnlTop: TPanel;
    lblAddress: TLabel;
    edtAddress: TEdit;
    btnConnect: TButton;
    ClientTree: TVirtualStringTree;
    Pages: TPageControl;
    tabDesktop: TTabSheet;
    tabShell: TTabSheet;
    tabCamera: TTabSheet;
    tabFiles: TTabSheet;
    tabUpdate: TTabSheet;
    MemoShell: TMemo;
    edtShellCommand: TEdit;
    btnSendShell: TButton;
    imgCamera: TImage;
    MemoLog: TMemo;
    btnSendUpdate: TButton;
    btnSendFile: TButton;
    dlgOpen: TFileOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure ClientTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure btnSendShellClick(Sender: TObject);
    procedure btnSendUpdateClick(Sender: TObject);
    procedure btnSendFileClick(Sender: TObject);
  private
    FHosts: TThreadSafeClientList;
    FSelectedHost: string;
    procedure AddLog(const Msg: string);
    procedure PopulateHosts(const Hosts: TArray<TClientIdentity>);
    procedure SimulateCameraFrame;
    procedure SendPacket(const Packet: TRoutedPacket);
  public
  end;

var
  ControlForm: TControlForm;

implementation

{$R *.dfm}

procedure TControlForm.AddLog(const Msg: string);
begin
  MemoLog.Lines.Add(FormatDateTime('hh:nn:ss.zzz', Now) + ' ' + Msg);
end;

procedure TControlForm.btnConnectClick(Sender: TObject);
var
  DummyHosts: TArray<TClientIdentity>;
  Host: TClientIdentity;
begin
  SetLength(DummyHosts, 1);
  Host.Id := 'HOST-001';
  Host.Name := 'Demo Host';
  Host.Ip := edtAddress.Text;
  Host.ClientType := 'Host';
  Host.LatencyMs := 25;
  Host.Status := 'Online';
  Host.Guid := '{DEMO-GUID}';
  Host.HWID := 'ABC123';
  DummyHosts[0] := Host;

  PopulateHosts(DummyHosts);
  AddLog('Conectado ao Gateway e lista de hosts carregada');
end;

procedure TControlForm.btnSendFileClick(Sender: TObject);
var
  FileName: string;
  FileStream: TFileStream;
begin
  if not dlgOpen.Execute then
    Exit;

  FileName := dlgOpen.FileName;
  FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    TTask.Run(
      procedure
      begin
        SendPacket(TRoutedPacket.Create(FSelectedHost, TGatewayChannel.gcFile, FileStream));
        TThread.Synchronize(nil,
          procedure
          begin
            AddLog(Format('Upload iniciado: %s (%d bytes)', [ExtractFileName(FileName), FileStream.Size]));
          end);
      end);
  except
    FileStream.Free;
    raise;
  end;
end;

procedure TControlForm.btnSendShellClick(Sender: TObject);
var
  Cmd: string;
  Payload: TStringStream;
begin
  Cmd := edtShellCommand.Text;
  Payload := TStringStream.Create(Cmd, TEncoding.UTF8);
  try
    SendPacket(TRoutedPacket.Create(FSelectedHost, TGatewayChannel.gcShell, Payload));
    MemoShell.Lines.Add('> ' + Cmd);
  except
    Payload.Free;
    raise;
  end;
end;

procedure TControlForm.btnSendUpdateClick(Sender: TObject);
var
  Payload: TStringStream;
begin
  Payload := TStringStream.Create('FORCE_UPDATE', TEncoding.UTF8);
  try
    SendPacket(TRoutedPacket.Create(FSelectedHost, TGatewayChannel.gcControl, Payload));
    AddLog('Comando de atualização enviado');
  except
    Payload.Free;
    raise;
  end;
end;

procedure TControlForm.ClientTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: PHostNode;
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
  FSelectedHost := Data^.Id;
end;

procedure TControlForm.FormCreate(Sender: TObject);
begin
  Caption := 'Control Demo';
  Width := 1100;
  Height := 700;

  FHosts := TThreadSafeClientList.Create;

  ClientTree.NodeDataSize := SizeOf(TClientIdentity);
  ClientTree.Header.Options := ClientTree.Header.Options + [hoVisible];
  ClientTree.Header.Columns.Add.Text := 'Nome de Usuário';
  ClientTree.Header.Columns.Add.Text := 'ID do Cliente';
  ClientTree.Header.Columns.Add.Text := 'IP';
  ClientTree.Header.Columns.Add.Text := 'Tipo (Host/Admin)';
  ClientTree.Header.Columns.Add.Text := 'Latência';
  ClientTree.Header.Columns.Add.Text := 'Status';
  ClientTree.Header.Columns.Add.Text := 'GUID';
  ClientTree.Header.Columns.Add.Text := 'HWID';

  Pages.ActivePage := tabDesktop;
end;

procedure TControlForm.PopulateHosts(const Hosts: TArray<TClientIdentity>);
var
  Node: PVirtualNode;
  Host: TClientIdentity;
  Data: PHostNode;
begin
  ClientTree.Clear;
  ClientTree.RootNodeCount := Length(Hosts);
  Node := ClientTree.GetFirst;
  for Host in Hosts do
  begin
    Data := ClientTree.GetNodeData(Node);
    Data^ := Host;
    Node := ClientTree.GetNext(Node);
    FHosts.AddClient(Host);
  end;
  ClientTree.Refresh;
end;

procedure TControlForm.SendPacket(const Packet: TRoutedPacket);
var
  Guard: TRoutingGuard;
begin
  Guard := TRoutingGuard.Create;
  Guard.EnsureValidChannel(Packet);
  TTask.Run(
    procedure
    begin
      Sleep(10); // Simula latência mínima
      if Assigned(Packet.Payload) then
        Packet.Payload.Free;
    end);
end;

procedure TControlForm.SimulateCameraFrame;
var
  Frame: TMemoryStream;
begin
  Frame := TMemoryStream.Create;
  try
    Frame.SetSize(256 * 1024);
    SendPacket(TRoutedPacket.Create(FSelectedHost, TGatewayChannel.gcCamera, Frame));
    AddLog('Frame de câmera enviado');
  except
    Frame.Free;
    raise;
  end;
end;

end.
