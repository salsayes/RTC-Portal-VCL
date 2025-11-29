unit rtcksAutoUpdater;

{*
  Auto-updater simples utilizado pelo RTC Portal para verificar e aplicar novas versões.
  O código foi renomeado a partir de ksAutoUpdater e contém comentários em português para
  facilitar a manutenção futura.
*}

interface

type
  // Evento disparado quando uma nova atualização é detectada.
  TrtcksUpdateAvailableEvent = reference to procedure(Sender: TObject; AETag, AVersion: string);

  // Interface pública do auto-updater.
  IrtcksAutoUpdater = interface
    function GetETag: string;
    function GetUpdateUrl: string;
    procedure SetUpdateUrl(const Value: string);
    function CheckForUpdate: Boolean;
    function UpdateAvailable: Boolean;
    procedure DoUpdate(const AReplaceRunningExe: Boolean = False; const AParams: string = '');
    property UpdateURL: string read GetUpdateUrl write SetUpdateUrl;
    property ETag: string read GetETag;
  end;

  function CreateRtcksAutoUpdater(AUrl,
                                  ACurrentETag: string;
                                  AIntervalSeconds: integer;
                                  AUpdateAvailableEvent: TrtcksUpdateAvailableEvent;
                                  const ACheckBuild: Boolean = True): IrtcksAutoUpdater;

implementation

uses
  Windows, Messages, Forms, System.Net.HttpClient, ExtCtrls, Classes,
  SysUtils, System.IOUtils, ShellAPi, IniFiles, DateUtils;

type
  // Implementação interna do auto-updater.
  TrtcksAutoUpdater = class(TInterfacedObject, IrtcksAutoUpdater)
  private
    FTimer: TTimer;
    FUpdateUrl: string;
    FIntervalSeconds: integer;
    FNewFile: string;
    FUpdateAvailable: Boolean;
    FETag: string;
    FCheckBuild: Boolean;
    FUpdateAvailableEvent: TrtcksUpdateAvailableEvent;
    function GetETag: string;
    procedure CreateTimer;
    function GetUpdateUrl: string;
    procedure SetUpdateUrl(const Value: string);
    procedure OnTimer(Sender: TObject);
  public
    {*
      Construtor recebe URL, ETag atual, intervalo de checagem e callback para notificar updates.
      ACheckBuild permite ignorar a comparação de build quando falso.
    *}
    constructor Create(AUpdateUrl: string;
                       ACheckIntervalSeconds: integer;
                       AETag: string;
                       ACheckBuild: Boolean;
                       AOnUpdateAvailable: TrtcksUpdateAvailableEvent); virtual;
    destructor Destroy; override;
    // Retorna se uma atualização já foi baixada e está pronta.
    function UpdateAvailable: Boolean;
    // Realiza o download e validação, retornando True quando há update.
    function CheckForUpdate: Boolean;
    // Executa o instalador baixado ou substitui o executável atual.
    procedure DoUpdate(const AReplaceRunningExe: Boolean = False; const AParams: string = '');
    property UpdateUrl: string read GetUpdateUrl write SetUpdateUrl;
    property ETag: string read GetETag;
  end;

  {*
    Cria uma instância do auto-updater.
    AIntervalSeconds define o intervalo de checagem, ACheckBuild determina se o número de build
    será comparado antes de considerar o update disponível.
  *}
  function CreateRtcksAutoUpdater(AUrl,
                                  ACurrentETag: string,
                                  AIntervalSeconds: integer,
                                  AUpdateAvailableEvent: TrtcksUpdateAvailableEvent,
                                  const ACheckBuild: Boolean = True): IrtcksAutoUpdater;
begin
  Result := TrtcksAutoUpdater.Create(AUrl, AIntervalSeconds, ACurrentETag, ACheckBuild, AUpdateAvailableEvent);
end;

procedure GetApplicationVersion(var AMajor, AMinor, ARelease, ABuild: integer; const AExe: string = '');
var
  VerInfoSize, VerValueSize, Dummy: DWORD;
  VerInfo: Pointer;
  VerValue: PVSFixedFileInfo;
  AFilename: string;
begin
  try
    AMajor := 0;
    AMinor := 0;
    ARelease := 0;
    ABuild := 0;
    AFilename := AExe;

    if AFilename = '' then
      AFilename := ParamStr(0);
    if FileExists(AFilename) = False then
      Exit;
    VerInfoSize := GetFileVersionInfoSize(PChar(AFilename), Dummy);
    GetMem(VerInfo, VerInfoSize);
    GetFileVersionInfo(PChar(AFilename), 0, VerInfoSize, VerInfo);
    VerQueryValue(VerInfo, '\\', Pointer(VerValue), VerValueSize);
    with VerValue^ do
    begin
      AMajor := dwFileVersionMS shr 16;
      AMinor := dwFileVersionMS and $FFFF;
      ARelease := dwFileVersionLS shr 16;
      ABuild := dwFileVersionLS and $FFFF;
    end;
    FreeMem(VerInfo, VerInfoSize);
  except
    // corrupt *.exe?
  end;
end;

function GetApplicationBuild(const AExe: string = ''): integer;
var
  v1, v2, v3, v4: integer;
begin
  GetApplicationVersion(v1, v2, v3, v4, AExe);
  Result := v4;
end;

function GetBuildFromVersionStr(AVersionStr: string): integer;
var
  AStrings: TStrings;
begin
  Result := 0;
  AStrings := TStringList.Create;
  try
    AStrings.Text := Trim(StringReplace(AVersionStr, '.', #13, [rfReplaceAll]));
    if AStrings.Count = 0 then
      Exit;
    Result := StrToIntDef(AStrings[AStrings.Count - 1], 0);
  finally
    AStrings.Free;
  end;
end;

{ TrtcksAutoUpdater }

constructor TrtcksAutoUpdater.Create(//AAppID: string;
  AUpdateUrl: string;
  ACheckIntervalSeconds: integer;
  AETag: string;
  ACheckBuild: Boolean;
  AOnUpdateAvailable: TrtcksUpdateAvailableEvent);
begin
  FETag := AETag;
  FUpdateUrl := AUpdateUrl;
  FUpdateAvailable := False;
  FIntervalSeconds := ACheckIntervalSeconds;
  FCheckBuild := ACheckBuild;
  FUpdateAvailableEvent := AOnUpdateAvailable;
  CreateTimer;
end;

destructor TrtcksAutoUpdater.Destroy;
begin
  FTimer.Free;
  inherited;
end;

procedure TrtcksAutoUpdater.DoUpdate(const AReplaceRunningExe: Boolean = False; const AParams: string = '');
begin
  // Quando AReplaceRunningExe é True, substituímos o executável atual pelo baixado.
  if AReplaceRunningExe then
  begin
    if FNewFile = '' then
    begin
      FUpdateAvailable := False;
      Exit;
    end;

    DeleteFile(ChangeFileExt(ParamStr(0), '.old'));
    RenameFile(ParamStr(0), ChangeFileExt(ParamStr(0), '.old'));
    if CopyFile(PWideChar(FNewFile), PWideChar(ParamStr(0)), False) then
    begin
      Sleep(1000);
      ShellExecute(0, nil, PChar(ParamStr(0)), PWideChar(AParams), nil, SW_SHOWNORMAL);
      DeleteFile(FNewFile);
      FUpdateAvailable := False;
      FNewFile := '';
      Sleep(1000);
      PostMessage(Application.MainForm.Handle, WM_QUIT, 0, 0);
    end;
  end
  else
  begin
    // Executa o instalador silenciosamente quando optamos por não substituir em runtime.
    ShellExecute(0, nil, PChar(FNewFile), PChar('/SILENT'), nil, SW_SHOWNORMAL);
  end;
end;

function TrtcksAutoUpdater.GetETag: string;
begin
  Result := FETag;
end;

function TrtcksAutoUpdater.GetUpdateUrl: string;
begin
  Result := FUpdateUrl;
end;

procedure TrtcksAutoUpdater.SetUpdateUrl(const Value: string);
begin
  FUpdateUrl := Value;
end;

function TrtcksAutoUpdater.CheckForUpdate: Boolean;
var
  AHttp: THTTPClient;
  AResponse: IHTTPResponse;
  AStream: TMemoryStream;
  ANewETag: string;
  ANewBuild: Integer;
begin
  Result := False;
  if Trim(FUpdateUrl) = '' then
    Exit;
  try
    AHttp := THTTPClient.Create;
    try
      AResponse := AHttp.Head(FUpdateUrl);
      ANewETag := AResponse.HeaderValue['ETag'];
      // Apenas continua se o servidor respondeu OK e o ETag mudou.
      if (AResponse.StatusCode = 200) and (ANewETag <> FETag) then
      begin
        AStream := TMemoryStream.Create;
        try
          AResponse := AHttp.Get(FUpdateUrl, AStream);
          if AResponse.StatusCode = 200 then
          begin
            FETag := AResponse.HeaderValue['ETag'];
            FNewFile := ChangeFileExt(TPath.GetTempFileName, '.exe');
            AStream.SaveToFile(FNewFile);
            ANewBuild := GetApplicationBuild(FNewFile);
            if (ANewBuild > GetApplicationBuild) or (FCheckBuild = False) then
            begin
              Result := True;
              TThread.Queue(nil,
                procedure
                begin
                  if Assigned(FUpdateAvailableEvent) then
                    FUpdateAvailableEvent(Self, FETag, ANewBuild.ToString);
                end
              );
            end;
          end;
        finally
          AStream.Free;
        end;
      end;
    finally
      AHttp.Free;
    end;
  except
    //
  end;
end;

procedure TrtcksAutoUpdater.OnTimer(Sender: TObject);
begin
  // Evita disparos simultâneos caso já haja update pendente.
  if FUpdateAvailable then
    Exit;

  FTimer.Enabled := False;

  TThread.CreateAnonymousThread(
    procedure
    begin
      FUpdateAvailable := CheckForUpdate;

      if FUpdateAvailable = False then
      begin
        TThread.Synchronize(nil,
          procedure
          begin
            FTimer.Interval := FIntervalSeconds * 1000;
            FTimer.Enabled := True;
          end
        );
      end;
    end
  ).Start;
end;

procedure TrtcksAutoUpdater.CreateTimer;
begin
  FTimer := TTimer.Create(nil);
  FTimer.Interval := 3000;
  FTimer.OnTimer := OnTimer;
  FTimer.Enabled := True;
end;

function TrtcksAutoUpdater.UpdateAvailable: Boolean;
begin
  Result := FUpdateAvailable;
end;

end.
