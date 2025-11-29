{ Component for silent client auto-updates using Gateway-hosted binaries }

unit rtcpAutoUpdater;

interface

{$INCLUDE rtcPortalDefs.inc}
{$INCLUDE rtcDefs.inc}

uses
  Windows, SysUtils, Classes;

const
  DEFAULT_UPDATE_FOLDER = 'Update';

type
  TFileVersionInfo = record
    Major: Word;
    Minor: Word;
    Release: Word;
    Build: Word;
    class function Empty: TFileVersionInfo; static;
    function AsString: String;
    function Compare(const Other: TFileVersionInfo): Integer;
  end;

  TUpdateCheckResult = record
    CurrentVersion: TFileVersionInfo;
    AvailableVersion: TFileVersionInfo;
    SourceFile: String;
    TargetFile: String;
    TempFile: String;
    HasUpdate: Boolean;
  end;

  TUpdateDownloadEvent = procedure(Sender: TObject; const SourceFile,
    TargetFile: String; var Handled: Boolean) of object;
  TUpdateEvent = procedure(Sender: TObject; const Info: TUpdateCheckResult)
    of object;

  { TAutoUpdater implements a Control/Gateway/Host update flow. }
  TAutoUpdater = class(TComponent)
  private
    FGatewayUpdatePath: String;
    FExecutableName: String;
    FTemporaryFileName: String;
    FSilentForce: Boolean;
    FLastCheck: TUpdateCheckResult;
    FOnDownloadUpdate: TUpdateDownloadEvent;
    FOnBeforeExecute: TUpdateEvent;

    function GetExecutableName: String;
    function GetTemporaryFileName: String;
    function GetUpdateSourceFile: String;
    function GetTempFolder: String;
    function QueryVersion(const FileName: String;
      out Info: TFileVersionInfo): Boolean;

    function BuildUpdateScript(const NewFile, TargetFile: String): String;
    procedure RunUpdateScript(const ScriptFile, NewFile,
      TargetFile: String);

    procedure ResetLastCheck;
  public
    constructor Create(AOwner: TComponent); override;

    { Control/Client asks if an update is available on the Gateway. }
    function CheckUpdate: Boolean;
    { Gateway forces update on the Client (silent when SilentForce=True). }
    function ForceUpdate: Boolean;
    { Starts an update cycle when an update is available. }
    function StartUpdate(const Silent: Boolean = False): Boolean;

    property LastCheck: TUpdateCheckResult read FLastCheck;
  published
    { Path where the Gateway keeps the latest client executable (\\host\share\Update). }
    property GatewayUpdatePath: String read FGatewayUpdatePath write FGatewayUpdatePath;
    { Full path to the running executable. Defaults to ParamStr(0). }
    property ExecutableName: String read GetExecutableName write FExecutableName;
    { Temporary file name used while downloading the new executable. }
    property TemporaryFileName: String read GetTemporaryFileName
      write FTemporaryFileName;
    { When TRUE, ForceUpdate runs without confirmations on the Client. }
    property SilentForce: Boolean read FSilentForce write FSilentForce default True;

    property OnDownloadUpdate: TUpdateDownloadEvent read FOnDownloadUpdate
      write FOnDownloadUpdate;
    property OnBeforeExecute: TUpdateEvent read FOnBeforeExecute
      write FOnBeforeExecute;
  end;

implementation

{ Helper routines }

function IncludeTrailingPathDelimiterEx(const Path: String): String;
begin
  Result := IncludeTrailingPathDelimiter(Path);
end;

{ TFileVersionInfo }

class function TFileVersionInfo.Empty: TFileVersionInfo;
begin
  Result.Major := 0;
  Result.Minor := 0;
  Result.Release := 0;
  Result.Build := 0;
end;

function TFileVersionInfo.AsString: String;
begin
  Result := Format('%d.%d.%d.%d', [Major, Minor, Release, Build]);
end;

function TFileVersionInfo.Compare(const Other: TFileVersionInfo): Integer;
begin
  if Major <> Other.Major then
    Result := Integer(Major) - Integer(Other.Major)
  else if Minor <> Other.Minor then
    Result := Integer(Minor) - Integer(Other.Minor)
  else if Release <> Other.Release then
    Result := Integer(Release) - Integer(Other.Release)
  else
    Result := Integer(Build) - Integer(Other.Build);
end;

{ TAutoUpdater }

constructor TAutoUpdater.Create(AOwner: TComponent);
begin
  inherited;
  FGatewayUpdatePath := DEFAULT_UPDATE_FOLDER;
  FSilentForce := True;
  ResetLastCheck;
end;

procedure TAutoUpdater.ResetLastCheck;
begin
  FillChar(FLastCheck, SizeOf(FLastCheck), 0);
  FLastCheck.CurrentVersion := TFileVersionInfo.Empty;
  FLastCheck.AvailableVersion := TFileVersionInfo.Empty;
end;

function TAutoUpdater.GetExecutableName: String;
begin
  if FExecutableName = '' then
    Result := ParamStr(0)
  else
    Result := FExecutableName;
end;

function TAutoUpdater.GetTempFolder: String;
var
  buffer: array [0 .. MAX_PATH] of Char;
begin
  FillChar(buffer, SizeOf(buffer), 0);
  if GetTempPath(MAX_PATH, @buffer[0]) = 0 then
    Result := IncludeTrailingPathDelimiter(ExtractFilePath(GetExecutableName))
  else
    Result := IncludeTrailingPathDelimiter(buffer);
end;

function TAutoUpdater.GetTemporaryFileName: String;
var
  baseName: String;
begin
  if FTemporaryFileName = '' then
  begin
    baseName := ChangeFileExt(ExtractFileName(GetExecutableName), '_New.exe');
    Result := GetTempFolder + baseName;
  end
  else
    Result := FTemporaryFileName;
end;

function TAutoUpdater.GetUpdateSourceFile: String;
var
  baseFolder: String;
begin
  baseFolder := FGatewayUpdatePath;
  if baseFolder = '' then
    baseFolder := DEFAULT_UPDATE_FOLDER;

  if not ((Length(baseFolder) > 0) and ((baseFolder[1] = '\\') or
    (Pos(':', baseFolder) > 0))) then
    baseFolder := IncludeTrailingPathDelimiter(ExtractFilePath(GetExecutableName)) +
      IncludeTrailingPathDelimiterEx(baseFolder)
  else
    baseFolder := IncludeTrailingPathDelimiterEx(baseFolder);

  Result := baseFolder + ExtractFileName(GetExecutableName);
end;

function TAutoUpdater.QueryVersion(const FileName: String;
  out Info: TFileVersionInfo): Boolean;
var
  handle: DWORD;
  size: DWORD;
  buffer: Pointer;
  len: UINT;
  FixedPtr: PVSFixedFileInfo;
begin
  Result := False;
  Info := TFileVersionInfo.Empty;
  if not FileExists(FileName) then
    Exit;

  size := GetFileVersionInfoSize(PChar(FileName), handle);
  if size = 0 then
    Exit;

  GetMem(buffer, size);
  try
    if not GetFileVersionInfo(PChar(FileName), 0, size, buffer) then
      Exit;

    if not VerQueryValue(buffer, '\', Pointer(FixedPtr), len) then
      Exit;

    if (len >= SizeOf(TVSFixedFileInfo)) and
      (FixedPtr^.dwSignature = $FEEF04BD) then
    begin
      Info.Major := HiWord(FixedPtr^.dwFileVersionMS);
      Info.Minor := LoWord(FixedPtr^.dwFileVersionMS);
      Info.Release := HiWord(FixedPtr^.dwFileVersionLS);
      Info.Build := LoWord(FixedPtr^.dwFileVersionLS);
      Result := True;
    end;
  finally
    FreeMem(buffer);
  end;
end;

function TAutoUpdater.CheckUpdate: Boolean;
var
  localVer, remoteVer: TFileVersionInfo;
begin
  ResetLastCheck;
  FLastCheck.SourceFile := GetUpdateSourceFile;
  FLastCheck.TargetFile := GetExecutableName;
  FLastCheck.TempFile := GetTemporaryFileName;

  if QueryVersion(FLastCheck.TargetFile, localVer) then
    FLastCheck.CurrentVersion := localVer;

  if QueryVersion(FLastCheck.SourceFile, remoteVer) then
    FLastCheck.AvailableVersion := remoteVer;

  FLastCheck.HasUpdate := (FLastCheck.AvailableVersion.Compare(
    FLastCheck.CurrentVersion) > 0);
  Result := FLastCheck.HasUpdate;
end;

function TAutoUpdater.BuildUpdateScript(const NewFile, TargetFile: String): String;
var
  script: TStringList;
  batName: String;
begin
  script := TStringList.Create;
  try
    batName := GetTempFolder + 'rtcp_update_' + IntToStr(GetTickCount) + '.cmd';

    script.Add('@echo off');
    script.Add('setlocal');
    script.Add('set PID=%1');
    script.Add('set SRC=%~2');
    script.Add('set DST=%~3');
    script.Add(':waitloop');
    script.Add('ping 127.0.0.1 -n 2 >nul');
    script.Add('tasklist /FI "PID=%PID%" | find "%PID%" >nul');
    script.Add('if %errorlevel%==0 goto waitloop');
    script.Add(':copyloop');
    script.Add('move /Y "%SRC%" "%DST%" >nul');
    script.Add('if errorlevel 1 goto copyloop');
    script.Add('start "" "%DST%"');
    script.Add('del "%~f0"');

    ForceDirectories(ExtractFilePath(batName));
    script.SaveToFile(batName);
    Result := batName;
  finally
    script.Free;
  end;
end;

procedure TAutoUpdater.RunUpdateScript(const ScriptFile, NewFile,
  TargetFile: String);
var
  si: TStartupInfo;
  pi: TProcessInformation;
  cmd: String;
begin
  FillChar(si, SizeOf(si), 0);
  si.cb := SizeOf(si);
  si.dwFlags := STARTF_USESHOWWINDOW;
  si.wShowWindow := SW_HIDE;

  FillChar(pi, SizeOf(pi), 0);

  cmd := Format('"%s" %d "%s" "%s"', [ScriptFile, GetCurrentProcessId,
    NewFile, TargetFile]);

  if CreateProcess(nil, PChar(cmd), nil, nil, False, CREATE_NO_WINDOW,
    nil, nil, si, pi) then
  begin
    CloseHandle(pi.hThread);
    CloseHandle(pi.hProcess);
  end
  else
    raise Exception.CreateFmt('Falha ao iniciar script de atualização: %s',
      [SysErrorMessage(GetLastError)]);
end;

function TAutoUpdater.StartUpdate(const Silent: Boolean): Boolean;
var
  handled: Boolean;
  newFile: String;
  scriptFile: String;
  downloadSource: String;
begin
  Result := False;
  if FLastCheck.SourceFile = '' then
    CheckUpdate;
  if (not FLastCheck.HasUpdate) and (not CheckUpdate) then
    Exit;

  downloadSource := FLastCheck.SourceFile;
  if downloadSource = '' then
    Exit;
  if not FileExists(downloadSource) then
    raise Exception.CreateFmt('Arquivo de atualização não encontrado: %s',
      [downloadSource]);
  newFile := FLastCheck.TempFile;

  ForceDirectories(ExtractFilePath(newFile));
  handled := False;
  if Assigned(FOnDownloadUpdate) then
    FOnDownloadUpdate(Self, downloadSource, newFile, handled);

  if not handled then
    if not CopyFile(PChar(downloadSource), PChar(newFile), False) then
      raise Exception.CreateFmt('Não foi possível copiar %s para %s. %s',
        [downloadSource, newFile, SysErrorMessage(GetLastError)]);

  if Assigned(FOnBeforeExecute) then
    FOnBeforeExecute(Self, FLastCheck);

  scriptFile := BuildUpdateScript(newFile, FLastCheck.TargetFile);
  RunUpdateScript(scriptFile, newFile, FLastCheck.TargetFile);

  Result := True;
  if Silent then
    TerminateProcess(GetCurrentProcess, 0)
  else
    Halt;
end;

function TAutoUpdater.ForceUpdate: Boolean;
begin
  if (FLastCheck.SourceFile = '') or (not FileExists(FLastCheck.SourceFile)) then
    CheckUpdate;
  FLastCheck.HasUpdate := True;
  Result := StartUpdate(FSilentForce);
end;

end.

