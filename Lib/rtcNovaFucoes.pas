unit rtcNovaFucoes;

interface

uses
  Windows, SysUtils, Classes, Variants, ActiveX, ComObj, WinInet, Winsock,
  TlHelp32, Registry, ShellAPI;

function NomeUsuario: String;
function NomeComputador: String;
function NomeGrupoRede: String;
function IPInterno: String;
function IPExterno: String;
function NomeProcessador: String;
function MemoriaRAMTotal: UInt64;
function PossuiWebcam: Boolean;
function RelancarComoAdministrador: Boolean;
function HWIDBase64: String;
function HWIDTexto: String;
function HWIDDecodificado: String;
function CriarGuidTexto: String;
function NumeroSerieDiscos: String;

// Funções adicionais úteis para administração remota
function ListaProcessosAtivos: TStringList;
function FinalizarProcesso(const AProcessId: Cardinal): Boolean;
function VersaoSistemaOperacional: String;
function TempoLigadoSegundos: Int64;

implementation

const
  CRYPT_STRING_BASE64 = $00000001;
  CRYPT_STRING_NOCRLF = $40000000;

type
  TMemoryStatusEx = record
    dwLength: DWORD;
    dwMemoryLoad: DWORD;
    ullTotalPhys: UInt64;
    ullAvailPhys: UInt64;
    ullTotalPageFile: UInt64;
    ullAvailPageFile: UInt64;
    ullTotalVirtual: UInt64;
    ullAvailVirtual: UInt64;
    ullAvailExtendedVirtual: UInt64;
  end;

function GlobalMemoryStatusEx(var Buffer: TMemoryStatusEx): BOOL; stdcall;
  external kernel32 name 'GlobalMemoryStatusEx';

function NetGetJoinInformation(lpServer: LPCWSTR; lpNameBuffer: LPWSTR;
  var BufferType: DWORD): DWORD; stdcall; external 'netapi32.dll';
function NetApiBufferFree(Buffer: Pointer): DWORD; stdcall; external 'netapi32.dll';

function CryptBinaryToStringA(pbBinary: PByte; cbBinary: DWORD; dwFlags: DWORD;
  pszString: PAnsiChar; var pcchString: DWORD): BOOL; stdcall;
  external advapi32 name 'CryptBinaryToStringA';
function CryptStringToBinaryA(pszString: PAnsiChar; cchString: DWORD;
  dwFlags: DWORD; pbBinary: PByte; var pcbBinary: DWORD; pdwSkip, pdwFlags: PCardinal): BOOL; stdcall;
  external advapi32 name 'CryptStringToBinaryA';

function StrToBase64(const Value: string): string;
var
  AnsiVal: AnsiString;
  OutLen: DWORD;
begin
  AnsiVal := AnsiString(Value);
  if Length(AnsiVal) = 0 then
    Exit('');
  OutLen := 0;
  if not CryptBinaryToStringA(PByte(@AnsiVal[1]), Length(AnsiVal),
    CRYPT_STRING_BASE64 or CRYPT_STRING_NOCRLF, nil, OutLen) then
    Exit('');
  SetLength(Result, OutLen);
  if not CryptBinaryToStringA(PByte(@AnsiVal[1]), Length(AnsiVal),
    CRYPT_STRING_BASE64 or CRYPT_STRING_NOCRLF, PAnsiChar(Result), OutLen) then
    Result := ''
  else
  begin
    SetLength(Result, OutLen);
    if (Result <> '') and (Result[Length(Result)] = #0) then
      SetLength(Result, Length(Result) - 1);
  end;
end;

function Base64ToStr(const Value: string): string;
var
  AnsiVal: AnsiString;
  OutLen: DWORD;
  Buffer: AnsiString;
begin
  AnsiVal := AnsiString(Value);
  OutLen := 0;
  if not CryptStringToBinaryA(PAnsiChar(AnsiVal), Length(AnsiVal),
    CRYPT_STRING_BASE64, nil, OutLen, nil, nil) then
    Exit('');
  SetLength(Buffer, OutLen);
  if not CryptStringToBinaryA(PAnsiChar(AnsiVal), Length(AnsiVal),
    CRYPT_STRING_BASE64, PByte(@Buffer[1]), OutLen, nil, nil) then
    Exit('');
  if OutLen > 0 then
  begin
    SetLength(Result, OutLen);
    Move(Buffer[1], Result[1], OutLen);
  end
  else
    Result := '';
end;

function NomeUsuario: String;
var
  Buffer: array [0 .. 255] of Char;
  Size: DWORD;
begin
  Size := Length(Buffer);
  if GetUserName(Buffer, Size) then
    Result := Buffer
  else
    Result := '';
end;

function NomeComputador: String;
var
  Buffer: array [0 .. MAX_COMPUTERNAME_LENGTH] of Char;
  Size: DWORD;
begin
  Size := Length(Buffer);
  if GetComputerName(Buffer, Size) then
    Result := Buffer
  else
    Result := '';
end;

function NomeGrupoRede: String;
var
  JoinBuffer: LPWSTR;
  JoinType: DWORD;
  Status: DWORD;
begin
  Result := '';
  JoinBuffer := nil;
  Status := NetGetJoinInformation(nil, JoinBuffer, JoinType);
  if Status = ERROR_SUCCESS then
  begin
    Result := JoinBuffer;
    NetApiBufferFree(JoinBuffer);
  end;
end;

function IPInterno: String;
var
  HostName: array [0 .. 255] of AnsiChar;
  HostEnt: PHostEnt;
  Addr: PAnsiChar;
  Wsa: TWSAData;
begin
  Result := '';
  if WSAStartup($0202, Wsa) <> 0 then
    Exit;
  try
    if gethostname(HostName, SizeOf(HostName)) = 0 then
    begin
      HostEnt := gethostbyname(HostName);
      if Assigned(HostEnt) then
      begin
        Addr := HostEnt^.h_addr_list^;
        if Assigned(Addr) then
          Result := string(inet_ntoa(PInAddr(Addr)^));
      end;
    end;
  finally
    WSACleanup;
  end;
end;

function HttpGet(const Url: string): string;
const
  USER_AGENT = 'rtcNovaFuncoes/1.0';
var
  hInet, hUrl: HINTERNET;
  Buffer: array [0 .. 1023] of Byte;
  BytesRead: DWORD;
  Response: TMemoryStream;
begin
  Result := '';
  hInet := InternetOpen(USER_AGENT, INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
  if hInet = nil then
    Exit;
  try
    hUrl := InternetOpenUrl(hInet, PChar(Url), nil, 0, INTERNET_FLAG_RELOAD, 0);
    if hUrl = nil then
      Exit;
    Response := TMemoryStream.Create;
    try
      repeat
        BytesRead := 0;
        if InternetReadFile(hUrl, @Buffer, SizeOf(Buffer), BytesRead) and
          (BytesRead > 0) then
          Response.Write(Buffer, BytesRead)
        else
          Break;
      until False;
      if Response.Size > 0 then
        SetString(Result, PAnsiChar(Response.Memory), Response.Size)
      else
        Result := '';
    finally
      Response.Free;
      InternetCloseHandle(hUrl);
    end;
  finally
    InternetCloseHandle(hInet);
  end;
end;

function IPExterno: String;
begin
  // Pode falhar caso não haja conexão com a internet
  Result := Trim(HttpGet('http://api.ipify.org'));
end;

function NomeProcessador: String;
var
  Reg: TRegistry;
begin
  Result := '';
  Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKey('\HARDWARE\DESCRIPTION\System\CentralProcessor\0', False) then
    begin
      Result := Reg.ReadString('ProcessorNameString');
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

function MemoriaRAMTotal: UInt64;
var
  Status: TMemoryStatusEx;
begin
  FillChar(Status, SizeOf(Status), 0);
  Status.dwLength := SizeOf(Status);
  if GlobalMemoryStatusEx(Status) then
    Result := Status.ullTotalPhys
  else
    Result := 0;
end;

function PossuiWebcam: Boolean;
var
  Locator: OLEVariant;
  Services: OLEVariant;
  ObjSet: OLEVariant;
  Obj: OleVariant;
  Enum: IEnumVariant;
  Value: LongWord;
begin
  Result := False;
  if Failed(CoInitialize(nil)) then
    Exit;
  try
    Locator := CreateOleObject('WbemScripting.SWbemLocator');
    Services := Locator.ConnectServer('.', 'root\CIMV2');
    ObjSet := Services.ExecQuery('SELECT * FROM Win32_PnPEntity WHERE (PNPClass = "Image" OR PNPClass = "Camera")');
    Enum := IUnknown(ObjSet._NewEnum) as IEnumVariant;
    while Enum.Next(1, Obj, Value) = 0 do
    begin
      Result := True;
      Break;
    end;
  finally
    CoUninitialize;
  end;
end;

function RelancarComoAdministrador: Boolean;
var
  FileName: array [0 .. MAX_PATH] of Char;
begin
  GetModuleFileName(0, FileName, MAX_PATH);
  Result := ShellExecute(0, 'runas', FileName, nil, nil, SW_SHOWNORMAL) > 32;
end;

function VolumeSerial: DWORD;
begin
  Result := 0;
  GetVolumeInformation('C:\', nil, 0, @Result, nil, nil, nil, 0);
end;

function HWIDTexto: String;
begin
  Result := Format('%s|%s|%x|%s', [NomeComputador, NomeUsuario, VolumeSerial,
    NomeProcessador]);
end;

function HWIDBase64: String;
begin
  Result := StrToBase64(HWIDTexto);
end;

function CriarGuidTexto: String;
var
  G: TGUID;
begin
  if CreateGUID(G) = S_OK then
    Result := GUIDToString(G)
  else
    Result := '';
end;

function NumeroSerieDiscos: String;
var
  Locator, Services, ObjSet, Obj: OLEVariant;
  Enum: IEnumVariant;
  Value: LongWord;
  Serial: string;
begin
  Result := '';
  if Failed(CoInitialize(nil)) then
    Exit;
  try
    Locator := CreateOleObject('WbemScripting.SWbemLocator');
    Services := Locator.ConnectServer('.', 'root\CIMV2');
    ObjSet := Services.ExecQuery('SELECT SerialNumber FROM Win32_PhysicalMedia');
    Enum := IUnknown(ObjSet._NewEnum) as IEnumVariant;
    while Enum.Next(1, Obj, Value) = 0 do
    begin
      Serial := Trim(Obj.SerialNumber);
      if Serial <> '' then
      begin
        if Result <> '' then
          Result := Result + ';';
        Result := Result + Serial;
      end;
    end;
  finally
    CoUninitialize;
  end;
end;

function ListaProcessosAtivos: TStringList;
var
  Snapshot: THandle;
  ProcEntry: TProcessEntry32;
begin
  Result := TStringList.Create;
  Snapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if Snapshot = INVALID_HANDLE_VALUE then
    Exit;
  try
    ProcEntry.dwSize := SizeOf(ProcEntry);
    if Process32First(Snapshot, ProcEntry) then
    begin
      repeat
        Result.Add(Format('%s (PID %d)', [ProcEntry.szExeFile, ProcEntry.th32ProcessID]));
      until not Process32Next(Snapshot, ProcEntry);
    end;
  finally
    CloseHandle(Snapshot);
  end;
end;

function FinalizarProcesso(const AProcessId: Cardinal): Boolean;
var
  hProcess: THandle;
begin
  Result := False;
  hProcess := OpenProcess(PROCESS_TERMINATE, False, AProcessId);
  if hProcess <> 0 then
  try
    Result := TerminateProcess(hProcess, 0);
  finally
    CloseHandle(hProcess);
  end;
end;

function VersaoSistemaOperacional: String;
var
  Info: TOSVersionInfoEx;
begin
  FillChar(Info, SizeOf(Info), 0);
  Info.dwOSVersionInfoSize := SizeOf(Info);
  if GetVersionEx(Info) then
    Result := Format('%d.%d Build %d %s', [Info.dwMajorVersion, Info.dwMinorVersion,
      Info.dwBuildNumber, Info.szCSDVersion])
  else
    Result := '';
end;

function TempoLigadoSegundos: Int64;
var
  Tick: Int64;
begin
  Tick := GetTickCount64;
  Result := Tick div 1000;
end;

function HWIDDecodificado: String;
begin
  Result := Base64ToStr(HWIDBase64);
end;

end.
