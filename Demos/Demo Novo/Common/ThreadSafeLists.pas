unit ThreadSafeLists;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  PortalTypes,
  Winapi.Windows;

type
  TThreadSafeClientList = class
  private
    FList: TList<TClientIdentity>;
    FCritical: TRTLCriticalSection;
    procedure Lock;
    procedure Unlock;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddClient(const Info: TClientIdentity);
    procedure RemoveClient(const ClientId: string);
    function Snapshot: TArray<TClientIdentity>;
  end;

implementation

{ TThreadSafeClientList }

constructor TThreadSafeClientList.Create;
begin
  InitializeCriticalSection(FCritical);
  FList := TList<TClientIdentity>.Create;
end;

destructor TThreadSafeClientList.Destroy;
begin
  Lock;
  try
    FList.Clear;
    FList.Free;
  finally
    Unlock;
    DeleteCriticalSection(FCritical);
  end;
  inherited;
end;

procedure TThreadSafeClientList.Lock;
begin
  EnterCriticalSection(FCritical);
end;

procedure TThreadSafeClientList.Unlock;
begin
  LeaveCriticalSection(FCritical);
end;

procedure TThreadSafeClientList.AddClient(const Info: TClientIdentity);
begin
  Lock;
  try
    FList.Add(Info);
  finally
    Unlock;
  end;
end;

procedure TThreadSafeClientList.RemoveClient(const ClientId: string);
var
  Index: Integer;
  Item: TClientIdentity;
begin
  Lock;
  try
    for Index := FList.Count - 1 downto 0 do
    begin
      Item := FList[Index];
      if SameText(Item.Id, ClientId) then
        FList.Delete(Index);
    end;
  finally
    Unlock;
  end;
end;

function TThreadSafeClientList.Snapshot: TArray<TClientIdentity>;
begin
  Lock;
  try
    Result := FList.ToArray;
  finally
    Unlock;
  end;
end;

end.
