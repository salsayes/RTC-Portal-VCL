program HostMain;

uses
  Vcl.Forms,
  HostMainForm in 'HostMainForm.pas' {HostForm};

{$R *.res}

begin
  Application.Initialize;
  Application.ShowMainForm := False;
  Application.CreateForm(THostForm, HostForm);
  Application.Run;
end.
