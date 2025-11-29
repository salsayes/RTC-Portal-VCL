program GatewayMain;

uses
  Vcl.Forms,
  GatewayMainForm in 'GatewayMainForm.pas' {GatewayForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TGatewayForm, GatewayForm);
  Application.Run;
end.
