program ControlMain;

uses
  Vcl.Forms,
  ControlMainForm in 'ControlMainForm.pas' {ControlForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TControlForm, ControlForm);
  Application.Run;
end.
