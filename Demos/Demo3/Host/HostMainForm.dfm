object HostForm: THostForm
  Caption = 'Host Demo'
  ClientHeight = 200
  ClientWidth = 300
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object TimerHeartbeat: TTimer
    Enabled = False
    OnTimer = TimerHeartbeatTimer
  end
end
