object GatewayForm: TGatewayForm
  Caption = 'Gateway Demo'
  ClientHeight = 600
  ClientWidth = 1000
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
  object ClientTree: TVirtualStringTree
    Align = alClient
    Header.AutoSizeIndex = -1
    Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs]
    TabOrder = 0
  end
  object MemoLog: TMemo
    Align = alBottom
    Height = 150
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object TimerHeartbeat: TTimer
    Enabled = False
    OnTimer = TimerHeartbeatTimer
  end
end
