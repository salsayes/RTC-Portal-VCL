object ControlForm: TControlForm
  Caption = 'Control Demo'
  ClientHeight = 700
  ClientWidth = 1100
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
  object pnlTop: TPanel
    Align = alTop
    Height = 41
    TabOrder = 0
    object lblAddress: TLabel
      Left = 8
      Top = 12
      Width = 44
      Height = 13
      Caption = 'Gateway'
    end
    object edtAddress: TEdit
      Left = 64
      Top = 8
      Width = 200
      Height = 21
      TabOrder = 0
      Text = '127.0.0.1'
    end
    object btnConnect: TButton
      Left = 272
      Top = 7
      Width = 90
      Height = 25
      Caption = 'Conectar'
      TabOrder = 1
      OnClick = btnConnectClick
    end
  end
  object ClientTree: TVirtualStringTree
    Align = alTop
    Height = 160
    Header.AutoSizeIndex = -1
    Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs]
    TabOrder = 1
    OnGetText = ClientTreeGetText
  end
  object MemoLog: TMemo
    Align = alBottom
    Height = 120
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object Pages: TPageControl
    Align = alClient
    TabOrder = 3
    ActivePage = tabDesktop
    object tabDesktop: TTabSheet
      Caption = 'Desktop'
    end
    object tabShell: TTabSheet
      Caption = 'Shell'
      object MemoShell: TMemo
        Align = alClient
        TabOrder = 0
        ScrollBars = ssVertical
      end
      object edtShellCommand: TEdit
        Align = alBottom
        Height = 24
        TabOrder = 1
        Text = 'whoami'
      end
      object btnSendShell: TButton
        Align = alBottom
        Height = 25
        Caption = 'Enviar'
        TabOrder = 2
        OnClick = btnSendShellClick
      end
    end
    object tabCamera: TTabSheet
      Caption = 'Câmera'
      object imgCamera: TImage
        Align = alClient
        Stretch = True
      end
    end
    object tabFiles: TTabSheet
      Caption = 'Arquivos'
      object btnSendFile: TButton
        Left = 8
        Top = 8
        Width = 120
        Height = 25
        Caption = 'Enviar Arquivo'
        TabOrder = 0
        OnClick = btnSendFileClick
      end
    end
    object tabUpdate: TTabSheet
      Caption = 'Update'
      object btnSendUpdate: TButton
        Left = 8
        Top = 8
        Width = 150
        Height = 25
        Caption = 'Enviar Atualização'
        TabOrder = 0
        OnClick = btnSendUpdateClick
      end
    end
  end
  object dlgOpen: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = []
    Left = 880
    Top = 32
  end
end
