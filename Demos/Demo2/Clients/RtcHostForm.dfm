object MainForm: TMainForm
  Left = 610
  Top = 152
  ActiveControl = eUserName
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsNone
  Caption = 'RTC Host v5'
  ClientHeight = 485
  ClientWidth = 273
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefault
  PrintScale = poNone
  Scaled = False
  ShowHint = True
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 14
  object pMaster: TPanel
    Left = 0
    Top = 28
    Width = 273
    Height = 457
    Align = alClient
    ParentBackground = False
    TabOrder = 0
    object lblStatusPanel: TPanel
      Left = 1
      Top = 423
      Width = 271
      Height = 33
      Align = alBottom
      BevelInner = bvLowered
      Color = clGray
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      object lblStatus: TLabel
        Left = 2
        Top = 2
        Width = 267
        Height = 29
        Align = alClient
        Alignment = taCenter
        AutoSize = False
        Caption = 'Click "START" to log in and start Hosting.'
        Color = clGray
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
        Layout = tlCenter
        WordWrap = True
      end
    end
    object Pages: TPageControl
      Left = 1
      Top = 1
      Width = 271
      Height = 422
      ActivePage = Page_Setup
      Align = alClient
      MultiLine = True
      TabOrder = 0
      TabStop = False
      object Page_Setup: TTabSheet
        Caption = 'Setup'
        object Label3: TLabel
          Left = 13
          Top = 200
          Width = 49
          Height = 14
          Alignment = taRightJustify
          Caption = 'Username'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object Label4: TLabel
          Left = 12
          Top = 224
          Width = 50
          Height = 14
          Alignment = taRightJustify
          Caption = 'Password'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object Label12: TLabel
          Left = 16
          Top = 170
          Width = 45
          Height = 14
          Caption = 'Gateway'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object btnGateway: TSpeedButton
          Left = 68
          Top = 164
          Width = 181
          Height = 25
          Caption = '< Click to set up connection >'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          OnClick = btnGatewayClick
        end
        object Label1: TLabel
          Left = 11
          Top = 248
          Width = 51
          Height = 14
          Alignment = taRightJustify
          Caption = 'Real Name'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object btnLogin: TButton
          Left = 188
          Top = 268
          Width = 73
          Height = 41
          Caption = 'START'
          Default = True
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          TabOrder = 6
          OnClick = btnLoginClick
        end
        object eUserName: TEdit
          Left = 68
          Top = 195
          Width = 181
          Height = 22
          TabOrder = 0
          OnChange = eUserNameChange
        end
        object ePassword: TEdit
          Left = 68
          Top = 219
          Width = 125
          Height = 22
          PasswordChar = '*'
          TabOrder = 1
          OnChange = ePasswordChange
        end
        object xAdvanced: TCheckBox
          Left = 68
          Top = 268
          Width = 117
          Height = 17
          Hint = 
            'Check this box if you want to change settings before you start h' +
            'osting'
          TabStop = False
          Caption = 'Change Settings'
          TabOrder = 4
          OnClick = xAdvancedClick
        end
        object xSavePassword: TCheckBox
          Left = 200
          Top = 220
          Width = 61
          Height = 21
          Hint = 
            'Check this box if you want your password to be saved for the nex' +
            't time'
          TabStop = False
          Caption = 'Save'
          TabOrder = 2
        end
        object xAutoConnect: TCheckBox
          Left = 68
          Top = 292
          Width = 117
          Height = 17
          Hint = 
            'Check thix box if you want to be logged in automatically after a' +
            'ny kind of connection problems'
          TabStop = False
          Caption = 'Auto Re-Login'
          TabOrder = 5
        end
        object Panel1: TPanel
          Left = 0
          Top = 316
          Width = 261
          Height = 77
          BevelInner = bvRaised
          BevelOuter = bvLowered
          TabOrder = 8
          object Label25: TLabel
            Left = 4
            Top = 4
            Width = 221
            Height = 14
            Caption = 'RTC Host can also run as a Windows Service'
          end
          object Label2: TLabel
            Left = 8
            Top = 26
            Width = 40
            Height = 14
            Caption = 'Service:'
          end
          object btnInstall: TSpeedButton
            Left = 52
            Top = 20
            Width = 53
            Height = 25
            Caption = 'Install'
            OnClick = btnInstallClick
          end
          object btnRun: TSpeedButton
            Left = 104
            Top = 20
            Width = 41
            Height = 25
            Caption = 'Run'
            OnClick = btnRunClick
          end
          object btnStop: TSpeedButton
            Left = 144
            Top = 20
            Width = 41
            Height = 25
            Caption = 'Stop'
            OnClick = btnStopClick
          end
          object btnUninstall: TSpeedButton
            Left = 184
            Top = 20
            Width = 61
            Height = 25
            Caption = 'Uninstall'
            OnClick = btnUninstallClick
          end
          object btnSaveSetup: TSpeedButton
            Left = 13
            Top = 46
            Width = 93
            Height = 25
            Caption = 'Save Setup'
            OnClick = btnSaveSetupClick
          end
          object btnRestartService: TSpeedButton
            Left = 104
            Top = 46
            Width = 140
            Height = 25
            Caption = 'Restart Service && Exit'
            OnClick = btnRestartServiceClick
          end
        end
        object Panel2: TPanel
          Left = 0
          Top = 4
          Width = 261
          Height = 157
          Cursor = crHandPoint
          BevelInner = bvRaised
          BevelOuter = bvLowered
          Color = clWhite
          ParentBackground = False
          TabOrder = 7
          OnClick = RtcCopyrightClick
          object Label7: TLabel
            Left = 4
            Top = 92
            Width = 253
            Height = 17
            Cursor = crHandPoint
            Alignment = taCenter
            AutoSize = False
            Caption = 'Copyright (c) RealThinClient components'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            OnClick = RtcCopyrightClick
          end
          object Label11: TLabel
            Left = 4
            Top = 22
            Width = 253
            Height = 17
            Cursor = crHandPoint
            Alignment = taCenter
            AutoSize = False
            Caption = 'Created with RTC SDK and RTC Portal for Delphi'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            OnClick = RtcCopyrightClick
          end
          object Label8: TLabel
            Left = 4
            Top = 4
            Width = 253
            Height = 17
            Cursor = crHandPoint
            Alignment = taCenter
            AutoSize = False
            Caption = 'RTC Portal Host'
            Color = clBlack
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWhite
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = []
            ParentColor = False
            ParentFont = False
            Transparent = False
            Layout = tlCenter
            WordWrap = True
            OnClick = RtcCopyrightClick
          end
          object Label5: TLabel
            Left = 4
            Top = 108
            Width = 253
            Height = 45
            Cursor = crHandPoint
            Alignment = taCenter
            AutoSize = False
            Caption = 
              'Get the component version to completely redesign the user interf' +
              'ace and rebrand everything to make it look-and-feel like your ot' +
              'her applications.'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clNavy
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            WordWrap = True
            OnClick = RtcCopyrightClick
          end
        end
        object eRealName: TEdit
          Left = 68
          Top = 243
          Width = 181
          Height = 22
          TabOrder = 3
          OnChange = eRealNameChange
        end
      end
      object Page_Hosting: TTabSheet
        Caption = 'Hosting'
        ImageIndex = 1
        object Label9: TLabel
          Left = 36
          Top = 8
          Width = 143
          Height = 14
          Caption = 'Users connected to this Host:'
        end
        object sStatus1: TShape
          Left = 8
          Top = 8
          Width = 15
          Height = 15
          Brush.Color = clSilver
          Shape = stCircle
        end
        object sStatus2: TShape
          Left = 16
          Top = 8
          Width = 15
          Height = 15
          Brush.Color = clSilver
          Shape = stCircle
        end
        object Label21: TLabel
          Left = 8
          Top = 315
          Width = 36
          Height = 14
          Caption = 'Priority:'
        end
        object btnLogout: TSpeedButton
          Left = 192
          Top = 348
          Width = 65
          Height = 33
          Caption = 'LOG OUT'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          OnClick = btnLogOutClick
        end
        object btnSettings: TSpeedButton
          Left = 148
          Top = 308
          Width = 109
          Height = 29
          Caption = 'Change Settings'
          OnClick = btnSettingsClick
        end
        object eConnected: TListView
          Left = 8
          Top = 28
          Width = 249
          Height = 274
          Columns = <>
          IconOptions.Arrangement = iaLeft
          IconOptions.AutoArrange = True
          ReadOnly = True
          RowSelect = True
          SortType = stText
          TabOrder = 1
          ViewStyle = vsSmallIcon
        end
        object cPriority: TComboBox
          Left = 48
          Top = 311
          Width = 97
          Height = 22
          Style = csDropDownList
          ItemHeight = 14
          ItemIndex = 1
          TabOrder = 0
          TabStop = False
          Text = 'NORMAL'
          OnChange = cPriorityChange
          Items.Strings = (
            'HIGH'
            'NORMAL'
            'LOW')
        end
        object pSendFiles: TPanel
          Left = 4
          Top = 344
          Width = 181
          Height = 41
          BorderWidth = 5
          Color = clTeal
          ParentBackground = False
          TabOrder = 2
          Visible = False
          object Label16: TLabel
            Left = 6
            Top = 6
            Width = 169
            Height = 29
            Align = alClient
            Alignment = taCenter
            AutoSize = False
            Caption = 
              'To download Files or Folders, drag them here from Windows Explor' +
              'er.'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWhite
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            Transparent = True
            WordWrap = True
          end
        end
      end
    end
  end
  object pTitlebar: TPanel
    Left = 0
    Top = 0
    Width = 273
    Height = 28
    Align = alTop
    Color = clMoneyGreen
    ParentBackground = False
    TabOrder = 1
    OnMouseDown = pTitlebarMouseDown
    OnMouseMove = pTitlebarMouseMove
    OnMouseUp = pTitlebarMouseUp
    object cTitleBar: TLabel
      Left = 1
      Top = 1
      Width = 271
      Height = 26
      Align = alClient
      Caption = '  RTC Portal Host v5'
      Color = clMoneyGreen
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -14
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
      Transparent = False
      Layout = tlCenter
      OnMouseDown = pTitlebarMouseDown
      OnMouseMove = pTitlebarMouseMove
      OnMouseUp = pTitlebarMouseUp
    end
    object btnMinimize: TSpeedButton
      Left = 224
      Top = 4
      Width = 25
      Height = 21
      Caption = '--'
      OnClick = btnMinimizeClick
    end
    object btnClose: TSpeedButton
      Left = 248
      Top = 4
      Width = 21
      Height = 21
      Caption = 'X'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = btnCloseClick
    end
  end
  object PClient: TRtcHttpPortalClient
    UserVisible = True
    GwStoreParams = True
    OnLogIn = PClientLogIn
    OnLogOut = PClientLogOut
    OnParams = PClientParams
    OnStart = PClientStart
    OnError = PClientError
    OnFatalError = PClientFatalError
    AutoSyncEvents = True
    DataEncrypt = 16
    DataForceEncrypt = True
    RetryOtherCalls = 5
    MultiThreaded = True
    Gate_Timeout = 300
    OnStatusGet = PClientStatusGet
    OnStatusPut = PClientStatusPut
    Left = 4
    Top = 456
  end
  object PFileTrans: TRtcPFileTransfer
    Client = PClient
    BeTheHost = True
    GwStoreParams = True
    OnUserJoined = PModuleUserJoined
    OnUserLeft = PModuleUserLeft
    OnNewUI = PFileTransNewUI
    Left = 36
    Top = 456
  end
  object PChat: TRtcPChat
    Client = PClient
    BeTheHost = True
    GwStoreParams = True
    OnNewUI = PChatNewUI
    OnUserJoined = PModuleUserJoined
    OnUserLeft = PModuleUserLeft
    Left = 68
    Top = 456
  end
  object PDesktopHost: TRtcPDesktopHost
    Client = PClient
    GwStoreParams = True
    FileTransfer = PFileTrans
    OnUserJoined = PModuleUserJoined
    OnUserLeft = PModuleUserLeft
    Left = 100
    Top = 456
  end
  object PDesktopControl: TRtcPDesktopControl
    Client = PClient
    OnNewUI = PDesktopControlNewUI
    Left = 132
    Top = 456
  end
end
