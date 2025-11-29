object MainForm: TMainForm
  Left = 462
  Top = 158
  ActiveControl = ePort
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsNone
  Caption = 'RTC Gateway v5'
  ClientHeight = 502
  ClientWidth = 276
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PrintScale = poNone
  Scaled = False
  ShowHint = True
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 14
  object pMaster: TPanel
    Left = 0
    Top = 28
    Width = 276
    Height = 441
    Align = alClient
    TabOrder = 0
    object Pages: TPageControl
      Left = 1
      Top = 1
      Width = 274
      Height = 439
      ActivePage = Page_Setup
      Align = alClient
      TabOrder = 0
      TabStop = False
      object Page_Setup: TTabSheet
        Caption = 'Setup'
        object Label2: TLabel
          Left = 12
          Top = 152
          Width = 48
          Height = 14
          Caption = 'Local Port'
        end
        object Label7: TLabel
          Left = 12
          Top = 180
          Width = 57
          Height = 14
          Caption = 'Secure Key'
        end
        object lblSelect: TLabel
          Left = 12
          Top = 128
          Width = 213
          Height = 14
          Caption = 'Gateway Setup (how to run your Server) ...'
        end
        object ePort: TEdit
          Left = 84
          Top = 148
          Width = 73
          Height = 22
          Hint = 'Enter the TCP Port you want to use'
          TabOrder = 0
        end
        object btnLogin: TButton
          Left = 173
          Top = 281
          Width = 77
          Height = 33
          Caption = 'START'
          Default = True
          TabOrder = 8
          OnClick = btnLoginClick
        end
        object eSecureKey: TEdit
          Left = 84
          Top = 176
          Width = 165
          Height = 22
          Hint = 
            'Enter the Secure Key you want to use. Hosts, Viewers and Control' +
            's will need to use the same.'
          PasswordChar = '*'
          TabOrder = 2
        end
        object xSSL: TCheckBox
          Left = 168
          Top = 148
          Width = 45
          Height = 21
          TabStop = False
          Caption = 'SSL'
          Enabled = False
          TabOrder = 1
          Visible = False
          OnClick = xSSLClick
        end
        object eAddress: TEdit
          Left = 84
          Top = 204
          Width = 165
          Height = 22
          Hint = 'Enter the IP Address of the Network Addapter you want to use'
          Color = clGray
          Enabled = False
          TabOrder = 4
        end
        object xISAPI: TCheckBox
          Left = 12
          Top = 232
          Width = 65
          Height = 21
          Hint = 
            'You want to emulate the Gateway as if it was running as an ISAPI' +
            ' DLL?'
          TabStop = False
          Caption = 'as ISAPI'
          TabOrder = 5
          OnClick = xISAPIClick
        end
        object eISAPI: TEdit
          Left = 84
          Top = 232
          Width = 165
          Height = 22
          Hint = 'Enter the PATH on which the ISAPI DLL would be accessible'
          Color = clGray
          Enabled = False
          TabOrder = 6
        end
        object xBindIP: TCheckBox
          Left = 12
          Top = 204
          Width = 69
          Height = 21
          Hint = 'You do not want to Listen on all Network Addapters?'
          TabStop = False
          Caption = 'Bind to IP'
          TabOrder = 3
          OnClick = xBindIPClick
        end
        object Panel2: TPanel
          Left = 3
          Top = 318
          Width = 257
          Height = 86
          BevelInner = bvLowered
          TabOrder = 9
          object Label25: TLabel
            Left = 5
            Top = 10
            Width = 244
            Height = 14
            Caption = 'RTC Gateway can also run as a Windows Service'
          end
          object Label24: TLabel
            Left = 12
            Top = 36
            Width = 40
            Height = 14
            Caption = 'Service:'
          end
          object btnInstall: TSpeedButton
            Left = 56
            Top = 30
            Width = 53
            Height = 25
            Caption = 'Install'
            OnClick = btnInstallClick
          end
          object btnRun: TSpeedButton
            Left = 108
            Top = 30
            Width = 41
            Height = 25
            Caption = 'Run'
            OnClick = btnRunClick
          end
          object btnStop: TSpeedButton
            Left = 148
            Top = 30
            Width = 41
            Height = 25
            Caption = 'Stop'
            OnClick = btnStopClick
          end
          object btnUninstall: TSpeedButton
            Left = 188
            Top = 30
            Width = 61
            Height = 25
            Caption = 'Uninstall'
            OnClick = btnUninstallClick
          end
          object btnRestartService: TSpeedButton
            Left = 108
            Top = 56
            Width = 141
            Height = 25
            Caption = 'Restart Service && Exit'
            OnClick = btnRestartServiceClick
          end
          object btnSaveSetup: TSpeedButton
            Left = 16
            Top = 56
            Width = 93
            Height = 25
            Caption = 'Save Setup'
            OnClick = btnSaveSetupClick
          end
        end
        object Panel3: TPanel
          Left = 4
          Top = 4
          Width = 253
          Height = 117
          BevelInner = bvRaised
          BevelOuter = bvLowered
          Color = clWhite
          TabOrder = 10
          OnClick = RtcCopyrightClick
          object Label1: TLabel
            Left = 4
            Top = 97
            Width = 241
            Height = 15
            Cursor = crHandPoint
            Alignment = taCenter
            AutoSize = False
            Caption = 'Copyright (c) RealThinClient components'
            OnClick = RtcCopyrightClick
          end
          object Label3: TLabel
            Left = 4
            Top = 26
            Width = 245
            Height = 13
            Alignment = taCenter
            AutoSize = False
            Caption = 'Created with RTC SDK ad RTC Portal for Delphi'
            OnClick = RtcCopyrightClick
          end
          object Label8: TLabel
            Left = 4
            Top = 4
            Width = 245
            Height = 19
            Cursor = crHandPoint
            Alignment = taCenter
            AutoSize = False
            Caption = 'RTC Portal Gateway'
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
        end
        object xNoAutoRegUsers: TCheckBox
          Left = 12
          Top = 259
          Width = 221
          Height = 21
          Hint = 
            'You want to disable automatic user registration (unregitered use' +
            'rs will not be able to connect)?'
          TabStop = False
          Caption = 'Disable Automatic User Registration'
          TabOrder = 7
          OnClick = xISAPIClick
        end
      end
      object Page_Active: TTabSheet
        Caption = 'Active'
        ImageIndex = 1
        object Label5: TLabel
          Left = 8
          Top = 10
          Width = 83
          Height = 14
          Caption = 'Logged-in Users:'
        end
        object btnLogout: TSpeedButton
          Left = 180
          Top = 370
          Width = 77
          Height = 37
          Caption = 'STOP'
          OnClick = btnLogoutClick
        end
        object eUsers: TVirtualStringTree
          Left = 8
          Top = 28
          Width = 249
          Height = 336
          Color = clBtnFace
          Enabled = False
          Header.AutoSizeIndex = 0
          Header.Options = [hoAutoResize, hoColumnResize, hoVisible]
          TabOrder = 0
          TreeOptions.AnimationOptions = [toAnimatedToggle]
          TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toGridExtensions, toInitOnSave]
          TreeOptions.SelectionOptions = [toExtendedFocus, toRightClickSelect]
          OnFreeNode = eUsersFreeNode
          OnGetText = eUsersGetText
        end
      end
    end
  end
  object pTitlebar: TPanel
    Left = 0
    Top = 0
    Width = 276
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
      Width = 274
      Height = 26
      Align = alClient
      Caption = '  RTC Portal Gateway v5'
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
      Left = 232
      Top = 4
      Width = 20
      Height = 21
      Caption = '--'
      OnClick = btnMinimizeClick
    end
    object btnClose: TSpeedButton
      Left = 252
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
  object lblStatusPanel: TPanel
    Left = 0
    Top = 469
    Width = 276
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
    TabOrder = 2
    object lblStatus: TLabel
      Left = 2
      Top = 2
      Width = 272
      Height = 29
      Align = alClient
      Alignment = taCenter
      AutoSize = False
      Caption = 'Click "START" to start the Gateway.'
      Transparent = False
      Layout = tlCenter
      WordWrap = True
    end
  end
  object HttpServer: TRtcHttpServer
    MultiThreaded = True
    Timeout.AfterConnecting = 300
    OnListenLost = HttpServerListenLost
    OnListenError = HttpServerListenError
    FixupRequest.RemovePrefix = True
    MaxRequestSize = 16000
    MaxHeaderSize = 64000
    Left = 16
    Top = 459
  end
  object RtcGateTestProvider: TRtcDataProvider
    Server = HttpServer
    OnCheckRequest = RtcGateTestProviderCheckRequest
    OnDataReceived = RtcGateTestProviderDataReceived
    Left = 96
    Top = 460
  end
  object Gateway: TRtcPortalGateway
    Server = HttpServer
    WriteLog = True
    AutoRegisterUsers = True
    AutoSaveInfo = True
    OnUserLogin = GatewayUserLogin
    OnUserLogout = GatewayUserLogout
    AutoSyncUserEvents = True
    EncryptionKey = 16
    ForceEncryption = True
    AutoSessionsLive = 600
    AutoSessions = True
    ModuleFileName = '/$rdgate'
    Left = 56
    Top = 460
  end
end
