object MainForm: TMainForm
  Left = 439
  Top = 169
  ActiveControl = eUserName
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsNone
  Caption = 'RTC Control v5'
  ClientHeight = 430
  ClientWidth = 281
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
  PixelsPerInch = 120
  TextHeight = 14
  object pMaster: TPanel
    Left = 0
    Top = 28
    Width = 281
    Height = 369
    Align = alClient
    TabOrder = 0
    object Notebook: TNotebook
      Left = 1
      Top = 1
      Width = 279
      Height = 367
      Align = alClient
      PageIndex = 1
      TabOrder = 1
      object TPage
        Left = 0
        Top = 0
        Caption = 'PageStart'
      end
      object TPage
        Left = 0
        Top = 0
        Caption = 'PageLoggedIn'
      end
    end
    object Pages: TPageControl
      Left = 1
      Top = 1
      Width = 279
      Height = 367
      ActivePage = Page_Setup
      Align = alClient
      TabOrder = 0
      TabStop = False
      object Page_Setup: TTabSheet
        Caption = 'Setup'
        object Label12: TLabel
          Left = 16
          Top = 178
          Width = 45
          Height = 14
          Caption = 'Gateway'
        end
        object Label3: TLabel
          Left = 17
          Top = 212
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
          Left = 16
          Top = 240
          Width = 50
          Height = 14
          Alignment = taRightJustify
          Caption = 'Password'
        end
        object btnGateway: TSpeedButton
          Left = 68
          Top = 172
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
        object Label6: TLabel
          Left = 15
          Top = 268
          Width = 51
          Height = 14
          Alignment = taRightJustify
          Caption = 'Real Name'
        end
        object btnLogin: TButton
          Left = 188
          Top = 288
          Width = 73
          Height = 37
          Caption = 'LOG IN'
          Default = True
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          TabOrder = 5
          OnClick = btnLoginClick
        end
        object eUserName: TEdit
          Left = 72
          Top = 207
          Width = 181
          Height = 22
          TabOrder = 0
          OnChange = eUserNameChange
        end
        object ePassword: TEdit
          Left = 72
          Top = 235
          Width = 125
          Height = 22
          PasswordChar = '*'
          TabOrder = 1
          OnChange = ePasswordChange
        end
        object xSavePassword: TCheckBox
          Left = 200
          Top = 236
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
          Left = 72
          Top = 296
          Width = 97
          Height = 21
          Hint = 
            'Check thix box if you want to be logged in automatically after a' +
            'ny kind of connection problems'
          TabStop = False
          Caption = 'Auto Re-Login'
          TabOrder = 4
        end
        object Panel2: TPanel
          Left = 8
          Top = 8
          Width = 253
          Height = 157
          Cursor = crHandPoint
          BevelInner = bvRaised
          BevelOuter = bvLowered
          Color = clWhite
          TabOrder = 6
          OnClick = RtcCopyrightClick
          object Label7: TLabel
            Left = 4
            Top = 92
            Width = 245
            Height = 15
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
            Top = 24
            Width = 245
            Height = 15
            Cursor = crHandPoint
            Alignment = taCenter
            AutoSize = False
            Caption = 'Created with RTC SDK and RTC Portal for Delphi'
            OnClick = RtcCopyrightClick
          end
          object lCopyright: TLabel
            Left = 4
            Top = 4
            Width = 245
            Height = 17
            Cursor = crHandPoint
            Alignment = taCenter
            AutoSize = False
            Caption = 'RTC Portal Control'
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
          object Label2: TLabel
            Left = 4
            Top = 110
            Width = 245
            Height = 41
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
          Left = 72
          Top = 263
          Width = 181
          Height = 22
          TabOrder = 3
          OnChange = eRealNameChange
        end
      end
      object Page_Control: TTabSheet
        Caption = 'Control'
        ImageIndex = 1
        DesignSize = (
          271
          338)
        object Label5: TLabel
          Left = 32
          Top = 6
          Width = 78
          Height = 14
          Caption = 'Available Hosts:'
        end
        object sStatus1: TShape
          Left = 4
          Top = 4
          Width = 15
          Height = 15
          Brush.Color = clSilver
          Shape = stCircle
        end
        object sStatus2: TShape
          Left = 12
          Top = 4
          Width = 15
          Height = 15
          Brush.Color = clSilver
          Shape = stCircle
        end
        object btnHelp: TLabel
          Left = 120
          Top = 6
          Width = 30
          Height = 14
          Cursor = crHandPoint
          Caption = 'HELP!'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold, fsUnderline]
          ParentFont = False
          OnClick = btnHelpClick
        end
        object btnLogout: TSpeedButton
          Left = 168
          Top = 292
          Width = 93
          Height = 33
          Caption = 'LOG OUT'
          OnClick = btnLogoutClick
        end
        object btnChat: TSpeedButton
          Left = 168
          Top = 32
          Width = 97
          Height = 29
          Caption = 'Chat'
          Enabled = False
          OnClick = btnChatClick
        end
        object btnFileTransfer: TSpeedButton
          Left = 168
          Top = 65
          Width = 97
          Height = 29
          Caption = 'File Transfer'
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          OnClick = btnFileTransferClick
        end
        object btnViewDesktop: TSpeedButton
          Tag = 1
          Left = 168
          Top = 98
          Width = 97
          Height = 29
          Caption = 'View Desktop'
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          OnClick = btnViewDesktopClick
        end
        object cPriority: TComboBox
          Left = 164
          Top = 1
          Width = 105
          Height = 22
          Style = csDropDownList
          ItemHeight = 14
          ItemIndex = 1
          TabOrder = 0
          Text = 'Normal Priority'
          OnChange = cPriorityChange
          Items.Strings = (
            'High Priority'
            'Normal Priority'
            'Low Priority')
        end
        object Panel1: TPanel
          Left = 160
          Top = 136
          Width = 109
          Height = 149
          BevelInner = bvRaised
          BevelOuter = bvLowered
          TabOrder = 1
          object Label1: TLabel
            Left = 4
            Top = 4
            Width = 82
            Height = 14
            Caption = 'Host initialization:'
            Color = clBtnFace
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clGray
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = []
            ParentColor = False
            ParentFont = False
          end
          object Label9: TLabel
            Left = 4
            Top = 56
            Width = 100
            Height = 14
            Caption = 'Host Control options:'
            Color = clBtnFace
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clGray
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = []
            ParentColor = False
            ParentFont = False
          end
          object xKeyMapping: TCheckBox
            Left = 8
            Top = 96
            Width = 77
            Height = 17
            Hint = 'Enable Universal Keyboard Mapping?'
            Caption = 'Map Keys'
            TabOrder = 3
            OnClick = xKeyMappingClick
          end
          object xSmoothView: TCheckBox
            Left = 8
            Top = 128
            Width = 85
            Height = 17
            Hint = 'Use Anti-aliasing when scaling down?'
            Caption = 'Smooth View'
            TabOrder = 5
            OnClick = xSmoothViewClick
          end
          object xForceCursor: TCheckBox
            Left = 8
            Top = 112
            Width = 85
            Height = 17
            Hint = 'Paint remote cursor when not standard?'
            Caption = 'Exact Cursor'
            TabOrder = 4
            OnClick = xForceCursorClick
          end
          object cbControlMode: TComboBox
            Left = 4
            Top = 72
            Width = 101
            Height = 22
            Hint = 'How do you want to control the Desktop?'
            Style = csDropDownList
            ItemHeight = 14
            ItemIndex = 1
            TabOrder = 2
            Text = 'Support mode'
            OnChange = cbControlModeChange
            Items.Strings = (
              'View mode'
              'Support mode'
              'Teach mode'
              'Admin mode')
          end
          object xHideWallpaper: TCheckBox
            Left = 8
            Top = 20
            Width = 97
            Height = 17
            Hint = 'Always Hide Desktop Wallpaper'
            Caption = 'Hide Wallpaper'
            TabOrder = 0
            OnClick = xSmoothViewClick
          end
          object xReduceColors: TCheckBox
            Left = 8
            Top = 36
            Width = 97
            Height = 17
            Hint = 'Start with 256 Colors'
            Caption = 'Use 256 colors'
            TabOrder = 1
            OnClick = xSmoothViewClick
          end
        end
        object Panel3: TPanel
          Left = 0
          Top = 19
          Width = 157
          Height = 315
          Anchors = [akLeft, akTop, akBottom]
          BevelOuter = bvNone
          TabOrder = 2
          object myDesktopPanel: TPanel
            Left = 0
            Top = 238
            Width = 157
            Height = 77
            Align = alBottom
            BevelOuter = bvNone
            TabOrder = 0
            DesignSize = (
              157
              77)
            object btnCloseMyDesktop: TSpeedButton
              Left = 4
              Top = 52
              Width = 149
              Height = 24
              Anchors = [akLeft, akRight, akBottom]
              Caption = 'Stop Desk View'
              Enabled = False
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'Arial'
              Font.Style = []
              ParentFont = False
              OnClick = btnCloseMyDesktopClick
            end
            object eConnected: TListView
              Left = 4
              Top = 3
              Width = 149
              Height = 50
              Anchors = [akLeft, akTop, akRight, akBottom]
              Color = clBtnFace
              Columns = <>
              Enabled = False
              IconOptions.Arrangement = iaLeft
              IconOptions.AutoArrange = True
              ReadOnly = True
              RowSelect = True
              SortType = stText
              TabOrder = 0
              ViewStyle = vsSmallIcon
              OnDblClick = eConnectedDblClick
            end
          end
          object Panel5: TPanel
            Left = 0
            Top = 0
            Width = 157
            Height = 238
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 1
            DesignSize = (
              157
              238)
            object btnShowMyDesktop: TSpeedButton
              Left = 4
              Top = 210
              Width = 149
              Height = 25
              Anchors = [akLeft, akRight, akBottom]
              Caption = 'Desktop to Host'
              Enabled = False
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'Arial'
              Font.Style = []
              ParentFont = False
              Transparent = False
              OnClick = btnShowMyDesktopClick
            end
            object eUsers: TVirtualStringTree
              Left = 4
              Top = 4
              Width = 149
              Height = 205
              Anchors = [akLeft, akTop, akRight, akBottom]
              BorderStyle = bsNone
              Header.AutoSizeIndex = 0
              Header.Options = [hoAutoResize, hoColumnResize, hoVisible]
              TabOrder = 0
              TreeOptions.AnimationOptions = [toAnimatedToggle]
              TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toGridExtensions, toInitOnSave]
              TreeOptions.SelectionOptions = [toExtendedFocus, toRightClickSelect]
              OnClick = eUsersClick
              OnDblClick = eUsersDblClick
              OnFreeNode = eUsersFreeNode
              OnGetText = eUsersGetText
            end
          end
        end
        object xWithExplorer: TCheckBox
          Left = 170
          Top = 67
          Width = 13
          Height = 13
          Hint = 'with Remote File Explorer'
          TabStop = False
          TabOrder = 3
          OnClick = xWithExplorerClick
        end
      end
    end
  end
  object pTitlebar: TPanel
    Left = 0
    Top = 0
    Width = 281
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
      Width = 279
      Height = 26
      Align = alClient
      Caption = '  RTC Portal Control v5'
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
      Left = 234
      Top = 4
      Width = 23
      Height = 21
      Caption = '--'
      OnClick = btnMinimizeClick
    end
    object btnClose: TSpeedButton
      Left = 257
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
    Top = 397
    Width = 281
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
      Width = 277
      Height = 29
      Align = alClient
      Alignment = taCenter
      AutoSize = False
      Caption = 'Click "LOG IN" to connect to the Gateway.'
      Transparent = False
      Layout = tlCenter
      WordWrap = True
      OnMouseDown = lblStatusMouseDown
      OnMouseMove = lblStatusMouseMove
      OnMouseUp = lblStatusMouseUp
    end
  end
  object PClient: TRtcHttpPortalClient
    UserNotify = True
    OnLogIn = PClientLogIn
    OnLogOut = PClientLogOut
    OnStart = PClientStart
    OnError = PClientError
    OnFatalError = PClientFatalError
    OnUserLoggedIn = PClientUserLoggedIn
    OnUserLoggedOut = PClientUserLoggedOut
    AutoSyncEvents = True
    DataEncrypt = 16
    DataForceEncrypt = True
    RetryOtherCalls = 5
    MultiThreaded = True
    Gate_Timeout = 300
    OnStatusGet = PClientStatusGet
    OnStatusPut = PClientStatusPut
    Left = 4
    Top = 392
  end
  object PFileTrans: TRtcPFileTransfer
    GAllowBrowse = False
    GAllowBrowse_Super = False
    OnNewUI = PFileTransNewUI
    Left = 36
    Top = 392
  end
  object PChat: TRtcPChat
    Client = PClient
    OnNewUI = PChatNewUI
    Left = 68
    Top = 392
  end
  object PDesktopControl: TRtcPDesktopControl
    Client = PClient
    OnNewUI = PDesktopControlNewUI
    Left = 100
    Top = 392
  end
  object PDesktopHost: TRtcPDesktopHost
    Client = PClient
    GAllowView = False
    GAllowView_Super = False
    GAllowControl = False
    GAllowControl_Super = False
    FileTransfer = PFileTrans
    OnUserJoined = PDesktopHostUserJoined
    OnUserLeft = PDesktopHostUserLeft
    Left = 132
    Top = 392
  end
end
