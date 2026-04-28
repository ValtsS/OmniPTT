object Form1: TForm1
  Left = 645
  Top = 203
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsToolWindow
  Caption = 'OmniRig demo'
  ClientHeight = 146
  ClientWidth = 217
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poDefault
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 124
    Height = 146
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    OnClick = Panel1Click
    object ErrTxt: TLabel
      Left = 0
      Top = 0
      Width = 124
      Height = 13
      Align = alTop
    end
    object HotCatcher1: THotCatcher
      Left = 24
      Top = 56
      Width = 20
      Height = 20
      OnHotkey = HotCatcher1Hotkey
      Color = clRed
    end
  end
  object Panel2: TPanel
    Left = 124
    Top = 0
    Width = 93
    Height = 146
    Align = alRight
    BevelOuter = bvNone
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    PopupMenu = PopupMenu1
    TabOrder = 1
    OnClick = Panel2Click
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 55
    OnTimer = Timer1Timer
    Left = 56
    Top = 48
  end
  object SlowTimer: TTimer
    Interval = 200
    OnTimer = SlowTimerTimer
    Left = 96
    Top = 64
  end
  object Misc: TTimer
    OnTimer = MiscTimer
    Left = 128
    Top = 64
  end
  object PopupMenu1: TPopupMenu
    Left = 96
    Top = 24
    object BoG451: TMenuItem
      Tag = 6
      Caption = 'BoG4'
      OnClick = N11Click
    end
    object BoG31: TMenuItem
      Tag = 5
      Caption = 'BoG3'
      OnClick = N11Click
    end
    object BoG21: TMenuItem
      Tag = 4
      Caption = 'BoG2'
      OnClick = N11Click
    end
    object BoG11: TMenuItem
      Tag = 3
      Caption = 'BoG1'
      OnClick = N11Click
    end
    object N21: TMenuItem
      Tag = 2
      Caption = 'LoG'
      OnClick = N11Click
    end
    object N11: TMenuItem
      Tag = 1
      Caption = 'Real-3dB'
      OnClick = N11Click
    end
    object Real1: TMenuItem
      Caption = 'Real'
      OnClick = N11Click
    end
  end
end
