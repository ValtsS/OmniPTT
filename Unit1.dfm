object Form1: TForm1
  Left = 480
  Top = 166
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsToolWindow
  Caption = 'OmniRig demo'
  ClientHeight = 146
  ClientWidth = 205
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
    Width = 157
    Height = 146
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object ErrTxt: TLabel
      Left = 0
      Top = 0
      Width = 157
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
    Left = 157
    Top = 0
    Width = 48
    Height = 146
    Align = alRight
    BevelOuter = bvNone
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
end
