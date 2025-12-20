object Form1: TForm1
  Left = 359
  Top = 137
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsToolWindow
  Caption = 'OmniRig demo'
  ClientHeight = 58
  ClientWidth = 120
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
    Width = 72
    Height = 58
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object HotCatcher1: THotCatcher
      Left = 16
      Top = 8
      Width = 20
      Height = 20
      OnHotkey = HotCatcher1Hotkey
      Color = clRed
    end
  end
  object Panel2: TPanel
    Left = 72
    Top = 0
    Width = 48
    Height = 58
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
    Top = 8
  end
  object SlowTimer: TTimer
    Interval = 200
    OnTimer = SlowTimerTimer
    Left = 64
    Top = 32
  end
end
