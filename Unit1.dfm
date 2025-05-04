object Form1: TForm1
  Left = 269
  Top = 114
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'OmniRig demo'
  ClientHeight = 86
  ClientWidth = 120
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 120
    Height = 86
    Align = alClient
    BevelInner = bvLowered
    Caption = 'Panel1'
    TabOrder = 0
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 55
    OnTimer = Timer1Timer
    Left = 32
    Top = 8
  end
end
