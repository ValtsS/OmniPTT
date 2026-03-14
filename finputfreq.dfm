object FreqInputForm: TFreqInputForm
  Left = 1180
  Top = 583
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'FreqInputForm'
  ClientHeight = 56
  ClientWidth = 219
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object EditFreq: TEdit
    Left = 8
    Top = 8
    Width = 201
    Height = 32
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnKeyPress = EditFreqKeyPress
  end
end
