object CodeTemplateInsight: TCodeTemplateInsight
  Left = 100
  Top = 100
  AlphaBlend = True
  AlphaBlendValue = 128
  Caption = 'CodeTemplateInsight'
  ClientHeight = 190
  ClientWidth = 304
  Color = clAqua
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnClick = FormClick
  OnDblClick = FormClick
  OnDestroy = FormDestroy
  OnDeactivate = FormDeactivate
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 7
    Top = 38
    Width = 290
    Height = 40
    AutoSize = False
    Caption = 
      'Press Spacebar or Enter to insert the following template, otherw' +
      'ise, press Escape to cancel the insertion and continue entering ' +
      'text.'
    WordWrap = True
    OnClick = FormClick
    OnDblClick = FormClick
  end
  object Label2: TLabel
    Left = 8
    Top = 83
    Width = 289
    Height = 102
    AutoSize = False
    Caption = 'Label2'
    WordWrap = True
    OnClick = FormClick
    OnDblClick = FormClick
  end
  object Label3: TLabel
    Left = 1
    Top = 12
    Width = 302
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'Chee Wee'#39's Code Template Insight'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
end
