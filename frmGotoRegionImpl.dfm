object frmGotoRegion: TfrmGotoRegion
  Left = 0
  Top = 0
  HorzScrollBar.Smooth = True
  VertScrollBar.Smooth = True
  Caption = 'Chee Wee'#39's Goto Region Expert - Select Region to go to...'
  ClientHeight = 231
  ClientWidth = 406
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ListBox1: TListBox
    Left = 8
    Top = 8
    Width = 393
    Height = 185
    ItemHeight = 13
    TabOrder = 0
    OnClick = ListBox1Click
    OnDblClick = ListBox1DblClick
  end
  object cbGoRegionTop: TCheckBox
    Left = 8
    Top = 200
    Width = 105
    Height = 17
    Caption = 'Go Region Top'
    Checked = True
    State = cbChecked
    TabOrder = 1
  end
  object cbExpandRegion: TCheckBox
    Left = 104
    Top = 200
    Width = 97
    Height = 17
    Caption = 'Expand Region'
    Checked = True
    State = cbChecked
    TabOrder = 2
  end
  object btnOk: TButton
    Left = 208
    Top = 200
    Width = 75
    Height = 25
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object btnCancel: TButton
    Left = 304
    Top = 200
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
end
