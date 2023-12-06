object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'ONVIF demo'
  ClientHeight = 789
  ClientWidth = 1098
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 584
    Width = 1092
    Height = 13
    Align = alBottom
    Caption = 'Logs'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    WordWrap = True
    ExplicitWidth = 26
  end
  object Panel2: TPanel
    Left = 360
    Top = 41
    Width = 497
    Height = 540
    Align = alRight
    Caption = 'Panel2'
    TabOrder = 0
    object ListView1: TListView
      Left = 1
      Top = 1
      Width = 495
      Height = 538
      Align = alClient
      Columns = <
        item
          Caption = 'Name'
          Width = 300
        end
        item
          Caption = 'Token'
          Width = 200
        end>
      TabOrder = 0
      ViewStyle = vsReport
      OnClick = ListView1Click
    end
  end
  object Memo1: TMemo
    Left = 0
    Top = 600
    Width = 1098
    Height = 189
    Align = alBottom
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1098
    Height = 41
    Align = alTop
    Caption = 'Panel1'
    ShowCaption = False
    TabOrder = 2
    object EUrl: TLabeledEdit
      Left = 83
      Top = 10
      Width = 150
      Height = 21
      EditLabel.Width = 69
      EditLabel.Height = 13
      EditLabel.Caption = 'Camera URL:  '
      LabelPosition = lpLeft
      TabOrder = 0
    end
    object Button1: TButton
      Left = 737
      Top = 10
      Width = 105
      Height = 25
      Caption = 'Get device'
      TabOrder = 1
      OnClick = Button1Click
    end
    object EUser: TLabeledEdit
      Left = 401
      Top = 11
      Width = 121
      Height = 21
      EditLabel.Width = 32
      EditLabel.Height = 13
      EditLabel.Caption = 'User:  '
      LabelPosition = lpLeft
      TabOrder = 2
    end
    object Epwd: TLabeledEdit
      Left = 591
      Top = 12
      Width = 121
      Height = 21
      EditLabel.Width = 60
      EditLabel.Height = 13
      EditLabel.Caption = 'Passwordr:  '
      LabelPosition = lpLeft
      TabOrder = 3
    end
    object Button3: TButton
      Left = 239
      Top = 8
      Width = 105
      Height = 25
      Caption = 'Change IP'
      TabOrder = 4
      OnClick = Button3Click
    end
  end
  object tv1: TTreeView
    Left = 0
    Top = 41
    Width = 360
    Height = 540
    Align = alClient
    Indent = 19
    TabOrder = 3
  end
  object pnlPTZ: TPanel
    Left = 857
    Top = 41
    Width = 241
    Height = 540
    Align = alRight
    Enabled = False
    TabOrder = 4
    object Label2: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 233
      Height = 13
      Align = alTop
      Caption = 'Current token'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      WordWrap = True
      ExplicitWidth = 79
    end
    object Panel4: TPanel
      Left = 1
      Top = 230
      Width = 239
      Height = 50
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object Label4: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 233
        Height = 13
        Align = alTop
        Caption = 'Zoom'
        ExplicitWidth = 26
      end
      object GridPanel1: TGridPanel
        AlignWithMargins = True
        Left = 3
        Top = 22
        Width = 233
        Height = 25
        Align = alClient
        BevelOuter = bvNone
        ColumnCollection = <
          item
            Value = 50.000000000000000000
          end
          item
            Value = 50.000000000000000000
          end>
        ControlCollection = <
          item
            Column = 0
            Control = btnPTZZoomOut
            Row = 0
          end
          item
            Column = 1
            Control = btnPTZZoomIn
            Row = 0
          end>
        RowCollection = <
          item
            Value = 100.000000000000000000
          end>
        TabOrder = 0
        DesignSize = (
          233
          25)
        object btnPTZZoomOut: TButton
          Left = 20
          Top = 0
          Width = 75
          Height = 25
          Anchors = []
          Caption = '+'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 0
          OnMouseDown = btnPTZZoomOutMouseDown
          OnMouseUp = btnPTZPanRightMouseUp
        end
        object btnPTZZoomIn: TButton
          Left = 137
          Top = 0
          Width = 75
          Height = 25
          Anchors = []
          Caption = '-'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 1
          OnMouseDown = btnPTZZoomInMouseDown
          OnMouseUp = btnPTZPanRightMouseUp
        end
      end
    end
    object ECurrentToken: TEdit
      AlignWithMargins = True
      Left = 4
      Top = 23
      Width = 233
      Height = 21
      Align = alTop
      TabOrder = 1
    end
    object Panel5: TPanel
      Left = 1
      Top = 47
      Width = 239
      Height = 152
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 2
      object Label6: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 233
        Height = 13
        Align = alTop
        Caption = 'pan/tilt:'
        ExplicitWidth = 38
      end
      object GridPanel2: TGridPanel
        AlignWithMargins = True
        Left = 3
        Top = 22
        Width = 233
        Height = 127
        Align = alClient
        BevelOuter = bvNone
        ColumnCollection = <
          item
            Value = 33.168569118911340000
          end
          item
            Value = 33.457633346622110000
          end
          item
            Value = 33.373797534466550000
          end>
        ControlCollection = <
          item
            Column = 1
            Control = Button12
            Row = 0
          end
          item
            Column = 0
            Control = Button15
            Row = 1
          end
          item
            Column = 1
            Control = BGoToHome
            Row = 1
          end
          item
            Column = 2
            Control = Button18
            Row = 1
          end
          item
            Column = 1
            Control = Button19
            Row = 2
          end>
        RowCollection = <
          item
            Value = 33.607421173338910000
          end
          item
            Value = 33.287701667411970000
          end
          item
            Value = 33.104877159249110000
          end>
        TabOrder = 0
        object Button12: TButton
          Left = 77
          Top = 0
          Width = 78
          Height = 43
          Align = alClient
          Cancel = True
          Caption = '^'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 0
          OnMouseDown = btnPTZTiltUpMouseDown
          OnMouseUp = btnPTZPanRightMouseUp
        end
        object Button15: TButton
          Left = 0
          Top = 43
          Width = 77
          Height = 42
          Align = alClient
          Cancel = True
          Caption = '<'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 1
          OnMouseDown = btnPTZPanLeftMouseDown
          OnMouseUp = btnPTZPanRightMouseUp
        end
        object BGoToHome: TButton
          AlignWithMargins = True
          Left = 80
          Top = 46
          Width = 72
          Height = 36
          Align = alClient
          Caption = 'Go to home'
          TabOrder = 2
          OnClick = BGoToHomeClick
        end
        object Button18: TButton
          Left = 155
          Top = 43
          Width = 78
          Height = 42
          Align = alClient
          Cancel = True
          Caption = '>'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 3
          OnMouseDown = btnPTZPanRightMouseDown
          OnMouseUp = btnPTZPanRightMouseUp
        end
        object Button19: TButton
          Left = 77
          Top = 85
          Width = 78
          Height = 42
          Align = alClient
          Cancel = True
          Caption = 'v'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 4
          OnMouseDown = btnPTZTiltDownMouseDown
          OnMouseUp = btnPTZPanRightMouseUp
        end
      end
    end
    object pFocus: TPanel
      Left = 1
      Top = 280
      Width = 239
      Height = 50
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 3
      object Label5: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 233
        Height = 13
        Align = alTop
        Caption = 'Focus:'
        ExplicitWidth = 32
      end
      object GridPanel3: TGridPanel
        AlignWithMargins = True
        Left = 3
        Top = 22
        Width = 233
        Height = 25
        Align = alClient
        BevelOuter = bvNone
        ColumnCollection = <
          item
            Value = 50.000000000000000000
          end
          item
            Value = 50.000000000000000000
          end>
        ControlCollection = <
          item
            Column = 0
            Control = Button10
            Row = 0
          end
          item
            Column = 1
            Control = Button9
            Row = 0
          end>
        RowCollection = <
          item
            Value = 100.000000000000000000
          end>
        TabOrder = 0
        DesignSize = (
          233
          25)
        object Button10: TButton
          Left = 20
          Top = 0
          Width = 75
          Height = 25
          Anchors = []
          Caption = '+'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 0
          OnMouseDown = Button10MouseDown
          OnMouseUp = Button10MouseUp
        end
        object Button9: TButton
          Left = 137
          Top = 0
          Width = 75
          Height = 25
          Anchors = []
          Caption = '-'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 1
          OnMouseDown = Button9MouseDown
          OnMouseUp = Button10MouseUp
        end
      end
    end
    object BSetHome: TButton
      AlignWithMargins = True
      Left = 4
      Top = 202
      Width = 233
      Height = 25
      Align = alTop
      Caption = 'Set home'
      TabOrder = 4
      OnClick = BSetHomeClick
    end
    object Panel3: TPanel
      Left = 1
      Top = 330
      Width = 239
      Height = 84
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 5
      object Label7: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 233
        Height = 13
        Align = alTop
        Caption = 'Preset:'
        ExplicitWidth = 35
      end
      object PnlPreset: TPanel
        Left = 0
        Top = 51
        Width = 239
        Height = 33
        Align = alBottom
        BevelOuter = bvNone
        Enabled = False
        TabOrder = 0
        object BGotoPreset: TButton
          Left = 7
          Top = 2
          Width = 105
          Height = 25
          Caption = 'Goto preset'
          TabOrder = 0
          OnClick = BGotoPresetClick
        end
        object BRemovePreset: TButton
          Left = 119
          Top = 3
          Width = 105
          Height = 25
          Caption = 'Remove preset'
          TabOrder = 1
          OnClick = BRemovePresetClick
        end
      end
      object BLoadPreset: TButton
        Left = 7
        Top = 20
        Width = 105
        Height = 25
        Caption = 'Load preset'
        TabOrder = 1
        OnClick = BLoadPresetClick
      end
      object BAddPreset: TButton
        Left = 119
        Top = 20
        Width = 105
        Height = 25
        Caption = 'Add  preset'
        TabOrder = 2
        OnClick = BAddPresetClick
      end
    end
    object pnlAuxCmd: TPanel
      Left = 1
      Top = 414
      Width = 239
      Height = 91
      Align = alTop
      BevelOuter = bvNone
      Enabled = False
      TabOrder = 6
      object Label3: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 233
        Height = 13
        Align = alTop
        Caption = 'AuxiliaryCommands:'
        ExplicitWidth = 97
      end
      object cbAuxCmd: TComboBox
        Left = 5
        Top = 32
        Width = 100
        Height = 21
        TabOrder = 0
        OnSelect = cbAuxCmdSelect
      end
      object cbAuxValue: TComboBox
        Left = 111
        Top = 32
        Width = 100
        Height = 21
        TabOrder = 1
      end
      object BSendAuxCmd: TButton
        Left = 5
        Top = 59
        Width = 105
        Height = 25
        Caption = 'Send'
        TabOrder = 2
        OnClick = BSendAuxCmdClick
      end
    end
  end
end
