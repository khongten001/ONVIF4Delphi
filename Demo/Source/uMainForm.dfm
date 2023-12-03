object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'ONVIF demo'
  ClientHeight = 789
  ClientWidth = 1297
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 1297
    Height = 656
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 1188
    object TabSheet1: TTabSheet
      Caption = 'Probe'
      object Label1: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 376
        Width = 1283
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
      object tv1: TTreeView
        Left = 497
        Top = 41
        Width = 792
        Height = 332
        Align = alClient
        Indent = 19
        TabOrder = 0
        ExplicitWidth = 683
      end
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 1289
        Height = 41
        Align = alTop
        Caption = 'Panel1'
        ShowCaption = False
        TabOrder = 1
        ExplicitWidth = 1180
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
      object Memo1: TMemo
        Left = 0
        Top = 392
        Width = 1289
        Height = 236
        Align = alBottom
        Lines.Strings = (
          'Memo1')
        ScrollBars = ssVertical
        TabOrder = 2
        ExplicitWidth = 1180
      end
      object Panel2: TPanel
        Left = 0
        Top = 41
        Width = 497
        Height = 332
        Align = alLeft
        Caption = 'Panel2'
        TabOrder = 3
        object Label2: TLabel
          AlignWithMargins = True
          Left = 4
          Top = 4
          Width = 489
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
        object ListView1: TListView
          Left = 1
          Top = 47
          Width = 495
          Height = 284
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
        object ECurrentToken: TEdit
          AlignWithMargins = True
          Left = 4
          Top = 23
          Width = 489
          Height = 21
          Align = alTop
          TabOrder = 1
        end
      end
    end
  end
  object PageControl2: TPageControl
    Left = 0
    Top = 656
    Width = 1297
    Height = 133
    ActivePage = TabPTZ
    Align = alBottom
    TabOrder = 1
    ExplicitWidth = 1188
    object TabPTZ: TTabSheet
      Caption = 'PTZ'
      Enabled = False
      object Label108: TLabel
        Left = 32
        Top = 6
        Width = 38
        Height = 13
        Caption = 'pan/tilt:'
      end
      object Label109: TLabel
        Left = 154
        Top = 7
        Width = 26
        Height = 13
        Caption = 'Zoom'
      end
      object btnPTZTiltUp: TButton
        Left = 83
        Top = 4
        Width = 21
        Height = 21
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
      object btnPTZPanLeft: TButton
        Left = 60
        Top = 26
        Width = 21
        Height = 21
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
      object btnPTZPanRight: TButton
        Left = 106
        Top = 26
        Width = 21
        Height = 21
        Caption = '>'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 2
        OnMouseDown = btnPTZPanRightMouseDown
        OnMouseUp = btnPTZPanRightMouseUp
      end
      object btnPTZTiltDown: TButton
        Left = 83
        Top = 46
        Width = 21
        Height = 21
        Caption = 'v'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 3
        OnMouseDown = btnPTZTiltDownMouseDown
        OnMouseUp = btnPTZPanRightMouseUp
      end
      object btnPTZZoomOut: TButton
        Left = 154
        Top = 26
        Width = 21
        Height = 21
        Caption = '-'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 4
        OnMouseDown = btnPTZZoomOutMouseDown
        OnMouseUp = btnPTZPanRightMouseUp
      end
      object btnPTZZoomIn: TButton
        Left = 174
        Top = 26
        Width = 21
        Height = 21
        Caption = '+'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 5
        OnMouseDown = btnPTZZoomInMouseDown
        OnMouseUp = btnPTZPanRightMouseUp
      end
      object Button2: TButton
        Left = 369
        Top = 6
        Width = 105
        Height = 25
        Caption = 'Load preset'
        TabOrder = 6
        OnClick = Button2Click
      end
      object Button7: TButton
        Left = 369
        Top = 43
        Width = 105
        Height = 25
        Caption = 'Go to home'
        TabOrder = 7
        OnClick = Button7Click
      end
      object PnlPreset: TPanel
        Left = 608
        Top = 3
        Width = 225
        Height = 33
        Enabled = False
        TabOrder = 8
        object Button4: TButton
          Left = 2
          Top = 3
          Width = 105
          Height = 25
          Caption = 'Goto preset'
          TabOrder = 0
          OnClick = Button4Click
        end
        object Button6: TButton
          Left = 113
          Top = 3
          Width = 105
          Height = 25
          Caption = 'Remove preset'
          TabOrder = 1
          OnClick = Button6Click
        end
      end
      object Button5: TButton
        Left = 497
        Top = 6
        Width = 105
        Height = 25
        Caption = 'Add  preset'
        TabOrder = 9
        OnClick = Button5Click
      end
      object Button8: TButton
        Left = 497
        Top = 43
        Width = 105
        Height = 25
        Caption = 'Set home'
        TabOrder = 10
        OnClick = Button8Click
      end
    end
  end
end
