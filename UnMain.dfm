object fmFF_RDBMS_Converter: TfmFF_RDBMS_Converter
  Left = 179
  Top = 230
  Width = 546
  Height = 412
  Caption = 'FlashFiler to RDBMS Data Converter'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefaultPosOnly
  Scaled = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pOkCancel: TPanel
    Left = 0
    Top = 348
    Width = 538
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    BorderWidth = 4
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 0
    DesignSize = (
      538
      33)
    object bPrior: TButton
      Left = 370
      Top = 4
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Convert'
      TabOrder = 2
      OnClick = bPriorClick
    end
    object bNext: TButton
      Left = 448
      Top = 4
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Cancel'
      TabOrder = 0
      OnClick = bNextClick
    end
    object bCancel: TButton
      Left = 292
      Top = 4
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Cancel'
      TabOrder = 1
      OnClick = bCancelClick
    end
  end
  object pc: TPageControl
    Left = 0
    Top = 0
    Width = 538
    Height = 348
    ActivePage = tshReviewFF
    Align = alClient
    TabOrder = 1
    object tsIntroduction: TTabSheet
      Caption = 'tsIntroduction'
      DesignSize = (
        530
        320)
      object Label1: TLabel
        Left = 32
        Top = 32
        Width = 367
        Height = 13
        Caption = 
          'The software purpose is to convert an entire FlashFiler database' +
          ' into RDBMS.'
      end
      object Label10: TLabel
        Left = 32
        Top = 96
        Width = 125
        Height = 13
        Caption = 'Destination RDBMS Type:'
      end
      object Label11: TLabel
        Left = 32
        Top = 64
        Width = 113
        Height = 13
        Caption = 'Source Database Type:'
      end
      object cbRDBMSType: TComboBox
        Left = 162
        Top = 93
        Width = 241
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        Ctl3D = False
        ItemHeight = 13
        ParentCtl3D = False
        TabOrder = 0
      end
      object Edit1: TEdit
        Left = 162
        Top = 60
        Width = 241
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        Enabled = False
        TabOrder = 1
        Text = 'FlashFiler'
      end
    end
    object tsSelectFF: TTabSheet
      Caption = 'tsSelectFF'
      ImageIndex = 1
      object tvFF: TShellTreeView
        Left = 0
        Top = 21
        Width = 530
        Height = 299
        ObjectTypes = [otFolders]
        Root = 'rfMyComputer'
        UseShellImages = True
        Align = alClient
        AutoRefresh = False
        Ctl3D = False
        Indent = 19
        ParentColor = False
        ParentCtl3D = False
        RightClickSelect = True
        ShowRoot = False
        TabOrder = 0
      end
      object Panel3: TPanel
        Left = 0
        Top = 0
        Width = 530
        Height = 21
        Align = alTop
        AutoSize = True
        BevelOuter = bvNone
        BorderWidth = 4
        TabOrder = 1
        object Label3: TLabel
          Left = 4
          Top = 4
          Width = 522
          Height = 13
          Align = alTop
          Caption = 'Select folder of FlashFiler database (only read)'
        end
      end
    end
    object tshReviewFF: TTabSheet
      Caption = 'tshReviewFF'
      ImageIndex = 2
      object Panel4: TPanel
        Left = 0
        Top = 0
        Width = 530
        Height = 48
        Align = alTop
        AutoSize = True
        BevelOuter = bvNone
        BorderWidth = 4
        TabOrder = 0
        DesignSize = (
          530
          48)
        object Label4: TLabel
          Left = 4
          Top = 8
          Width = 144
          Height = 13
          Caption = 'Review schema (FF database)'
        end
        object lRowsCount: TLabel
          Left = 520
          Top = 4
          Width = 6
          Height = 13
          Alignment = taRightJustify
          Anchors = [akTop, akRight]
          Caption = '0'
        end
        object cbUseNonUnicodeTypes: TCheckBox
          Left = 3
          Top = 27
          Width = 185
          Height = 17
          Caption = 'Use non-unicode types'
          TabOrder = 0
          Visible = False
          OnClick = cbUseNonUnicodeTypesClick
        end
        object cbUseIdentity: TCheckBox
          Left = 190
          Top = 27
          Width = 249
          Height = 17
          Caption = 'Use identity fields'
          TabOrder = 1
          Visible = False
          OnClick = cbUseNonUnicodeTypesClick
        end
      end
      object sgGrid: TStringGrid
        Left = 0
        Top = 48
        Width = 530
        Height = 272
        Align = alClient
        ColCount = 4
        Ctl3D = False
        DefaultRowHeight = 21
        FixedCols = 0
        RowCount = 2
        ParentCtl3D = False
        TabOrder = 1
      end
    end
    object tshSelectMSSQL: TTabSheet
      Caption = 'tshSelectMSSQL'
      ImageIndex = 3
      object Label2: TLabel
        Left = 172
        Top = 227
        Width = 3
        Height = 13
      end
      object Panel5: TPanel
        Left = 0
        Top = 0
        Width = 530
        Height = 21
        Align = alTop
        AutoSize = True
        BevelOuter = bvNone
        BorderWidth = 4
        TabOrder = 0
        object Label9: TLabel
          Left = 4
          Top = 4
          Width = 49
          Height = 13
          Align = alTop
          Caption = 'Connect ()'
        end
      end
      object Panel7: TPanel
        Left = 0
        Top = 21
        Width = 530
        Height = 107
        Align = alTop
        AutoSize = True
        BevelOuter = bvNone
        BorderWidth = 4
        TabOrder = 1
        DesignSize = (
          530
          107)
        object sbConnectionStringEdit: TSpeedButton
          Left = 492
          Top = 81
          Width = 23
          Height = 22
          Anchors = [akTop, akRight]
          Caption = '...'
          Enabled = False
          OnClick = sbConnectionStringEditClick
        end
        object rbStandard: TRadioButton
          Left = 7
          Top = 14
          Width = 113
          Height = 17
          Caption = 'Standard security'
          Checked = True
          TabOrder = 0
          TabStop = True
          OnClick = rbStandardClick
        end
        object chbRDBMS_Trusted: TCheckBox
          Left = 349
          Top = 55
          Width = 138
          Height = 17
          Anchors = [akTop, akRight]
          Caption = 'Trusted'
          TabOrder = 7
          Visible = False
          OnClick = chbRDBMS_TrustedClick
        end
        object rbConnectionString: TRadioButton
          Left = 6
          Top = 84
          Width = 113
          Height = 17
          Caption = 'Connection string'
          TabOrder = 1
          OnClick = rbStandardClick
        end
        object edConnectionString: TEdit
          Left = 126
          Top = 82
          Width = 362
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          Enabled = False
          TabOrder = 8
        end
        object edRDBMS_Host: TLabeledEdit
          Left = 176
          Top = 4
          Width = 158
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          EditLabel.Width = 25
          EditLabel.Height = 13
          EditLabel.Caption = 'Host:'
          EditLabel.Layout = tlCenter
          LabelPosition = lpLeft
          LabelSpacing = 11
          TabOrder = 2
        end
        object edRDBMS_DB: TLabeledEdit
          Left = 176
          Top = 27
          Width = 158
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          EditLabel.Width = 18
          EditLabel.Height = 13
          EditLabel.Caption = 'DB:'
          EditLabel.Layout = tlCenter
          LabelPosition = lpLeft
          LabelSpacing = 18
          TabOrder = 4
        end
        object edRDBMS_UserName: TLabeledEdit
          Left = 405
          Top = 4
          Width = 112
          Height = 21
          Anchors = [akTop, akRight]
          EditLabel.Width = 25
          EditLabel.Height = 13
          EditLabel.Caption = 'User:'
          EditLabel.Layout = tlCenter
          LabelPosition = lpLeft
          LabelSpacing = 30
          TabOrder = 5
        end
        object edRDBMS_Password: TLabeledEdit
          Left = 405
          Top = 27
          Width = 112
          Height = 21
          Anchors = [akTop, akRight]
          EditLabel.Width = 49
          EditLabel.Height = 13
          EditLabel.Caption = 'Password:'
          EditLabel.Layout = tlCenter
          LabelPosition = lpLeft
          LabelSpacing = 7
          PasswordChar = '*'
          TabOrder = 6
        end
        object edRDBMS_Port: TLabeledEdit
          Left = 176
          Top = 51
          Width = 158
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          EditLabel.Width = 22
          EditLabel.Height = 13
          EditLabel.Caption = 'Port:'
          EditLabel.Layout = tlCenter
          LabelPosition = lpLeft
          LabelSpacing = 14
          TabOrder = 3
          Visible = False
        end
      end
    end
    object tshConfirm: TTabSheet
      Caption = 'tshConfirm'
      ImageIndex = 4
      DesignSize = (
        530
        320)
      object Label6: TLabel
        Left = 16
        Top = 24
        Width = 408
        Height = 26
        Caption = 
          'Warning!'#13#10'If RDBMS Server has tables with the same names, they w' +
          'ill be dropped.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label7: TLabel
        Left = 172
        Top = 87
        Width = 54
        Height = 13
        Caption = '0 = All rows'
      end
      object Label8: TLabel
        Left = 27
        Top = 87
        Width = 45
        Height = 13
        Caption = 'Row limit:'
      end
      object SpeedButton2: TSpeedButton
        Left = 444
        Top = 107
        Width = 23
        Height = 22
        Anchors = [akTop, akRight]
        Caption = '...'
        OnClick = SpeedButton2Click
      end
      object Panel8: TPanel
        Left = 0
        Top = 0
        Width = 530
        Height = 21
        Align = alTop
        AutoSize = True
        BevelOuter = bvNone
        BorderWidth = 4
        TabOrder = 0
        object Label5: TLabel
          Left = 4
          Top = 4
          Width = 64
          Height = 13
          Align = alTop
          Caption = 'Confirm \ Log'
        end
      end
      object seRowLimit: TSpinEdit
        Left = 81
        Top = 84
        Width = 79
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 1
        Value = 0
      end
      object edLogFile: TLabeledEdit
        Left = 80
        Top = 109
        Width = 361
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 37
        EditLabel.Height = 13
        EditLabel.Caption = 'Log file:'
        LabelPosition = lpLeft
        LabelSpacing = 15
        TabOrder = 2
      end
      object cbSkipIndexes: TCheckBox
        Left = 80
        Top = 136
        Width = 257
        Height = 17
        Caption = 'Skip creation of indexes'
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 3
      end
      object cbUseLowerCase: TCheckBox
        Left = 80
        Top = 156
        Width = 257
        Height = 17
        Caption = 'Convert all identifiers to lowercase'
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 4
      end
    end
    object tshProgress: TTabSheet
      Caption = 'tshProgress'
      ImageIndex = 5
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 530
        Height = 50
        Align = alTop
        AutoSize = True
        BevelOuter = bvNone
        BorderWidth = 8
        TabOrder = 0
        object pbValue0: TProgressBar
          Left = 8
          Top = 8
          Width = 514
          Height = 17
          Align = alTop
          Smooth = True
          TabOrder = 0
        end
        object pbValue1: TProgressBar
          Left = 8
          Top = 25
          Width = 514
          Height = 17
          Align = alTop
          Smooth = True
          TabOrder = 1
        end
      end
      object mLog: TMemo
        Left = 0
        Top = 50
        Width = 530
        Height = 270
        Align = alClient
        Ctl3D = False
        ParentCtl3D = False
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 1
      end
    end
    object tshResult: TTabSheet
      Caption = 'tshResult'
      ImageIndex = 6
      object Label12: TLabel
        Left = 0
        Top = 89
        Width = 530
        Height = 13
        Align = alTop
        Caption = 'Log:'
      end
      object mResult: TMemo
        Left = 0
        Top = 102
        Width = 530
        Height = 218
        Align = alClient
        Ctl3D = False
        ParentCtl3D = False
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 530
        Height = 89
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        DesignSize = (
          530
          89)
        object lResultLeft: TLabel
          Left = 0
          Top = 0
          Width = 5
          Height = 89
          Align = alLeft
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object pResult: TPanel
          Left = 194
          Top = 0
          Width = 141
          Height = 89
          Anchors = [akTop, akBottom]
          AutoSize = True
          BevelOuter = bvNone
          TabOrder = 0
          object lResultN: TLabel
            Left = 0
            Top = 0
            Width = 86
            Height = 89
            Align = alLeft
            AutoSize = False
            Caption = 'Data verified:  '
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object lResultV: TLabel
            Left = 86
            Top = 0
            Width = 55
            Height = 89
            Align = alLeft
            Alignment = taRightJustify
            AutoSize = False
            Caption = ' 00:00:00'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
        end
      end
    end
  end
  object XPManifest1: TXPManifest
    Left = 218
    Top = 120
  end
  object OpenDialog1: TOpenDialog
    Left = 264
    Top = 120
  end
end
