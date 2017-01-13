object Form1: TForm1
  Left = 0
  Top = 0
  ActiveControl = lbKnownPackages
  Caption = 'James (Delphi Project Manager)'
  ClientHeight = 576
  ClientWidth = 780
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    780
    576)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 60
    Width = 80
    Height = 13
    Caption = 'Known Packages'
  end
  object Label2: TLabel
    Left = 266
    Top = 60
    Width = 58
    Height = 13
    Caption = 'Library Path'
  end
  object Label3: TLabel
    Left = 8
    Top = 11
    Width = 67
    Height = 13
    Caption = 'Delphi Version'
  end
  object Label4: TLabel
    Left = 524
    Top = 60
    Width = 106
    Height = 13
    Caption = 'Environment Variables'
  end
  object lbKnownPackages: TListBox
    Left = 8
    Top = 76
    Width = 250
    Height = 196
    ItemHeight = 13
    TabOrder = 0
  end
  object lbLibraryPath: TListBox
    Left = 266
    Top = 76
    Width = 250
    Height = 196
    ItemHeight = 13
    TabOrder = 1
  end
  object cbDelphiVersion: TComboBox
    Left = 8
    Top = 27
    Width = 145
    Height = 21
    Style = csDropDownList
    TabOrder = 2
    OnSelect = cbDelphiVersionSelect
  end
  object btLoadFromRegistry: TButton
    Left = 184
    Top = 25
    Width = 121
    Height = 25
    Caption = 'Load from Registry'
    TabOrder = 3
    OnClick = btLoadFromRegistryClick
  end
  object btSaveToRegistry: TButton
    Left = 311
    Top = 25
    Width = 121
    Height = 25
    Caption = 'Save to Registry'
    TabOrder = 4
    OnClick = btSaveToRegistryClick
  end
  object SynEdit1: TSynEdit
    Left = 8
    Top = 312
    Width = 764
    Height = 256
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 5
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Highlighter = SynJSONSyn1
    Lines.Strings = (
      '{'
      '    "version": "5",'
      '    "known_packages": ['
      
        '        "C:\\Users\\alefr\\Documents\\Delphi\\SynEdit\\Bin\\SynE' +
        'dit_D101B.bpl",'
      '        "$(BDSBIN)\\samplevisualizers240.bpl"'
      '    ],'
      '    "library_path": ['
      '        "$(BDSLIB)\\$(Platform)\\release",'
      '        "$(BDSUSERDIR)\\Imports",'
      '        "$(BDS)\\Imports",'
      '        "$(BDSCOMMONDIR)\\Dcp",'
      '        "$(BDS)\\include",'
      '        "C:\\Users\\alefr\\Documents\\Delphi\\SynEdit\\Lib"'
      '    ]'
      '}')
    FontSmoothing = fsmNone
  end
  object lbEnvironmentVariables: TListBox
    Left = 524
    Top = 76
    Width = 250
    Height = 196
    ItemHeight = 13
    TabOrder = 6
  end
  object btLoadFromFile: TButton
    Left = 8
    Top = 281
    Width = 121
    Height = 25
    Caption = 'Load from File'
    TabOrder = 7
    OnClick = btLoadFromFileClick
  end
  object btSaveToFile: TButton
    Left = 135
    Top = 281
    Width = 121
    Height = 25
    Caption = 'Save to File'
    TabOrder = 8
    OnClick = btSaveToFileClick
  end
  object SynJSONSyn1: TSynJSONSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    Left = 248
    Top = 296
  end
end
