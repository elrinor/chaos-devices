object Form1: TForm1
  Left = 254
  Top = 134
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'ADOM Launcher'
  ClientHeight = 219
  ClientWidth = 322
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 0
    Top = 0
    Width = 152
    Height = 13
    Caption = 'Укажите директорию ADOM'#39'a'
  end
  object Label2: TLabel
    Left = 168
    Top = 0
    Width = 100
    Height = 13
    Caption = 'Выберите ваш сейв'
  end
  object Label3: TLabel
    Left = 168
    Top = 136
    Width = 153
    Height = 17
    AutoSize = False
    Caption = 'Ваш персонаж:'
  end
  object DirectoryListBox1: TDirectoryListBox
    Left = 0
    Top = 16
    Width = 153
    Height = 177
    ItemHeight = 16
    TabOrder = 0
    OnChange = DirectoryListBox1Change
  end
  object DriveComboBox1: TDriveComboBox
    Left = 0
    Top = 200
    Width = 153
    Height = 19
    TabOrder = 1
    OnChange = DriveComboBox1Change
  end
  object FileListBox1: TFileListBox
    Left = 168
    Top = 16
    Width = 153
    Height = 113
    ItemHeight = 13
    Mask = '*.svg'
    TabOrder = 2
    OnClick = FileListBox1Click
  end
  object Button1: TButton
    Left = 168
    Top = 192
    Width = 153
    Height = 25
    Caption = 'Запустить ADOM'
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 168
    Top = 160
    Width = 153
    Height = 25
    Caption = 'Сделать резевную копию'
    TabOrder = 4
    OnClick = Button2Click
  end
end
