object Form1: TForm1
  Left = 407
  Top = 198
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Аварии на АЭС'
  ClientHeight = 401
  ClientWidth = 617
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 424
    Top = 32
    Width = 5
    Height = 13
    Caption = 'x'
  end
  object Label2: TLabel
    Left = 528
    Top = 32
    Width = 5
    Height = 13
    Caption = 'y'
  end
  object Label3: TLabel
    Left = 424
    Top = 64
    Width = 42
    Height = 13
    Caption = 'Реактор'
  end
  object Label4: TLabel
    Left = 464
    Top = 88
    Width = 69
    Height = 13
    Caption = 'Азимут ветра'
  end
  object Label5: TLabel
    Left = 416
    Top = 112
    Width = 113
    Height = 13
    Caption = 'Угол разворота ветра'
  end
  object Label6: TLabel
    Left = 424
    Top = 136
    Width = 108
    Height = 13
    Caption = 'Скорость ветра (м/с)'
  end
  object Label7: TLabel
    Left = 504
    Top = 192
    Width = 28
    Height = 13
    Caption = 'Часы'
  end
  object Label8: TLabel
    Left = 496
    Top = 216
    Width = 39
    Height = 13
    Caption = 'Минуты'
  end
  object Label9: TLabel
    Left = 408
    Top = 160
    Width = 60
    Height = 13
    Caption = 'Облачность'
  end
  object Label10: TLabel
    Left = 416
    Top = 240
    Width = 109
    Height = 13
    Caption = 'Прошедшее время (ч)'
  end
  object Image1: TImage
    Left = 0
    Top = 0
    Width = 401
    Height = 401
  end
  object Label11: TLabel
    Left = 408
    Top = 320
    Width = 153
    Height = 17
    AutoSize = False
    Caption = 'Радиус эвакуации: '
  end
  object Label12: TLabel
    Left = 408
    Top = 344
    Width = 161
    Height = 17
    AutoSize = False
    Caption = 'Радиус укрытия:'
  end
  object Label13: TLabel
    Left = 432
    Top = 0
    Width = 161
    Height = 33
    AutoSize = False
    Caption = 'Координаты АЭС (км от правого-верхнего угла карты)'
    WordWrap = True
  end
  object Label14: TLabel
    Left = 416
    Top = 200
    Width = 72
    Height = 13
    Caption = 'Время аварии'
  end
  object SpinEdit1: TSpinEdit
    Left = 432
    Top = 32
    Width = 81
    Height = 22
    MaxValue = 500
    MinValue = 1
    TabOrder = 0
    Value = 1
  end
  object SpinEdit2: TSpinEdit
    Left = 536
    Top = 32
    Width = 81
    Height = 22
    MaxValue = 500
    MinValue = 1
    TabOrder = 1
    Value = 1
  end
  object ComboBox1: TComboBox
    Left = 472
    Top = 64
    Width = 145
    Height = 21
    Style = csDropDownList
    Ctl3D = True
    ItemHeight = 13
    ParentCtl3D = False
    TabOrder = 2
    Items.Strings = (
      'ВВЭР-1000, БН-350, БН-600'
      'ВВЭР-440 (проект 230)')
  end
  object SpinEdit3: TSpinEdit
    Left = 536
    Top = 88
    Width = 81
    Height = 22
    MaxValue = 359
    MinValue = 0
    TabOrder = 3
    Value = 0
  end
  object SpinEdit4: TSpinEdit
    Left = 536
    Top = 112
    Width = 81
    Height = 22
    MaxValue = 359
    MinValue = 0
    TabOrder = 4
    Value = 0
  end
  object SpinEdit5: TSpinEdit
    Left = 536
    Top = 136
    Width = 81
    Height = 22
    MaxValue = 200
    MinValue = 0
    TabOrder = 5
    Value = 0
  end
  object SpinEdit6: TSpinEdit
    Left = 536
    Top = 192
    Width = 81
    Height = 22
    MaxValue = 23
    MinValue = 0
    TabOrder = 6
    Value = 0
  end
  object SpinEdit7: TSpinEdit
    Left = 536
    Top = 216
    Width = 81
    Height = 22
    MaxValue = 59
    MinValue = 0
    TabOrder = 7
    Value = 0
  end
  object Button1: TButton
    Left = 408
    Top = 264
    Width = 209
    Height = 33
    Caption = 'Считать!'
    TabOrder = 8
    OnClick = Button1Click
  end
  object ComboBox2: TComboBox
    Left = 472
    Top = 160
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 9
    Items.Strings = (
      'Ясно'
      'Переменная'
      'Сплошная')
  end
  object SpinEdit8: TSpinEdit
    Left = 536
    Top = 240
    Width = 81
    Height = 22
    MaxValue = 10000
    MinValue = 1
    TabOrder = 10
    Value = 1
  end
end
