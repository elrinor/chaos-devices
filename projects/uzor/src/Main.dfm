object Form1: TForm1
  Left = 539
  Top = 167
  BorderStyle = bsSingle
  Caption = 'Генератор '
  ClientHeight = 376
  ClientWidth = 184
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
    Left = 0
    Top = 0
    Width = 83
    Height = 13
    Caption = 'Имя .bmp файла'
  end
  object Label2: TLabel
    Left = 0
    Top = 40
    Width = 142
    Height = 13
    Caption = 'Количество витков спирали'
  end
  object Label3: TLabel
    Left = 0
    Top = 312
    Width = 44
    Height = 13
    Caption = 'Процесс'
  end
  object Label4: TLabel
    Left = 0
    Top = 80
    Width = 49
    Height = 13
    Caption = 'Число N='
  end
  object Edit1: TEdit
    Left = 0
    Top = 16
    Width = 185
    Height = 21
    TabOrder = 0
  end
  object Edit2: TEdit
    Left = 0
    Top = 56
    Width = 185
    Height = 21
    TabOrder = 1
  end
  object RadioGroup1: TRadioGroup
    Left = 0
    Top = 144
    Width = 185
    Height = 161
    Caption = 'Определяющая функция'
    TabOrder = 2
  end
  object RadioButton1: TRadioButton
    Left = 8
    Top = 160
    Width = 121
    Height = 17
    Caption = 'Простота числа'
    TabOrder = 3
  end
  object RadioButton2: TRadioButton
    Left = 8
    Top = 184
    Width = 121
    Height = 17
    Caption = 'Делимость на N'
    TabOrder = 4
  end
  object Edit3: TEdit
    Left = 0
    Top = 94
    Width = 185
    Height = 21
    TabOrder = 5
  end
  object Button1: TButton
    Left = 0
    Top = 352
    Width = 185
    Height = 25
    Caption = 'Сгенерить!'
    TabOrder = 6
    OnClick = Button1Click
  end
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 328
    Width = 185
    Height = 17
    Min = 1
    Max = 100
    Position = 1
    TabOrder = 7
  end
  object RadioButton3: TRadioButton
    Left = 8
    Top = 208
    Width = 137
    Height = 17
    Caption = 'Простота суммы цифр'
    TabOrder = 8
  end
  object RadioButton4: TRadioButton
    Left = 8
    Top = 232
    Width = 169
    Height = 17
    Caption = 'Делимость суммы цифр на N'
    TabOrder = 9
  end
  object RadioButton5: TRadioButton
    Left = 8
    Top = 256
    Width = 169
    Height = 17
    Caption = 'Cумма цифр больше N'
    TabOrder = 10
  end
  object CheckBox1: TCheckBox
    Left = 0
    Top = 120
    Width = 161
    Height = 17
    Caption = 'Белый цвет фона'
    Checked = True
    State = cbChecked
    TabOrder = 11
  end
  object RadioButton6: TRadioButton
    Left = 8
    Top = 280
    Width = 145
    Height = 17
    Caption = 'Палиндромность'
    TabOrder = 12
  end
end
