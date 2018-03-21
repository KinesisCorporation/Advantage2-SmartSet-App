unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, Spin, StdCtrls, ColorBox, SliderBars;

type

  { TForm1 }

  TForm1 = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    Button1: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    ColorBox1: TColorBox;
    ColorBox2: TColorBox;
    ColorBox3: TColorBox;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    PageControl1: TPageControl;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    SpinEdit3: TSpinEdit;
    SpinEdit4: TSpinEdit;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure Button1Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure CheckBox4Change(Sender: TObject);
    procedure CheckBox5Change(Sender: TObject);
    procedure ColorBox1Change(Sender: TObject);
    procedure ColorBox2Change(Sender: TObject);
    procedure ColorBox3Change(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure PageControl1Changing(Sender: TObject; var AllowChange: Boolean);
    procedure RadioButton1Change(Sender: TObject);
    procedure SpinEdit4Change(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    Slider: TCustomSlider;
    procedure SliderChange(Sender: TObject);
    procedure BoxChange(Sender: TObject);
    procedure SliderPlanePaint(Sender: TObject; aRect: TRect);
    function CreateColorBar: TColorBar;
    function CreateColorPlane: TColorPlane;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  LCLIntf;

{ TForm1 }

function TForm1.CreateColorBar: TColorBar;
begin
  result := TColorBar.create(self);
  with result do begin
    left := 20;
    top := 20;
    parent := self;
    Setcolors(RGB(0,0,0), RGB(255,0,0));
    Associate := SpinEdit1;
    MaxValue := 255;
    OnChange := @SliderChange;
    TabOrder := 0;
  end;
end;

function TForm1.CreateColorPlane: TColorPlane;
var
  green: byte;
begin
  result := TColorPlane.create(self);
  with result do begin
    left := 20;
    top := 20;

    OnChange := @BoxChange;
    parent := self;
    TabOrder := 0;
    //OnPaint := @SliderBoxPaint;
    Green := SpinEdit4.value;
    CutChannel := ccGreen;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Slider := CreateColorBar;
end;

procedure TForm1.PageControl1Change(Sender: TObject);
begin
  if PageControl1.TabIndex = 0 then begin
    Slider := CreateColorBar;
  end;
  if PageControl1.TabIndex = 1 then begin
    Slider := CreateColorPlane;
  end;
end;

procedure TForm1.PageControl1Changing(Sender: TObject; var AllowChange: Boolean
  );
begin
  freeandnil(Slider);
  ColorBox3.Selected:=clBlack;
  ColorBox1.Selected:=clWhite;
  ColorBox2.Selected:=clSilver;
  ComboBox1.ItemIndex:=0;
  CheckBox1.Checked:=false;
  CheckBox2.Checked:=false;
  CheckBox4.Checked:=false;
  CheckBox5.Checked:=false;
end;

function ColorChannelToStr(aValue: TColorChannel): string;
begin
  case aValue of
    ccRed: result := 'red';
    ccGreen: result := 'green';
    ccBlue: result := 'blue';
    ccAlpha: result := 'alpha';
  else
    result := '??';
  end;
end;

procedure TForm1.RadioButton1Change(Sender: TObject);
begin
  with TColorPlane(Slider) do begin
    CutChannel := TColorChannel(TRadioButton(Sender).Tag);
    label13.Caption := Format('X (%s) value', [ColorChannelToStr(HorizontalChannel)]);
    label14.Caption := Format('Y (%s) value', [ColorChannelToStr(VerticalChannel)]);
    SpinEdit4.Value := CutValue;
  end;
end;

procedure TForm1.SpinEdit4Change(Sender: TObject);
begin
  TColorPlane(Slider).CutValue := SpinEdit4.value;
end;

procedure TForm1.SliderChange(Sender: TObject);
begin
  assert(assigned(Sender));
  assert(Sender is TSliderBar);
  with TSliderBar(Sender) do
    Label6.Caption := Format('%.2f', [Position])
end;

procedure TForm1.BoxChange(Sender: TObject);
var
  aColor: TRGBAColor;
begin
  assert(assigned(Sender));
  assert(Sender is TColorPlane);
  with TColorPlane(Sender) do begin
    aColor.Color := Color;
    Label9.Caption := Format('%.2f', [PositionX]);
    Label12.Caption := Format('%.2f', [PositionY]);
    Label16.Caption := Format('%.2x%.2x%.2x', [aColor.Red,aColor.Green,aColor.Blue]);
  end;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  Slider.MarkerStyle:= TMarkerStyle(ComboBox1.ItemIndex);
end;

procedure TForm1.ColorBox1Change(Sender: TObject);
begin
  Slider.MarginColor := ColorBox1.Selected;
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
var
  wd, ht: integer;
begin
  if not assigned(Slider) then exit;
  with Slider as TSliderBar do begin
    wd := width;
    ht := height;
    SetBounds(left, top, ht, wd);
    if CheckBox1.Checked then
      Orientation := sboVertical
    else
      Orientation := sboHorizontal;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  close
end;

procedure TForm1.CheckBox2Change(Sender: TObject);
begin
  if not assigned(Slider) then exit;
  if CheckBox2.Checked then
    TSliderBar(Slider).MaxValue := 100
  else
    TSliderBar(Slider).MaxValue := 255;
end;

procedure TForm1.CheckBox4Change(Sender: TObject);
begin
  if not assigned(Slider) then exit;
  if CheckBox4.Checked then
    TColorPlane(Slider).MaxValueX := 100
  else
    TColorPlane(Slider).MaxValueX := 255;
end;

procedure TForm1.CheckBox5Change(Sender: TObject);
begin
  if not assigned(Slider) then exit;
  if CheckBox5.Checked then
    TColorPlane(Slider).MaxValueY := 100
  else
    TColorPlane(Slider).MaxValueY := 255;
end;

procedure TForm1.ColorBox2Change(Sender: TObject);
begin
  if not assigned(Slider) then exit;
  Slider.BorderColor := ColorBox2.Selected;
end;

procedure TForm1.ColorBox3Change(Sender: TObject);
begin
  if not assigned(Slider) then exit;
  Slider.MarkerColor := ColorBox3.Selected;
end;

procedure TForm1.SliderPlanePaint(Sender: TObject; aRect: TRect);
var
  aColor: record
    case boolean of
      false: (color: TColor);
      true: (Red: byte; Green: byte; Blue: byte; alpha: byte);
    end;
  ht, wd, x, y: integer;
begin
  ht := aRect.Bottom - aRect.Top - 1;
  wd := aRect.Right - aRect.Left - 1;
  aColor.Blue := 0;
  for y := 0 to ht do begin
    aColor.Green := ((ht-y)*255) div ht;
    for x := 0 to wd do begin
      aColor.Red := (x*255) div wd;
      TSliderPlane(Sender).Canvas.Pixels[x + aRect.Left, y + aRect.Top] := aColor.Color;
    end;
  end;
end;

end.

