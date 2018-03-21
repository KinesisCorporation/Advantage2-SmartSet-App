unit pickerdlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls,
  Graphics, Dialogs, StdCtrls, Spin, ExtCtrls, ButtonPanel, ComCtrls,
  SliderBars;

type

  { Tpickerform }

  TPickerForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    BlueBar: TColorBar;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CutBar: TColorBar;
    ColorPlane: TColorPlane;
    GreenBar: TColorBar;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RedBar: TColorBar;
    Label5: TLabel;
    PageControl1: TPageControl;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    colorBar: TSliderBar;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    SpinEdit3: TSpinEdit;
    RedEdit: TSpinEdit;
    GreenEdit: TSpinEdit;
    BlueEdit: TSpinEdit;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure ColorBarChange(Sender: TObject);
    procedure colorBarPaint(Sender: TObject; aRect: TRect);
    procedure ColorPlaneChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure SlicerChange(Sender: TObject);
  private
    { private declarations }
    FInitial, FSelected: packed record
      case boolean of
        false: (color: TColor);
        true: (Red: byte;
               Green: byte;
               Blue: byte;
               Alpha: byte);
      end;
    procedure RedrawBars;
    procedure RedrawSelected;
  public
    { public declarations }
    property InitialColor: TColor read FInitial.Color write FInitial.Color;
    property SelectedColor: TColor read FSelected.color;
  end;

implementation

{$R *.lfm}

uses
  LCLIntf;

{ TPickerForm }

procedure TPickerForm.ColorBarChange(Sender: TObject);
begin
  // when one colour chanel value is change d
  // all the colours bars need to be redrawn
  RedrawBars;
end;

procedure TPickerForm.CheckBox1Change(Sender: TObject);
var
  aMax: integer;
begin
  if CheckBox1.Checked then
    aMax := 100
  else
    aMax := 255;
  ColorPlane.MaxValue := aMax;
end;


procedure TPickerForm.colorBarPaint(Sender: TObject; aRect: TRect);
var
  middle: integer;
  orgRight: integer;
begin
  orgRight := aRect.Right;
  middle := (aRect.left + orgRight) div 2;
  with Sender as TSliderBar do begin
    with canvas do begin
      // fill the left half with the initial colour
      aRect.right := middle;
      brush.color := InitialColor;
      FillRect(aRect);
      // and fill the right half with the currently selected colour
      aRect.Right := orgRight;
      aRect.Left := middle;
      brush.color := SelectedColor;
      FillRect(aRect);
    end;
  end;
end;

procedure TPickerForm.FormShow(Sender: TObject);
begin
  RedBar.value := FInitial.Red;
  GreenBar.value := FInitial.Green;
  BlueBar.value := FInitial.Blue;
  // this will update the FSelected;
end;

procedure TPickerForm.RedrawSelected;
begin
  // update the hexadecima value of the selected color
  with FSelected do
    label4.Caption := Format('%.2x%.2x%.2x', [Red, Green, Blue]);
  // redraw the control showing the selected color
  colorBar.Invalidate;
end;

procedure TPickerForm.RedrawBars;
begin
  // wait for all four bars to be loaded before drawing them
  if csLoading in ComponentState then exit;
  with FSelected do begin
    // update the selected colour using the colour bar values
    // one of which has presumably been changed
    Red := round((255*RedBar.value)/RedBar.MaxValue);
    Green := round((255*GreenBar.value)/GreenBar.MaxValue);
    Blue := round((255*BlueBar.value)/BlueBar.MaxValue);
    // redraw all the colour bars, because they are interdependant
    RedBar.SetColors(RGB(0, Green, Blue), RGB(255, Green, Blue));
    GreenBar.SetColors(RGB(Red, 0, Blue), RGB(Red, 255, Blue));
    BlueBar.SetColors(RGB(Red, Green, 0), RGB(Red, Green, 255));
  end;
  RedrawSelected;
end;

{ }

procedure TPickerForm.PageControl1Change(Sender: TObject);
var
  aColor: TRGBAColor;
begin
  if PageControl1.TabIndex = 0 then begin
    aColor.color := FSelected.Color;
    with aColor do begin
      // Can't use FSelected because each call
      // below reforms FSelected with the TabSheet0
      // values for the other channels!

      // The transformation is just in case the color
      // range has been changed
      RedBar.Value := round( (RedBar.MaxValue*Red)/255 );
      GreenBar.Value := round( (GreenBar.MaxValue*Green)/255 );
      BlueBar.Value := round( (BlueBar.MaxValue*Blue)/255 );
    end;
  end
  else if PageControl1.TabIndex = 1 then begin
    ColorPlane.Color := FSelected.Color;
  end;
end;

{ RGB Cube }

procedure TPickerForm.ColorPlaneChange(Sender: TObject);
begin
  FSelected.Color := ColorPlane.Color;
  RedrawSelected;
end;

procedure TPickerForm.SlicerChange(Sender: TObject);
begin
  ColorPlane.CutChannel := TColorChannel(TRadioButton(Sender).Tag);
end;


 { Color range }

 procedure TPickerForm.CheckBox2Change(Sender: TObject);
var
  aMax: integer;
begin
  if CheckBox2.checked then
    aMax := 100
  else
    aMax := 255;
  RedBar.MaxValue := aMax;
  GreenBar.MaxValue := aMax;
  BlueBar.MaxValue := aMax;
end;

end.

