unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, SliderBars;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Panel1: TPanel;
    SliderPlane1: TSliderPlane;
    SliderPlane2: TSliderPlane;
    procedure FormCreate(Sender: TObject);
    procedure SliderPlane1Change(Sender: TObject);
    procedure SliderPlane1Paint(Sender: TObject; aRect: TRect);
    procedure SliderPlane2Paint(Sender: TObject; aRect: TRect);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  imgwd, imght: integer;
begin
  imgwd := Image1.Width;
  imght := Image1.Height;
  SliderPlane1 := TSliderPlane.Create(self);
  with SliderPlane1 do begin
    SetBounds(Image1.Left-2, Image1.Top-2, 4 + imgwd, 4 + imght);
    MaxValueX := imgwd-1;
    MaxValueY := imght-1;
    MarkerStyle := msLine;
    TabOrder := 0;
    parent := self;
    OnChange := @SliderPlane1Change;
    OnPaint := @SliderPlane1Paint;
  end;

  SliderPlane2 := TSliderPlane.Create(self);
  with SliderPlane2 do begin
    Left := 328;
    Height := 204;
    Top := 32;
    Width := 204;
    MarkerStyle := msNone;
    TabStop := False;
    Enabled := False;
    parent := self;
    OnPaint := @SliderPlane2Paint;
  end;

  SliderPlane1Change(nil);
end;


procedure TForm1.SliderPlane1Change(Sender: TObject);
var
  cl: TColor;
begin
  with SliderPlane1 do begin
    label2.Caption := Format('x: %d, y: %d', [ValueX, ValueY]);
    cl := Image1.Picture.Bitmap.Canvas.Pixels[ValueX, Image1.Height - ValueY];
    panel1.color := cl;
    label4.Caption := ColorToString(cl);
  end;
  SliderPlane2.Invalidate;
end;

procedure TForm1.SliderPlane1Paint(Sender: TObject; aRect: TRect);
var
  bRect: TRect;
begin
  with Image1 do
    bRect := Rect(0, 0, width, height);
  with (Sender as TSliderPlane) do begin
    canvas.BrushCopy(aRect, Image1.Picture.Bitmap, bRect, clNone);
  end;
end;


procedure TForm1.SliderPlane2Paint(Sender: TObject; aRect: TRect);
var
  bRect: TRect;
  x, y: integer;
begin
  with SliderPlane1 do begin
    x := ValueX;
    y := Image1.Height - ValueY;
  end;
  bRect := rect(x-9, y-9, x+10, y+10);
  with (Sender as TSliderPlane) do begin
    canvas.BrushCopy(aRect, Image1.Picture.Bitmap, bRect, clNone);
  end;
end;



end.

