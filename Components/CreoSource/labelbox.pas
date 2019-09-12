unit LabelBox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, lcltype, LCLIntf, BGRABitmap, BGRABitmapTypes;

type

  { TLabelBox }

  TLabelBox = class(TLabel)
  private
    { Private declarations }
    FBorderColor: TColor;
    FBackColor: TColor;
    FIndex: integer;
    FCornerSize: integer;
    FBorderStyle: TBorderStyle;
    FBorderWidth: integer;
    FOpaque: boolean;
    FTransparency: integer;
    procedure DrawOpacityBrush(ACanvas: TCanvas; X, Y: Integer; AColor: TColor;
      ASize: Integer; Opacity: Byte);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
  published
    { Published declarations }
    property BorderStyle: TBorderStyle read FBorderStyle write FBorderStyle default bsNone;
    property BorderColor: TColor read FBorderColor write FBorderColor;
    property BackColor: TColor read FBackColor write FBackColor default clNone;
    property Index: integer read FIndex write FIndex default -1;
    property CornerSize: integer read FCornerSize write FCornerSize default 0;
    property BorderWidth: integer read FBorderWidth write FBorderWidth default 1;
    property Opaque: boolean read FOpaque write FOpaque default true;
    property Transparency: integer read FTransparency write FTransparency default 0;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CS',[TLabelBox]);
end;

{ TLabelBox }

constructor TLabelBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TLabelBox.Paint;
var
  ww, hh: Integer;
    var image: TBGRABitmap;
    c: TBGRAPixel;
const
  corners = 10;
begin
  inherited Paint;

  //Transparent:= true;

  //if (BorderStyle = bsSingle) then
  if (FBackColor <> clNone) then
  begin
    image := TBGRABitmap.Create(Width,Height,BGRAPixelTransparent);
    if (FOpaque) then
    begin
      //image.FillRoundRectAntialias(0,0,Width,Height,corners,corners,ColorToBGRA(ColorToRGB(FBackColor), 200));
      //image.Draw(Canvas,0,0, false);
      image.FillRoundRectAntialias(0,0,Width,Height,corners,corners,ColorToBGRA(ColorToRGB(FBackColor)));
      image.Draw(Canvas,0,0);
      image.Free;
    end
    else
    begin
      image.FillRoundRectAntialias(0,0,Width,Height,corners,corners,ColorToBGRA(ColorToRGB(FBackColor), FTransparency));
      image.Draw(Canvas,0,0, false);
      image.Free;
    end;

    //image := TBGRABitmap.Create(ClientWidth,ClientHeight,
    //                            ColorToBGRA(ColorToRGB(FBackColor), 80));
    ////c := ColorToBGRA(ColorToBGRA(ColorToRGB(FBackColor), 80));
    //
    ////image.JoinStyle := pjsBevel;
    ////image.RectangleAntialias(0, 0, Width, Height, c, 0);
    //
    //image.Draw(Canvas,0,0,false);
    //image.free;
  end;
  if (BorderStyle = bsSingle) then
  begin
    image := TBGRABitmap.Create(Width,Height,BGRAPixelTransparent);
    image.RoundRectAntialias(0,0,Width - FBorderWidth,Height - FBorderWidth,corners,corners,FBorderColor, FBorderWidth);
    image.Draw(Canvas,0,0, false);
    image.Free;
    //Canvas.Pen.Color := FBorderColor;
    //Canvas.Pen.Width := FBorderWidth;
    //Canvas.RoundRect (0, 0, ClientWidth, ClientHeight, FCornerSize, FCornerSize);
  end
  //if (BorderStyle = bsSingle) then
  //begin
  //  if (FBackColor <> clNone) then
  //    Canvas.Brush.Color := FBackColor;
  //  Canvas.Pen.Color := FBorderColor;
  //  Canvas.Pen.Width := FBorderWidth;
  //  Canvas.RoundRect (0, 0, ClientWidth, ClientHeight, FCornerSize, FCornerSize);
  //end
  //else if (FBackColor <> clNone) then
  //begin
  //  Canvas.Brush.Color := FBackColor;
  //  Canvas.Pen.Color := FBackColor;
  //  Canvas.Pen.Width := 1;
  //  Canvas.RoundRect (0, 0, ClientWidth, ClientHeight, FCornerSize, FCornerSize);
  //  //DrawOpacityBrush(Canvas, 0, 0, FBackColor, Width, 50);
  //  //ww := 0;
  //  //hh := 0;
  //  //Canvas.GetTextSize(self.Caption, ww, hh);
  //  //Canvas.TextOut((Width-ww) div 2, (Height-hh) div 2, self.Caption);
  //end;
end;

procedure TLabelBox.DrawOpacityBrush(ACanvas: TCanvas; X, Y: Integer; AColor: TColor; ASize: Integer; Opacity: Byte);
var
  Bmp: TBitmap;
  I, J: Integer;
  Pixels: PRGBQuad;
  ColorRgb: Integer;
  ColorR, ColorG, ColorB: Byte;
begin
  Bmp := TBitmap.Create;
  try
    Bmp.PixelFormat := pf32Bit; // needed for an alpha channel
    Bmp.SetSize(ASize, ASize);

    with Bmp.Canvas do
    begin
      Brush.Color := clFuchsia; // background color to mask out
      ColorRgb := ColorToRGB(Brush.Color);
      FillRect(Rect(0, 0, ASize, ASize));
      Pen.Color := AColor;
      Pen.Style := psSolid;
      Pen.Width := ASize;
      MoveTo(ASize div 2, ASize div 2);
      LineTo(ASize div 2, ASize div 2);
    end;

    ColorR := GetRValue(ColorRgb);
    ColorG := GetGValue(ColorRgb);
    ColorB := GetBValue(ColorRgb);

    for I := 0 to Bmp.Height-1 do
    begin
      Pixels := PRGBQuad(Bmp.ScanLine[I]);
      for J := 0 to Bmp.Width-1 do
      begin
        with Pixels^ do
        begin
          if (rgbRed = ColorR) and (rgbGreen = ColorG) and (rgbBlue = ColorB) then
            rgbReserved := 0
          else
            rgbReserved := Opacity;
          // must pre-multiply the pixel with its alpha channel before drawing
          rgbRed := (rgbRed * rgbReserved) div $FF;
          rgbGreen := (rgbGreen * rgbReserved) div $FF;
          rgbBlue := (rgbBlue * rgbReserved) div $FF;
        end;
        Inc(Pixels);
      end;
    end;

    ACanvas.Draw(X, Y, Bmp);
  finally
    Bmp.Free;
  end;
end;


end.
