{-----------------------------------------------------------------------------
  TuEGauge v0.2  2015-05-17
  Author: Miguel A. Risco Castillo
  http://ue.accesus.com/uecontrols

  THE COPYRIGHT NOTICES IN THE SOURCE CODE MAY NOT BE REMOVED OR MODIFIED.
  IF YOU MODIFY AND/OR DISTRIBUTE THE CODE TO ANY THIRD PARTY THEN YOU MUST NOT
  VEIL THE ORIGINAL AUTHOR. IT MUST ALWAYS BE CLEARLY IDENTIFIABLE.

  The contents of this file are subject in priority to the License in this header,
  in the license.txt file and the Mozilla Public License Version 1.1 (MPL);
  you may not use this file except in compliance with these licenses. You may obtain
  a copy of the MPL License at http://www.mozilla.org/MPL/MPL-1.1.html

  Software distributed under the Licenses is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the Licenses for
  the specific language governing rights and limitations under the Licenses.
-----------------------------------------------------------------------------}

unit uEGauge;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  LCLIntf, LCLType, LCLProc, Types,
  BGRABitmap, BGRABitmapTypes, uEBase, uEKnob;


  { TCustomuEGauge }

type
  TCustomuEGauge = class(TCustomuEKnob)
  private
    FCapColor: TColor;
    FCapEdgeColor: TColor;
    FCapRadius: integer;
    FNeedleColor: TColor;
    procedure SetCapColor(AValue: TColor);
    procedure SetCapEdgeColor(AValue: TColor);
    procedure SetCapRadius(AValue: integer);
    procedure SetNeedleColor(AValue: TColor);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure BackgroundChanged(Sender : TObject); override;
    procedure DefaultPicture(r:integer); override;
    procedure DefaultBackImage; virtual;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    property NeedleLenght: integer read FDefKnobRadius write SetDefKnobRadius default 56;
    property NeedleColor: TColor read FNeedleColor write SetNeedleColor default clred;
    property CapRadius: integer read FCapRadius write SetCapRadius default 5;
    property CapColor: TColor read FCapColor write SetCapColor default clsilver;
    property CapEdgeColor: TColor read FCapEdgeColor write SetCapEdgeColor default clgray;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TuEGauge }

  TuEGauge = class(TCustomuEGauge)
  published
    property Debug;
//
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
//
    property NeedleLenght;
    property NeedleColor;
    property CapRadius;
    property CapColor;
    property CapEdgeColor;
//
    property Max;
    property MaxAngle;
    property Min;
    property MinAngle;
    property OffsetAngle;
    property Image;
    property BackImage;
    property Position;
    property TicksMargin;
    property LTicks;
    property LTicksSize;
    property LTicksWidth;
    property LTicksColor;
    property STicks;
    property STicksSize;
    property STicksColor;
    property ShowValues;
    property ValuesMargin;
    property ValuesFont;
    property Transparent;
    property OnChange;
    property OnPaint;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
//
    property Align;
    property Anchors;
    property BorderSpacing;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnChangeBounds;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnClick;
    property OnConstrainedResize;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property About;// This property must not be removed to follow the licence statements
  end;


implementation

{ TCustomuEGauge }

procedure TCustomuEGauge.DefaultPicture(r:integer);
var
  c:real;
  NeedleCol:TBGRAPixel;
  CapCol:TBGRAPixel;
  CapEdgeCol:TBGRAPixel;
begin
  if r<=0 then exit;
  NeedleCol:=ColorToBGRA(FNeedleColor);
  CapCol:=ColorToBGRA(FCapColor);
  CapEdgeCol:=ColorToBGRA(FCapEdgeColor);
  c:=(r-1)/2;
  Bitmap.SetSize(r,r);
  Bitmap.Fill(BGRAPixelTransparent);
  Bitmap.DrawLineAntialias(c,c,c,r,NeedleCol,2);
  Bitmap.FillEllipseAntialias(c,c,FCapRadius,FCapRadius,CapCol);
  Bitmap.EllipseAntialias(c,c,FCapRadius,FCapRadius,CapEdgeCol,2);
  AssignBGRAtoImage(Bitmap,Image);
end;

procedure TCustomuEGauge.DefaultBackImage;
var
  r,h,w:integer;
  xc,yc:single;
begin
  r:=9+(FTicksMargin+FLTicksSize+FValuesMargin);
  h:=Height;
  w:=Width;
  if r<=0 then exit;
  xc:=w/2;
  yc:=h/2;
  Bitmap.SetSize(w,h);
  Bitmap.Fill(BGRAPixelTransparent);
  Bitmap.FillEllipseAntialias(xc,yc,r,r,ColorToBGRA(clcream));
  Bitmap.GradientFill(0,0,w,h,
                      ColorToBGRA(clWhite),BGRA(0,0,0,0),
                      gtRadial,PointF(xc,yc),PointF(xc+r,yc),
                      dmDrawWithTransparency, true);
  Bitmap.EllipseAntialias(xc,yc,r,r,BGRABlack,2);
  AssignBGRAtoImage(Bitmap,BackImage);
end;

procedure TCustomuEGauge.BackgroundChanged(Sender: TObject);
begin
  if BackImage.Empty then DefaultBackImage;
  inherited
end;


function TCustomuEGauge.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  result:=false;
end;

constructor TCustomuEGauge.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FNeedleColor:=clred;
  FDefKnobRadius:=56;
  FCapColor:=clWhite;
  FCapEdgeColor:=clBlack;
  FCapRadius:=5;
  FTicksMargin:=19;
  FLTicksColor:=clNavy;
  FSTicksColor:=clSkyBlue;
  DefaultPicture(FDefKnobRadius);
  DefaultBackImage;
end;

procedure TCustomuEGauge.SetNeedleColor(AValue: TColor);
begin
  if FNeedleColor=AValue then Exit;
  FNeedleColor:=AValue;
  DefaultPicture(FDefKnobRadius);
end;

procedure TCustomuEGauge.SetCapColor(AValue: TColor);
begin
  if FCapColor=AValue then Exit;
  FCapColor:=AValue;
  DefaultPicture(FDefKnobRadius);
end;

procedure TCustomuEGauge.SetCapEdgeColor(AValue: TColor);
begin
  if FCapEdgeColor=AValue then Exit;
  FCapEdgeColor:=AValue;
  DefaultPicture(FDefKnobRadius);
end;

procedure TCustomuEGauge.SetCapRadius(AValue: integer);
begin
  if FCapRadius=AValue then Exit;
  FCapRadius:=AValue;
  DefaultPicture(FDefKnobRadius);
end;

procedure TCustomuEGauge.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
//
end;

procedure TCustomuEGauge.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
//
end;

procedure TCustomuEGauge.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
//
end;

end.
