{------------------------------------------------------------------------------
  TuELED v1.0 2015-05-15
  Author:Miguel A. Risco-Castillo
  http://ue.accesus.com/uecontrols

  Properties:
  Active:boolean, for On/Off the LED
  Bright:boolean, enable/disable halo (for improve performance)
  Color:TColor, actual color of the LED, automatic darken color for off state
  LedType:ledRound/ledSquare, shape of the LED
  Reflection:boolean, enable 3D/flat effect (for improve performance)

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
------------------------------------------------------------------------------}

unit ueled;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  LCLIntf, LCLType, Types, BGRABitmap, BGRABitmapTypes, uEBase;

type

  TLedType = (ledRound, ledSquare);

{ TuELED }
  TCustomuELED = class(TuEBaseControl)
  private
    FActive: Boolean;
    FColor: TColor;
    FBright : Boolean;
    FReflection : Boolean;
    FLedType : TLedType;
    FOnChange: TNotifyEvent;
    FChanging:Boolean;
    procedure DrawLedRound(const r: integer; const LColor: TColor);
    procedure DrawLedSquare(const r: integer; const LColor: TColor);
    procedure SetActive(AValue:Boolean);
  protected
    class procedure WSRegisterClass; override;
    class function GetControlClassDefaultSize: TSize; override;
    procedure Loaded; override;
    procedure Resize; override;
    procedure SetColor(AValue: TColor); override;
    procedure SetBright(Avalue:Boolean); virtual;
    procedure SetReflection(Avalue:Boolean); virtual;
    procedure SetLedType(AValue:TLedType); virtual;
//    procedure DrawControl; override;
    procedure RenderControl; override;
    procedure DoChange; virtual;
    property Active: boolean read FActive write SetActive;
    property LedType: TLedType read FLedType write SetLedType;
    property Bright: boolean read FBright write SetBright;
    property Reflection: boolean read FReflection write SetReflection;
    property Color: tcolor read FColor write SetColor default clDefault;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TuELED = class(TCustomuELED)
  published
    property Debug;
    property Active;
    property LedType;
    property Bright;
    property Reflection;
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
    property OnChange;
    property OnChangeBounds;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPaint;
    property OnClick;
    property OnConstrainedResize;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

constructor TCustomuELED.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable, csCaptureMouse, csClickEvents, csDoubleClicks];
  with GetControlClassDefaultSize do SetInitialBounds(0, 0, CX, CY);
  FChanging:=false;
  FActive:=true;
  FBright:=true;
  FReflection:=true;
  FColor:=clLime;
  FLedType:=ledRound;
end;

procedure TCustomuELED.Loaded;
begin
  inherited Loaded;
end;

procedure TCustomuELED.Resize;
begin
  inherited Resize;
  RenderControl;
  Invalidate;
  DoChange;
end;

procedure TCustomuELED.SetColor(AValue: TColor);
begin
  if FColor = AValue then exit;
  FColor := AValue;
  RenderControl;
  inherited SetColor(AValue);
  Invalidate;
  DoChange;
end;

procedure TCustomuELED.SetBright(Avalue: Boolean);
begin
  if FBright = AValue then exit;
  FBright := AValue;
  RenderControl;
  Invalidate;
  DoChange;
end;

procedure TCustomuELED.SetReflection(Avalue: Boolean);
begin
  if FReflection = AValue then exit;
  FReflection := AValue;
  RenderControl;
  Invalidate;
  DoChange;
end;

procedure TCustomuELED.SetLedType(AValue: TLedType);
begin
  if FLedType = AValue then exit;
  FLedType := AValue;
  RenderControl;
  Invalidate;
  DoChange;
end;


procedure TCustomuELED.RenderControl;
var r:integer;
begin
  Bitmap.SetSize(width,height);
  Bitmap.Fill(BGRAPixelTransparent);
  if Width < Height then r:=Width else r:=Height;
  r:=r div 10;
  Case FLedType of
    ledSquare : DrawLedSquare(r+2, FColor);
  else
    DrawLedRound(r+3, FColor)
  end;
  inherited RenderControl;
end;

procedure TCustomuELED.DrawLedRound(const r: integer; const LColor: TColor);
var
  mask: TBGRABitmap;
  layer: TBGRABitmap;
begin
  //Bright
  if FActive and FBright then
  begin
    layer:=TBGRABitmap.Create(Width, Height);
    layer.GradientFill(0,0,layer.Width,layer.Height,
                       ColorToBGRA(ColortoRGB(LColor),240),ColorToBGRA(ColortoRGB(LColor),0),
                       gtRadial,PointF(layer.Width/2,layer.Height/2),PointF(0,layer.Height/2),
                       dmSet);
    Bitmap.PutImage(0,0,layer,dmDrawWithTransparency);
    layer.free;
  end;

  // Solid Led
  if FActive then
  begin
    layer:=TBGRABitmap.Create(Width-2*r, Height-2*r);
    layer.GradientFill(0,0,layer.Width,layer.Height,
                       ColorToBGRA(ColortoRGB(LColor)),BGRA(0,0,0),
                       gtRadial,PointF(layer.Width/2,layer.Height*8/15),PointF(layer.Width*1.5,layer.Height*1.5),
                       dmSet);
    mask := TBGRABitmap.Create(layer.Width,layer.Height,BGRABlack);
    mask.FillEllipseAntialias((layer.Width-1)/2,(layer.Height-1)/2,layer.Width/2,layer.Height/2,BGRAWhite);
    layer.ApplyMask(mask);
    mask.Free;
    Bitmap.PutImage(r,r,layer,dmDrawWithTransparency);
    layer.free;
  end else Bitmap.FillEllipseAntialias((Width-1)/2,(Height-1)/2,Width/2-r,Height/2-r, Darken(LColor,80));

  //Reflexion
  if FReflection then
  begin
    layer:=TBGRABitmap.Create((Width-1)-2*r, 5*(Height-2*r) div 8);
    layer.GradientFill(0,0,layer.Width,layer.Height,
                       BGRA(255,255,255,128),BGRA(255,255,255,0),
                       gtLinear,PointF(layer.Width/2,0),PointF(layer.Width/2,layer.Height*6/10),
                       dmSet);
    mask := TBGRABitmap.Create(layer.Width,layer.Height,BGRABlack);
    mask.FillEllipseAntialias(layer.Width/2,layer.Height/2,(layer.Width/2)*(4/5),(layer.Height/2)*(9/10),BGRAWhite);
    layer.ApplyMask(mask);
    mask.Free;
    Bitmap.PutImage(r,r,layer,dmDrawWithTransparency);
    layer.free;
  end;
end;

procedure TCustomuELED.DrawLedSquare(const r: integer; const LColor: TColor);
var
  mask: TBGRABitmap;
  layer: TBGRABitmap;
begin
  //Bright
  if FActive and FBright then
  begin
    layer:=TBGRABitmap.Create(Width, Height);
    layer.GradientFill(0,0,layer.Width,layer.Height,
                       ColorToBGRA(ColortoRGB(LColor),255),ColorToBGRA(ColortoRGB(LColor),0),
                       gtRadial,PointF(layer.Width/2,layer.Height/2),PointF(0,3*layer.Height/4),
                       dmSet);
    Bitmap.PutImage(0,0,layer,dmDrawWithTransparency);
    layer.free;
  end;


  // Solid Led
  if FActive then
  begin
    layer:=TBGRABitmap.Create(Width-2*r, Height-2*r);
    layer.GradientFill(0,0,layer.Width,layer.Height,
                       ColorToBGRA(ColortoRGB(LColor)),BGRA(0,0,0),
                       gtRadial,PointF(layer.Width/2,layer.Height/2),PointF(layer.Width*1.5,layer.Height*1.5),
                       dmSet);
    mask := TBGRABitmap.Create(layer.Width,layer.Height,BGRABlack);
    mask.FillRoundRectAntialias(0,0,layer.Width,layer.Height,r/2,r/2,BGRAWhite);
    layer.ApplyMask(mask);
    mask.Free;
    Bitmap.PutImage(r,r,layer,dmDrawWithTransparency);
    layer.free;
  end else Bitmap.FillRoundRectAntialias(r,r,Width-r,Height-r,r,r, Darken(LColor,80));

  //Reflexion
  if FReflection then
  begin
    layer:=TBGRABitmap.Create((Width-1)-2*r, 5*(Height-2*r) div 8);
    layer.GradientFill(0,0,layer.Width,layer.Height,
                       BGRA(255,255,255,160),BGRA(255,255,255,0),
                       gtLinear,PointF(layer.Width/2,0),PointF(layer.Width/2,layer.Height*6/10),
                       dmSet);
    mask := TBGRABitmap.Create(layer.Width,layer.Height,BGRABlack);
    mask.FillRoundRectAntialias(layer.Width*(1/20),layer.Height*(1/20),layer.Width*(19/20),layer.Height*(19/20),r,r,BGRAWhite);
    layer.ApplyMask(mask);
    mask.Free;
    Bitmap.PutImage(r,r,layer,dmDrawWithTransparency);
    layer.free;
  end;

end;

procedure TCustomuELED.SetActive(AValue: Boolean);
begin
  if AValue <> FActive then
  begin
    FActive := AValue;
    RenderControl;
    Invalidate;
    DoChange;
  end;
end;

class function TCustomuELED.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 24;
  Result.CY := 24;
end;

procedure TCustomuELED.DoChange;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

class procedure TCustomuELED.WSRegisterClass;
begin
  inherited WSRegisterClass;
end;

end.


