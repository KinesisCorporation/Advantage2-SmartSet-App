{-----------------------------------------------------------------------------
  TuESelector v1.0 2015-05-17
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

unit uESelector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  LCLIntf, LCLType, LCLProc, Types,
  BGRABitmap, BGRABitmapTypes, uEBase, uEKnob;

type
  { TCustomuESelector }
  TCustomuESelector = class(TCustomuEKnob)
  private
    FIndex: Integer;
    FItems: TStringList;
    procedure SetIndex(const AValue: Integer);
    procedure SetItems(const AValue: TStringList);
  protected
    procedure SetupDefaults;override;
    procedure ItemsChanged(Sender:TObject); virtual;
    procedure ForcePosition(const AValue: Real); override;
    property Index: Integer read FIndex write SetIndex;
    property Items: TStringList read FItems write SetItems;
    procedure DrawScales(LBitmap:TBGRABitmap); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DefaultPicture(r:integer); override;
  end;


  TuESelector = class(TCustomuESelector)
  published
    property Debug;
//  This property is deprecated, use Image and LoadfromFile
    property Picture;
    property MaxAngle;
    property MinAngle;
    property OffsetAngle;
    property Image;
    property BackImage;
    property Index;
    property Items;
    property LTicksSize;
    property LTicksColor;
    property LTicksWidth;
    property TicksMargin;
    property ShowValues;
    property ValuesMargin;
    property ValuesFont;
    property Transparent;
    property DefKnobRadius;
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
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
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
  end;

implementation

procedure TCustomuESelector.SetIndex(const AValue: Integer);
begin
  if FIndex=AValue then exit;
  FIndex:=AValue;
  ForcePosition(AValue);
end;

procedure TCustomuESelector.SetItems(const AValue: TStringList);
begin
  if FItems.Equals(AValue) then exit;
  if AValue.Count>2 then FItems.Assign(AValue);
end;

procedure TCustomuESelector.SetupDefaults;
begin
  inherited SetupDefaults;
  FMax:=6;
  FMin:=0;
  FValuesMargin:=10;
  FLTicksSize:=5;
end;

procedure TCustomuESelector.DefaultPicture(r:integer);
var
  c:real;
begin
  if r<=0 then exit;
  c:=(r-1)/2;
  Bitmap.SetSize(r,r);
  Bitmap.Fill(BGRAPixelTransparent);
  Bitmap.FillEllipseAntialias(c,c,c,c,BGRABlack);
  Bitmap.GradientFill(0,0,r,r,
                      BGRA(128,128,128,255),BGRA(0,0,0,0),
                      gtRadial,PointF(c,c),PointF(0,c),
                      dmDrawWithTransparency);
  Bitmap.FillRectAntialias(c-5,0,c+5,r,BGRABlack);
  Bitmap.DrawLineAntialias(c,c+5,c,r-5,BGRAWhite,2);
  AssignBGRAtoImage(Bitmap,Image);
end;

procedure TCustomuESelector.ItemsChanged(Sender: TObject);
begin
  FLTicks:=FItems.Count;
  FMax:=FItems.Count-1;
  ForcePosition(FIndex);
  invalidate;
end;

constructor TCustomuESelector.Create(AOwner: TComponent);
begin
  FItems:=TStringList.Create;
  FItems.Add('S1=10');
  FItems.Add('S2=20');
  FItems.Add('S3=30');
  FItems.Add('S4=40');
  FItems.Add('S5=50');
  FItems.Add('S6=60');
  FItems.Add('S7=70');
  inherited Create(AOwner);
  FIndex:=0;
  FItems.OnChange := @ItemsChanged;
end;

destructor TCustomuESelector.Destroy;
begin
  FItems.OnChange:=nil;
  FreeThenNil(FItems);
  inherited Destroy;
end;

procedure TCustomuESelector.DrawScales(LBitmap: TBGRABitmap);
var i:integer;
    x1,y1,x2,y2,lpos:real;
    xc,yc,langle:real;
    sn,cn:real;
    lc,vc:TBGRAPixel;
    ts:TSize;
    la:string;
    ss:boolean;
begin
  xc:=LBitmap.Width/2-1;
  yc:=LBitmap.Height/2-1;
  lc:=ColorToBGRA(ColorToRGB(FLTicksColor));
  vc:=ColorToBGRA(ColorToRGB(FValuesFont.Color));
  LBitmap.FontHeight:=abs(FValuesFont.Height);
  LBitmap.FontStyle:=FValuesFont.Style;
  LBitmap.FontName:=FValuesFont.Name;
  LBitmap.FontOrientation:=FValuesFont.Orientation;
  ss:=((FMaxAngle-FMinAngle) mod 3600)=0;
  if FItems.Count>0 then For i:=0 to  FItems.Count-1 do
  begin
    lpos:=(i/(FItems.Count-1))*(FMax-FMin)+FMin;
    langle:=(PosToAngle(lpos)+FOffsetAngle)*PI/1800 +PI/2;
    sn:=sin(langle);
    cn:=cos(langle);
    x1:=xc+FTicksMargin*cn;
    y1:=yc+FTicksMargin*sn;
    x2:=xc+(FTicksMargin+FLTicksSize)*cn;
    y2:=yc+(FTicksMargin+FLTicksSize)*sn;
    if FLTicksSize>0 then LBitmap.DrawLineAntialias(x1,y1,x2,y2,lc, 1);
    if FShowValues  and not(ss and (i=0)) then
    begin
      la:=FItems.Names[i];
      if la='' then la:=FItems.ValueFromIndex[i];
      ts:=LBitmap.TextSize(la);
      x2:=xc+(FTicksMargin+FLTicksSize+FValuesMargin+ts.cx/8)*cn;
      y2:=yc+(FTicksMargin+FLTicksSize+FValuesMargin)*sn-ts.cy/2;
      LBitmap.TextOut(round(x2), round(y2), la, vc, taCenter);
    end;
  end;
end;

procedure TCustomuESelector.ForcePosition(const AValue: Real);
var lastindex:integer;
begin
  if AValue<FMin then FPosition:=FMin
  else if AValue>FMax then FPosition:=FMax
  else FPosition:=AValue;
  lastindex:=FIndex;
  FIndex:=round(FPosition);
  Angle:=(PostoAngle(FIndex)+FOffsetAngle)/10;
  if FIndex<>lastindex then DoOnChange;
end;

end.
