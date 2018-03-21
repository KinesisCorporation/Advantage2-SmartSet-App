{**************************************************************************************************
 This file is part of the Eye Candy Controls (EC-C)

  Copyright (C) 2014-2016 Vojtěch Čihák, Czech Republic

  This library is free software; you can redistribute it and/or modify it under the terms of the
  GNU Library General Public License as published by the Free Software Foundation; either version
  2 of the License, or (at your option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you permission to link this
  library with independent modules to produce an executable, regardless of the license terms of
  these independent modules,and to copy and distribute the resulting executable under terms of
  your choice, provided that you also meet, for each linked independent module, the terms and
  conditions of the license of that module. An independent module is a module which is not derived
  from or based on this library. If you modify this library, you may extend this exception to your
  version of the library, but you are not obligated to do so. If you do not wish to do so, delete
  this exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
  without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See
  the GNU Library General Public License for more details.

  You should have received a copy of the GNU Library General Public License along with this
  library; if not, write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
  Boston, MA 02111-1307, USA.

**************************************************************************************************}

unit ECConfCurve;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, LMessages, Math,
  ECTypes, Themes, types;

type
  {$PACKENUM 2}
  TCurveOption = (ecoAllowOverdraw, ecoAntiAliasing, ecoFixedMin, ecoFixedMax,
                  ecoGuidelines, ecoReadOnly, ecoReversed, ecoShowGrid,
                  ecoSnapToGrid, ecoWheelShifts);
  TCurveOptions = set of TCurveOption;
  TCurveStyle = (ecsLinear, ecsBezier);
  TPointOption = (epoFixedX, epoFixedY);
  TPointOptions = set of TPointOption;
  TCurvePoint = record
    X: Single;
    Y: Single;
    Options: TPointOptions;
  end;

  { TCustomECConfCurve }
  TCustomECConfCurve = class(TGraphicControl)
  private
    FGridX: Single;
    FGridColor: TColor;
    FGridY: Single;
    FGuideColor: TColor;
    FLineColor: TColor;
    FLineWidth: SmallInt;
    FMaxX: Single;
    FMaxY: Single;
    FMinX: Single;
    FMinY: Single;
    FOnChange: TNotifyEvent;
    FOptions: TCurveOptions;
    FPointColor: TColor;
    FPointColorFixed: TColor;
    FPointSize: SmallInt;
    FSmoothness: Single;
    FSnapGrid: Single;
    FStyle: TCurveStyle;
    FValueAtMax: Single;
    FValueAtMin: Single;
    FWheelShift: Single;
    procedure SetGridX(AValue: Single);
    procedure SetGridColor(AValue: TColor);
    procedure SetGridY(AValue: Single);
    procedure SetLineColor(AValue: TColor);
    procedure SetLineWidth(AValue: SmallInt);
    procedure SetMaxX(AValue: Single);
    procedure SetMaxY(AValue: Single);
    procedure SetMinX(AValue: Single);
    procedure SetMinY(AValue: Single);
    procedure SetOptions(AValue: TCurveOptions);
    procedure SetPointColor(AValue: TColor);
    procedure SetPointColorFixed(AValue: TColor);
    procedure SetPointSize(AValue: SmallInt);
    procedure SetSmoothness(AValue: Single);
    procedure SetStyle(AValue: TCurveStyle);
    procedure SetValueAtMax(AValue: Single);
    procedure SetValueAtMin(AValue: Single);
  protected const
    cDefLineWidth = 2;
    cDefOptions = [ecoAntiAliasing, ecoShowGrid, ecoWheelShifts];
    cDefPointSize = 7;
    cMinHovered = -1;
    cMaxHovered = -2;
  protected
    type TPointPos = (eppMiddle, eppLeftSide, eppRightSide);
  protected
    FCursorBkgnd: TCursor;
    FCursorLock: Boolean;
    FDragging: Boolean;
    FHovered: SmallInt;
    FInitPoint: TPoint;
    FRealRevered: Boolean;
    FUpdateCount: Integer;
    FXHelp, FYHelp: Single;
    class function GetControlClassDefaultSize: TSize; override;
    procedure Calculate;
    procedure ChangeBounds(ALeft, ATop, AWidth, AHeight: integer; KeepBase: boolean);
      override;
    procedure ChangeCursor(ADrag: Boolean);
    procedure CMBiDiModeChanged(var Message: TLMessage); message CM_BIDIMODECHANGED;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    function GetCanvasPoint(AX, AY: Single): TPoint;
    function GetPointRect(APoint: TPoint; APointPos: TPointPos): TRect;
    procedure InvalidateNonUpdated;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure SetCursor(Value: TCursor); override;
  public
    Points: array of TCurvePoint;
    constructor Create(AOwner: TComponent); override;
    function AddPoint: Integer;
    procedure BeginUpdate;
    procedure ClearPoints;
    procedure DeletePoint(AIndex: Integer);
    procedure EndUpdate;
    function CoordToValueX(AX: Integer): Single;
    function CoordToValueY(AY: Integer): Single;
    function GetValueAtX(AX: Single): Single;
    function GetXForValue(AValue: Single): Single;
    procedure InsertPoint(AIndex: Integer);
    property GridX: Single read FGridX write SetGridX;
    property GridY: Single read FGridY write SetGridY;
    property GridColor: TColor read FGridColor write SetGridColor default clDefault;
    property GuideColor: TColor read FGuideColor write FGuideColor default clDefault;
    property LineColor: TColor read FLineColor write SetLineColor default clDefault;
    property LineWidth: SmallInt read FLineWidth write SetLineWidth default cDefLineWidth;
    property MaxX: Single read FMaxX write SetMaxX;
    property MaxY: Single read FMaxY write SetMaxY;
    property MinX: Single read FMinX write SetMinX;
    property MinY: Single read FMinY write SetMinY;
    property Options: TCurveOptions read FOptions write SetOptions default cDefOptions;
    property PointColor: TColor read FPointColor write SetPointColor default clDefault;
    property PointColorFixed: TColor read FPointColorFixed write SetPointColorFixed default clDefault;
    property PointSize: SmallInt read FPointSize write SetPointSize default cDefPointSize;
    property Smoothness: Single read FSmoothness write SetSmoothness;
    property SnapGrid: Single read FSnapGrid write FSnapGrid;
    property Style: TCurveStyle read FStyle write SetStyle default ecsLinear;
    property ValueAtMax: Single read FValueAtMax write SetValueAtMax;
    property ValueAtMin: Single read FValueAtMin write SetValueAtMin;
    property WheelShift: Single read FWheelShift write FWheelShift;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  { TECConfCurve }
  TECConfCurve = class(TCustomECConfCurve)
  published
    property Align;
    property Anchors;
    property BiDiMode;
    property BorderSpacing;
    property Caption;
    property Color;
    property Constraints;
    property Font;
    property GridX;
    property GridY;
    property GridColor;
    property GuideColor;
    property LineColor;
    property LineWidth;
    property MaxX;
    property MaxY;
    property MinX;
    property MinY;
    property Options;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PointColor;
    property PointColorFixed;
    property PointSize;
    property PopupMenu;
    property ShowHint;
    property Smoothness;
    property SnapGrid;
    property Style;
    property ValueAtMax;
    property ValueAtMin;
    property Visible;
    property WheelShift;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnEditingDone;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPaint;
    property OnResize;
  end;

implementation

{ TCustomECConfCurve }

constructor TCustomECConfCurve.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle:=ControlStyle-[csOpaque];
  FCursorBkgnd:=Cursor;
  FGridX:=0.1;
  FGridY:=0.1;
  FGridColor:=clDefault;
  FGuideColor:=clDefault;
  FLineColor:=clDefault;
  FLineWidth:=cDefLineWidth;
  FMinX:=0;
  FMaxX:=1;
  FMinY:=0;
  FMaxY:=1;
  FOptions:=cDefOptions;
  FPointColor:=clDefault;
  FPointColorFixed:=clDefault;
  FPointSize:=cDefPointSize;
  FSmoothness:=0.25;
  FSnapGrid:=0.05;
  FValueAtMin:=0.25;
  FValueAtMax:=0.75;
  FWheelShift:=0.05;
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
  FHovered:=low(SmallInt);
  Calculate;
end;

function TCustomECConfCurve.AddPoint: Integer;
var ax, ay: Single;
begin
  Result:=length(Points);
  SetLength(Points, Result+1);
  if Result=0 then
    begin;
      ax:=MinX;
      ay:=ValueAtMin;
    end else
    begin
      ax:=Points[Result-1].X;
      ay:=Points[Result-1].Y;
    end;
  Points[Result].X:=(ax+MaxX)/2;
  Points[Result].Y:=(ay+ValueAtMax)/2;
  Points[Result].Options:=[];
  InvalidateNonUpdated;
end;

procedure TCustomECConfCurve.BeginUpdate;
begin
  inc(FUpdateCount);
end;

procedure TCustomECConfCurve.Calculate;
begin
  FXHelp:=(Width-1)/(MaxX-MinX);
  FYHelp:=(Height-1)/(MaxY-MinY);
end;

procedure TCustomECConfCurve.ChangeBounds(ALeft, ATop, AWidth,
  AHeight: integer; KeepBase: boolean);
begin
  inherited ChangeBounds(ALeft, ATop, AWidth, AHeight, KeepBase);
  Calculate;
end;

procedure TCustomECConfCurve.ChangeCursor(ADrag: Boolean);
begin
  FCursorLock:=True;
  if ADrag
    then Cursor:=crDrag
    else Cursor:=FCursorBkgnd;
  FCursorLock:=False;
end;

procedure TCustomECConfCurve.ClearPoints;
begin
  SetLength(Points, 0);
  InvalidateNonUpdated;
end;

procedure TCustomECConfCurve.CMBiDiModeChanged(var Message: TLMessage);
begin
  inherited CMBiDiModeChanged(Message);
  FRealRevered:= IsRightToLeft xor (ecoReversed in Options);
end;

function TCustomECConfCurve.CoordToValueX(AX: Integer): Single;
begin
  if FRealRevered then AX:=Width-AX;
  Result:=AX/FXHelp+MinX;
end;

function TCustomECConfCurve.CoordToValueY(AY: Integer): Single;
begin
  Result:=(Height-AY)/FYHelp+MinY;
end;

procedure TCustomECConfCurve.DeletePoint(AIndex: Integer);
var i, aLength: Integer;
begin
  aLength:=length(Points);
  if (AIndex>=0) and (AIndex<aLength) then
    begin
      for i:=AIndex to aLength-2 do
        Points[i]:=Points[i+1];
      SetLength(Points, aLength-1);
      InvalidateNonUpdated;
    end;
end;

function TCustomECConfCurve.DoMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint): Boolean;
var aInc: Single;
    i: Integer;
begin
  Result:=inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  if ([ecoReadOnly, ecoWheelShifts]*Options)=[ecoWheelShifts] then
    begin
      aInc:=WheelShift;
      if WheelDelta<0 then aInc:=-aInc;
      if not (ecoFixedMin in Options) then FValueAtMin:=FValueAtMin+aInc;
      if not (ecoFixedMax in Options) then FValueAtMax:=FValueAtMax+aInc;
      for i:=0 to length(Points)-1 do
        Points[i].Y:=Points[i].Y+aInc;
      InvalidateNonUpdated;
    end;
end;

procedure TCustomECConfCurve.EndUpdate;
begin
  dec(FUpdateCount);
  if FUpdateCount=0 then Invalidate;
end;

class function TCustomECConfCurve.GetControlClassDefaultSize: TSize;
begin
  Result:=Size(240, 240);
end;

function TCustomECConfCurve.GetCanvasPoint(AX, AY: Single): TPoint;
begin
  Result.X:=round((AX-MinX)*FXHelp);
  if FRealRevered then Result.X:=Width-Result.X;
  Result.Y:=Height-round((AY-MinY)*FYHelp);
end;

function TCustomECConfCurve.GetPointRect(APoint: TPoint; APointPos: TPointPos): TRect;
var aPointSize: SmallInt;
begin
  aPointSize:=PointSize;
  case APointPos of
    eppMiddle:
      begin
        Result.Left:=aPoint.X-(aPointSize div 2);
        Result.Right:=Result.Left+aPointSize;
      end;
    eppLeftSide:
      begin
        Result.Left:=0;
        Result.Right:=aPointSize;
      end;
    eppRightSide:
      begin
        Result.Right:=Width;
        Result.Left:=Result.Right-aPointSize;
      end;
  end;
  Result.Top:=aPoint.Y-(aPointSize div 2);
  Result.Bottom:=Result.Top+aPointSize;
end;

function TCustomECConfCurve.GetValueAtX(AX: Single): Single;
var X1, X2, Y1, Y2: Single;
    aCount, i: Integer;
begin
  aCount:=length(Points);
  X1:=MinX;
  Y1:=ValueAtMin;
  if aCount>0 then
    begin
      X2:=Points[0].X;
      Y2:=Points[0].Y;
    end else
    begin
      X2:=MaxX;
      Y2:=ValueAtMax;
    end;
  if AX>=X1 then
    begin
      i:=1;
      while (i<=aCount) and (AX>X2) do
        begin
          X1:=X2;
          Y1:=Y2;
          if i<aCount then
            begin
              X2:=Points[i].X;
              Y2:=Points[i].Y;
            end else
            begin
              X2:=MaxX;
              Y2:=ValueAtMax;
            end;
          inc(i);
        end;
    end;
  Result:=Y1+(Y2-Y1)*(AX-X1)/(X2-X1);
end;

function TCustomECConfCurve.GetXForValue(AValue: Single): Single;
var X1, X2, Y1, Y2: Single;
    aCount, i: Integer;
begin
  Result:=Nan;
  aCount:=length(Points);
  X1:=MinX;
  Y1:=ValueAtMin;
  if aCount>0 then
    begin
      X2:=Points[0].X;
      Y2:=Points[0].Y;
    end else
    begin
      X2:=MaxX;
      Y2:=ValueAtMax;
    end;
  if AValue>=Y1 then
    begin
      i:=1;
      while (i<=aCount) and (AValue>Y2) do
        begin
          X1:=X2;
          Y1:=Y2;
          if i<aCount then
            begin
              X2:=Points[i].X;
              Y2:=Points[i].Y;
            end else
            begin
              X2:=MaxX;
              Y2:=ValueAtMax;
            end;
          inc(i);
        end;
      if AValue<=Y2 then Result:=X1+(X2-X1)*(AValue-Y1)/(Y2-Y1);
    end;
end;

procedure TCustomECConfCurve.InsertPoint(AIndex: Integer);
var aCount, i: Integer;
    ax, ay, bx, by: Single;
begin
  aCount:=length(Points);
  if (AIndex>=0) and (AIndex<=aCount) then
    begin
      SetLength(Points, aCount+1);
      for i:=aCount-1 downto AIndex do
        Points[i+1]:=Points[i];
      if AIndex=0 then
        begin;
          ax:=MinX;
          ay:=ValueAtMin;
        end else
        begin
          ax:=Points[AIndex-1].X;
          ay:=Points[AIndex-1].Y;
        end;
      if AIndex=aCount then
        begin;
          bx:=MaxX;
          by:=ValueAtMax;
        end else
        begin
          bx:=Points[AIndex+1].X;
          by:=Points[AIndex+1].Y;
        end;
      Points[AIndex].X:=(ax+bx)/2;
      Points[AIndex].Y:=(ay+by)/2;
      Points[AIndex].Options:=[];
      InvalidateNonUpdated;
    end;
end;

procedure TCustomECConfCurve.InvalidateNonUpdated;
begin
  if FUpdateCount=0 then Invalidate;
end;

procedure TCustomECConfCurve.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var aPoint: TPoint;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button=mbLeft) and (FHovered>low(SmallInt)) then
    begin
      FDragging:=True;
      case FHovered of
        cMaxHovered: aPoint:=GetCanvasPoint(MaxX, ValueAtMax);
        cMinHovered: aPoint:=GetCanvasPoint(MinX, ValueAtMin);
        0..high(SmallInt): aPoint:=GetCanvasPoint(Points[FHovered].X, Points[FHovered].Y);
      end;
      FInitPoint.X:=aPoint.X-X;
      FInitPoint.Y:=aPoint.Y-Y;
    end;
end;

procedure TCustomECConfCurve.MouseLeave;
begin
  inherited MouseLeave;
  if ecoGuidelines in Options then InvalidateNonUpdated;
end;

procedure TCustomECConfCurve.MouseMove(Shift: TShiftState; X, Y: Integer);
var i: Integer;
    f, aLeftBound, aRightBound: Single;
    b: Boolean;
    aPoint: TPoint;
    aRect: TRect;
begin
  inherited MouseMove(Shift, X, Y);
  if not FDragging then
    begin
      if not (ecoReadOnly in Options) then
        begin
          if not (ecoFixedMin in Options) then
            begin
              aPoint:=GetCanvasPoint(MinX, ValueAtMin);
              if not FRealRevered
                then aRect:=GetPointRect(aPoint, eppLeftSide)
                else aRect:=GetPointRect(aPoint, eppRightSide);
              if PtInRect(aRect, Point(X, Y)) then
                begin
                  FHovered:=cMinHovered;
                  ChangeCursor(True);
                  exit;  { Exit! }
                end;
            end;
          if not (ecoFixedMax in Options) then
            begin
              aPoint:=GetCanvasPoint(MaxX, ValueAtMax);
              if not FRealRevered
                then aRect:=GetPointRect(aPoint, eppRightSide)
                else aRect:=GetPointRect(aPoint, eppLeftSide);
              if PtInRect(aRect, Point(X, Y)) then
                begin
                  FHovered:=cMaxHovered;
                  ChangeCursor(True);
                  exit;  { Exit! }
                end;
            end;
          for i:=0 to length(Points)-1 do
            if [epoFixedX, epoFixedY]*Points[i].Options<>[epoFixedX, epoFixedY] then
              begin
                aPoint:=GetCanvasPoint(Points[i].X, Points[i].Y);
                aRect:=GetPointRect(aPoint, eppMiddle);
                if PtInRect(aRect, Point(X, Y)) then
                  begin
                    FHovered:=i;
                    ChangeCursor(True);
                    exit;  { Exit! }
                  end;
              end;
          FHovered:=low(SmallInt);
          ChangeCursor(False);
        end;
    end else
    begin  { Dragging }
      case FHovered of
        cMaxHovered:
          begin
            f:=CoordToValueY(Y+FInitPoint.Y);
            if ecoSnapToGrid in Options then f:=round(f/SnapGrid)*SnapGrid;
            if (f<>ValueAtMax) and assigned(OnChange) then OnChange(self);
            ValueAtMax:=f;
          end;
        cMinHovered:
          begin
            f:=CoordToValueY(Y+FInitPoint.Y);
            if ecoSnapToGrid in Options then f:=round(f/SnapGrid)*SnapGrid;
            if (f<>ValueAtMin) and assigned(OnChange) then OnChange(self);
            ValueAtMin:=f;
          end;
        0..high(SmallInt):
          begin
            b:=False;
            if not (epoFixedX in Points[FHovered].Options) then
              begin
                f:=CoordToValueX(X+FInitPoint.X);
                if ecoSnapToGrid in Options then f:=round(f/SnapGrid)*SnapGrid;
                if not (ecoAllowOverdraw in Options) then
                  begin
                    if FHovered>0
                      then aLeftBound:=Points[FHovered-1].X
                      else aLeftBound:=MinX;
                    if FHovered<(length(Points)-1)
                      then aRightBound:=Points[FHovered+1].X
                      else aRightBound:=MaxX;
                    if f<aLeftBound
                      then f:=aLeftBound
                      else if f>aRightBound then f:=aRightBound;
                  end;
                b:= Points[FHovered].X<>f;
                Points[FHovered].X:=f;
              end;
            if not (epoFixedY in Points[FHovered].Options) then
              begin
                f:=CoordToValueY(Y+FInitPoint.Y);
                if ecoSnapToGrid in Options then f:=round(f/SnapGrid)*SnapGrid;
                b:= b or (Points[FHovered].Y<>f);
                Points[FHovered].Y:=f;
              end;
            if b and assigned(OnChange) then OnChange(self);
          end;
      end;
      InvalidateNonUpdated;
    end;
  if ecoGuidelines in Options then InvalidateNonUpdated;
end;

procedure TCustomECConfCurve.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FDragging then
    begin;
      FDragging:=False;
      if assigned(OnEditingDone) then OnEditingDone(self);
    end;
end;

procedure TCustomECConfCurve.Paint;
const cDetails: array[Boolean] of TThemedButton = (tbPushButtonDisabled, tbPushButtonNormal);
var i, k: Integer;
    bEnabled: Boolean;
    aBackColor, aColor, aFPColor: TColor;
    aDetails: TThemedElementDetails;
    aPoint: TPoint;
    aPoints: array of TPoint;
    aRect: TRect;
    aSmoothX, aSmoothY: Single;
begin
  inherited Paint;
  bEnabled:=IsEnabled;
  Canvas.Brush.Style:=bsClear;
  Canvas.AntialiasingMode:=amOff;
  { fill background }
  aBackColor:=Color;
  if (aBackColor<>clDefault) and not ParentColor then
    begin
      if not bEnabled then aBackColor:=GetMonochromaticColor(aBackColor);
      Canvas.Brush.Color:=aBackColor;
      Canvas.FillRect(ClientRect);
    end else
    aBackColor:=Parent.Brush.Color;
  { draw grid }
  if ecoShowGrid in Options then
    begin
      aColor:=GridColor;
      if aColor=clDefault then aColor:=clBtnText;
      if not bEnabled then
        begin
          aColor:=GetMonochromaticColor(aColor);
          aColor:=GetMergedColor(aColor, aBackColor, 0.5);
        end else
        aColor:=GetMergedColor(aColor, aBackColor, 0.8);
      Canvas.Pen.Color:=aColor;
      Canvas.Pen.Style:=psDot;
      Canvas.Pen.Width:=1;
      for i:=0 to trunc((MaxX-MinX)/GridX)+1 do
        begin
          k:=round((i*GridX-MinX)*FXHelp);
          Canvas.Line(k, 0, k, Height);
        end;
      for i:=0 to trunc((MaxY-MinY)/GridY)+1 do
        begin
          k:=Height-round((i*GridY-MinY)*FYHelp)-1;
          Canvas.Line(0, k, Width, k);
        end;
    end;
  { draw curve }
  if ecoAntiAliasing in Options then Canvas.AntialiasingMode:=amOn;
  aColor:=LineColor;
  if aColor=clDefault then aColor:=clBtnText;
  if not bEnabled then
    begin
      aColor:=GetMonochromaticColor(aColor);
      aColor:=GetMergedColor(aColor, aBackColor, 0.55);
    end;
  Canvas.Pen.Color:=aColor;
  Canvas.Pen.Style:=psSolid;
  Canvas.Pen.Width:=LineWidth;
  if Style=ecsLinear then
    begin
      Canvas.MoveTo(GetCanvasPoint(MinX, ValueAtMin));
      for i:=0 to length(Points)-1 do
        Canvas.LineTo(GetCanvasPoint(Points[i].X, Points[i].Y));
      Canvas.LineTo(GetCanvasPoint(MaxX, ValueAtMax));
    end else
    begin
      SetLength(aPoints, 4+3*length(Points));
      aPoints[0]:=GetCanvasPoint(MinX, ValueAtMin);
      aPoints[length(aPoints)-1]:=GetCanvasPoint(MaxX, ValueAtMax);
      for i:=0 to length(Points)-1 do
        aPoints[3+i*3]:=GetCanvasPoint(Points[i].X, Points[i].Y);
      if Smoothness>0 then
        begin
          aSmoothX:=Smoothness;
          aSmoothY:=0;
        end else
        begin
          aSmoothX:=0;
          aSmoothY:=-Smoothness;
        end;
      for i:=0 to length(aPoints)-1 do
        begin
          case i mod 3 of
            1:
              begin
                 aPoints[i].X:=round(aPoints[i-1].X*(1-aSmoothX))+round(aPoints[i+2].X*aSmoothX);
                 aPoints[i].Y:=round(aPoints[i-1].Y*(1-aSmoothY))+round(aPoints[i+2].Y*aSmoothY);
              end;
            2:
              begin
                aPoints[i].X:=round(aPoints[i+1].X*(1-aSmoothX))+round(aPoints[i-2].X*aSmoothX);
                aPoints[i].Y:=round(aPoints[i+1].Y*(1-aSmoothY))+round(aPoints[i-2].Y*aSmoothY);
              end;
          end;
        end;
      Canvas.PolyBezier(aPoints, False, True);
    end;
  Canvas.AntialiasingMode:=amOff;
  { draw guidelines }
  if bEnabled and MouseEntered and (ecoGuidelines in Options) then
    begin
      aPoint:=Mouse.CursorPos;
      aPoint:=ScreenToControl(aPoint);
      aColor:=GuideColor;
      if aColor=clDefault then aColor:=clBtnText;
      Canvas.Pen.Color:=aColor;
      Canvas.Pen.Style:=psDot;
      Canvas.Pen.Width:=1;
      Canvas.Line(0, aPoint.Y, Width, aPoint.Y);
      Canvas.Line(aPoint.X, 0, aPoint.X, Height);
    end;
  { draw caption }
  if Caption<>'' then
    begin
      aDetails:=ThemeServices.GetElementDetails(cDetails[bEnabled]);
      aRect:=ThemeServices.GetTextExtent(Canvas.Handle, aDetails, Caption, 0, nil);
      OffsetRect(aRect, 2, 2);
      if IsRightToLeft then
        begin
          aRect.Left:=Width-aRect.Right;
          aRect.Right:=Width-2;
        end;
      ThemeServices.DrawText(Canvas, aDetails, Caption, aRect, 0, 0);
    end;
  { draw points }
  aColor:=PointColor;
  if aColor=clDefault then aColor:=clBtnText;
  if not bEnabled then
    begin
      aColor:=GetMonochromaticColor(aColor);
      aColor:=GetMergedColor(aColor, aBackColor, 0.55);
    end;
  aFPColor:=PointColorFixed;
  if aFPColor=clDefault then aFPColor:=clBtnText;
  if not bEnabled then
    begin
      aFPColor:=GetMonochromaticColor(aFPColor);
      aFPColor:=GetMergedColor(aFPColor, aBackColor, 0.55);
    end;
  Canvas.Pen.Color:=aColor;
  Canvas.Brush.Color:=aFPColor;
  Canvas.Pen.Style:=psSolid;
  Canvas.Pen.Width:=1;
  aPoint:=GetCanvasPoint(MinX, ValueAtMin);
  if not FRealRevered
    then aRect:=GetPointRect(aPoint, eppLeftSide)
    else aRect:=GetPointRect(aPoint, eppRightSide);
  if ecoFixedMin in Options
    then Canvas.FillRect(aRect)
    else Canvas.Frame(aRect);
  for i:=0 to length(Points)-1 do
    begin
      aPoint:=GetCanvasPoint(Points[i].X, Points[i].Y);
      aRect:=GetPointRect(aPoint, eppMiddle);
      if [epoFixedX, epoFixedY]*Points[i].Options=[epoFixedX, epoFixedY]
        then Canvas.FillRect(aRect)
        else Canvas.Frame(aRect);
    end;
  aPoint:=GetCanvasPoint(MaxX, ValueAtMax);
  if not FRealRevered
    then aRect:=GetPointRect(aPoint, eppRightSide)
    else aRect:=GetPointRect(aPoint, eppLeftSide);
  if ecoFixedMax in Options
    then Canvas.FillRect(aRect)
    else Canvas.Frame(aRect);
end;

procedure TCustomECConfCurve.SetCursor(Value: TCursor);
begin
  inherited SetCursor(Value);
  inherited SetCursor(Value);
  if not FCursorLock then FCursorBkgnd:=Value;
end;

{ Setters }

procedure TCustomECConfCurve.SetGridX(AValue: Single);
begin
  if FGridX=AValue then exit;
  FGridX:=AValue;
  if ecoShowGrid in Options then InvalidateNonUpdated;
end;

procedure TCustomECConfCurve.SetGridColor(AValue: TColor);
begin
  if FGridColor=AValue then exit;
  FGridColor:=AValue;
  if ecoShowGrid in Options then InvalidateNonUpdated;
end;

procedure TCustomECConfCurve.SetGridY(AValue: Single);
begin
  if FGridY=AValue then exit;
  FGridY:=AValue;
  if ecoShowGrid in Options then InvalidateNonUpdated;
end;

procedure TCustomECConfCurve.SetLineColor(AValue: TColor);
begin
  if FLineColor=AValue then exit;
  FLineColor:=AValue;
  InvalidateNonUpdated;
end;

procedure TCustomECConfCurve.SetLineWidth(AValue: SmallInt);
begin
  if FLineWidth=AValue then exit;
  FLineWidth:=AValue;
  InvalidateNonUpdated;
end;

procedure TCustomECConfCurve.SetMaxX(AValue: Single);
begin
  if FMaxX=AValue then exit;
  FMaxX:=AValue;
  Calculate;
  InvalidateNonUpdated;
end;

procedure TCustomECConfCurve.SetMaxY(AValue: Single);
begin
  if FMaxY=AValue then exit;
  FMaxY:=AValue;
  Calculate;
  InvalidateNonUpdated;
end;

procedure TCustomECConfCurve.SetMinX(AValue: Single);
begin
  if FMinX=AValue then exit;
  FMinX:=AValue;
  Calculate;
  InvalidateNonUpdated;
end;

procedure TCustomECConfCurve.SetMinY(AValue: Single);
begin
  if FMinY=AValue then exit;
  FMinY:=AValue;
  Calculate;
  InvalidateNonUpdated;
end;

procedure TCustomECConfCurve.SetOptions(AValue: TCurveOptions);
const cVisibleOpts = [ecoAntiAliasing, ecoFixedMin, ecoFixedMax, ecoReversed, ecoShowGrid];
var bInv: Boolean;
begin
  if FOptions=AValue then exit;
  bInv:= ((FOptions*cVisibleOpts)<>(AValue*cVisibleOpts));
  FOptions:=AValue;
  FRealRevered:= IsRightToLeft xor (ecoReversed in AValue);
  if bInv then InvalidateNonUpdated;
end;

procedure TCustomECConfCurve.SetPointColor(AValue: TColor);
begin
  if FPointColor=AValue then exit;
  FPointColor:=AValue;
  InvalidateNonUpdated;
end;

procedure TCustomECConfCurve.SetPointColorFixed(AValue: TColor);
begin
  if FPointColorFixed=AValue then exit;
  FPointColorFixed:=AValue;
  InvalidateNonUpdated;
end;

procedure TCustomECConfCurve.SetPointSize(AValue: SmallInt);
begin
  if FPointSize=AValue then exit;
  FPointSize:=AValue;
  InvalidateNonUpdated;
end;

procedure TCustomECConfCurve.SetSmoothness(AValue: Single);
begin
  if FSmoothness=AValue then exit;
  FSmoothness:=AValue;
  InvalidateNonUpdated;
end;

procedure TCustomECConfCurve.SetStyle(AValue: TCurveStyle);
begin
  if FStyle=AValue then exit;
  FStyle:=AValue;
  InvalidateNonUpdated;
end;

procedure TCustomECConfCurve.SetValueAtMax(AValue: Single);
begin
  if FValueAtMax=AValue then exit;
  FValueAtMax:=AValue;
  InvalidateNonUpdated;
end;

procedure TCustomECConfCurve.SetValueAtMin(AValue: Single);
begin
  if FValueAtMin=AValue then exit;
  FValueAtMin:=AValue;
  InvalidateNonUpdated;
end;

end.


