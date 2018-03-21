{**************************************************************************************************
 This file is part of the Eye Candy Controls (EC-C)

  Copyright (C) 2016 Vojtěch Čihák, Czech Republic

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

unit ECTriangle;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, LCLType, LMessages, Types, Themes, ECTypes;

type
  {$PACKENUM 2}
  TCaptionVisibility = (ecvNone, ecvTopCaptionOnly, ecvCaptions, ecvValues, ecvBoth);
  TColorFill = (ecfNone, ecfSimple, ecfThreeColors, ecfGradient);
  TMarkPos = (empCenter, empTop, empSide, empBottom,
              empTopH, empBottomH, empSideH,                                       { etroMarksHalf - around }
              empCenterHTop, empCenterHSide, empCenterHBottom,                     { etroMarksHalf - inner }
              empTopTH, empTopTL, empBottomTH, empBottomTL, empSideTL, empSideTH,  { etroMarksThirds - around }
              empCenterTTop, empCenterTSide, empCenterTBottom,                     { etroMarksThirds - inner }
              empTopQH, empTopQL, empBottomQH, empBottomQL, empSideQL, empSideQH,  { etroMarksQuarters - around }
              empCenterQTop, empCenterQSide, empCenterQBottom,
              empCenterQTopL, empCenterQSideL, empCenterQBottomL);                 { etroMarksThirds - inner }
  TPointerStyle = (epsCircle, epsCross, epsRectangle, epsViewFinder);
  TTriangleValueFormat = (etvfDecimal, etvfPercentual, etvfPerMille);
  TTriangleFlag = (etrfDragging,
                   etrfHovered,
                   etrfLockCursor,
                   etrfMarkHovered,
                   etrfNeedRecalc,
                   etrfNeedRedraw,
                   etrfPointerCoordsCached,
                   etrfPointerHovered);
  TTriangleFlags = set of TTriangleFlag;
  TTriangleOption = (etroKeepSumAvgOne,  { sum of average is 1 (centroid of triangle) }
                     etroMarksApexes,
                     etroMarksHalfs,
                     etroMarksThirds,
                     etroMarksQuarters,
                     etroReadonly,
                     etroReversed);
  TTriangleOptions = set of TTriangleOption;
  TValuePosition = (evpTop, evpBottom, evpSide);

  { TCustomECTriangle }
  TCustomECTriangle = class(TGraphicControl)
  private
    FCaptionBottom: TCaption;
    FCaptionSide: TCaption;
    FCaptionVisibility: TCaptionVisibility;
    FColorBottom: TColor;
    FColorSide: TColor;
    FColorTop: TColor;
    FColorFill: TColorFill;
    FIndent: SmallInt;
    FMarkSize: SmallInt;
    FMaxBottom: Single;
    FMaxSide: Single;
    FMaxTop: Single;
    FOnChange: TNotifyEvent;
    FOptions: TTriangleOptions;
    FPointerStyle: TPointerStyle;
    FRounding: Byte;
    FValueRelBottom: Single;
    FValueFormat: TTriangleValueFormat;
    FValueRelTop: Single;
    function GetValueAbsBottom: Single;
    function GetValueAbsSide: Single;
    function GetValueAbsTop: Single;
    function GetValueRelSide: Single;
    procedure SetCaptionBottom(AValue: TCaption);
    procedure SetCaptionSide(AValue: TCaption);
    procedure SetCaptionVisibility(AValue: TCaptionVisibility);
    procedure SetColorBottom(AValue: TColor);
    procedure SetColorSide(AValue: TColor);
    procedure SetColorTop(AValue: TColor);
    procedure SetColorFill(AValue: TColorFill);
    procedure SetIndent(AValue: SmallInt);
    procedure SetMarkSize(AValue: SmallInt);
    procedure SetMaxBottom(AValue: Single);
    procedure SetMaxSide(AValue: Single);
    procedure SetMaxTop(AValue: Single);
    procedure SetOptions(AValue: TTriangleOptions);
    procedure SetPointerStyle(AValue: TPointerStyle);
    procedure SetRounding(AValue: Byte);
    procedure SetValueFormat(AValue: TTriangleValueFormat);
  protected const
    cDefCaptionVisi = ecvCaptions;
    cDefColorBottom = clBtnShadow;
    cDefColorFill = ecfGradient;
    cDefColorSide = clHighlightText;
    cDefColorTop = clHighlight;
    cDefIndent = 4;
    cDefMarkSize = 7;
    cDefOption = [etroMarksApexes, etroMarksHalfs];
    cDefPointerStyle = epsRectangle;
    cDefRounding = 2;
  protected
    DefCursor: TCursor;
    Flags: TTriangleFlags;
    HoveredMark: TMarkPos;
    MarkRects: array[TMarkPos] of TRect;
    PointerRect: TRect;
    PointerX, PointerY: Integer;
    PrevHeight, PrevWidth: Integer;
    PtTopBMP,PtSideBMP, PtBottomBMP: TPoint;
    TriBMP: TBitmap;
    procedure BeginUpdate;
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: Integer;
                                     {%H-}WithThemeSpace: Boolean); override;
    procedure ChangeCursor(AHovered: Boolean);
    procedure CMBiDiModeChanged(var {%H-}Message: TLMessage); message CM_BIDIMODECHANGED;
    procedure Calculate;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoSetValues(ATop, ABottom: Single; ACalcPointerPos: Boolean): Boolean;
    procedure DrawTriangleBMP;
    procedure EnabledChanged; override;
    procedure EndUpdate;
    class function GetControlClassDefaultSize: TSize; override;
    procedure InvalidateNonUpdated;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure PlacePointer;
    procedure Recalc;
    procedure RecalcRedraw;
    procedure Redraw;
    procedure ResetHovered;
    procedure Resize; override;
    procedure SetAutoSize(Value: Boolean); override;
    procedure SetCursor(Value: TCursor); override;
    procedure TextChanged; override;
  public
    UpdateCount: SmallInt;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetValues(X, Y: Integer); overload;
    procedure SetValues(ATop, ABottom: Single); overload;
    property CaptionBottom: TCaption read FCaptionBottom write SetCaptionBottom;
    property CaptionSide: TCaption read FCaptionSide write SetCaptionSide;
    property CaptionVisibility: TCaptionVisibility read FCaptionVisibility write SetCaptionVisibility default cDefCaptionVisi;
    property ColorBottom: TColor read FColorBottom write SetColorBottom default clDefault;
    property ColorFill: TColorFill read FColorFill write SetColorFill default cDefColorFill;
    property ColorSide: TColor read FColorSide write SetColorSide default clDefault;
    property ColorTop: TColor read FColorTop write SetColorTop default clDefault;
    property Indent: SmallInt read FIndent write SetIndent default cDefIndent;
    property MarkSize: SmallInt read FMarkSize write SetMarkSize default cDefMarkSize;
    property MaxBottom: Single read FMaxBottom write SetMaxBottom;
    property MaxSide: Single read FMaxSide write SetMaxSide;
    property MaxTop: Single read FMaxTop write SetMaxTop;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Options: TTriangleOptions read FOptions write SetOptions default cDefOption;
    property PointerStyle: TPointerStyle read FPointerStyle write SetPointerStyle default cDefPointerStyle;
    property Rounding: Byte read FRounding write SetRounding default cDefRounding;
    property ValueAbsBottom: Single read GetValueAbsBottom;
    property ValueAbsSide: Single read GetValueAbsSide;
    property ValueAbsTop: Single read GetValueAbsTop;
    property ValueFormat: TTriangleValueFormat read FValueFormat write SetValueFormat default etvfDecimal;
    property ValueRelBottom: Single read FValueRelBottom;
    property ValueRelSide: Single read GetValueRelSide;
    property ValueRelTop: Single read FValueRelTop;
  end;

  { TECTriangle }
  TECTriangle = class(TCustomECTriangle)
  published
    property Align;
    property Anchors;
    property AutoSize default True;
    property BiDiMode;
    property BorderSpacing;
    property Color;
    property Caption;
    property CaptionBottom;
    property CaptionSide;
    property CaptionVisibility;
    property ColorBottom;
    property ColorFill;
    property ColorSide;
    property ColorTop;
    property Constraints;
    property Enabled;
    property Font;
    property Indent;
    property MarkSize;
    property MaxBottom;
    property MaxSide;
    property MaxTop;
    property Options;
    property PopupMenu;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PointerStyle;
    property Rounding;
    property ShowHint;
    property ValueAbsBottom;
    property ValueAbsSide;
    property ValueAbsTop;
    property ValueFormat;
    property ValueRelBottom;
    property ValueRelSide;
    property ValueRelTop;
    property Visible;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnPaint;
    property OnResize;
  end;

implementation

const
  c1Third = 1/3;
  c2Thirds = 2/3;
  c1Sixth= 1/6;
  cValues: array[TMarkPos, evpTop..evpBottom] of Single =
    ((c1Third, c1Third), (1, 0), (0, 0), (0, 1), (0.5, 0), (0, 0.5), (0.5, 0.5), (0.5, 0.25),
     (0.25, 0.25), (0.25, 0.5), (c2Thirds, 0),(c1Third, 0), (0, c1Third), (0, c2Thirds),
     (c1Third, c2Thirds), (c2Thirds, c1Third), (c2Thirds, c1Sixth), (c1Sixth, c1Sixth),
     (c1Sixth, c2Thirds), (0.75, 0), (0.25, 0), (0, 0.25), (0, 0.75), (0.25, 0.75), (0.75, 0.25),
     (0.75, 0.125), (0.125, 0.125), (0.125, 0.75), (0.375, 0.25), (0.375, 0.375), (0.25, 0.375));

{ TCustomECTringle }

constructor TCustomECTriangle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaptionVisibility:=cDefCaptionVisi;
  FColorBottom:=clDefault;
  FColorSide:=clDefault;
  FColorTop:=clDefault;
  FColorFill:=cDefColorFill;
  FIndent:=cDefIndent;
  FMarkSize:=cDefMarkSize;
  FMaxBottom:=1;
  FMaxSide:=1;
  FMaxTop:=1;
  FOptions:=cDefOption;
  FPointerStyle:=cDefPointerStyle;
  FRounding:=cDefRounding;
  FValueRelTop:=1.0;
  TriBMP:=TBitmap.Create;
  with TriBMP do
    begin
      Transparent:=True;
      TransparentColor:=clForm;
      Canvas.Pen.Style:=psSolid;
      Canvas.Pen.Width:=1;
    end;
  DefCursor:=Cursor;
  AutoSize:=True;
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, cx, cy);
end;

destructor TCustomECTriangle.Destroy;
begin
  TriBMP.Free;
  inherited Destroy;
end;

procedure TCustomECTriangle.BeginUpdate;
begin
  inc(UpdateCount);
end;

procedure TCustomECTriangle.CalculatePreferredSize(var PreferredWidth,
            PreferredHeight: Integer; WithThemeSpace: Boolean);
begin
  PreferredWidth:=0;
  PreferredHeight:=round(1.15470054*(Width-2*Indent))+2*Indent;
end;

procedure TCustomECTriangle.Calculate;
var aIndent, aMarkSize: SmallInt;
    aWidthBMP, aHeightBMP: Integer;
    aPt: TPoint;
    bReversed: Boolean;
begin
  aIndent:=Indent;
  aWidthBMP:=Width-2*aIndent;
  aHeightBMP:=Height-2*aIndent;
  TriBMP.SetSize(aWidthBMP, aHeightBMP);
  bReversed:= (etroReversed in Options) xor IsRightToLeft;
  aMarkSize:=MarkSize;
  PtTopBMP.Y:=0;
  PtSideBMP.Y:=Height div 2 -aIndent;
  PtBottomBMP.Y:=aHeightBMP-1;
  aPt.X:=Width-aIndent-1;
  aPt.Y:=Height div 2;
  if not bReversed then
    begin
      MarkRects[empTop]:=PointToRect(Point(aIndent, aIndent), aMarkSize);
      MarkRects[empSide]:=PointToRect(aPt, aMarkSize);
      MarkRects[empBottom]:=PointToRect(Point(aIndent, Height-aIndent-1), aMarkSize);
      PtTopBMP.X:=0;
      PtSideBMP.X:=aWidthBMP-1;
      PtBottomBMP.X:=0;
      aPt.X:=aWidthBMP div 3 +aIndent;
    end else
    begin
      MarkRects[empTop]:=PointToRect(Point(aPt.X, aIndent), aMarkSize);
      MarkRects[empSide]:=PointToRect(Point(aIndent, aPt.Y), aMarkSize);
      MarkRects[empBottom]:=PointToRect(Point(aPt.X, Height-aIndent-1), aMarkSize);
      PtTopBMP.X:=aWidthBMP-1;
      PtSideBMP.X:=0;
      PtBottomBMP.X:=aWidthBMP-1;
      aPt.X:=2*aWidthBMP div 3 +aIndent;
    end;
  MarkRects[empCenter]:=PointToRect(aPt, aMarkSize);
  if ([etroMarksHalfs, etroMarksQuarters]*Options)<>[] then
    begin
      if not bReversed
        then aPt.X:=aIndent
        else aPt.X:=Width-aIndent-1;
      aPt.Y:=Height div 2;
      MarkRects[empSideH]:=PointToRect(aPt, aMarkSize);
      aPt.X:=Width div 2;
      aPt.Y:=aHeightBMP div 4 +aIndent;
      MarkRects[empTopH]:=PointToRect(aPt, aMarkSize);
      aPt.Y:=Height-aPt.Y;
      MarkRects[empBottomH]:=PointToRect(aPt, aMarkSize);
      aPt.X:=aWidthBMP div 2 +aIndent;
      aPt.Y:=Height div 2;
      MarkRects[empCenterHSide]:=PointToRect(aPt, aMarkSize);
      aPt.X:=aWidthBMP div 4 +aIndent;
      if bReversed then aPt.X:=Width-aPt.X;
      aPt.Y:=3*aHeightBMP div 8 +aIndent;
      MarkRects[empCenterHTop]:=PointToRect(aPt, aMarkSize);
      aPt.Y:=Height-aPt.Y;
      MarkRects[empCenterHBottom]:=PointToRect(aPt, aMarkSize);
    end;
  if etroMarksThirds in Options then
    begin
      if not bReversed
        then aPt.X:=aIndent
        else aPt.X:=Width-aIndent-1;
      aPt.Y:=aHeightBMP div 3 +aIndent;
      MarkRects[empSideTH]:=PointToRect(aPt, aMarkSize);
      aPt.Y:=Height-aPt.Y;
      MarkRects[empSideTL]:=PointToRect(aPt, aMarkSize);
      if not bReversed
        then aPt.X:=2*aWidthBMP div 3 +aIndent
        else aPt.X:=aWidthBMP div 3 +aIndent;
      aPt.Y:=aHeightBMP div 3 +aIndent;
      MarkRects[empTopTL]:=PointToRect(aPt, aMarkSize);
      aPt.Y:=Height-aPt.Y;
      MarkRects[empBottomTH]:=PointToRect(aPt, aMarkSize);
      aPt.X:=Width-aPt.X;
      aPt.Y:=aHeightBMP div 6 +aIndent;
      MarkRects[empTopTH]:=PointToRect(aPt, aMarkSize);
      aPt.Y:=Height-aPt.Y;
      MarkRects[empBottomTL]:=PointToRect(aPt, aMarkSize);
      aPt.X:=2*aWidthBMP div 3 +aIndent;
      if bReversed then aPt.X:=Width-aPt.X;
      aPt.Y:=Height div 2;
      MarkRects[empCenterTSide]:=PointToRect(aPt, aMarkSize);
      aPt.X:=aWidthBMP div 6 +aIndent;
      if bReversed then aPt.X:=Width-aPt.X;
      aPt.Y:=aHeightBMP div 4 +aIndent;
      MarkRects[empCenterTTop]:=PointToRect(aPt, aMarkSize);
      aPt.Y:=Height-aPt.Y;
      MarkRects[empCenterTBottom]:=PointToRect(aPt, aMarkSize);
    end;
  if etroMarksQuarters in Options then
    begin
      if not bReversed
        then aPt.X:=aIndent
        else aPt.X:=Width-aIndent-1;
      aPt.Y:=aHeightBMP div 4 +aIndent;
      MarkRects[empSideQH]:=PointToRect(aPt, aMarkSize);
      aPt.Y:=Height-aPt.Y;
      MarkRects[empSideQL]:=PointToRect(aPt, aMarkSize);
      if not bReversed
        then aPt.X:=3*aWidthBMP div 4 +aIndent
        else aPt.X:=aWidthBMP div 4 +aIndent;
      aPt.Y:=3*aHeightBMP div 8 +aIndent;
      MarkRects[empTopQL]:=PointToRect(aPt, aMarkSize);
      aPt.Y:=Height-aPt.Y;
      MarkRects[empBottomQH]:=PointToRect(aPt, aMarkSize);
      aPt.X:=Width-aPt.X;
      aPt.Y:=aHeightBMP div 8 +aIndent;
      MarkRects[empTopQH]:=PointToRect(aPt, aMarkSize);
      aPt.Y:=Height-aPt.Y;
      MarkRects[empBottomQL]:=PointToRect(aPt, aMarkSize);
      aPt.X:=3*aWidthBMP div 4 +aIndent;
      if bReversed then aPt.X:=Width-aPt.X;
      aPt.Y:=Height div 2;
      MarkRects[empCenterQSide]:=PointToRect(aPt, aMarkSize);
      aPt.X:=aWidthBMP div 8 +aIndent;
      if bReversed then aPt.X:=Width-aPt.X;
      aPt.Y:=3*aHeightBMP div 16 +aIndent;
      MarkRects[empCenterQTop]:=PointToRect(aPt, aMarkSize);
      aPt.Y:=Height-aPt.Y;
      MarkRects[empCenterQBottom]:=PointToRect(aPt, aMarkSize);
      aPt.X:=aWidthBMP div 4 +aIndent;
      if bReversed then aPt.X:=Width-aPt.X;
      aPt.Y:=Height div 2;
      MarkRects[empCenterQSideL]:=PointToRect(aPt, aMarkSize);
      aPt.X:=3*aWidthBMP div 8 +aIndent;
      if bReversed then aPt.X:=Width-aPt.X;
      aPt.Y:=7*aHeightBMP div 16 +aIndent;
      MarkRects[empCenterQTopL]:=PointToRect(aPt, aMarkSize);
      aPt.Y:=Height-aPt.Y;
      MarkRects[empCenterQBottomL]:=PointToRect(aPt, aMarkSize);
    end;
end;

function TCustomECTriangle.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result:=inherited DoMouseWheelDown(Shift, MousePos);
  SetValues(PointerX, PointerY+1);
end;

function TCustomECTriangle.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result:=inherited DoMouseWheelUp(Shift, MousePos);
  SetValues(PointerX, PointerY-1);
end;

procedure TCustomECTriangle.ChangeCursor(AHovered: Boolean);
begin
  include(Flags, etrfLockCursor);
  if AHovered
    then Cursor:=crHandPoint
    else Cursor:=DefCursor;
  exclude(Flags, etrfLockCursor);
end;

procedure TCustomECTriangle.CMBiDiModeChanged(var Message: TLMessage);
begin
  RecalcRedraw;
end;

function TCustomECTriangle.DoSetValues(ATop, ABottom: Single; ACalcPointerPos: Boolean): Boolean;
begin
  Result:= ((FValueRelTop<>ATop) or (FValueRelBottom<>ABottom));
  if Result then
    begin
      FValueRelTop:=ATop;
      FValueRelBottom:=ABottom;
      if ACalcPointerPos then exclude(Flags, etrfPointerCoordsCached);
      if assigned(OnChange) then OnChange(self);
      InvalidateNonUpdated;
    end;
end;

procedure TCustomECTriangle.DrawTriangleBMP;
var i, aHeight, aHeight_2, aWidth_2: Integer;
    aPoint: TPoint;
    aRatio: Single;
    aRect: TRect;
    aTopColor, aBottomColor, aSideColor, aPenColor: TColor;
begin
  TriBMP.Transparent:=False;
  TriBMP.TransparentClear;
  TriBMP.Transparent:=True;
  aHeight:=TriBMP.Height;
  aTopColor:=GetColorResolvingDefault(ColorTop, cDefColorTop);
  aSideColor:=GetColorResolvingDefault(ColorSide, cDefColorSide);;
  aBottomColor:=GetColorResolvingDefault(ColorBottom, cDefColorBottom);
  aPenColor:=clBtnShadow;
  if not IsEnabled then
    begin
      aTopColor:=GetMonochromaticColor(aTopColor);
      aSideColor:=GetMonochromaticColor(aSideColor);
      aBottomColor:=GetMonochromaticColor(aBottomColor);
      aPenColor:=GetMonochromaticColor(aPenColor);
    end;
  case ColorFill of
    ecfSimple:
      with TriBMP.Canvas do
        begin
          Pen.Color:=aPenColor;
          AntialiasingMode:=amOn;
          Brush.Color:=aTopColor;
          Brush.Style:=bsSolid;
          Polygon([PtTopBMP, PtSideBMP, PtBottomBMP]);
        end;
    ecfThreeColors:
       with TriBMP.Canvas do
        begin
          Pen.Color:=aPenColor;
          AntialiasingMode:=amOn;
          aPoint.X:=PtTopBMP.X;
          aPoint.Y:=PtSideBMP.Y;
          aWidth_2:=TriBMP.Width div 2;
          Brush.Color:=aTopColor;
          Polygon([PtTopBMP, Point(aWidth_2, PtSideBMP.Y div 2), aPoint]);
          Brush.Color:=aSideColor;
          Polygon([Point(aWidth_2, PtSideBMP.Y div 2), PtSideBMP, Point(aWidth_2, 3*aHeight div 4)]);
          Brush.Color:=aBottomColor;
          Polygon([aPoint, Point(aWidth_2, 3*aHeight div 4), PtBottomBMP]);
        end;
    ecfGradient:
      begin
        with TriBMP do
          begin
            Canvas.AntialiasingMode:=amOff;
            aHeight_2:=(Height-1) div 2;
            aWidth_2:=(Width-1) div 2;
            BeginUpdate(True);
            if not ((etroReversed in Options) xor IsRightToLeft) then
              begin
                aRect.Left:=0;
                aRect.Right:=2;
                for i:=0 to aWidth_2 do
                  begin
                    aRatio:=i/aWidth_2;
                    aRect.Top:=trunc(aRatio*aHeight_2)+1;
                    aRect.Bottom:=aHeight-aRect.Top-1;
                    Canvas.GradientFill(aRect, GetMergedColor(aSideColor, aTopColor, aRatio),
                                        GetMergedColor(aSideColor, aBottomColor, aRatio), gdVertical);
                    aRect.Left:=aRect.Right;
                    inc(aRect.Right, 2);
                  end;
              end else
              begin
                aRect.Right:=Width-1;
                aRect.Left:=aRect.Right-2;
                for i:=aWidth_2 downto 0 do
                  begin
                    aRatio:=i/aWidth_2;
                    aRect.Top:=trunc((1-aRatio)*aHeight_2)+1;
                    aRect.Bottom:=aHeight-aRect.Top-1;
                    Canvas.GradientFill(aRect, GetMergedColor(aTopColor, aSideColor, aRatio),
                                        GetMergedColor(aBottomColor, aSideColor, aRatio), gdVertical);
                    dec(aRect.Left, 2);
                    dec(aRect.Right, 2);
                  end;
              end;
            EndUpdate(False);
          end;
      end;
  end;  {case}
  if ColorFill in [ecfNone, ecfGradient] then
    with TriBMP.Canvas do
      begin
        Pen.Color:=aPenColor;
        AntialiasingMode:=amOn;
        Brush.Style:=bsClear;
        Polygon([PtTopBMP, PtSideBMP, PtBottomBMP]);
      end;
end;

procedure TCustomECTriangle.EnabledChanged;
begin
  inherited EnabledChanged;
  Redraw;
end;

procedure TCustomECTriangle.EndUpdate;
begin
  dec(UpdateCount);
  RecalcRedraw;
end;

class function TCustomECTriangle.GetControlClassDefaultSize: TSize;
begin
  Result.cx:=200;
  Result.cy:=230;
end;

procedure TCustomECTriangle.InvalidateNonUpdated;
begin
  if UpdateCount=0 then Invalidate;
end;

procedure TCustomECTriangle.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var aPt: TPoint;

  function MarkClicked(AMarkPos: TMarkPos): Boolean;
  begin
    Result:=PtInRect(MarkRects[AMarkPos], aPt);
    if Result then
      begin
        aPt:=RectToPoint(MarkRects[AMarkPos]);
        PointerX:=aPt.X;
        PointerY:=aPt.Y;
        include(Flags, etrfPointerCoordsCached);
        DoSetValues(cValues[AMarkPos, evpTop], cValues[AMarkPos, evpBottom], False);
      end;
  end;

var aMarkPos: TMarkPos;
    bMarkClicked: Boolean;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if etroReadonly in Options then exit;  { Exit; }
  if Button=mbLeft then
    begin
      if etrfHovered in Flags then
        begin
          include(Flags, etrfDragging);
        end else
        begin
          aPt:=Point(X, Y);
          bMarkClicked:=False;
          if (([etroMarksApexes, etroMarksHalfs,  etroMarksThirds, etroMarksQuarters]*Options)<>[])
            then bMarkClicked:=MarkClicked(empCenter);
          if not bMarkClicked and (etroMarksApexes in Options) then
            for aMarkPos:=empTop to empBottom do
              begin
                bMarkClicked:=MarkClicked(aMarkPos);
                if bMarkClicked then break;
              end;
          if not bMarkClicked and (([etroMarksHalfs, etroMarksQuarters]*Options)<>[]) then
            for aMarkPos:=empTopH to empCenterHBottom do
              begin
                bMarkClicked:=MarkClicked(aMarkPos);
                if bMarkClicked then break;
              end;
          if not bMarkClicked and (etroMarksThirds in Options) then
            for aMarkPos:=empTopTH to empCenterTBottom do
              begin
                bMarkClicked:=MarkClicked(aMarkPos);
                if bMarkClicked then break;
              end;
          if not bMarkClicked and (etroMarksQuarters in Options) then
            for aMarkPos:=empTopQH to empCenterQBottomL do
              begin
                bMarkClicked:=MarkClicked(aMarkPos);
                if bMarkClicked then break;
              end;
          if not bMarkClicked then SetValues(X, Y);
        end;
    end;
end;

procedure TCustomECTriangle.MouseMove(Shift: TShiftState; X, Y: Integer);
var aMarkSize2: Integer;
begin
  inherited MouseMove(Shift, X, Y);
  if not (etroReadonly in Options) then
    begin
      if not (etrfDragging in Flags) then
        begin
          aMarkSize2:=MarkSize+2;
          if (X>(PointerX-aMarkSize2)) and (X<(aMarkSize2+PointerX)) and
            (Y>(PointerY-aMarkSize2)) and (Y<(aMarkSize2+PointerY)) then
              begin
                include(Flags, etrfHovered);
                ChangeCursor(True);
              end else
              ResetHovered;
        end else
        SetValues(X, Y);
    end;
end;

procedure TCustomECTriangle.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if (Button=mbLeft) and (etrfDragging in Flags) then
    begin
      exclude(Flags, etrfDragging);
      ChangeCursor(False);
    end;
end;

procedure TCustomECTriangle.Paint;

  function GetFormatedValue(AValue: Single): string;
  begin
    case ValueFormat of
      etvfDecimal: Result:=floatToStrF(AValue, ffFixed, 1, Rounding);
      etvfPercentual: Result:=floatToStrF(100*AValue, ffFixed, 3, Rounding)+'%';
      etvfPerMille: Result:=floatToStrF(1000*AValue, ffFixed, 4, Rounding)+'‰';
    end;
  end;

var aBaseFlags, aFlags: Cardinal;
    aDetails: TThemedElementDetails;
    aMarkPos: TMarkPos;
    aMaxSum: Single;
    aRect: TRect;
    aText: string;
    aTextHeight: SmallInt;
    bEnabled, bR2L: Boolean;
begin
  bEnabled:=IsEnabled;
  bR2L:=IsRightToLeft;
  if etrfNeedRecalc in Flags then
    begin
      Calculate;
      exclude(Flags, etrfNeedRecalc);
    end;
  if etrfNeedRedraw in Flags then
    begin
      DrawTriangleBMP;
      exclude(Flags, etrfNeedRedraw);
    end;
  if Color<>clDefault then
    begin
      Canvas.Brush.Color:=Color;
      Canvas.FillRect(ClientRect);
    end;
  with Canvas do
    begin
      Draw(Indent, Indent, TriBMP);
      if bEnabled
        then Pen.Color:=clBtnText
        else Pen.Color:=GetMonochromaticColor(clBtnText);
      Pen.Style:=psSolid;
      Pen.Width:=1;
      if ([etroMarksApexes, etroMarksHalfs, etroMarksThirds, etroMarksQuarters]*Options)<>[]
        then Frame(MarkRects[empCenter]);
      if etroMarksApexes in Options then
        for aMarkPos:=empTop to empBottom do
          Frame(MarkRects[aMarkPos]);
      if ([etroMarksHalfs, etroMarksQuarters]*Options)<>[] then
        for aMarkPos:=empTopH to empCenterHBottom do
          Frame(MarkRects[aMarkPos]);
      if etroMarksThirds in Options then
        for aMarkPos:=empTopTH to empCenterTBottom do
          Frame(MarkRects[aMarkPos]);
      if etroMarksQuarters in Options then
        for aMarkPos:=empTopQH to empCenterQBottomL do
          Frame(MarkRects[aMarkPos]);
    end;
  if CaptionVisibility<>ecvNone then
    begin
      if bEnabled
        then aDetails:=ThemeServices.GetElementDetails(tbPushButtonNormal)
        else aDetails:=ThemeServices.GetElementDetails(tbPushButtonDisabled);
      aBaseFlags:=DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX;
      if bR2L then aBaseFlags:=aBaseFlags or DT_RTLREADING;
      aFlags:=aBaseFlags or DT_CENTER;
      if CaptionVisibility in [ecvTopCaptionOnly, ecvCaptions, ecvBoth] then aText:=Caption;
      if CaptionVisibility in [ecvValues, ecvBoth] then
        begin
          if not (etroKeepSumAvgOne in Options)
            then aMaxSum:=1
            else aMaxSum:=(MaxTop+MaxSide+MaxBottom)/3;
          if aText<>'' then aText:=aText+' ';
          aText:=aText+GetFormatedValue(ValueRelTop*MaxTop/aMaxSum);
        end;
      aTextHeight:=Canvas.TextHeight(aText);
      aRect:=Rect(Indent, Indent, Width-3*Indent, Indent+aTextHeight);
      ThemeServices.DrawText(Canvas, aDetails, aText, aRect, aFlags, 0);
      if CaptionVisibility<>ecvTopCaptionOnly then
        begin
          aText:='';
          if CaptionVisibility in [ecvCaptions, ecvBoth] then aText:=CaptionBottom;
          if CaptionVisibility in [ecvValues, ecvBoth] then
            begin
              if aText<>'' then aText:=aText+' ';
              aText:=aText+GetFormatedValue(ValueRelBottom*MaxBottom/aMaxSum);
            end;
          aRect:=Rect(Indent, Height-Indent-aTextHeight, Width-3*Indent, Height-Indent);
          ThemeServices.DrawText(Canvas, aDetails, aText, aRect, aFlags, 0);
          aFlags:=aBaseFlags;
          if not (bR2L xor (etroReversed in Options)) then aFlags:=aFlags or DT_RIGHT;
          aText:='';
          if CaptionVisibility in [ecvCaptions, ecvBoth] then aText:=CaptionSide;
          if CaptionVisibility in [ecvValues, ecvBoth] then
            begin
              if aText<>'' then aText:=aText+' ';
              aText:=aText+GetFormatedValue(ValueRelSide*MaxSide/aMaxSum);
            end;
          aRect:=Rect(Indent, Height div 5, Width-Indent, Height div 5 +aTextHeight);
          ThemeServices.DrawText(Canvas, aDetails, aText, aRect, aFlags, 0);
        end;
    end;
  if bEnabled
    then Canvas.Brush.Color:=clBtnHighlight
    else Canvas.Brush.Color:=GetMonochromaticColor(clBtnHighlight);
  if not (etrfPointerCoordsCached in Flags) then PlacePointer;
  if PointerStyle in [epsCircle, epsRectangle, epsViewFinder] then
    aRect:=PointToRect(Point(PointerX, PointerY), MarkSize+2);
  case PointerStyle of
    epsCircle: Canvas.Ellipse(aRect);
    epsCross, epsViewFinder:
      begin
        Canvas.Line(PointerX-MarkSize, PointerY, PointerX+MarkSize+1, PointerY);
        Canvas.Line(PointerX, PointerY-MarkSize, PointerX, PointerY+MarkSize+1);
        if PointerStyle=epsViewFinder then Canvas.Frame(aRect);
      end;
    epsRectangle: Canvas.Rectangle(aRect);
  end;
  inherited Paint;
end;

procedure TCustomECTriangle.PlacePointer;
var aRelXPos: Single;
begin
  PointerX:=Indent+round((TriBMP.Width-1)*ValueRelSide);
  if (etroReversed in Options) xor IsRightToLeft then
    begin
      PointerX:=Width-PointerX;
      aRelXPos:=(PointerX-Indent)/(TriBMP.Width-1);
    end else
    aRelXPos:=1-(PointerX-Indent)/(TriBMP.Width-1);
  PointerY:=Indent+round(PtSideBMP.Y*(1-((ValueRelTop-ValueRelBottom)/(ValueRelTop+ValueRelBottom))*aRelXPos));
  include(Flags, etrfPointerCoordsCached);
end;

procedure TCustomECTriangle.Recalc;
begin
  include(Flags, etrfNeedRecalc);
  if UpdateCount=0 then Invalidate;
end;

procedure TCustomECTriangle.RecalcRedraw;
begin
  Flags:=Flags+[etrfNeedRecalc, etrfNeedRedraw]-[etrfPointerCoordsCached];
  if UpdateCount=0 then Invalidate;
end;

procedure TCustomECTriangle.Redraw;
begin
  include(Flags, etrfNeedRedraw);
  if UpdateCount=0 then Invalidate;
end;

procedure TCustomECTriangle.ResetHovered;
begin
  exclude(Flags, etrfHovered);
  ChangeCursor(False);
end;

procedure TCustomECTriangle.Resize;
begin
  inherited Resize;
  if AutoSize then
    begin
      if Width<>PrevWidth then
        begin
          InvalidatePreferredSize;
          RecalcRedraw;
        end;
    end else
    if (Width<>PrevWidth) or (Height<>PrevHeight) then
      begin
        PrevWidth:=Width;
        PrevHeight:=Height;
        RecalcRedraw;
      end;
end;

procedure TCustomECTriangle.SetAutoSize(Value: Boolean);
begin
  inherited SetAutoSize(Value);
  if Value then
    begin
      InvalidatePreferredSize;
      RecalcRedraw;
    end;
end;

procedure TCustomECTriangle.SetCursor(Value: TCursor);
begin
  inherited SetCursor(Value);
  if not (etrfLockCursor in Flags) then DefCursor:=Value;
end;

procedure TCustomECTriangle.SetValues(X, Y: Integer);
var aTop, aSide, aBottom, aHelpY: Single;
    aIndent: SmallInt;
    aMaxY, aTopY, aWidth: Integer;
    bReversed: Boolean;
begin
  bReversed:= (etroReversed in Options) xor IsRightToLeft;
  aIndent:=Indent;
  dec(X, aIndent);
  dec(Y, aIndent);
  aWidth:=TriBMP.Width-1;
  if X<0 then X:=0;
  if X>aWidth then X:=aWidth;
  aHelpY:=X*PtSideBMP.Y/aWidth;
  if bReversed then aHelpY:=PtSideBMP.Y-aHelpY;
  aMaxY:=round(aHelpY);
  aTopY:=aMaxY;
  if Y<aMaxY then Y:=aMaxY;
  aMaxY:=round(PtBottomBMP.Y-aHelpY)+1;
  if Y>aMaxY then Y:=aMaxY;
  PointerX:=X+aIndent;
  PointerY:=Y+aIndent;
  include(Flags, etrfPointerCoordsCached);
  aSide:=X/aWidth;
  if bReversed then aSide:=1-aSide;
  dec(Y, aTopY);
  dec(aMaxY, aTopY);
  if aMaxY>0
    then aTop:=(1-aSide)*(1-Y/aMaxY)
    else aTop:=0;
  aBottom:=1-aSide-aTop;
  DoSetValues(aTop, aBottom, False);
end;

procedure TCustomECTriangle.SetValues(ATop, ABottom: Single);
begin
  DoSetValues(ATop, ABottom, True);
end;

procedure TCustomECTriangle.TextChanged;
begin
  inherited TextChanged;
  if CaptionVisibility in [ecvTopCaptionOnly, ecvCaptions, ecvBoth] then InvalidateNonUpdated;
end;

{ G/Setters }

function TCustomECTriangle.GetValueAbsBottom: Single;
begin
  Result:=FValueRelBottom*FMaxBottom;
end;

function TCustomECTriangle.GetValueAbsSide: Single;
begin
  Result:=GetValueRelSide*FMaxSide;
end;

function TCustomECTriangle.GetValueAbsTop: Single;
begin
  Result:=FValueRelTop*FMaxTop;
end;

function TCustomECTriangle.GetValueRelSide: Single;
begin
  Result:=1-FValueRelBottom-FValueRelTop;
end;

procedure TCustomECTriangle.SetCaptionBottom(AValue: TCaption);
begin
  if FCaptionBottom=AValue then exit;
  FCaptionBottom:=AValue;
  if CaptionVisibility in [ecvCaptions, ecvBoth] then InvalidateNonUpdated;
end;

procedure TCustomECTriangle.SetCaptionSide(AValue: TCaption);
begin
  if FCaptionSide=AValue then exit;
  FCaptionSide:=AValue;
  if CaptionVisibility in [ecvCaptions, ecvBoth] then InvalidateNonUpdated;
end;

procedure TCustomECTriangle.SetCaptionVisibility(AValue: TCaptionVisibility);
begin
  if FCaptionVisibility=AValue then exit;
  FCaptionVisibility:=AValue;
  InvalidateNonUpdated;
end;

procedure TCustomECTriangle.SetColorBottom(AValue: TColor);
begin
  if FColorBottom=AValue then exit;
  FColorBottom:=AValue;
  Redraw;
end;

procedure TCustomECTriangle.SetColorSide(AValue: TColor);
begin
  if FColorSide=AValue then exit;
  FColorSide:=AValue;
  Redraw;
end;

procedure TCustomECTriangle.SetColorTop(AValue: TColor);
begin
  if FColorTop=AValue then exit;
  FColorTop:=AValue;
  Redraw;
end;

procedure TCustomECTriangle.SetColorFill(AValue: TColorFill);
begin
  if FColorFill=AValue then exit;
  FColorFill:=AValue;
  Redraw;
end;

procedure TCustomECTriangle.SetIndent(AValue: SmallInt);
begin
  if FIndent=AValue then exit;
  FIndent:=AValue;
  RecalcRedraw;
end;

procedure TCustomECTriangle.SetMarkSize(AValue: SmallInt);
begin
  if FMarkSize=AValue then exit;
  FMarkSize:=AValue;
  Recalc;
end;

procedure TCustomECTriangle.SetMaxBottom(AValue: Single);
begin
  if FMaxBottom=AValue then exit;
  FMaxBottom:=AValue;
  InvalidateNonUpdated
end;

procedure TCustomECTriangle.SetMaxSide(AValue: Single);
begin
  if FMaxSide=AValue then exit;
  FMaxSide:=AValue;
  InvalidateNonUpdated
end;

procedure TCustomECTriangle.SetMaxTop(AValue: Single);
begin
  if FMaxTop=AValue then exit;
  FMaxTop:=AValue;
  InvalidateNonUpdated
end;

procedure TCustomECTriangle.SetOptions(AValue: TTriangleOptions);
var bRecalcRedraw: Boolean;
begin
  if FOptions=AValue then exit;
  bRecalcRedraw:= (etroReversed in (FOptions><AValue));
  FOptions:=AValue;
  if not bRecalcRedraw
    then Recalc         { Marks don't need Redraw }
    else RecalcRedraw;
end;

procedure TCustomECTriangle.SetPointerStyle(AValue: TPointerStyle);
begin
  if FPointerStyle=AValue then exit;
  FPointerStyle:=AValue;
  InvalidateNonUpdated;
end;

procedure TCustomECTriangle.SetRounding(AValue: Byte);
begin
  if FRounding=AValue then exit;
  FRounding:=AValue;
  if CaptionVisibility in [ecvValues, ecvBoth] then InvalidateNonUpdated;
end;

procedure TCustomECTriangle.SetValueFormat(AValue: TTriangleValueFormat);
begin
  if FValueFormat=AValue then exit;
  FValueFormat:=AValue;
  if CaptionVisibility in [ecvValues, ecvBoth] then InvalidateNonUpdated;
end;

end.


