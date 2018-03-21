{**************************************************************************************************
 This file is part of the Eye Candy Controls (EC-C)

  Copyright (C) 2013-2016 Vojtěch Čihák, Czech Republic

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

unit ECProgressBar;
{$mode objfpc}{$H+}

//{$DEFINE DBGPROGBAR}  {don't remove, just comment}

interface

uses
  Classes, SysUtils, Controls, Forms, Graphics, LCLIntf, LCLProc, LMessages,
  Math, ECSlider, ECSpinCtrls, ECScale, ECTypes, Types;

type  
  {$PACKENUM 2}
  TProgressTextStyle = (eptNone, eptSolid, eptInverted);
  
  { TCustomECProgressBar }
  TCustomECProgressBar = class(TBaseECSlider)
  private
    FCaptionInline: Boolean;
    FProgressDigits: Word;
    FProgressFontOptions: TFontOptions;
    FProgressTextAlign: SmallInt;
    FProgressTextStyle: TProgressTextStyle;
    FUnits: string;
    procedure SetCaptionInline(AValue: Boolean);
    procedure SetProgressDigits(AValue: Word); virtual;
    procedure SetProgressTextAlign(AValue: SmallInt);
    procedure SetProgressTextStyle(AValue: TProgressTextStyle);
    procedure SetUnits(const AValue: string);
  protected const
    cDefGrooveWidth = 16;
    cDefProgMarkSize = 3;
    cDefProgressText = eptInverted;
  protected
    procedure CalcGrooveMiddle; override;
    procedure CalcInvalidRectDyn; override;
    procedure CalcInvalidRectStat; override;
    procedure CalcProgressInvRect; virtual;
    procedure CorrectGrooveLength(var z1, z2: Integer; AVertical: Boolean); override;
    procedure DrawGroove; override;
    function GetRelGroovePos: Integer; override;
    function HasCaption: Boolean; override;
    procedure OrientationChanged(AValue: TObjectOrientation); override;
    procedure PaintSelf(AEnabled: Boolean); override;
    procedure SetPosition(AValue: Double); override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property CaptionInline: Boolean read FCaptionInline write SetCaptionInline default False;
    property ProgressDigits: Word read FProgressDigits write SetProgressDigits default 0;
    property ProgressFontOptions: TFontOptions read FProgressFontOptions write FProgressFontOptions;
    property ProgressTextAlign: SmallInt read FProgressTextAlign write SetProgressTextAlign default 0;
    property ProgressTextStyle: TProgressTextStyle read FProgressTextStyle write SetProgressTextStyle default cDefProgressText;
    property Units: string read FUnits write SetUnits;
  end;

  { TECProgressBar }
  TECProgressBar = class(TCustomECProgressBar)
  published
    property Align;
    property Anchors;
    property AutoSize default True;
    property BevelInner;
    property BevelOuter;
    property BevelSpace;
    property BevelWidth;
    property BorderSpacing;
    property Caption;
    property CaptionInline;
    property CaptionPos;
    property Color;
    property Color3DLight;
    property Color3DDark;
    property Constraints;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property GrooveBevelWidth;
    property GrooveColor;
    property GrooveStyle;
    property GrooveTransparent;
    property GrooveWidth default cDefGrooveWidth;
    property ImageIndex;
    property ImagePos;
    property Images;
    property Indent;
    property Max;
    property Min;
    property Orientation default eooHorizontal;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Position;
    property PositionToHint;
    property ProgressColor;
    property ProgressColor2;
    property ProgressDigits;
    property ProgressFontOptions;
    property ProgressFromMiddle;
    property ProgressMark;
    property ProgressMarkSize default cDefProgMarkSize;
    property ProgressMiddlePos;
    property ProgressParameter;
    property ProgressStyle;
    property ProgressTextAlign;
    property ProgressTextStyle;
    property Reversed;
    property Scale;
    property ScaleFontOptions;
    property ScaleTickPos;
    property ScaleValuePos;
    property ShowHint;
    property Style;
    property Units;
    property Visible;
    property Width;
    property OnChange;
    property OnChangeBounds;
    property OnDrawProgressBMP;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
  end;  

  { TCustomECPositionBar }
  TCustomECPositionBar = class(TCustomECProgressBar)
  private
    FCursorLock: Boolean;
    FMouseDragPx: Integer;
    FMouseDragPxFine: Integer;
    FProgressSign: Boolean;
    procedure SetProgressSign(AValue: Boolean);
  protected const
    cDefIndent = 2;
    cDefMouseDragPxFine = 10;
    cDefMouseDragPx = 1;     
  protected
    FCursorBkgnd: TCursor;
    FDragAreaEntered: Boolean;
    FDragState: Boolean;
    FPrevCTRLDown: Boolean;  { wheter CTRL was pressed in previous MouseMove }
    InitCoord: Integer;
    InitDelta: Double;     
    procedure CalcInvalidRectDyn; override;
    procedure CalcProgressInvRect; override;
    procedure ChangeCursors(AMouseHoverDragArea: Boolean);
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    procedure DrawGroove; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure SetCursor(Value: TCursor); override;
  public
    constructor Create(TheOwner: TComponent); override;    
    property MouseDragPixels: Integer read FMouseDragPx write FMouseDragPx default cDefMouseDragPx;
    property MouseDragPixelsFine: Integer read FMouseDragPxFine write FMouseDragPxFine default cDefMouseDragPxFine;
    property ProgressSign: Boolean read FProgressSign write SetProgressSign default False;
  end; 

  { TECPositionBar }
  TECPositionBar = class(TCustomECPositionBar)
  published
    property Align;
    property Anchors;
    property AutoSize default True;
    property BevelInner;
    property BevelOuter;
    property BevelSpace;
    property BevelWidth;
    property BiDiMode;
    property BorderSpacing;
    property Caption;
    property CaptionInline;
    property CaptionPos;
    property Color;
    property Color3DLight;
    property Color3DDark;
    property Constraints;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property GrooveBevelWidth;
    property GrooveColor;
    property GrooveStyle;
    property GrooveTransparent;
    property GrooveWidth default cDefGrooveWidth;
    property ImageIndex;
    property ImagePos;
    property Images;
    property Indent default cDefIndent;
    property Max;
    property Min;
    property MouseDragPixels;
    property MouseDragPixelsFine;
    property Orientation default eooHorizontal;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Position;
    property PositionToHint;
    property ProgressColor;
    property ProgressColor2;
    property ProgressDigits;
    property ProgressFontOptions;
    property ProgressFromMiddle;
    property ProgressMark;
    property ProgressMarkSize default cDefProgMarkSize;
    property ProgressMiddlePos;
    property ProgressParameter;
    property ProgressSign;
    property ProgressStyle;
    property ProgressTextAlign;
    property ProgressTextStyle;
    property ProgressVisible;
    property Reversed;
    property Scale;
    property ScaleFontOptions;
    property ScaleTickPos;
    property ScaleValuePos;
    property ShowHint;
    property Style;
    property Units;
    property Visible;
    property Width;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawProgressBMP;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;           
  end;

  { TECSpinBtnsPos }
  TECSpinBtnsPos = class(TCustomSpinBtns)
  protected
    procedure RecalcRedraw; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AnchorSideLeft stored False;
    property AnchorSideTop stored False;
    property AnchorSideRight stored False;
    property AnchorSideBottom stored False;
    property BtnBigDec;
    property BtnBigInc;
    property BtnDec;
    property BtnDrag;
    property BtnInc;
    property BtnMax;
    property BtnMenu;
    property BtnMiddle;
    property BtnMin;
    property DiscreteChange;
    property DragControl;
    property DragOrientation;
    property Font;
    property GlyphStyle;
    property Height stored False;
    property Images;
    property Increment;
    property Left stored False;
    property MenuControl;
    property Middle;
    property Mode;
    property MouseFromMiddle;
    property MouseIncrementX;
    property MouseIncrementY;
    property MouseStepPixelsX;
    property MouseStepPixelsY;
    property PageSize;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Reversed;
    property ShowHint;
    property Spacing;
    property Style;
    property TimerDelay;
    property TimerRepeating;
    property Top stored False;
    property Width stored False;
    property OnClick;
    property OnContextPopup;
    property OnDrawGlyph;
    property OnMenuClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
  end;

  { TECSpinPosition }
  TECSpinPosition = class(TCustomECPositionBar)
  private
    FIndentBtns: SmallInt;
    FOnVisibleChanged: TOnVisibleChanged;
    FSpinBtns: TECSpinBtnsPos;
    function GetWidthInclBtn: Integer;
    procedure SetIndentBtns(AValue: SmallInt);
    procedure SetWidthInclBtn(AValue: Integer);
  protected const
    cDefIndentBtns = 0;
  protected
    procedure ChangePosition;
    procedure CMBiDiModeChanged(var Message: TLMessage); message CM_BIDIMODECHANGED;
    procedure DoOnChangeBounds; override;
    procedure InitializeWnd; override;
    procedure RecalcRedraw; override;
    procedure SetEnabled(Value: Boolean); override;
    procedure SetMax(const AValue: Double); override;
    procedure SetMin(const AValue: Double); override;
    procedure SetParent(NewParent: TWinControl); override;
    procedure SetPosition(AValue: Double); override;
    procedure SetSpinBtnsPosition;
    procedure VisibleChanged; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Anchors;
    property AutoSize default True;
    property BevelInner;
    property BevelOuter;
    property BevelSpace;
    property BevelWidth;
    property BiDiMode;
    property BorderSpacing;
    property Buttons: TECSpinBtnsPos read FSpinBtns write FSpinBtns;
    property Caption;
    property CaptionInline;
    property CaptionPos;
    property Color;
    property Color3DLight;
    property Color3DDark;
    property Constraints;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property GrooveBevelWidth;
    property GrooveColor;
    property GrooveStyle;
    property GrooveTransparent;
    property GrooveWidth default cDefGrooveWidth;
    property ImageIndex;
    property ImagePos;
    property Images;
    property Indent default cDefIndent;
    property IndentBtns: SmallInt read FIndentBtns write SetIndentBtns default cDefIndentBtns;
    property Max;
    property Min;
    property MouseDragPixels;
    property MouseDragPixelsFine;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Position;
    property PositionToHint;
    property ProgressColor;
    property ProgressColor2;
    property ProgressDigits;
    property ProgressFontOptions;
    property ProgressFromMiddle;
    property ProgressMark;
    property ProgressMarkSize default cDefProgMarkSize;
    property ProgressMiddlePos;
    property ProgressParameter;
    property ProgressSign;
    property ProgressStyle;
    property ProgressTextAlign;
    property ProgressTextStyle;
    property ProgressVisible;
    property Reversed;
    property Scale;
    property ScaleFontOptions;
    property ScaleTickPos;
    property ScaleValuePos;
    property ShowHint;
    property Style;
    property Units;
    property Visible;
    property Width;
    property WidthInclBtn: Integer read GetWidthInclBtn write SetWidthInclBtn stored False;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawProgressBMP;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
    property OnVisibleChanged: TOnVisibleChanged read FOnVisibleChanged write FOnVisibleChanged;
  end;

implementation

{ TCustomECProgressBar }

constructor TCustomECProgressBar.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ControlStyle:=ControlStyle+[csNoFocus];
  FGrooveWidth:=cDefGrooveWidth;
  FOrientation:=eooHorizontal;
  FProgressFontOptions:=TFontOptions.Create(self);
  with FProgressFontOptions do
    begin
      FontStyles:=[fsBold];
      OnRecalcRedraw:=@RecalcRedraw;
      OnRedraw:=@Redraw;
    end;
  FProgressMarkSize:=cDefProgMarkSize;
  FProgressTextStyle:=cDefProgressText;
  SetInitialBounds(0, 0, 240, 80);
  AccessibleRole:=larProgressIndicator;
end;

destructor TCustomECProgressBar.Destroy;
begin
  FreeAndNil(FProgressFontOptions);
  inherited Destroy;
end;

procedure TCustomECProgressBar.CalcGrooveMiddle;
begin
  if ProgressMiddlePos<Min 
    then FGrooveMiddle:=0
    else if ProgressMiddlePos>Max 
           then FGrooveMiddle:=FGrooveInnerLength-1
           else FGrooveMiddle:=trunc(((ProgressMiddlePos-Min)/(Max-Min))*FGrooveInnerLength);
end;

procedure TCustomECProgressBar.CalcInvalidRectDyn;
var aRect: TRect;
    aCurrentPosition: Integer;
begin
  {$IFDEF DBGPROGBAR} DebugLn('TCustomECProgressBar.CalcInvalidRectDyn'); {$ENDIF}
  if Orientation=eooHorizontal
    then FInvalidRect.Bottom:=FInvRectLimit       
    else FInvalidRect.Right:=FInvRectLimit;
  aRect:=FInvalidRect;
  aCurrentPosition:=round(((Position-Min)/(Max-Min))*FGrooveInnerLength);
  if not RealReversed 
    then aCurrentPosition:=FGrooveMin+aCurrentPosition
    else aCurrentPosition:=FGrooveMax-aCurrentPosition-1;
  if Orientation=eooHorizontal then
    begin
      if aRect.Left<aCurrentPosition then
        begin  { Moves Right }
          if aRect.Right<aCurrentPosition then FInvalidRect.Right:=aCurrentPosition
        end else  { Moves Left }
        FInvalidRect.Left:=aCurrentPosition;
    end else     
    begin
      if aRect.Top<aCurrentPosition then
        begin  { Moves Down }
          if aRect.Bottom<aCurrentPosition then FInvalidRect.Bottom:=aCurrentPosition;
        end else  { Moves Up }
        FInvalidRect.Top:=aCurrentPosition;
    end;    
  if not FPrevInvRectPainted then UnionRect(FInvalidRect, aRect, FInvalidRect);
  inc(FInvalidRect.Right);
  inc(FInvalidRect.Bottom);
end;

procedure TCustomECProgressBar.CalcInvalidRectStat;
begin
  {$IFDEF DBGPROGBAR} DebugLn('CalcInvalidRectStat'); {$ENDIF}
  if FOrientation=eooHorizontal then
    begin
      FInvalidRect.Top:=FGrooveRect.Top+FGrooveBevelWidth;
      FInvalidRect.Bottom:=FGrooveRect.Bottom-FGrooveBevelWidth;
      FInvRectLimit:=FInvalidRect.Bottom;
    end else    
    begin
      FInvalidRect.Left:=FGrooveRect.Left+FGrooveBevelWidth;
      FInvalidRect.Right:=FGrooveRect.Right-FGrooveBevelWidth;
      FInvRectLimit:=FInvalidRect.Right;
    end;    
end; 

procedure TCustomECProgressBar.CalcProgressInvRect;
var aHelp: Integer;
    
  procedure CalcProgressInvRectLimit;
  begin
    if not RealReversed           
      then aHelp:=FGrooveMin+round(GetRelPxPos)
      else aHelp:=FGrooveMax-round(GetRelPxPos)-1;        
  end;    
    
begin
  if Orientation=eooHorizontal then 
    begin
      if (ProgressTextStyle=eptNone) and (ProgressVisible=epvProgress) then
        begin
          CalcProgressInvRectLimit;
          FInvalidRect.Left:=aHelp;
          FInvalidRect.Right:=aHelp;
        end else 
        begin
          FInvalidRect.Left:=FGrooveMin;
          FInvalidRect.Right:=FGrooveMax;
        end;
    end else
    begin
      if (ProgressTextStyle=eptNone) and (ProgressVisible=epvProgress) then  
        begin
          CalcProgressInvRectLimit;
          FInvalidRect.Top:=aHelp;
          FInvalidRect.Bottom:=aHelp;
        end else  
        begin
          FInvalidRect.Top:=FGrooveMin;
          FInvalidRect.Bottom:=FGrooveMax;
        end;
    end;                        
end;    

procedure TCustomECProgressBar.CorrectGrooveLength(var z1, z2: Integer; AVertical: Boolean);
var aHalfSize: SmallInt;
begin  
  if Scale.ValueVisible<>evvNone then
    begin
      if AVertical 
        then aHalfSize:=Background.Canvas.TextHeight('0,9') div 2
        else aHalfSize:=(Math.max(Background.Canvas.TextWidth(Scale.GetStringMin),
                                 Background.Canvas.TextWidth(Scale.GetStringMax))-1) div 2;
      
      dec(aHalfSize, GrooveBevelWidth);  
      inc(z1, aHalfSize);
      dec(z2, aHalfSize);  
    end;
end;         

procedure TCustomECProgressBar.DrawGroove;  { must be called from within Paint or PaintSelf! }
var aColor: TColor;
    bHorizontal: Boolean;
    aMiddlePos, aMin, aMax, aProgressPos, aStart, aStop, aTextX, aTextY: Integer;
    aRect, aFullGrooveRect: TRect;
    aSize: TSize;
    aStr: string;
begin
  inherited DrawGroove;
  if ProgressTextStyle>eptNone then
    with Canvas do
      begin
        Font.Size:=ProgressFontOptions.FontSize;
        Font.Style:=ProgressFontOptions.FontStyles;
        bHorizontal:= (Orientation=eooHorizontal);
        if bHorizontal 
          then Font.Orientation:=0
          else Font.Orientation:=900;
        Brush.Style:=bsClear;
        aStr:=Units;
        DeleteAmpersands(aStr);
        if aStr<>'' then aStr:=' '+aStr;
        aStr:=Scale.GetStringPosition(Position, ProgressDigits)+aStr;
        if CaptionInline and (Caption<>'') then aStr:=Caption+' '+aStr;
        aSize:=TextExtent(aStr);
        aFullGrooveRect:=FGrooveRect;
        InflateRect(aFullGrooveRect, -GrooveBevelWidth, -GrooveBevelWidth);
        aRect:=aFullGrooveRect;
        if bHorizontal then
          begin
            if ProgressTextAlign=0 
              then aRect.Left:=(aRect.Right+aRect.Left-aSize.cx) div 2  { Align to H-Center }
              else if ProgressTextAlign>0                                  
                     then aRect.Left:=aRect.Right-ProgressTextAlign-aSize.cx  { Align to Right }
                     else dec(aRect.Left, ProgressTextAlign);  { Align to Left }                    
            aRect.Top:=(aRect.Bottom+aRect.Top-aSize.cy) div 2;
            aRect.Right:=aRect.Left+aSize.cx;
            aRect.Bottom:=aRect.Top+aSize.cy; 
            aTextX:=aRect.Left;
            aTextY:=aRect.Top;
          end else
          begin
            aRect.Left:=(aRect.Right+aRect.Left-aSize.cy) div 2;
            if ProgressTextAlign=0           
              then aRect.Top:=(aRect.Bottom+aRect.Top-aSize.cx) div 2  { Align to V-Center }
              else if ProgressTextAlign>0
                     then inc(aRect.Top, ProgressTextAlign)  { Align to Top }
                     else aRect.Top:=aRect.Bottom+ProgressTextAlign-aSize.cx;  { Align to Bottom }
            aRect.Right:=aRect.Left+aSize.cy;
            aRect.Bottom:=aRect.Top+aSize.cx+2;    
            aTextX:=aRect.Left;
            aTextY:=aRect.Bottom-1;  { necessary for Font.Ori = 900 }
          end;
        IntersectRect(aRect, aRect, aFullGrooveRect);
        case ProgressTextStyle of
          eptSolid:
            begin
              aColor:=ProgressFontOptions.FontColor;
              if aColor=clDefault then
                begin
                  aColor:=GetColorResolvingDefault(GrooveColor, clBtnText);
                  if not GrooveTransparent then aColor:=InvertColor(ColorToRGB(aColor));
                end; 
              if not IsEnabled then aColor:=GetMonochromaticColor(aColor);
              Font.Color:=aColor; 
              TextOut(aTextX, aTextY, aStr);
            end;
          eptInverted: 
            begin
              Clipping:=True;
              case ProgressVisible of
                epvNone:
                  begin
                    aColor:=GetColorResolvingDefault(ProgressColor, clHighlight);
                    if not IsEnabled then aColor:=GetMonochromaticColor(aColor);
                    Font.Color:=aColor;
                    TextOut(aTextX, aTextY, aStr);
                  end;
                epvProgress:
                  begin
                    if GrooveTransparent
                      then aColor:=clBtnFace
                      else aColor:=GetColorResolvingDefault(GrooveColor, cl3DDkShadow);
                    if not IsEnabled then aColor:=GetMonochromaticColor(aColor);
                    if not RealReversed then 
                      begin
                        aProgressPos:=FGrooveMin;
                        aMiddlePos:=aProgressPos+FGrooveMiddle;
                        inc(aProgressPos, GetRelGroovePos);
                      end else
                      begin
                        aProgressPos:=FGrooveMax-GetRelGroovePos;
                        aMiddlePos:=FGrooveMax-FGrooveMiddle;
                      end;
                    if not ProgressFromMiddle then
                      begin  { Normal Progress }
                        if bHorizontal
                          then aStart:=aRect.Left
                          else aStart:=aRect.Top; 
                        if aStart<aProgressPos then
                          begin
                            if not RealReversed 
                              then Font.Color:=aColor
                              else Font.Color:=GetColorResolvingDefAndEnabled(ProgressColor, clHighlight, IsEnabled); 
                            if bHorizontal
                              then ClipRect:=Rect(aRect.Left, aRect.Top, aProgressPos, aRect.Bottom)
                              else ClipRect:=Rect(aRect.Left, aRect.Top, aRect.Right, aProgressPos);
                            TextOut(aTextX, aTextY, aStr);
                          end;
                        if bHorizontal
                          then aStart:=aRect.Right
                          else aStart:=aRect.Bottom;  
                        if aProgressPos<aStart then
                          begin
                            if not RealReversed
                              then Font.Color:=GetColorResolvingDefAndEnabled(ProgressColor, clHighlight, IsEnabled)
                              else Font.Color:=aColor;
                            if bHorizontal
                              then ClipRect:=Rect(aProgressPos, aRect.Top, aRect.Right, aRect.Bottom)
                              else ClipRect:=Rect(aRect.Left, aProgressPos, aRect.Right, aRect.Bottom);
                           TextOut(aTextX, aTextY, aStr);
                          end;
                      end else
                      begin  { Progress from Middle }
                        if bHorizontal then 
                          begin
                            aStart:=aRect.Left;
                            aStop:=aRect.Right;
                          end else 
                          begin
                            aStart:=aRect.Top;
                            aStop:=aRect.Bottom;
                          end;
                        aMin:=Math.min(aProgressPos, aMiddlePos);
                        if aStart<aMin then 
                          begin
                            Font.Color:=GetColorResolvingDefAndEnabled(ProgressColor, clHighlight, IsEnabled);
                            if bHorizontal
                              then ClipRect:=Rect(aRect.Left, aRect.Top, aMin, aRect.Bottom)
                              else ClipRect:=Rect(aRect.Left, aRect.Top, aRect.Right, aMin);
                            TextOut(aTextX, aTextY, aStr);
                          end;
                        aMax:=Math.max(aProgressPos, aMiddlePos);
                        if (aStart<aMax) and (aStop>aMin) and (aProgressPos<>aMiddlePos) then
                          begin  
                            Font.Color:=aColor;
                            if bHorizontal
                              then ClipRect:=Rect(Math.max(aStart, aMin), aRect.Top, Math.min(aStop, aMax), aRect.Bottom)
                              else ClipRect:=Rect(aRect.Left, Math.max(aStart, aMin), aRect.Right, Math.min(aStop, aMax));
                            TextOut(aTextX, aTextY, aStr);
                          end;
                        if aStop>aMax then
                          begin
                            Font.Color:=GetColorResolvingDefAndEnabled(ProgressColor, clHighlight, IsEnabled);
                            if bHorizontal
                              then ClipRect:=Rect(aMax, aRect.Top, aRect.Right, aRect.Bottom)
                              else ClipRect:=Rect(aRect.Left, aMax, aRect.Right, aRect.Bottom);
                            TextOut(aTextX, aTextY, aStr);
                          end;
                      end;
                  end;
                epvFull:
                  begin
                    if GrooveTransparent
                      then aColor:=clBtnFace
                      else aColor:=GetColorResolvingDefault(GrooveColor, cl3DDkShadow);
                    if not IsEnabled then aColor:=GetMonochromaticColor(aColor);
                    Font.Color:=aColor;
                    TextOut(aTextX, aTextY, aStr);
                  end;
              end;
              Clipping:=False;
            end;
        end;
      end;
end;

function TCustomECProgressBar.GetRelGroovePos: Integer;
begin
  Result:=round(((Position-Min)/(Max-Min))*FGrooveInnerLength);
end;

function TCustomECProgressBar.HasCaption: Boolean;
begin
  Result:= ((Caption<>'') and not CaptionInline);
end;  

procedure TCustomECProgressBar.OrientationChanged(AValue: TObjectOrientation);
begin
  if not (csLoading in ComponentState) then SetBounds(Left, Top, Height, Width);
  inherited OrientationChanged(AValue);
end;

procedure TCustomECProgressBar.PaintSelf(AEnabled: Boolean);
begin
  if WasEnabled<>AEnabled then
    if RedrawMode<ermRedrawBkgnd then RedrawMode:=ermRedrawBkgnd;
  if RedrawMode=ermRecalcRedraw then Calculate;
  if RedrawMode>=ermRedrawBkgnd then
    begin
      DrawBackground;
      DrawGrooveBMP;
    end;
  if RedrawMode>=ermFreeRedraw then
    begin
      Canvas.Draw(0, 0, Background);
      DrawGroove;
    end;
  if RedrawMode=ermMoveKnob then
    begin
      Canvas.CopyRect(FInvalidRect, Background.Canvas, FInvalidRect);
      DrawGroove;
    end;
  FPrevInvRectPainted:=True;
  CalcProgressInvRect;
end;

procedure TCustomECProgressBar.SetPosition(AValue: Double);
begin
  if ([csLoading, csDestroying]*ComponentState=[]) and (UpdateCount=0) then
    if AValue<Min 
      then AValue:=Min
      else if AValue>Max then AValue:=Max;
  if FPosition=AValue then exit;
  FPosition:=AValue;
  if PositionToHint then Hint:=Scale.GetStringPosition(Position, ProgressDigits);
  if UpdateCount=0 then
    begin
      InvalidateCustomRect(True);
      if assigned(FOnChange) then FOnChange(self);
    end;
end;

{ Setters }

procedure TCustomECProgressBar.SetCaptionInline(AValue: Boolean);
begin
  if FCaptionInline=AValue then exit;
  FCaptionInline:=AValue;
  RecalcRedraw;
end;      

procedure TCustomECProgressBar.SetProgressDigits(AValue: Word);
begin
  if FProgressDigits=AValue then exit;
  FProgressDigits:=AValue;
  if ProgressTextStyle>eptNone then InvalidateNonUpdated;
end;       

procedure TCustomECProgressBar.SetProgressTextAlign(AValue: SmallInt);
begin
  if FProgressTextAlign=AValue then exit;
  FProgressTextAlign:=AValue;
  if ProgressTextStyle<>eptNone then InvalidateNonUpdated;
end;           

procedure TCustomECProgressBar.SetProgressTextStyle(AValue: TProgressTextStyle);
begin
  if FProgressTextStyle=AValue then exit;
  FProgressTextStyle:=AValue;
  InvalidateNonUpdated;
end;

procedure TCustomECProgressBar.SetUnits(const AValue: string);
begin
  if FUnits=AValue then exit;
  FUnits:=AValue;
  InvalidateNonUpdated;
end;      

{ TCustomECPositionBar }

constructor TCustomECPositionBar.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FCursorBkgnd:=Cursor;
  FMouseDragPxFine:=cDefMouseDragPxFine;
  FMouseDragPx:=cDefMouseDragPx;
  Indent:=cDefIndent;
  AccessibleRole:=larTrackBar;
end;

procedure TCustomECPositionBar.CalcInvalidRectDyn;
var aRect: TRect;
    aCurrentPosition: Integer;
begin
  {$IFDEF DBGPROGBAR} DebugLn('TCustomECPositionBar.CalcInvalidRectDyn'); {$ENDIF}
  if Orientation=eooHorizontal
    then FInvalidRect.Bottom:=FInvRectLimit       
    else FInvalidRect.Right:=FInvRectLimit;
  aRect:=FInvalidRect;
  aCurrentPosition:=round(((Position-Scale.Min)/(Scale.Max-Scale.Min))*FGrooveInnerLength);
  if not RealReversed 
    then aCurrentPosition:=FGrooveMin+aCurrentPosition
    else aCurrentPosition:=FGrooveMax-aCurrentPosition-1;
  if (ProgressTextStyle=eptNone) and (ProgressSign or (ProgressVisible=epvProgress)) then
    if Orientation=eooHorizontal then
      begin  { Horizontal }
        {$IFDEF DBGPROGBAR} WriteLn('aR.L ', aRect.Left, ' aR.R ', aRect.Right, ' aCP ', aCurrentPosition, ' aR.C ', (aRect.Left+aRect.Right) div 2); {$ENDIF}
        if ((aRect.Left+aRect.Right) div 2)<aCurrentPosition then
          begin  { Moves Right }
            {$IFDEF DBGPROGBAR} DebugLn('MoveRight'); {$ENDIF}
            FInvalidRect.Right:=aCurrentPosition+ProgressMarkSize;
          end else  { Moves Left }
          begin
            {$IFDEF DBGPROGBAR} DebugLn('MoveLeft'); {$ENDIF}
            FInvalidRect.Left:=aCurrentPosition-ProgressMarkSize;
          end;
      end else     
      begin  { Vertical }
        if ((aRect.Top+aRect.Bottom) div 2)<aCurrentPosition 
          then FInvalidRect.Bottom:=aCurrentPosition+ProgressMarkSize  { Moves Down }  
          else FInvalidRect.Top:=aCurrentPosition-ProgressMarkSize;  { Moves Up }
      end;    
  if not FPrevInvRectPainted then UnionRect(FInvalidRect, aRect, FInvalidRect);
  inc(FInvalidRect.Right); 
  inc(FInvalidRect.Bottom);
end;                   
              
procedure TCustomECPositionBar.CalcProgressInvRect;
var aMin, aMax, i: Integer;
    
  procedure CalcProgressInvRectLimit;
  begin
    if not RealReversed then 
        begin
          aMin:=FGrooveMin+round(GetRelPxPos);
          aMax:=aMin;
        end else 
        begin
          aMin:=FGrooveMax-round(GetRelPxPos)-1;
          aMax:=aMin;
        end;
    if ProgressSign then
      begin
        i:=ProgressMarkSize;
        dec(aMin, i);
        inc(i);
        inc(aMax, i);
      end;
  end;    
    
begin
  if Orientation=eooHorizontal then 
    begin
      if (ProgressTextStyle=eptNone) and (ProgressSign or (ProgressVisible=epvProgress)) then
        begin
          CalcProgressInvRectLimit;
          FInvalidRect.Left:=aMin;
          FInvalidRect.Right:=aMax;
        end else 
        begin
          FInvalidRect.Left:=FGrooveMin;
          FInvalidRect.Right:=FGrooveMax;
        end;
    end else
    begin
      if (ProgressTextStyle=eptNone) and (ProgressSign or (ProgressVisible=epvProgress)) then  
        begin
          CalcProgressInvRectLimit;
          FInvalidRect.Top:=aMin;
          FInvalidRect.Bottom:=aMax;
        end else  
        begin
          FInvalidRect.Top:=FGrooveMin;
          FInvalidRect.Bottom:=FGrooveMax;
        end;
    end;          
end;   

procedure TCustomECPositionBar.ChangeCursors(AMouseHoverDragArea: Boolean);
begin           
  FCursorLock:=True;
  if AMouseHoverDragArea 
    then if Orientation=eooHorizontal 
           then Cursor:=crSizeWE
           else Cursor:=crSizeNS
    else Cursor:=FCursorBkgnd;
  FCursorLock:=False;                    
end;  

function TCustomECPositionBar.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
var d: Double;
begin
  Result:=inherited DoMouseWheelDown(Shift, MousePos);
  if not (ssCtrl in Shift)
    then d:=Mouse.WheelScrollLines*MouseDragPixels
    else d:=Mouse.WheelScrollLines*MouseDragPixels/MouseDragPixelsFine;
  if not RealReversed
    then Position:=Position+d
    else Position:=Position-d; 
end;

function TCustomECPositionBar.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
var d: Double;
begin
  Result:=inherited DoMouseWheelUp(Shift, MousePos);
  if not (ssCtrl in Shift)
    then d:=Mouse.WheelScrollLines*MouseDragPixels
    else d:=Mouse.WheelScrollLines*MouseDragPixels/MouseDragPixelsFine;
  if not RealReversed
    then Position:=Position-d
    else Position:=Position+d;
end;      

procedure TCustomECPositionBar.DrawGroove;
var aColor, aInvColor: TColor;
    aPos, i, j: Integer;
    aRect: TRect;
begin
  inherited DrawGroove;
  if ProgressSign and (ProgressMarkSize>=1) then
    with Canvas do
      begin              
        aColor:=GetColorResolvingDefault(ProgressFontOptions.FontColor, clBtnText);
        aInvColor:=GetMergedColor(aColor, InvertColor(aColor), 0.25);
        Pen.Color:=aColor;
        Pen.Style:=psSolid;
        Pen.Width:=1;
        aPos:=GetRelGroovePos;
        aRect:=FGrooveRect;
        i:=GrooveBevelWidth;
        InflateRect(aRect, -i, -i);
        ClipRect:=aRect;
        Clipping:=True;
        if Orientation=eooHorizontal then
          begin  { Horizontal }
            if not RealReversed
              then inc(aPos, aRect.Left)
              else aPos:=aRect.Right-aPos;
            for i:=1 to ProgressMarkSize-1 do
              begin
                j:=aRect.Top+i+1;
                Line(aPos-i, j, aPos+i+1, j);
                Pixels[aPos-i-1, j]:=aInvColor;
                Pixels[aPos+i+1, j]:=aInvColor;
                j:=aRect.Bottom-i-2;
                Line(aPos-i, j, aPos+i+1, j);
                Pixels[aPos-i-1, j]:=aInvColor;
                Pixels[aPos+i+1, j]:=aInvColor; 
              end;
            Pen.Color:=aInvColor;
            i:=ProgressMarkSize;
            j:=aRect.Top+1;
            Pixels[aPos, j]:=aColor;
            Pixels[aPos-1, j]:=aInvColor;
            Pixels[aPos+1, j]:=aInvColor;
            inc(j, i);
            Line(aPos-i+1, j, aPos+i, j);
            j:=aRect.Bottom-2;
            Pixels[aPos, j]:=aColor;
            Pixels[aPos-1, j]:=aInvColor;
            Pixels[aPos+1, j]:=aInvColor;    
            dec(j, i);
            Line(aPos-i+1, j, aPos+i, j); 
          end else
          begin  { Vertical }
            if not Reversed
              then inc(aPos, aRect.Top)
              else aPos:=aRect.Bottom-aPos;
            for i:=1 to ProgressMarkSize-1 do
              begin
                j:=aRect.Left+i+1;
                Line(j, aPos-i, j, aPos+i+1);
                Pixels[j, aPos-i-1]:=aInvColor;
                Pixels[j, aPos+i+1]:=aInvColor;
                j:=aRect.Right-i-2;
                Line(j, aPos-i, j, aPos+i+1);
                Pixels[j, aPos-i-1]:=aInvColor;
                Pixels[j, aPos+i+1]:=aInvColor;
              end;
            Pen.Color:=aInvColor;
            j:=ProgressMarkSize; 
            i:=aRect.Left+1;
            Pixels[i, aPos]:=aColor;
            Pixels[i, aPos-1]:=aInvColor;
            Pixels[i, aPos+1]:=aInvColor;
            inc(i, j);
            Line(i, aPos-j+1, i, aPos+j);
            i:=aRect.Right-2;
            Pixels[i, aPos]:=aColor;
            Pixels[i, aPos-1]:=aInvColor;
            Pixels[i, aPos+1]:=aInvColor;
            dec(i, j);
            Line(i, aPos-j+1, i, aPos+j);
          end;
        Clipping:=False;
      end;
end;     

procedure TCustomECPositionBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var aMousePos: Double;

  procedure SetMousePos(ACoord: Integer);
  begin
    aMousePos:=GetPosFromCoord(ACoord);
    if not RealReversed
      then aMousePos:=Min+aMousePos
      else aMousePos:=Max-aMousePos;
  end;
                   
begin
  inherited MouseDown(Button, Shift, X, Y);
  case Button of 
    mbLeft:
      begin
        if Orientation=eooHorizontal
          then SetMousePos(X)
          else SetMousePos(Y);
        if FDragAreaEntered then
          begin  { Left click on Drag Area}
            if Orientation=eooHorizontal then 
              begin
                InitCoord:=X;
                InitDelta:=GetPosFromCoord(X)-Position
              end else 
              begin
                InitCoord:=Y;
                InitDelta:=GetPosFromCoord(Y)-Position;
              end;
            FDragState:=True;
          end else  { Middle or Double click }
          Position:=aMousePos;               
      end;
    mbMiddle: 
      if not ProgressFromMiddle
        then Position:=0.5*(Max+Min)
        else Position:=ProgressMiddlePos;
  end;
end;

procedure TCustomECPositionBar.MouseMove(Shift: TShiftState; X, Y: Integer);
const cTolerance = 5;
var aInit, aPosition: Integer;
    bCTRLDown: Boolean;
    bPrevDragAreaEntered: Boolean;
begin
  inherited MouseMove(Shift, X, Y);
  if IsEnabled then
    begin  
      aPosition:=GetRelGroovePos+GrooveBevelWidth;
      if not FDragState then
        begin
          if Orientation=eooHorizontal then
            begin  { Horizontal }
              if not RealReversed 
                then aPosition:=aPosition+FGrooveRect.Left
                else aPosition:=FGrooveRect.Right-aPosition;
              bPrevDragAreaEntered:=FDragAreaEntered;
              FDragAreaEntered:=(Y>=FGrooveRect.Top) and (Y<=FGrooveRect.Bottom)
                and (X>=(aPosition-cTolerance)) and (X<=(aPosition+cTolerance));
            end else
            begin  { Vertical }
              if not Reversed
                then aPosition:=aPosition+FGrooveRect.Top
                else aPosition:=FGrooveRect.Bottom-aPosition;
              bPrevDragAreaEntered:=FDragAreaEntered;
              FDragAreaEntered:= (X>=FGrooveRect.Left) and (X<=FGrooveRect.Right)
                and (Y>=(aPosition-cTolerance)) and (Y<=(aPosition+cTolerance));
            end;
          if FDragAreaEntered<>bPrevDragAreaEntered then ChangeCursors(FDragAreaEntered);
        end else
        begin
          bCTRLDown:= (ssCtrl in Shift);
          if bCTRLDown<>FPrevCTRLDown then
            begin
              if Orientation=eooHorizontal then 
                begin
                  InitCoord:=X;
                  InitDelta:=GetPosFromCoord(X)-Position
                end else
                begin
                  InitCoord:=Y;
                  InitDelta:=GetPosFromCoord(Y)-Position;
                end;
            end; 
          aInit:=InitCoord;
          if Orientation=eooHorizontal 
            then aPosition:=X-aInit
            else aPosition:=Y-aInit;
          if not bCTRLDown 
            then aPosition:=aPosition div MouseDragPixels
            else aPosition:=aPosition div MouseDragPixelsFine;
          if not RealReversed
            then Position:=GetPosFromCoord(aInit+aPosition)-InitDelta
            else Position:=GetPosFromCoord(aInit-aPosition)-InitDelta;
          FPrevCTRLDown:=bCTRLDown;
        end;   
    end;
end;

procedure TCustomECPositionBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FDragState then
    begin
      FDragState:=False;
      if not FDragAreaEntered then ChangeCursors(False);
    end;        
  if ShowHint and PositionToHint then Application.ActivateHint(Mouse.CursorPos);
end;                 

procedure TCustomECPositionBar.SetCursor(Value: TCursor);
begin
  inherited SetCursor(Value);
  if not FCursorLock then FCursorBkgnd:=Value;   
end;     

{ Setters }

procedure TCustomECPositionBar.SetProgressSign(AValue: Boolean);
begin
  if FProgressSign=AValue then exit;
  FProgressSign:=AValue;
  InvalidateNonUpdated;
end;

{ TECSpinBtnsPos }

procedure TECSpinBtnsPos.RecalcRedraw;
begin
  inherited RecalcRedraw;
  if UpdateCount=0 then
    begin
      CalcInternalGeometry;
      AdjustWidth;
      (Owner as TECSpinPosition).SetSpinBtnsPosition;
      RedrawMode:=ermRedrawBkgnd;
      Invalidate;
    end;
end;

constructor TECSpinBtnsPos.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle+[csNoDesignSelectable];
  SetSubComponent(True);
end;

{ TECSpinPosition }

constructor TECSpinPosition.Create(TheOwner: TComponent);
begin
  FSpinBtns:=TECSpinBtnsPos.Create(self);
  inherited Create(TheOwner);
  ControlStyle:=ControlStyle-[csSetCaption];
  FIndentBtns:=cDefIndentBtns;
  FScale.TickVisible:=etvNone;
  FScale.ValueVisible:=evvNone;
  with FSpinBtns do
    begin
      CustomChange:=@ChangePosition;
      Name:='ECSpinPosButtons';
      AnchorParallel(akTop, 0, self);
      AnchorParallel(akBottom, 0, self);
      Middle:=0.5*(Scale.Min+Scale.Max);
    end;
end;

destructor TECSpinPosition.Destroy;
begin
  FreeAndNil(FSpinBtns);
  inherited Destroy;
end;

procedure TECSpinPosition.ChangePosition;
begin
  Position:=FSpinBtns.Value;
end;

procedure TECSpinPosition.CMBiDiModeChanged(var Message: TLMessage);
begin
  inherited CMBiDiModeChanged(Message);
  FSpinBtns.BiDiMode:=BiDiMode;
  SetSpinBtnsPosition;
end;

procedure TECSpinPosition.DoOnChangeBounds;
begin
  inherited DoOnChangeBounds;
  SetSpinBtnsPosition;
end;

procedure TECSpinPosition.InitializeWnd;
begin
  inherited InitializeWnd;
  SetSpinBtnsPosition;
end;

procedure TECSpinPosition.RecalcRedraw;
begin
  FSpinBtns.Max:=Scale.Max;
  FSpinBtns.Min:=Scale.Min;
  inherited RecalcRedraw;
end;

procedure TECSpinPosition.SetEnabled(Value: Boolean);
begin
  inherited SetEnabled(Value);
  FSpinBtns.Enabled:=Value;
end;

procedure TECSpinPosition.SetMax(const AValue: Double);
begin
  inherited SetMax(AValue);
  FSpinBtns.Max:=Scale.Max;
end;

procedure TECSpinPosition.SetMin(const AValue: Double);
begin
  inherited SetMin(AValue);
  FSpinBtns.Min:=Scale.Min;
end;

procedure TECSpinPosition.SetParent(NewParent: TWinControl);
begin
  if assigned(FSpinBtns) then FSpinBtns.Anchors:=[];
  inherited SetParent(NewParent);
  if assigned(FSpinBtns) then
    begin
      FSpinBtns.Parent:=Parent;
      FSpinBtns.Anchors:=[akTop, akBottom];
    end;
end;

procedure TECSpinPosition.SetPosition(AValue: Double);
begin
  inherited SetPosition(AValue);
  FSpinBtns.CustomChange:=nil;
  FSpinBtns.Value:=AValue;
  FSpinBtns.CustomChange:=@ChangePosition;
end;

procedure TECSpinPosition.SetSpinBtnsPosition;
begin
  if not IsRightToLeft
    then FSpinBtns.Left:=Left+Width+IndentBtns
    else FSpinBtns.Left:=Left-IndentBtns-FSpinBtns.Width;
end;

procedure TECSpinPosition.VisibleChanged;
begin
  inherited VisibleChanged;
  FSpinBtns.Visible:=Visible;
  if assigned(OnVisibleChanged) then OnVisibleChanged(self, Visible);
end;

{ TECSpinPosition.Getters & Setters }

function TECSpinPosition.GetWidthInclBtn: Integer;
begin
  Result:=Width+IndentBtns+FSpinBtns.Width;
end;

procedure TECSpinPosition.SetIndentBtns(AValue: SmallInt);
begin
  if FIndentBtns=AValue then exit;
  FIndentBtns:=AValue;
  SetSpinBtnsPosition;
end;

procedure TECSpinPosition.SetWidthInclBtn(AValue: Integer);
begin
  Width:=AValue-IndentBtns-FSpinBtns.Width;
end;

end.


