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

unit ECRuler;
{$mode objfpc}{$H+}  

//{$DEFINE DBGRULER}  {don't remove, just comment}

interface

uses
  Classes, SysUtils, Controls, Graphics, ECScale, ECTypes, Forms, Math, LCLIntf,
  LMessages, {$IFDEF DBGRULER} LCLProc, {$ENDIF} LCLType, Themes, Types;

type 
  {$PACKENUM 2}
  TPointerMode = (epmNone, epmFixed, epmMovable);
  TPointerStyle = (epsSimple, epsDashed, epsDotted);
  { Event }
  TOnChangePosition = procedure(Sender: TObject; APosition: Double) of object;
  
  { TECRulerScale }
  TECRulerScale = class(TCustomECScale)
  protected const
    cDefTickAlign = etaInner;
    cDefValueVisible = evvValues;
  public
    constructor Create(AParent: TControl);
  published
    property DateTimeFormat;
    property Digits;
   { property FontOrientation; }
    property LogarithmBase;
    property Max;
    property Min;
    property ScaleType;
    property Text;
    property TickAlign default cDefTickAlign;
    property TickColor;
    property TickDesign;
    property TickIndent default 0;
    property TickLength;
    property TickLongValue;
    property TickMiddleValue;
    property TickShortValue;
    property TickVisible;
    property ValueDisplay;
    property ValueFormat;
    property ValueIndent;
    property ValueShift;
    property ValueVisible default cDefValueVisible; 
    property OnPrepareValue; 
  end;    
  
  { TCustomECRuler }
  TCustomECRuler = class(TECBaseControl)
  private
    FCaptionAlign: SmallInt;
    FCaptionPos: TBasicPos;
    FIndentBottomRight: Integer;
    FIndentTopLeft: Integer;
    FMouseCoord: Integer;
    FOnChangePosition: TOnChangePosition;
    FPointerColor: TColor;
    FPointerMode: TPointerMode;
    FPointerStyle: TPointerStyle;
    FPointerWidth: SmallInt;
    FPositionToHint: Boolean;
    FReversed: Boolean;
    FScale: TECRulerScale;
    FScaleFontOptions: TFontOptions;
    FScalePos: TBasicPos;
    FTransparent: Boolean;
    function GetMax: Double;
    function GetMin: Double; 
    function GetPosition: Double;
    procedure SetCaptionAlign(AValue: SmallInt);
    procedure SetCaptionPos(AValue: TBasicPos);
    procedure SetIndentBottomRight(AValue: Integer);
    procedure SetIndentTopLeft(AValue: Integer);
    procedure SetMax(AValue: Double);
    procedure SetMin(AValue: Double);
    procedure SetPointerColor(AValue: TColor);
    procedure SetPointerMode(AValue: TPointerMode);
    procedure SetPointerStyle(AValue: TPointerStyle);
    procedure SetPointerWidth(AValue: SmallInt);
    procedure SetPosition(AValue: Double);
    procedure SetPositionToHint(AValue: Boolean);
    procedure SetReversed(AValue: Boolean);
    procedure SetScalePos(AValue: TBasicPos);  
    procedure SetTransparent(AValue: Boolean);
  protected const
    cDefPointerMode = epmMovable;
    cDefPointerWidth = 1; 
  protected
    Background: TBitmap;
    CurrInvRect: TRect;
    FullBorderWidth: SmallInt;
    PointerOverlay: Integer;
    RedrawPointer: procedure(ACoord: Integer) of object;
    ScaleLength: Double;
    ScalePxLength, ScalePxStart: Integer;
    TimeFormat: TFormatSettings;
    WasEnabled: Boolean;  { state of IsEnabled from previous Paint }
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: Integer;
                                     {%H-}WithThemeSpace: Boolean); override;
    procedure Calculate;
    procedure CMColorChanged(var {%H-}Message: TLMessage); message CM_COLORCHANGED;
    procedure CMParentColorChanged(var Message: TLMessage); message CM_PARENTCOLORCHANGED;
    procedure DrawBackground;
    procedure DrawScaleAndCaption(ACanvas: TCanvas);
    procedure FontChanged(Sender: TObject); override;
    procedure InvalidateCustomRect({%H-}AMove: Boolean); override;
    function MouseCoordToPosition(AMouseCoord: Integer): Double; inline;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure OrientationChanged(AValue: TObjectOrientation); override;
    procedure Paint; override;
    procedure RecalcRedraw; override;				
    procedure Redraw3DColorAreas; override; 
    procedure RedrawHorizontal(X: Integer);
    procedure RedrawVertical(Y: Integer);
    procedure SetPointerPenStyle;
    procedure TextChanged; override;
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate; override;
    procedure EndUpdate(Recalculate: Boolean=True); override;
    procedure Redraw; override;
    procedure SetMouseCoord(AValue: Integer);
    property CaptionAlign: SmallInt read FCaptionAlign write SetCaptionAlign default 0;
    property CaptionPos: TBasicPos read FCaptionPos write SetCaptionPos default ebpBottomRight;
    property IndentBottomRight: Integer read FIndentBottomRight write SetIndentBottomRight default 0;
    property IndentTopLeft: Integer read FIndentTopLeft write SetIndentTopLeft default 0;
    property Max: Double read GetMax write SetMax stored False;
    property Min: Double read GetMin write SetMin stored False;
    property MouseCoord: Integer read FMouseCoord write SetMouseCoord;
    property PointerColor: TColor read FPointerColor write SetPointerColor default clDefault;
    property PointerMode: TPointerMode read FPointerMode write SetPointerMode default cDefPointerMode;
    property PointerStyle: TPointerStyle read FPointerStyle write SetPointerStyle default epsSimple;
    property PointerWidth: SmallInt read FPointerWidth write SetPointerWidth default cDefPointerWidth;
    property Position: Double read GetPosition write SetPosition stored False;
    property PositionToHint: Boolean read FPositionToHint write SetPositionToHint default False;
    property Reversed: Boolean read FReversed write SetReversed default False;
    property Scale: TECRulerScale read FScale write FScale;
    property ScaleFontOptions: TFontOptions read FScaleFontOptions write FScaleFontOptions;
    property ScalePos: TBasicPos read FScalePos write SetScalePos default ebpTopLeft;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property OnChangePosition: TOnChangePosition read FOnChangePosition write FOnChangePosition;
  end;

  TECRuler = class(TCustomECRuler)
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
    property CaptionAlign;
    property CaptionPos;
    property Color;
    property Color3DDark;
    property Color3DLight;
    property Constraints;
    property Enabled;
    property Font;
    property IndentBottomRight;
    property IndentTopLeft;
    property Max;
    property Min;
    property Orientation default eooHorizontal;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PointerColor;
    property PointerMode;
    property PointerStyle;
    property PointerWidth;
    property PopupMenu;
    property Position;
    property PositionToHint;
    property Reversed;
    property Scale;
    property ScaleFontOptions;
    property ScalePos;
    property ShowHint;
    property Style default eosPanel;
    property Transparent;
    property Visible;
    property OnChangeBounds;
    property OnChangePosition;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
  end;

implementation

{ TECRulerScale }

constructor TECRulerScale.Create(AParent: TControl);
begin
  inherited Create(AParent);
  FTickAlign := cDefTickAlign;
  FTickIndent := 0;
  FValueVisible := cDefValueVisible;  
end;

{ TCustomECRuler }

constructor TCustomECRuler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csNoFocus]
                               - [csSetCaption];
  FCaptionPos := ebpBottomRight;
  FMouseCoord := low(Integer);
  FOrientation := eooHorizontal;
  RedrawPointer := @RedrawHorizontal;
  FPointerColor := clDefault;
  FPointerMode := cDefPointerMode;
  PointerWidth := cDefPointerWidth;  {Set PointerOverlay}
  FStyle := eosPanel;
  AutoSize := True;
  TabStop := False;
  FScaleFontOptions := TFontOptions.Create(self);
  with FScaleFontOptions do
    begin
      FontSize := 7;
      OnRecalcRedraw := @self.RecalcRedraw;
      OnRedraw := @self.Redraw;
    end;
  FScale := TECRulerScale.Create(self);
  with FScale do
    begin
      OnRecalcRedraw := @self.RecalcRedraw;
      OnRedraw := @self.Redraw;
    end;
  Background := TBitmap.Create;
  with Background do
    begin
      Transparent := True;
      TransparentMode := tmFixed;
    end;
  SetInitialBounds(0, 0, 320, 50);
  RedrawMode := ermRecalcRedraw;
end;

destructor TCustomECRuler.Destroy;
begin
  FreeAndNil(Background);
  FreeAndNil(FScale);
  FreeAndNil(FScaleFontOptions);
  inherited Destroy;
end;

procedure TCustomECRuler.CalculatePreferredSize(var PreferredWidth, 
            PreferredHeight: Integer; WithThemeSpace: Boolean);
var aSize: Integer;
begin
  Canvas.Font.Size := FScaleFontOptions.FontSize;
  Canvas.Font.Style := FScaleFontOptions.FontStyles;
  aSize := 2*GetBorderWidth + FScale.GetPreferredSize(Canvas, Orientation=eooHorizontal, True, True);
  if Orientation = eooHorizontal then
    begin
      PreferredHeight := aSize;
      PreferredWidth := 0;      
    end else
    begin
      PreferredHeight := 0;
      PreferredWidth := aSize;      
    end;
end;

procedure TCustomECRuler.BeginUpdate;
begin
  inherited BeginUpdate;
  FScale.BeginUpdate;
end;

procedure TCustomECRuler.Calculate;
var aBorderWidth, aLength: Integer;
begin
  aBorderWidth := GetBorderWidth;
  FullBorderWidth := aBorderWidth;
  if Orientation = eooHorizontal 
    then aLength := Width
    else aLength := Height;
  aLength := aLength - IndentTopLeft - IndentBottomRight - 2*aBorderWidth;
  ScaleLength := Max - Min;
  ScalePxLength := aLength;
  ScalePxStart := IndentTopLeft + aBorderWidth;
  FScale.CalcTickPosAndValues(aLength, Reversed);
end;

procedure TCustomECRuler.CMColorChanged(var Message: TLMessage);
begin
  Redraw;
end;  

procedure TCustomECRuler.CMParentColorChanged(var Message: TLMessage);
begin
  inherited CMParentColorChanged(Message);
  if (Style = eosPanel) and (Color = clDefault) then Redraw;
end;  

procedure TCustomECRuler.DrawBackground;
var aColor: TColor;
begin
  {$IFDEF DBGRULER} DebugLn('ECRuler DrawBackground'); {$ENDIF}
  Background.SetSize(Width, Height);          
  aColor := ColorToRGB(GetColorResolvingDefault(Color, Parent.Brush.Color));
  if (aColor and $FF) > 0
    then dec(aColor)
    else inc(aColor);
  Background.TransparentColor := aColor;
  Background.TransparentClear;
  case Style of
    eosButton: Background.Canvas.DrawButtonBackground(ClientRect, True);
    eosPanel: Background.Canvas.DrawPanelBackGround(ClientRect, BevelInner, BevelOuter,
                BevelSpace, BevelWidth, Color3DDark, Color3DLight, 
                GetColorResolvingDefault(Color, Parent.Brush.Color));
    eosThemedPanel: Background.Canvas.DrawThemedPanelBkgnd(ClientRect);
  end;
  DrawScaleAndCaption(Background.Canvas);
  SetPointerPenStyle;
end;

procedure TCustomECRuler.DrawScaleAndCaption(ACanvas: TCanvas);
const cIndent = 3;
var aFlags, aHelpFlag: Cardinal;
    i, j, aAbsCaptionIndent, aBorderWidth, aBottom, aRight: Integer;
    aRect: TRect;
    aSize: TSize;
begin
  aBorderWidth := GetBorderWidth;
  ACanvas.Font.Assign(Font);
  if Caption <> '' then
    begin
      ACanvas.Font.Color := GetColorResolvingDefault(ACanvas.Font.Color, clBtnText);
      aFlags := DT_NOPREFIX or DT_MODIFYSTRING or DT_END_ELLIPSIS or DT_SINGLELINE;
      if IsRightToLeft then aFlags := aFlags or DT_RTLREADING;
      aHelpFlag := DT_TOP;
      aBottom := Height;
      aRight := Width;   
      aSize := ACanvas.TextExtent(Caption);
      aAbsCaptionIndent := abs(CaptionAlign);
      if Orientation = eooHorizontal then
        begin  { Horizontal }
          if FScale.TickVisible > etvNone then
            begin
              if ScalePos = ebpTopLeft 
                then j := Math.max(aBorderWidth + FScale.TickIndent, cIndent)
                else j := aBottom - Math.max(FScale.TickIndent + aBorderWidth, cIndent) - aSize.cy;
            end else
            begin
              if FScalePos = ebpTopLeft 
                then j := Math.max(aBorderWidth + FScale.ValueIndent, cIndent)
                else j := aBottom - Math.max(aBorderWidth + FScale.ValueIndent, cIndent) - aSize.cy;
            end;
          if CaptionPos = ebpTopLeft then 
            begin
              i := aBorderWidth;
              aRight := ScalePxStart;
              case CaptionAlign of
                low(CaptionAlign)..-1: inc(i, aAbsCaptionIndent);
                0: aHelpFlag := DT_CENTER;
                1..high(CaptionAlign): 
                  begin
                    aHelpFlag := DT_RIGHT;
                    dec(aRight, aAbsCaptionIndent);
                  end;
              end;
            end else 
            begin 
              i := ScalePxStart + ScalePxLength;
              dec(aRight, aBorderWidth);
              case CaptionAlign of
                low(SmallInt)..-1: 
                  begin
                    aHelpFlag := DT_RIGHT;
                    dec(aRight, aAbsCaptionIndent);
                  end;
                0: aHelpFlag := DT_CENTER;
                1..high(CaptionAlign): inc(i, aAbsCaptionIndent);
              end;
            end;
          dec(aBottom, aBorderWidth);
        end else                                               
        begin  { Vertical }
          if FScale.TickVisible > etvNone then
            begin
              if ScalePos = ebpTopLeft 
                then i := Math.max(aBorderWidth + FScale.TickIndent, cIndent)
                else i := aRight - Math.max(FScale.TickIndent + aBorderWidth, cIndent) - aSize.cx;
            end else
            begin
              if ScalePos = ebpTopLeft 
                then i := Math.max(aBorderWidth + FScale.ValueIndent, cIndent)
                else i := aRight - Math.max(aBorderWidth + FScale.ValueIndent, cIndent) - aSize.cx;
            end;
          if CaptionPos = ebpTopLeft then
            begin
              j := aBorderWidth;
              aBottom := ScalePxStart;
              case CaptionAlign of
                low(CaptionAlign)..-1: inc(j, aAbsCaptionIndent);
                0: aHelpFlag := DT_VCENTER;
                1..high(CaptionAlign): 
                  begin
                    aHelpFlag := DT_BOTTOM;
                    dec(aBottom, aAbsCaptionIndent);
                  end;
              end;
            end else
            begin
              j := ScalePxStart + ScalePxLength;
              dec(aBottom, aBorderWidth);
              case CaptionAlign of
                low(SmallInt)..-1: 
                  begin
                    aHelpFlag := DT_BOTTOM;
                    dec(aBottom, aAbsCaptionIndent);
                  end;
                0: aHelpFlag := DT_VCENTER;
                1..high(CaptionAlign): inc(j, aAbsCaptionIndent);
              end;   
            end;
          dec(aRight, aBorderWidth);
        end;
      aRect := Rect(i, j, aRight, aBottom);
      aFlags := aFlags or aHelpFlag;
      with ThemeServices do 
        DrawText(ACanvas, GetElementDetails(caThemedContent[caItemState[IsEnabled]]), 
          Caption, aRect, aFlags, 0);
    end; 
  ACanvas.Font.Color := GetColorResolvingDefault(FScaleFontOptions.FontColor, clBtnText);
  ACanvas.Font.Orientation := FScale.FontOrientation;
  ACanvas.Font.Size := FScaleFontOptions.FontSize;              
  ACanvas.Font.Style := FScaleFontOptions.FontStyles;
  if Orientation = eooHorizontal then
    begin
      if ScalePos = ebpTopLeft 
        then FScale.Draw(ACanvas, True, True, eopBottom, 
               Color3DDark, Color3DLight, Point(ScalePxStart, aBorderWidth - 1), [])
        else FScale.Draw(ACanvas, True, True, eopTop, 
               Color3DDark, Color3DLight, Point(ScalePxStart, Height - aBorderWidth), []);
    end else
    begin
      if ScalePos = ebpTopLeft 
        then FScale.Draw(ACanvas, True, True, eopRight, 
               Color3DDark, Color3DLight, Point(aBorderWidth - 1, ScalePxStart), [])     
        else FScale.Draw(ACanvas, True, True, eopLeft,
               Color3DDark, Color3DLight, Point(Width - aBorderWidth, ScalePxStart), []);
    end;                 
end;

procedure TCustomECRuler.EndUpdate(Recalculate: Boolean);
begin
  FScale.EndUpdate;
  inherited EndUpdate(Recalculate);
end;

procedure TCustomECRuler.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);
  RecalcRedraw;
end;                                                             

procedure TCustomECRuler.InvalidateCustomRect(AMove: Boolean);
begin 
end;

function TCustomECRuler.MouseCoordToPosition(AMouseCoord: Integer): Double;
begin
  Result := (AMouseCoord - ScalePxStart)*ScaleLength/(ScalePxLength - 1);
  if Reversed then Result := Max - Result;
end;   

procedure TCustomECRuler.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if (PointerMode = epmMovable) and WasEnabled then
    begin
      if Orientation = eooHorizontal
        then RedrawPointer(X)
        else RedrawPointer(Y);
      if PositionToHint and ShowHint then
        begin
          Application.CancelHint;
          Hint := FScale.GetStringPosition(Position);
          Application.ActivateHint(Mouse.CursorPos);
        end;

    end;
end;

procedure TCustomECRuler.OrientationChanged(AValue: TObjectOrientation);
begin
  if AValue = eooHorizontal 
    then RedrawPointer := @RedrawHorizontal
    else RedrawPointer := @RedrawVertical;
  FMouseCoord := low(Integer);
  if not(csLoading in ComponentState) then SetBounds(Left, Top, Height, Width); 
  inherited OrientationChanged(AValue);
end;

procedure TCustomECRuler.Paint;
var bEnabled: Boolean;
    
  procedure DrawPointer;
  var aBorderWidth: SmallInt;
  begin
    aBorderWidth := FullBorderWidth + PointerOverlay;
    if Orientation = eooHorizontal 
      then Canvas.Line(MouseCoord, aBorderWidth + 1, MouseCoord, self.Height - aBorderWidth)
      else Canvas.Line(aBorderWidth + 1, MouseCoord, self.Width - aBorderWidth, MouseCoord);        
  end;
    
begin
  {$IFDEF DBGRULER} DebugLn('Ruler Paint'); {$ENDIF}
  inherited Paint;
  if RedrawMode = ermRecalcRedraw then Calculate;
  bEnabled := IsEnabled;
  if bEnabled <> WasEnabled then 
    begin
      RedrawMode := ermRedrawBkgnd; 
      CurrInvRect := ClientRect;  { the case that Position was changed when Disabled }
    end;
  if not Transparent then
    begin
      if RedrawMode >= ermRedrawBkgnd then DrawBackground;
      if RedrawMode >= ermFreeRedraw then Canvas.Draw(0, 0, Background);
      if bEnabled and (PointerMode > epmNone) then
        begin
          if RedrawMode < ermFreeRedraw then Canvas.CopyRect(FInvalidRect, Background.Canvas, FInvalidRect);
          DrawPointer;
        end;
      FInvalidRect := CurrInvRect;
    end else
    begin
      Canvas.Pen.Width:=1;
      DrawScaleAndCaption(Canvas);
      if bEnabled and (PointerMode > epmNone) then
        begin
          SetPointerPenStyle;
          DrawPointer;
        end;
    end;
  WasEnabled := bEnabled;
  RedrawMode := ermFreeRedraw;
end;

procedure TCustomECRuler.RecalcRedraw;
begin
  RedrawMode := ermRecalcRedraw;
  if UpdateCount = 0 then
    begin
      if AutoSize then
        begin
          InvalidatePreferredSize;
          AdjustSize;
        end;
      Invalidate;
    end;
end;

procedure TCustomECRuler.Redraw;
begin
  if RedrawMode <  ermRedrawBkgnd then RedrawMode := ermRedrawBkgnd;
  if UpdateCount = 0 then Invalidate;
end;

procedure TCustomECRuler.Redraw3DColorAreas;
begin
  Redraw;
end;

procedure TCustomECRuler.RedrawHorizontal(X: Integer);
var aRect: TRect;
    aBorderWidth, i: Integer;
begin
  if MouseCoord <> X then
    begin
      if RedrawMode <= ermFreeRedraw then RedrawMode := ermMoveKnob;
      i := PointerOverlay;
      aBorderWidth := FullBorderWidth;
      if (X >= aBorderWidth) and (X <= (Width - aBorderWidth)) then
        begin
          FMouseCoord := X;
          if assigned(FOnChangePosition) then FOnChangePosition(self, MouseCoordToPosition(X));
        end;
      if not Transparent then
        begin
          aRect := Rect(X - i, aBorderWidth, X + i, Height - aBorderWidth);  
          CurrInvRect := aRect;
          FInvalidRect.Left := Math.min(FInvalidRect.Left, aRect.Left);
          FInvalidRect.Right := Math.max(FInvalidRect.Right, aRect.Right);
          InvalidateRect(Handle, @FInvalidRect, False);
        end else
        Invalidate;
    end;
end;

procedure TCustomECRuler.RedrawVertical(Y: Integer);
var aRect: TRect;
    aBorderWidth, j: Integer;
begin
  if MouseCoord <> Y then
    begin
      if RedrawMode <= ermFreeRedraw then RedrawMode := ermMoveKnob;
      j := PointerOverlay;
      aBorderWidth := FullBorderWidth;
      if (Y >= aBorderWidth) and (Y <= (Height - aBorderWidth)) then 
        begin
          FMouseCoord := Y;   
          if assigned(FOnChangePosition) then FOnChangePosition(self, MouseCoordToPosition(Y));
        end;
      if not Transparent then
        begin     
          aRect := Rect(aBorderWidth, Y - j, Width - aBorderWidth, Y + j);
          CurrInvRect := aRect;
          FInvalidRect.Top := Math.min(FInvalidRect.Top, aRect.Top);
          FInvalidRect.Bottom := Math.max(FInvalidRect.Bottom, aRect.Bottom);
          InvalidateRect(Handle, @FInvalidRect, False);
        end else
        Invalidate;
    end;
end;

procedure TCustomECRuler.SetPointerPenStyle;
begin
  with Canvas do
    begin
      case FPointerStyle of
        epsSimple: Pen.Style := psSolid;
        epsDashed: Pen.Style := psDash;
        epsDotted: Pen.Style := psDot;
      end;
      Brush.Style := bsClear;  { necessary }
      Pen.Color := GetColorResolvingDefault(PointerColor, clBtnText);
      Pen.Width := PointerWidth;
    end;
end;

procedure TCustomECRuler.TextChanged;
begin
  inherited TextChanged;
  Redraw;
end;

procedure TCustomECRuler.WMSize(var Message: TLMSize);
begin
  inherited WMSize(Message);
  RedrawMode := ermRecalcRedraw;
  if UpdateCount = 0 then Invalidate;
end;

{ TCustomECRuler.Setters }

function TCustomECRuler.GetMax: Double;
begin
  Result := Scale.Max;
end;

function TCustomECRuler.GetMin: Double;
begin
  Result := Scale.Min;
end;     

function TCustomECRuler.GetPosition: Double;
begin
  Result := MouseCoordToPosition(MouseCoord)
end;

procedure TCustomECRuler.SetCaptionAlign(AValue: SmallInt);
begin
  if FCaptionAlign = AValue then exit;
  FCaptionAlign := AValue;
  if Caption <> '' then Redraw;
end;  

procedure TCustomECRuler.SetCaptionPos(AValue: TBasicPos);
begin
  if FCaptionPos = AValue then exit;
  FCaptionPos := AValue;
  if Caption <> '' then Redraw;
end;

procedure TCustomECRuler.SetIndentBottomRight(AValue: Integer);
begin
  if FIndentBottomRight = AValue then exit;
  FIndentBottomRight := AValue;
  RecalcRedraw;
end;

procedure TCustomECRuler.SetIndentTopLeft(AValue: Integer);
begin
  if FIndentTopLeft = AValue then exit;
  FIndentTopLeft := AValue;
  RecalcRedraw;
end;

procedure TCustomECRuler.SetMax(AValue: Double);
begin
  Scale.Max := AValue;
end;

procedure TCustomECRuler.SetMin(AValue: Double);
begin
  Scale.Min := AValue;
end;

procedure TCustomECRuler.SetMouseCoord(AValue: Integer);
begin
  if (PointerMode > epmNone) and WasEnabled then RedrawPointer(AValue);
  FMouseCoord := AValue;
  if PositionToHint then Hint := FScale.GetStringPosition(Position);    
end;

procedure TCustomECRuler.SetPointerColor(AValue: TColor);
begin
  if FPointerColor = AValue then exit;
  FPointerColor := AValue;
  SetPointerPenStyle;
  if PointerMode > epmNone then InvalidateNonUpdated;
end;

procedure TCustomECRuler.SetPointerMode(AValue: TPointerMode);
begin
  if FPointerMode = AValue then exit;
  FPointerMode := AValue;
  if AValue > epmNone then CurrInvRect := ClientRect;
  InvalidateNonUpdated;
end;

procedure TCustomECRuler.SetPointerStyle(AValue: TPointerStyle);
begin
  if FPointerStyle = AValue then exit;
  FPointerStyle := AValue;
  SetPointerPenStyle;
  if PointerMode > epmNone then InvalidateNonUpdated;
end;

procedure TCustomECRuler.SetPointerWidth(AValue: SmallInt);
begin
  if FPointerWidth = AValue then exit;
  FPointerWidth := AValue;
  PointerOverlay := 1 + AValue div 2;
  SetPointerPenStyle;
  if PointerMode > epmNone then InvalidateNonUpdated;
end;

procedure TCustomECRuler.SetPosition(AValue: Double);
var aCoord: Integer;
begin
  if Reversed then AValue := Max - AValue;
  aCoord := round(AValue*ScalePxLength/ScaleLength) + ScalePxStart;
  if PositionToHint then Hint := FScale.GetStringPosition(AValue); 
  if (PointerMode > epmNone) and WasEnabled then RedrawPointer(aCoord);
end;

procedure TCustomECRuler.SetPositionToHint(AValue: Boolean);
begin
  if FPositionToHint = AValue then exit;   
  FPositionToHint := AValue;
  if AValue then Hint := FScale.GetStringPosition(Position); 
end;    

procedure TCustomECRuler.SetReversed(AValue: Boolean);
begin
  if FReversed = AValue then exit;
  FReversed := AValue;       
  RecalcRedraw;
end;

procedure TCustomECRuler.SetScalePos(AValue: TBasicPos);
begin
  if FScalePos = AValue then exit;  
  FScalePos := AValue;
  if (FScale.TickVisible > etvNone) or (FScale.ValueVisible > evvNone) then RecalcRedraw;
end;

procedure TCustomECRuler.SetTransparent(AValue: Boolean);
begin
  if FTransparent = AValue then exit;
  FTransparent := AValue;
  if not AValue then RedrawMode := ermRedrawBkgnd;
  InvalidateNonUpdated;  
end;

end.


