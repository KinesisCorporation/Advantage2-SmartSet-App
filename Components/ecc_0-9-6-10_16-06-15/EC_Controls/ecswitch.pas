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

unit ECSwitch;    
{$mode objfpc}{$H+}

//{$DEFINE DBGSWITCH}  {don't remove, just comment}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, Graphics, Math, ActnList, Forms,
  LCLIntf, LMessages, LCLProc, LResources, LCLType, Themes, Types, ECTypes, dialogs;

type
  {$PACKENUM 2}
  TGlyphStyle = (egsNone, egsOneZero, egsCircles, egsPlusMinus, egsDot, egsOnOff, egsText);
    
  { TECSwitchKnob }
  TECSwitchKnob = class(TECCustomKnob)
  published
    property BevelWidth;
    property Color;
    property Style;
    property TickMarkCount;
    property TickMarkDesign;
    property TickMarkSpacing;
    property TickMarkStyle;
  end;

  TCustomECSwitch = class;

  { TECSwitchActionLink }
  TECSwitchActionLink = class(TWinControlActionLink)
  protected
    FClientSwitch: TCustomECSwitch;
    procedure AssignClient(AClient: TObject); override;
    procedure SetChecked(Value: Boolean); override;
  public
    function IsCheckedLinked: Boolean; override;
  end;

  TECSwitchActionLinkClass = class of TECSwitchActionLink;

  { TCustomECSwitch }
  TCustomECSwitch = class(TECBaseControl)
  private
    FAllowGrayed: Boolean;
    FCaptionPos: TObjectPos;
    FCheckFromAction: Boolean;
    FGlyphStyle: TGlyphStyle;
    FGrooveCheckedClr: TColor;
    FGrooveIndent: SmallInt;
    FGrooveUncheckedClr: TColor;
    FKnob: TECSwitchKnob;
    FKnobHovered: Boolean;
    FKnobIndent: SmallInt;
    FOnChange: TNotifyEvent;
    FState: TCheckBoxState;
    FSwitchColor: TColor;
    FSwitchHeight: Integer;
    FSwitchWidth: Integer;
    FShowFocusedKnob: Boolean;
    FTextChecked: string;
    FTextUnchecked: string;
    function GetChecked: Boolean;
    procedure SetCaptionPos(AValue: TObjectPos);
    procedure SetChecked(AValue: Boolean);
    procedure SetGlyphStyle(AValue: TGlyphStyle);
    procedure SetGrooveCheckedClr(AValue: TColor);
    procedure SetGrooveIndent(AValue: SmallInt);
    procedure SetGrooveUncheckedClr(AValue: TColor);
    procedure SetKnobHovered(AValue: Boolean);
    procedure SetKnobIndent(AValue: SmallInt);
    procedure SetState(AValue: TCheckBoxState);
    procedure SetSwitchColor(AValue: TColor);
    procedure SetSwitchHeight(AValue: Integer);
    procedure SetSwitchWidth(AValue: Integer);
  protected const
    caClrGlyph: array[False..True] of TColor = ($D8D8D8, $F4F4F4);
    cDefGlyphStyle = egsOneZero;
    cDefGrooveIndent = 7;
    cDefKnobIndent = 4;
    cDefSwitchHeight = 28;
    cDefSwitchWidth = 64;
    cFocusRectIndent: SmallInt = 3;
    cIndent = 5;
  protected
    CaptionPoint, GlyphOnePoint, GlyphZeroPoint, SwitchPoint: TPoint;
    CaptionSize: TSize;
    GlyphSize: SmallInt;  {0 - No glyph; 4 - Small glyph; 8 - Normal glyph }
    InitMouseCoord: Integer;
    KnobCaptured: Boolean;                  
    KnobMouseDown: Boolean;
    KnobPosUnchecked, KnobPosChecked, KnobPosGrayed: Integer;
    NeedCalculate: Boolean;
    class var GlyphFullCircle8, GlyphZero8, GlyphDot4, GlyphZero4: TPortableNetworkGraphic;
    class constructor LoadGlyph;
    class destructor FreeGlyph;  
  protected  
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: Integer; 
                                     {%H-}WithThemeSpace: Boolean); override;
    procedure Calculate;
    procedure CMBiDiModeChanged(var {%H-}Message: TLMessage); message CM_BIDIMODECHANGED;
    procedure CMEnabledChanged(var Message: TLMessage); message CM_ENABLEDCHANGED;
    procedure CMParentColorChanged(var Message: TLMessage); message CM_PARENTCOLORCHANGED;
    function DialogChar(var Message: TLMKey): Boolean; override;
    procedure DoClick;
    procedure DoEnter; override;
    procedure DoExit; override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure InvalidateCustomRect({%H-}AMove: Boolean); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure OrientationChanged(AValue: TObjectOrientation); override;
    procedure Paint; override;
    procedure RecalcInvalidate;
    procedure RecalcRedraw; override;
    procedure Redraw3DColorAreas; override;        
    procedure ResizeKnob;     
    procedure SetAutoSize(Value: Boolean); override;
    procedure SetKnobBackground;
    procedure StyleChanged(AValue: TObjectStyle); override;
    procedure TextChanged; override;
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
    property CheckFromAction: Boolean read FCheckFromAction write FCheckFromAction;
    property KnobHovered: Boolean read FKnobHovered write SetKnobHovered;
  public  
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate; override;
    procedure EndUpdate(Recalculate: Boolean = True); override;
    procedure Redraw; override;
    property AllowGrayed: Boolean read FAllowGrayed write FAllowGrayed default False;
    property CaptionPos: TObjectPos read FCaptionPos write SetCaptionPos default eopRight;
    property Checked: Boolean read GetChecked write SetChecked default False;
    property GlyphStyle: TGlyphStyle read FGlyphStyle write SetGlyphStyle default cDefGlyphStyle;
    property GrooveCheckedClr: TColor read FGrooveCheckedClr write SetGrooveCheckedClr default clDefault;
    property GrooveUncheckedClr: TColor read FGrooveUncheckedClr write SetGrooveUncheckedClr default clDefault;
    property GrooveIndent: SmallInt read FGrooveIndent write SetGrooveIndent default cDefGrooveIndent;
    property Knob: TECSwitchKnob read FKnob write FKnob;
    property KnobIndent: SmallInt read FKnobIndent write SetKnobIndent default cDefKnobIndent;
    property State: TCheckBoxState read FState write SetState default cbUnchecked;
    property SwitchColor: TColor read FSwitchColor write SetSwitchColor default clDefault;
    property SwitchHeight: Integer read FSwitchHeight write SetSwitchHeight default cDefSwitchHeight;
    property SwitchWidth: Integer read FSwitchWidth write SetSwitchWidth default cDefSwitchWidth;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property ShowFocusedKnob: boolean read FShowFocusedKnob write FShowFocusedKnob default false;
    property TextChecked: string read FTextChecked write FTextChecked;
    property TextUnchecked: string read FTextUnchecked write FTextUnchecked;
  end;
  
  { TECSwitch }
  TECSwitch = class(TCustomECSwitch)
  published
    property Action;
    property Align;
    property AllowGrayed;
    property Anchors;
    property AutoSize default True;
    property BevelInner;
    property BevelOuter;
    property BevelSpace;
    property BevelWidth;
    property BiDiMode;                
    property BorderSpacing;
    property Caption;
    property CaptionPos;
    property Checked;
    {property Color;}  { not needed }
    property Color3DDark;
    property Color3DLight;
    property Constraints;
    property Cursor;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property GlyphStyle;
		property GrooveCheckedClr;		
    property GrooveIndent;
    property GrooveUncheckedClr;
    property Height;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Hint;
    property Knob;
    property KnobIndent;
    property Left;
    property Orientation default eooHorizontal;
    property ParentBiDiMode;
    {property ParentColor;}  { not needed }
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property State;
    property Style default eosButton;
    property SwitchColor;
    property SwitchHeight;
    property SwitchWidth;
    property ShowFocusedKnob;
    property TabOrder;
    property TabStop default True;
    property TextChecked;
    property TextUnchecked;
    property Top;
    property Visible;
    property Width;
    property OnChangeBounds;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
    property OnUTF8KeyPress;   
  end;

implementation

{ TECSwitchActionLink }

procedure TECSwitchActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);
  FClientSwitch := AClient as TCustomECSwitch;
end;

function TECSwitchActionLink.IsCheckedLinked: Boolean;
begin
  Result := inherited IsCheckedLinked and
            (FClientSwitch.Checked = (Action as TCustomAction).Checked);
end;

procedure TECSwitchActionLink.SetChecked(Value: Boolean);
begin
  if IsCheckedLinked then
    begin
      FClientSwitch.CheckFromAction := True;
      try
        FClientSwitch.Checked := Value;
      finally
        FClientSwitch.CheckFromAction := False;
      end;
    end;
end;

{ TCustomECSwitch }

constructor TCustomECSwitch.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - csMultiClicks - [csClickEvents, csNoStdEvents];  { inherited Click not used }
  FAllowGrayed := False;
  FCaptionPos := eopRight;
  FGlyphStyle := egsOneZero;
  FGrooveCheckedClr := clDefault;
  FGrooveIndent := cDefGrooveIndent;
  FGrooveUncheckedClr := clDefault; 
  FKnob := TECSwitchKnob.Create(self);
  FKnobIndent := cDefKnobIndent;
  FSwitchColor := clDefault;
  FSwitchHeight := cDefSwitchHeight;
  FSwitchWidth := cDefSwitchWidth;
  ResizeKnob;
  AutoSize := True;
  TabStop := True;
  AccessibleRole := larCheckBox;
end;

destructor TCustomECSwitch.Destroy;
begin
  FreeAndNil(FKnob);
  inherited Destroy;
end;

class constructor TCustomECSwitch.LoadGlyph;
begin
  {$I ecswitch.lrs}
  GlyphFullCircle8 := TPortableNetworkGraphic.Create;
  GlyphFullCircle8.LoadFromLazarusResource('fullcircle8');  
  GlyphZero8 := TPortableNetworkGraphic.Create;
  GlyphZero8.LoadFromLazarusResource('zero8');  
  GlyphDot4 := TPortableNetworkGraphic.Create;
  GlyphDot4.LoadFromLazarusResource('fullcircle4');  
  GlyphZero4 := TPortableNetworkGraphic.Create;
  GlyphZero4.LoadFromLazarusResource('zero4');
end;

class destructor TCustomECSwitch.FreeGlyph;
begin
  FreeAndNil(GlyphFullCircle8);
  FreeAndNil(GlyphZero8);
  FreeAndNil(GlyphDot4);
  FreeAndNil(GlyphZero4);
end;  

procedure TCustomECSwitch.CalculatePreferredSize(var PreferredWidth, 
            PreferredHeight: Integer; WithThemeSpace: Boolean);
var aCaption: string;
    aTextSize: TSize;
begin
  aCaption := Caption;
  if aCaption <> '' then
    begin
      DeleteAmpersands(aCaption);
      aTextSize := Canvas.TextExtent(aCaption);
      inc(aTextSize.cx, 2 * cFocusRectIndent);
      inc(aTextSize.cy, 2 * cFocusRectIndent);
      if CaptionPos in [eopRight, eopLeft] then
        begin
          PreferredWidth := SwitchWidth + cIndent + aTextSize.cx;
          PreferredHeight := max(SwitchHeight, aTextSize.cy);
        end else
        begin
          PreferredHeight := aTextSize.cy + cIndent + SwitchHeight;
          PreferredWidth := max(SwitchWidth, aTextSize.cx);
        end;
    end else
    begin
      PreferredWidth := SwitchWidth;
      PreferredHeight := SwitchHeight;
    end;  
end;  

procedure TCustomECSwitch.BeginUpdate;
begin
  inherited BeginUpdate;
  FKnob.BeginUpdate;
end;

procedure TCustomECSwitch.Calculate;
var aHelp, aMax, aUnchecked, aChecked: Integer;
    aRealCaptionPos: TObjectPos;
    aCaption: string;
    aTextSize: TSize;
    bRightToLeft: Boolean;
begin
  {$IFDEF DBGSWITCH} DebugLn('TCustomECSwitch.Calculate'); {$ENDIF}
  aRealCaptionPos := CaptionPos;
  bRightToLeft := IsRightToLeft;
  if bRightToLeft then
    case aRealCaptionPos of
      eopRight: aRealCaptionPos := eopLeft;
      eopLeft: aRealCaptionPos := eopRight;
    end;
  aCaption := Caption;
  if aCaption <> '' then
    begin
      DeleteAmpersands(aCaption);
      aTextSize := Canvas.TextExtent(aCaption);
      inc(aTextSize.cx, 2 * cFocusRectIndent);  { additional space for FocusRect }
      inc(aTextSize.cy, 2 * cFocusRectIndent);
      CaptionSize := aTextSize;
      if aRealCaptionPos in [eopRight, eopLeft] then
        begin
          CaptionPoint.Y := (Height - aTextSize.cy) div 2;
          SwitchPoint.Y := (Height - SwitchHeight) div 2;
        end else
        begin    
          aMax := Width - max(aTextSize.cx, SwitchWidth);
          aHelp := (aTextSize.cx - SwitchWidth) div 2;
          if bRightToLeft then
            begin
              if aHelp < 0 then
                begin
                  CaptionPoint.X := aMax - aHelp;
                  SwitchPoint.X := aMax;
                end else
                begin
                  CaptionPoint.X := aMax;
                  SwitchPoint.X := aMax + aHelp;
                end;      
            end else
            begin
              if aHelp < 0 then
                begin
                  CaptionPoint.X := -aHelp;
                  SwitchPoint.X := 0;
                end else
                begin
                  CaptionPoint.X := 0;
                  SwitchPoint.X := aHelp;
                end;
            end;
          aHelp := aTextSize.cy + cIndent;
        end;
      case aRealCaptionPos of
        eopTop:
          begin
            if AutoSize then
              begin
                CaptionPoint.Y := 0;
                SwitchPoint.Y := Height - SwitchHeight;
              end else
              begin
                CaptionPoint.Y := (Height - aHelp - SwitchHeight) div 2;
                SwitchPoint.Y := CaptionPoint.Y + aHelp;         
              end;
          end;  
        eopRight: 
          begin
            if AutoSize then
              begin
                CaptionPoint.X := Width - aTextSize.cx; 
                SwitchPoint.X := 0;  
              end else
              if bRightToLeft then
                begin
                  CaptionPoint.X := Width - aTextSize.cx;
                  SwitchPoint.X := CaptionPoint.X - cIndent - SwitchWidth;
                end else
                begin
                  CaptionPoint.X := SwitchWidth + cIndent;
                  SwitchPoint.X := 0;
                end;
          end;    
        eopBottom:
          begin
            if AutoSize then 
              begin
                CaptionPoint.Y := Height - aTextSize.cy - 1;  { -1 'cause of underlined chars }
                SwitchPoint.Y := 0;   
              end else
              begin
                SwitchPoint.Y := (Height - aHelp - SwitchHeight) div 2;
                CaptionPoint.Y := SwitchPoint.Y + SwitchHeight + cIndent;
              end;  
          end;
        eopLeft:
          begin
            if AutoSize then
              begin
                CaptionPoint.X := 0;
                SwitchPoint.X := Width - SwitchWidth;  
              end else
              if bRightToLeft then
                begin
                  SwitchPoint.X := Width - SwitchWidth;
                  CaptionPoint.X := SwitchPoint.X - cIndent - aTextSize.cx;
                end else
                begin
                  CaptionPoint.X := 0;
                  SwitchPoint.X := cIndent + aTextSize.cx;
                end;
          end;
      end;
    end else
    begin
      if bRightToLeft 
        then SwitchPoint.X := Width - SwitchWidth
        else SwitchPoint.X := 0;
      SwitchPoint.Y := (Height - SwitchHeight) div 2;
    end;
  GlyphSize := 0;
  if Orientation = eooHorizontal then
    begin
      aUnchecked := SwitchPoint.X + KnobIndent;  
      aChecked := SwitchPoint.X + SwitchWidth - KnobIndent - Knob.Width;
      Knob.Top := SwitchPoint.Y + (SwitchHeight - Knob.Height) div 2; 
      aMax := SwitchHeight - 2*FGrooveIndent;
      if ((aMax > 12) and (SwitchWidth >= 48)) 
        then GlyphSize := 8
        else if ((aMax > 6) and (SwitchWidth >= 42)) then GlyphSize := 4;
      if GlyphSize > 0 then
        begin
          GlyphOnePoint.X := (SwitchPoint.X + FGrooveIndent + aChecked - GlyphSize) div 2;
          GlyphZeroPoint.X := (SwitchPoint.X + SwitchWidth - FGrooveIndent 
                               + aUnchecked + Knob.Width - GlyphSize) div 2;
          GlyphOnePoint.Y := (SwitchPoint.Y + SwitchPoint.Y + SwitchHeight - GlyphSize) div 2;
          GlyphZeroPoint.Y := GlyphOnePoint.Y;
        end;
      if bRightToLeft then 
        begin
          aHelp := aUnchecked;
          aUnchecked := aChecked;
          aChecked := aHelp;
          aHelp := GlyphZeroPoint.X;
          GlyphZeroPoint.X := GlyphOnePoint.X;
          GlyphOnePoint.X := aHelp;
        end; 
    end else 
    begin
      aUnchecked := SwitchPoint.Y + SwitchHeight - KnobIndent - Knob.Height;
      aChecked := SwitchPoint.Y + KnobIndent;
      Knob.Left := SwitchPoint.X + (SwitchWidth - Knob.Width) div 2;
      aMax := SwitchWidth - 2*FGrooveIndent;
      if ((aMax > 12) and (SwitchHeight >= 48)) 
        then GlyphSize := 8
        else if ((aMax > 6) and (SwitchHeight >= 42)) then GlyphSize := 4;
      if GlyphSize > 0 then
        begin 
          GlyphOnePoint.X := (2*SwitchPoint.X + SwitchWidth - GlyphSize) div 2;
          GlyphZeroPoint.X := GlyphOnePoint.X;
          GlyphOnePoint.Y := (SwitchPoint.Y + SwitchHeight - FGrooveIndent
                              + aChecked + Knob.Height - GlyphSize) div 2;
          GlyphZeroPoint.Y := (SwitchPoint.Y + FGrooveIndent + aUnchecked - GlyphSize) div 2; 
        end;  
    end;
  KnobPosUnchecked := aUnchecked;
  KnobPosChecked := aChecked;
  KnobPosGrayed := (aUnchecked + aChecked) div 2; 
  NeedCalculate := False;
end;           

procedure TCustomECSwitch.CMBiDiModeChanged(var Message: TLMessage);
begin
  RecalcInvalidate;
end;    

procedure TCustomECSwitch.CMEnabledChanged(var Message: TLMessage);
begin
  if IsEnabled then FKnobHovered:=False;
  inherited CMEnabledChanged(Message);
end;    

procedure TCustomECSwitch.CMParentColorChanged(var Message: TLMessage);
begin
  {$IFDEF DBGSWITCH} DebugLn('TCustomECSwitch.CMParentColorChanged'); {$ENDIF}
  inherited CMParentColorChanged(Message);
  if assigned(FKnob) and (SwitchColor = clDefault) then SetKnobBackground;
end;  

function TCustomECSwitch.DialogChar(var Message: TLMKey): Boolean;
begin
  Result:=False;
  if Message.Msg=LM_SYSCHAR then
    begin
      if IsEnabled and IsVisible then
        begin
          if IsAccel(Message.CharCode, Caption) then
            begin
              DoClick;
              SetFocus;
              Result := True;      
            end else
            Result := inherited DialogChar(Message);
        end;					
    end;
end;           

procedure TCustomECSwitch.DoClick;
begin
  if AllowGrayed then   
    begin
      case FState of 
        cbUnchecked: State := cbGrayed;
        cbGrayed: State := cbChecked;
        cbChecked: State := cbUnchecked;
      end;
    end else
    Checked := not Checked;  
end;                

procedure TCustomECSwitch.DoEnter;
begin
  inherited DoEnter;
  Invalidate;
end;                

procedure TCustomECSwitch.DoExit;
begin
  inherited DoExit;
  Invalidate;
end;

procedure TCustomECSwitch.EndUpdate(Recalculate: Boolean = True);
begin
  FKnob.EndUpdate;
  inherited EndUpdate(Recalculate);
end;

function TCustomECSwitch.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TECSwitchActionLink;  
end;

procedure TCustomECSwitch.InvalidateCustomRect(AMove: Boolean);
begin
  Invalidate;
end;                

procedure TCustomECSwitch.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if (Key in [VK_RETURN, VK_SPACE]) and (Shift*[ssCtrl, ssAlt, ssShift] = []) then DoClick;
end;                

procedure TCustomECSwitch.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);    
  if (Button = mbLeft) and KnobHovered then KnobMouseDown := True;   
  SetFocus;
end;                

procedure TCustomECSwitch.MouseLeave;
begin
  inherited MouseLeave;
  KnobHovered := False;
end;                

procedure TCustomECSwitch.MouseMove(Shift: TShiftState; X, Y: Integer);
var aLeft, aTop: Integer;
begin
  inherited MouseMove(Shift, X, Y);
  if KnobCaptured then
    begin
      if Orientation = eooHorizontal then
        begin
          if IsRightToLeft
            then aLeft := EnsureRange(InitMouseCoord + X, KnobPosChecked, KnobPosUnchecked)
            else aLeft := EnsureRange(InitMouseCoord + X, KnobPosUnchecked, KnobPosChecked); 
          if Knob.Left <> aLeft then
            begin
              Knob.Left := aLeft;
              Invalidate;
            end;
        end else
        begin
          aTop := EnsureRange(InitMouseCoord + Y, KnobPosChecked, KnobPosUnchecked);
          if Knob.Top <> aTop then
            begin
              Knob.Top := aTop;
              Invalidate;
            end;
        end;
    end else     
    begin
      if KnobMouseDown then
        begin
          KnobCaptured := True;
          if Orientation = eooHorizontal 
            then InitMouseCoord := Knob.Left - X
            else InitMouseCoord := Knob.Top - Y;       
        end else
        begin
          aLeft := Knob.Left;
          aTop := Knob.Top;
          KnobHovered := ((aLeft <= X) and (aTop <= Y) and (X < (aLeft + Knob.Width)) and (Y < (aTop + Knob.Height))); 
        end;
    end;      
end;                

procedure TCustomECSwitch.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var aHelp, aPosition: Integer; 
    aState: TCheckBoxState;
    b: Boolean;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Button = mbLeft then
    begin
      if KnobCaptured then 
        begin
          if Orientation = eooHorizontal then
            begin
              if AllowGrayed then 
                begin
                  aHelp := Math.Min(KnobPosUnchecked, KnobPosChecked);
                  aPosition := (aHelp + 2*(KnobPosGrayed - aHelp) div 3);
                  if aPosition > Knob.Left then aState := cbUnchecked
                    else 
                    begin
                      aHelp := Math.Max(KnobPosUnchecked, KnobPosChecked);
                      aPosition := (KnobPosGrayed + (aHelp - KnobPosGrayed) div 3);
                      if aPosition > Knob.Left
                        then aState := cbGrayed
                        else aState := cbChecked;
                    end;
                  if IsRightToLeft then
                    case aState of
                      cbUnchecked: aState := cbChecked;
                      cbChecked: aState := cbUnchecked;
                    end;
                  State := aState;
                end else
                begin
                  b := ((KnobPosUnchecked + KnobPosChecked) < 2*Knob.Left);
                  if IsRightToLeft then b := not b;
                  Checked := b;
                end;
            end else
            begin
              if AllowGrayed then
                begin
                  aPosition := (KnobPosGrayed + (KnobPosUnchecked - KnobPosGrayed) div 3);
                  if aPosition < Knob.Top then State := cbUnchecked
                    else 
                    begin
                      aPosition := (KnobPosChecked + 2*(KnobPosGrayed - KnobPosChecked) div 3);
                      if aPosition < Knob.Top
                        then State := cbGrayed
                        else State := cbChecked;
                    end;  
                end else
                Checked := ((KnobPosUnchecked + KnobPosChecked) > 2*Knob.Top); 
            end;
          { Knob remains hovered when mouse is over Switch but out of Knob; does not matter }
          if not PtInRect(ClientRect, Point(X, Y)) then FKnobHovered := False;
          Invalidate;
          KnobCaptured := False;
        end else
        if PtInRect(ClientRect, Point(X, Y)) then DoClick; 
      KnobMouseDown := False;
    end;
end;                 

procedure TCustomECSwitch.OrientationChanged(AValue: TObjectOrientation);
var aHelp: Integer;
begin
  if not (csLoading in ComponentState) then 
    begin
      aHelp := SwitchHeight;
      FSwitchHeight := SwitchWidth;
      SwitchWidth := aHelp;
      if aHelp = SwitchHeight then ResizeKnob;  { when Switch is square }
      NeedCalculate := True;   
    end; 
  inherited OrientationChanged(AValue);
end;                 

procedure TCustomECSwitch.Paint;
var aColor, aColor2: TColor; 
    bEnabled: Boolean;
    aRect: TRect;
    x, y: Integer;
begin
  {$IFDEF DBGSWITCH} DebugLn('TCustomECSwitch.Paint'); {$ENDIF}
  inherited Paint;
  if NeedCalculate then Calculate;
  bEnabled := IsEnabled;
  { Paint Switch Body }          
  x := SwitchPoint.X;
  y := SwitchPoint.Y;
  aRect:=Rect(x, y - 2, x + SwitchWidth, y + SwitchHeight + 2);
  aColor := GetColorResolvingDefault(SwitchColor, Parent.Brush.Color);
  case Style of
    eosButton: Canvas.DrawButtonBackground(aRect, bEnabled);
    eosPanel: Canvas.DrawPanelBackGround(aRect, BevelInner, BevelOuter, BevelSpace, 
                BevelWidth, Color3DDark, Color3DLight, aColor);
    eosThemedPanel: Canvas.DrawThemedPanelBkgnd(aRect);
  end;
  { Paint Groove }
  InflateRect(aRect, -GrooveIndent, -GrooveIndent);
  Canvas.Pen.Style := psSolid;
  Canvas.Frame3D(aRect, GetColorResolvingDefault(Color3DDark, clBtnShadow),
                 GetColorResolvingDefault(Color3DLight, clBtnHilight), 1);
  if not KnobCaptured or AllowGrayed 
    then case State of
           cbUnchecked: aColor := GetColorResolvingDefault(GrooveUncheckedClr, cl3DDkShadow);
           cbChecked: aColor := GetColorResolvingDefault(GrooveCheckedClr, clActiveCaption);
         end 
    else 
    begin
      aColor := GetColorResolvingDefault(GrooveCheckedClr, clActiveCaption);
      aColor2 := GetColorResolvingDefault(GrooveUncheckedClr, cl3DDkShadow);
      if Orientation = eooHorizontal
        then aColor := GetMergedColor(aColor, aColor2, 
          (Knob.Left - KnobPosUnchecked)/(KnobPosChecked - KnobPosUnchecked))
        else aColor := GetMergedColor(aColor, aColor2,
          (KnobPosUnChecked - Knob.Top)/(KnobPosUnchecked - KnobPosChecked));
    end;
  if bEnabled
    then Canvas.Brush.Color := aColor
    else Canvas.Brush.Color := GetMonochromaticColor(aColor);
  if State <> cbGrayed then Canvas.FillRect(aRect);
  { Paint Glyphs }  { impossible to draw directly from resources, class vars used instead }
  if (GlyphSize > 0) and (GlyphStyle <> egsNone) then
    with Canvas do
      begin
        if KnobCaptured or (State <> cbChecked) then
          begin
            x := GlyphZeroPoint.X;
            y := GlyphZeroPoint.Y; 
            case GlyphStyle of
              egsOneZero, egsCircles: 
                if GlyphSize = 8 
                  then Draw(x, y, GlyphZero8)
                  else Draw(x, y, GlyphZero4);
              egsPlusMinus:
                begin
                  if GlyphSize = 8 then 
                    begin
                      Brush.Color := caClrGlyph[bEnabled];
                      FillRect(x, y + 3, x + GlyphSize, y + 5);
                    end else 
                    begin
                      Pen.Color := caClrGlyph[bEnabled];
                      if (Orientation = eooHorizontal) and ((SwitchHeight and 1) = 0) then dec(y)
                        else if (Orientation=eooVertical) and ((SwitchWidth and 1) = 0) then dec(x);
                      Line(x, y + 2, x + GlyphSize + 1, y + 2);
                    end;
                end;
              egsOnOff:
                begin
                  Pen.Color := caClrGlyph[bEnabled];
                  if (Orientation = eooHorizontal) and ((SwitchHeight and 1) = 0) then dec(y)
                    else if (Orientation=eooVertical) and ((SwitchWidth and 1) = 0) then dec(x);

                  Canvas.Brush.Style := bsClear;
                  if (SwitchHeight <= 26) then
                    Canvas.TextOut(x - 6, y - 3, 'Off')
                  else
                    Canvas.TextOut(x - 3, y - 2, 'Off');
                end;
              egsText:
                begin
                  Pen.Color := caClrGlyph[bEnabled];
                  if (Orientation = eooHorizontal) and ((SwitchHeight and 1) = 0) then dec(y)
                    else if (Orientation=eooVertical) and ((SwitchWidth and 1) = 0) then dec(x);

                  Canvas.Brush.Style := bsClear;
                  if (SwitchHeight <= 26) then
                    Canvas.TextOut(x - 6, y - 3, TextUnchecked)
                  else
                    Canvas.TextOut(x - 6, y - 2, TextUnchecked);
                end;
            end;
          end;
        if KnobCaptured or (State <> cbUnchecked) then
          begin
            x := GlyphOnePoint.X;
            y := GlyphOnePoint.Y; 
            case GlyphStyle of
              egsOneZero: 
                begin
                  if GlyphSize = 8 then
                    begin
                      Brush.Color := caClrGlyph[bEnabled];
                      FillRect(x + 3, y, x + 5, y + GlyphSize);
                    end else 
                    begin
                      Pen.Color := clWhite;
                      if (Orientation=eooHorizontal) or ((SwitchWidth and 1) = 1) 
                        then inc(x, 2)
                        else inc(x);
                      Line(x, y, x, y + GlyphSize);
                    end;
                end;
              egsCircles: 
                if GlyphSize=8
                  then Draw(x, y, GlyphFullCircle8) 
                  else Draw(x, y, GlyphDot4);
              egsPlusMinus: 
                begin
                  if GlyphSize = 8 then
                    begin
                      Brush.Color := caClrGlyph[bEnabled];
                      FillRect(x, y + 3, x + GlyphSize, y + 5);
                      FillRect(x + 3, y, x + 5, y + GlyphSize);
                    end else
                    begin
                      Pen.Color := caClrGlyph[bEnabled];
                      if (Orientation = eooHorizontal) and ((SwitchHeight and 1) = 0) then dec(y)
                        else if (Orientation=eooVertical) and ((SwitchWidth and 1) = 0) then dec(x);
                      Line(x, y + 2, x + GlyphSize + 1, y + 2);
                      inc(x, 2);
                      Line(x, y, x, y + GlyphSize + 1);
                    end;
                end;
              egsDot:       
                if GlyphSize = 8
                  then Draw(x + 2, y + 2, GlyphDot4)
                  else Draw(x, y, GlyphDot4);
              egsOnOff:
                begin
                  Pen.Color := caClrGlyph[bEnabled];
                  if (Orientation = eooHorizontal) and ((SwitchHeight and 1) = 0) then dec(y)
                    else if (Orientation=eooVertical) and ((SwitchWidth and 1) = 0) then dec(x);

                  Canvas.Brush.Style := bsClear;
                  if (SwitchHeight <= 26) then
                    Canvas.TextOut(x - 5, y - 3, 'On')
                  else
                    Canvas.TextOut(x - 3, y - 2, 'On');
                end;
              egsText:
                begin
                  Pen.Color := caClrGlyph[bEnabled];
                  if (Orientation = eooHorizontal) and ((SwitchHeight and 1) = 0) then dec(y)
                    else if (Orientation=eooVertical) and ((SwitchWidth and 1) = 0) then dec(x);

                  Canvas.Brush.Style := bsClear;
                  if (SwitchHeight <= 26) then
                    Canvas.TextOut(x - 6, y - 3, TextChecked)
                  else
                    Canvas.TextOut(x - 6, y - 2, TextChecked);
                end;
            end;  
          end;
      end;
  { Paint Knob }
  if not KnobCaptured then  
    begin
      if Orientation = eooHorizontal then
        begin
          case State of
            cbUnchecked: x := KnobPosUnchecked;
            cbChecked: x := KnobPosChecked; 
            cbGrayed: x := KnobPosGrayed; 
          end;
          Knob.Left := x;
          y := Knob.Top; 
        end else
        begin
          case State of
            cbUnchecked: y := KnobPosUnchecked; 
            cbChecked: y := KnobPosChecked; 
            cbGrayed: y := KnobPosGrayed; 
          end;
          Knob.Top := y;
          x := Knob.Left; 
        end;
    end else
    begin
      x := Knob.Left;
      y := Knob.Top;
    end;
  if not bEnabled 
    then Canvas.Draw(x, y, Knob.KnobDisabled)
    else if KnobHovered
           then Canvas.Draw(x, y, Knob.KnobHighlighted)
           else Canvas.Draw(x, y, Knob.KnobNormal);
  { Paint Caption }
  if (ShowFocusedKnob) then
  begin
    if Caption <> '' then
      begin
        aRect := Rect(CaptionPoint.X, CaptionPoint.Y,
                   CaptionPoint.X + CaptionSize.cx, CaptionPoint.Y + CaptionSize.cy);
        { Paint FocusRect around Caption}
        if Focused then
          begin
            LCLIntf.SetBkColor(Canvas.Handle, ColorToRGB(clBtnFace));
            LCLIntf.DrawFocusRect(Canvas.Handle, aRect);
          end;
        InflateRect(aRect, -cFocusRectIndent, -cFocusRectIndent);
        with ThemeServices do
          DrawText(Canvas, GetElementDetails(caThemedContent[caItemState[bEnabled]]),
            Caption, aRect, DT_SINGLELINE, 0);
      end else
      { Paint FocusRect on Switch when there's no Caption }
      if Focused then
        begin
          aRect := Rect(x + 3, y + 3, x + Knob.Width - 3, y + Knob.Height - 3);
          Canvas.DrawFocusRectNonThemed(aRect);
        end;
  end;
end;

procedure TCustomECSwitch.RecalcInvalidate;
begin
  NeedCalculate := True;
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

procedure TCustomECSwitch.RecalcRedraw;
begin    
  if UpdateCount = 0 then Invalidate;
end;

procedure TCustomECSwitch.Redraw;
begin 
  if UpdateCount = 0 then Invalidate;
end;

procedure TCustomECSwitch.Redraw3DColorAreas;
begin
  if assigned(Knob) and (Knob.Style = eosPanel) then Knob.DrawKnobs;
  if UpdateCount = 0 then Invalidate;
end;

procedure TCustomECSwitch.ResizeKnob;
begin
  {$IFDEF DBGSWITCH} DebugLn('TCustomECSwitch.ResizeKnob'); {$ENDIF}
  if Orientation = eooHorizontal 
    then FKnob.SetSize(SwitchWidth div 2, SwitchHeight - 2*KnobIndent)
    else FKnob.SetSize(SwitchWidth - 2*KnobIndent, SwitchHeight div 2);
end;

procedure TCustomECSwitch.SetAutoSize(Value: Boolean);
begin
  inherited SetAutoSize(Value);
  if Value then NeedCalculate := True;
end;

procedure TCustomECSwitch.SetKnobBackground;
var aColor: TColor;
begin
  if Style = eosPanel 
    then aColor := GetColorResolvingDefault(SwitchColor, Parent.Brush.Color)
    else aColor := clBtnFace;
  aColor := ColorToRGB(aColor);
  FKnob.BackgroundColor := aColor;
end;        

procedure TCustomECSwitch.StyleChanged(AValue: TObjectStyle);
begin
  SetKnobBackground;
  inherited StyleChanged(AValue);
end;          

procedure TCustomECSwitch.TextChanged;
begin
  inherited TextChanged;
  RecalcInvalidate;
end;

procedure TCustomECSwitch.WMSize(var Message: TLMSize);
begin
  inherited WMSize(Message);
  NeedCalculate := True;
  Invalidate;  
end;

{ Setters }

function TCustomECSwitch.GetChecked: Boolean;
begin
  Result := (FState = cbChecked);
end;

procedure TCustomECSwitch.SetCaptionPos(AValue: TObjectPos);
begin
  if FCaptionPos = AValue then exit;
  FCaptionPos := AValue;
  RecalcInvalidate;
end;   

procedure TCustomECSwitch.SetChecked(AValue: Boolean);
begin
  if AValue 
    then State := cbChecked
    else State := cbUnChecked;
end;           

procedure TCustomECSwitch.SetGlyphStyle(AValue: TGlyphStyle);
begin
  if FGlyphStyle = AValue then exit;
  FGlyphStyle := AValue;
  Redraw;
end;           

procedure TCustomECSwitch.SetGrooveCheckedClr(AValue: TColor);
begin
  if FGrooveCheckedClr = AValue then exit;
  FGrooveCheckedClr := AValue;
  if Checked then Redraw;
end;           

procedure TCustomECSwitch.SetGrooveIndent(AValue: SmallInt);
begin
  if FGrooveIndent = AValue then exit;
  FGrooveIndent := AValue;
  NeedCalculate := True;
  Redraw;  
end;           

procedure TCustomECSwitch.SetGrooveUncheckedClr(AValue: TColor);
begin
  if FGrooveUncheckedClr = AValue then exit;
  FGrooveUncheckedClr := AValue;
  if not Checked then Redraw;
end;           

procedure TCustomECSwitch.SetKnobHovered(AValue: Boolean);
begin
  if FKnobHovered = AValue then exit;
  FKnobHovered := AValue;
  Redraw;
end;       

procedure TCustomECSwitch.SetKnobIndent(AValue: SmallInt);
begin
  if FKnobIndent = AValue then exit;
  FKnobIndent := AValue;
  ResizeKnob;
  NeedCalculate := True;
  Redraw;
end;       

procedure TCustomECSwitch.SetState(AValue: TCheckBoxState);
begin
  if FState = AValue then exit;
  FState := AValue;
  if [csLoading, csDestroying, csDesigning]*ComponentState = [] then
    begin
      if assigned(OnChange) then OnChange(self);
      { Execute only when Action.Checked is changed }
      if not CheckFromAction then
        begin
          if assigned(OnClick) then
            if not (assigned(Action) and 
            CompareMethods(TMethod(Action.OnExecute), TMethod(OnClick))) 
            then OnClick(self); 
          if assigned(Action) and (Action is TCustomAction) and
            (TCustomAction(Action).Checked <> (AValue = cbChecked)) 
            then ActionLink.Execute(self);
        end;
    end; 
  Redraw;
end;   

procedure TCustomECSwitch.SetSwitchColor(AValue: TColor);
begin
  if FSwitchColor = AValue then exit;
  FSwitchColor := AValue;
  if Style = eosPanel then
    begin
      SetKnobBackground;
      Redraw;
    end;
end;   

procedure TCustomECSwitch.SetSwitchHeight(AValue: Integer);
begin
  if FSwitchHeight = AValue then exit;
  FSwitchHeight := AValue;
  ResizeKnob;
  RecalcInvalidate;
end;

procedure TCustomECSwitch.SetSwitchWidth(AValue: Integer);
begin
  if FSwitchWidth = AValue then exit;
  FSwitchWidth := AValue;
  ResizeKnob;
  RecalcInvalidate;
end;   

end.


