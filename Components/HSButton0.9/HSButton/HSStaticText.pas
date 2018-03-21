{
    HSButtons Suite - Two new buttons for Delphi!
    Copyright (C) 2000-2010  Leif Bruder, Haitham Shatti
    e-Mail: haitham.shatti@gmail.com

    This file is part of HSButtons Suite.

    HSButtons Suite is free software; you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation; either version 2.1 of the License, or
    (at your option) any later version.

    HSButtons Suite is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with HSButtons Suite; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

unit HSStaticText;

interface

uses
  {$ifdef Win32}Windows, {$endif}
  {$ifdef Darwin}LCLIntf, lcltype, MacOSAll, CarbonUtils, CarbonDef, CarbonProc, {$endif}
  {$ifdef fpc} LMessages {$else} messages {$endif}, Classes, Graphics, Controls, Forms, HSButtons;

type
   THSStaticText = class(TCustomControl)
   private
      FAlignment: TAlignment;
      FFlat: boolean;
      FWordWrap: boolean;
      FGlyph: TBitmap;
      FHotTrackColor: TColor;
      FHTFont: TFont;
      FLayout: THSButtonLayout;
      FLightColor: TColor;
      FNumGlyphs: integer;
      FShadowColor: TColor;
      FStyle: THSButtonStyle;
      FUseHTFont: boolean;
      FOnMouseEnter: TNotifyEvent;
      FOnMouseExit: TNotifyEvent;
      FDummyStyle: THSColorStyle;
      bCursorOnButton: boolean;
      FTimer: THSButtonTimer;
      FSlowDecease: THSSlowDecease;
      FHotTrackStep: integer;
    FColor: TColor;
    FBorder: SmallInt;
    FSmooth: SmallInt;
    FIndex: integer;
    procedure SetColor(const Value: TColor);
    procedure SetBorder(const Value: SmallInt);
    procedure SetSmooth(const Value: SmallInt);
   public
      constructor Create(aOwner: TComponent); override;
      destructor Destroy; override;
      procedure Click; override;
   protected
      procedure Paint; override;
      procedure DoStep(Sender: TObject);

      function GetCaption: TCaption;

      procedure SetAlignment(fNew: TAlignment);
      procedure SetFlat(fNew: boolean);
      procedure SetColorStyle(fNew: THSColorStyle);
      procedure SetCaption(const fNew: TCaption);
      procedure SetGlyph(fNew: TBitmap);
      procedure SetHTFont(fNew: TFont);
      procedure SetLayout(fNew: THSButtonLayout);
      procedure SetLightColor(fNew: TColor);
      procedure SetNumGlyphs(fNew: integer);
      procedure SetShadowColor(fNew: TColor);
      procedure SetSlowDecease(fNew: THSSlowDecease);
      procedure SetStyle(fNew: THSButtonStyle);
      procedure SetWordWrap(fNew: boolean);

      procedure DoMouseEnter(var Msg: TLMessage); message CM_MOUSEENTER;
      procedure DoMouseLeave(var Msg: TLMessage); message CM_MOUSELEAVE;
      procedure DoDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
   published
      property Align;
      property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
      property Anchors;
      property Border:Smallint read FBorder write SetBorder default 1;
      property Caption: TCaption read GetCaption write SetCaption;
      property Color:TColor read FColor write SetColor;
      property ColorStyle: THSColorStyle read FDummyStyle write SetColorStyle default lcsCustom;
      property Flat: boolean read FFlat write SetFlat default false;
      property Font;
      property Glyph: TBitmap read FGlyph write SetGlyph;
      property HotTrackColor: TColor read FHotTrackColor write FHotTrackColor default clNone;
      property HotTrackFont: TFont read FHTFont write SetHTFont;
      property Hint;
      property Layout: THSButtonLayout read FLayout write SetLayout default blGlyphLeft;
      property LightColor: TColor read FLightColor write SetLightColor default clWhite;
      property NumGlyphs: integer read FNumGlyphs write SetNumGlyphs default 0;
      property ParentColor;
      property ParentFont;
      property ParentShowHint;
      property PopupMenu;
      property ShadowColor: TColor read FShadowColor write SetShadowColor default clBlack;
      property ShowHint;
      property SlowDecease: THSSlowDecease read FSlowDecease write SetSlowDecease default sdNone;
      property Smooth:SmallInt read FSmooth write SetSmooth;
      property Style: THSButtonStyle read FStyle write SetStyle default bsNormal;
      property UseHotTrackFont: boolean read FUseHTFont write FUseHTFont default false;
      property Visible;
      property WordWrap: boolean read FWordWrap write SetWordWrap default false;
      property Index: integer read FIndex write FIndex;

      property OnClick;
      property OnDblClick;
      property OnMouseDown;
      property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
      property OnMouseExit: TNotifyEvent read FOnMouseExit write FOnMouseExit;
      property OnMouseMove;
      property OnMouseUp;
   end;

procedure Register;

implementation

{##############################################################################}

procedure Register;
begin
   RegisterComponents('HS', [THSStaticText]);
end;

{##############################################################################}

constructor THSStaticText.Create(aOwner: TComponent);
begin
   inherited;

   ControlStyle := ControlStyle + [csSetCaption];
   ControlStyle := ControlStyle - [csOpaque];

   Height := 23;
   Width := 100;
   FBorder:=1;
   bCursorOnButton := false;
   FLightColor := clWhite;
   FShadowColor := clBlack;
   FHotTrackColor := clNone;
   FWordWrap := false;
   FAlignment := taCenter;
   FDummyStyle := lcsCustom;
   FSlowDecease := sdNone;

   FGlyph := TBitmap.Create;
   FHtFont := TFont.Create;
   FHotTrackStep := 0;
   FIndex := -1;

   FTimer := THSButtonTimer.Create(DoStep);
//   FThread.FreeOnTerminate := false;

   if aOwner is TForm then FHtFont.Assign(TForm(aOwner).Font);
end;

{##############################################################################}

destructor THSStaticText.Destroy;
begin
   if Assigned(FTimer) then
      FTimer.Free;
   FHtFont.Free;
   FGlyph.Free;
   inherited;
end;

{##############################################################################}

procedure THSStaticText.SetAlignment(fNew: TAlignment);
begin
   FAlignment := fNew;
   paint;
end;

procedure THSStaticText.SetBorder(const Value: SmallInt);
begin
  FBorder := Value;
  Paint
end;

{##############################################################################}

procedure THSStaticText.SetCaption(const fNew: TCaption);
begin
   inherited Caption := fNew;
   invalidate;
end;

{##############################################################################}

function THSStaticText.GetCaption: TCaption;
begin
   Result := inherited Caption;
end;

{##############################################################################}

procedure THSStaticText.SetColor(const Value: TColor);
begin
  FColor := Value;
{  if FStyle in [bsGlass,bsModern] then} begin
    FLightColor:=ColorBright(FColor,128);
//    FHotTrackColor:=ColorBright(FColor,$20);

    FShadowColor:=ColorBright(FColor,-$80);
    if GetGrayScale(FColor)<$606060 then
      Font.Color:=clWhite
    else
      Font.Color:=clBlack;
  end;
//  ColorWhenDown:=clNone
end;

procedure THSStaticText.SetColorStyle(fNew: THSColorStyle);
var
   bModern, bQuicken: boolean;
   FColor, FontColor, FDummy: TColor;

begin
   if fNew = lcsCustom then exit;

   GetPreDefinedColors(fNew, FColor, FLightColor, FShadowColor, FDummy, FHotTrackColor, FontColor, FFlat, bModern, bQuicken);
//   Color := FColor;
   Font.Color := FontColor;
   FStyle := bsNormal; if bModern then FStyle := bsModern; if bQuicken then FStyle := bsQuicken;
   if fNew in [lcsGlassGold,lcsGlassChrome,lcsGlassBlue,lcsGlassRed,lcsGlassAqua] then
     FStyle:=bsGlass;
   Paint;
end;

{##############################################################################}

procedure THSStaticText.SetFlat(fNew: boolean);
begin
   FFlat := fNew;
    Invalidate;
end;

{##############################################################################}

procedure THSStaticText.SetGlyph(fNew: TBitmap);
begin
   if fNew <> nil then
   begin
      FGlyph.Assign(fNew);
      if fNew.Height <> 0 then FNumGlyphs := fNew.Width div fNew.Height else FNumGlyphs := 0;
   end
   else
   begin
      FGlyph.Height := 0;
      FNumGlyphs := 0;
   end;
   invalidate;
end;

{##############################################################################}

procedure THSStaticText.SetHTFont(fNew: TFont);
begin
   FHTFont.Assign(fNew);
end;

{##############################################################################}

procedure THSStaticText.SetLayout(fNew: THSButtonLayout);
begin
   FLayout := fNew;
   paint;
end;

{##############################################################################}

procedure THSStaticText.SetLightColor(fNew: TColor);
begin
   FLightColor := fNew;
   paint;
end;

{##############################################################################}

procedure THSStaticText.SetNumGlyphs(fNew: integer);
begin
   FNumGlyphs := fNew;
   invalidate;
end;

{##############################################################################}

procedure THSStaticText.SetShadowColor(fNew: TColor);
begin
   FShadowColor := fNew;
   paint;
end;

{##############################################################################}

procedure THSStaticText.SetSlowDecease(fNew: THSSlowDecease);
begin
   FSlowDecease := fNew;

   if FSlowDecease<>sdNone then
     begin
       if not Assigned(FTimer) then
         begin
           FTimer := THSButtonTimer.Create(DoStep);
//           FThread.FreeOnTerminate := false;
         end;
     end
   else
     begin
       if Assigned(FTimer) then
         begin
           FHotTrackStep := 0;
           FTimer.Free;
           FTimer := nil;
        end;
     end;
   Invalidate;
end;

procedure THSStaticText.SetSmooth(const Value: SmallInt);
begin
  FSmooth := Value;
  Paint
end;

{##############################################################################}

procedure THSStaticText.SetStyle(fNew: THSButtonStyle);
begin
   if fNew = bsModern then if GetDeviceCaps(Canvas.Handle, BITSPIXEL) <= 8 then if not (csDesigning in ComponentState) then fNew := bsNormal;
   FStyle := fNew;
   invalidate;
end;

{##############################################################################}

procedure THSStaticText.SetWordWrap(fNew: boolean);
begin
   FWordwrap := fNew;
   paint;
end;

{##############################################################################}

procedure THSStaticText.Paint;
var
//   aStyle: THSButtonStyle;
 //  aLayout: THSButtonLayout;
   aBitmap: TBitmap;
   aFont: TFont;
  {$ifdef FPC} mem:TMemoryStream;{$endif}

begin
   if not (Visible or (csDesigning in ComponentState)) or (csLoading in ComponentState) then exit;

{   aStyle := HSButtons.bsNormal;
   case FStyle of
      bsEncarta:  aStyle := HSButtons.bsEncarta;
      bsModern:   aStyle := HSButtons.bsModern;
      bsOld:      aStyle := HSButtons.bsOld;
      bsShape:    aStyle := HSButtons.bsShape;
      bsQuicken:  aStyle := HSButtons.bsQuicken;
   end;}

{   aLayout := HSButtons.blGlyphLeft;
   case FLayout of
      blGlyphTop:    aLayout := HSButtons.blGlyphTop;
      blGlyphRight:  aLayout := HSButtons.blGlyphRight;
      blGlyphBottom: aLayout := HSButtons.blGlyphBottom;
   end;}

   aBitmap := TBitmap.Create;
   aBitmap.PixelFormat:=pf32bit;
   aBitmap.Height := Height;
   aBitmap.Width := Width;

   if FStyle in [bsModern, bsShape, bsQuicken, bsGlass] then
   begin
      aBitmap.TransparentMode := tmAuto;
      aBitmap.Transparent := True;
      aBitmap.Canvas.Brush.Color := clFuchsia;
      aBitmap.Canvas.FillRect(Rect(0, 0, Width, Height));
      aBitmap.TransparentColor := clFuchsia;
   end;

   aFont := Font; if bCursorOnButton and FUseHTFont then aFont := FHtFont;
   HSPaintButton(aBitmap, Width, Height,FBorder, FHotTrackStep,FSmooth, FNumGlyphs, FGlyph, false, bCursorOnButton, false, true, Flat, FWordWrap, assigned(PopupMenu), FStyle, Color, clNone, FHotTrackColor, FLightColor, FShadowColor, aFont, FLayout, Caption, FAlignment);
   {$ifdef FPC}
   // for some reason FPC doesn't apply transparency
   // except of a stream or file was loaded fooling FPC below
   mem:=TMemoryStream.Create;
   aBitmap.SaveToStream(mem);
   mem.Position:=0;
   aBitmap.Clear;
   aBitmap.FreeImage;
   aBitmap.Transparent:=true;
   aBitmap.TransparentColor:=clFuchsia;
   aBitmap.LoadFromStream(mem);
   mem.Free;
   {$endif}

   Canvas.Draw(0, 0, aBitmap);
   aBitmap.Free;
end;

{##############################################################################}

procedure THSStaticText.DoMouseEnter(var Msg: TLMessage);
begin
   if not(csDesigning in Componentstate) and Enabled and Visible and (Parent <> nil) and Parent.Showing then
   begin
      bCursorOnButton := true;
      if FSlowDecease=sdEnterLeave then
        if Assigned(FTimer) then
           if (FTimer.Suspended) then FTimer.Start
           else
        else
      else
       begin
         FHotTrackStep := cHotTrackSteps;
         paint;
       end;
      if assigned(FOnMouseEnter) then FOnMouseEnter(self);
   end;
end;

{##############################################################################}

procedure THSStaticText.DoMouseLeave(var Msg: TLMessage);
begin
   bCursorOnButton := false;
   if FSlowDecease<>sdNone then
     if Assigned(FTimer) then
        if (FTimer.Suspended) then FTimer.Start
        else
     else
   else
     begin
       FHotTrackStep := 0;
       paint;
     end;
   if Enabled and Visible and (Parent <> nil) and Parent.Showing then if assigned(FOnMouseExit) then FOnMouseExit(self);
end;

{##############################################################################}

procedure THSStaticText.DoDialogChar(var Message: TCMDialogChar);
begin
   with Message do
   begin
      if IsAccel(CharCode, Caption) and Visible and (Parent <> nil) and Parent.Showing then
      begin
         Click;
         Result := 1;
      end
      else
         inherited;
   end;
end;

{##############################################################################}

procedure THSStaticText.Click;
begin
//   if assigned(PopupMenu) then PopupMenu.PopUp(ClientToScreen(Point(0, Height)).X + Width, ClientToScreen(Point(0, Height)).Y);
   if assigned(PopupMenu) then PopupMenu.PopUp(ClientToScreen(Point(0, Height)).X, ClientToScreen(Point(0, Height)).Y);
   inherited;  // just to make it public :-)
end;

{##############################################################################}

procedure THSStaticText.DoStep(Sender: TObject);
begin
   if csDestroying in ComponentState then exit;

   if bCursorOnButton then
     inc(FHotTrackStep)
   else
     dec(FHotTrackStep);
   if (0>=FHotTrackStep) or (FHotTrackStep>=cHotTrackSteps) then
   begin
      if Assigned(FTimer) and (not FTimer.Suspended) then
          FTimer.Suspend;
      if bCursorOnButton then FHotTrackStep:=cHotTrackSteps else FHotTrackStep := 0;
   end;

   paint;
end;

{##############################################################################}

end.
