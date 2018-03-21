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

unit HSSpeedButton;

interface

uses
  {$ifdef Win32}Windows, {$endif}
  {$ifdef Darwin}LCLIntf, lcltype, MacOSAll, CarbonUtils, CarbonDef, CarbonProc, {$endif}
  SysUtils, {$ifdef fpc} LMessages {$else} messages {$endif}, Classes, Graphics, Controls, Forms, ImgList, ActnList, HSButtons;

type
   THSSpeedButton = class(TGraphicControl)
   private
      FAlignment: TAlignment;
      FAllowAllUp: boolean;
      FColorWhenDown: TColor;
      FDown: boolean;
      {$ifdef FPC} FMem:TMemoryStream;   {$endif}
      FFlat: boolean;
      FWordWrap: boolean;
      FGlyph: TBitmap;
      FGroupIndex: integer;
      FHotTrackColor: TColor;
      FHTFont: TFont;
      FLayout: THSButtonLayout;
      FLightColor: TColor;
      FNumGlyphs: integer;
      FShadowColor: TColor;
      FStyle: THSButtonStyle;
      FTransparent: boolean;
      FUseHTFont: boolean;
      FOnMouseEnter: TNotifyEvent;
      FOnMouseExit: TNotifyEvent;
      FDummyStyle: THSColorStyle;
      bCursorOnButton: boolean;
      FTimer: THSButtonTimer;
      FSlowDecease: THSSlowDecease;
      FHotTrackStep: integer;
      FTransparentColor: TColor;
    FColor: TColor;
    FBorder: Byte;
    FSmooth: Smallint;
    FIndex: integer;
    procedure SetColor(const Value: TColor);
    procedure SetBorder(const Value: Byte);
    procedure SetSmooth(const Value: Smallint);
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
      procedure SetColorWhenDown(fNew: TColor);
      procedure SetColorStyle(fNew: THSColorStyle);
      procedure SetDown(fNew: boolean);
      procedure SetCaption(const fNew: TCaption);
      procedure SetGlyph(fNew: TBitmap);
      procedure SetGroupIndex(fNew: integer);
      procedure SetHTFont(fNew: TFont);
      procedure SetLayout(fNew: THSButtonLayout);
      procedure SetLightColor(fNew: TColor);
      procedure SetNumGlyphs(fNew: integer);
      procedure SetShadowColor(fNew: TColor);
      procedure SetStyle(fNew: THSButtonStyle);
      procedure SetTransparent(fNew: boolean);
      procedure SetWordWrap(fNew: boolean);
      procedure SetSlowDecease(Value: THSSlowDecease);
      procedure DoMouseEnter(var Msg: TLMessage); message CM_MOUSEENTER;
      procedure DoMouseLeave(var Msg: TLMessage); message CM_MOUSELEAVE;
      procedure DoDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;

      procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
      procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
      procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
      {$ifdef Win32}
      procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
      {$endif}
      {$ifdef Darwin}
      procedure WMLButtonDblClk(var Message: TLMLButtonDblClk); message LM_LBUTTONDBLCLK;
      {$endif}
   published
      property Action;
      property Align;
      property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
      property AllowAllUp: boolean read FAllowAllUp write FAllowAllUp default false;
      property Anchors;
      property Border:Byte read FBorder write SetBorder default 4;
      property Caption: TCaption read GetCaption write SetCaption;
      property Color:TColor read FColor write SetColor;
      property ColorWhenDown: TColor read FColorWhenDown write SetColorWhenDown default clNone;
      property ColorStyle: THSColorStyle read FDummyStyle write SetColorStyle default lcsCustom;
      property Down: boolean read FDown write SetDown default false;
      property Enabled;
      property Flat: boolean read FFlat write SetFlat default false;
      property Font;
      property Glyph: TBitmap read FGlyph write SetGlyph;
      property GroupIndex: integer read FGroupIndex write SetGroupIndex default 0;
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
      property Smooth:Smallint read FSmooth write SetSmooth;
      property Style: THSButtonStyle read FStyle write SetStyle default bsNormal;
      property Transparent: boolean read FTransparent write SetTransparent default false;
      property UseHotTrackFont: boolean read FUseHTFont write FUseHTFont default false;
      property Visible;
      property WordWrap: boolean read FWordWrap write SetWordWrap default false;
      property Index: integer read FIndex write FIndex;
      property TransparentColor: TColor read FTransparentColor write FTransparentColor default clFuchsia;

      property OnClick;
//      property OnDblClick;
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
   RegisterComponents('HS', [THSSpeedButton]);
end;

{##############################################################################}

constructor THSSpeedButton.Create(aOwner: TComponent);
begin
   inherited;

//  ControlStyle := [csCaptureMouse, csDoubleClicks];
   ControlStyle := ControlStyle + [csSetCaption, csDoubleClicks, csCaptureMouse];
   ControlStyle := ControlStyle - [csOpaque];

   Height := 23;
   Width := 100;

   bCursorOnButton := false;
   FLightColor := clWhite;
   FShadowColor := clBlack;
   FColorWhenDown := clNone;
   {$ifdef FPC}   Fmem:=TMemoryStream.Create; {$endif}
   FHotTrackColor := clNone;
   FWordWrap := false;
   FAlignment := taCenter;
   FBorder:=1;
   FDummyStyle := lcsCustom;
   FSlowDecease := sdNone;

   FGlyph := TBitmap.Create;
   FHtFont := TFont.Create;
   FHotTrackStep := 0;
   FIndex := -1;

   FTimer := nil;
   FColor:=clBtnFace;
   if aOwner is TForm then
     FHtFont.Assign(TForm(aOwner).Font);
end;

{##############################################################################}

destructor THSSpeedButton.Destroy;
begin

   if Assigned(FTimer) then
   begin
     FTimer.Enabled:=False;
     FTimer.Free;
   end;
   {$ifdef FPC }FMem.Free;  {$endif FPC}
   FHtFont.Free;
   FGlyph.Free;
   inherited;
end;

{##############################################################################}

procedure THSSpeedButton.SetAlignment(fNew: TAlignment);
begin
   FAlignment := fNew;
   if FTransparent then invalidate else paint;  // Could have generally used invalidate, but why make it flicker if it is not transparent?
end;

procedure THSSpeedButton.SetBorder(const Value: Byte);
begin
  FBorder := Value;
  Invalidate
end;

{##############################################################################}

procedure THSSpeedButton.SetCaption(const fNew: TCaption);
begin
   inherited Caption := fNew;
   invalidate;
end;

{##############################################################################}

function THSSpeedButton.GetCaption: TCaption;
begin
   Result := inherited Caption;
end;

{##############################################################################}

procedure THSSpeedButton.SetColorWhenDown(fNew: TColor);
begin
   FColorWhenDown := fNew;
   if FTransparent then invalidate else paint;
end;

{##############################################################################}

procedure THSSpeedButton.SetColorStyle(fNew: THSColorStyle);
var
   bModern, bQuicken: boolean;
   FontColor: TColor;

begin
   if fNew = lcsCustom then exit;

   GetPreDefinedColors(fNew, FColor, FLightColor, FShadowColor, FColorWhenDown, FHotTrackColor, FontColor, FFlat, bModern, bQuicken);
//   Color := FColor;
   Font.Color := FontColor;
   FStyle := bsNormal; if bModern then FStyle := bsModern; if bQuicken then FStyle := bsQuicken;
   if fNew in [lcsGlassGold,lcsGlassChrome,lcsGlassBlue,lcsGlassRed,lcsGlassAqua] then
     FStyle:=bsGlass;
   Paint;
end;

{##############################################################################}

procedure THSSpeedButton.SetDown(fNew: boolean);
var
   i: integer;

begin
   if FDown = fNew then exit;

   // If grouped, set all siblings to down=false
   if GroupIndex <> 0 then
      for i := 0 to Parent.ControlCount-1 do
         if Parent.Controls[i] is THSSpeedButton then
            if THSSpeedButton(Parent.Controls[i]).GroupIndex = GroupIndex then
               if THSSpeedButton(Parent.Controls[i]) <> self then
                  THSSpeedButton(Parent.Controls[i]).Down := false;

   FDown := fNew;

   if FTransparent then invalidate else paint;
end;

{##############################################################################}

procedure THSSpeedButton.SetFlat(fNew: boolean);
begin
   FFlat := fNew;
   Invalidate;
end;

{##############################################################################}

procedure THSSpeedButton.SetGlyph(fNew: TBitmap);
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

procedure THSSpeedButton.SetGroupIndex(fNew: integer);
begin
   FGroupIndex := fNew;
   if FTransparent then invalidate else paint;
end;

{##############################################################################}

procedure THSSpeedButton.SetHTFont(fNew: TFont);
begin
   FHTFont.Assign(fNew);
end;

{##############################################################################}

procedure THSSpeedButton.SetLayout(fNew: THSButtonLayout);
begin
   FLayout := fNew;
   if FTransparent then invalidate else paint;
end;

{##############################################################################}

procedure THSSpeedButton.SetLightColor(fNew: TColor);
begin
   FLightColor := fNew;
   if FTransparent then invalidate else paint;
end;

{##############################################################################}

procedure THSSpeedButton.SetNumGlyphs(fNew: integer);
begin
   FNumGlyphs := fNew;
   invalidate;
end;

{##############################################################################}

procedure THSSpeedButton.SetShadowColor(fNew: TColor);
begin
   FShadowColor := fNew;
   if FTransparent then invalidate else paint;
end;

{##############################################################################}

procedure THSSpeedButton.SetSlowDecease(Value: THSSlowDecease);
begin
   FSlowDecease := Value;

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

procedure THSSpeedButton.SetSmooth(const Value: Smallint);
begin
  FSmooth := Value;
  Invalidate
end;

{##############################################################################}

procedure THSSpeedButton.SetStyle(fNew: THSButtonStyle);
begin
   if fNew = bsModern then if GetDeviceCaps(Canvas.Handle, BITSPIXEL) <= 8 then if not (csDesigning in ComponentState) then fNew := bsNormal;
   FStyle := fNew;
   invalidate;
end;

{##############################################################################}

procedure THSSpeedButton.SetTransparent(fNew: boolean);
begin
   FTransparent := fNew;
   if FTransparent then invalidate else paint;
end;

{##############################################################################}

procedure THSSpeedButton.SetWordWrap(fNew: boolean);
begin
   FWordwrap := fNew;
   if FTransparent then invalidate else paint;
end;

{##############################################################################}

procedure THSSpeedButton.Paint;
var
   aStyle: THSButtonStyle;
   aLayout: THSButtonLayout;
   aBitmap: TBitmap;
//   aPicture: TPicture;
begin
//   if not (Visible or (csDesigning in ComponentState)) or (csLoading in ComponentState) then exit;
   Inherited ;
   aStyle := FStyle;//HSButtons.bsNormal;
{   case FStyle of
      bsEncarta:  aStyle := HSButtons.bsEncarta;
      bsModern:   aStyle := HSButtons.bsModern;
      bsOld:      aStyle := HSButtons.bsOld;
      bsShape:    aStyle := HSButtons.bsShape;
      bsQuicken:  aStyle := HSButtons.bsQuicken;
   end;}

   aLayout := HSButtons.blGlyphLeft;
   case FLayout of
      blGlyphTop:    aLayout := HSButtons.blGlyphTop;
      blGlyphRight:  aLayout := HSButtons.blGlyphRight;
      blGlyphBottom: aLayout := HSButtons.blGlyphBottom;
   end;

   aBitmap := TBitmap.Create;
   aBitmap.PixelFormat:=pf32bit;
   aBitmap.Height := Height;
   aBitmap.Width := Width;

   if FTransparent or (FStyle in [bsGlass,bsModern, bsShape, bsQuicken]) then
   begin
//      aBitmap.TransparentMode := tmAuto;
      aBitmap.Canvas.Brush.Color := TransparentColor;
      aBitmap.Transparent := True;
      aBitmap.TransparentColor := TransparentColor;
      aBitmap.Canvas.FillRect(Rect(0, 0, Width, Height));
//      TWinControl(Self).SetShape(aBitmap)
   end;

   if bCursorOnButton and FUseHTFont then
       HSPaintButton(aBitmap, Width, Height,FBorder, FHotTrackStep,FSmooth, FNumGlyphs, FGlyph, FDown, bCursorOnButton, FTransparent, Enabled, Flat, FWordWrap, assigned(PopupMenu), aStyle, Color, FColorWhenDown, FHotTrackColor, FLightColor, FShadowColor, FHTFont, aLayout, Caption, FAlignment)
   else
       HSPaintButton(aBitmap, Width, Height,FBorder, FHotTrackStep,FSmooth, FNumGlyphs, FGlyph, FDown, bCursorOnButton, FTransparent, Enabled, Flat, FWordWrap, assigned(PopupMenu), aStyle, Color, FColorWhenDown, FHotTrackColor, FLightColor, FShadowColor, Font, aLayout, Caption, FAlignment);

{
   if FStyle in [bsShape, bsModern] then
   begin
      aPicture := TPicture.Create;
      try aPicture.Bitmap.Assign(aBitmap); except end;
      aPicture.Bitmap.Width := Width;
      aPicture.Bitmap.Height := Height;
      aPicture.Graphic.Transparent := true;
      Canvas.Draw(0, 0, aPicture.Graphic);
      aPicture.Free;
   end
   else
      Canvas.Draw(0, 0, aBitmap);
}

   aBitmap.TransparentColor:=TransparentColor;
   aBitmap.Transparent:=True;
   {$ifdef FPC}
   // for some reason FPC doesn't apply transparency
   // except of a stream or file was loaded fooling FPC below
   FMem.Clear;
   aBitmap.SaveToStream(Fmem);

   Fmem.Position:=0;
   aBitmap.Clear;
   aBitmap.FreeImage;
   aBitmap.Transparent:=true;
   aBitmap.TransparentColor:=TransparentColor;
   aBitmap.LoadFromStream(Fmem);
   {$endif}

   if Parent<>nil then
     begin
//       Canvas.Lock;
       Canvas.Draw(0, 0, aBitmap);
//       Canvas.Unlock;
     end;
   aBitmap.Free;
end;

{##############################################################################}

procedure THSSpeedButton.DoMouseEnter(var Msg: TLMessage);
begin
   if not(csDesigning in Componentstate) and Enabled and Visible and (Parent <> nil) and Parent.Showing then
   begin
      bCursorOnButton := true;
      if FSlowDecease=sdEnterLeave then
        if Assigned(FTimer) then
           if not (FTimer.Enabled) then
             FTimer.Start
           else
        else
      else
       begin
         FHotTrackStep := cHotTrackSteps;
         if FTransparent then invalidate else paint;
       end;
      if assigned(FOnMouseEnter) then FOnMouseEnter(self);
   end;
end;

{##############################################################################}

procedure THSSpeedButton.DoMouseLeave(var Msg: TLMessage);
begin
   bCursorOnButton := false;
   if FSlowDecease<>sdNone then
 //    if not Assigned(FThread) then FThread:=THSButtonThread.Create(DoStep);
     if Assigned(FTimer) then
        if not(FTimer.Enabled) then
          FTimer.Start
        else
     else
   else
     begin
       FHotTrackStep := 0;
       if FTransparent then invalidate else paint;
     end;
   if Enabled and Visible and (Parent <> nil) and Parent.Showing then if assigned(FOnMouseExit) then FOnMouseExit(self);
end;

{##############################################################################}

procedure THSSpeedButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   inherited;

   if (Button <> mbLeft) then exit;

   if GroupIndex = 0 then Down := true
   else
   begin
      if Down then
      begin
         if FAllowAllUp then Down := false;
      end
      else
         Down := true;
   end;
end;

{##############################################################################}

procedure THSSpeedButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   inherited;
   if GroupIndex = 0 then Down := false;
end;

{##############################################################################}

{$ifdef Win32}
procedure THSSpeedButton.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  Perform(WM_LBUTTONDOWN, Message.Keys, Longint(Message.Pos));
end;
{$endif}
{$ifdef Darwin}
procedure THSSpeedButton.WMLButtonDblClk(var Message: TLMLButtonDblClk);
begin
  Perform(LM_LBUTTONDOWN, Message.Keys, Longint(Message.Pos));
end;
{$endif}

{##############################################################################}

procedure THSSpeedButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);

   procedure CopyImage(ImageList: TCustomImageList; Index: Integer);
   begin
      with Glyph do
      begin
         Width := ImageList.Width;
         Height := ImageList.Height;
         Canvas.Brush.Color := clBlack;
         Canvas.FillRect(Rect(0,0, Width, Height));
         ImageList.Draw(Canvas, 0, 0, Index);
      end;
      NumGlyphs := 1;
      Paint;
   end;

begin
   inherited;

   if Sender is TCustomAction then
   begin
      with TCustomAction(Sender) do
      begin
         if (Glyph.Empty) and (ActionList <> nil) and (ActionList.Images <> nil) and (ImageIndex >= 0) and (ImageIndex < ActionList.Images.Count) then
            CopyImage(ActionList.Images, ImageIndex);
      end;
   end;
end;

{##############################################################################}

procedure THSSpeedButton.DoDialogChar(var Message: TCMDialogChar);
var
   bWasDown: boolean;

begin
   with Message do
   begin
      if IsAccel(CharCode, Caption) and Enabled and Visible and (Parent <> nil) and Parent.Showing then
      begin
         bWasDown := Down;
         Down := true;
         Paint;
         Click;
         Down := bWasDown;
         Paint;
         Result := 1;
      end
      else
         inherited;
   end;
end;

{##############################################################################}

procedure THSSpeedButton.Click;
begin
//   if assigned(PopupMenu) then PopupMenu.PopUp(ClientToScreen(Point(0, Height)).X + Width, ClientToScreen(Point(0, Height)).Y);
   if assigned(PopupMenu) then PopupMenu.PopUp(ClientToScreen(Point(0, Height)).X, ClientToScreen(Point(0, Height)).Y);
   inherited;  // just to make it public :-)
end;

{##############################################################################}

procedure THSSpeedButton.DoStep(Sender: TObject);
begin
   if csDestroying in ComponentState then exit;

   if bCursorOnButton then
     inc(FHotTrackStep)
   else
     dec(FHotTrackStep);
   if (0>=FHotTrackStep) or (FHotTrackStep>=cHotTrackSteps) then
   begin
      if bCursorOnButton then
        FHotTrackStep:=cHotTrackSteps
      else
        FHotTrackStep := 0;
      if Assigned(FTimer) and (FTimer.Enabled) then
        begin
          FTimer.Suspend;
        end;
   end;

   if FTransparent then invalidate else paint;
end;

{##############################################################################}

procedure THSSpeedButton.SetColor(const Value: TColor);
begin
//  inherited;
  FColor := Value;
{  if FStyle in [bsGlass,bsModern] then} begin
    FLightColor:=ColorBright(FColor,128);
    FHotTrackColor:=ColorBright(FColor,$20);

    FShadowColor:=ColorBright(FColor,-$80);
    if GetGrayScale(FColor)<$606060 then
      Font.Color:=clWhite
    else
      Font.Color:=clBlack;
  end;
  ColorWhenDown:=clNone
end;

end.
