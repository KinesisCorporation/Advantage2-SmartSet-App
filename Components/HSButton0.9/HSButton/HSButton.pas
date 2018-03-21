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

unit HSButton;

interface

uses
   {$ifdef Win32}Windows, {$endif}
   {$ifdef Darwin}LCLIntf, lcltype, MacOSAll, CarbonUtils, CarbonDef, CarbonProc, {$endif}
   {$ifdef fpc} LMessages {$else} messages {$endif},SysUtils, Classes, Graphics, Controls, Forms, ImgList, ActnList, HSButtons, Dialogs;

type

   { THSButton }

   THSButton = class(TCustomControl)
   private
     {$ifdef FPC}Fmem:TMemoryStream; {$endif}
     FAlignment: TAlignment;
     FShadowColor: TColor;
     FColorWhenDown: TColor;
     FEnabled: boolean;
     FFlat: boolean;
     FWordWrap: boolean;
     FGlyph: TBitmap;
     FHotTrackColor: TColor;
     FHTFont: TFont;
     FKind: THSButtonKind;
     FLayout: THSButtonLayout;
     FLightColor: TColor;
     FModalResult: TModalResult;
     FNumGlyphs: integer;
     FUseHTFont: boolean;
     FOnClick: TNotifyEvent;
     FOnMouseEnter: TNotifyEvent;
     FOnMouseExit: TNotifyEvent;
     FDummyStyle: THSColorStyle;
     FStyle: THSButtonStyle;
     FDefault, FCancel: boolean;
     bDown: boolean;
     bCursorOnButton: boolean;
     FTimer: THSButtonTimer;
     FHotTrackStep: integer;
     FColor: TColor;
     FBorder: Byte;
     FSmooth: Smallint;
     FSlowDecease: THSSlowDecease;
     FIndex: integer;
     procedure SetColor(const Value: TColor);
     procedure SetBorder(const Value: Byte);
     procedure SetSmooth(const Value: Smallint);
     procedure SetSlowDecease(Value: THSSlowDecease);
   public
      constructor Create(aOwner: TComponent); override;
      destructor Destroy; override;
      procedure Click; override;
   protected
      function GetCaption: TCaption;
      procedure DoExit; override;
      procedure SetAlignment(fNew: TAlignment);
      procedure SetCaption(const fNew: TCaption);
      procedure SetEnabled(fNew: boolean); override;
      procedure SetFlat(fNew: boolean);
      procedure SetGlyph(fNew: TBitmap);
      procedure SetHTFont(fNew: TFont);
      procedure SetKind(fNew: THSButtonKind);
      procedure SetLayout(fNew: THSButtonLayout);
      procedure SetLightColor(fNew: TColor);
      procedure SetModalResult(fNew: TModalResult);
      procedure SetNumGlyphs(fNew: integer);
      procedure SetStyle(fNew: THSButtonStyle);
      procedure SetShadowColor(fNew: TColor);
      procedure SetColorStyle(fNew: THSColorStyle);
      procedure SetWordWrap(fNew: boolean);

      procedure DoMouseEnter(var Msg: TLMessage); message CM_MOUSEENTER;
      procedure DoMouseLeave(var Msg: TLMessage); message CM_MOUSELEAVE;
      procedure DoFocusChanged(var Msg: Tobject); message CM_FOCUSCHANGED;
      procedure DoKeyDown(var Msg: TLMKeyDown); message CN_KEYDOWN;
      procedure DoKeyUp(var Msg: TLMKeyUp); message CN_KEYUP;
      procedure DoDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
      procedure DoDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;

//      procedure WMPaint(var Message: TWMPaint); message WM_PAINT;

      procedure Paint; override;
      procedure DoStep(Sender: TObject);
      procedure DoClick;
      procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
      procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
      procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
   published
      property Action;
      property Align;
      property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
      property Anchors;
      property Border:Byte read FBorder write SetBorder Default 4;
      property Cancel: boolean read FCancel write FCancel default false;
      property Caption: TCaption read GetCaption write SetCaption;
      property Color:TColor read FColor write SetColor;
      property ColorStyle: THSColorStyle read FDummyStyle write SetColorStyle default lcsCustom;
      property ColorWhenDown: TColor read FColorWhenDown write FColorWhenDown default clNone;
      property Default: boolean read FDefault write FDefault default false;
      property DragCursor;
      property DragKind;
      property DragMode;
//      property Enabled: boolean read FEnabled write SetEnabled default true;
      property Enabled;
      property Flat: boolean read FFlat write SetFlat default false;
      property Font;
      property Glyph: TBitmap read FGlyph write SetGlyph;
      property Hint;
      property HotTrackColor: TColor read FHotTrackColor write FHotTrackColor default clNone;
      property HotTrackFont: TFont read FHTFont write SetHTFont;
      property Kind: THSButtonKind read FKind write SetKind default bkCustom;
      property Layout: THSButtonLayout read FLayout write SetLayout default blGlyphLeft;
      property LightColor: TColor read FLightColor write SetLightColor default clWhite;
      property ModalResult: TModalResult read FModalResult write SetModalResult;
      property NumGlyphs: integer read FNumGlyphs write SetNumGlyphs default 0;
      property ParentColor;
      property ParentFont;
      property ParentShowHint;
      property PopupMenu;
      property ShadowColor: TColor read FShadowColor write SetShadowColor default clGray;
      property ShowHint;
      property SlowDecease: THSSlowDecease read FSlowDecease write SetSlowDecease default sdNone;
      property Style: THSButtonStyle read FStyle write SetStyle default bsNormal;
      property Smooth:Smallint read FSmooth write SetSmooth;
      property TabOrder;
      property TabStop default true;
      property UseHotTrackFont: boolean read FUseHTFont write FUseHTFont;
      property Visible;
      property WordWrap: boolean read FWordWrap write SetWordWrap default false;
      property Index: integer read FIndex write FIndex;

      property OnClick: TNotifyEvent read FOnClick write FOnClick;
      property OnDragDrop;
      property OnDragOver;
      property OnEndDrag;
      property OnEnter;
      property OnExit;
      property OnKeyDown;
      property OnKeyPress;
      property OnKeyUp;
      property OnMouseDown;
      property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
      property OnMouseExit: TNotifyEvent read FOnMouseExit write FOnMouseExit;
      property OnMouseMove;
      property OnMouseUp;
      property OnStartDrag;
   end;

procedure Register;

implementation

{##############################################################################}

procedure Register;
begin
   RegisterComponents('HS', [THSButton]);
end;

{##############################################################################}

constructor THSButton.Create(aOwner: TComponent);
begin
   inherited ;

   Height := 23;
   Width := 100;

   ControlStyle := [csSetCaption, csCaptureMouse];

   FGlyph := TBitmap.Create;
   FHTFont := TFont.Create;
   {$ifdef FPC}  Fmem:=TMemoryStream.Create;    {$endif}
   bDown := false;
   bCursorOnButton := false;
   FBorder:=1;
   FLightColor := clWhite;
   FShadowColor := clGray;
   FColorWhenDown := clNone;
   FEnabled := true;
   FStyle := bsNormal;
   FKind := bkCustom;
   TabStop := true;
   FDummyStyle := lcsCustom;
   FHotTrackColor := clNone;
   FAlignment := taCenter;
   FWordWrap := false;
   FIndex := -1;

   FDefault := false;
   FCancel := false;

  // FColor := clRed;
   FTimer := THSButtonTimer.Create(DoStep);
//   FThread.FreeOnTerminate := false;

   if aOwner is TForm then FHtFont.Assign(TForm(aOwner).Font);
end;

{##############################################################################}

destructor THSButton.Destroy;
begin
   if Assigned(FTimer) then
       FTimer.Free;
   FHtFont.Free;
   FGlyph.Free;
   {$ifdef FPC}Fmem.Free;  {$endif}
   inherited;
end;

{##############################################################################}

procedure THSButton.DoClick;
begin
   if Visible and Enabled and bDown then
   begin
      bDown := false;
      if assigned(FOnClick) then FOnClick(self);
      if FModalResult <> mrNone then GetParentForm(self).ModalResult := FModalResult;
      if assigned(PopupMenu) then PopupMenu.PopUp(ClientToScreen(Point(0, Height)).X, ClientToScreen(Point(0, Height)).Y);
   end;
end;

{##############################################################################}

procedure THSButton.Click;
begin
   bDown := true;
   DoClick;
   inherited;
end;

{##############################################################################}

procedure THSButton.SetAlignment(fNew: TAlignment);
begin
   FAlignment := fNew;
   Paint;
end;

procedure THSButton.SetBorder(const Value: Byte);
begin
  FBorder := Value;
  Invalidate
end;

{##############################################################################}

procedure THSButton.SetCaption(const fNew: TCaption);
begin
   inherited Caption := fNew;
   Invalidate;
end;

{##############################################################################}

function THSButton.GetCaption: TCaption;
begin
   result := inherited Caption;
end;

procedure THSButton.DoExit;
begin
  Paint;
end;

{##############################################################################}

procedure THSButton.SetColor(const Value: TColor);
begin
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

procedure THSButton.SetColorStyle(fNew: THSColorStyle);
var
   bModern, bQuicken: boolean;
   FColor, FontColor: TColor;

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

procedure THSButton.SetEnabled(fNew: boolean);
begin
   inherited;
   FEnabled := fNew;
   bDown := false;
   Paint;
end;

{##############################################################################}

procedure THSButton.SetFlat(fNew: boolean);
begin
   FFlat := fNew;
   Invalidate;
end;

{##############################################################################}

procedure THSButton.SetGlyph(fNew: TBitmap);
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
   FKind := bkCustom;
   Invalidate;
end;

{##############################################################################}

procedure THSButton.SetHTFont(fNew: TFont);
begin
   FHTFont.Assign(fNew);
end;

{##############################################################################}

procedure THSButton.SetKind(fNew: THSButtonKind);
begin
   if fNew <> bkCustom then FNumGlyphs := 2;
   case fNew of
      bkOK:     begin ModalResult := mrOK;      FGlyph.LoadFromResourceName(hInstance, 'BBOK');      Caption := 'OK';     end;
      bkCancel: begin ModalResult := mrCancel;  FGlyph.LoadFromResourceName(hInstance, 'BBCANCEL');  Caption := 'Cancel'; end;
      bkHelp:   begin ModalResult := mrNone;    FGlyph.LoadFromResourceName(hInstance, 'BBHELP');    Caption := 'Help';   end;
      bkYes:    begin ModalResult := mrYes;     FGlyph.LoadFromResourceName(hInstance, 'BBYES');     Caption := 'Yes';    end;
      bkNo:     begin ModalResult := mrNo;      FGlyph.LoadFromResourceName(hInstance, 'BBNO');      Caption := 'No';     end;
      bkClose:  begin ModalResult := mrNone;    FGlyph.LoadFromResourceName(hInstance, 'BBCLOSE');   Caption := 'Close';  end;
      bkAbort:  begin ModalResult := mrAbort;   FGlyph.LoadFromResourceName(hInstance, 'BBABORT');   Caption := 'Abort';  end;
      bkRetry:  begin ModalResult := mrRetry;   FGlyph.LoadFromResourceName(hInstance, 'BBRETRY');   Caption := 'Retry';  end;
      bkIgnore: begin ModalResult := mrIgnore;  FGlyph.LoadFromResourceName(hInstance, 'BBIGNORE');  Caption := 'Ignore'; end;
      bkAll:    begin ModalResult := mrAll;     FGlyph.LoadFromResourceName(hInstance, 'BBALL');     Caption := 'All';    end;
   end;

   FKind := fNew;
   Invalidate;
end;

{##############################################################################}

procedure THSButton.SetLayout(fNew: THSButtonLayout);
begin
   FLayout := fNew;
   Paint;
end;

{##############################################################################}

procedure THSButton.SetNumGlyphs(fNew: integer);
begin
   FNumGlyphs := fNew;
   Invalidate;
end;

{##############################################################################}

procedure THSButton.SetModalResult(fNew: TModalResult);
begin
   FModalResult := fNew;
   FKind := bkCustom;
end;

{##############################################################################}

procedure THSButton.SetLightColor(fNew: TColor);
begin
   FLightColor := fNew;
   Paint;
end;

{##############################################################################}

procedure THSButton.SetShadowColor(fNew: TColor);
begin
   FShadowColor := fNew;
   Paint;
end;

{##############################################################################}

procedure THSButton.SetSlowDecease(Value: THSSlowDecease);
begin
   FSlowDecease := Value;

   if FSlowDecease<>sdNone then
   begin
      if not Assigned(FTimer) then
      begin
         FTimer := THSButtonTimer.Create(DoStep);
//         FThread.FreeOnTerminate := false;
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

procedure THSButton.SetSmooth(const Value: Smallint);
begin
  FSmooth := Value;
  Invalidate
end;

{##############################################################################}

procedure THSButton.SetStyle(fNew: THSButtonStyle);
begin
   FStyle := fNew;
   if fStyle in [bsModern, bsGlass, bsShape, bsQuicken] then SetWindowLong(Handle, GWL_EXSTYLE, WS_EX_TRANSPARENT) else SetWindowLong(Handle, GWL_EXSTYLE, 0);
   Invalidate;
end;

{##############################################################################}

procedure THSButton.SetWordWrap(fNew: boolean);
begin
   FWordWrap := fNew;
   Paint;
end;

{##############################################################################}

procedure THSButton.DoMouseEnter(var Msg: TLMessage);
begin
   if Enabled and Visible and (Parent <> nil) and Parent.Showing then
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
         Repaint;
       end;
      if assigned(FOnMouseEnter) then FOnMouseEnter(self);
    end;
end;

{##############################################################################}

procedure THSButton.DoMouseLeave(var Msg: TLMessage);
begin
   bCursorOnButton := false;
   if FSlowDecease<>sdNone then
   begin
      if Assigned(FTimer) and (FTimer.Suspended) then FTimer.Start;
   end
   else
   begin
      FHotTrackStep := 0;
      Paint;
   end;
   if Enabled and Visible and (Parent <> nil) and Parent.Showing then if assigned(FOnMouseExit) then FOnMouseExit(self);
end;

{##############################################################################}

procedure THSButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   inherited;
   if Enabled and (Button = mbLeft) then
   begin
      bDown := true;
      SetFocus;
      Paint;
   end;
end;

{##############################################################################}

procedure THSButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   inherited;
//   if bCursorOnButton then Click;
   if bDown then Click;
   bDown := false;
   Paint;
end;

{##############################################################################}

procedure THSButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);

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

procedure THSButton.DoFocusChanged(var Msg: TObject);
begin
   Paint;
end;

{##############################################################################}

procedure THSButton.DoKeyDown(var Msg: TLMKeyDown);
begin
   inherited;
   if Enabled then if Msg.CharCode in [VK_SPACE, VK_RETURN] then
   begin
      bDown := true;
      Paint;
   end;
end;

{##############################################################################}

procedure THSButton.DoKeyUp(var Msg: TLMKeyUp);
begin
   inherited;
   if Msg.CharCode in [VK_SPACE, VK_RETURN] then Click;
   Paint;
end;

{##############################################################################}

procedure THSButton.Paint;
var
   aBitmap: TBitmap;
   aFont: TFont;
begin

   inherited ;
   if not (Visible or (csDesigning in ComponentState)) or (csLoading in ComponentState) or (csDestroying in ComponentState) then exit;

   // Draw on a Bitmap first, then just copy the Bitmap to the Canvas. Just to avoid flickering...
   aBitmap := TBitmap.Create;
   aBitmap.PixelFormat:=pf32bit;
   aBitmap.Height := Height;
   aBitmap.Width := Width;

   aFont := TFont.Create;
   aFont.Assign(Font);
   if (bCursorOnButton or focused) and FUseHTFont then aFont.Assign(FHtFont);

   if FStyle in [bsShape, bsModern, bsQuicken,bsGlass] then
   begin
//      aBitmap.TransparentMode := tmAuto;
      aBitmap.Canvas.Brush.Color := clBlack; //jm temp clFuchsia
      aBitmap.Canvas.Brush.Style:=bsSolid;
      aBitmap.Transparent := True;
      aBitmap.TransparentColor := clBlack; //jm temp clFuchsia
      aBitmap.Canvas.FillRect(Rect(0, 0, Width, Height));
   end;

   if (FStyle = bsOld) and Focused then
   begin
      if bDown then HSPaintButton(aBitmap, Width-2, Height-2,FBorder, FHotTrackStep,FSmooth, FNumGlyphs, FGlyph, bDown, bCursorOnButton or focused, false, Enabled, Flat, FWordWrap, assigned(PopupMenu), FStyle, Color, FColorWhenDown, FHotTrackColor, FShadowColor, FShadowColor, aFont, FLayout, Caption, FAlignment)
      else HSPaintButton(aBitmap, Width-2, Height-2,FBorder, FHotTrackStep,FSmooth, FNumGlyphs, FGlyph, bDown, bCursorOnButton or focused, false, Enabled, Flat, FWordWrap, assigned(PopupMenu), FStyle, Color, FColorWhenDown, FHotTrackColor, FLightColor, FShadowColor, aFont, FLayout, Caption, FAlignment);
      aBitmap.Canvas.Draw(1, 1, aBitmap);
      aBitmap.Canvas.Brush.Style := bsClear;
      aBitmap.Canvas.Pen.Color := clBlack;
      aBitmap.Canvas.Rectangle(0, 0, Width, Height);
   end
   else
      HSPaintButton(aBitmap, Width, Height,FBorder, FHotTrackStep,FSmooth, FNumGlyphs, FGlyph, bDown, bCursorOnButton or focused, false, Enabled, Flat, FWordWrap, assigned(PopupMenu), FStyle, Color, FColorWhenDown, FHotTrackColor, FLightColor, FShadowColor, aFont, FLayout, Caption, FAlignment);

   aBitmap.Transparent:=true;
   aBitmap.TransparentColor:=clBlack; //jm temp clFuchsia

   {$ifdef FPC}
   // for some reason FPC doesn't apply transparency
   // except of a stream or file was loaded fooling FPC below
   Fmem.Clear;
   aBitmap.SaveToStream(Fmem);
   Fmem.Position:=0;
   aBitmap.Clear;
   aBitmap.FreeImage;
   aBitmap.Transparent:=true;
   aBitmap.TransparentColor:=clBlack; //jm temp clFuchsia
   aBitmap.LoadFromStream(Fmem);
//   SetShape(aBitmap);

   {$endif}
   Canvas.Draw(0, 0, aBitmap);
   aFont.Free;
   aBitmap.Free;

   if focused and enabled and (FStyle <> bsShape) then Canvas.DrawFocusRect(Rect(4, 4, Width-4, Height - 4));
end;

{##############################################################################}

procedure THSButton.DoDialogChar(var Message: TCMDialogChar);
begin
   with Message do
   begin
      if IsAccel(CharCode, Caption) and Visible and Enabled and (Parent <> nil) and Parent.Showing then
      begin
         bDown := true;
         Click;
         Result := 1;
      end
      else
         inherited;
   end;
end;

{##############################################################################}

procedure THSButton.DoDialogKey(var Message: TCMDialogKey);

   function OwnerForm(From: TComponent): TComponent;
   begin
      if From is TForm then Result := From else Result := OwnerForm(From.Owner);
   end;

begin
   with Message do
   begin
      if (((CharCode = VK_RETURN) and FDefault) or ((CharCode = VK_ESCAPE) and FCancel) and (KeyDataToShiftState(Message.KeyData) = []) and Visible and Enabled) and ((pos('BUTTON', (TForm(OwnerForm(self)).ActiveControl.ClassName)) = 0) or (pos('RADIOBUTTON', AnsiUpperCase(TForm(OwnerForm(self)).ActiveControl.ClassName)) > 0)) then
      begin
         bDown := true;
         Click;
         Result := 1;
      end
      else
         inherited;
   end;
end;

{##############################################################################}

procedure THSButton.DoStep(Sender: TObject);
begin
   if csDestroying in ComponentState then exit;
   
   if bCursorOnButton then
     inc(FHotTrackStep)
   else
     dec(FHotTrackStep);
   if (FHotTrackStep <= 0) or (FHotTrackStep>=cHotTrackSteps) then
   begin
      if Assigned(FTimer) and (not FTimer.Suspended) then
          FTimer.Suspend;
      if bCursorOnButton then FHotTrackStep:=cHotTrackSteps else FHotTrackStep := 0;
   end;

   Paint;
end;

{##############################################################################}

end.
