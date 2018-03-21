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

unit HSButtons; {$ifdef FPC}
{$mode Delphi}

{$endif}
interface

uses
  {$ifdef Win32}Windows, {$endif}
  {$ifdef Darwin}LCLIntf, lcltype, {$endif}
  sysutils, ExtCtrls, Graphics, Classes {$ifdef FPC}, GraphType, IntfGraphics {$endif},Dialogs;

const
   cHotTrackSteps = 8;
   cHotTrackSpeed = 30;

type
   THSColorStyle = (lcsCustom, lcsGold, lcsChrome, lcsBlue, lcsRed, lcsAqua, lcsGlassGold, lcsGlassChrome, lcsGlassBlue, lcsGlassRed, lcsGlassAqua,lcsUltraFlat1, lcsUltraFlat2, lcsUltraFlatXP, lcsDefault, lcsQuicken);
   THSButtonKind = (bkCustom, bkOK, bkCancel, bkHelp, bkYes, bkNo, bkClose, bkAbort, bkRetry, bkIgnore, bkAll);
   THSButtonLayout = (blGlyphLeft, blGlyphRight, blGlyphTop, blGlyphBottom);
   THSButtonStyle = (bsNormal, bsEncarta, bsModern, bsOld, bsShape, bsQuicken,bsGlass);
   THSDrawOrientation=(doTopBottom,doLeftRight,doBottomTop,doRightLeft);
   THSSlowDecease=(sdNone,sdEnterLeave,sdLeave);

   { THSButtonTimer }

   THSButtonTimer = class(TTimer)
   public


      constructor Create(DoNotify: TNotifyEvent);
      function Suspended:boolean;
      procedure Start;
      procedure Suspend;
   end;

//##############################################################################

procedure GetPreDefinedColors(ColorStyle: THSColorStyle; var Color, LightColor, ShadowColor, ColorWhenDown, HotTrackColor, FontColor: TColor; var Flat, Modern, Quicken: boolean);
procedure HSPaintButton(Bitmap: TBitmap; Width, Height,RoundArc, HotTrackStep,Smooth, NumGlyphs: Smallint; Glyph: TBitmap; Down, CursorOnButton, Transparent, Enabled, Flat, Wordwrap, PopupArrow: boolean; Style: THSButtonStyle; Color, ColorWhenDown, HotTrackColor, LightColor, ShadowColor: TColor; Font: TFont; Layout: THSButtonLayout; Caption: string; Alignment: TAlignment);
function ColorBright(C:TColor;B:Integer):TColor;
function GetGrayScale(C:TColor):TColor;

//##############################################################################

implementation

uses Math;

{ $R HSBUTTONS.RES}

//##############################################################################
function Limit(V,Min,Max:Integer):Integer;
begin
  if V>Max then
    Result:=Max
  else if V<Min then
    Result:=Min
  else Result:=V

end;

function GetGrayScale(C:TColor):TColor;
var g:Integer;
begin
  g:=(GetRValue(C)+GetGValue(C)+GetBValue(C)) div 3;
  Result:=RGB(g,g,g);
end;

function ColorBright(C:TColor;B:Integer):TColor;
begin
//  C:=ColorToRGB(C);
  Result:=RGB(Limit(GetRValue(c)+B,0,255),Limit(GetGValue(c)+B,0,255),Limit(GetBValue(c)+B,0,255))
end;

{ THSButtonTimer }

constructor THSButtonTimer.Create(DoNotify: TNotifyEvent);
begin
  inherited Create(nil);
//   Suspended:=True;
  Enabled:=False;
  Interval:=cHotTrackSpeed;
  OnTimer := DoNotify;

end;

function THSButtonTimer.Suspended: boolean;
begin
  result:=not Enabled;
end;

procedure THSButtonTimer.Start;
begin
  Enabled:=True;
end;

procedure THSButtonTimer.Suspend;
begin
  Enabled:=False;
end;


{------------------------------------------------------------------------------}


//##############################################################################

procedure GetPreDefinedColors(ColorStyle: THSColorStyle; var Color, LightColor, ShadowColor, ColorWhenDown, HotTrackColor, FontColor: TColor; var Flat, Modern, Quicken: boolean);
begin
   Quicken := false;
   Modern := false;
   Flat := false;
   FontColor := clBlack;

   case ColorStyle of
      lcsDefault:     begin Color := clBtnFace;  LightColor := clWhite;     ShadowColor := clGray;     ColorWhenDown := clNone;     HotTrackColor := clNone;     end;
      lcsGold:        begin Color := $0000C0C0;  LightColor := clYellow;    ShadowColor := clOlive;    ColorWhenDown := clNone;     HotTrackColor := $0000DFDF;  Modern := true; end;
      lcsChrome:      begin Color := clSilver;   LightColor := clWhite;     ShadowColor := clGray;     ColorWhenDown := clNone;     HotTrackColor := clNone;     Modern := true; end;
      lcsBlue:        begin Color := $00FF8000;  LightColor := clAqua;      ShadowColor := clBlue;     ColorWhenDown := clNone;     HotTrackColor := clNone;     Modern := true; end;
      lcsRed:         begin Color := clRed;      LightColor := $00C0C0FF;   ShadowColor := $000000C0;  ColorWhenDown := clNone;     HotTrackColor := clNone;     Modern := true; end;
      lcsAqua:        begin Color := $00ECCE94;  LightColor := $00FCE6D4;   ShadowColor := clBlack;    ColorWhenDown := clNone;     HotTrackColor := clNone;     Modern := true; end;
      lcsGlassGold:        begin Color := $0000C0C0;  LightColor := clYellow;    ShadowColor := clOlive;    ColorWhenDown := clNone;     HotTrackColor := $0000DFDF; end;
      lcsGlassChrome:      begin Color := clSilver;   LightColor := clWhite;     ShadowColor := clGray;     ColorWhenDown := clNone;     HotTrackColor := clNone; end;
      lcsGlassBlue:        begin Color := $00FF8000;  LightColor := clAqua;      ShadowColor := clBlue;     ColorWhenDown := clNone;     HotTrackColor := clNone; end;
      lcsGlassRed:         begin Color := clRed;      LightColor := $00C0C0FF;   ShadowColor := $000000C0;  ColorWhenDown := clNone;     HotTrackColor := clNone; end;
      lcsGlassAqua:        begin Color := $00ECCE94;  LightColor := clWhite;   ShadowColor := clBlack;    ColorWhenDown := clNone;     HotTrackColor := clNone; end;
      lcsUltraFlat1:  begin Color := clBtnFace;  LightColor := $00B59284;   ShadowColor := $00B59284;  ColorWhenDown := $00B59284;  HotTrackColor := $00DED3D6;  Flat := True;   end;
      lcsUltraFlat2:  begin Color := clBtnFace;  LightColor := clBlack;     ShadowColor := clBlack;    ColorWhenDown := $0024DABC;  HotTrackColor := $008CF6E4;  Flat := True;   end;
      lcsUltraFlatXP: begin Color := $00D1D8DB;  LightColor := $00B59284;   ShadowColor := $00B59285;  ColorWhenDown := $00B59285;  HotTrackColor := $00D2BDB6;  Flat := True;   end;
      lcsQuicken:     begin Color := $00603000;  LightColor := clSilver;    ShadowColor := clSilver;   ColorWhenDown := $00AD6529;  HotTrackColor := $00CEAE9C;  Quicken := true; FontColor := clWhite; end;
   end;
end;

//##############################################################################

procedure HSPaintButton(Bitmap:TBitmap ; Width, Height,RoundArc , HotTrackStep,Smooth, NumGlyphs: SmallInt; Glyph: TBitmap; Down, CursorOnButton, Transparent, Enabled, Flat, Wordwrap, PopupArrow: boolean; Style: THSButtonStyle; Color, ColorWhenDown, HotTrackColor, LightColor, ShadowColor: TColor; Font: TFont; Layout: THSButtonLayout; Caption: string; Alignment: TAlignment);

var
   iCaptionHeight, iCaptionWidth, iGlyphHeight, iGlyphWidth, iGlyphOffset: integer;
   dtMode: integer;
   iGlyphIndex: integer;
   iOffset, iVertHeight: integer;
   iBorder:Byte;
   clBackColor: TColor;
   iCapX, iCapY, iGlX, iGlY: integer;
   wR, wG, wB: word;
   aRect: TRect;
   FArrowGlyph: TPicture;

   procedure GoBlur(Bitmap:  TBitmap ;Density:SmallInt);
   type
     TRGBQuadArray = array[word] of TRGBQuad;
   var
     i,j,k:Integer;
     tR,tG,tB,tA:LongWord;
     RGBArray,aRGBArray:array of ^TRGBQuadArray;

     {$ifdef FPC}
     LazImage: TLazIntfImage;
     LazImageDescription:TRawImageDescription;
     {$endif}
   begin
     SetLength( RGBArray,Bitmap.Height);
     SetLength( aRGBArray,Bitmap.Height);
     {$ifdef fpc}
     LazImage:=Bitmap.CreateIntfImage;
     {$endif}

     for i := 0 to Bitmap.Height-1 do
       begin
         {$ifdef FPC}
         RGBArray[i]:= LazImage.GetDataLineStart(i);
         aRGBArray[i]:=LazImage.GetDataLineStart(i);;
         {$else}
         RGBArray[i]:=Bitmap.ScanLine[i];
         aRGBArray[i]:=Bitmap.ScanLine[i]
         {$endif}
       end;
     for i :=0  to Bitmap.Height - 1  do
       for j:=0 to Bitmap.Width - 1  do
         begin
           tR:=0;tG:=0;tB:=0;
           for k:=-Density  to Density  do
             begin
               if ((j+k)<0) or ((j+k)>Bitmap.Width-1) then
                 begin
                   tR:=tR+RGBArray[i][j].rgbRed;
                   tG:=tG+RGBArray[i][j].rgbGreen;
                   tB:=tB+RGBArray[i][j].rgbBlue;
                 end
               else
                 begin
                   tR:=tR+RGBArray[i][j+k].rgbRed;
                   tG:=tG+RGBArray[i][j+k].rgbGreen;
                   tB:=tB+RGBArray[i][j+k].rgbBlue;
                 end;
             end;
           aRGBArray[i][j].rgbRed:=tR div (1+2*Density);
           aRGBArray[i][j].rgbGreen:=tG div (1+2*Density);
           aRGBArray[i][j].rgbBlue:=tB div (1+2*Density);
         end;
     for i := 0 to Bitmap.Width -1 -0  do
       for j:=0 to Bitmap.Height-1 -0 do
         begin
           tR:=0;tG:=0;tB:=0;
           for k:=-Density to Density do
             begin
               if ((j+k)<0) or ((j+k)>Bitmap.Height-1) then
                 begin
                   tR:=tR+aRGBArray[j][i].rgbRed;
                   tG:=tG+aRGBArray[j][i].rgbGreen;
                   tB:=tB+aRGBArray[j][i].rgbBlue;
                 end
               else
                 begin
                   tR:=tR+aRGBArray[j+k][i].rgbRed;
                   tG:=tG+aRGBArray[j+k][i].rgbGreen;
                   tB:=tB+aRGBArray[j+k][i].rgbBlue;
                 end;
             end;
           RGBArray[j][i].rgbRed:=tR div (1+2*Density);
           RGBArray[j][i].rgbGreen:=tG div (1+2*Density);
           RGBArray[j][i].rgbBlue:=tB div (1+2*Density);
         end;
     {$ifdef FPC}
     Bitmap.LoadFromIntfImage(LazImage);
     {$endif}
//     aBitmap.FreeImage;
//     aBitmap.ReleaseHandle;
//     aBitmap.Free
   end;

   procedure DrawColorFade(StartColor, StopColor: TColor; iLeft, iTop, iRight, iBottom, iArc: integer;Orientation:THSDrawOrientation);
   var
      iCounter, iBuffer, iFillStep: integer;
      bR1, bG1, bB1, bR2, bG2, bB2: byte;
      aColor1, aColor2: LongInt;
      dCurrentR, dCurrentG, dCurrentB, dRStep, dGStep, dBStep: double;
      aOldStyle: TPenStyle;
      iHeight,iWidth,iCorner, iDrawBottom: integer;
      isCornerTop:boolean;

   begin
      iHeight := (iBottom - iTop);
      iWidth:= (iRight-iLeft);
      isCornerTop:= iTop<=1 ;

      aOldStyle := Bitmap.Canvas.Pen.Style; Bitmap.Canvas.Pen.Style := psClear;
      if Orientation =doTopBottom then begin
        aColor1 := ColorToRGB(StartColor); bR1 := GetRValue(aColor1); bG1 := GetGValue(aColor1); bB1 := GetBValue(aColor1);
        aColor2 := ColorToRGB(StopColor);  bR2 := GetRValue(aColor2); bG2 := GetGValue(aColor2); bB2 := GetBValue(aColor2);
      end;
      if Orientation=doBottomTop then begin
        aColor1 := ColorToRGB(StopColor); bR1 := GetRValue(aColor1); bG1 := GetGValue(aColor1); bB1 := GetBValue(aColor1);
        aColor2 := ColorToRGB(StartColor);  bR2 := GetRValue(aColor2); bG2 := GetGValue(aColor2); bB2 := GetBValue(aColor2);
      end;
      dCurrentR := bR1; dCurrentG := bG1; dCurrentB := bB1;
      dRStep := (bR2-bR1) / 31; dGStep := (bG2-bG1) / 31; dBStep := (bB2-bB1) / 31;

//      if Orientation=doTopBottom then
        iFillStep := (iHeight div 32) +1;
        for iCounter := 0 to 31 do
          begin
            iBuffer := iCounter * iHeight div 32;
            if isCornerTop then //Drawing rounded corners
              if iBuffer<iArc then
                iCorner:=iArc-Round(Sqrt(2*(iArc*iBuffer)-(iBuffer*iBuffer)))
              else
                iCorner:=0
            else
              if iBuffer>(iHeight-iArc) then
                iCorner:=iArc-Round(Sqrt(2*(iArc*(iHeight-iBuffer-1))-Sqr(iHeight-iBuffer-1)))
              else
                iCorner:=0;
            Bitmap.Canvas.Brush.Color := rgb(trunc(dCurrentR), trunc(dCurrentG), trunc(dCurrentB));
            dCurrentR := dCurrentR + dRStep; dCurrentG := dCurrentG + dGStep; dCurrentB := dCurrentB + dBStep;
            iDrawBottom := iTop + iBuffer + iFillStep; if iDrawBottom > iBottom then iDrawBottom := iBottom;
            Bitmap.Canvas.FillRect(Rect(iLeft+iCorner, iTop + iBuffer, iRight-iCorner, iDrawBottom));
          end;
      Bitmap.Canvas.Pen.Style := aOldStyle;
   end;

   function GetSteppedColor: TColor;
   var
      bR1, bG1, bB1, bR2, bG2, bB2: byte;
      aColor1, aColor2: LongInt;
      d1, d2: double;

   begin
      aColor1 := ColorToRGB(Color);
      aColor2 := ColorToRGB(HotTrackColor);
      bR1 := GetRValue(aColor1); bG1 := GetGValue(aColor1); bB1 := GetBValue(aColor1);
      bR2 := GetRValue(aColor2); bG2 := GetGValue(aColor2); bB2 := GetBValue(aColor2);
      d2 := HotTrackStep / cHotTrackSteps;
      d1 := 1 - d2;
      Result := RGB(Trunc(d1 * bR1 + d2 * bR2), Trunc(d1 * bG1 + d2 * bG2), Trunc(d1 * bB1 + d2 * bB2));
   end;

   procedure DrawGlyph(iDestLeft, iDestTop, iSrcLeft, iSrcTop, iWidth, iHeight: integer);  // transparent draw
   var
      aPicture: TPicture;

   begin
      aPicture := TPicture.Create;
      try aPicture.Bitmap.Assign(Glyph); except end;
      aPicture.Bitmap.Width := iWidth;
      aPicture.Bitmap.Height := iHeight;
      aPicture.Graphic.Transparent := true;
      aPicture.Bitmap.Canvas.Draw(-iSrcLeft, -iSrcTop, Glyph);
      Bitmap.Canvas.Draw(iDestLeft, iDestTop, aPicture.Graphic);
      aPicture.Free;
   end;

begin
  if not Enabled then Down := false;
  iOffset := 0; if Down then if Style in [bsNormal, bsModern, bsOld, bsShape, bsQuicken,bsGlass] then iOffset := 2;

  // Background
  clBackColor := colortorgb(Color);
  if Enabled then if HotTrackColor <> clNone then clBackColor := GetSteppedColor;
  if Down then if ColorWhenDown <> clNone then clBackColor := ColorWhenDown;
  if Style = bsShape then
    begin
      Bitmap.Canvas.Brush.Color := clFuchsia;
      Bitmap.Canvas.Rectangle(-1, -1, Width+1, Height+1)
    end
  else
    begin
      if not Transparent then
       begin
         Bitmap.Canvas.Brush.Color := clBackColor;
         if Style in [bsNormal, bsEncarta, bsOld] then
           Bitmap.Canvas.Rectangle(-1, -1, Width+1, Height+1)
         else
           if Style = bsModern then
             begin
               DrawColorFade(LightColor, clBackColor,0, 0, Width , Height div 4 + iOffset,RoundArc,doBottomTop);
               DrawColorFade(clBackColor, LightColor,0, Height div 4 + iOffset, Width , Height,RoundArc,doBottomTop);
             end
           else if Style=bsGlass then
             begin
               DrawColorFade(LightColor, ColorBright(LightColor,-$20), 0, 0, Width , Height div 2 + iOffset,RoundArc,doTopBottom);
               DrawColorFade(clBackColor, ColorBright(LightColor,0), 0, Height div 2 + iOffset, Width , Height,RoundArc,doTopBottom);
               if not Flat then
                 begin
                   Bitmap.Canvas.Pixels[1,Height div 2 + iOffset]:=ColorBright(LightColor,-$20);
                   Bitmap.Canvas.Pixels[Width-2,Height div 2 + iOffset]:=ColorBright(LightColor,-$20);
                 end
             end;

       end;
    end;

  Bitmap.Canvas.Brush.Style := bsclear;
   // Border
  if Style in [bsNormal, bsEncarta, bsOld] then
    begin
      if (Enabled and (not Flat or CursorOnButton or Down)) or (not Enabled and not Flat) then
        begin
          with Bitmap.Canvas do
            begin
              if Style = bsOld then
                begin
                  Pen.Color := ShadowColor;
//                  Pen.Width:=RoundArc;
                  if Down then
                    begin                         //  _
                      MoveTo(1, Height-3);       // |
                      LineTo(1, 1);
                      LineTo(Width-2, 1);
                    end
                  else
                    begin
                      MoveTo(Width-2, 0);          //   |
                      LineTo(Width-2, Height-2);   // -
                      LineTo(0, Height-2);
                    end;
                  if Down then Pen.Color := cl3DDkShadow else Pen.Color := cl3DLight;
                  MoveTo(0, Height-1);
                  LineTo(0, 0);
                  LineTo(Width-1, 0);
                  Pen.Width:=3;
                  if Down then Pen.Color := cl3DLight else Pen.Color := cl3DDkShadow;
                  LineTo(Width-1, Height-1);
                  LineTo(-1, Height-1);
                  Pen.Width:=1
                end
              else
                begin
 //                 Pen.Width:=RoundArc;
                 if RoundArc>0 then

                  for  iBorder:= 0 to RoundArc-1 do begin
                    if Down then Pen.Color := ShadowColor else Pen.Color := LightColor;
                    MoveTo(iBorder, Height-iBorder-1);
                    LineTo(iBorder, iBorder);
                    LineTo(Width-iBorder-1, iBorder);
                    if Down then Pen.Color := LightColor else Pen.Color := ShadowColor;
                    LineTo(Width-iBorder-1, Height-iBorder-1);
                    LineTo(iBorder, Height-iBorder-1);
                  end
                end;
            end;
        end;
      if Smooth>0 then GoBlur(Bitmap,Smooth);
    end
  else
    if (Style in [bsModern]) and not Flat then
      begin
        with Bitmap.Canvas do
          begin
            Pen.Color := clBackColor;
            if Down then Pen.Color := ShadowColor;
//            Rectangle(1, 1, Width-1, Height-1);
//            Pen.Color := ShadowColor;
            RoundRect(0, 0, Width, Height, RoundArc*2, RoundArc*2);
//            Ellipse(0,0,16,16);
          end;
      end
    else
      if (Style in [bsGlass]) and not Flat then
        begin
          with Bitmap.Canvas do
            begin
              Pen.Color := clBackColor; if Down then Pen.Color := ShadowColor;
//              Rectangle(1, 1, Width-1, Height-1);
//              Pen.Color := ShadowColor;
              RoundRect(0, 0, Width, Height, RoundArc*2, RoundArc*2);
            end;
        end
      else
        if Style in [bsQuicken] then
          begin
            with Bitmap.Canvas do
              begin
                Pen.Color := clBackColor;
                if Down then Pen.Color := ShadowColor;
                Pen.Style := psClear;
                Brush.Color := clBackColor;
                Brush.Style := bssolid;
                RoundRect(0, 1, Width, Height, RoundArc*2, RoundArc*2);
                Brush.Style := bsclear;
                Pen.Style := psSolid;
                Pen.Color := ShadowColor;
                MoveTo(RoundArc, 0); LineTo(Width-RoundArc-1, 0);
                MoveTo(RoundArc, Height-1); LineTo(Width-RoundArc-1, Height-1);
              end;
          end;
  // Prepare layout
  Bitmap.Canvas.Font := Font;
  if ((Style = bsEncarta) and down) {or ((Style = bsQuicken) and (CursorOnButton or Down))} then Bitmap.Canvas.Font.Style := Bitmap.Canvas.Font.Style + [fsBold];
    iGlX := 0;
  dtMode := DT_CENTER or DT_VCENTER or DT_SINGLELINE;
  iGlyphHeight := Glyph.Height;
  if NumGlyphs <> 0 then iGlyphWidth := Glyph.Width div NumGlyphs else iGlyphWidth := 0;
  iGlyphIndex := 0;
  if not Enabled then iGlyphIndex := iGlyphWidth
  else
    begin
      if CursorOnButton and (NumGlyphs > 3) then iGlyphIndex := 3 * iGlyphWidth;
      if Down and (NumGlyphs > 2) then iGlyphIndex := 2 * iGlyphWidth;
    end;

  case Alignment of
     taLeftJustify:  dtMode := DT_LEFT or DT_WORDBREAK;
     taRightJustify: dtMode := DT_RIGHT or DT_WORDBREAK;
     taCenter:       dtMode := DT_CENTER or DT_WORDBREAK;
  end;

  if Style = bsShape then
    begin
      iCaptionWidth := Width - 8; if iCaptionWidth < 0 then iCaptionWidth := 0;
      if not WordWrap then while Bitmap.Canvas.TextWidth(Caption) > iCaptionWidth do Caption := copy(Caption, 1, length(Caption)-1);
      aRect := Rect(0, 0, iCaptionWidth, 0);
      DrawText(Bitmap.Canvas.Handle, pChar(Caption), Length(Caption), aRect, DT_WORDBREAK or DT_CALCRECT);
      iCaptionHeight := aRect.Bottom;

      DrawGlyph((Width-iGlyphWidth) div 2, (Height-iGlyphHeight) div 2, iGlyphIndex, 0, iGlyphWidth, iGlyphHeight);
      aRect := Rect(iOffset, iOffset + (Height-iCaptionHeight) div 2, Width, Height);
      DrawText(Bitmap.Canvas.Handle, pChar(Caption), Length(Caption), aRect, DT_CENTER or DT_WORDBREAK);
    end
  else
    begin
      if Layout in [blGlyphLeft, blGlyphRight] then
        begin
          iCaptionWidth := Width - 8; if iGlyphWidth > 0 then dec(iCaptionWidth, iGlyphWidth + 4);
          if iCaptionWidth < 0 then iCaptionWidth := 0;
          if not WordWrap then while Bitmap.Canvas.TextWidth(Caption) > iCaptionWidth do Caption := copy(Caption, 1, length(Caption)-1);
          aRect := Rect(0, 0, iCaptionWidth, 0);
          DrawText(Bitmap.Canvas.Handle, pChar(Caption), Length(Caption), aRect, DT_WORDBREAK or DT_CALCRECT);
          iCaptionHeight := aRect.Bottom;
          iCapY := (Height - iCaptionHeight) div 2 + iOffset;
          iCapX := 4 + iOffset;
          iGlY := (Height - iGlyphHeight) div 2 + iOffset;
          iGlyphOffset := 0; if Caption <> '' then iGlyphOffset := aRect.Right + 4;

          if Layout = blGlyphLeft then
            begin
              if iGlyphWidth > 0 then inc(iCapX, iGlyphWidth + 4);
              iGlX := iOffset + 4;
              if not WordWrap then
                  case Alignment of
                    taLeftJustify:  iGlX := iOffset + 4;
                    taRightJustify: iGlX := Width - 4 - iGlyphWidth - iGlyphOffset + iOffset;
                    taCenter:       iGlX := (Width - iGlyphWidth) div 2 + iOffset - (iGlyphOffset div 2);
                  end;
            end
          else
            begin
              iGlX := iCapX + iGlyphOffset;
              if not WordWrap then
                  case Alignment of
                    taLeftJustify:  iGlX := iCapX + iGlyphOffset;
                    taRightJustify: iGlX := Width - 4 + iOffset - iGlyphWidth;
                    taCenter:       iGlX := (Width - iGlyphWidth) div 2 + iOffset + (iGlyphOffset div 2);
                  end;
            end;
        end
      else
        begin
          iCaptionWidth := Width - 8;
          if iCaptionWidth < 0 then iCaptionWidth := 0;
          if not WordWrap then while Bitmap.Canvas.TextWidth(Caption) > iCaptionWidth do Caption := copy(Caption, 1, length(Caption)-1);
          aRect := Rect(0, 0, iCaptionWidth, 0);
          DrawText(Bitmap.Canvas.Handle, pChar(Caption), Length(Caption), aRect, DT_WORDBREAK or DT_CALCRECT);
          iCaptionHeight := aRect.Bottom;
          iVertHeight := iCaptionHeight; if iGlyphHeight > 0 then inc(iVertHeight, iGlyphHeight + 4);
          iCapX := 4 + iOffset;

          if Layout = blGlyphTop then
            begin
              iCapY := (Height - iVertHeight) div 2 + iGlyphHeight + iOffset;
              iGlY := (Height - iVertHeight) div 2 + iOffset;
              case Alignment of
                taLeftJustify:  iGlX := 4 + iOffset;
                taRightJustify: iGlX := Width - iGlyphWidth - 4 + iOffset;
                taCenter:       iGlX := (Width - iGlyphWidth) div 2 + iOffset;
              end;
            end
          else
            begin
              iCapY := (Height - iVertHeight) div 2 + iOffset;
              iGlY := (Height - iVertHeight) div 2 + iCaptionHeight + iOffset;
              case Alignment of
                taLeftJustify:  iGlX := 4 + iOffset;
                taRightJustify: iGlX := Width - iGlyphWidth - 4 + iOffset;
                taCenter:       iGlX := (Width - iGlyphWidth) div 2 + iOffset;
              end;
            end;
        end;

      if not Enabled then Bitmap.Canvas.Font.Color := clGray;

      aRect := Rect(iCapX, iCapY, iCapX + iCaptionWidth, iCapY + iCaptionHeight);
      DrawText(Bitmap.Canvas.Handle, pChar(Caption), Length(Caption), aRect, dtMode);
      DrawGlyph(iGlX, iGlY, iGlyphIndex, 0, iGlyphWidth, iGlyphHeight);
   end;

   if PopupArrow then
   begin
      FArrowGlyph := TPicture.Create;
      FArrowGlyph.Bitmap.LoadFromResourceName(hInstance, 'HSARROW');
      FArrowGlyph.Graphic.Transparent := true;
      Bitmap.Canvas.Draw(Width - 11, Height div 2 - 1, FArrowGlyph.Graphic);
      FArrowGlyph.Free;
   end;
end;

//##############################################################################

end.
