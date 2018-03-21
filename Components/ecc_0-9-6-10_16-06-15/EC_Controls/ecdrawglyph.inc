{%MainUnit ectypes.pas}

{**************************************************************************************************
 This file is part of the Eye Candy Controls (EC-C)
  This is *include* file, belongs to ectypes.pas

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

procedure TCanvasHelper.DrawGlyph(ARect: TRect; AGlyphDesign: TGlyphDesign; AState: TItemState);
var aEvenH, aEvenW, x, y, x2, y2: SmallInt;
    
  procedure DrawArrowDown(ATop: SmallInt; ASize: SmallInt = 4);
  var i: SmallInt;
  begin
    for i := 4 - ASize to 2 do
      begin
        Line(x + i, ATop, x2 - i, ATop);
        inc(ATop);
      end;
    if aEvenW = 1
      then Line(x + 3, ATop, x2 - 3, ATop)
      else Pixels[x + 3, ATop] := Pen.Color;     
  end;
  
  procedure DrawArrowLeft(ALeft: SmallInt; ASize: SmallInt = 4);
  var i: SmallInt;
  begin
    if aEvenH = 1 
      then Line(ALeft, y + 3, aLeft, y2 - 3)     
      else Pixels[ALeft, y + 3] := Pen.Color;  
    for i := 2 downto 4 - ASize do
      begin
        inc(ALeft);
        Line(ALeft, y + i, ALeft, y2 - i);
      end;
  end;
  
  procedure DrawArrowRight(ALeft: SmallInt; ASize: SmallInt = 4);
  var i: SmallInt;
  begin
     for i := 4 - ASize to 2 do
      begin
        Line(ALeft, y + i, ALeft, y2 - i);
        inc(ALeft);
      end;
    if aEvenH = 1 
      then Line(ALeft, y + 3, aLeft, y2 - 3)     
      else Pixels[ALeft, y + 3] := Pen.Color; 
  end;
  
  procedure DrawArrowUp(ATop: SmallInt; ASize: SmallInt = 4);
  var i: SmallInt;
  begin
    if aEvenW = 1
      then Line(x + 3, ATop, x2 - 3, ATop)
      else Pixels[x + 3, ATop] := Pen.Color; 
     for i := 2 downto 4 - ASize do
      begin
        inc(ATop);
        Line(x + i, ATop, x2 - i, ATop);
      end;
  end;
  
  procedure DrawArrowUL(ATop: SmallInt);
  var i: SmallInt;
  begin
    i := x + 1;
    Line(i, ATop, x2 - 1, ATop);
    inc(ATop);
    Line(i, ATop, x2 - 2, ATop);
    inc(ATop);
    Line(i, ATop, x2 - 3, ATop);
    inc(ATop);
    Line(i, ATop, x2 - 4, ATop);
    inc(ATop);
    Line(i, ATop, x2 - 5, ATop);
    if aEvenW = 1 then inc(ATop);
    Pixels[i, ATop] := Pen.Color;
  end;
  
  procedure DrawArrowUR(ATop: SmallInt);
  var i: SmallInt;
  begin
    i := x2 - 1;
    Line(x + 1, ATop, i, ATop);
    inc(ATop);
    Line(x + 2, ATop, i, ATop);
    inc(ATop);
    Line(x + 3, ATop, i, ATop);
    inc(ATop);
    Line(x + 4, ATop, i, ATop);
    inc(ATop);
    Line(x + 5, ATop, i, ATop);
    if aEvenW = 1 then inc(ATop);
    dec(i);
    Pixels[i, ATop] := Pen.Color;
  end; 
  
  procedure DrawArrowDL(ATop: SmallInt);
  var i: SmallInt;
  begin
    i := x + 1;
    Pixels[i, ATop + 1] := Pen.Color;
    if aEvenW = 1 then inc(ATop);  
    inc(ATop);
    Line(i, ATop, x2 - 5, ATop);
    inc(ATop);
    Line(i, ATop, x2 - 4, ATop);
    inc(ATop);
    Line(i, ATop, x2 - 3, ATop);
    inc(ATop);
    Line(i, ATop, x2 - 2, ATop);
    inc(ATop);
    Line(i, ATop, x2 - 1, ATop);             
  end; 
  
  procedure DrawArrowDR(ATop: SmallInt);
  var i: SmallInt;
  begin
    i := x2 - 1;
    Pixels[i - 1, ATop + 1] := Pen.Color;
    if aEvenW = 1 then inc(ATop);  
    inc(ATop);
    Line(x + 5, ATop, i, ATop);
    inc(ATop);
    Line(x + 4, ATop, i, ATop);
    inc(ATop);
    Line(x + 3, ATop, i, ATop);
    inc(ATop);
    Line(x + 2, ATop, i, ATop);
    inc(ATop);
    Line(x + 1, ATop, i, ATop);  
  end; 
  
  procedure DrawArrowBDown(ATop: SmallInt; ASharpOdd: Boolean = True);
  var i: SmallInt;
  begin
    i := x;
    Line(i, ATop, i, ATop + 2);
    inc(i);
    Line(i, ATop, i, ATop + 3);
    inc(i);
    Line(i, ATop + 1, i, ATop + 4);
    inc(i);
    if (aEvenW = 1) or ASharpOdd 
      then Line(i, ATop + 2, i, ATop + 5)
      else Line(i, ATop + 2, i, ATop + 4);
    inc(i);
    if aEvenW = 1 then
      begin
        Line(i, ATop + 2, i, ATop + 5);
        inc(i);
      end;
    Line(i, ATop + 1, i, ATop + 4);
    inc(i);
    Line(i, ATop, i, ATop + 3);
    inc(i);
    Line(i, ATop, i, ATop + 2);
  end;     
  
  procedure DrawArrowBLeft(ALeft: SmallInt; ASharpOdd: Boolean = True);
  var j: SmallInt;
  begin
    j := y;
    Line(ALeft + 3, j, ALeft + 5, j);
    inc(j);
    Line(ALeft + 2, j, ALeft + 5, j);
    inc(j);
    Line(ALeft + 1, j, ALeft + 4, j);
    inc(j);
    if (aEvenH = 1) or ASharpOdd
      then Line(ALeft, j, ALeft + 3, j)
      else Line(ALeft + 1, j, ALeft + 3, j);
    inc(j);
    if aEvenH = 1 then
      begin
        Line(ALeft, j, ALeft + 3, j);
        inc(j);
      end;
    Line(ALeft + 1, j, ALeft + 4, j);
    inc(j);
    Line(ALeft + 2, j, ALeft + 5, j);
    inc(j);
    Line(ALeft + 3, j, ALeft + 5, j);
  end;

  procedure DrawArrowBRight(ALeft: SmallInt; ASharpOdd: Boolean = True);
  var j: SmallInt;
  begin
    j := y;
    Line(ALeft, j, ALeft + 2, j);
    inc(j);
    Line(ALeft, j, ALeft + 3, j);
    inc(j);
    Line(ALeft + 1, j, ALeft + 4, j);
    inc(j);
    if (aEvenH = 1) or ASharpOdd
      then Line(ALeft + 2, j, ALeft + 5, j)
      else Line(ALeft + 2, j, ALeft + 4, j);
    inc(j);
    if aEvenH = 1 then
      begin
        Line(ALeft + 2, j, ALeft + 5, j);  
        inc(j);
      end;
    Line(ALeft + 1, j, ALeft + 4, j);
    inc(j);
    Line(ALeft, j, ALeft + 3, j);
    inc(j);
    Line(ALeft, j, ALeft + 2, j);
  end;                       
  
  procedure DrawArrowBUp(ATop: SmallInt; ASharpOdd: Boolean = True);
  var i: SmallInt;
  begin
    i := x;
    Line(i, ATop + 3, i, ATop + 5);
    inc(i);
    Line(i, ATop + 2, i, ATop + 5);
    inc(i);
    Line(i, ATop + 1, i, ATop + 4);
    inc(i);
    if (aEvenW = 1) or ASharpOdd  
      then Line(i, ATop, i, ATop + 3)
      else Line(i, ATop + 1, i, ATop + 3);
    inc(i);
    if aEvenW = 1 then
      begin
        Line(i, ATop, i, ATop + 3);
        inc(i);
      end;
    Line(i, ATop + 1, i, ATop + 4);
    inc(i);
    Line(i, ATop + 2, i, ATop + 5);
    inc(i);
    Line(i, ATop + 3, i, ATop + 5);
  end;

  procedure DrawThemedGlyph(AThemedBtn: TThemedButton);
  var aDetails: TThemedElementDetails;
      aExtent: TSize;
  begin
    Brush.Color:=clBtnFace;
    aDetails := ThemeServices.GetElementDetails(AThemedBtn);
    aExtent := ThemeServices.GetDetailSize(aDetails);
    ARect.Left := (ARect.Right + ARect.Left - aExtent.cx) div 2;
    ARect.Top := (ARect.Bottom + ARect.Top - aExtent.cy) div 2;
    ARect.Right := ARect.Left + aExtent.cx;
    ARect.Bottom := ARect.Top + aExtent.cy;
    ThemeServices.DrawElement(Handle, aDetails, ARect);
  end;

const cRadioOff: array[TItemState] of TThemedButton = (tbRadioButtonUncheckedDisabled,
        tbRadioButtonUncheckedHot, tbRadioButtonUncheckedNormal, tbRadioButtonUncheckedNormal,
        tbRadioButtonUncheckedHot, tbRadioButtonUncheckedDisabled);
      cRadioOn: array[TItemState] of TThemedButton = (tbRadioButtonCheckedDisabled,
        tbRadioButtonCheckedHot, tbRadioButtonCheckedNormal, tbRadioButtonCheckedNormal,
        tbRadioButtonCheckedHot, tbRadioButtonCheckedDisabled);
      cCheckOff: array[TItemState] of TThemedButton = (tbCheckBoxUncheckedDisabled,
        tbCheckBoxUncheckedHot, tbCheckBoxUncheckedNormal, tbCheckBoxUncheckedNormal,
        tbCheckBoxUncheckedHot, tbCheckBoxUncheckedDisabled);
      cCheckOn: array[TItemState] of TThemedButton = (tbCheckBoxCheckedDisabled,
        tbCheckBoxCheckedHot, tbCheckBoxCheckedNormal, tbCheckBoxCheckedNormal,
        tbCheckBoxCheckedHot, tbCheckBoxCheckedDisabled);
begin
  Pen.Style:=psSolid;
  Pen.Width:=1;
  case AGlyphDesign of
    egdSizeArrUp: 
      begin
        x := ARect.Left + 1;
        x2 := ARect.Right - 1;
        y2 := (ARect.Bottom + ARect.Top) div 2 + (x2 - x) div 4;
        while x<x2 do
          begin
            Line(x, y2, x2, y2);
            inc(x);
            dec(x2);
            dec(y2);
          end;
      end;
    egdSizeArrRight: 
      begin
        y := ARect.Top + 1;
        y2 := ARect.Bottom - 1;
        x2 := (ARect.Right + ARect.Left) div 2 - (y2 - y) div 4;
        while y<y2 do
          begin
            Line(x2, y, x2, y2);
            inc(y);
            dec(y2);
            inc(x2);
          end;         
      end;
    egdSizeArrDown:
      begin
        x := ARect.Left + 1;
        x2 := ARect.Right - 1;
        y2 := (ARect.Bottom+ARect.Top) div 2 - (x2 - x) div 4 + 1;
        while x<x2 do
          begin
            Line(x, y2, x2, y2);
            inc(x);
            dec(x2);
            inc(y2);
          end;                
      end;
    egdSizeArrLeft: 
      begin
        y := ARect.Top + 1;
        y2 := ARect.Bottom - 1;
        x2 := (ARect.Right + ARect.Left) div 2 + (y2 - y) div 4;
        while y<y2 do
          begin
            Line(x2, y, x2, y2);
            inc(y);
            dec(y2);
            dec(x2);
          end;    
      end;
    egdRectBeveled:
      begin
        Frame3D(ARect, 1, bvLowered);
        FillRect(ARect);
      end;
    egdRectFramed:
      begin
        Pen.Color := clBtnText;
        Rectangle(ARect);
      end;
    egdWinRectClr:
      begin
        Pen.Color := clBtnText;
        Frame(ARect);
        y := ARect.Top + 1;
        Line(ARect.Left + 1, y, ARect.Left + 3, y);
        Line(ARect.Right - 3, y, ARect.Right - 1, y);
        inc(y);
        Line(ARect.Left + 1, y, ARect.Right - 1, y);  
        FillRect(ARect.Left + 2, y + 2 , ARect.Right - 2, ARect.Bottom - 2);        
      end;
    egdWinRoundClr:
      begin
        Pen.Color := clBtnText;
        InflateRect(ARect, -1, -1);
        Frame(ARect);
        x := ARect.Left - 1;
        Line(x, ARect.Top, x, ARect.Bottom);
        x := ARect.Right;
        Line(x, ARect.Top, x, ARect.Bottom);
        y := ARect.Top - 1;
        Line(ARect.Left, y, ARect.Right, y);
        y := ARect.Bottom;
        Line(ARect.Left, y, ARect.Right, y);
        FillRect(ARect.Left + 2, ARect.Top + 5, ARect.Right - 2, ARect.Bottom - 2);
        Brush.Color := clBtnText;
        y := ARect.Top + 2;
        FillRect(ARect.Left + 2, y, ARect.Right - 2, y + 2);
      end;
    egdWindowRect: 
      begin
        Frame(ARect);
        y := ARect.Top + 1;
        Line(ARect.Left + 1, y, ARect.Left + 3, y);
        Line(ARect.Right - 3, y, ARect.Right - 1, y);
        inc(y);
        Line(ARect.Left + 1, y, ARect.Right - 1, y);
        y := (ARect.Bottom + ARect.Top + 2) div 2;
        Line(ARect.Left + 3, y, ARect.Right - 3, y);
        dec(y, 2);
        Line(ARect.Left + 3, y, ARect.Right - 3, y);
        inc(y, 4);
        if y < ARect.Bottom then Line(ARect.Left + 3, y, ARect.Right - 3, y);
      end; 
    egdWindowRound:
      begin
        InflateRect(ARect, -1, -1);
        Frame(ARect);
        x := ARect.Left - 1;
        Line(x, ARect.Top, x, ARect.Bottom);
        x := ARect.Right;
        Line(x, ARect.Top, x, ARect.Bottom);
        y := ARect.Top - 1;
        Line(ARect.Left, y, ARect.Right, y);
        y := ARect.Bottom;
        Line(ARect.Left, y, ARect.Right, y);
        y := ARect.Top + 2;
        FillRect(ARect.Left + 2, y, ARect.Right - 2, y + 2);
        y := (ARect.Bottom + ARect.Top + 1) div 2;
        Line(ARect.Left + 2, y, ARect.Right - 2, y);
        inc(y, 2);
        if y < ARect.Bottom then Line(ARect.Left + 2, y, ARect.Right - 2, y);
      end;
    egdMenu:
      begin
        x := (ARect.Bottom - ARect.Top) div 12;
        InflateRect(ARect, -x, -x);
        y2 := ARect.Bottom - ARect.Top;
        y := y2 div 5;  { line }
        x := y;         { space }
        case y2 mod 5 of
          2: inc(x);     { l-S-l-S-l }
          3, 4: inc(y);  { L-s-L-s-L }
        end;
        for x2 := 0 to 2 do
          begin
            for y2 := 0 to y - 1 do
              Line(ARect.Left, ARect.Top + y2, ARect.Right, ARect.Top + y2);
            inc(ARect.Top, x + y);
          end;
      end;
    otherwise 
      x := ARect.Right - ARect.Left;
      y := ARect.Bottom - ARect.Top;
      aEvenW := (x + 1) mod 2;
      aEvenH := (y + 1) mod 2;
      x := ARect.Left + (x - 7) div 2;
      y := ARect.Top + (y - 7) div 2;  
      x2 := x + 7 + aEvenW;
      y2 := y + 7 + aEvenH;
      case AGlyphDesign of  
        egdNone: ; 
       	egdArrowDec:
          begin
            if aEvenH = 1 then inc(y);
            Line(x, y, x2, y);
            inc(y);
            Line(x, y, x2, y);
            inc(y);
            Line(x + 1, y, x2 - 1, y);
            inc(y);
            Line(x + 1, y, x2 - 1, y);
            inc(y);
            Line(x + 2, y, x2 - 2, y);
            inc(y);
            Line(x + 2, y, x2 - 2, y);
            inc(y);
            if aEvenW = 1
              then Line(x + 3, y, x2 - 3, y)
              else Pixels[x + 3, y] := Pen.Color;  
          end;  
        egdArrowInc: 
          begin
            if aEvenW = 1
              then Line(x + 3, y, x2 - 3, y)
              else Pixels[x + 3, y] := Pen.Color;  
            inc(y);
            Line(x + 2, y, x2 - 2, y);
            inc(y);
            Line(x + 2, y, x2 - 2, y);
            inc(y);
            Line(x + 1, y, x2 - 1, y);
            inc(y);
            Line(x + 1, y, x2 - 1, y);
            inc(y);
            Line(x, y, x2, y);
            inc(y);
            Line(x, y, x2, y);
          end;  
        egdArrowUp: DrawArrowUp(y + 1 + aEvenH); 
        egdArrowRight: DrawArrowRight(x + 2 + aEvenW); 
        egdArrowDown: DrawArrowDown(y + 2); 
        egdArrowLeft: DrawArrowLeft(x + 1 + aEvenW);
        egdArrowUR: DrawArrowUR(y + 1);
        egdArrowDR: DrawArrowDR(y);
        egdArrowDL: DrawArrowDL(y);
        egdArrowUL: DrawArrowUL(y + 1);
        egdArrowsUU: 
          begin
            DrawArrowUp(y - 1 + aEvenH);
            DrawArrowUp(y + 3 + aEvenH);
          end; 
        egdArrowsRR: 
          begin
            DrawArrowRight(x + aEvenW);
            DrawArrowRight(x + 4 + aEvenW);
          end; 
        egdArrowsDD: 
          begin
            DrawArrowDown(y);
            DrawArrowDown(y + 4);
          end; 
        egdArrowsLL: 
          begin
            DrawArrowLeft(x - 1 + aEvenW);
            DrawArrowLeft(x + 3 + aEvenW);
          end;
        egdArrowsUD: 
          begin
            DrawArrowUp(y - 1);
            DrawArrowDown(y + 4 + aEvenH);
          end; 
        egdArrowsMiddle: 
          begin
            DrawArrowDown(y - 1);
            FillRect(x, y + 3, x2, y + 4 + aEvenH);
            DrawArrowUp(y + 4 + aEvenH);
          end;
        egdArrowsLR: 
          begin
            DrawArrowLeft(x - 1);
            DrawArrowRight(x + 4 + aEvenW);
          end; 
        egdArrowsHMiddle: 
          begin
            DrawArrowRight(x - 1);
            FillRect(x + 3, y, x + 4 + aEvenW, y2);
            DrawArrowLeft(x + 4 + aEvenW);
          end;
        egdArrowsMax:
          begin
            FillRect(x, y - 1, x2, y + aEvenH);
            DrawArrowUp(y + aEvenH);
            DrawArrowUp(y + 4 + aEvenH);
          end; 
        egdArrowMax:
          begin
            FillRect(x, y + 1, x2, y + 2 + aEvenH);
            DrawArrowUp(y + 2 + aEvenH);
          end;   
        egdArrowMin:
          begin
            DrawArrowDown(y + 1);
            FillRect(x, y + 5, x2, y + 6 + aEvenH);
          end;      
        egdArrowsMin: 
          begin
            DrawArrowDown(y - 1);
            DrawArrowDown(y + 3);
            FillRect(x, y + 7, x2, y + 8 + aEvenH);
          end;
        egdArrowsHMax: 
          begin
            DrawArrowRight(x - 1);
            DrawArrowRight(x + 3);
            FillRect(x + 7, y, x + 8 + aEvenW, y2);
          end;  
        egdArrowHMax: 
          begin
            DrawArrowRight(x + 1);
            FillRect(x + 5, y, x + 6 + aEvenW, y2); 
          end;
        egdArrowHMin: 
          begin
            FillRect(x + 1, y, x + 2 + aEvenW, y2);
            DrawArrowLeft(x + 2 + aEvenW); 
          end;
        egdArrowsHMin: 
          begin
            FillRect(x - 1, y, x + aEvenW, y2);
            DrawArrowLeft(x + aEvenW);
            DrawArrowLeft(x + 4 + aEvenW);
          end;
        egdArrowsUDHor:
          begin  
            Line(x - 3, y + 2, x + 4, y + 2);  {Down}
            Line(x - 2, y + 3, x + 3, y + 3);
            Line(x - 1, y + 4, x + 2, y + 4);
            Pixels[x, y + 5] := Pen.Color;
            Pixels[x2 - 1, y2 - 6] := Pen.Color;  {Up}
            Line(x2 - 2, y2 - 5, x2 + 1, y2 - 5);
            Line(x2 - 3, y2 - 4, x2 + 2, y2 - 4);
            Line(x2 - 4, y2 - 3, x2 + 3, y2 - 3);
          end;
        egdArrowsURDL_S: 
          begin
            Pixels[x + 1, y - 1] := Pen.Color;  {Up}
            Line(x, y, x + 3, y);
            Line(x - 1, y + 1, x + 4, y + 1);
            Line(x - 2, y + 2, x + 4 + aEvenW, y + 2);
            Line(x2 - 4 - aEvenW, y2 - 3, x2 + 2, y2 - 3);  {Down}
            Line(x2 - 4, y2 - 2, x2 + 1, y2 - 2);
            Line(x2 - 3, y2 - 1, x2 , y2 - 1);
            Pixels[x2 - 2, y2] := Pen.Color; 
            Line(x2 - 2, y - 3, x2 - 2, y + 3 + aEvenH);  {Right}
            Line(x2 - 1, y - 2, x2 - 1, y + 3);
            Line(x2, y - 1, x2, y + 2);
            Pixels[x2 + 1, y] := Pen.Color;
            Line(x + 1, y2 - 3 - aEvenH, x + 1, y2 + 3);  {Left}
            Line(x, y2 - 3, x, y2 + 2);
            Line(x - 1, y2 - 2, x - 1, y2 + 1);
            Pixels[x - 2, y2 - 1] := Pen.Color;
          end;  
        egdArrowsURDL_M:
          begin
            DrawArrowUp(y - 4);
            Line(x2 - 3, y + 1, x2 - 3, y2 - 1); 
            Line(x2 - 2, y + 1, x2 - 2, y2 - 1);
            Line(x2 - 1, y + 2, x2 - 1, y2 - 2);
            if aEvenH = 1 
              then Line(x2, y + 3, x2, y2 - 3)     
              else Pixels[x2, y + 3] := Pen.Color;
            DrawArrowDown(y2); 
            Line(x + 2, y + 1, x + 2, y2 - 1); 
            Line(x + 1, y + 1, x + 1, y2 - 1);
            Line(x, y + 2, x, y2 - 2);
            if aEvenH = 1 
              then Line(x - 1, y + 3, x - 1, y2 - 3)     
              else Pixels[x - 1, y + 3] := Pen.Color;
          end;
        egdArrowURDL_L:
          begin
            DrawArrowUp(y - 4);
            DrawArrowRight(x2);
            DrawArrowDown(y2);
            DrawArrowLeft(x - 4);
          end;
        egdArrowURDL_XL:
          begin
            DrawArrowUp(y - 5);
            DrawArrowRight(x2 + 1);
            DrawArrowDown(y2 + 1);
            DrawArrowLeft(x - 5);
          end;
        egdArrowsUL_DR:
          begin
            dec(x);
            dec(x2);  
            DrawArrowUL(y);
            inc(x, 2);
            inc(x2, 2);
            DrawArrowDR(y2 - 6);
          end;
        egdArrowsUR_DL:
          begin
            inc(x);
            inc(x2);
            DrawArrowUR(y);
            dec(x, 2);
            dec(x2, 2);
            DrawArrowDL(y2 - 6); 
          end;
        egdArrsB_Min: 
          begin          
            DrawArrowBDown(y - 2, False);
            DrawArrowBDown(y + 2);
            FillRect(x, y2, x2, y2 + 2);
          end;
        egdArrB_Min: 
          begin
            DrawArrowBDown(y);
            FillRect(x, y2 - 2, x2, y2);
          end;
        egdArrsB_DD: 
          begin
            DrawArrowBDown(y, False);
            DrawArrowBDown(y + 4);
          end;
        egdArrB_Down: DrawArrowBDown(y + 2);
        egdArrsB_Middle: 
          begin
            DrawArrowBDown(y - 3);
            FillRect(x, y + 3, x2, y2 - 3);
            DrawArrowBUp(y2 - 2);
          end;
        egdArrsB_UD: 
          begin
            DrawArrowBUp(y - 2);
            DrawArrowBDown(y2 - 3);
          end;
        egdArrB_Up: DrawArrowBUp(y + 1);
        egdArrsB_UU: 
          begin
            DrawArrowBUp(y2 - 8);
            DrawArrowBUp(y2 - 4, False);
          end;
        egdArrB_Max: 
          begin
            FillRect(x, y, x2, y + 2);
            DrawArrowBUp(y2 - 5);
          end;
        egdArrsB_Max: 
          begin
            FillRect(x, y - 2, x2, y);
            DrawArrowBUp(y2 - 7);
            DrawArrowBUp(y2 - 3, False);
          end;
        egdArrsB_HMin: 
          begin
            FillRect(x - 2, y, x, y2);
            DrawArrowBLeft(x2 - 7);
            DrawArrowBLeft(x2 - 3, False);
          end;
        egdArrB_HMin:
          begin
            FillRect(x, y, x + 2, y2);
            DrawArrowBLeft(x2 - 5);
          end;
        egdArrsB_LL: 
          begin
            DrawArrowBLeft(x - 1);
            DrawArrowBLeft(x + 3, False);
          end;
        egdArrB_Left: DrawArrowBLeft(x + 1);
        egdArrsB_HMiddle: 
          begin
            DrawArrowBRight(x - 2);
            DrawArrowBLeft(x2 - 3);
          end;
        egdArrsB_LR: 
          begin
            DrawArrowBLeft(x - 2);
            DrawArrowBRight(x2 -3);
          end;
        egdArrB_Right: DrawArrowBRight(x + 2);
        egdArrsB_RR: 
          begin
            DrawArrowBRight(x - 1, False);
            DrawArrowBRight(x + 3);
          end;
        egdArrB_HMax: 
          begin
            DrawArrowBRight(x);
            FillRect(x2 - 2, y, x2, y2);
          end;
        egdArrsB_HMax: 
          begin
            DrawArrowBRight(x - 2, False);
            DrawArrowBRight(x + 2);
            FillRect(x2, y, x2 + 2, y2);
          end; 
        egdArrC_Min: 
          begin
            DrawArrowDown(y + 1, 5);
            Line(x - 1, y + 6, x2 + 1, y + 6);
          end;
        egdArrC_DD: 
          begin
            DrawArrowDown(y2 - 8, 5);
            DrawArrowDown(y2 - 3, 5);
          end;
        egdArrC_Down: DrawArrowDown(y + 2, 5);
        egdArrC_Middle: 
          begin
            DrawArrowDown(y - 2 + aEvenH, 5);
            if aEvenH = 0 then Line(x - 1, y + 3, x2 + 1, y + 3);
            DrawArrowUp(y2 - 3 - aEvenH, 5);
          end;
        egdArrC_UD: 
          begin
            DrawArrowUp(y - 2, 5);
            DrawArrowDown(y2 - 3, 5);
          end;
        egdArrC_LR:
          begin
            DrawArrowLeft(x - 2, 5);
            DrawArrowRight(x2 - 3, 5);
          end;
        egdArrC_Up: DrawArrowUp(y + 1, 5);
        egdArrC_UU: 
          begin
            DrawArrowUp(y2 - 9, 5);
            DrawArrowUp(y2 - 4, 5);
          end;
        egdArrC_Max: 
          begin
            Line(x - 1, y + aEvenH, x2 + 1, y + aEvenH);
            DrawArrowUp(y + 1 + aEvenH, 5);
          end;
        egdArrC_URDL:
          begin
            DrawArrowUp(y - 7, 5);
            DrawArrowRight(x2 + 2, 5);
            DrawArrowDown(y2 + 2, 5);
            DrawArrowLeft(x - 7, 5);
          end;
        egdPlayRec: Ellipse(x, y, x2 - 1, y2 - 1);
        egdPlayPause:
          begin
            FillRect(x, y, x + 3, y2);
            FillRect(x2 - 3, y, x2, y2);
          end;
        egdPlayUpDown: 
          begin
            DrawArrowUp(y - 2);
            FillRect(x, y + 3, x2, y2 - 3);
            DrawArrowDown(y2 - 2);
          end;
        egdPlayStop: FillRect(x, y, x2, y2);
        egdPlayEject: 
          begin
            DrawArrowUp(y);
            FillRect(x, y + 5, x2, y2 - 1);
          end;
        egdPlayEjectD:
          begin
            FillRect(x, y + 1, x2, y2 - 5);
            DrawArrowDown(y2 - 4);
          end;
        egdMathBigMinus: 
          begin
            if aEvenW = 1 then
              begin
                if aEvenH = 0 then dec(y);
                FillRect(x, y + 3, x2, y + 5);
              end else
              FillRect(x - 1, y + 2, x2 + 1, y + 5);  
          end;    
        egdMathMinus: 
          begin
            if aEvenW = 1 then
              begin
                if aEvenH = 0 then dec(y);
                FillRect(x + 1, y + 3, x2 - 1, y + 5);
              end else
              FillRect(x, y + 2, x2, y + 5);  
          end;     
        egdMathEqual: 
          begin
            FillRect(x, y + 1, x2, y + 3);
            FillRect(x, y2 - 3, x2, y2 - 1);
          end;
        egdMathPlusMinus: 
          begin
            dec(y, 2);
            if aEvenW = 1 then
              begin
                FillRect(x + 1, y + 3, x2 - 1, y + 5);
                FillRect(x + 3, y + 1,  x + 5, y + 7); 
                FillRect(x + 1, y + 8, x2 - 1, y + 10);
              end else
              begin
                if aEvenH = 1 then inc(y);  
                FillRect(x, y + 2, x2, y + 5);  
                FillRect(x + 2, y, x + 5, y + 7);
                FillRect(x, y + 8, x2, y + 10);
              end;
          end;   
        egdMathPlus: 
          begin
            if aEvenW = 1 then
              begin
                if aEvenH = 0 then dec(y);
                FillRect(x + 1, y + 3, x2 - 1, y + 5);
                FillRect(x + 3, y + 1,  x + 5, y + 7);  
              end else
              begin
                FillRect(x, y + 2, x2, y + 5);  
                FillRect(x + 2, y, x + 5, y + 7);
              end;
          end;
        egdMathBigPlus: 
          begin
            if aEvenW = 1 then
              begin
                if aEvenH = 0 then dec(y);
                FillRect(x, y + 3, x2, y + 5);
                FillRect(x + 3, y,  x + 5, y + 8);  
              end else
              begin
                FillRect(x - 1, y + 2, x2 + 1, y + 5);  
                FillRect(x + 2, y - 1, x + 5, y + 8);
              end;
          end;    
        egdCombo: 
          begin
            dec(x);
            FillRect(x, y - 1, x2 + 1, y + 1);
            inc(y, 2);
            dec(x2);
            Line(x, y, x2, y);
            inc(y, 2);
            Line(x, y, x2, y);
            inc(y, 2);
            Line(x, y, x2, y);
            inc(y, 2);
            Line(x, y, x2, y);
          end;
        egdList: 
          begin
            dec(y);
            if aEvenH = 1 then 
              begin
                Line(x, y, x2, y);
                inc(y);
              end;
            Line(x, y, x2, y);
            inc(y, 2);
            Line(x, y, x2, y);
            inc(y, 2);
            Line(x, y, x2, y);
            inc(y, 2);
            Line(x, y, x2, y);
            inc(y, 2);
            Line(x, y, x2, y);
          end;
        egdFramedList: 
          begin
            Frame(x - 1, y - 2, x2 + 1, y + 9);
            inc(x);
            dec(x2);
            Line(x, y, x2, y);
            inc(y, 2);
            Line(x, y, x2, y);
            inc(y, 2);
            Line(x, y, x2, y);
            inc(y, 2);
            Line(x, y, x2, y); 
          end;
        egdFrame: Frame(x - 1, y - 2, x2 + 1, y + 9);
        egdRadioOffTh: DrawThemedGlyph(cRadioOff[AState]);
        egdRadioOnTh: DrawThemedGlyph(cRadioOn[AState]);
        egdCheckOffTh: DrawThemedGlyph(cCheckOff[AState]);
        egdCheckOnTh: DrawThemedGlyph(cCheckOn[AState]);
        egdGrid:
          begin
            if aEvenH = 1 then dec(ARect.Bottom);
            Frame(ARect);                    
            x := ARect.Left + 3;
            Line(x, ARect.Top + 1, x, ARect.Bottom - 1);
            y := ARect.Top + 2;
            while y < (ARect.Bottom - 2) do
              begin
                Line(ARect.Left + 1, y, ARect.Right - 1, y);
                inc(y, 2);
              end;
            if (ARect.Right - ARect.Left) > 15 then
              begin
                x := x + (ARect.Right - x - 1) div 2;
                Line(x, ARect.Top + 1, x, ARect.Bottom - 1);
              end;
          end;
        egdGuidelines:
          begin
            x := (ARect.Right + 2*ARect.Left) div 3;
            Line(x, ARect.Top, x, ARect.Bottom);
            y := (ARect.Bottom + 2*ARect.Top) div 3;
            Line(ARect.Left, y, ARect.Right, y);
          end;
      end;  { case }                
  end;  { case }
end;   
  
