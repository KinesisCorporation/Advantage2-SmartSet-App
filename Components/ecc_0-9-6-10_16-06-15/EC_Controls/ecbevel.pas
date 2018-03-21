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

unit ECBevel;
{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, ExtCtrls, Graphics, SysUtils, Types, ECTypes;

type
  {$PACKENUM 2}
  TPointPos = (eppNone, eppTopLeft, eppTopRight, eppBottomRight, eppBottomLeft);
  { event }
  TOnGetCoordinate = procedure (Sender: TObject; var AValue: Integer) of object;

  { TECBevel }
  TECBevel = class(TGraphicControl)
  private
    FOnGetPointAX: TOnGetCoordinate;
    FOnGetPointAY: TOnGetCoordinate;
    FOnGetPointBX: TOnGetCoordinate;
    FOnGetPointBY: TOnGetCoordinate;
    FPointA: TPointPos;
    FPointAX: Integer;
    FPointAY: Integer;
    FPointB: TPointPos;
    FPointBX: Integer;
    FPointBY: Integer;
    FShape: TBevelShape;
    FStyle: TBevelStyle;
    FThemed: Boolean;
    function GetPointAX: Integer;
    function GetPointAY: Integer;
    function GetPointBX: Integer;
    function GetPointBY: Integer;
    procedure SetPointA(AValue: TPointPos);
    procedure SetPointAX(AValue: Integer);
    procedure SetPointAY(AValue: Integer);
    procedure SetPointB(AValue: TPointPos);
    procedure SetPointBX(AValue: Integer);
    procedure SetPointBY(AValue: Integer);
    procedure SetShape(AValue: TBevelShape);
    procedure SetStyle(AValue: TBevelStyle);
    procedure SetThemed(AValue: Boolean);
  protected
    class function GetControlClassDefaultSize: TSize; override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property Constraints;
    property PointA: TPointPos read FPointA write SetPointA default eppNone;
    property PointAX: Integer read GetPointAX write SetPointAX default 0;
    property PointAY: Integer read GetPointAY write SetPointAY default 0;
    property PointB: TPointPos read FPointB write SetPointB default eppNone;
    property PointBX: Integer read GetPointBX write SetPointBX default 0;
    property PointBY: Integer read GetPointBY write SetPointBY default 0;
    property ParentShowHint;
    property ShowHint;
    property Shape: TBevelShape read FShape write SetShape default bsBox;
    property Style: TBevelStyle read FStyle write SetStyle default bsLowered;
    property Themed: Boolean read FThemed write SetThemed default False;
    property Visible;
    property OnGetPointAX: TOnGetCoordinate read FOnGetPointAX write FOnGetPointAX;
    property OnGetPointAY: TOnGetCoordinate read FOnGetPointAY write FOnGetPointAY;
    property OnGetPointBX: TOnGetCoordinate read FOnGetPointBX write FOnGetPointBX;
    property OnGetPointBY: TOnGetCoordinate read FOnGetPointBY write FOnGetPointBY;
    property OnChangeBounds;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnPaint;
    property OnResize;
  end;

const cECBevelAuto = -1;  { for lines; it set AX/AY to Width/Height-2 }

implementation

{ TECBevel }

constructor TECBevel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle:=ControlStyle-[csSetCaption,csOpaque];
  FPointA:=eppNone;
  FPointB:=eppNone;
  FStyle:=bsLowered;
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
end;

procedure TECBevel.Assign(Source: TPersistent);
begin
  if Source is TECBevel then
    begin
      FPointA:=TECBevel(Source).FPointA;
      FPointB:=TECBevel(Source).FPointB;
      FShape:=TECBevel(Source).FShape;
      FStyle:=TECBevel(Source).Style;
    end else
    inherited Assign(Source);
end;

class function TECBevel.GetControlClassDefaultSize: TSize;
begin
  Result.cx:=160;
  Result.cy:=160;
end;

procedure TECBevel.Paint;
var AColor, BColor: TColor;
    aTL, aTR, aBL, aBR: TPoint;
    x, y: Integer;
begin
  if not Themed then
    begin
      case Style of
        bsLowered:
          begin
            AColor:=cl3DShadow;
            BColor:=cl3DHilight;
          end;
        bsRaised:
          begin
            AColor:=cl3DHilight;
            BColor:=cl3DShadow;
          end;
      end;
      if (Shape in [bsBox, bsFrame]) or (PointA=eppNone) then
        case Shape of
          bsBox, bsFrame:
            begin
              aTL:=Point(0, 0);
              aTR:=Point(Width-1, 0);
              aBL:=Point(0, Height-1);
              aBR:=Point(aTR.X, aBL.Y);
              x:=PointAX;
              y:=PointAY;
              if (x>0) and (x<(Width-1)) and (y>0) and (y<(Height-1)) then
                case PointA of
                  eppTopLeft: aTL:=Point(x, y);
                  eppTopRight: aTR:=Point(x, y);
                  eppBottomRight: aBR:=Point(x, y);
                  eppBottomLeft: aBL:=Point(x, y);
                end;
              x:=PointBX;
              y:=PointBY;
              if (PointA<>PointB) and (x>0) and (x<(Width-1)) and (y>0) and (y<(Height-1)) then
                case PointB of
                  eppTopLeft: aTL:=Point(x, y);
                  eppTopRight: aTR:=Point(x, y);
                  eppBottomRight: aBR:=Point(x, y);
                  eppBottomLeft: aBL:=Point(x, y);
                end;
            end;
          bsTopLine, bsBottomLine:
            begin
              x:=PointAX;
              if x=cECBevelAuto then x:=Width-2;
              if x>=Width then x:=0;
              y:=Height-1;
            end;
          bsLeftLine, bsRightLine:
            begin
              y:=PointAY;
              if y=cECBevelAuto then y:=Height-2;
              if y>=Height then y:=0;
              x:=Width-1;
            end;
        end;
      with Canvas do
        begin
          Pen.Color:=AColor;
          Pen.Width:=1;
          if (Shape in [bsBox, bsFrame]) or (PointA=eppNone) then
            case Shape of
              bsBox:
                begin
                  MoveTo(aBL.X, self.Height-1);
                  if aBL.X>0 then
                    begin
                      LineTo(aBL.X, aBL.Y);
                      Pen.Color:=BColor;
                      LineTo(0, aBL.Y);
                      Pen.Color:=AColor;
                    end;
                  LineTo(0, aTL.Y);
                  if aTL.X>0 then
                    begin
                      LineTo(aTL.X, aTL.Y);
                      LineTo(aTL.X, 0);
                    end;
                  LineTo(aTR.X, 0);
                  Pen.Color:=BColor;
                  if aTR.Y>0 then
                    begin
                      LineTo(aTR.X, aTR.Y);
                      Pen.Color:=AColor;
                      LineTo(self.Width-1, aTR.Y);
                      Pen.Color:=BColor;
                    end;
                  LineTo(self.Width-1, aBR.Y);
                  if aBR.Y<(self.Height-1) then
                    begin
                      LineTo(aBR.X, aBR.Y);
                      LineTo(aBR.X, self.Height-1);
                    end;
                  LineTo(aBL.X, self.Height-1);
                end;
              bsFrame:
                begin
                  MoveTo(aBL.X, self.Height-2);
                  if aBL.X>0 then
                    begin
                      LineTo(aBL.X, aBL.Y-1);
                      Pen.Color:=BColor;
                      MoveTo(aBL.X+1, self.Height-1);
                      LineTo(PenPos.X, aBL.Y-1);
                      MoveTo(aBL.X-1, aBL.Y);
                      LineTo(0, aBL.Y);
                      Pen.Color:=AColor;
                      MoveTo(aBL.X, aBL.Y-1);
                      LineTo(0, aBL.Y-1);
                    end;
                  MoveTo(0, aBL.Y-1);
                  LineTo(0, aTL.Y);
                  Pen.Color:=BColor;
                  MoveTo(1, aBL.Y-2);
                  LineTo(1, aTL.Y+1);
                  Pen.Color:=AColor;
                  if aTL.X>0 then
                    begin
                      Pen.Color:=BColor;
                      LineTo(aTL.X+1, aTL.Y+1);
                      LineTo(PenPos.X, 1);
                      Pen.Color:=AColor;
                      MoveTo(0, aTL.Y);
                      LineTo(aTL.X, aTL.Y);
                      LineTo(aTL.X, 0);
                    end;
                  MoveTo(aTL.X, 0);
                  LineTo(aTR.X, 0);
                  Pen.Color:=BColor;
                  MoveTo(aTL.X+1, 1);
                  LineTo(aTR.X+1, 1);
                  if aTR.Y>0 then
                    begin
                      LineTo(aTR.X+1, aTR.Y);
                      Pen.Color:=AColor;
                      MoveTo(aTR.X, 0);
                      LineTo(aTR.X, aTR.Y);
                      MoveTo(aTR.X, aTR.Y);
                      LineTo(self.Width-1, aTR.Y);
                      Pen.Color:=BColor;
                      MoveTo(aTR.X+1, aTR.Y+1);
                      LineTo(self.Width-2, PenPos.Y);
                    end;
                  MoveTo(self.Width-1, aTR.Y);
                  LineTo(PenPos.X, aBR.Y);
                  Pen.Color:=AColor;
                  MoveTo(self.Width-2, aTR.Y);
                  LineTo(PenPos.X, aBR.Y);
                  Pen.Color:=BColor;
                  if aBR.Y<(self.Height-1) then
                    begin
                      LineTo(aBR.X, aBR.Y);
                      LineTo(aBR.X, self.Height-1);
                      Pen.Color:=AColor;
                      MoveTo(self.Width-2, aBR.Y-1);
                      LineTo(aBR.X-1, aBR.Y-1);
                      LineTo(PenPos.X, self.Height-1);
                      Pen.Color:=BColor;
                    end;
                  MoveTo(aBL.X, self.Height-1);
                  LineTo(aBR.X, PenPos.Y);
                  Pen.Color:=AColor;
                  MoveTo(aBR.X-1, self.Height-2);
                  LineTo(aBL.X, PenPos.Y);
                end;
              bsTopLine:
                begin
                  MoveTo(0, 0);
                  if x>0 then
                    begin
                      LineTo(x, 0);
                      LineTo(x, y-1);
                      LineTo(self.Width-1, PenPos.Y);
                      Pen.Color:=BColor;
                      MoveTo(0, 1);
                      LineTo(x-1, 1);
                      MoveTo(x+1, 0);
                      LineTo(x+1, y-2);
                      MoveTo(x, y);
                      LineTo(self.Width-1, y);
                    end else
                    begin
                      LineTo(self.Width-1, 0);
                      Pen.Color:=BColor;
                      MoveTo(0, 1);
                      LineTo(self.Width-1, 1);
                    end;
                end;
              bsBottomLine:
                begin
                  MoveTo(0, y-1);
                  if x>0 then
                    begin
                      LineTo(x, PenPos.Y);
                      LineTo(x, 0);
                      LineTo(self.Width-1, 0);
                      Pen.Color:=BColor;
                      MoveTo(0, y);
                      LineTo(x+1, y);
                      LineTo(x+1, 1);
                      LineTo(self.Width-1, 1);
                    end else
                    begin
                      LineTo(self.Width-1, PenPos.Y);
                      Pen.Color:=BColor;
                      MoveTo(0, y);
                      LineTo(self.Width-1, y);
                    end;
                end;
              bsLeftLine:
                begin
                  MoveTo(0, 0);
                  if y>0 then
                    begin
                      LineTo(0, y);
                      LineTo(x-1, y);
                      LineTo(PenPos.X, self.Height-1);
                      Pen.Color:=BColor;
                      MoveTo(1, 0);
                      LineTo(1, y-1);
                      MoveTo(0, y+1);
                      LineTo(x-2, PenPos.Y);
                      MoveTo(x, y);
                      LineTo(x, self.Height-1);
                    end else
                    begin
                      LineTo(0, self.Height-1);
                      Pen.Color:=BColor;
                      MoveTo(1, 0);
                      LineTo(1, self.Height-1);
                    end;
                end;
              bsRightLine:
                begin
                  MoveTo(x-1, 0);
                  if y>0 then
                    begin
                      LineTo(PenPos.X, y);
                      LineTo(0, y);
                      LineTo(0, self.Height-1);
                      Pen.Color:=BColor;
                      MoveTo(x, 0);
                      LineTo(x, y+1);
                      LineTo(1, PenPos.Y);
                      LineTo(1, self.Height-1);
                    end else
                    begin
                      LineTo(x-1, self.Height-1);
                      Pen.Color:=BColor;
                      MoveTo(x, 0);
                      LineTo(x, self.Height-1);
                    end;
                end;
            end else
            begin
              x:=self.Width;
              y:=self.Height;
              case PointA of
                eppTopLeft:
                  begin
                    MoveTo(0, y-1);
                    LineTo(0, 0);
                    LineTo(x-1, 0);
                    Pen.Color:=BColor;
                    MoveTo(1, y-1);
                    LineTo(1, 1);
                    LineTo(x-1, 1);
                  end;
                eppTopRight:
                  begin
                    MoveTo(0, 0);
                    LineTo(x-1, 0);
                    MoveTo(x-2, 1);
                    LineTo(x-2, y-1);
                    Pen.Color:=BColor;
                    MoveTo(0, 1);
                    LineTo(x-2, 1);
                    MoveTo(x-1, 1);
                    LineTo(x-1, y-1);
                  end;
                eppBottomLeft:
                  begin
                    MoveTo(0, 0);
                    LineTo(0, y-1);
                    MoveTo(1, y-2);
                    LineTo(x-1, y-2);
                    Pen.Color:=BColor;
                    MoveTo(1, 0);
                    LineTo(1, y-2);
                    MoveTo(1, y-1);
                    LineTo(x-1, y-1);
                  end;
                eppBottomRight:
                  begin
                    MoveTo(0, y-2);
                    LineTo(x-2, y-2);
                    LineTo(x-2, 0);
                    Pen.Color:=BColor;
                    MoveTo(0, y-1);
                    LineTo(x-1, y-1);
                    LineTo(x-1, 0);
                  end;
              end;
            end;
        end;
    end else
    begin
      Canvas.DrawThemedPanelBkgnd(ClientRect);
    end;
  inherited Paint;
end;

{ Getters & Setters }

function TECBevel.GetPointAX: Integer;
begin
  Result:=FPointAX;
  if assigned(FOnGetPointAX) then
    begin
      FOnGetPointAX(self, Result);
      dec(Result, Left);
    end;
end;

function TECBevel.GetPointAY: Integer;
begin
  Result:=FPointAY;
  if assigned(FOnGetPointAY) then
    begin
      FOnGetPointAY(self, Result);
      dec(Result, Top);
    end;
end;

function TECBevel.GetPointBX: Integer;
begin
  Result:=FPointBX;
  if assigned(FOnGetPointBX) then
    begin
      FOnGetPointBX(self, Result);
      dec(Result, Left);
    end;
end;

function TECBevel.GetPointBY: Integer;
begin
  Result:=FPointBY;
  if assigned(FOnGetPointBY) then
    begin
      FOnGetPointBY(self, Result);
      dec(Result, Top);
    end;
end;

procedure TECBevel.SetPointA(AValue: TPointPos);
begin
  if FPointA=AValue then exit;
  FPointA:=AValue;
  Invalidate;
end;

procedure TECBevel.SetPointAX(AValue: Integer);
begin
  if FPointAX=AValue then exit;
  FPointAX:=AValue;
  if (PointA<>eppNone) or (Shape in [bsTopLine, bsBottomLine]) then Invalidate;
end;

procedure TECBevel.SetPointAY(AValue: Integer);
begin
  if FPointAY=AValue then exit;
  FPointAY:=AValue;
  if (PointA<>eppNone) or (Shape in [bsLeftLine, bsRightLine]) then Invalidate;
end;

procedure TECBevel.SetPointB(AValue: TPointPos);
begin
  if FPointB=AValue then exit;
  FPointB:=AValue;
  Invalidate;
end;

procedure TECBevel.SetPointBX(AValue: Integer);
begin
  if FPointBX=AValue then exit;
  FPointBX:=AValue;
  if PointB<>eppNone then Invalidate;
end;

procedure TECBevel.SetPointBY(AValue: Integer);
begin
  if FPointBY=AValue then exit;
  FPointBY:=AValue;
  if PointB<>eppNone then Invalidate;
end;

procedure TECBevel.SetShape(AValue: TBevelShape);
begin
  if FShape=AValue then exit;
  FShape:=AValue;
  Invalidate;
end;

procedure TECBevel.SetStyle(AValue: TBevelStyle);
begin
  if FStyle=AValue then exit;
  FStyle:=AValue;
  Invalidate;
end;

procedure TECBevel.SetThemed(AValue: Boolean);
begin
  if FThemed=AValue then exit;
  FThemed:=AValue;
  Invalidate;
end;

end.


