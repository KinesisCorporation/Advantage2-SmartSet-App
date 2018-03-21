{-----------------------------------------------------------------------------
  TuEMultiTurn v0.2.2  2015-05-23
  Author: Miguel A. Risco Castillo
  http://ue.accesus.com/uecontrols

  THE COPYRIGHT NOTICES IN THE SOURCE CODE MAY NOT BE REMOVED OR MODIFIED.
  IF YOU MODIFY AND/OR DISTRIBUTE THE CODE TO ANY THIRD PARTY THEN YOU MUST NOT
  VEIL THE ORIGINAL AUTHOR. IT MUST ALWAYS BE CLEARLY IDENTIFIABLE.

  The contents of this file are subject in priority to the License in this header,
  in the license.txt file and the Mozilla Public License Version 1.1 (MPL);
  you may not use this file except in compliance with these licenses. You may obtain
  a copy of the MPL License at http://www.mozilla.org/MPL/MPL-1.1.html

  Software distributed under the Licenses is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the Licenses for
  the specific language governing rights and limitations under the Licenses.
-----------------------------------------------------------------------------}

unit uEMultiTurn;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  LCLIntf, LCLType, LCLProc, Types,
  BGRABitmap, BGRABitmapTypes, uEBase, uEKnob;

type

  TuEAngle = Integer;   //3600 = 360.0 deg


  { TCustomuEMultiTurn }

  TCustomuEMultiTurn = class(TCustomuEKnob)
  private
  protected
    class function GetControlClassDefaultSize: TSize; override;
    procedure SetupDefaults;override;
    procedure DefaultPicture(r: integer); override;
    procedure ForcePosition(const AValue: Real); override;
    function AngleToPos(AnAngle: integer): Real; override;
    function PosToAngle(Pos: Real): integer; override;
    function Delta(X, Y: Integer): TuEAngle;
    procedure MouseWheelPos(Shift: TShiftState; WheelDelta: Integer); override;
    procedure Mouse2Pos(const Y: Integer;const X: Integer); override;
  public
//
  end;


  { TuEMultiTurn }

  TuEMultiTurn = class(TCustomuEMultiTurn)
  private
  published
    property Debug;
//  This property is deprecated, use Image and LoadfromFile
    property Picture;
    property Image;
    property BackImage;
    property Position;
    property Transparent;
    property DefKnobRadius;
    property OnChange;
    property OnPaint;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnBackgroundChanged;
    property Align;
    property Anchors;
    property BorderSpacing;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnChangeBounds;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnClick;
    property OnConstrainedResize;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

{ TCustomuEMultiTurn }

procedure TCustomuEMultiTurn.ForcePosition(const AValue: Real);
begin
  FPosition:=AValue;
  inherited Angle:=PostoAngle(FPosition)/10;
  DoOnChange;
end;

class function TCustomuEMultiTurn.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 50;
  Result.CY := 50;
end;

procedure TCustomuEMultiTurn.DefaultPicture(r:integer);
var
  i,Margin,Size:integer;
  c,langle,sn,cn,x1,y1,x2,y2:single;
begin
  if r<=0 then exit;
  c:=(r-1)/2;
  Bitmap.SetSize(r,r);
  Bitmap.Fill(BGRAPixelTransparent);
  Bitmap.FillEllipseAntialias(c,c,c,c,BGRABlack);
  Bitmap.GradientFill(0,0,r-1,r-1,
                      BGRA(128,128,128,255),BGRA(0,0,0,0),
                      gtRadial,PointF(c,c),PointF(0,c),
                      dmDrawWithTransparency);
  Margin:=r div 3;
  Size:=r div 10;
  For i:=0 to 11 do
  begin
    langle:=i*PI/6;
    sn:=sin(langle);
    cn:=cos(langle);
    x1:=c+Margin*cn;
    y1:=c+Margin*sn;
    x2:=c+(Margin+Size)*cn;
    y2:=c+(Margin+Size)*sn;
    Bitmap.DrawLineAntialias(x1,y1,x2,y2,BGRAWhite, 2);
  end;
  AssignBGRAtoImage(Bitmap,Image);
end;

// Convert angle AnAngle to a position.
function TCustomuEMultiTurn.AngleToPos(AnAngle: integer): Real;
begin
  Result :=  1000*AnAngle/3600;
end;

// Convert position Pos to an angle.
function TCustomuEMultiTurn.PosToAngle(Pos: Real): integer;
begin
  Result := Round(Pos*3600/1000);
end;

function TCustomuEMultiTurn.Delta(X,Y:Integer):TuEAngle;
Var C,M,D:TuEAngle;
begin
  if Angle>0 then C:=Round(Angle*10) mod 3600 else C:=3600+(Round(Angle*10) mod 3600);
  M:=PointToAngle(Point(X, Y), GetCenter);
  D:=C-M;
  Result:=0;
  if (D>-1800) or (D<1800) then Result:=-D;
  if (D>1800) then Result:=(3600-D);
  if (D<-1800) then Result:=-(3600+D);
end;

procedure TCustomuEMultiTurn.MouseWheelPos(Shift: TShiftState; WheelDelta: Integer);
begin
  SetPosition(FPosition+WheelDelta/200);
end;

procedure TCustomuEMultiTurn.Mouse2Pos(const Y: Integer; const X: Integer);
begin
  SetPosition(FPosition+AngletoPos(Delta(X,Y)));
end;

procedure TCustomuEMultiTurn.SetupDefaults;
begin
  inherited SetupDefaults;
  FShowValues:=false;
  FLTicks:=0;
  FSTicks:=0;
  FDefKnobRadius:=33;
end;

end.
