{-----------------------------------------------------------------------------
  TuEKnob v1.0 2015-05-20
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

unit uEKnob;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  LCLIntf, LCLType, LCLProc, Types,
  BGRABitmap, BGRABitmapTypes, uEBase, uERotImage;

type

  TuEAngle = 0..3600; // 0.0 - 360.0 deg


  { TCustomuEKnob }

  TCustomuEKnob = class(TCustomuERotImage)
  protected
    FMax: Real;
    FMaxAngle: TuEAngle;
    FMin: Real;
    FMinAngle: TuEAngle;
    FOffsetAngle:integer;
    FLTicks: integer;
    FLTicksColor: TColor;
    FLTicksSize: integer;
    FSTicks: integer;
    FSTicksColor: TColor;
    FSTicksSize: integer;
    FLTicksWidth: integer;
    FTicksMargin: integer;
    FShowValues: Boolean;
    FValuesFont: TFont;
    FValuesMargin: integer;
    FOnChange: TNotifyEvent;
    FPosition: Real;
    FTransparent:Boolean;
    FDefKnobRadius:integer;
    FBackImage: TBitmap;
    FOnBackgroundChanged: TNotifyEvent;
    procedure SetLTicksColor(const AValue: TColor); virtual;
    procedure SetMax(const AValue: Real); virtual;
    procedure SetMaxAngle(const AValue: TuEAngle); virtual;
    procedure SetMin(const AValue: Real);  virtual;
    procedure SetMinAngle(const AValue: TuEAngle); virtual;
    procedure SetPosition(const AValue: Real); virtual;
    procedure SetLargeTicks(const AValue: integer); virtual;
    procedure SetLTicksSize(const AValue: integer); virtual;
    procedure SetSTicks(const AValue: integer); virtual;
    procedure SetSTicksColor(const AValue: TColor); virtual;
    procedure SetSTicksSize(const AValue: integer); virtual;
    procedure SetTicksMargin(const AValue: integer); virtual;
    procedure SetShowValues(const AValue: Boolean); virtual;
    procedure SetTransparent(const AValue: Boolean); virtual;
    procedure SetupDefaults;virtual;
    procedure SetValueMargin(const AValue: integer); virtual;
    procedure SetValuesFont(const AValue: TFont); virtual;
    procedure SetDefKnobRadius(AValue: integer); virtual;
    procedure SetOffsetAngle(AValue: integer); virtual;
    procedure SetBackground(const AValue: TBitmap);virtual;
    procedure RenderBackground;virtual;
    procedure SetLTicksWidth(AValue: integer);virtual;
    procedure ImageChanged(Sender : TObject); override;
    procedure BackgroundChanged(Sender : TObject); virtual;
    class procedure WSRegisterClass; override;
    procedure CalculatePreferredSize(var PreferredWidth,
                                     PreferredHeight: integer;
                                     WithThemeSpace: Boolean); override;
    class function GetControlClassDefaultSize: TSize; override;
    procedure Drawcontrol; override;
    procedure RenderControl; override;
    procedure SetColor(AValue: TColor); override;
    procedure FontChanged(Sender: TObject); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure MouseWheelPos(Shift: TShiftState; WheelDelta: Integer); virtual;
    procedure Mouse2Pos(const Y: Integer;const X: Integer); virtual;
    procedure ForcePosition(const AValue: Real); virtual;
    procedure DrawScales(LBitmap:TBGRABitmap); virtual;
    procedure DoOnChange; virtual;
    procedure DoOnResize; override;
    procedure DefaultPicture(r:integer); virtual;
    function GetCenter: TPoint;virtual;
    function PointToAngle(APoint, ACenter: TPoint): integer;virtual;
    function AngleToPos(AnAngle: integer): Real; virtual;
    function PosToAngle(Pos: Real): integer; virtual;
    property Position: Real read FPosition write SetPosition;
    property Max: Real read FMax write SetMax;
    property MaxAngle: TuEAngle read FMaxAngle write SetMaxAngle default 3300;
    property Min: Real read FMin write SetMin;
    property MinAngle: TuEAngle read FMinAngle write SetMinAngle default 300;
    property OffsetAngle: integer read FOffsetAngle write SetOffsetAngle default 0;
    property TicksMargin:integer read FTicksMargin write SetTicksMargin default 20;
    property LTicks:integer read FLTicks write SetLargeTicks default 10;
    property LTicksSize:integer read FLTicksSize write SetLTicksSize default 8;
    property LTicksColor:TColor read FLTicksColor write SetLTicksColor default clblack;
    property LTicksWidth:integer read FLTicksWidth write SetLTicksWidth default 1;
    property STicks:integer read FSTicks write SetSTicks default 3;
    property STicksSize:integer read FSTicksSize write SetSTicksSize default 5;
    property STicksColor:TColor read FSTicksColor write SetSTicksColor default clBlack;
    property ShowValues:Boolean read FShowValues write SetShowValues default true;
    property ValuesMargin:integer read FValuesMargin write SetValueMargin default 8;
    property ValuesFont:TFont read FValuesFont write SetValuesFont;
    property Transparent:Boolean read FTransparent write SetTransparent default true;
    property DefKnobRadius:integer read FDefKnobRadius write SetDefKnobRadius default 34;
    property BackImage: TBitmap read FBackImage write SetBackground;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnBackgroundChanged: TNotifyEvent read FOnBackgroundChanged write FOnBackgroundChanged;
  public
    Background:TBGRABitmap;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function LoadBackFromFile(f:string):boolean; virtual;
  end;


  { TuEKnob }

  TuEKnob = class(TCustomuEKnob)
  published
    property Debug;
//  This property is deprecated, use Image and LoadfromFile
    property Picture;
    property Max;
    property MaxAngle;
    property Min;
    property MinAngle;
    property OffsetAngle;
    property Image;
    property BackImage;
    property Position;
    property TicksMargin;
    property LTicks;
    property LTicksSize;
    property LTicksColor;
    property LTicksWidth;
    property STicks;
    property STicksSize;
    property STicksColor;
    property ShowValues;
    property ValuesMargin;
    property ValuesFont;
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
    property About;// This property must not be removed to follow the licence statements
  end;

implementation

{ TCustomuEKnob }

constructor TCustomuEKnob.Create(AOwner: TComponent);
begin
  FValuesFont:=TFont.Create;
  inherited Create(AOwner);
  inherited Transparent:=true;
  SetupDefaults;
  ControlStyle := ControlStyle + [csReplicatable, csCaptureMouse, csClickEvents, csDoubleClicks];
  DefaultPicture(FDefKnobRadius);
  FBackImage := TBitmap.Create;
  FBackImage.SetSize(0,0);
  FBackImage.OnChange := @BackgroundChanged;
  with GetControlClassDefaultSize do
  begin
    SetInitialBounds(0, 0, CX, CY);
    BackGround:=TBGRABitmap.Create(CX,CY);
  end;
end;

destructor TCustomuEKnob.Destroy;
begin
  FreeThenNil(Background);
  FValuesFont.OnChange:=nil;
  FreeThenNil(FValuesFont);
  FBackImage.OnChange := nil;
  FreeThenNil(FBackImage);
  inherited Destroy;
end;

function TCustomuEKnob.LoadBackFromFile(f: string): boolean;
begin
  result:=false;
  try
    BackGround.LoadFromFile(f);
  except
    exit;
  end;
  FBackImage.Assign(BackGround);
  result:=true;
end;

procedure TCustomuEKnob.SetMax(const AValue: Real);
begin
  if (FMax=AValue) and (AValue<=FMin) then exit;
  FMax:=AValue;
  RenderBackground;
  RenderControl;
  Invalidate;
end;

procedure TCustomuEKnob.SetMaxAngle(const AValue: TuEAngle);
begin
  if FMaxAngle=AValue then exit;
  FMaxAngle:=AValue;
  RenderBackground;
  RenderControl;
  Invalidate;
end;

procedure TCustomuEKnob.SetMin(const AValue: Real);
begin
  if (FMin=AValue) and (AValue>=FMax) then exit;
  FMin:=AValue;
  RenderBackground;
  RenderControl;
  Invalidate;
end;

procedure TCustomuEKnob.SetMinAngle(const AValue: TuEAngle);
begin
  if FMinAngle=AValue then exit;
  FMinAngle:=AValue;
  RenderBackground;
  RenderControl;
  Invalidate;
end;

procedure TCustomuEKnob.SetPosition(const AValue: Real);
begin
  if FPosition=AValue then exit;
  FPosition:=AValue;
  RenderControl;
  Invalidate;
end;

procedure TCustomuEKnob.SetShowValues(const AValue: Boolean);
begin
  if FShowValues=AValue then exit;
  FShowValues:=AValue;
  RenderBackground;
  Invalidate;
end;

procedure TCustomuEKnob.SetTransparent(const AValue: Boolean);
begin
  if FTransparent=AValue then exit;
  FTransparent:=AValue;
  RenderBackground;
  RenderControl;
  Invalidate;
end;

procedure TCustomuEKnob.SetupDefaults;
begin
  FDefKnobRadius:=34;
  FPosition:=0;
  FMax:=100;
  FMin:=0;
  FMaxAngle:=3300;
  FMinAngle:=300;
  FOffsetAngle:=0;
  FTicksMargin:=20;
  FLTicks:=10;
  FLTicksSize:=8;
  FLTicksColor:=clBlack;
  FLTicksWidth:=1;
  FSTicks:=3;
  FSTicksSize:=5;
  FSTicksColor:=clBlack;
  FShowValues:=true;
  FValuesMargin:=8;
  FValuesFont.Name:='Sans';
  FValuesFont.Orientation:=0;
  FValuesFont.Style:=[];
  FValuesFont.Color:=clBlack;
  FValuesFont.Size:=8;
  FValuesFont.OnChange:=@FontChanged;
  FTransparent:=true;
  UniqueSize:=true;
  Center:=true;
end;

procedure TCustomuEKnob.SetSTicks(const AValue: integer);
begin
  if FSTicks=AValue then exit;
  FSTicks:=AValue;
  RenderBackground;
  Invalidate;
end;

procedure TCustomuEKnob.SetSTicksColor(const AValue: TColor);
begin
  if FSTicksColor=AValue then exit;
  FSTicksColor:=AValue;
  RenderBackground;
  Invalidate;
end;

procedure TCustomuEKnob.SetSTicksSize(const AValue: integer);
begin
  if FSTicksSize=AValue then exit;
  FSTicksSize:=AValue;
  RenderBackground;
  Invalidate;
end;

procedure TCustomuEKnob.SetTicksMargin(const AValue: integer);
begin
  if FTicksMargin=AValue then exit;
  FTicksMargin:=AValue;
  RenderBackground;
  Invalidate;
end;

procedure TCustomuEKnob.SetValueMargin(const AValue: integer);
begin
  if FValuesMargin=AValue then exit;
  FValuesMargin:=AValue;
  RenderBackground;
  Invalidate;
end;

procedure TCustomuEKnob.SetValuesFont(const AValue: TFont);
begin
  if FValuesFont.IsEqual(AValue) then exit;
  FValuesFont.Assign(AValue);
end;

class procedure TCustomuEKnob.WSRegisterClass;
begin
  inherited WSRegisterClass;
end;

procedure TCustomuEKnob.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  inherited CalculatePreferredSize(PreferredWidth, PreferredHeight,
    WithThemeSpace);
end;

class function TCustomuEKnob.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 90;
  Result.CY := 90;
end;

procedure TCustomuEKnob.Drawcontrol;
begin
  Background.Draw(inherited Canvas,0,0,false);
  inherited Drawcontrol;
end;

procedure TCustomuEKnob.DefaultPicture(r: integer);
var
  c:single;
begin
  if r<=0 then exit;
  c:=(r-1)/2;
  Bitmap.SetSize(r,r);
  Bitmap.Fill(BGRAPixelTransparent);
  Bitmap.FillEllipseAntialias(c,c,c,c,BGRABlack);
  Bitmap.GradientFill(0,0,r,r,
                      BGRA(128,128,128,255),BGRA(0,0,0,0),
                      gtRadial,PointF(c,c),PointF(0,c),
                      dmDrawWithTransparency);
  Bitmap.DrawLineAntialias(c,c+5,c,r-5,BGRAWhite,2);
  AssignBGRAtoImage(Bitmap,Image);
end;

procedure TCustomuEKnob.SetColor(AValue: TColor);
begin
  if Color=AValue then exit;
  RenderControl;
  inherited SetColor(AValue);
end;

procedure TCustomuEKnob.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);
  RenderBackground;
  Invalidate;
end;

function TCustomuEKnob.AngleToPos(AnAngle: integer): Real;
// Convert angle AnAngle to a position.
begin
  Result := FMin + ((FMax - FMin) * (AnAngle - FMinAngle)/(FMaxAngle - FMinAngle));
end;

// Convert position Pos to an angle.
function TCustomuEKnob.PosToAngle(Pos: Real): integer;
begin
  Result := FMinAngle + Round((FMaxAngle - FMinAngle) * (Pos - FMin) / (FMax - FMin));
end;

procedure TCustomuEKnob.DoOnResize;
begin
  BackGround.SetSize(width,height);
  RenderBackground;
  RenderControl;
  inherited DoOnResize;
end;

function TCustomuEKnob.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result:=inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  MouseWheelPos(Shift,WheelDelta);
end;

procedure TCustomuEKnob.MouseWheelPos(Shift: TShiftState; WheelDelta: Integer);
begin
  SetPosition(FPosition+(FMax-FMin)*WheelDelta/20000);
end;

// Convert a APoint to an angle (relative to ACenter),
// where bottom is 0, left is 900, top is 1800 and so on.
function TCustomuEKnob.PointToAngle(APoint, ACenter: TPoint): integer;
var
  N: Integer;
begin
  N := APoint.X - ACenter.X;
  if N = 0 then
    if APoint.Y < ACenter.Y then Result := 900 else Result := 2700
  else
  begin
    Result:=Round(ArcTan((ACenter.Y - APoint.Y) / N) * 1800 / PI);
  end;
  if N < 0 then Result := Result + 1800;
  Result := 2700 - Result;
end;

procedure TCustomuEKnob.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  MouseCapture := True;
  Mouse2Pos(Y, X);
end;

procedure TCustomuEKnob.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  If MouseCapture then Mouse2Pos(Y, X);
end;

procedure TCustomuEKnob.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  MouseCapture := False;
end;

procedure TCustomuEKnob.DoOnChange;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TCustomuEKnob.GetCenter: TPoint;
begin
  with Result do
  begin
    X := Width div 2;
    Y := Height div 2;
  end;
end;

procedure TCustomuEKnob.SetDefKnobRadius(AValue: integer);
begin
  if FDefKnobRadius=AValue then Exit;
  FDefKnobRadius:=AValue;
  DefaultPicture(AValue);
end;

procedure TCustomuEKnob.SetOffsetAngle(AValue: integer);
begin
  if FOffsetAngle=AValue then Exit;
  FOffsetAngle:=AValue;
  RenderBackground;
  RenderControl;
  Invalidate;
end;

procedure TCustomuEKnob.SetBackground(const AValue: TBitmap);
begin
  if FBackImage=AValue then exit;
  FBackImage.Assign(AValue);
end;

procedure TCustomuEKnob.BackgroundChanged(Sender: TObject);
begin
  RenderBackground;
  RenderControl;
  if Assigned(OnImageChanged) then OnImageChanged(Self);
  invalidate;
end;

procedure TCustomuEKnob.SetLTicksWidth(AValue: integer);
begin
  if FLTicksWidth=AValue then Exit;
  FLTicksWidth:=AValue;
  RenderBackground;
  invalidate;
end;

procedure TCustomuEKnob.SetLTicksColor(const AValue: TColor);
begin
  if FLTicksColor=AValue then exit;
  FLTicksColor:=AValue;
  RenderBackground;
  invalidate;
end;

procedure TCustomuEKnob.Mouse2Pos(const Y: Integer; const X: Integer);
var
  TmpAngle: Integer;
begin
  TmpAngle:=PointToAngle(Point(X, Y), GetCenter)-OffsetAngle;
  if TmpAngle>3600 then TmpAngle:=TmpAngle-3600;
  if TmpAngle<0 then TmpAngle:=TmpAngle+3600;
  SetPosition(AngletoPos(TmpAngle));
end;

procedure TCustomuEKnob.SetLargeTicks(const AValue: integer);
begin
  if FLTicks=AValue then exit;
  FLTicks:=AValue;
  RenderBackground;
  invalidate;
end;

procedure TCustomuEKnob.SetLTicksSize(const AValue: integer);
begin
  if FLTicksSize=AValue then exit;
  FLTicksSize:=AValue;
  RenderBackground;
  invalidate;
end;

procedure TCustomuEKnob.DrawScales(LBitmap:TBGRABitmap);
var i,j:integer;
    x1,y1,x2,y2,lpos:single;
    xc,yc,langle:single;
    sn,cn:single;
    lc,sc,vc:TBGRAPixel;
    ts:TSize;
    la:string;
    ss:boolean;
begin
  xc:=LBitmap.Width/2;//  if (Image.Width mod 2) = 0 then xc:=LBitmap.Width/2-1 else xc:=LBitmap.Width/2;
  yc:=LBitmap.Height/2;//  if (Image.Height mod 2) = 0 then yc:=LBitmap.Height/2-1 else yc:=LBitmap.Height/2;
  lc:=ColorToBGRA(ColorToRGB(FLTicksColor));
  sc:=ColorToBGRA(ColorToRGB(FSTicksColor));
  vc:=ColorToBGRA(ColorToRGB(FValuesFont.Color));
  AssignFontToBGRA(FValuesFont,LBitmap);
  ss:=((FMaxAngle-FMinAngle) mod 3600)=0;
  if FLTicks>0 then For i:=0 to FLTicks do
  begin
    lpos:=(i/FLTicks)*(FMax-FMin)+FMin;
    langle:=(PosToAngle(lpos)+FOffsetAngle)*PI/1800 +PI/2;
    sn:=sin(langle);
    cn:=cos(langle);
    x1:=xc+FTicksMargin*cn;
    y1:=yc+FTicksMargin*sn;
    x2:=xc+(FTicksMargin+FLTicksSize)*cn;
    y2:=yc+(FTicksMargin+FLTicksSize)*sn;
    LBitmap.DrawLineAntialias(x1,y1,x2,y2,lc, FLTicksWidth);
    if FShowValues and not(ss and (i=0)) then
    begin
      x2:=xc+(FTicksMargin+FLTicksSize+FValuesMargin)*cn;
      y2:=yc+(FTicksMargin+FLTicksSize+FValuesMargin)*sn;
      la:=floattostrF(lpos,ffGeneral,4,2);
      ts:=LBitmap.TextSize(la);
      LBitmap.TextOut(trunc(x2+1), trunc(y2-ts.cy/2+1), la, vc, taCenter);
    end;
    if (lpos<Fmax) then For j:=1 to FSTicks do
    begin
      lpos:=(i/FLTicks)*(FMax-FMin)+FMin+j*((FMax-FMin)/FLTicks)/(FSTicks+1);
      langle:=(PosToAngle(lpos)+FOffsetAngle)*PI/1800 +PI/2;
      sn:=sin(langle);
      cn:=cos(langle);
      x1:=xc+FTicksMargin*cn;
      y1:=yc+FTicksMargin*sn;
      x2:=xc+(FTicksMargin+FSTicksSize)*cn;
      y2:=yc+(FTicksMargin+FSTicksSize)*sn;
      LBitmap.DrawLineAntialias(x1,y1,x2,y2,sc, 1);
    end;
  end;
end;

procedure TCustomuEKnob.ImageChanged(Sender : TObject);
begin
  if Image.Empty then DefaultPicture(FDefKnobRadius);
  inherited ImageChanged(Sender);
end;

procedure TCustomuEKnob.RenderControl;
begin
  if ([csLoading,csDestroying]*ComponentState<>[]) or
     (csCreating in FControlState) or
      IsUpdating or (width=0) then Exit;
  ForcePosition(FPosition);
  inherited RenderControl;
end;

procedure TCustomuEKnob.ForcePosition(const AValue: Real);
begin
  if AValue<FMin then FPosition:=FMin
  else if AValue>FMax then FPosition:=FMax
  else FPosition:=AValue;
  inherited Angle:=(PostoAngle(FPosition)+FOffsetAngle)/10;
  DoOnChange;
end;

procedure TCustomuEKnob.RenderBackground;
var fillc:TBGRAPixel;
    tbmp:TBGRABitmap;
begin
  If FTransparent then Fillc:=BGRAPixelTransparent else Fillc:=ColortoBGRA(ColortoRGB(color));
  if not FBackImage.Empty then
  begin
    Background.Fill(Fillc);
    tbmp:=TBGRABitmap.Create(FBackImage);
    Background.PutImage(0,0,tbmp,dmDrawWithTransparency);
    tbmp.Free;
  end else
  begin
    Background.SetSize(Width,Height);
    Background.Fill(Fillc);
  end;
  DrawScales(BackGround);
end;

end.
