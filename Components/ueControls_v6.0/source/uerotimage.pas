{------------------------------------------------------------------------------
  TuERotImage v1.0  2015-05-17
  Author: Miguel A. Risco-Castillo
  http://ue.accesus.com/uecontrols

  using some ideas from:
  TRotateImage v1.54 by Kambiz R. Khojasteh

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
------------------------------------------------------------------------------}

unit uERotImage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  Types, BGRABitmap, BGRABitmapTypes,uEBase;

type

  { TCustomuERotImage }

  TCustomuERotImage = class(TuEBaseControl)
  private
    FColor: TColor;
    FImage: TBitmap;
    FOnImageChanged: TNotifyEvent;
    FPicture: TPicture;
    FStretch: Boolean;
    FCenter: Boolean;
    FTransparent: Boolean;
    FProportional: Boolean;
    FAngle: Extended;
    FUniqueSize: Boolean;
    FMaxSize: Integer;
    FOnRotation: TNotifyEvent;
    FBeforeRotation: TNotifyEvent;
    function GetCanvas: TCanvas;
    procedure RenderRotation;
    procedure SetCenter(const AValue: Boolean);
    procedure SetImage(const AValue: TBitmap);
    procedure SetStretch(const AValue: Boolean);
    procedure SetProportional(const AValue: Boolean);
    procedure SetTransparent(const AValue: Boolean);
    procedure SetAngle(const Value: Extended);
    procedure SetUniqueSize(Value: Boolean);
  protected
    class procedure WSRegisterClass; override;
    procedure ImageChanged(Sender : TObject); virtual;
    procedure DeprecatedPictureChanged(Sender:TObject);
    procedure CalculatePreferredSize(var PreferredWidth,
                                     PreferredHeight: integer;
                                     WithThemeSpace: Boolean); override;
    class function GetControlClassDefaultSize: TSize; override;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    function DestRect: TRect; override;
    procedure RenderControl; override;
    procedure SetColor(AValue: TColor); override;
    procedure DoRotation; virtual;
    procedure DoBeforeRotation; virtual;
    procedure SetPicture(AValue: TPicture);
    property Canvas: TCanvas read GetCanvas;
    property MaxSize: Integer read FMaxSize;
    property Angle: Extended read FAngle write SetAngle;
    property BorderSpacing;
    property Center: Boolean read FCenter write SetCenter default False;
    property Color: tcolor read FColor write SetColor default clDefault;
    property Image: TBitmap read FImage write SetImage;
    property Proportional: Boolean read FProportional write setProportional default False;
    property Stretch: Boolean read FStretch write SetStretch default False;
    property Transparent: Boolean read FTransparent write SetTransparent default true;
    property UniqueSize: Boolean read FUniqueSize write SetUniqueSize default False;
    property OnImageChanged: TNotifyEvent read FOnImageChanged write FOnImageChanged;
    property OnRotation: TNotifyEvent read FOnRotation write FOnRotation;
    property BeforeRotation: TNotifyEvent read FBeforeRotation write FBeforeRotation;
    property Picture:TPicture read FPicture write SetPicture;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function LoadFromFile(f:string):boolean; virtual;
    procedure ForceRotate(AValue: Extended);
  end;


  { TuECustomRotImage }

  TuERotImage = class(TCustomuERotImage)
  published
    property Debug;
//  This property is deprecated, use Image and LoadfromFile
    property Picture;
    property Align;
    property Anchors;
    property Angle;
    property AutoSize;
    property BorderSpacing;
    property Center;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property MaxSize;
    property ParentColor;
    property ParentShowHint;
    property Image;
    property PopupMenu;
    property Proportional;
    property ShowHint;
    property Stretch;
    property Transparent;
    property UniqueSize;
    property Visible;
    property OnChangeBounds;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPaint;
    property OnImageChanged;
    property OnClick;
    property OnConstrainedResize;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnRotation;
    property BeforeRotation;
    property OnStartDock;
    property OnStartDrag;
  end;


implementation

uses LCLProc;

const DefaultSize=90;

constructor TCustomuERotImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable, csCaptureMouse, csClickEvents, csDoubleClicks];
  AutoSize := False;
  FCenter := False;
  FProportional := False;
  FStretch := False;
  FTransparent := True;
  FUniqueSize := False;
  FImage := TBitmap.Create;
  FImage.Clear; //  FImage.SetSize(DefaultSize,DefaultSize);
  FImage.OnChange := @ImageChanged;
  with GetControlClassDefaultSize do SetInitialBounds(0, 0, CX, CY);
  //Deprecated:
  FPicture := TPicture.Create;
  FPicture.OnChange:=@DeprecatedPictureChanged;
end;

destructor TCustomuERotImage.Destroy;
begin
  FImage.OnChange := nil;
  FreeThenNil(FImage);
  //Deprecated:
  FPicture.OnChange:=nil;
  FreeThenNil(FPicture);
  inherited Destroy;
end;

function TCustomuERotImage.LoadFromFile(f: string): boolean;
begin
  result:=false;
  try
    Bitmap.LoadFromFile(f);
  except
    exit;
  end;
  FImage.Assign(Bitmap);
  result:=true;
end;

procedure TCustomuERotImage.RenderControl;
begin
  if (csLoading in ComponentState) or (csCreating in FControlState) or IsUpdating then Exit;
  RenderRotation;
  inherited RenderControl;
end;

procedure TCustomuERotImage.RenderRotation;
var xc,yc,w,h:real;
  rad,s,c:Extended;
  tbmp:TBGRABitmap;
  fillc:TBGRAPixel;
begin

//ShowMessage('RenderRotation '+Self.GetNamePath);

  If FTransparent then Fillc:=BGRAPixelTransparent else Fillc:=ColortoBGRA(ColortoRGB(color));
  if not FImage.Empty then
  begin
    if UniqueSize then
      tbmp:=TBGRABitmap.Create(MaxSize,MaxSize,Fillc)
    else
    begin
      rad:=FAngle*PI/180;
      s:=abs(sin(rad));
      c:=abs(cos(rad));
      w:= FImage.width*c + FImage.height*s;
      h:= FImage.width*s + FImage.height*c;
      tbmp:=TBGRABitmap.Create(Round(w),Round(h),Fillc);
    end;
    xc:=FImage.width/2;
    yc:=FImage.height/2;
    Bitmap.SetSize(1,1);
    Bitmap.Fill(Fillc);
    Bitmap.Assign(FImage);
  end else if width>0 then
  begin
    xc:=width/2;
    yc:=height/2;
    tbmp:=TBGRABitmap.Create(width,height,Fillc);
    Bitmap.SetSize(width,height);
    Bitmap.Fill(Fillc);
  end else exit;
  w:=(tbmp.Width)/2;
  h:=(tbmp.Height)/2;
  DoBeforeRotation;
  tbmp.PutImageAngle(w-1,h-1,Bitmap,FAngle,xc-0.5,yc-0.5,255);
  Bitmap.Assign(tbmp);
  tbmp.free;
  DoRotation;
end;


function TCustomuERotImage.GetCanvas: TCanvas;
begin
  Result := inherited Canvas;
end;

procedure TCustomuERotImage.SetCenter(const AValue: Boolean);
begin
  if FCenter = AValue then exit;
  FCenter := AValue;
  invalidate;
end;

procedure TCustomuERotImage.SetImage(const AValue: TBitmap);
begin
  if (FImage <> nil) and (FImage=AValue) then exit;
  FImage.Assign(AValue);
end;

procedure TCustomuERotImage.SetPicture(AValue: TPicture);
begin
  if FPicture=AValue then Exit;
  FPicture:=AValue;
end;

procedure TCustomuERotImage.SetStretch(const AValue: Boolean);
begin
  if FStretch = AValue then exit;
  FStretch := AValue;
  invalidate;
end;

procedure TCustomuERotImage.SetProportional(const AValue: Boolean);
begin
  if FProportional = AValue then exit;
  FProportional := AValue;
  invalidate;
end;

procedure TCustomuERotImage.SetTransparent(const AValue: Boolean);
begin
  if FTransparent = AValue then exit;
  FTransparent := AValue;
  RenderControl;
  invalidate;
end;

procedure TCustomuERotImage.SetColor(AValue: TColor);
begin
  if FColor = AValue then exit;
  FColor := AValue;
  RenderControl;
  inherited SetColor(AValue);
  invalidate;
end;

procedure TCustomuERotImage.ForceRotate(AValue: Extended);
begin
  FAngle:=AValue;
  RenderRotation;
  Invalidate;
end;

procedure TCustomuERotImage.SetAngle(const Value: Extended);
begin
  if Value <> FAngle then
  begin
    FAngle := Value;
    RenderControl;
    invalidate;
  end;
end;

procedure TCustomuERotImage.SetUniqueSize(Value: Boolean);
begin
  if Value <> UniqueSize then
  begin
    FUniqueSize := Value;
    RenderControl;
    invalidate;
  end;
end;

procedure TCustomuERotImage.ImageChanged(Sender: TObject);
begin

//ShowMessage('ImageChanged '+Self.GetNamePath);

  FMaxSize := Round(Sqrt(Sqr(FImage.Width) + Sqr(FImage.Height)));
  RenderControl;
  if Assigned(OnImageChanged) then OnImageChanged(Self);
  invalidate;
end;

procedure TCustomuERotImage.DeprecatedPictureChanged(Sender: TObject);
begin
  if assigned(FPicture.Bitmap) and (FPicture.Bitmap.Width>0) then
  begin
    Image.Assign(FPicture.Bitmap);
    ShowMessage('The property "Picture" is deprecated, use Image');
    FPicture.Clear;
  end;
end;

procedure TCustomuERotImage.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  PreferredWidth := Bitmap.Width;
  PreferredHeight := Bitmap.Height;
end;

class function TCustomuERotImage.GetControlClassDefaultSize: TSize;
begin
  Result.CX := DefaultSize;
  Result.CY := DefaultSize;
end;

function TCustomuERotImage.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := True;
  if not (csDesigning in ComponentState) or (MaxSize <> 0) then
  begin
    if Align in [alNone, alLeft, alRight] then
      if UniqueSize then
        NewWidth := MaxSize
      else
        NewWidth := Bitmap.Width;
    if Align in [alNone, alTop, alBottom] then
      if UniqueSize then
        NewHeight := MaxSize
      else
        NewHeight := Bitmap.Height;
  end;
end;

procedure TCustomuERotImage.DoRotation;
begin
  if Assigned(FOnRotation) then FOnRotation(Self);
end;

procedure TCustomuERotImage.DoBeforeRotation;
begin
  if Assigned(FBeforeRotation) then FBeforeRotation(Self);
end;

class procedure TCustomuERotImage.WSRegisterClass;
begin
  inherited WSRegisterClass;
end;

function TCustomuERotImage.DestRect: TRect;
var
  PicWidth: Integer;
  PicHeight: Integer;
  ImgWidth: Integer;
  ImgHeight: Integer;
  w: extended;
  h: extended;
begin
  if not Assigned(Bitmap) then exit;
  PicWidth := Bitmap.Width;
  PicHeight := Bitmap.Height;
  ImgWidth := ClientWidth;
  ImgHeight := ClientHeight;
  if Stretch or (Proportional and ((PicWidth > ImgWidth) or (PicHeight > ImgHeight))) then
  begin
    if Proportional and (PicWidth > 0) and (PicHeight > 0) then
    begin
      w:=ImgWidth;
      h:=(PicHeight*w)/PicWidth;
      if h>ImgHeight then
      begin
        h:=ImgHeight;
        w:=(PicWidth*h)/PicHeight;
      end;
      PicWidth:=round(w);
      PicHeight:=round(h);
    end
    else begin
      PicWidth := ImgWidth;
      PicHeight := ImgHeight;
    end;
  end;
  Result:=Rect(0,0,PicWidth,PicHeight);
  if Center then
  OffsetRect(Result,Round((ImgWidth-PicWidth+2)/2),Round((ImgHeight-PicHeight+2)/2));

//{$IFDEF WINDOWS}
//    OffsetRect(Result,Round((ImgWidth-PicWidth)/2),Round((ImgHeight-PicHeight)/2));
//{$ELSE}
//    OffsetRect(Result,-Round((ImgWidth-PicWidth)/2),-Round((ImgHeight-PicHeight)/2));
//{$ENDIF}
end;


end.

