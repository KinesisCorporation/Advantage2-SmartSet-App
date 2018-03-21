{------------------------------------------------------------------------------
  TuETileImage v1.0  2015-05-19
  Author: Miguel A. Risco-Castillo
  http://ue.accesus.com/uecontrols

  using some ideas from: TjanTiledPanel

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

unit uETileImage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  Types, BGRABitmap, BGRABitmapTypes, uEBase, LCLType, LCLIntf;

type

  { TCustomuETileImage }

  TCustomuETileImage = class(TuEBaseControl)
  private
    FColor: TColor;
    FImage: TBitmap;
    FOnImageChanged: TNotifyEvent;
    FTransparent: Boolean;
    FOnTile: TNotifyEvent;
    FBeforeTile: TNotifyEvent;
    function GetCanvas: TCanvas;
    procedure SetImage(const AValue: TBitmap);
    procedure SetTransparent(const AValue: Boolean);
  protected
    class procedure WSRegisterClass; override;
    procedure ImageChanged(Sender : TObject); virtual;
    procedure CalculatePreferredSize(var PreferredWidth,
                                     PreferredHeight: integer;
                                     WithThemeSpace: Boolean); override;
    class function GetControlClassDefaultSize: TSize; override;
    procedure DrawControl; override;
    procedure SetColor(AValue: TColor); override;
    procedure DoTile; virtual;
    procedure DoBeforeTile; virtual;
    property Canvas: TCanvas read GetCanvas;
    property BorderSpacing;
    property Color: tcolor read FColor write SetColor default clDefault;
    property Image: TBitmap read FImage write SetImage;
    property Transparent: Boolean read FTransparent write SetTransparent default true;
    property OnImageChanged: TNotifyEvent read FOnImageChanged write FOnImageChanged;
    property OnTile: TNotifyEvent read FOnTile write FOnTile;
    property BeforeTile: TNotifyEvent read FBeforeTile write FBeforeTile;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function LoadFromFile(f:string):boolean; virtual;
  end;


  { TuECustomRotImage }

  TuETileImage = class(TCustomuETileImage)
  published
    property Debug;
    property Align;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentColor;
    property ParentShowHint;
    property Image;
    property PopupMenu;
    property ShowHint;
    property Transparent;
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
    property OnTile;
    property BeforeTile;
    property OnStartDock;
    property OnStartDrag;
  end;


implementation

uses LCLProc;

const DefaultSize=90;

constructor TCustomuETileImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable, csCaptureMouse, csClickEvents, csDoubleClicks];
  AutoSize := False;
  FTransparent := True;
  FImage := TBitmap.Create;
  FImage.Clear;
  FImage.OnChange := @ImageChanged;
  with GetControlClassDefaultSize do SetInitialBounds(0, 0, CX, CY);
end;

destructor TCustomuETileImage.Destroy;
begin
  FImage.OnChange := nil;
  FreeThenNil(FImage);
  inherited Destroy;
end;

function TCustomuETileImage.LoadFromFile(f: string): boolean;
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

procedure TCustomuETileImage.DrawControl;
var
  ix, iy: Integer;
  BmpWidth, BmpHeight: Integer;
  BmpCanvas: THandle;
  bm:Tbitmap;
begin
    DoBeforeTile;
    bm:=FImage;
    if assigned(bm) and (bm.Height <> 0) and
        (bm.Width <> 0) then
      begin
        BmpWidth := bm.Width;
        BmpHeight := bm.Height;
        BmpCanvas := bm.Canvas.Handle;
        for iy := 0 to ClientHeight div BmpHeight do
          for ix := 0 to ClientWidth div BmpWidth do
            BitBlt(Canvas.handle, ix * BmpWidth, iy * BmpHeight,
              BmpWidth, BmpHeight, BmpCanvas,
              0, 0, SRCCOPY);
      end
      else
       inherited;
    DoTile;
end;



function TCustomuETileImage.GetCanvas: TCanvas;
begin
  Result := inherited Canvas;
end;

procedure TCustomuETileImage.SetImage(const AValue: TBitmap);
begin
  if (FImage <> nil) and (FImage=AValue) then exit;
  FImage.Assign(AValue);
end;

procedure TCustomuETileImage.SetTransparent(const AValue: Boolean);
begin
  if FTransparent = AValue then exit;
  FTransparent := AValue;
  RenderControl;
  invalidate;
end;

procedure TCustomuETileImage.SetColor(AValue: TColor);
begin
  if FColor = AValue then exit;
  FColor := AValue;
  RenderControl;
  inherited SetColor(AValue);
  invalidate;
end;

procedure TCustomuETileImage.ImageChanged(Sender: TObject);
begin
  RenderControl;
  if Assigned(OnImageChanged) then OnImageChanged(Self);
  invalidate;
end;

procedure TCustomuETileImage.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  PreferredWidth := Bitmap.Width;
  PreferredHeight := Bitmap.Height;
end;

class function TCustomuETileImage.GetControlClassDefaultSize: TSize;
begin
  Result.CX := DefaultSize;
  Result.CY := DefaultSize;
end;

procedure TCustomuETileImage.DoTile;
begin
  if Assigned(FOnTile) then FOnTile(Self);
end;

procedure TCustomuETileImage.DoBeforeTile;
begin
  if Assigned(FBeforeTile) then FBeforeTile(Self);
end;

class procedure TCustomuETileImage.WSRegisterClass;
begin
  inherited WSRegisterClass;
end;


end.

