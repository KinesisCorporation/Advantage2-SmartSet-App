unit SliderBars;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, ExtCtrls, Spin, LMessages;

type
  TSliderBarOrientation = (sboHorizontal, sboVertical);
  TMarkerStyle = (msTriangle, msLine, msBeam, msNone);

  { TCustomSlider }

  TCustomSlider = class(TCustomControl)
  private
    FIsSliderPlane: boolean;
    FOrientation: TSliderBarOrientation;
    FBorderColor: TColor;
    FMarginColor: TColor;
    FMarkerColor: TColor;
    FMarkerStyle: TMarkerStyle;
    FMaxValue: integer;
    FMaxValueY: integer;
    FMinValue: integer;
    FMinValueY: integer;
    FPosition: extended;
    FPositionY: extended;
    FLeftMarker,
    FTopMarker,
    FRightMarker,
    FBottomMarker: array of TPoint;
    FAssociate: TSpinEdit;
    FAssociateUpdating: boolean;
    FOnChange: TNotifyEvent;
    procedure AssociateChanged(Sender: TObject);
    procedure Change;
    procedure UpdateAssociateValue;
    procedure UpdateAssociateValueY; virtual;
    function GetValue: integer;
    function GetValueY: integer;
    procedure SetAssociate(AValue: TSpinEdit);
    procedure SetBorderColor(AValue: TColor);
    procedure SetMarkerStyle(AValue: TMarkerStyle);
    procedure SetMarginColor(AValue: TColor);
    procedure SetMarkerColor(AValue: TColor);
    procedure SetOrientation(AValue: TSliderBarOrientation);
    procedure SetMaxValue(AValue: integer); virtual;
    procedure SetMinValue(AValue: integer);
    procedure SetPosition(AValue: extended);
    procedure SetPositionY(AValue: extended);
    procedure SetValue(AValue: integer);
    procedure SetValueY(AValue: integer);
    procedure SetMarkerPosition(X,Y:Integer);
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure Resize; override;
  public
    constructor create(AOwner: TComponent); override;
    destructor destroy; override;
  protected
    procedure Paint; override;
    procedure PaintContent(aRect: TRect); virtual;
    property Width default 200;
    property Height default 28;
  published
    property BorderColor: TColor read FBorderColor write SetBorderColor default clSilver;
    property MarginColor: TColor read FMarginColor write SetMarginColor default clWhite;
    property MarkerColor: TColor read FMarkerColor write SetMarkerColor default clBlack;
    property MarkerStyle: TMarkerStyle read FMarkerStyle write SetMarkerStyle default msTriangle;
    property TabStop default true;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;


  TSliderPaint = procedure(Sender: TObject; aRect: TRect) of object;

  { TSliderBar }

  TSliderBar = class(TCustomSlider)
  private
    FOnPaint: TSliderPaint;
  protected
    procedure PaintContent(aRect: TRect); override;
  public
    property Position: extended read FPosition;
  published
    property Align;
    property Anchors;
    property Associate: TSpinEdit read FAssociate write SetAssociate;
    property BorderSpacing;
    property Constraints;
    property Cursor;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Height;
    property MaxValue: integer read FMaxValue write SetMaxValue default 100;
    property MinValue: integer read FMinValue write SetMinValue default 0;
    property Orientation: TSliderBarOrientation read FOrientation write SetOrientation default sboHorizontal;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Value: integer read GetValue write SetValue default 0;
    property Visible;
    property Width;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDockDrop;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPaint: TSliderPaint read FOnPaint write FOnPaint;
    property OnResize;
    property OnStartDrag;
  end;

  { TColorBar }

  TColorBar = class(TSliderBar)
  private
    FStartColor: TColor;
    FStopColor: TColor;
    procedure SetStartColor(AValue: TColor);
    procedure SetStopColor(AValue: TColor);
  protected
    procedure PaintContent(aRect: TRect); override;
  public
    procedure SetColors(aStart, aStop: TColor);
    property Position: extended read FPosition;
  published
    property Align;
    property Anchors;
    property Associate: TSpinEdit read FAssociate write SetAssociate;
    property BorderColor;
    property BorderSpacing;
    property Constraints;
    property Cursor;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Height;
    property MarginColor;
    property MarkerColor;
    property MarkerStyle;
    property MinValue: integer read FMinValue write SetMinValue default 0;
    property MaxValue: integer read FMaxValue write SetMaxValue default 100;
    property Orientation: TSliderBarOrientation read FOrientation write SetOrientation default sboHorizontal;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property StartColor: TColor read FStartColor write SetStartColor;
    property StopColor: TColor read FStopColor write SetStopColor;
    property TabOrder;
    property TabStop;
    property Value: integer read GetValue write SetValue default 0;
    property Visible;
    property Width;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDockDrop;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDrag;
  end;

  { TCustomSliderPlane }

  TCustomSliderPlane = class(TCustomSlider)
  private
    FAssociateY: TSpinEdit;
    FAssociateUpdatingY: boolean;
    procedure AssociateChangedY(Sender: TObject);
    procedure SetAssociate(AValue: TSpinEdit);
    procedure UpdateAssociateValueY; override;
    procedure SetAssociateY(AValue: TSpinEdit);
    procedure SetMaxValueY(AValue: integer);
    procedure SetMinValueY(AValue: integer);
    procedure SetValueY(AValue: integer);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property AssociateX: TSpinEdit read FAssociate write SetAssociate;
    property AssociateY: TSpinEdit read FAssociateY write SetAssociateY;
    property MaxValueX: integer read FMaxValue write SetMaxValue default 100;
    property MaxValueY: integer read FMaxValueY write SetMaxValueY default 100;
    property MinValueX: integer read FMinValue write SetMinValue default 0;
    property MinValueY: integer read FMinValueY write SetMinValueY default 0;
    property ValueX: integer read GetValue write SetValue default 0;
    property ValueY: integer read GetValueY write SetValueY default 0;
  public
    constructor create(AOwner: TComponent); override;
    destructor destroy; override;
    property PositionX: extended read FPosition;
    property PositionY: extended read FPositionY;
  published
    property Width default 200;
  end;

  { TSliderPlane }

  TSliderPlane = class(TCustomSliderPlane)
  private
    FOnPaint: TSliderPaint;
  protected
    procedure PaintContent(aRect: TRect); override;
  published
    property Align;
    property Anchors;
    property AssociateX;
    property AssociateY;
    property BorderSpacing;
    property Constraints;
    property Cursor;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Height;
    property MaxValueX;
    property MaxValueY;
    property MinValueX;
    property MinValueY;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property ValueX;
    property ValueY;
    property Visible;
    property Width;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDockDrop;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPaint: TSliderPaint read FOnPaint write FOnPaint;
    property OnResize;
    property OnStartDrag;
  end;

  { TColorPlane }

  TColorChannel = (ccRed, ccGreen, ccBlue, ccAlpha);

  { This control shows a plane cut through the RGB cube.
    This means that a two colour gradient will be drawn
    with the third colour fixed. That fixed color will
    be called the cut.}
  TColorPlane = class(TCustomSliderPlane)
  private
    FCutChannel: TColorChannel;
    FPositionCut: extended;
    FVerticalChannel: TColorChannel;
    FHorizontalChannel: TColorChannel;
    FAssociateBar: TColorBar;
    FAssociateUpdatingBar: boolean;
    // the following are allocated to
    // AssociateX, AssociateY and AssociateBar as required
    FRedEdit: TSpinEdit;
    FGreenEdit: TSpinEdit;
    FBlueEdit: TSpinEdit;
    procedure AssociateChangedBar(Sender: TObject);
    function GetColor: TColor;
    function GetCutValue: integer;
    procedure SetAssociateBar(AValue: TColorBar);
    procedure SetPositionCut(AValue: extended);
    procedure SetCutChannel(AValue: TColorChannel);
    procedure SetCutValue(AValue: integer);
    procedure SetMaxValue(AValue: integer); override;
    procedure UpdateAssociateBar;
  protected
    procedure SetColor(AValue: TColor); override;
    procedure PaintContent(aRect: TRect); override;
    procedure Loaded; override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor destroy; override;
    property PositionCut: extended read FPositionCut;
    property HorizontalChannel: TColorChannel read FHorizontalChannel;
    property VerticalChannel: TColorChannel read FVerticalChannel;
  published
    property Align;
    property Anchors;
    property BlueEdit: TSpinEdit read FBlueEdit write FBlueEdit;
    property BorderSpacing;
    property Constraints;
    property Color: TColor read GetColor write SetColor;
    property Cursor;
    property CutChannel: TColorChannel read FCutChannel write SetCutChannel;
    property CutValue: integer read GetCutValue write SetCutValue;
    property AssociateBar: TColorBar read FAssociateBar write SetAssociateBar;
    property DragCursor;
    property DragMode;
    property Enabled;
    property GreenEdit: TSpinEdit read FGreenEdit write FGreenEdit;
    property Height;
    property MaxValue: integer read FMaxValue write SetMaxValue default 255;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property RedEdit: TSpinEdit read FRedEdit write FRedEdit;
    property TabOrder;
    property TabStop;
    property Visible;
    property Width;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDockDrop;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDrag;
  end;

  { TRGBAColor }

  TRGBAColor = packed record
    case boolean of
      false: (color: TColor);
      true: (Red: byte; Green: byte; Blue: byte; Alpha: byte);
  end;

procedure Register;

implementation

uses
  LCLType, LCLIntf;

procedure Register;
begin
  RegisterComponents('MD',[TSliderBar, TColorBar, TSliderPlane, TColorPlane]);
end;

{ TCustomSlider }

constructor TCustomSlider.create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csRequiresKeyboardInput];
  FBorderColor := clSilver;
  //FIsSliderPlane := false;
  FMarginColor := clWhite;
  //FMarkerColor := clBlack;
  //FMarkerStyle := msBeam;
  //FOrientation := sboHorizontal;
  //FMinValue := 0;
  FMaxValue := 100;
  SetLength(FTopMarker, 3);
  SetLength(FBottomMarker, 3);
  SetLength(FLeftMarker, 3);
  SetLength(FRightMarker, 3);
  SetBounds(Left, Top, 200, 28);
  TabStop := true;
end;

destructor TCustomSlider.destroy;
begin
  if assigned(FAssociate) then
    FAssociate.OnChange := nil;
  inherited destroy;
end;

procedure TCustomSlider.AssociateChanged(Sender: TObject);
begin
  FAssociateUpdating := true;
  try
    SetValue(FAssociate.Value);
  finally
    FAssociateUpdating := false;
  end;
end;

procedure TCustomSlider.Change;
begin
  if assigned(FOnChange) then
    FOnChange(self);
end;

function TCustomSlider.GetValue: integer;
begin
  result := round(FPosition*(FMaxValue - FMinValue) + FMinValue);
end;

function TCustomSlider.GetValueY: integer;
begin
  result := round(FPositionY*(FMaxValueY - FMinValueY) + FMinValueY);
end;

procedure TCustomSlider.KeyDown(var Key: Word; Shift: TShiftState);
var
  step: integer;
begin
  step := 1;
  if ssShift in Shift then
    step := 5;
  if (Key = VK_RIGHT) and ((FOrientation = sboHorizontal) or FIsSliderPlane) then
    SetValue(GetValue + step)
  else if (Key = VK_LEFT) and ((FOrientation = sboHorizontal) or FIsSliderPlane) then
    SetValue(GetValue - step)
  else if (Key = VK_UP) and ((FOrientation = sboVertical) or FIsSliderPlane) then begin
    if FIsSliderPlane then
      SetValueY(GetValueY + step)
    else if FOrientation = sboVertical then
      SetValue(GetValue + step);
  end
  else if (Key = VK_DOWN) and ((FOrientation = sboVertical) or FIsSliderPlane) then begin
    if FIsSliderPlane then
      SetValueY(GetValueY - step)
    else if FOrientation = sboVertical then
      SetValue(GetValue - step);
  end
  else begin
    inherited KeyDown(Key, Shift);
    exit;
  end;
  Key := 0;
end;

procedure TCustomSlider.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Button = mbLeft then begin
    if TabStop then SetFocus;
    SetMarkerPosition(X, Y);
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCustomSlider.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if shift = [ssLeft] then
    SetMarkerPosition(X, Y);
  inherited MouseMove(Shift, X, Y);
end;

procedure TCustomSlider.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (AComponent = FAssociate) and (Operation = opRemove) then
    FAssociate := nil;
  inherited Notification(AComponent, Operation);
end;

procedure TCustomSlider.Paint;
const
  MarkerSize = 4;
var
  aRect: TRect;
  x, y, y1, y2, x1, x2: integer;
  yp: extended;
begin
  // First paint the content... done by descendants
  aRect := ClientRect;
  inflateRect(aRect, -2, -2);
  PaintContent(aRect);

  if MarkerStyle <> msNone then begin
    // calculate marker's position
    if (Forientation = sboHorizontal) or FIsSliderPlane then
      x := round ( FPosition * (aRect.Right - aRect.left - 1)) + aRect.Left;
    if FIsSliderPlane then
      yp := FPositionY
    else
      yp := FPosition;
    if (FOrientation = sboVertical) or FIsSliderPlane then
      y := aRect.bottom - round ( yp * (aRect.bottom - aRect.top - 1)) - 1;
  end;

  // paint margin
  inflateRect(aRect, 1, 1);
  with Canvas do begin
    pen.width := 1;
    pen.style := psSolid;
    pen.mode := pmCopy;
    pen.color := MarginColor;
    Frame(aRect);
  end;

  if MarkerStyle <> msNone then begin

    if MarkerStyle <> msLine then begin
      if (FOrientation = sboHorizontal) or FIsSliderPlane then begin
        FTopMarker[2] := Point(x, aRect.Top+MarkerSize);
        FTopMarker[0] := Point(x-MarkerSize, aRect.Top);
        FTopMarker[1] := Point(x+MarkerSize, aRect.Top);

        FBottomMarker[2] := Point(x, aRect.Bottom-MarkerSize-1);
        FBottomMarker[0] := Point(x-MarkerSize, aRect.Bottom-1);
        FBottomMarker[1] := Point(x+MarkerSize, aRect.Bottom-1);
      end;
      if (FOrientation = sboVertical) or FIsSliderPlane then begin
        FLeftMarker[2] := Point(aRect.Left+MarkerSize, y);
        FLeftMarker[0] := Point(aRect.left, y-MarkerSize);
        FLeftMarker[1] := Point(aRect.left, y+MarkerSize);

        FRightMarker[2] := Point(aRect.Right-MarkerSize-1, y);
        FRightMarker[0] := Point(aRect.Right-1, y-MarkerSize);
        FRightMarker[1] := Point(aRect.Right-1, y+MarkerSize);
      end;
    end;

    with canvas do begin
      if MarkerStyle <> msTriangle then begin
        // draw margin around marker line
        pen.color := MarginColor;
        if (FOrientation = sboHorizontal) or FIsSliderPlane then begin
          moveto(x-1, aRect.Top+1);
          lineto(x-1, aRect.Bottom-1);
          moveto(x+1, aRect.Top+1);
          lineto(x+1, aRect.Bottom-1);
        end;
        if (FOrientation = sboVertical) or FIsSliderPlane then begin
          moveto(aRect.Left+1, y-1);
          lineto(aRect.Right-1, y-1);
          moveto(aRect.Left+1, y+1);
          lineto(aRect.Right-1, y+1);
        end;
      end;
      if MarkerStyle <> msLine then begin
        // draw markers
        pen.color := MarginColor;
        brush.color := MarkerColor;
        if (FOrientation = sboHorizontal) or FIsSliderPlane then begin
          Polygon(FTopMarker);
          Polygon(FBottomMarker);
        end;
        if (FOrientation = sboVertical) or FIsSliderPlane then begin
          Polygon(FLeftMarker);
          Polygon(FRightMarker);
        end;
      end;

      if MarkerStyle <> msTriangle then begin
        // draw marker line
        pen.color := MarkerColor;
        if (Forientation = sboHorizontal) or FIsSliderPlane then begin
          y1 := aRect.Top+1;
          y2 := aRect.Bottom-1;
          if MarkerStyle = msBeam then begin
            inc(y1);
            dec(y2);
          end
          else if MarkerColor = BorderColor then begin
            dec(y1, 2);
            inc(y2, 2);
          end;
          moveto(x, y1);
          lineto(x, y2);
        end;
        if (FOrientation = sboVertical) or FIsSliderPlane then begin
          x1 := aRect.Left+1;
          x2 := aRect.Right-1;
          if MarkerStyle = msBeam then begin
            inc(x1);
            dec(x2);
          end
          else if MarkerColor = BorderColor then begin
            dec(x1, 2);
            inc(x2, 2);
          end;
          moveto(x1, y);
          lineto(x2, y);
        end;
      end;
    end;
  end;

  with canvas do begin
     // draw frame
    if Focused then
      pen.color := clHighlight
    else
      pen.color := BorderColor;
    aRect := ClientRect;
    Frame(aRect);
  end;
end;

procedure TCustomSlider.PaintContent(aRect: TRect);
begin
  // do nothing, its up to the descendants
end;

procedure TCustomSlider.Resize;
begin
  inherited Resize;
  invalidate;
end;

procedure TCustomSlider.SetAssociate(AValue: TSpinEdit);
begin
  if FAssociate=AValue then Exit;
  if assigned(FAssociate) then
    FAssociate.OnChange := nil;
  FAssociate:=AValue;
  if assigned(FAssociate) then begin
    FAssociate.MaxValue := FMaxValue;
    FAssociate.MinValue := FMinValue;
    UpdateAssociateValue;
  end;
end;

procedure TCustomSlider.SetBorderColor(AValue: TColor);
begin
  if FBorderColor=AValue then Exit;
  FBorderColor:=AValue;
  Invalidate;
end;

procedure TCustomSlider.SetMarginColor(AValue: TColor);
begin
  if FMarginColor=AValue then Exit;
  FMarginColor:=AValue;
  Invalidate;
end;

procedure TCustomSlider.SetMarkerColor(AValue: TColor);
begin
  if FMarkerColor=AValue then Exit;
  FMarkerColor:=AValue;
  Invalidate;
end;

procedure TCustomSlider.SetMarkerPosition(X,Y:Integer);
begin
  if (FOrientation = sboHorizontal) or FIsSliderPlane then begin
    if x < 2 then
      x := 2
    else if x > width - 2 then
      x := width-2;
    SetPosition( (x - 2)/(width - 4) );
  end;
  if (FOrientation = sboVertical) or FIsSliderPlane then begin
    if y < 2 then
      y := 2
    else if y > height - 2 then
      y := height-2;
    if FIsSliderPlane then
      SetPositionY( 1 - (y - 2)/(height - 4) )
    else
      SetPosition( 1 - (y - 2)/(height - 4) );
  end
end;

procedure TCustomSlider.SetMaxValue(AValue: integer);
var
  pos: extended;
begin
  if FMaxValue=AValue then Exit;
  pos := FPosition;
  FMaxValue:=AValue;
  if assigned(fAssociate) then
    FAssociate.MaxValue := AValue;
  FPosition := pos;
  UpdateAssociateValue;
  Change;
end;

procedure TCustomSlider.SetMarkerStyle(AValue: TMarkerStyle);
begin
  if FMarkerStyle=AValue then Exit;
  FMarkerStyle:=AValue;
  Invalidate;
end;

procedure TCustomSlider.SetMinValue(AValue: integer);
var
  pos: extended;
begin
  if FMinValue=AValue then Exit;
  pos := FPosition;
  FMinValue:=AValue;
  if assigned(fAssociate) then
    FAssociate.MinValue := AValue;
  FPosition := pos;
  UpdateAssociateValue;
  Change;
end;

procedure TCustomSlider.SetOrientation(AValue: TSliderBarOrientation);
var
  nh, nw: integer;
begin
  if FOrientation=AValue then Exit;
  FOrientation:=AValue;
  nh := height;
  nw := width;
  if FOrientation = sboVertical then begin
    if width > height then begin
      nh := width;
      nw := height;
    end;
  end
  else if FOrientation = sboHorizontal then begin
    if height > width then begin
      nw := height;
      nh := width;
    end;
  end;
  setbounds(Left, Top, nw, nh);
  Invalidate;
end;

procedure TCustomSlider.SetPosition(AValue: extended);
begin
  if FPosition=AValue then Exit;
  if AValue < 0.0 then
    AValue := 0.0
  else if AValue > 1.0 then
    AValue := 1.0;
  FPosition:=AValue;
  Invalidate;
  UpdateAssociateValue;
  Change;
end;

procedure TCustomSlider.SetPositionY(AValue: extended);
begin
  if FPositionY=AValue then Exit;
  if AValue < 0.0 then
    AValue := 0.0
  else if AValue > 1.0 then
    AValue := 1.0;
  FPositionY:=AValue;
  Invalidate;
  UpdateAssociateValueY;
  Change;
end;

procedure TCustomSlider.SetValue(AValue: integer);
begin
  SetPosition( (aValue - FMinValue)/(FMaxValue - FMinValue) );
end;

procedure TCustomSlider.SetValueY(AValue: integer);
begin
  SetPositionY( (aValue - FMinValueY)/(FMaxValueY - FMinValueY) );
end;

procedure TCustomSlider.UpdateAssociateValue;
begin
  if assigned(FAssociate) and not FAssociateUpdating then begin
    // don't want associate's OnChange to start loop;
    FAssociate.OnChange := nil;
    try
      FAssociate.Value := GetValue;
    finally
      FAssociate.OnChange := @AssociateChanged;
    end;
  end;
end;

procedure TCustomSlider.UpdateAssociateValueY;
begin
  // do nothing TCustomPlane handles this
end;

{ TSliderBar }

procedure TSliderBar.PaintContent(aRect: TRect);
begin
  if assigned(FOnPaint) then
    FOnPaint(Self, aRect);
end;

{ TColorBar }

procedure TColorBar.PaintContent(aRect: TRect);
begin
  if orientation = sboHorizontal then
    Canvas.GradientFill(aRect, FStartColor, FStopColor, gdHorizontal)
  else
    Canvas.GradientFill(aRect, FStopColor, FStartColor, gdVertical)
end;

procedure TColorBar.SetColors(aStart, aStop: TColor);
begin
  FStartColor := aStart;
  FStopColor := aStop;
  Invalidate;
end;

procedure TColorBar.SetStartColor(AValue: TColor);
begin
  if FStartColor=AValue then Exit;
  FStartColor:=AValue;
  Invalidate;
end;

procedure TColorBar.SetStopColor(AValue: TColor);
begin
  if FStopColor=AValue then Exit;
  FStopColor:=AValue;
  Invalidate;
end;

{ TCustomSliderPlane }

constructor TCustomSliderPlane.create(AOwner: TComponent);
begin
  inherited create(AOwner);
  FIsSliderPlane := true;
  //FMinValueY := 0;
  FMaxValueY := 100;
  Height := Width;
end;

destructor TCustomSliderPlane.destroy;
begin
  if assigned(FAssociateY) then
    FAssociateY.OnChange := nil;
  inherited destroy;
end;

procedure TCustomSliderPlane.AssociateChangedY(Sender: TObject);
begin
  FAssociateUpdatingY := true;
  try
    SetValueY(FAssociateY.Value);
  finally
    FAssociateUpdatingY := false;
  end;
end;

procedure TCustomSliderPlane.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (AComponent = FAssociateY) and (Operation = opRemove) then
    FAssociateY := nil;
  inherited Notification(AComponent, Operation);
end;

procedure TCustomSliderPlane.SetAssociate(AValue: TSpinEdit);
begin
  if FAssociate=AValue then Exit;
  FAssociate:=AValue;
end;

procedure TCustomSliderPlane.SetAssociateY(AValue: TSpinEdit);
begin
  if FAssociateY=AValue then Exit;
  if assigned(FAssociateY) then
    FAssociateY.OnChange := nil;
  FAssociateY:=AValue;
  if assigned(FAssociateY) then begin
    FAssociateY.MaxValue := FMaxValueY;
    FAssociateY.MinValue := FMinValueY;
    UpdateAssociateValueY;
  end;
end;

procedure TCustomSliderPlane.SetMaxValueY(AValue: integer);
var
  posY: extended;
begin
  if FMaxValueY=AValue then Exit;
  posY := FPositionY;
  FMaxValueY:=AValue;
  if assigned(fAssociateY) then
    FAssociateY.MaxValue := AValue;
  FPositionY := posY;
  UpdateAssociateValueY;
  Change;
end;

procedure TCustomSliderPlane.SetMinValueY(AValue: integer);
var
  posY: extended;
begin
  if FMinValueY=AValue then Exit;
  posY := FPositionY;
  FMinValueY:=AValue;
  if assigned(fAssociateY) then
    FAssociateY.MinValue := AValue;
  FPositionY := posY;
  UpdateAssociateValueY;
  Change;
end;

procedure TCustomSliderPlane.SetValueY(AValue: integer);
begin
  SetPositionY( (aValue - FMinValueY)/(FMaxValueY - FMinValueY) );
end;

procedure TCustomSliderPlane.UpdateAssociateValueY;
begin
  if assigned(FAssociateY) and not FAssociateUpdatingY then begin
    // don't want associate's OnChange to start loop;
    FAssociateY.OnChange := nil;
    try
      FAssociateY.Value := GetValueY;
    finally
      FAssociateY.OnChange := @AssociateChangedY;
    end;
  end;
end;

{ TSliderPlane }

procedure TSliderPlane.PaintContent(aRect: TRect);
begin
  if assigned(FOnPaint) then
    FOnPaint(Self, aRect);
end;

{ TColorPlane }

constructor TColorPlane.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FMaxValue := 255;
  FMaxValueY := 255;
end;

destructor TColorPlane.destroy;
begin
  if assigned(FAssociateBar) then
    FAssociateBar.OnChange := nil;
  inherited destroy;
end;

procedure TColorPlane.AssociateChangedBar(Sender: TObject);
begin
  FAssociateUpdatingBar := true;
  try
    SetCutValue(FAssociateBar.Value);
  finally
    FAssociateUpdatingBar := false;
  end;
end;

procedure TColorPlane.Loaded;
begin
  inherited Loaded;
  FCutChannel := ccBlue;
  CutChannel := ccRed;
end;

function TColorPlane.GetColor: TColor;
begin
  result := clBlack;
  case FCutChannel of
    ccRed: result := RGB(round(FPositionCut*255),
                         round(FPositionY*255),
                         round(FPosition*255));
    ccGreen: result := RGB(round(FPositionY*255),
                           round(FPositionCut*255),
                           round(FPosition*255));
    ccBlue: result := RGB(round(FPositionY*255),
                          round(FPosition*255),
                          round(FPositionCut*255));
  end;
end;

function TColorPlane.GetCutValue: integer;
begin
  result := round(FPositionCut*FMaxValue);
end;

procedure TColorPlane.PaintContent(aRect: TRect);
var
  aColor: TRGBAColor;
  ht, wd, x, y: integer;
  cutColor: integer;
begin
  ht := aRect.Bottom - aRect.Top - 1;
  wd := aRect.Right - aRect.Left - 1;
  cutColor := round(FPositionCut*255);
  if CutChannel = ccRed then begin
    aColor.Red := cutColor;
    for y := 0 to ht do begin
      aColor.Green := ((ht-y)*255) div ht;
      for x := 0 to wd do begin
        aColor.Blue := (x*255) div wd;
        Canvas.Pixels[x + aRect.Left, y + aRect.Top] := aColor.Color;
      end;
    end;
  end
  else if CutChannel = ccGreen then begin
    aColor.Green := cutColor;
    for y := 0 to ht do begin
      aColor.Red := ((ht-y)*255) div ht;
      for x := 0 to wd do begin
        aColor.Blue := (x*255) div wd;
        Canvas.Pixels[x + aRect.Left, y + aRect.Top] := aColor.Color;
      end;
    end;
  end
  else if CutChannel = ccBlue then begin
    aColor.Blue := CutColor;
    for y := 0 to ht do begin
      aColor.Red := ((ht-y)*255) div ht;
      for x := 0 to wd do begin
        aColor.Green := (x*255) div wd;
        Canvas.Pixels[x + aRect.Left, y + aRect.Top] := aColor.Color;
      end;
    end;
  end
end;

procedure TColorPlane.SetAssociateBar(AValue: TColorBar);
var
  aColor: TColor;
begin
  if FAssociateBar = AValue then exit;
  if assigned(FAssociateBar) then
    FAssociateBar.OnChange := nil;
  FAssociateBar := AValue;
  if assigned(FAssociateBar) then begin
     FAssociateBar.MaxValue := FMaxValue;
     FAssociateBar.MinValue := 0;
     case cutChannel of
       ccRed: aColor := clRed;
       ccGreen: aColor := clLime;
       ccBlue: aColor := clBlue;
     end;
     FAssociateBar.SetColors(clBlack, aColor);
     UpdateAssociateBar;
   end;
end;

procedure TColorPlane.SetColor(AValue: TColor);
var
  AColor: TRGBAColor;
begin
  if AValue = GetColor then
    exit;
  AColor.Color := AValue;
  case FCutChannel of
    ccRed:
      begin
        FPosition := AColor.Blue/255;
        FPositionY := AColor.Green/255;
        FPositionCut := AColor.Red/255;
      end;
    ccGreen:
      begin
        FPosition := AColor.Blue/255;
        FPositionY := AColor.Red/255;
        FPositionCut := AColor.Green/255;
      end;
    ccBlue:
      begin
        FPosition := AColor.Green/255;
        FPositionY := AColor.Red/255;
        FPositionCut := AColor.Blue/255;
      end;
  end;
  invalidate;
  UpdateAssociateValue;
  UpdateAssociateValueY;
  UpdateAssociateBar;
  change;
end;

procedure TColorPlane.SetCutChannel(AValue: TColorChannel);
var
  current: TRGBAColor;
begin
  if FCutChannel = AValue then Exit;
  current.color := GetColor;
  FCutChannel := AValue;
  case FCutChannel of
    ccRed: begin
             FHorizontalChannel := ccBlue;
             FPosition := current.Blue/255;
             SetAssociate(FBlueEdit);

             FVerticalChannel := ccGreen;
             FPositionY := current.Green/255;
             SetAssociateY(FGreenEdit);

             if assigned(FAssociateBar) then begin
               AssociateBar.SetColors(clBlack, clRed);
               AssociateBar.Associate := FRedEdit;
             end;
             FPositionCut := current.Red/255;
           end;
    ccGreen: begin
             FHorizontalChannel := ccBlue;
             FPosition := current.Blue/255;
             SetAssociate(FBlueEdit);

             FVerticalChannel := ccRed;
             FPositionY := current.Red/255;
             SetAssociateY(FRedEdit);

             if assigned(FAssociateBar) then begin
               AssociateBar.SetColors(clBlack, clLime);
               AssociateBar.Associate := FGreenEdit;
             end;
             FPositionCut := current.Green/255;
           end;
    ccBlue: begin
             FHorizontalChannel := ccGreen;
             FPosition := current.Green/255;
             SetAssociate(FGreenEdit);

             FVerticalChannel := ccRed;
             FPositionY := current.Red/255;
             SetAssociateY(FRedEdit);

             if assigned(FAssociateBar) then begin
               AssociateBar.SetColors(clBlack, clBlue);
               AssociateBar.Associate := FBlueEdit;
             end;
             FPositionCut := current.Blue/255;
           end;
  else
    exit;
  end;
  Invalidate;
  UpdateAssociateValue;
  UpdateAssociateValueY;
  UpdateAssociateBar;
  Change;
end;

procedure TColorPlane.SetPositionCut(AValue: extended);
begin
  if FPositionCut=AValue then Exit;
  if AValue < 0.0 then
    AValue := 0.0
  else if AValue > 1.0 then
    AValue := 1.0;
  FPositionCut:=AValue;
  Invalidate;
  UpdateAssociateBar;
  Change;
end;

procedure TColorPlane.SetCutValue(AValue: integer);
begin
  SetPositionCut(AValue/FMaxValue);
end;

procedure TColorPlane.SetMaxValue(AValue: integer);
var
  posCut: extended;
begin
  if FMaxValue=AValue then Exit;
  posCut := FPositionCut;
  if assigned(FAssociateBar) then
    FAssociateBar.MaxValue := AValue;
  FPositionCut := posCut;
  inherited SetMaxValue(AValue);
  inherited SetMaxValueY(AValue);
  Change;
end;

procedure TColorPlane.UpdateAssociateBar;
begin
  if assigned(FAssociateBar) and not FAssociateUpdatingBar then begin
     // don't want associate's OnChange to start loop;
     FAssociateBar.OnChange := nil;
     try
       FAssociateBar.Value := CutValue;
     finally
       FAssociateBar.OnChange := @AssociateChangedBar;
     end;
   end;
end;

end.

