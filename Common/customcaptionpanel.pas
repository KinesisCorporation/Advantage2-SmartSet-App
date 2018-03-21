unit CustomCaptionPanel;

interface

uses
  Windows, SysUtils, Classes, Controls, Graphics;

type
  TCustomCaptionPanel = class(TCustomControl)
  private const
    DEFAULT_BORDER_COLOR = $0033CCFF;
    DEFAULT_CLIENT_COLOR = clWindow;
    DEFAULT_BORDER_RADIUS = 16;
  private
    { Private declarations }
    FBorderColor: TColor;
    FClientColor: TColor;
    FBorderRadius: integer;
    FCaption: TCaption;
    FAlignment: TAlignment;
    procedure SetBorderColor(BorderColor: TColor);
    procedure SetClientColor(ClientColor: TColor);
    procedure SetBorderRadius(BorderRadius: integer);
    procedure SetCaption(const sCaption: TCaption);
    procedure SetAlignment(Alignment: TAlignment);
  protected
    procedure Paint; override;
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    { Public declarations }
  published
    { Published declarations }
    property Color;
    property Caption read FCaption write SetCaption;
    property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
    property Font;
    property BorderColor: TColor read FBorderColor write SetBorderColor default DEFAULT_BORDER_COLOR;
    property ClientColor: TColor read FClientColor write SetClientColor default DEFAULT_CLIENT_COLOR;
    property BorderRadius: integer read FBorderRadius write SetBorderRadius default DEFAULT_BORDER_RADIUS;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Rejbrand 2009', [TCustomCaptionPanel]);
end;

{ TCustomCaptionPanel }

constructor TCustomCaptionPanel.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csSetCaption, csOpaque, csDoubleClicks, csReplicatable];
  FBorderColor := DEFAULT_BORDER_COLOR;
  FClientColor := DEFAULT_CLIENT_COLOR;
  FBorderRadius := DEFAULT_BORDER_RADIUS;
  FAlignment := taCenter;
end;

procedure TCustomCaptionPanel.Paint;
var
  r: TRect;
const
  Alignments: array[TAlignment] of integer = (DT_LEFT, DT_RIGHT, DT_CENTER);
begin
  inherited;
  Canvas.Pen.Color := FBorderColor;
  Canvas.Brush.Color := FBorderColor;
  Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(Rect(FBorderRadius,
    0,
    ClientWidth - FBorderRadius,
    FBorderRadius));
  Canvas.Ellipse(Rect(0,
    0,
    2*FBorderRadius,
    2*FBorderRadius));
  Canvas.Ellipse(Rect(ClientWidth - 2*FBorderRadius,
    0,
    ClientWidth,
    2*FBorderRadius));
  Canvas.Brush.Color := FClientColor;
  Canvas.Rectangle(Rect(0,
    FBorderRadius,
    ClientWidth,
    ClientHeight));
  Canvas.Font.Assign(Self.Font);
  r := Rect(FBorderRadius, 0, ClientWidth - FBorderRadius, FBorderRadius);
  Canvas.Brush.Style := bsClear;
  DrawText(Canvas.Handle,
    PChar(Caption),
    length(Caption),
    r,
    DT_SINGLELINE or DT_LEFT or DT_VCENTER or DT_END_ELLIPSIS or Alignments[FAlignment]);
end;

procedure TCustomCaptionPanel.SetAlignment(Alignment: TAlignment);
begin
  if FAlignment <> Alignment then
  begin
    FAlignment := Alignment;
    Invalidate;
  end;
end;

procedure TCustomCaptionPanel.SetBorderColor(BorderColor: TColor);
begin
  if FBorderColor <> BorderColor then
  begin
    FBorderColor := BorderColor;
    Invalidate;
  end;
end;

procedure TCustomCaptionPanel.SetBorderRadius(BorderRadius: integer);
begin
  if FBorderRadius <> BorderRadius then
  begin
    FBorderRadius := BorderRadius;
    Invalidate;
  end;
end;

procedure TCustomCaptionPanel.SetCaption(const sCaption: TCaption);
begin
  if not(AnsiUpperCase(FCaption) = AnsiUpperCase(sCaption)) then
  begin
    FCaption := sCaption;
    Invalidate;
  end;
end;

procedure TCustomCaptionPanel.SetClientColor(ClientColor: TColor);
begin
  if FClientColor <> ClientColor then
  begin
    FClientColor := ClientColor;
    Invalidate;
  end;
end;

end.
