unit PanelTransparent;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls;

type
  TPanelTransparent = class(TPanel)
  private
    FBackground: TBitmap;
    procedure WMEraseBkGnd( Var msg: TWMEraseBkGnd ); message WM_ERASEBKGND;
  protected
    procedure CaptureBackground;
    procedure Paint; override;
  public
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    property Canvas;
    constructor Create( aOwner: TComponent ); override;
    destructor Destroy; override;
  published
    { Published declarations }
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Standard', [TPanelTransparent]);
end;

procedure TPanelTransparent.CaptureBackground;
var
  tempCanvas: TCanvas;
  dc: HDC;
  sourcerect: TRect;
begin
  FBackground := TBitmap.Create;
  with Fbackground do
  begin
    width := clientwidth;
    height := clientheight;
  end;
  sourcerect.TopLeft := ClientToScreen(clientrect.TopLeft);
  sourcerect.BottomRight := ClientToScreen( clientrect.BottomRight );
  dc:= CreateDC( 'DISPLAY', nil, nil, nil );
  try
    tempCanvas:= TCanvas.Create;
    try
      tempCanvas.handle:= dc;
      Fbackground.Canvas.CopyRect( clientrect, tempCanvas, sourcerect );
    finally
      tempCanvas.handle := 0;
      tempCanvas.free;
    end;
  finally
    DeleteDC( dc );
  end;
end;

constructor TPanelTransparent.Create(aOwner: TComponent);
begin
  inherited;
  ControlStyle := controlStyle - [csSetCaption];
end;

destructor TPanelTransparent.Destroy;
begin
  FBackground.free;
  inherited;
end;

procedure TPanelTransparent.Paint;
begin
  if csDesigning In ComponentState then
    inherited
    {would need to draw frame and optional caption here do not call
    inherited, the control fills its client area if you do}
end;

procedure TPanelTransparent.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if Visible and HandleAllocated and not (csDesigning In ComponentState) then
  begin
    Fbackground.Free;
    Fbackground := Nil;
    Hide;
    inherited;
    Parent.Update;
    Show;
  end
  else
    inherited;
end;

procedure TPanelTransparent.WMEraseBkGnd(var msg: TWMEraseBkGnd);
var
  tempCanvas: TCanvas;
begin
  if csDesigning In ComponentState then
    inherited
  else
  begin
    if not Assigned( FBackground ) then
      Capturebackground;
    tempCanvas := TCanvas.create;
    try
      tempCanvas.handle := msg.DC;
      tempCanvas.draw( 0, 0, FBackground );
    finally
      tempCanvas.handle := 0;
      tempCanvas.free;
    end;
    msg.result := 1;
  end;
end;

end.
