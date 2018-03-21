unit LabelBox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, lcltype;

type

  { TLabelBox }

  TLabelBox = class(TLabel)
  private
    { Private declarations }
    FBorderColor: TColor;
    FIndex: integer;
    FCornerSize: integer;
    FBorderStyle: TBorderStyle;
    FBorderWidth: integer;
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
  published
    { Published declarations }
    property BorderStyle: TBorderStyle read FBorderStyle write FBorderStyle default bsNone;
    property BorderColor: TColor read FBorderColor write FBorderColor;
    property Index: integer read FIndex write FIndex default -1;
    property CornerSize: integer read FCornerSize write FCornerSize default 0;
    property BorderWidth: integer read FBorderWidth write FBorderWidth default 1;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Standard',[TLabelBox]);
end;

{ TLabelBox }

constructor TLabelBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TLabelBox.Paint;
//var
//  Rect: TRect;
begin
  inherited Paint;

  if (BorderStyle = bsSingle) then
  begin
    //Rect := GetClientRect;
    //Frame3D(Canvas, Rect, FBorderColor, FBorderColor, 1);
    Canvas.Pen.Color := FBorderColor;
    Canvas.Pen.Width := FBorderWidth;
    Canvas.RoundRect (0, 0, ClientWidth, ClientHeight, FCornerSize, FCornerSize);
  end;
end;


end.
