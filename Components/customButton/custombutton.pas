unit CustomButton;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, lcltype, Messages, windows;

type

  { TCustomButton }

  TCustomButton = class(TWinControl)
  private
    { Private declarations }
    FEmbeddedLabel: TLabel;
    FEmbeddedShape: TShape;
  protected
    { Protected declarations }
    procedure CreateParams(var params: TCreateParams); override;
    procedure WMEraseBkGnd(var msg: TWMEraseBkGnd); message WM_ERASEBKGND;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  published
    { Published declarations }
    property EmbeddedLabel: TLabel read FEmbeddedLabel;
    property EmbeddedShape: TShape read FEmbeddedShape;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Standard',[TCustomButton]);
end;

{ TCustomButton }

procedure TCustomButton.CreateParams(var params: TCreateParams);
begin
  inherited CreateParams(params);
  params.ExStyle := params.ExStyle or WS_EX_TRANSPARENT;
end;

procedure TCustomButton.WMEraseBkGnd(var msg: TWMEraseBkGnd);
begin
  SetBkMode(msg.DC, TRANSPARENT);
  msg.result := 1;
end;

constructor TCustomButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csOpaque];

    //Default properties
    //FDown := false;
    //FIndex := 0;

    //self.FFontAlignment := taCenter;

    // Set default width and height
    with GetControlClassDefaultSize do
      SetInitialBounds(0, 0, 60, 60);

    // Add the embedded shape
    FEmbeddedShape := TShape.Create(self);
    FEmbeddedShape.Parent := self;
    FEmbeddedShape.SetSubComponent(true); // Tell the IDE to store the modified properties
    FEmbeddedShape.Align := alClient;
    FEmbeddedShape.Pen.Color := clNone;
    FEmbeddedShape.Pen.Width := 0;
    FEmbeddedShape.Brush.Color := clBlack;
    FEmbeddedShape.Shape := stRectangle;
    FEmbeddedShape.Name := 'EmbeddedShape';
    FEmbeddedShape.Visible := false;
    // Make sure the embedded shape can not be selected/deleted within the IDE
    FEmbeddedShape.ControlStyle := FEmbeddedShape.ControlStyle - [csNoDesignSelectable];

    // Add the embedded label
    FEmbeddedLabel := TLabel.Create(Self); // Add the embedded label
    FEmbeddedLabel.Parent := self;         // Show the label in the panel
    FEmbeddedLabel.SetSubComponent(true);  // Tell the IDE to store the modified properties
    FEmbeddedLabel.Name := 'EmbeddedLabel';
    FEmbeddedLabel.Caption := '';
    FEmbeddedLabel.AutoSize := false;
    FEmbeddedLabel.Alignment := taCenter;
    FEmbeddedLabel.Layout := tlCenter;
    FEmbeddedLabel.WordWrap := true;
    //FEmbeddedLabel.Color := Self.Color;
    FEmbeddedLabel.ParentColor := false;
    FEmbeddedLabel.ParentFont := true;
    FEmbeddedLabel.Transparent := true;
    //FEmbeddedLabel.Font.Color := FFontColor;
    //FEmbeddedLabel.OnClick := @LabelClick;
    FEmbeddedLabel.Align := alClient;
    //FEmbeddedLabel.Alignment := FFontAlignment;
    FEmbeddedLabel.ShowAccelChar := false; //To display the & symbol
    // Make sure the embedded label can not be selected/deleted within the IDE
    FEmbeddedLabel.ControlStyle := FEmbeddedLabel.ControlStyle - [csNoDesignSelectable];

    // Set other properties if necessary
    //...
    FEmbeddedLabel.BringToFront;
end;

end.
