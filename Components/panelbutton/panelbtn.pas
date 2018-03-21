unit PanelBtn;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, lcltype;

type

  { TPanelBtn }

  TPanelBtn = class(TPanel)
  private
    { Private declarations }
    FEmbeddedLabel: TLabel;
    FEmbeddedShape: TShape;
    FDown: boolean;
    //FIsChanged: boolean;
    FBorderColor: TColor;
    FBorderColorAlt: TColor;
    //FFontColor: TColor;
    FText: string;
    //FBackColor: TColor;
    FIndex: integer;
    FDefaultFontSize: integer;
    FDefaultFontName: string;
    FFontAlignment: TAlignment;
    FAltBorderColor: boolean;

    FOnPress: TNotifyEvent;
    FOnChange: TNotifyEvent;
    procedure LabelClick(Sender: TObject);
  protected
    { Protected declarations }
    procedure Paint; override;
    procedure Click; override;

    procedure SetDown(value: boolean);
    function GetDown: boolean;
    //procedure SetIsChanged(value: boolean);
    //function GetIsChanged: boolean;
    procedure SetBorderColor(oColor: TColor);
    function GetBorderColor: TColor;
    procedure SetBorderColorAlt(oColor: TColor);
    function GetBorderColorAlt: TColor;
    //procedure SetFontColor(oColor: TColor);
    //function GetFontColor: TColor;
    //procedure SetBackColor(oColor: TColor);
    //function GetBackColor: TColor;
    procedure SetText(sText: string);
    function GetText: string;
    procedure SetAlignment(fontAlign: TAlignment);
    function GetAlignment: TAlignment;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure SetButtonColor(backColor: TColor);
    procedure SetFontColor(fontColor: TColor);
    procedure SetMainBorderColor(brdColor: TColor);
    procedure SetAltBorderColor(brdColor: TColor; brdWidth: integer; showBorder: boolean);
    property DefaultFontSize: integer read FDefaultFontSize write FDefaultFontSize;
    property DefaultFontName: string read FDefaultFontName write FDefaultFontName;
  published
    { Published declarations }
    property EmbeddedLabel: TLabel read FEmbeddedLabel;
    property EmbeddedShape: TShape read FEmbeddedShape;
    property Down: boolean read GetDown write SetDown;
    //property IsChanged: boolean read GetIsChanged write SetIsChanged;
    property OnPress: TNotifyEvent read FOnPress write FOnPress;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property BorderColor: TColor read FBorderColor write FBorderColor;
    //property FontColor: TColor read FFontColor write FFontColor;
    property Text: string read GetText write SetText;
    property Index: integer read FIndex write FIndex;
    //property BackColor: TColor read FBackColor write FBackColor default clBlack;
    property FontAlignment: TAlignment read GetAlignment write SetAlignment;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Standard',[TPanelBtn]);
end;

{ TPanelBtn }

constructor TPanelBtn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  //Default properties
  FDown := false;
  FIndex := 0;
  //FIsChanged := false;
  BevelInner := bvRaised;
  BevelOuter := bvRaised;
  BorderStyle := bsSingle;
  //FFontColor := clWhite;
  FBorderColor := clSilver;
  FBorderColorAlt := clSilver;
  FAltBorderColor := false;;
  self.Color := clBlack;
  self.Font.Color := clWhite;
  self.FDefaultFontSize := self.Font.Size;
  self.FDefaultFontName := self.Font.Name;
  self.FFontAlignment := taCenter;

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
  FEmbeddedLabel.OnClick := @LabelClick;
  FEmbeddedLabel.Align := alClient;
  FEmbeddedLabel.Alignment := FFontAlignment;
  FEmbeddedLabel.ShowAccelChar := false; //To display the & symbol
  // Make sure the embedded label can not be selected/deleted within the IDE
  FEmbeddedLabel.ControlStyle := FEmbeddedLabel.ControlStyle - [csNoDesignSelectable];

  // Set other properties if necessary
  //...
  FEmbeddedLabel.BringToFront;
end;

procedure TPanelBtn.SetButtonColor(backColor: TColor);
begin
  self.Color := backColor;
  self.FEmbeddedShape.Brush.Color := backColor;
end;

procedure TPanelBtn.SetFontColor(fontColor: TColor);
begin
  self.Font.Color := fontColor;
  self.FEmbeddedLabel.Font.Color := fontColor;
end;

procedure TPanelBtn.SetMainBorderColor(brdColor: TColor);
begin
  self.FBorderColor := brdColor;
end;

procedure TPanelBtn.SetAltBorderColor(brdColor: TColor; brdWidth: integer; showBorder: boolean);
begin
  self.FEmbeddedShape.Pen.Color := brdColor;
  self.FEmbeddedShape.Pen.Width := brdWidth;
  self.FEmbeddedShape.Visible := showBorder;
end;

procedure TPanelBtn.LabelClick(Sender: TObject);
begin
  //Execute panel click event when clicking label
  self.Click;
  //Down := not Down;
end;

procedure TPanelBtn.Click;
begin
  inherited;
  Down := not Down;
end;

procedure TPanelBtn.Paint;
var
  Rect: TRect;
begin
  inherited Paint;

  Rect := GetClientRect;
  if BevelOuter <> bvNone then
  begin
    Frame3D(Canvas, Rect, FBorderColor, FBorderColor, BevelWidth);
  end;
  Frame3D(Canvas, Rect, FBorderColor, FBorderColor, BorderWidth);
  if BevelInner <> bvNone then
  begin
    Frame3D(Canvas, Rect, FBorderColor, FBorderColor, BevelWidth);
  end;
end;

procedure TPanelBtn.SetDown(value: boolean);
begin
  FDown := value;
  //if (FDown) then
  //begin
  //  //Color := FBackColor;
  //  FEmbeddedLabel.Color := clBlack;
  //  FEmbeddedLabel.Font.Color := clWhite;
  //  //Font.Color := clWhite;
  //  //BevelInner := bvRaised;
  //  //BevelOuter := bvRaised;
  //end
  //else
  //begin
  //  //Color := FBackColor;
  //  FEmbeddedLabel.Color := clBtnFace;
  //  FEmbeddedLabel.Font.Color := clBlack;
  //  //Font.Color := clBlack;
  //  //BevelInner := bvNone;
  //  //BevelOuter := bvNone;
  //end;
  if Assigned(FOnPress) then FOnPress(Self);
end;

function TPanelBtn.GetDown: boolean;
begin
  Result := FDown;
end;

//procedure TPanelBtn.SetIsChanged(value: boolean);
//begin
//  FIsChanged := value;
//  if (FIsChanged) then
//  begin
//    Color := clBlack;
//    Font.Color := clWhite;
//    //BevelInner := bvRaised;
//    //BevelOuter := bvRaised;
//  end
//  else
//  begin
//    Color := clBtnFace;
//    Font.Color := clBlack;
//    //BevelInner := bvNone;
//    //BevelOuter := bvNone;
//  end;
//  if Assigned(FOnChange) then FOnChange(Self);
//end;

//function TPanelBtn.GetIsChanged: boolean;
//begin
//  Result := FIsChanged;
//end;

procedure TPanelBtn.SetBorderColor(oColor: TColor);
begin
  FBorderColor := oColor;
end;

function TPanelBtn.GetBorderColor: TColor;
begin
  result := FBorderColor;
end;

procedure TPanelBtn.SetBorderColorAlt(oColor: TColor);
begin
  FBorderColorAlt := oColor;
end;

function TPanelBtn.GetBorderColorAlt: TColor;
begin
  result := FBorderColorAlt;
end;

//procedure TPanelBtn.SetFontColor(oColor: TColor);
//begin
//  FFontColor := oColor;
//end;
//
//function TPanelBtn.GetFontColor: TColor;
//begin
//  result := FBorderColor;
//end;

procedure TPanelBtn.SetText(sText: string);
begin
  FText := sText;
  EmbeddedLabel.Caption := FText;
end;

function TPanelBtn.GetText: string;
begin
  result := FText;
end;

procedure TPanelBtn.SetAlignment(fontAlign: TAlignment);
begin
  FFontAlignment := fontAlign;
  EmbeddedLabel.Alignment := FFontAlignment;
end;

function TPanelBtn.GetAlignment: TAlignment;
begin
  result := FFontAlignment;
end;

//procedure TPanelBtn.SetBackColor(oColor: TColor);
//begin
//  FBackColor := oColor;
//  FEmbeddedLabel.Font.Color := oColor;
//end;
//
//function TPanelBtn.GetBackColor: TColor;
//begin
//  result := FBackColor;
//end;



end.
