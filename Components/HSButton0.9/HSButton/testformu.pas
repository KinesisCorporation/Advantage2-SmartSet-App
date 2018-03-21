unit TestFormu;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFNDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  Forms, sysutils,Graphics, Dialogs, HSSpeedButton ,HSButton, HSButtons, HSStaticText, Classes, Controls,
  StdCtrls, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    ScrollBar1: TScrollBar;
    Label1: TLabel;
    Label2: TLabel;
    ScrollBar2: TScrollBar;
    Label3: TLabel;
    CheckBox1: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure ScrollBar2Change(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  bmp:TBitmap;
//  procedure goBlur(Bitmap:TBitmap;Density:Smallint);
implementation

{$IFNDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TForm1.CheckBox1Click(Sender: TObject);
var i: integer;
begin
  for i := 0 to ControlCount - 1 do
    if (Controls[i] is THSSpeedButton) then
      THSSpeedButton(Controls[i]).Flat:=CheckBox1.Checked

end;

procedure TForm1.FormClick(Sender: TObject);
var i:word;
begin
  if ComponentCount>0 then for i:= 0 to ComponentCount-1 do
    if Components[i] is THSSpeedButton then
        THSSpeedButton(Components[i]).Down:=not THSSpeedButton(Components[i]).Down
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  Canvas.Lock;
  Canvas.Draw(0,0,bmp);
  Canvas.Unlock;
end;

procedure TForm1.FormCreate(Sender: TObject);
var i:Word;
begin
  bmp:=TBitmap.Create;
  bmp.Transparent:=true;
  bmp.TransparentColor:=clWhite;
  if FileExists('D:\Downloads\test.bmp') then bmp.LoadFromFile('D:\Downloads\test.bmp');

  for i := 0 to 7 do
    with THSSpeedButton.Create(self) do
      begin
        Parent:=self;
  //      Transparent:=True;
        SlowDecease:=sdEnterLeave;
//        Color:=clRed;

        Flat:=True;
        Smooth:=0;
        Border:=8;
        SetBounds(10,10+i*45,100,40);
        Caption:='Caption';
        NumGlyphs:=2;

        Layout:=blGlyphTop;
//        Glyph.LoadFromResourceName(HInstance,'BBOK');
        ColorStyle:=THSColorStyle(i+8);
        Style:=THSButtonStyle.bsNormal;
        HotTrackColor:=clRed;

      end;

//  exit;

  for i := 0 to 7 do

    with THSSpeedButton.Create(self) do
      begin
        Name:='ss'+IntToStr(i);
        Parent:=self;
//        Transparent:=True;
        SlowDecease:=sdEnterLeave;
        Flat:=True;
        Smooth:=2;
        Border:=8 ;
        SetBounds(120,10+i*45,100,40);
        Caption:='Caption';
//        Kind:=bkCancel;
        NumGlyphs:=2;
        Layout:=blGlyphTop;
//        Glyph.LoadFromResourceName(HInstance,'BBOK');
//        HotTrackColor:=clRed;
        ColorStyle:=THSColorStyle(i);
//        Color:=clRed;
//        HotTrackColor:=clYellow;

      end;
  for i := 0 to 6 do

    with THSButton.Create(self) do
      begin
        Parent:=self;
//        SlowDecease:=sdEnterLeave;
        Color:=clGreen;
//        HotTrackColor:=clWhite;
//        UseHotTrackFont:=True;
//        HotTrackFont.Color:=clSilver;
        SetBounds(230,10+i*45,100,40);

        Caption:='Caption';
//        Kind:=bkCancel;
//        Flat:=True;

        NumGlyphs:=2;
        Smooth:=2;
//        Default:=true;
        Border:=8;
        Layout:=blGlyphTop;
//        Glyph.LoadFromResourceName(HInstance,'BBOK');
//        HotTrackColor:=clRed;
        Style:=THSButtonStyle(i);

//      Color:=clNavy
      end;
end;

procedure TForm1.ScrollBar1Change(Sender: TObject);
var i:Integer;
begin
  for i := 0 to ControlCount - 1 do
    if (Controls[i] is THSSpeedButton) then
      THSSpeedButton(Controls[i]).Border:=ScrollBar1.Position
    else if (Controls[i] is THSButton) then
      THSButton(Controls[i]).Border:=ScrollBar1.Position
    else if (Controls[i] is THSStaticText) then
      THSStaticText(Controls[i]).Border:=ScrollBar1.Position

end;

procedure TForm1.ScrollBar2Change(Sender: TObject);
var i:Integer;
begin
  for i := 0 to ControlCount - 1 do
    if (Controls[i] is THSSpeedButton) then
      THSSpeedButton(Controls[i]).Smooth:=ScrollBar2.Position
    else if (Controls[i] is THSButton) then
      THSButton(Controls[i]).Smooth:=ScrollBar2.Position
    else if (Controls[i] is THSStaticText) then
      THSStaticText(Controls[i]).Smooth:=ScrollBar2.Position

end;

end.
