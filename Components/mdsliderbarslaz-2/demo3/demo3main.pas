unit demo3main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, SliderBars;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    ProgressBar1: TProgressBar;
    ProgressBar2: TProgressBar;
    TrackBar1: TTrackBar;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SliderBar1Change(Sender: TObject);
    procedure SliderBar1Paint(Sender: TObject; aRect: TRect);
    procedure SliderBar1Resize(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    { private declarations }
  public
    SliderBar1: TSliderBar;
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.SliderBar1Paint(Sender: TObject; aRect: TRect);
var
  wd: integer;
  x: integer;
  i: integer;
  tw: integer;
  s: string;
begin
  wd := (aRect.Right - aRect.Left - 11) div 10;
  x := 0;
  with (Sender as TSliderBar), canvas do begin
    pen.color := clBlack;
    pen.width := 1;
    pen.Style := psSolid;
    brush.Color := clNone;
    brush.Style := bsClear;
    for i := 0 to 10 do begin
      moveto(aRect.left + x, aRect.Top+5);
      lineto(aRect.Left + x, aRect.Bottom-4);
      if i = Value then begin
        if i < 6 then
          TextOut(aRect.Left + x + 2, aRect.Top+3, inttostr(i));
        if i > 5 then begin
          s := inttostr(i);
          tw := TextWidth(s);
          TextOut(aRect.Left + x - 2 - tw, aRect.Top+3, s);
        end;
      end;
      x := x + wd + 1;
    end;
  end;
end;

procedure TForm1.SliderBar1Change(Sender: TObject);
begin
  ProgressBar1.Position := SliderBar1.value;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  close
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SliderBar1 := TSliderBar.create(self);
  with SliderBar1 do begin
    parent := self;
    Left := 30;
    Top := 104;
    Width := 220;
    TabOrder := 2;
    MaxValue := 10;
    OnChange := @SliderBar1Change;
    OnPaint := @SliderBar1Paint;
    OnResize := @SliderBar1Resize;
  end
end;

procedure TForm1.SliderBar1Resize(Sender: TObject);
var
  wd, preferedWidth: integer;
begin
  with SliderBar1 do begin
    wd := (width - 4 - 11) div 10;
    preferedWidth := 10*wd + 11 + 4;
    if width <> preferedWidth then begin
      setbounds(Left, Top, preferedWidth, height);
    end;
  end;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  ProgressBar2.Position := TrackBar1.Position;
end;

end.

