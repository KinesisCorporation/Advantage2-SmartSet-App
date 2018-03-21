unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls,
  RichMemo;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    FontDialog1: TFontDialog;
    OpenDialog1: TOpenDialog;
    RichMemo1: TRichMemo;
    SaveDialog1: TSaveDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RichMemo1Change(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 
  
var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  fp   :  TFontParams;
begin
  Caption := Format('sel start %d,  sel length %d', [RichMemo1.SelStart, RichMemo1.SelLength]);
  RichMemo1.GetTextAttributes(RichMemo1.SelStart, fp);
  fp.Color := clRed;
  fp.Style := [fsBold];
  RichMemo1.SetTextAttributes(RichMemo1.SelStart, RichMemo1.SelLength, fp);
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  prm : TFontParams;
begin
  if RichMemo1.GetTextAttributes( RichMemo1.SelStart, prm) then begin
    RichMemo1.Lines.Add('name   '+ prm.Name);
    RichMemo1.Lines.Add('size   '+ IntToStr(prm.Size));
    RichMemo1.Lines.Add('color  '+ IntToHex(Integer(prm.Color), 8));
    RichMemo1.Lines.Add('style  '+ IntToHex(Integer(prm.Style), 8));
  end else
    RichMemo1.Lines.Add('failed');
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if Fontdialog1.Execute then begin
    RichMemo1.SetTextAttributes(
      RichMemo1.SelStart, RichMemo1.SelLength, FontDialog1.Font);
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  fs : TFileStream;
begin
  if SaveDialog1.Execute then begin
    fs := nil;
    try
      fs := TFileStream.Create( Utf8ToAnsi(SaveDialog1.FileName), fmCreate);
      RichMemo1.SaveRichText(fs);
    except
    end;
    fs.Free;
  end;
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  fs : TFileStream;
begin
  if OpenDialog1.Execute then begin
    fs := nil;
    try
      // Utf8ToAnsi is required for windows
      fs := TFileStream.Create(Utf8ToAnsi(OpenDialog1.FileName), fmOpenRead or fmShareDenyNone);
      RichMemo1.LoadRichText(fs);
    except
    end;
    fs.Free;
  end;
end;

procedure TForm1.Button6Click(Sender: TObject);
var
  ofs, len  : Integer;
begin
  RichMemo1.GetStyleRange( RichMemo1.SelStart, ofs, len );
  if (ofs = RichMEmo1.SelStart) and (len = RichMemo1.SelLength) then begin
    ofs := ofs + len;
    RichMemo1.GetStyleRange( ofs, ofs, len );
  end;
  RichMemo1.SelStart := ofs;
  RichMemo1.SelLength := len;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
end;

procedure TForm1.RichMemo1Change(Sender: TObject);
begin
  Caption := Caption + '.';
  if length(CAption)>20 then Caption:='';
end;

initialization
  {$I unit1.lrs}

end.

