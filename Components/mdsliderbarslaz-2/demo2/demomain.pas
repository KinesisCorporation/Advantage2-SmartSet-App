unit DemoMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, SliderBars;

type

  { TDemoMainForm }

  TDemoMainForm = class(TForm)
    Button1: TButton;
    ColorBar1: TColorBar;
    ColorBar2: TColorBar;
    procedure Button1Click(Sender: TObject);
    procedure ColorBar1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  DemoMainForm: TDemoMainForm;

implementation

{$R *.lfm}

uses
  pickerdlg;

{ TDemoMainForm }

procedure TDemoMainForm.ColorBar1Click(Sender: TObject);
begin
  with Sender as TColorBar do begin
    with TPickerForm.Create(self) do try
      InitialColor := StartColor;
      if Sender = ColorBar1 then
        Caption := 'Select foreground color'
      else
        Caption := 'Select background color';
      if showmodal = mrOk then begin
        StartColor := SelectedColor;
        StopColor := SelectedColor;
      end;
    finally
      free;
    end;
  end;
end;

procedure TDemoMainForm.Button1Click(Sender: TObject);
begin
  close;
end;


end.

