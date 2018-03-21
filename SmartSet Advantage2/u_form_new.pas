unit u_form_new;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  HSSpeedButton, userdialog, u_const;

type

  { TFormNew }

  TFormNew = class(TForm)
    btnCreate: THSSpeedButton;
    btnCancel: THSSpeedButton;
    cboLayout: TComboBox;
    chkLoadAfterClose: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    lblQwerty: TLabel;
    lblDvorak: TLabel;
    rgQwerty: TRadioButton;
    rgDvorak: TRadioButton;
    procedure btnCancelClick(Sender: TObject);
    procedure btnCreateClick(Sender: TObject);
    procedure cboLayoutSelect(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lblDvorakClick(Sender: TObject);
    procedure lblQwertyClick(Sender: TObject);
    procedure radioChange(Sender: TObject);
    procedure radioClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormNew: TFormNew;
  function ShowNewFile(backColor: TColor; fontColor: TColor; allowEditSettings: boolean; var layoutType: string; var layoutFilePos: string; var loadAfterClose: boolean): string;

implementation

{$R *.lfm}

function ShowNewFile(backColor: TColor; fontColor: TColor; allowEditSettings: boolean; var layoutType: string; var layoutFilePos: string; var loadAfterClose: boolean): string;
begin
  layoutFilePos := '';

  //Close the dialog if opened
  if FormNew <> nil then
    FreeAndNil(FormNew);

  //Creates the dialog form
  Application.CreateForm(TFormNew, FormNew);

  FormNew.Color := backColor;
  FormNew.Font.Color := fontColor;
  FormNew.lblQwerty.Color := backColor;
  FormNew.lblQwerty.Font.Color := fontColor;
  FormNew.lblDvorak.Color := backColor;
  FormNew.lblDvorak.Font.Color := fontColor;
  FormNew.chkLoadAfterClose.Enabled := allowEditSettings;
  //FormNew.cboLayout.Enabled := false;

  //Shows dialog and returns value
  if FormNew.ShowModal = mrOK then
  begin
    layoutFilePos := FormNew.cboLayout.Text;
    loadAfterClose := FormNew.chkLoadAfterClose.Checked;
    if (FormNew.rgQwerty.Checked) then
    begin
      layoutType := 'Qwerty';
      result := FormNew.cboLayout.Text + '_qwerty.txt'
    end
    else
    begin
      layoutType := 'Dvorak';
      result := FormNew.cboLayout.Text + '_dvorak.txt';
    end;
  end;
end;

{ TFormNew }

procedure TFormNew.lblDvorakClick(Sender: TObject);
begin
  rgDvorak.Checked := true;
end;

procedure TFormNew.FormCreate(Sender: TObject);
begin
  SetFont(self, 'Segoe UI');
  rgQwerty.Checked := true;
end;

procedure TFormNew.btnCreateClick(Sender: TObject);
begin
  if (Trim(cboLayout.Text) = '') then
  begin
    ShowDialog('New Layout', 'You must select a layout position a-z or 0 to 9', mtError, [mbOK], DEFAULT_DIAG_HEIGHT,
          clWhite, clDefault);
    cboLayout.SetFocus;
  end
  else
    ModalResult := mrOK;
end;

procedure TFormNew.cboLayoutSelect(Sender: TObject);
begin
  //For Mac to close combo box after selection
  rgQwerty.SetFocus;
end;

procedure TFormNew.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TFormNew.lblQwertyClick(Sender: TObject);
begin
  rgQwerty.Checked := true;
end;

procedure TFormNew.radioChange(Sender: TObject);
begin
  cboLayout.Enabled := true;
end;

procedure TFormNew.radioClick(Sender: TObject);
begin
  if (self.Visible) then
  begin
    cboLayout.SetFocus;
    cboLayout.DroppedDown := true;
  end;
end;

end.

