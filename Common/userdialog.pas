unit UserDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, u_const;

type

  { TFormUserDialog }

  TFormUserDialog = class(TForm)
    chkCheckBox: TCheckBox;
    ilDialog: TImageList;
    imgDialog: TImage;
    lblCheckBox: TLabel;
    lblMessage: TLabel;
    pnlPadRight: TPanel;
    pnlImage: TPanel;
    pnlBottom: TPanel;
    pnlClient: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure lblCheckBoxClick(Sender: TObject);
  private
    { private declarations }
    aButtons: array of TBitBtn;
    procedure AddButton(btnCaption: String; btnType: TBitBtnKind; btnWidth: integer = 90; onBtnClick: TNotifyEvent = nil);
  public
    { public declarations }
  end;

var
  FormUserDialog: TFormUserDialog;
  LastDialog: TFormUserDialog;
  function ShowDialog(Caption, Message: string; dlgType: TMsgDlgTypeApp; dlgButtons: TMsgDlgButtons; dialogHeight: integer = DEFAULT_DIAG_HEIGHT; backColor: TColor = clWhite; fontColor: TColor = clDefault; customButtons: TCustomButtons = nil; checkBoxCaption: string = ''): integer;
  procedure CloseDialog;

implementation

{$R *.lfm}

//Function to call a dialog box
function ShowDialog(Caption, Message: string; dlgType: TMsgDlgTypeApp; dlgButtons: TMsgDlgButtons; dialogHeight: integer = DEFAULT_DIAG_HEIGHT; backColor: TColor = clWhite; fontColor: TColor = clDefault; customButtons: TCustomButtons = nil; checkBoxCaption: string = ''): integer;
var
  i: integer;
  formDialog: TFormUserDialog;
  modalResult: integer;
const
  WARNING_ICON = 0;
  ERROR_ICON = 1;
  INFO_ICON = 2;
  CONFIRM_ICON = 3;
  FSEDGE_ICON = 4;
  FSPRO_ICON = 5;
begin
  //Close the dialog if opened
  //if FormUserDialog <> nil then
  //  FreeAndNil(FormUserDialog);

  //Creates the dialog form
  Application.CreateForm(TFormUserDialog, formDialog);
  LastDialog := formDialog;

  //Loads Caption text
  formDialog.Caption := Caption;
  formDialog.lblMessage.Caption := Message;
  formDialog.Height := dialogHeight;

  //Loads colors
  formDialog.Color := backColor;
  formDialog.Font.Color := fontColor;
  formDialog.lblMessage.Color := backColor;
  formDialog.lblMessage.Font.Color := fontColor;
  formDialog.pnlBottom.Color := backColor;
  formDialog.lblCheckBox.Font.Color := fontColor;

  {$ifdef Darwin}
  formDialog.chkCheckBox.Top := formDialog.chkCheckBox.Top - 5;
  formDialog.lblCheckBox.Left := formDialog.lblCheckBox.Left + 5;
  {$endif}

  //Loads the image for dialog type
  if dlgType in [mtConfirmation] then
    formDialog.ilDialog.GetBitmap(CONFIRM_ICON, formDialog.imgDialog.Picture.Bitmap)
  else if dlgType in [mtWarning] then
    formDialog.ilDialog.GetBitmap(WARNING_ICON, formDialog.imgDialog.Picture.Bitmap)
  else if dlgType in [mtError] then
    formDialog.ilDialog.GetBitmap(ERROR_ICON, formDialog.imgDialog.Picture.Bitmap)
  else if dlgType in [mtInformation] then
    formDialog.ilDialog.GetBitmap(INFO_ICON, formDialog.imgDialog.Picture.Bitmap)
  else if dlgType in [mtFSEdge] then
    formDialog.ilDialog.GetBitmap(FSEDGE_ICON, formDialog.imgDialog.Picture.Bitmap)
  else if dlgType in [mtFSPro] then
    formDialog.ilDialog.GetBitmap(FSPRO_ICON, formDialog.imgDialog.Picture.Bitmap)
  else
    formDialog.ilDialog.GetBitmap(CONFIRM_ICON, formDialog.imgDialog.Picture.Bitmap);

  //Adds buttons according to options
  if mbYes in dlgButtons then
    formDialog.AddButton('Yes', bkYes);
  if mbNo in dlgButtons then
    formDialog.AddButton('No', bkNo);
  if mbOK in dlgButtons then
    formDialog.AddButton('OK', bkOK);
  if mbCancel in dlgButtons then
    formDialog.AddButton('Cancel', bkCancel);

  if (Length(customButtons) > 0) then
  begin
    for i := 0 to Length(customButtons) - 1 do
      formDialog.AddButton(customButtons[i].Caption, customButtons[i].Kind, customButtons[i].Width, customButtons[i].OnClick);
  end;

  if (checkBoxCaption <> '') then
  begin
    formDialog.lblCheckBox.Caption := checkBoxCaption;
    formDialog.lblCheckBox.Visible := true;
    formDialog.chkCheckBox.Caption := '';
    formDialog.chkCheckBox.Visible := true;

    if (Length(customButtons) > 2) then
    begin
      formDialog.Height := formDialog.Height + 25;
      formDialog.pnlBottom.Height := formDialog.pnlBottom.Height + 25;
      formDialog.chkCheckBox.Top := formDialog.chkCheckBox.Top + 30;
      formDialog.lblCheckBox.Top := formDialog.lblCheckBox.Top + 30;
    end;
  end
  else
  begin
    formDialog.lblCheckBox.Visible := false;
    formDialog.chkCheckBox.Visible := false;
  end;

  //Shows dialog and returns value
  modalResult := formDialog.ShowModal;
  if (checkBoxCaption <> '') then
  begin
    if (formDialog.chkCheckBox.Checked) then
      result := modalResult + DISABLE_NOTIF
    else
      result := modalResult;
  end
  else
    result := modalResult;
end;

procedure CloseDialog;
begin
  if LastDialog <> nil then
  begin
    LastDialog.Close;
  end;
end;

{ TFormUserDialog }

procedure TFormUserDialog.FormCreate(Sender: TObject);
begin
  //Windows
  {$ifdef Win32}
  SetFont(self, 'Segoe UI');
  {$endif}
    //MacOS
  {$ifdef Darwin}
  SetFont(self, 'Helvetica');
  {$endif}
end;

procedure TFormUserDialog.lblCheckBoxClick(Sender: TObject);
begin
  chkCheckBox.Checked := not chkCheckBox.Checked;
end;

procedure TFormUserDialog.AddButton(btnCaption: String; btnType: TBitBtnKind; btnWidth: integer = 90; onBtnClick: TNotifyEvent = nil);
var
  i, idx:integer;
  button: TBitBtn;
begin
  idx := Length(aButtons) + 1;
  button := TBitBtn.Create(pnlBottom);
  button.Parent := pnlBottom;
  button.Caption := btnCaption;
  button.Kind := btnType;
  button.Name := 'btn' + IntToStr(idx);
  button.Width := btnWidth;
  button.ParentFont := false;
  button.Font.Color := clDefault;
  if (onBtnClick <> nil) then
    button.OnClick := onBtnClick;

  SetLength(aButtons, idx);
  aButtons[idx - 1] := button;

  for i := 0 to idx - 1 do
  begin
    aButtons[i].Left := pnlBottom.Width - ((idx - i) * btnWidth) - (10 * (idx - i));
  end;
end;

end.

