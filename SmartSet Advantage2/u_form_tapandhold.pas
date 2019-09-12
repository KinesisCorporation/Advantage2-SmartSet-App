unit u_form_tapandhold;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, HSSpeedButton, LineObj, u_const, UserDialog, lcltype,
  u_keys;

type

  { TFormTapAndHold }

  TFormTapAndHold = class(TForm)
    btnCancel: THSSpeedButton;
    btnDone: THSSpeedButton;
    eHoldAction: TEdit;
    eTapAction: TEdit;
    eTimingDelay: TEdit;
    lblTapAction: TLabel;
    lblHoldAction: TLabel;
    lblDelay: TLabel;
    procedure btnCancelClick(Sender: TObject);
    procedure btnDoneClick(Sender: TObject);
    procedure eHoldActionKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure eTapActionKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure eTimingDelayChange(Sender: TObject);
    procedure eTimingDelayKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    function Validate: boolean;
  public
    tapAction: word;
    holdAction: word;
    timingDelay: integer;
    procedure SetKeyPress(key: word; edit: TEdit = nil);
  end;

var
  FormTapAndHold: TFormTapAndHold;
  function ShowTapAndHold(aTapAction: TKey; aHoldAction: TKey; iTimingDelay: integer; backColor: TColor; fontColor: TColor): boolean;

implementation

uses u_form_main;

{$R *.lfm}

function MainForm: TFormMain;
begin
  result := (Application.MainForm as TFormMain);
end;

function ShowTapAndHold(aTapAction: TKey; aHoldAction: TKey; iTimingDelay: integer; backColor: TColor; fontColor: TColor): boolean;
const
  DefaultDelay = 250;
begin
  result := false;
  //Close the dialog if opened
  if FormTapAndHold <> nil then
    FreeAndNil(FormTapAndHold);

  //Creates the dialog form
  Application.CreateForm(TFormTapAndHold, FormTapAndHold);

  FormTapAndHold.Color := backColor;
  FormTapAndHold.Font.Color := fontColor;
  FormTapAndHold.lblTapAction.Color := backColor;
  FormTapAndHold.lblTapAction.Font.Color := fontColor;
  FormTapAndHold.lblHoldAction.Color := backColor;
  FormTapAndHold.lblHoldAction.Font.Color := fontColor;
  FormTapAndHold.lblDelay.Color := backColor;
  FormTapAndHold.lblDelay.Font.Color := fontColor;

  //FormTapAndHold.timingDelay := iTimingDelay;
  if (iTimingDelay <= 0) then
    iTimingDelay := DefaultDelay;

  FormTapAndHold.eTimingDelay.Text := IntToStr(iTimingDelay);
  if (aTapAction <> nil) then
    FormTapAndHold.SetKeyPress(aTapAction.Key, FormTapAndHold.eTapAction)
  else
    FormTapAndHold.tapAction := 0;
  if (aHoldAction <> nil) then
    FormTapAndHold.SetKeyPress(aHoldAction.Key, FormTapAndHold.eHoldAction)
   else
    FormTapAndHold.holdAction := 0;

  //Shows dialog and returns value
  if FormTapAndHold.ShowModal = mrOK then
  begin
    result := true;
  end;
end;

{ TFormTapAndHold }

procedure TFormTapAndHold.FormCreate(Sender: TObject);
begin
  inherited;
end;

procedure TFormTapAndHold.FormShow(Sender: TObject);
begin
  eTapAction.SetFocus;
end;

procedure TFormTapAndHold.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

function TFormTapAndHold.Validate: boolean;
begin
  result := false;
  if (timingDelay < MIN_TIMING_DELAY) or (timingDelay > MAX_TIMING_DELAY) then
    ShowDialog('Tap and Hold Action', 'Please select a timing delay between 1ms and 999ms. To achieve a longer delay, insert multiple delays back-to-back.', mtError, [mbOK], DEFAULT_DIAG_HEIGHT)
  else if (tapAction <= 0) then
    ShowDialog('Tap and Hold Action', 'Plese select a Tap Action', mtError, [mbOK], DEFAULT_DIAG_HEIGHT)
  else if (holdAction <= 0) then
    ShowDialog('Tap and Hold Action', 'Plese select a Hold Action', mtError, [mbOK], DEFAULT_DIAG_HEIGHT)
  else
    result := true;
end;

procedure TFormTapAndHold.btnDoneClick(Sender: TObject);
begin
  if (Validate) then
    ModalResult := mrOK;
end;

procedure TFormTapAndHold.eHoldActionKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  {$ifdef Darwin}
  SetKeyPress(key, eHoldAction);
  {$endif}
  key := 0;
end;

procedure TFormTapAndHold.eTapActionKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  {$ifdef Darwin}
  SetKeyPress(key, eTapAction);
  {$endif}
  key := 0;
end;

procedure TFormTapAndHold.eTimingDelayChange(Sender: TObject);
begin
  timingDelay := ConvertToInt(FormTapAndHold.eTimingDelay.Text);
end;

procedure TFormTapAndHold.eTimingDelayKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_UP) or (Key = VK_DOWN) then
  begin
    timingDelay := ConvertToInt(FormTapAndHold.eTimingDelay.Text, 0);
    if (Key = VK_UP) then
    begin
      if (timingDelay < MAX_TIMING_DELAY) then
        inc(timingDelay)
      else
        timingDelay := MAX_TIMING_DELAY;
      FormTapAndHold.eTimingDelay.Text := IntToStr(timingDelay);
    end
    else if (Key = VK_DOWN) then
    begin
      if (timingDelay > MIN_TIMING_DELAY) then
        dec(timingDelay)
      else
        timingDelay := MIN_TIMING_DELAY;
      FormTapAndHold.eTimingDelay.Text := IntToStr(timingDelay);
    end;
  end
  else if ((key < VK_0) or (key > VK_9)) and (key <> VK_BACK) then
   key := 0;
end;

procedure TFormTapAndHold.SetKeyPress(key: word; edit: TEdit);
var
  aKey: TKey;
begin
  if (key <> 0) then
  begin
    aKey := MainForm.keyService.GetKeyConfig(key);
    if (aKey <> nil) then
    begin
      if (edit = eTapAction) or (eTapAction.Focused) then
      begin
        eTapAction.Text := aKey.OtherDisplayText;
        tapAction := key;
      end
      else if (edit = eHoldAction) or (eHoldAction.Focused) then
      begin
        eHoldAction.Text := aKey.OtherDisplayText;
        holdAction := key;
      end;
    end;
    FreeAndNil(aKey);
  end;
end;

end.

