unit u_form_about;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, VersionSupport, lclintf, HSSpeedButton, u_const, UserDialog;

type

  { TFormAbout }

  TFormAbout = class(TForm)
    btnReadManual: THSSpeedButton;
    btnWatchTutorial: THSSpeedButton;
    Label5: TLabel;
    lblEmail: TLabel;
    lblTitle: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lblFirmware: TLabel;
    lblWebsite: TLabel;
    lblVersion: TLabel;
    lblVersion1: TLabel;
    lblVersion2: TLabel;
    procedure bOkClick(Sender: TObject);
    procedure btnReadManualClick(Sender: TObject);
    procedure btnWatchTutorialClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure lblEmailClick(Sender: TObject);
    procedure lblWebsiteClick(Sender: TObject);
  private
    { private declarations }
    function FindFirstNumberPos(value: string): integer;
  public
    { public declarations }
    procedure SetFirmwareVersion(firmwareVersion: string);
  end;

var
  FormAbout: TFormAbout;

implementation

{$R *.lfm}

{ TFormAbout }

procedure TFormAbout.bOkClick(Sender: TObject);
begin
  Close;
end;

procedure TFormAbout.btnReadManualClick(Sender: TObject);
begin
  OpenUrl(ADV2_MANUAL);
//var
//  filePath: string;
//begin
//  filePath := GApplicationPath + '\' + USER_MANUAL_ADV2;
//  {$ifdef Darwin}filePath := GApplicationPath + '/' + USER_MANUAL_ADV2;{$endif}
//
//  if FileExists(filePath) then
//    OpenDocument(filePath)
//  else
//    ShowDialog('Help file', 'Help file not found!', mtError, [mbOK], DEFAULT_DIAG_HEIGHT, KINESIS_DARK_GRAY_FS, clWhite);
end;

procedure TFormAbout.btnWatchTutorialClick(Sender: TObject);
begin
  OpenUrl(ADV2_TUTORIAL);
end;

procedure TFormAbout.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if CloseAction = caFree then
  begin
    FormAbout := nil;
  end;
end;

procedure TFormAbout.FormCreate(Sender: TObject);
begin
  SetFont(self, 'Segoe UI');
  lblTitle.Caption := GApplicationName;
  lblVersion.Caption := 'Version : ' + GetFileVersion;
end;

procedure TFormAbout.lblEmailClick(Sender: TObject);
begin
  OpenUrl('mailto:tech@kinesis.com');
end;

procedure TFormAbout.lblWebsiteClick(Sender: TObject);
begin
  if Copy(lblWebsite.Caption, 1, 4) <> 'http' then //Add HTTP for MacOS
    OpenUrl('http://' + lblWebsite.Caption)
  else
    OpenUrl(lblWebsite.Caption);
end;

function TFormAbout.FindFirstNumberPos(value: string): integer;
var
  i: integer;
  tempInt, Code: integer;
begin
  result := 0;
  for i := 0 to Length(value) do
  begin
    val(value[i], tempInt, Code);
    if Code = 0 then
    begin
      result := i;
      Code := tempInt; //to remove Hint
      break;
    end;
  end;
end;

procedure TFormAbout.SetFirmwareVersion(firmwareVersion: string);
begin
  if (firmwareVersion <> '') then
    lblFirmware.Caption := 'Firmware version : ' + firmwareVersion
  else
    lblFirmware.Caption := 'Firmware version : not found';
end;

end.

