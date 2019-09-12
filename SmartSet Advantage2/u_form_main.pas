unit u_form_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  lcltype, Menus, ExtCtrls, Buttons, lclintf, ComCtrls, u_const, u_key_service,
  u_key_layer, u_file_service, PanelBtn, LabelBox, LineObj, ueled, uEKnob,
  ECSwitch, ECSlider, HSSpeedButton, RichMemo, u_keys, userdialog, contnrs,
  u_form_about, u_form_new, u_form_tapandhold
  {$ifdef Win32},Windows, JwaWinUser{$endif}
  {$ifdef Darwin}, MacOSAll, CarbonUtils, CarbonDef, CarbonProc{$endif};

type

  { TFormMain }

  TFormMain = class(TForm)
    bLAltMacro: THSSpeedButton;
    bLCtrlMacro: THSSpeedButton;
    bLShiftMacro: THSSpeedButton;
    bRAltMacro: THSSpeedButton;
    bRCtrlMacro: THSSpeedButton;
    bRShiftMacro: THSSpeedButton;
    btnBackspace: THSSpeedButton;
    btnCancelKey: THSSpeedButton;
    btnCancelMacro: THSSpeedButton;
    btnClear: THSSpeedButton;
    btnClose: THSSpeedButton;
    btnCopy: THSSpeedButton;
    btnDoneKey: THSSpeedButton;
    btnDoneMacro: THSSpeedButton;
    btnHelpIcon: THSSpeedButton;
    btnMaximize: THSSpeedButton;
    btnMinimize: THSSpeedButton;
    btnPaste: THSSpeedButton;
    btnResetKey: THSSpeedButton;
    btnResetLayer: THSSpeedButton;
    btnResetLayout: THSSpeedButton;
    btnSpecialActionsMacro: THSSpeedButton;
    btnSpecialActionsRemap: THSSpeedButton;
    btnEsc1: TPanelBtn;
    btnF1: TPanelBtn;
    btnF10: TPanelBtn;
    btnF11: TPanelBtn;
    btnF12: TPanelBtn;
    btnF13: TPanelBtn;
    btnF14: TPanelBtn;
    btnF15: TPanelBtn;
    btnF16: TPanelBtn;
    btnF17: TPanelBtn;
    btnF18: TPanelBtn;
    btnF19: TPanelBtn;
    btnF2: TPanelBtn;
    btnF20: TPanelBtn;
    btnF21: TPanelBtn;
    btnF22: TPanelBtn;
    btnF23: TPanelBtn;
    btnF24: TPanelBtn;
    btnF25: TPanelBtn;
    btnF26: TPanelBtn;
    btnF27: TPanelBtn;
    btnF28: TPanelBtn;
    btnF29: TPanelBtn;
    btnF3: TPanelBtn;
    btnF30: TPanelBtn;
    btnF31: TPanelBtn;
    btnF32: TPanelBtn;
    btnF33: TPanelBtn;
    btnF34: TPanelBtn;
    btnF35: TPanelBtn;
    btnF36: TPanelBtn;
    btnF37: TPanelBtn;
    btnF38: TPanelBtn;
    btnF39: TPanelBtn;
    btnF4: TPanelBtn;
    btnF40: TPanelBtn;
    btnF41: TPanelBtn;
    btnF42: TPanelBtn;
    btnF43: TPanelBtn;
    btnF44: TPanelBtn;
    btnF45: TPanelBtn;
    btnF46: TPanelBtn;
    btnF47: TPanelBtn;
    btnF48: TPanelBtn;
    btnF49: TPanelBtn;
    btnF5: TPanelBtn;
    btnF50: TPanelBtn;
    btnF51: TPanelBtn;
    btnF52: TPanelBtn;
    btnF53: TPanelBtn;
    btnF54: TPanelBtn;
    btnF55: TPanelBtn;
    btnF56: TPanelBtn;
    btnF57: TPanelBtn;
    btnF58: TPanelBtn;
    btnF59: TPanelBtn;
    btnF6: TPanelBtn;
    btnF60: TPanelBtn;
    btnF61: TPanelBtn;
    btnF62: TPanelBtn;
    btnF63: TPanelBtn;
    btnF64: TPanelBtn;
    btnLCtrl: TPanelBtn;
    btnLAlt: TPanelBtn;
    btnLoad: THSSpeedButton;
    btnNew: THSSpeedButton;
    btnRWin: TPanelBtn;
    btnRCtrl: TPanelBtn;
    btnF69: TPanelBtn;
    btnF7: TPanelBtn;
    btnF71: TPanelBtn;
    btnF72: TPanelBtn;
    btnF73: TPanelBtn;
    btnF74: TPanelBtn;
    btnF75: TPanelBtn;
    btnF76: TPanelBtn;
    btnF77: TPanelBtn;
    btnF78: TPanelBtn;
    btnF79: TPanelBtn;
    btnF8: TPanelBtn;
    btnF80: TPanelBtn;
    btnF81: TPanelBtn;
    btnF82: TPanelBtn;
    btnF83: TPanelBtn;
    btnF84: TPanelBtn;
    btnF85: TPanelBtn;
    btnF9: TPanelBtn;
    btnLeftPedal: TPanelBtn;
    btnMiddlePedal: TPanelBtn;
    btnRightPedal: TPanelBtn;
    btnSave: THSSpeedButton;
    gbPedals: TGroupBox;
    gbThumbKeys: TGroupBox;
    imageList: TImageList;
    imgLogo: TImage;
    imgKinesis: TImage;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lblDisplaying: TStaticText;
    lblKeyClicks: TLabel;
    lblKeyTones: TLabel;
    lblMacro1: TLabel;
    lblMacro2: TLabel;
    lblMacro3: TLabel;
    lblPlaybackSpeed: TStaticText;
    lblPS2: TStaticText;
    lblPS4: TStaticText;
    lblPS6: TStaticText;
    lblPS8: TStaticText;
    lblPSGlobal: TStaticText;
    lblThumbKeys1: TStaticText;
    lblVDrive: TLabel;
    lblGlobal0: TStaticText;
    lblGlobal17: TStaticText;
    lblGlobal18: TStaticText;
    lblGlobal5: TStaticText;
    lblGlobal9: TStaticText;
    lblGlobalMacroSpeed: TLabel;
    lblProgramming: TLabel;
    lblProgramming1: TLabel;
    lblMenu: TLabel;
    lblLayoutFile: TLabel;
    lblRemap: TLabel;
    lblLayer: TLabel;
    lblMacro: TLabel;
    lblSettings: TLabel;
    lblStatus1: TStaticText;
    lblStatus2: TStaticText;
    lblStatus3: TStaticText;
    lblStatus5: TStaticText;
    lblStatus6: TStaticText;
    lblStatusReport: TLabel;
    lblTitle: TLabel;
    btnEsc: TPanelBtn;
    MenuItem1: TMenuItem;
    miMeh: TMenuItem;
    miHyper: TMenuItem;
    miTapHold: TMenuItem;
    miBackM: TMenuItem;
    miForwardM: TMenuItem;
    miHomeM: TMenuItem;
    miNewTabM: TMenuItem;
    miSwitchTabsM: TMenuItem;
    mnuWebShortcutsMacro: TMenuItem;
    miPasteM: TMenuItem;
    miCopyM: TMenuItem;
    miCutM: TMenuItem;
    miRedoM: TMenuItem;
    miUndoM: TMenuItem;
    miSelectAllM: TMenuItem;
    miDesktopM: TMenuItem;
    miCtrlAltDelM: TMenuItem;
    miRightMouseM: TMenuItem;
    miMiddleMouseM: TMenuItem;
    miLeftMouseM: TMenuItem;
    mnuMouseActionsMacro: TMenuItem;
    mnuInternalDelays: TMenuItem;
    mnuFunctionKeysMacro: TMenuItem;
    mnuToolsMacro: TMenuItem;
    mnuMultiMacro: TMenuItem;
    mnuCommon: TMenuItem;
    miRightMouse: TMenuItem;
    miMiddleMouse: TMenuItem;
    miLeftMouse: TMenuItem;
    mnuMouse: TMenuItem;
    mnuKeypad: TMenuItem;
    mnuFunctionKeys: TMenuItem;
    mnuMultimedia: TMenuItem;
    mnuTools: TMenuItem;
    miWinM: TMenuItem;
    miLongDelayM: TMenuItem;
    miShortDelayM: TMenuItem;
    miLastAppM: TMenuItem;
    miCalculatorM: TMenuItem;
    miF24: TMenuItem;
    miF23: TMenuItem;
    miF22: TMenuItem;
    miF16: TMenuItem;
    miF21: TMenuItem;
    miF20: TMenuItem;
    miF19: TMenuItem;
    miF18: TMenuItem;
    miF17: TMenuItem;
    miF13M: TMenuItem;
    miF14M: TMenuItem;
    miF15M: TMenuItem;
    miF16M: TMenuItem;
    miF17M: TMenuItem;
    miF18M: TMenuItem;
    miF19M: TMenuItem;
    miF20M: TMenuItem;
    miF21M: TMenuItem;
    miF22M: TMenuItem;
    miF23M: TMenuItem;
    miF24M: TMenuItem;
    miInternationalKeyM: TMenuItem;
    miMenuM: TMenuItem;
    miMuteM: TMenuItem;
    miNextTrackM: TMenuItem;
    miPlayPauseM: TMenuItem;
    miPreviousTrackM: TMenuItem;
    miShutdownM: TMenuItem;
    miVolumeDownM: TMenuItem;
    miVolumeUp: TMenuItem;
    miF15: TMenuItem;
    miF14: TMenuItem;
    miF13: TMenuItem;
    miPreviousTrack: TMenuItem;
    miMute: TMenuItem;
    miVolumeDown: TMenuItem;
    miShutdown: TMenuItem;
    miPlayPause: TMenuItem;
    miNextTrack: TMenuItem;
    miNull: TMenuItem;
    miCalculator: TMenuItem;
    miInternationalKey: TMenuItem;
    miMenu: TMenuItem;
    miKeypadShift: TMenuItem;
    miKeypadToggle: TMenuItem;
    miVolumeUpM: TMenuItem;
    OpenDialog: TOpenDialog;
    Panel1: TPanel;
    SaveDialog: TSaveDialog;
    pmTokensKeys: TPopupMenu;
    pmTokensMacros: TPopupMenu;
    shpMenu: TShape;
    memoMacro: TRichMemo;
    pnlMacro: TPanel;
    rgMacro1: TRadioButton;
    rgMacro2: TRadioButton;
    rgMacro3: TRadioButton;
    shpLine: TLineObj;
    pnlMain: TPanel;
    pnlBotOptions: TPanel;
    pnlKeysLeft: TPanel;
    pnlBody: TPanel;
    pnlBot: TPanel;
    pnlKb: TPanel;
    pnlKeys: TPanel;
    pnlLayers: TPanel;
    pnlMenu: TPanel;
    pnlTitle: TPanel;
    pnlTopBody: TPanel;
    shpKeys: TShape;
    shpSettings: TShape;
    slMacroSpeed: TECSlider;
    slPlaybackSpeed: TECSlider;
    slStatusReport: TECSlider;
    lblThumbKeys: TStaticText;
    StaticText3: TStaticText;
    swKeyClicks: TECSwitch;
    swKeyTones: TECSwitch;
    swAutoVDrive: TECSwitch;
    swLayerSwitch: TECSwitch;
    textMacroInput: TStaticText;
    procedure bCoTriggerClick(Sender: TObject);
    procedure btnActivateMacroClick(Sender: TObject);
    procedure btnSpecialActionsMacroClick(Sender: TObject);
    procedure btnBackspaceClick(Sender: TObject);
    procedure btnCancelKeyClick(Sender: TObject);
    procedure btnCancelMacroClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
    procedure btnDoneKeyClick(Sender: TObject);
    procedure btnDoneMacroClick(Sender: TObject);
    procedure btnHelpIconClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnMaximizeClick(Sender: TObject);
    procedure btnMinimizeClick(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure btnPasteClick(Sender: TObject);
    procedure btnResetKeyClick(Sender: TObject);
    procedure btnResetLayerClick(Sender: TObject);
    procedure btnResetLayoutClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnSpecialActionsRemapClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure imgKinesisClick(Sender: TObject);
    procedure memoMacroMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure miTokenMacroClick(Sender: TObject);
    procedure miTokenKeyClick(Sender: TObject);
    procedure pmTokensKeysPopup(Sender: TObject);
    procedure pmTokensMacrosPopup(Sender: TObject);
    procedure pnlMacroClick(Sender: TObject);
    procedure pnlTitleMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pnlTitleMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure rgMacroClick(Sender: TObject);
    procedure slMacroSpeedChange(Sender: TObject);
    procedure slPlaybackSpeedChange(Sender: TObject);
    procedure slStatusReportChange(Sender: TObject);
    procedure swAutoVDriveChange(Sender: TObject);
    procedure swAutoVDriveClick(Sender: TObject);
    procedure swKeyClicksChange(Sender: TObject);
    procedure swKeyTonesChange(Sender: TObject);
    procedure swLayerSwitchClick(Sender: TObject);
    procedure tbStatusReportChange(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure PnlButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure textMacroInputClick(Sender: TObject);
  private
    { private declarations }
    activePnlBtn: TPanelBtn;
    activeKbKey: TKBKey;
    activeLayer: TKBLayer;
    MacroMode: boolean;
    MacroModified: boolean;
    KeyModified: boolean;
    SaveState: TSaveState;
    panelBtnList: TObjectList;
    currentLayoutFile: string;
    resetLayer: boolean;
    loadingSettings: boolean;
    infoMessageShown: boolean;
    loadingMacro: boolean;
    blueColor: TColor;
    fontColor: TColor;
    backColor: TColor;
    copiedMacro: TKeyList;
    remapCount: integer;
    macroCount: integer;
    tapHoldCount: integer;

    procedure SetConfigOS;
    procedure SetKeyboardHook;
    procedure RemoveKeyboardHook;
    procedure PnlButtonClick(Sender: TObject);
    procedure InitPnlButtons(container: TWinControl);
    procedure SetActivePnlButton(pnlbutton: TPanelBtn);
    procedure SetPnlButtonText(pnlbutton: TPanelBtn; panelText: string);
    procedure SetSaveState(Value: TSaveState);
    function LoadStateSettings: boolean;
    function LoadFirwareVersion: boolean;
    function LoadKeyboardLayout(layoutFile: string): boolean;
    function CheckToSave: boolean;
    function Save(allowNew: boolean = false; showSaveDialog: boolean = true): boolean;
    //procedure SaveAs;
    procedure LoadLayer(layer: TKBLayer);
    //procedure SetPnlButtonKey(kbKey: TKBKey);
    procedure UpdatePnlButtonKey(kbKey: TKBKey; pnlButton: TPanelBtn; unselectKey: boolean = false);
    function GetPnlButtonByIndex(index: integer): TPanelBtn;
    procedure SetModifiedKey(key: word; Modifiers: string; isMacro: boolean; bothLayers: boolean = false);
    procedure SetActiveLayer(layerIdx: integer);
    procedure SetOtherPanelClick(container: TWinControl);
    procedure OtherPanelClick(Sender: TObject);
    procedure RefreshRemapInfo;
    procedure SetMacroMode(value: boolean; reset: boolean = true);
    procedure SetMacroText(pushCursorToEnd: boolean; cursorPos: integer = -1);
    procedure LoadMacro;
    procedure SetMemoTextColor(aMemo: TRichMemo; aKeysPos: TKeysPos);
    procedure ResetMacroCoTriggers;
    Procedure ResetCoTrigger(coTriggerBtn: THSSpeedButton);
    Procedure ActivateCoTrigger(coTriggerBtn: THSSpeedButton);
    procedure SetCoTrigger(aKey: TKey);
    function IsKeyLoaded: boolean;
    function GetCoTriggerKey(button: THSSpeedButton): TKey;
    procedure RemoveCoTrigger(key: word);
    //function CheckSaveMacro(canSave: boolean): boolean;
    procedure SaveStateSettings;
    procedure OnRestore(Sender: TObject);
    procedure AppDeactivate(Sender: TObject);
    procedure createCustomButton(var customBtns: TCustomButtons; btnCaption: string; btnWidth: integer; btnOnClick: TNotifyEvent; btnKind: TBitBtnKind = bkCustom);
    procedure continueClick(Sender: TObject);
    procedure readManualClick(Sender: TObject);
    procedure openTroubleshootingTipsClick(Sender: TObject);
    function CheckSaveKey(canSave: boolean): boolean;
    function GetCursorPrevKey(cursorPos: integer): integer;
    function GetCursorNextKey(cursorPos: integer): integer;
    function GetKeyOtherLayer(keyIdx: integer): TKBKey;
    procedure watchTutorialClick(Sender: TObject);
    function ValidateBeforeDone: boolean;
    procedure EnableMacroBox(value: boolean);
    procedure OpenTapAndHold;
    procedure openFirwareWebsite(Sender: TObject);
  public
    { public declarations }
    keyService: TKeyService;
    fileService: TFileService;
  end;


var
  FormMain: TFormMain;
  NeedInput: boolean;
  lastKeyDown: word;
  KBHook: HHook;
  lastKeyPressed: word;
  MPos:TPoint; {Position of the Form before drag}

  procedure SetKeyPress(Key: word; Modifiers: string);
  {$ifdef Win32}
  function KeyboardHookProc(Code, wParam, lParam: longint): longint; stdcall;  {this intercepts keyboard input}
  {$endif}

implementation

{$R *.lfm}

{ TFormMain }

{$ifdef Win32}
//Keyboard hook to trap key presses and process them
function KeyboardHookProc(Code, wParam, lParam: longint): longint; stdcall;
var
  Transition: TTransitionState;
  extended: TExtendedState;
  //KeystrokeDataPtr: PKeystrokeData;
  currentKey: longint;
  scanCode: longint;
begin
  //If we need keyboard input (ex: file prompt) allow key presses
  if NeedInput or
    ((FormTapAndHold <> nil) and FormTapAndHold.eTimingDelay.Focused) then
  begin
    Result := CallNextHookEx(WH_KEYBOARD, Code, wParam, lParam);
    exit;
  end;

  if (not FormMain.Active) and
    not((FormTapAndHold <> nil) and FormTapAndHold.Active and
    (FormTapAndHold.eTapAction.Focused or FormTapAndHold.eHoldAction.Focused)) then
    exit;

  currentKey := wParam;

  //Checks if key is up or down
  Transition := TTransitionState((lParam shr 31) and 1);

  //Checks if key is normal or extended
  extended := TExtendedState((lParam shr 24) and 1);

  //Gets ScanCode
  scancode := (lParam and $00ff0000) >> 16;

  //Detects if numpadenter is pressed, changes key for user-defined numpad enter
  if (extended = esExtended) and (currentKey = VK_RETURN) then
    currentKey := VK_NUMPADENTER;

  //Distinguish between left and right Ctrl
  if (currentKey = VK_CONTROL) then
  begin
    if (extended = esExtended) then
      currentKey := VK_RCONTROL
    else
      currentKey := VK_LCONTROL;
  end;

  //Distinguish between left and right Alt
  if (currentKey = VK_MENU) then
  begin
    if (extended = esExtended) then
      currentKey := VK_RMENU
    else
      currentKey := VK_LMENU;
  end;

  //Distinguish between left and right Shift
  if (currentKey = VK_SHIFT) then
  begin
    currentKey := MapVirtualKey(scancode, MAPVK_VSC_TO_VK_EX);
  end;

  if (Code = HC_ACTION) then
  begin
    if (Transition = tsPressed) then //On key down
    begin
      //If not a modifier
      if not (IsModifier(currentKey)) then
      begin
        //If key is different then last pressed key (hasn't been released yet)
        if currentKey <> lastKeyPressed then
          SetKeyPress(currentKey, FormMain.keyService.GetModifierText);

        //To prevent Windows from passing the keystrokes  to the target window, the Result value must  be a nonzero value.
        Result := 1;

        //Sets last key pressed
        lastKeyPressed := currentKey;
      end
      else
      begin
        //Adds modifier to list of active modifiers
        FormMain.keyService.AddModifier(currentKey);
      end;
    end
    else if (Transition = tsReleased) then //On key up
    begin
      //When last key pressed is released we reset it
      if currentKey = lastKeyPressed then
        lastKeyPressed := 0;

      //If it's a  modifier and it's the last key pressed or print screen (only works on key up)
      if ((currentKey = lastKeyDown) and IsModifier(currentKey)) or
        (currentKey in [VK_PRINT, VK_SNAPSHOT]) then
      begin
        SetKeyPress(currentKey, FormMain.keyService.GetModifierText);

        //To prevent Windows from passing the keystrokes  to the target window, the Result value must  be a nonzero value.
        Result := 1;
      end;

      if IsModifier(currentKey) then
      begin
        //Removes modifier from list of active modifiers
        FormMain.keyService.RemoveModifier(currentKey);
      end;
    end;
  end;
  lastKeyDown := currentKey;
end;
{$endif}

//Only used for Mac version to trap key presses
procedure TFormMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
{$ifdef Darwin}var currentKey: longint;{$endif}
begin
  {$ifdef Darwin}
  //If we need keyboard input (ex: file prompt) allow key presses
  if NeedInput then
  begin
    exit;
  end;

  if (not (Screen.ActiveForm.Name = self.Name)) then
    exit;

  currentKey := key;

  //If not a modifier
  if not (IsModifier(currentKey)) then
  begin
    //If key is different then last pressed key (hasn't been released yet)
    if currentKey <> lastKeyPressed then
      SetKeyPress(currentKey, keyService.GetModifierText);

    //Sets last key pressed
    lastKeyPressed := currentKey;

    //To prevent Windows from passing the keystrokes  to the target window, the Result value must  be a nonzero value.
    Key := 0;
  end
  else
  begin
    //Adds modifier to list of active modifiers
    keyService.AddModifier(currentKey);
  end;

  lastKeyDown := currentKey;
  {$endif}
end;

//Only used for Mac OS to trap key presses
procedure TFormMain.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState
  );
{$ifdef Darwin}var currentKey: longint;{$endif}
begin
  {$ifdef Darwin}
  currentKey := key;

  //When last key pressed is released we reset it
  if currentKey = lastKeyPressed then
     lastKeyPressed := 0;

  if ((currentKey = lastKeyDown) and IsModifier(currentKey)) then //or
    //(currentKey in [VK_PRINT, VK_SNAPSHOT]) then
  begin
    SetKeyPress(currentKey, keyService.GetModifierText);

    //To prevent application from passing the keystrokes  to the target window, the Result value must  be a nonzero value.
    Key := 0;
  end;

  if IsModifier(currentKey) then
  begin
    //Removes modifier from list of active modifiers
    keyService.RemoveModifier(currentKey);
  end;
  {$endif}
end;

//Adds key to list of keys and writes back to edit field
procedure SetKeyPress(Key: word; Modifiers: string);
begin
  if (FormTapAndHold <> nil) and (FormTapAndHold.Visible) then
    FormTapAndHold.SetKeyPress(Key)
  else
    FormMain.SetModifiedKey(Key, Modifiers, FormMain.memoMacro.Focused);
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  customBtns: TCustomButtons;
  canShowApp: boolean;
begin
  //Sets Height and Width of form according to screen resolution
  self.Width := 1100;
  if screen.Width < self.Width then
    self.Width := screen.Width - 20;

  self.Height := 720;
  if screen.Height < self.Height then
    self.Height := screen.Height - 20;

  lblLayoutFile.Caption := '';
  loadingSettings := false;
  infoMessageShown := false;
  loadingMacro := false;
  resetLayer := false;
  panelBtnList := TObjectList.Create;
  activePnlBtn := nil;
  activeKbKey := nil;
  activeLayer := nil;
  SetConfigOS;
  keyService := TKeyService.Create;
  fileService := TFileService.Create;
  SetSaveState(ssNone);
  NeedInput := False;
  //Caption := GApplicationName;
  InitPnlButtons(pnlKb);
  SetOtherPanelClick(self);
  Application.OnRestore := @OnRestore;
  Application.OnDeactivate:=@AppDeactivate;

  SetKeyboardHook;
  SetMacroMode(false);

  //Set UI elements
  lblTitle.Font.Color := KINESIS_BLUE;
  shpKeys.Color := KINESIS_BLUE;
  shpLine.Color := KINESIS_BLUE;
  shpMenu.Color := KINESIS_BLUE;
  lblMenu.Font.Color := KINESIS_BLUE;
  lblRemap.Font.Color := KINESIS_BLUE;
  blueColor := KINESIS_BLUE;
  fontColor := clDefault;
  backColor := clWhite;
  {$ifdef Win32}
  self.BorderStyle := bsNone;
  pnlMain.BorderStyle := bsSingle;
  {$endif}
  {$ifdef Darwin}
  self.BorderStyle := bsSizeable;
  btnClose.Visible := false;
  btnMinimize.Visible := false;
  btnMaximize.Visible := false;
  {$endif}

  canShowApp := fileService.FirmwareExists;
  if (canShowApp) then
  begin
    //Load config keys depending on app version
    keyService.LoadConfigKeys;

    RefreshRemapInfo;

    swLayerSwitch.Checked := true;

    if (LoadStateSettings) then
    begin
      LoadFirwareVersion;
      fileService.LoadAppSettings(GAppSettingsFile);
      LoadKeyboardLayout(currentLayoutFile);

      slMacroSpeed.Enabled := fileService.AllowEditSettings;
      slStatusReport.Enabled := fileService.AllowEditSettings;
      swKeyClicks.Enabled := fileService.AllowEditSettings;
      swKeyTones.Enabled := fileService.AllowEditSettings;
      swAutoVDrive.Enabled := fileService.AllowEditSettings;
    end
    else
      canShowApp := false;
  end;

  if not canShowApp then
  begin
    createCustomButton(customBtns, 'Troubleshooting Tips', 175, @openTroubleshootingTipsClick);
    ShowDialog('SmartSet App File Error', 'The SmartSet App cannot find the necessary layout and settings files on the v-drive. Replug the keyboard to regenerate these files and try launching the App again.',
      mtFSEdge, [], DEFAULT_DIAG_HEIGHT, backColor, fontColor, customBtns);
    Application.Terminate;
  end;
end;

procedure TFormMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if not CheckToSave then
    CloseAction := caNone
  else
  begin
    FreeAndNil(keyService);
    FreeAndNil(fileService);
    if CloseAction = caFree then
      self := nil;
  end;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  RemoveKeyboardHook;
end;

procedure TFormMain.imgKinesisClick(Sender: TObject);
begin
  OpenUrl('http://www.kinesis.com');
end;

procedure TFormMain.FormActivate(Sender: TObject);
var
  customBtns: TCustomButtons;
  hideNotif: integer;
begin
  if (not infoMessageShown) and (not fileService.AppSettings.AppIntroMsg) then
  begin
    createCustomButton(customBtns, 'Continue', 120, @continueClick);
    //createCustomButton(customBtns, 'Watch Tutorial', 120, @watchTutorialClick);
    createCustomButton(customBtns, 'Read Manual', 120, @readManualClick);

    hideNotif := ShowDialog('Introduction', 'To program, first select a key by clicking on the keyboard image' + #10 +
      '- Remap: Tap the desired key action on the keyboard or use the Special Actions button' + #10 +
      '- Macro: Click the macro box and then type your macro on the keyboard',
      mtInformation, [], 200, backColor, fontColor, customBtns, 'Hide this notification?');
    if (hideNotif >= DISABLE_NOTIF) then
    begin
      fileService.SetAppIntroMsg(true);
      fileService.SaveAppSettings;
    end;
  end;
  infoMessageShown := true;
end;

procedure TFormMain.pnlTitleMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  MPos.X := X;
  MPos.Y := Y;
end;

procedure TFormMain.pnlTitleMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if ssLeft in Shift then
  begin
    self.Left := self.Left - (MPos.X-X);
    self.Top := self.Top - (MPos.Y-Y);
  end;
end;

procedure TFormMain.SetConfigOS;
begin
  //Windows
  {$ifdef Win32}
  SetFont(self, 'Segoe UI');
  //memoMacro.Color := clWhite;
  //memoConfigDefaultColor := clWhite;
  {$endif}

  //MacOS
  {$ifdef Darwin}
  self.AutoScroll := false; //No scroll bars OSX, does not work well
  self.KeyPreview := true; //traps key presses at form level
  SetFont('Helvetica');
  //memoConfigDefaultColor := $00F6F6F6;
  //memoMacro.BorderStyle := bsSingle;
  //lblInfoConfig2.Font.Size := 9;
  //lblPedal.Font.Color := clActiveCaption;
  {$endif}
end;

{Set the keyboard hook so we  can intercept keyboard input}
procedure TFormMain.SetKeyboardHook;
{$ifdef Darwin}var eventType: EventTypeSpec;{$endif}
begin
  //Windows
  {$ifdef Win32}
  KBHook := SetWindowsHookEx(WH_KEYBOARD, @KeyboardHookProc, HInstance,
    GetCurrentThreadId());
  {$endif}
end;

{unhook the keyboard interception}
procedure TFormMain.RemoveKeyboardHook;
begin
  //Windows
  {$ifdef Win32}
  UnHookWindowsHookEx(KBHook);
  {$endif}
end;

procedure TFormMain.SetOtherPanelClick(container: TWinControl);
//var
  //i: integer;
begin
  //for i := 0 to container.ControlCount - 1 do
  //begin
  //  if (container.Controls[i] is TPanel) and not(container.Controls[i] is TPanelBtn) then
  //  begin
  //    (container.Controls[i] as TPanel).OnClick := @OtherPanelClick;
  //    SetOtherPanelClick(container.Controls[i] as TPanel);
  //  end;
  //end;
  pnlKb.OnClick := @OtherPanelClick;
end;

procedure TFormMain.btnActivateMacroClick(Sender: TObject);
begin
  //if IsKeyLoaded then
  //begin
  //  SetMacroMode(true);
  //  UpdatePnlButtonKey(activeKbKey, activePnlBtn);
  //  RefreshRemapInfo;
  //end;
end;

procedure TFormMain.btnResetKeyClick(Sender: TObject);
var
  response: integer;
begin
  if IsKeyLoaded then
  begin
    if (not fileService.AppSettings.ResetKeyMsg) then
    begin
      response := ShowDialog('Reset current key',
        'Do you want to reset the current Key?' + #10 + 'The remapped key action and any stored macros will be lost.',
        mtConfirmation, [mbYes, mbNo], DEFAULT_DIAG_HEIGHT, backColor, fontColor, nil, 'Hide this notification?');
      if (response >= DISABLE_NOTIF) then
      begin
        fileService.SetResetKeyMsg(true);
        fileService.SaveAppSettings;
      end;
    end
    else
      response := mrYes;

    if (response = mrYes) or (response = mrYes + DISABLE_NOTIF) then
    begin
      activeKbKey.ResetKey;
      UpdatePnlButtonKey(activeKbKey, activePnlBtn);
      SetSaveState(ssModifed);
    end;
  end;

  RefreshRemapInfo;

end;

procedure TFormMain.btnResetLayerClick(Sender: TObject);
begin
  if ShowDialog('Reset layer',
      'Do you want to reset the current Layer?' + #10 + 'All remapped keys and stored macros will be lost.',
      mtConfirmation, [mbYes, mbNo], DEFAULT_DIAG_HEIGHT, backColor, fontColor) = mrYes then
  begin
    keyService.ResetLayer(activeLayer);
    LoadLayer(activeLayer);
    SetActivePnlButton(nil);
    RefreshRemapInfo;
    SetSaveState(ssModifed);
  end;
end;

procedure TFormMain.btnResetLayoutClick(Sender: TObject);
begin
  if ShowDialog('Reset layout',
        'Do you want to reset the current Layout?' + #10 + 'All remapped keys and stored macros in both layers will be lost.',
        mtConfirmation, [mbYes, mbNo], DEFAULT_DIAG_HEIGHT, backColor, fontColor) = mrYes then
  begin
    keyService.ResetLayout;
    LoadLayer(activeLayer);
    SetActivePnlButton(nil);
    RefreshRemapInfo;
    SetSaveState(ssModifed);
  end;
end;

procedure TFormMain.btnSpecialActionsRemapClick(Sender: TObject);
var
  lPoint: TPoint;
begin
  //if IsKeyLoaded then
  //begin
    //Popup menu over special button
    lPoint.x := 0;
    lPoint.y := 0;
    lPoint := btnSpecialActionsRemap.ClientToScreen(lPoint);
    pmTokensKeys.Popup(lPoint.x, lPoint.y);
  //end;
end;

procedure TFormMain.btnCloseClick(Sender: TObject);
begin
  if CheckSaveKey(true) then
    Close;
end;

procedure TFormMain.btnCopyClick(Sender: TObject);
var
  hideNotif: integer;
begin
  if (IsKeyLoaded) then
  begin
    if (activeKbKey.ActiveMacro <> nil) then
    begin
      copiedMacro := keyService.CopyMacro(activeKbKey.ActiveMacro);
      if (not fileService.AppSettings.CopyMacroMsg) then
      begin
        hideNotif := ShowDialog('Copy', 'Macro copied. Now select a new trigger key or load a new layout, then hit Paste.',
          mtInformation, [mbOK], DEFAULT_DIAG_HEIGHT, backColor, fontColor, nil, 'Hide this notification?');
        if (hideNotif >= DISABLE_NOTIF) then
        begin
          fileService.SetCopyMacroMsg(true);
          fileService.SaveAppSettings;
        end;
      end;
    end
    else
      ShowDialog('Copy Macro', 'You must have an active maro to copy', mtInformation, [mbOK], DEFAULT_DIAG_HEIGHT,
        backColor, fontColor);
  end;
end;

procedure TFormMain.btnMaximizeClick(Sender: TObject);
begin
  if (Self.WindowState = wsMaximized) then
  begin
    Self.WindowState := wsNormal;
    imageList.GetBitmap(4, btnMaximize.Glyph);
    btnMaximize.Hint := 'Maximize';
  end
  else
  begin
    Self.WindowState := wsMaximized;
    imageList.GetBitmap(5, btnMaximize.Glyph);
    btnMaximize.Hint := 'Restore window';
  end;
end;

procedure TFormMain.btnMinimizeClick(Sender: TObject);
begin
  {$ifdef Win32}self.BorderStyle := bsSizeable;{$endif}
  Self.WindowState := wsMinimized;
end;

procedure TFormMain.btnSpecialActionsMacroClick(Sender: TObject);
var
  lPoint: TPoint;
begin
  if IsKeyLoaded then
  begin
    miWinM.Checked := keyService.IsWinKeyDown;

    //Popup menu over special button
    lPoint.x := 0;
    lPoint.y := 0;
    lPoint := btnSpecialActionsMacro.ClientToScreen(lPoint);
    pmTokensMacros.Popup(lPoint.x, lPoint.y);
  end;
end;

procedure TFormMain.btnBackspaceClick(Sender: TObject);
var
  cursorPos:integer;
  keyIdx: integer;
  aKey: TKey;
  keyText: string;
  isLongText: boolean;
  prevKey: integer;
begin
  if (IsKeyLoaded) and (activeKbKey.IsMacro) then
  begin
    cursorPos := memoMacro.SelStart;
    if (cursorPos > 0) then
    begin
      keyIdx := keyService.GetKeyAtPosition(activeKbKey.ActiveMacro, cursorPos);
      if (keyIdx >= 0) then
      begin
        aKey := activeKbKey.ActiveMacro[keyIdx];
        keyText := keyService.GetSingleKeyText(aKey, isLongText);
        prevKey := GetCursorPrevKey(cursorPos);
        if keyService.RemoveKey(activeKbKey, keyIdx) then
        begin
          MacroModified := true;

          //Erase key from memo
          if (prevKey = -1) then
            memoMacro.SelStart := 0
          else
            memoMacro.SelStart := prevKey;
          memoMacro.SelLength := Length(keyText);
          memoMacro.SelText := '';

          if (prevKey = -1) then
            memoMacro.SelStart := 0
          else
            memoMacro.SelStart := prevKey;
        end;
      end;
    end;
  end;
end;

procedure TFormMain.btnCancelKeyClick(Sender: TObject);
begin
  if IsKeyLoaded then
  begin
    KeyModified := false;
    MacroModified := false;
    keyService.RestoreMacro(activeKbKey); //Returns to previous values for Macro
    keyService.RestoreKbKey(activeKbKey); //Returns to previous values for Key
    SetActivePnlButton(nil);
    RefreshRemapInfo;
  end;
end;

procedure TFormMain.btnCancelMacroClick(Sender: TObject);
begin
  KeyModified := false;
  MacroModified := false;
  keyService.RestoreMacro(activeKbKey); //Returns to previous values
  activeKbKey.IsMacro := (activeKbKey.Macro1.Count > 0) or (activeKbKey.Macro2.Count > 0) or (activeKbKey.Macro3.Count > 0) ;
  SetMacroMode(true);
  UpdatePnlButtonKey(activeKbKey, activePnlBtn);
  SetActivePnlButton(nil);
  pnlKb.SetFocus;
  RefreshRemapInfo;
end;

procedure TFormMain.btnClearClick(Sender: TObject);
begin
  if (IsKeyLoaded) and (activeKbKey.IsMacro) then
  begin
    memoMacro.Lines.Clear;
    keyService.ClearModifiers;
    activeKbKey.ActiveMacro.Clear;
    MacroModified := true;
  end;
end;

procedure TFormMain.btnDoneKeyClick(Sender: TObject);
begin
  if IsKeyLoaded then
  begin
    if ValidateBeforeDone then
    begin
      KeyModified := false;
      MacroModified := false;
      SetSaveState(ssModifed);
      activeKbKey.IsMacro := (activeKbKey.Macro1.Count > 0) or (activeKbKey.Macro2.Count > 0) or (activeKbKey.Macro3.Count > 0);
      //SetMacroMode(false);
      SetActivePnlButton(nil);
      RefreshRemapInfo;
    end;
  end;
end;

procedure TFormMain.btnDoneMacroClick(Sender: TObject);
var
  keyAssigned: string;
  extraInfo: string;
begin
  if ValidateBeforeDone then
  begin
    KeyModified := false;
    MacroModified := false;
    SetSaveState(ssModifed);
    activeKbKey.IsMacro := (activeKbKey.Macro1.Count > 0) or (activeKbKey.Macro2.Count > 0) or (activeKbKey.Macro3.Count > 0);
    if (activeKbKey.IsMacro) then
    begin
      keyAssigned := '';

      if activeKbKey.ActiveMacro.CoTrigger1 <> nil then
        keyAssigned := activeKbKey.ActiveMacro.CoTrigger1.OtherDisplayText;

      if activeKbKey.ActiveMacro.CoTrigger2 <> nil then
      begin
        if (keyAssigned <> '') then
          keyAssigned := keyAssigned + ' + ' + activeKbKey.ActiveMacro.CoTrigger2.OtherDisplayText
        else
          keyAssigned := activeKbKey.ActiveMacro.CoTrigger2.OtherDisplayText;
      end;

      if activeKbKey.ActiveMacro.CoTrigger3 <> nil then
      begin
        if (keyAssigned <> '') then
          keyAssigned := keyAssigned + ' + ' + activeKbKey.ActiveMacro.CoTrigger3.OtherDisplayText
        else
          keyAssigned := activeKbKey.ActiveMacro.CoTrigger3.OtherDisplayText;
      end;

      if (keyAssigned <> '') then
        keyAssigned := keyAssigned + ' + ' + activeKbKey.OriginalKey.OtherDisplayText
      else
        keyAssigned := activeKbKey.OriginalKey.OtherDisplayText;
    end;
    SetMacroMode(true);
    UpdatePnlButtonKey(activeKbKey, activePnlBtn);
    pnlKb.SetFocus;
    SetActivePnlButton(nil);
    RefreshRemapInfo;

    //Show message for assigned macro
    if (keyAssigned <> '') then
    begin
      if (activeLayer.LayerIndex = BOTLAYER_IDX) then
        extraInfo := ' in the embedded layer';
      ShowDialog('Macro', 'Macro assigned to ' + StringReplace(keyAssigned, #10, ' ', [rfReplaceAll]) + extraInfo, mtInformation, [mbOK], DEFAULT_DIAG_HEIGHT,
          backColor, fontColor);
    end;
  end;
end;

procedure TFormMain.btnLoadClick(Sender: TObject);
begin
  CheckToSave;

  OpenDialog.InitialDir := GPedalsFilePath;
  if OpenDialog.Execute then
  begin
    currentLayoutFile := OpenDialog.FileName;
    LoadKeyboardLayout(currentLayoutFile);
    SetSaveState(ssNone);
  end;
end;

procedure TFormMain.btnPasteClick(Sender: TObject);
begin
  if (IsKeyLoaded) then
  begin
    if (activeKbKey.ActiveMacro <> nil) and (copiedMacro <> nil) then
    begin
      SetMacroMode(true, false);
      activeKbKey.IsMacro := true;
      activeKbKey.ActiveMacro.Assign(copiedMacro);
      SetMacroText(true);
      LoadMacro;
      MacroModified := true;
    end;
  end;
end;

procedure TFormMain.btnSaveClick(Sender: TObject);
begin
  Save;
end;

procedure TFormMain.OtherPanelClick(Sender: TObject);
begin
  if IsKeyLoaded then
    SetActivePnlButton(nil);
end;

procedure TFormMain.PnlButtonMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  lPoint: TPoint;
begin
  if (Button = TMouseButton.mbRight) then
  begin
    SetActivePnlButton(sender as TPanelBtn);
    if (IsKeyLoaded) then
    begin
      lPoint.x := (Sender as TLabelBox).Width;
      lPoint.y := (Sender as TLabelBox).Height;
      lPoint := (Sender as TLabelBox).ClientToScreen(lPoint);
      pmTokensKeys.Popup(lPoint.x, lPoint.y);
    end;
  end;
end;

procedure TFormMain.pmTokensKeysPopup(Sender: TObject);
var
  keyLoaded: boolean;

  procedure CheckToDisable(items: TMenuItem);
  var
    i:integer;
  begin
    for i := 0 to items.Count - 1 do
    begin
      if (not items[i].Default) then
        items[i].Enabled := (keyLoaded or (items[i].Tag = 1));

      if (items[i].Count > 0) then
        CheckToDisable(items[i]);
    end;
  end;

begin
  keyLoaded := IsKeyLoaded;
  CheckToDisable(pmTokensKeys.Items);
end;

procedure TFormMain.RefreshRemapInfo;
var
  i, j:integer;
  aLayer: TKBLayer;
  aKbKey: TKBKey;
begin
  remapCount := 0;
  macroCount := 0;
  tapHoldCount := 0;
  for i := 0 to keyService.KBLayers.Count - 1 do
  begin
    aLayer := keyService.KBLayers[i];
    for j := 0 to aLayer.KBKeyList.Count - 1 do
    begin
      aKbKey := aLayer.KBKeyList[j];
      if (aKbKey.TapAndHold) then
        inc(tapHoldCount)
      else if (aKbKey.IsModified) then
        inc(remapCount);

      if (aKbKey.IsMacro) then
      begin
        if (aKbKey.Macro1.Count > 0) then
          inc(macroCount);
        if (aKbKey.Macro2.Count > 0) then
          inc(macroCount);
        if (aKbKey.Macro3.Count > 0) then
          inc(macroCount);
      end;
    end;
  end;

  if (remapCount > 0) then
    lblRemap.Caption := 'Remap (' + IntToStr(remapCount) + ')'
  else
    lblRemap.Caption := 'Remap';

  if (macroCount > 0) then
    lblMacro.Caption := 'Macro (' + IntToStr(macroCount) + ')'
  else
    lblMacro.Caption := 'Macro';

  btnResetLayer.Enabled := (remapCount > 0) or (macroCount > 0);
  btnResetLayout.Enabled := (remapCount > 0) or (macroCount > 0);
    lblRemap.Caption := 'Remap';
end;

procedure TFormMain.rgMacroClick(Sender: TObject);
begin
  if (IsKeyLoaded) then
  begin
    LoadMacro;
    SetMacroText(true);
  end;
end;

procedure TFormMain.slPlaybackSpeedChange(Sender: TObject);
begin
  if (IsKeyLoaded) and (not loadingMacro) then
  begin
    activeKbKey.ActiveMacro.MacroSpeed := Trunc(slPlaybackSpeed.Position);
    MacroModified := true;
  end;
end;

procedure TFormMain.textMacroInputClick(Sender: TObject);
begin
    memoMacro.SetFocus;
end;

procedure TFormMain.swAutoVDriveChange(Sender: TObject);
begin
  if (not loadingSettings) then
  begin
    fileService.SetVDriveStatut(swAutoVDrive.Checked);
    SetSaveState(ssModifed);
    if (swAutoVDrive.Checked) then
      ShowDialog('V-Drive', 'Caution! V-Drive will remain open. Read manual before enabling.', mtWarning, [mbOK]);
  end;
end;

procedure TFormMain.swAutoVDriveClick(Sender: TObject);
begin

end;

procedure TFormMain.swKeyClicksChange(Sender: TObject);
begin
  if (not loadingSettings) then
  begin
    fileService.SetKeyClicks(swKeyClicks.Checked);
    SetSaveState(ssModifed);
  end;
end;

procedure TFormMain.swKeyTonesChange(Sender: TObject);
begin
  if (not loadingSettings) then
  begin
    fileService.SetKeyTones(swKeyTones.Checked);
    SetSaveState(ssModifed);
  end;
end;

procedure TFormMain.swLayerSwitchClick(Sender: TObject);
begin
  if (not resetLayer) then
  begin
    if (CheckSaveKey(true)) then
    begin
      if (swLayerSwitch.Checked) then
        SetActiveLayer(TOPLAYER_IDX)
      else
        SetActiveLayer(BOTLAYER_IDX);
    end
    else
    begin
      resetLayer := true;
      swLayerSwitch.Checked := not swLayerSwitch.Checked;
    end;
  end;
  resetLayer := false;
end;

procedure TFormMain.slMacroSpeedChange(Sender: TObject);
begin
  if (not loadingSettings) then
  begin
    fileService.SetMacroSpeed(Trunc(slMacroSpeed.Position));
    SetSaveState(ssModifed);
  end;
end;

procedure TFormMain.slStatusReportChange(Sender: TObject);
begin
  if (not loadingSettings) then
  begin
    fileService.SetStatusPlaySpeed(Trunc(slStatusReport.Position));
    SetSaveState(ssModifed);
  end;
end;

procedure TFormMain.tbStatusReportChange(Sender: TObject);
begin
  //if (not loadingSettings) then
  //begin
  //  fileService.SetStatusPlaySpeed(tbStatusReport.Position);
  //  SetSaveState(ssModifed);
  //end;
end;

procedure TFormMain.pmTokensMacrosPopup(Sender: TObject);
begin
  miWinM.Checked := keyService.IsWinKeyDown;
end;

procedure TFormMain.pnlMacroClick(Sender: TObject);
begin
  if (IsKeyLoaded and not(activeKbKey.CanAssignMacro)) then
  begin
    ShowDialog('Macro', 'You cannot assign a macro to a modifier key', mtError, [mbOK]);
  end;
end;

procedure TFormMain.miTokenMacroClick(Sender: TObject);
var
  menuItem: TMenuItem;
begin
  menuItem := sender as TMenuItem;

  if menuItem = miMenuM then
    SetModifiedKey(VK_APPS, '', true)
  else if menuItem = miCalculatorM then
    SetModifiedKey(VK_CALC, '', true)
  else if menuItem = miInternationalKeyM then
    SetModifiedKey(VK_OEM_102, '', true)
  else if menuItem = miShutdownM then
    SetModifiedKey(VK_SHUTDOWN, '', true)
  else if menuItem = miPlayPauseM then
    SetModifiedKey(VK_MEDIA_PLAY_PAUSE, '', true)
  else if menuItem = miNextTrackM then
    SetModifiedKey(VK_MEDIA_NEXT_TRACK, '', true)
  else if menuItem = miPreviousTrackM then
    SetModifiedKey(VK_MEDIA_PREV_TRACK, '', true)
  else if menuItem = miMuteM then
    SetModifiedKey(VK_VOLUME_MUTE, '', true)
  else if menuItem = miVolumeDownM then
    SetModifiedKey(VK_VOLUME_DOWN, '', true)
  else if menuItem = miVolumeUpM then
    SetModifiedKey(VK_VOLUME_UP, '', true)
  else if menuItem = miWinM then
  begin
    if keyService.IsWinKeyDown then
    begin
      keyService.RemoveModifier(VK_LWIN);
      keyService.RemoveModifier(VK_RWIN);
    end
    else
    begin
      keyService.AddModifier(VK_LWIN);
      ShowDialog('Windows Combination Active', 'Now press the key(s) you wish to combine with the Windows key in your macro. Then deselect Windows Combination from the Special Actions menu if you wish to continue programming or click Done.',
        mtInformation, [mbOK], DEFAULT_DIAG_HEIGHT, backColor, fontColor);
      memoMacro.SetFocus;
    end;
  end
  else if menuItem = miCutM then
  begin
    {$ifdef Win32} //Windows
    SetModifiedKey(VK_X, L_CTRL_MOD, true)
    {$endif}
    {$ifdef Darwin}  //MacOS
    SetModifiedKey(VK_X, L_WIN_MOD, true)
    {$endif}
  end
  else if menuItem = miCopyM then
  begin
    {$ifdef Win32} //Windows
    SetModifiedKey(VK_C, L_CTRL_MOD, true)
    {$endif}
    {$ifdef Darwin}  //MacOS
    SetModifiedKey(VK_C, L_WIN_MOD, true)
    {$endif}
  end
  else if menuItem = miPasteM then
  begin
    {$ifdef Win32} //Windows
    SetModifiedKey(VK_V, L_CTRL_MOD, true)
    {$endif}
    {$ifdef Darwin}  //MacOS
    SetModifiedKey(VK_V, L_WIN_MOD, true)
    {$endif}
  end
  else if menuItem = miSelectAllM then
  begin
    {$ifdef Win32} //Windows
    SetModifiedKey(VK_A, L_CTRL_MOD, true)
    {$endif}
    {$ifdef Darwin}  //MacOS
    SetModifiedKey(VK_A, L_WIN_MOD, true)
    {$endif}
  end
  else if menuItem = miUndoM then
  begin
    {$ifdef Win32} //Windows
    SetModifiedKey(VK_Z, L_CTRL_MOD, true)
    {$endif}
    {$ifdef Darwin}  //MacOS
    SetModifiedKey(VK_Z, L_WIN_MOD, true)
    {$endif}
  end
  else if menuItem = miRedoM then
  begin
    {$ifdef Win32} //Windows
    SetModifiedKey(VK_Y, L_CTRL_MOD, true)
    {$endif}
    {$ifdef Darwin}  //MacOS
    SetModifiedKey(VK_Y, L_WIN_MOD, true)
    {$endif}
  end
  else if menuItem = miDesktopM then
  begin
    {$ifdef Win32} //Windows
    SetModifiedKey(VK_D, L_WIN_MOD, true)
    {$endif}
    {$ifdef Darwin}  //MacOS
    SetModifiedKey(VK_D, L_WIN_MOD, true)
    {$endif}
  end
  else if menuItem = miLastAppM then
  begin
    {$ifdef Win32} //Windows
    SetModifiedKey(VK_TAB, L_ALT_MOD, true)
    {$endif}
    {$ifdef Darwin}  //MacOS
    SetModifiedKey(VK_TAB, L_ALT_MOD, true)
    {$endif}
  end
  else if menuItem = miCtrlAltDelM then
  begin
    {$ifdef Win32} //Windows
    SetModifiedKey(VK_DELETE, L_CTRL_MOD + ',' + L_ALT_MOD, true)
    {$endif}
    {$ifdef Darwin}  //MacOS
    SetModifiedKey(VK_DELETE, L_CTRL_MOD + ',' + L_ALT_MOD, true)
    {$endif}
  end
  else if menuItem = miShortDelayM then
    SetModifiedKey(VK_125MS, '', true)
  else if menuItem = miLongDelayM then
    SetModifiedKey(VK_500MS, '', true)
  else if menuItem = miF13M then
    SetModifiedKey(VK_F13, '', true)
  else if menuItem = miF14M then
    SetModifiedKey(VK_F14, '', true)
  else if menuItem = miF15M then
    SetModifiedKey(VK_F15, '', true)
  else if menuItem = miF16M then
    SetModifiedKey(VK_F16, '', true)
  else if menuItem = miF17M then
    SetModifiedKey(VK_F17, '', true)
  else if menuItem = miF18M then
    SetModifiedKey(VK_F18, '', true)
  else if menuItem = miF19M then
    SetModifiedKey(VK_F19, '', true)
  else if menuItem = miF20M then
    SetModifiedKey(VK_F20, '', true)
  else if menuItem = miF21M then
    SetModifiedKey(VK_F21, '', true)
  else if menuItem = miF22M then
    SetModifiedKey(VK_F22, '', true)
  else if menuItem = miF23M then
    SetModifiedKey(VK_F23, '', true)
  else if menuItem = miF24M then
    SetModifiedKey(VK_F24, '', true)
  else if menuItem = miHomeM then
  begin
    {$ifdef Win32} //Windows
    SetModifiedKey(VK_HOME, L_ALT_MOD, true)
    {$endif}
    {$ifdef Darwin}  //MacOS
    SetModifiedKey(VK_HOME, L_ALT_MOD, true)
    {$endif}
  end
  else if menuItem = miForwardM then
  begin
    {$ifdef Win32} //Windows
    SetModifiedKey(VK_RIGHT, L_ALT_MOD, true)
    {$endif}
    {$ifdef Darwin}  //MacOS
    SetModifiedKey(VK_RIGHT, L_ALT_MOD, true)
    {$endif}
  end
  else if menuItem = miBackM then
  begin
    {$ifdef Win32} //Windows
    SetModifiedKey(VK_lEFT, L_ALT_MOD, true)
    {$endif}
    {$ifdef Darwin}  //MacOS
    SetModifiedKey(VK_lEFT, L_ALT_MOD, true)
    {$endif}
  end
  else if menuItem = miNewTabM then
  begin
    {$ifdef Win32} //Windows
    SetModifiedKey(VK_T, L_CTRL_MOD, true)
    {$endif}
    {$ifdef Darwin}  //MacOS
    SetModifiedKey(VK_T, L_CTRL_MOD, true)
    {$endif}
  end
  else if menuItem = miSwitchTabsM then
  begin
    {$ifdef Win32} //Windows
    SetModifiedKey(VK_TAB, L_CTRL_MOD, true)
    {$endif}
    {$ifdef Darwin}  //MacOS
    SetModifiedKey(VK_TAB, L_CTRL_MOD, true)
    {$endif}
  end
  else if (menuItem = miLeftMouseM) then
    SetModifiedKey(VK_MOUSE_LEFT, '', true)
  else if (menuItem = miRightMouseM) then
    SetModifiedKey(VK_MOUSE_RIGHT, '', true)
  else if (menuItem = miMiddleMouseM) then
    SetModifiedKey(VK_MOUSE_MIDDLE, '', true);
end;

procedure TFormMain.miTokenKeyClick(Sender: TObject);
var
  menuItem: TMenuItem;
  customBtns: TCustomButtons;
begin
  menuItem := sender as TMenuItem;

  if menuItem = miKeypadShift then
    SetModifiedKey(VK_KEYPAD_SHIFT, '', false, true)
  else if menuItem = miKeypadToggle then
    SetModifiedKey(VK_KEYPAD_TOGGLE, '', false, true)
  else if menuItem = miMenu then
    SetModifiedKey(VK_APPS, '', false)
  else if menuItem = miNull then
    SetModifiedKey(VK_NULL, '', false)
  else if menuItem = miCalculator then
    SetModifiedKey(VK_CALC, '', false)
  else if menuItem = miInternationalKey then
    SetModifiedKey(VK_OEM_102, '', false)
  else if menuItem = miShutdown then
    SetModifiedKey(VK_SHUTDOWN, '', false)
  else if menuItem = miPlayPause then
    SetModifiedKey(VK_MEDIA_PLAY_PAUSE, '', false)
  else if menuItem = miNextTrack then
    SetModifiedKey(VK_MEDIA_NEXT_TRACK, '', false)
  else if menuItem = miPreviousTrack then
    SetModifiedKey(VK_MEDIA_PREV_TRACK, '', false)
  else if menuItem = miMute then
    SetModifiedKey(VK_VOLUME_MUTE, '', false)
  else if menuItem = miVolumeDown then
    SetModifiedKey(VK_VOLUME_DOWN, '', false)
  else if menuItem = miVolumeUp then
    SetModifiedKey(VK_VOLUME_UP, '', false)
  else if menuItem = miF13 then
    SetModifiedKey(VK_F13, '', false)
  else if menuItem = miF14 then
    SetModifiedKey(VK_F14, '', false)
  else if menuItem = miF15 then
    SetModifiedKey(VK_F15, '', false)
  else if menuItem = miF16 then
    SetModifiedKey(VK_F16, '', false)
  else if menuItem = miF17 then
    SetModifiedKey(VK_F17, '', false)
  else if menuItem = miF18 then
    SetModifiedKey(VK_F18, '', false)
  else if menuItem = miF19 then
    SetModifiedKey(VK_F19, '', false)
  else if menuItem = miF20 then
    SetModifiedKey(VK_F20, '', false)
  else if menuItem = miF21 then
    SetModifiedKey(VK_F21, '', false)
  else if menuItem = miF22 then
    SetModifiedKey(VK_F22, '', false)
  else if menuItem = miF23 then
    SetModifiedKey(VK_F23, '', false)
  else if menuItem = miF24 then
    SetModifiedKey(VK_F24, '', false)
  else if menuItem = miLeftMouse then
    SetModifiedKey(VK_MOUSE_LEFT, '', false)
  else if menuItem = miMiddleMouse then
    SetModifiedKey(VK_MOUSE_MIDDLE, '', false)
  else if menuItem = miRightMouse then
    SetModifiedKey(VK_MOUSE_RIGHT, '', false)
  else if (menuItem = miHyper) or (menuItem = miMeh) then
  begin
    if (fileService.VersionBiggerEqual(1, 0, 516)) then
    begin
      if (menuItem = miHyper) then
        SetModifiedKey(VK_HYPER, '', false)
      else if (menuItem = miMeh) then
        SetModifiedKey(VK_MEH, '', false);
    end
    else
    begin
      createCustomButton(customBtns, 'OK', 150, nil, bkOK);
      createCustomButton(customBtns, 'Update Firmware', 150, @openFirwareWebsite);
      ShowDialog('Multimodifiers', 'To utilize Multimodifiers, please download and install the latest firmware.',
        mtWarning, [], DEFAULT_DIAG_HEIGHT, backColor, fontColor, customBtns);
    end;
  end
  else if menuItem = miTapHold then
  begin
    OpenTapAndHold;
  end;
end;

procedure TFormMain.memoMacroMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  //Prevent selecting text in macro with the mouse
  if memoMacro.SelLength > 0 then
     memoMacro.SelLength := 0;
end;

procedure TFormMain.watchTutorialClick(Sender: TObject);
begin
  OpenUrl(ADV2_TUTORIAL);
end;

procedure TFormMain.readManualClick(Sender: TObject);
var
  filePath: string;
begin
  filePath := GApplicationPath + '\' + USER_MANUAL_ADV2;
  {$ifdef Darwin}filePath := GApplicationPath + '/' + USER_MANUAL_ADV2;{$endif}

  if FileExists(filePath) then
    OpenDocument(filePath)
  else
    ShowDialog('Help file', 'Help file not found!', mtError, [mbOK], DEFAULT_DIAG_HEIGHT, backColor, fontColor);
end;

procedure TFormMain.openTroubleshootingTipsClick(Sender: TObject);
begin
  OpenUrl('https://www.kinesis-ergo.com/advantage2-resources/');
end;

procedure TFormMain.createCustomButton(var customBtns: TCustomButtons;
  btnCaption: string; btnWidth: integer; btnOnClick: TNotifyEvent;
  btnKind: TBitBtnKind);
var
  customBtn: TCustomButton;
begin
  customBtn.Caption := btnCaption;
  customBtn.Width := btnwidth;
  customBtn.OnClick := btnOnClick;
  customBtn.Kind := btnKind;

  SetLength(customBtns, Length(customBtns) + 1);
  customBtns[Length(customBtns) - 1] := customBtn;
end;

procedure TFormMain.continueClick(Sender: TObject);
begin
  CloseDialog;
end;

procedure TFormMain.bCoTriggerClick(Sender: TObject);
var
  button: THSSpeedButton;
  aKey: TKey;
begin
  if IsKeyLoaded then
  begin
    button := Sender as THSSpeedButton;
    if (button.Down) then
    begin
      ActivateCoTrigger(button);
      if (activeKbKey.ActiveMacro.CoTrigger1 = nil) then
      begin
        aKey := GetCoTriggerKey(button);
        if (aKey <> nil) then
        begin
          activeKbKey.ActiveMacro.CoTrigger1 := aKey.CopyKey;
          MacroModified := true;
        end;
      end
      else if (activeKbKey.ActiveMacro.CoTrigger2 = nil) then
      begin
        aKey := GetCoTriggerKey(button);
        if (aKey <> nil) then
        begin
          activeKbKey.ActiveMacro.CoTrigger2 := aKey.CopyKey;
          MacroModified := true;
        end;
      end
      else if (activeKbKey.ActiveMacro.CoTrigger3 = nil) then
      begin
        aKey := GetCoTriggerKey(button);
        if (aKey <> nil) then
        begin
          activeKbKey.ActiveMacro.CoTrigger3 := aKey.CopyKey;
          MacroModified := true;
        end;
      end
      else
      begin
        ShowDialog('Co-Triggers', 'You cannot add more than 3 co-triggers', mtWarning, [mbOK]);
        ResetCoTrigger(button);
      end;
    end
    else
    begin
      ResetCoTrigger(button);
      aKey := GetCoTriggerKey(button);
      if (aKey <> nil) then
        RemoveCoTrigger(aKey.Key);
    end;
  end;
end;

procedure TFormMain.btnHelpIconClick(Sender: TObject);
begin
  Application.CreateForm(TFormAbout, FormAbout);
  FormAbout.SetFirmwareVersion(fileService.FirmwareVersion);
  FormAbout.ShowModal;
end;

function TFormMain.GetCoTriggerKey(button: THSSpeedButton): TKey;
begin
  if (button = bLShiftMacro) then
    result := keyService.FindKeyConfig(VK_LSHIFT)
  else if (button = bRShiftMacro) then
    result := keyService.FindKeyConfig(VK_RSHIFT)
  else if (button = bLCtrlMacro) then
    result := keyService.FindKeyConfig(VK_LCONTROL)
  else if (button = bRCtrlMacro) then
    result := keyService.FindKeyConfig(VK_RCONTROL)
  else if (button = bLAltMacro) then
    result := keyService.FindKeyConfig(VK_LMENU)
  else if (button = bRAltMacro) then
    result := keyService.FindKeyConfig(VK_RMENU)
  else
    result := nil;
end;

procedure TFormMain.RemoveCoTrigger(key: word);
begin
  if IsKeyLoaded then
  begin
    MacroModified := true;
    if (activeKbKey.ActiveMacro.CoTrigger1 <> nil) and (activeKbKey.ActiveMacro.CoTrigger1.Key = key) then
    begin
      activeKbKey.ActiveMacro.CoTrigger1 := nil;
      activeKbKey.ActiveMacro.CoTrigger1 := activeKbKey.ActiveMacro.CoTrigger2;
      activeKbKey.ActiveMacro.CoTrigger2 := activeKbKey.ActiveMacro.CoTrigger3;
      activeKbKey.ActiveMacro.CoTrigger3 := nil;
    end
    else if (activeKbKey.ActiveMacro.CoTrigger2 <> nil) and (activeKbKey.ActiveMacro.CoTrigger2.Key = key) then
    begin
      activeKbKey.ActiveMacro.CoTrigger2 := nil;
      activeKbKey.ActiveMacro.CoTrigger2 := activeKbKey.ActiveMacro.CoTrigger3;
      activeKbKey.ActiveMacro.CoTrigger3 := nil;
    end
    else if (activeKbKey.ActiveMacro.CoTrigger3 <> nil) and (activeKbKey.ActiveMacro.CoTrigger3.Key = key) then
    begin
      activeKbKey.ActiveMacro.CoTrigger3 := nil;
    end;
  end;
end;

function TFormMain.ValidateBeforeDone: boolean;
var
  errorMsg: string;
  errorTitle: string;
  isValid: boolean;
begin
  isValid := keyService.ValidateMacros(activeKbKey, errorMsg, errorTitle);

  if isValid then
    RefreshRemapInfo
  else
    ShowDialog(errorTitle, errorMsg, mtError, [mbOK], DEFAULT_DIAG_HEIGHT,
          backColor, fontColor);

  result := isValid;
end;

procedure TFormMain.AppDeactivate(Sender: TObject);
begin
  keyService.ClearModifiers;
end;

procedure TFormMain.EnableMacroBox(value: boolean);
begin
  memoMacro.Enabled := value;
  rgMacro1.Enabled := value;
  rgMacro2.Enabled := value;
  rgMacro3.Enabled := value;
  bLShiftMacro.Enabled := value;
  bRShiftMacro.Enabled := value;
  bLCtrlMacro.Enabled := value;
  bRCtrlMacro.Enabled := value;
  bLAltMacro.Enabled := value;
  bRAltMacro.Enabled := value;
  //jm todo tbSpeed.Enabled := value;
  slPlaybackSpeed.Enabled := value;
end;

procedure TFormMain.OpenTapAndHold;
var
  customBtns: TCustomButtons;
  otherLayer: TKBLayer;
  keyOtherLayer: TKBKey;
begin
  if (IsKeyLoaded) then
  begin
    if (fileService.VersionBiggerEqual(1, 0, 516)) then
    begin
      //Check key other layer
      if (activeLayer.LayerIndex = TOPLAYER_IDX) then
        otherLayer := keyService.GetLayer(BOTLAYER_IDX)
      else
        otherLayer := keyService.GetLayer(TOPLAYER_IDX);
      keyOtherLayer := keyService.GetKbKeyByIndex(otherLayer, activeKbKey.Index);

      if (keyOtherLayer <> nil) and (keyOtherLayer.TapAndHold) then
        ShowDialog('Tap and Hold', 'You cannot assign a Tap and Hold Action to the same key in both layers.', mtWarning, [mbOk], DEFAULT_DIAG_HEIGHT, backColor, fontColor)
      else if (activeKbKey.TapAndHold = false) and (tapHoldCount >= MAX_TAP_HOLD) then
        ShowDialog('Tap and Hold', 'You have reached the maximum number of Tap and Hold actions for this Profile.', mtWarning, [mbOk], DEFAULT_DIAG_HEIGHT, backColor, fontColor)
      else if (activeKbKey.IsMacro) then
      begin
        ShowDialog('Tap and Hold', 'You cannot assign a Tap and Hold Action to a macro trigger key.',
          mtWarning, [mbOk], DEFAULT_DIAG_HEIGHT, backColor, fontColor);
      end
      else if (activeLayer.LayerIndex = TOPLAYER_IDX) and
        (((activeKbKey.OriginalKey.Key >= VK_A) and (activeKbKey.OriginalKey.Key <= VK_Z)) or
        ((activeKbKey.OriginalKey.Key >= VK_0) and (activeKbKey.OriginalKey.Key <= VK_9))) then
      begin
        ShowDialog('Tap and Hold', 'You cannot assign a Tap and Hold Action to these keys (A-Z, 0-9) on the Top Layer.',
          mtWarning, [mbOk], DEFAULT_DIAG_HEIGHT, backColor, fontColor);
      end
      else
      begin
        if (ShowTapAndHold(activeKbKey.TapAction, activeKbKey.HoldAction, activeKbKey.TimingDelay, backColor, fontColor)) then
        begin
          KeyModified := true;
          SetSaveState(ssModifed);
          keyService.SetTapAndHold(activeKbKey, FormTapAndHold.tapAction, FormTapAndHold.holdAction, FormTapAndHold.timingDelay);
          UpdatePnlButtonKey(activeKbKey, activePnlBtn);
          RefreshRemapInfo;
        end;
      end;
    end
    else
    begin
      createCustomButton(customBtns, 'OK', 150, nil, bkOK);
      createCustomButton(customBtns, 'Update Firmware', 150, @openFirwareWebsite);
      ShowDialog('Tap and Hold', 'To utilize Tap and Hold Actions, please download and install the latest firmware.',
        mtWarning, [], DEFAULT_DIAG_HEIGHT, backColor, fontColor, customBtns);
    end;
  end;
end;

procedure TFormMain.openFirwareWebsite(Sender: TObject);
begin
  OpenUrl('https://kinesis-ergo.com/support/advantage2/#firmware-updates');
end;

function TFormMain.GetKeyOtherLayer(keyIdx: integer): TKBKey;
var
  i: integer;
begin
  result := nil;
  if (activeLayer <> nil) then
  begin
    for i := 0 to keyService.KBLayers.Count - 1 do
    begin
      if (keyService.KBLayers[i].LayerIndex <> activeLayer.LayerIndex) then
      begin
        result := keyService.GetKbKeyByIndex(keyService.KBLayers[i], keyIdx);
        break;
      end;
    end;
  end;
end;

//On application restore, remove borderstyle
procedure TFormMain.OnRestore(Sender: TObject);
begin
  self.WindowState := wsNormal;
  {$ifdef Win32}self.BorderStyle := bsNone;{$endif}

  //To repaint red color in macro box
  SetMacroText(true);
end;

procedure TFormMain.SetMacroMode(value: boolean; reset: boolean = true);
begin
  MacroMode := false;

  if IsKeyLoaded then
  begin
    MacroMode := value and (activeKbKey.CanAssignMacro);
    MacroModified := false;
  end;

  if MacroMode then
    keyService.BackupMacro(activeKbKey);

  if reset then
    rgMacro1.Checked := true;

  EnableMacroBox(MacroMode);
  //rgMacro1.Checked := true;
  //pnlMacro.Enabled := MacroMode;
  btnCancelMacro.Enabled := MacroMode;
  btnDoneMacro.Enabled := MacroMode;
  btnBackspace.Enabled := MacroMode;
  btnClear.Enabled := MacroMode;
  btnSpecialActionsMacro.Enabled := MacroMode;
  btnCopy.Enabled := MacroMode;
  btnPaste.Enabled := MacroMode;
  bLShiftMacro.Enabled := MacroMode;
  bLCtrlMacro.Enabled := MacroMode;
  bLAltMacro.Enabled := MacroMode;
  bRAltMacro.Enabled := MacroMode;
  bRCtrlMacro.Enabled := MacroMode;
  bRShiftMacro.Enabled := MacroMode;
  ResetMacroCoTriggers;
  textMacroInput.Visible := IsKeyLoaded and (activeKbKey.ActiveMacro.Count = 0) and (activeKbKey.CanAssignMacro);

  if (MacroMode) and (IsKeyLoaded) and (activeKbKey.IsMacro) then
  begin
    LoadMacro;
    SetMacroText(true);
  end
  else
  begin
    lblMacro1.Font.Color := fontColor;
    lblMacro2.Font.Color := fontColor;
    lblMacro3.Font.Color := fontColor;
    loadingMacro := true;
    memoMacro.Text := '';
    slPlaybackSpeed.Position := DEFAULT_MACRO_SPEED;
    loadingMacro := false;
  end;
end;

procedure TFormMain.PnlButtonClick(Sender: TObject);
begin
  SetActivePnlButton(sender as TPanelBtn);
end;

function TFormMain.CheckSaveKey(canSave: boolean): boolean;
var
  msgResult: integer;
begin
  result := true;

  if IsKeyLoaded and (MacroModified) then
  begin
    if (canSave) then
    begin
      msgResult := ShowDialog('Apply changes',
        'This macro has been modified, do you want to apply these changes?', mtConfirmation,
        [mbYes, mbNo, mbCancel], DEFAULT_DIAG_HEIGHT, backColor, fontColor);
    if msgResult = mrYes then
      btnDoneMacro.Click
     else if msgResult = mrNo then
      btnCancelMacro.Click
    else
      result := false;
    end
    else
    begin
      ShowDialog('Macro', 'You must finish editing macro before proceeding', mtWarning, [mbOK], DEFAULT_DIAG_HEIGHT, backColor, fontColor);
      result := false;
    end;
  end
  else if (KeyModified) then
    btnDoneKey.Click;
end;

procedure TFormMain.SetActivePnlButton(pnlbutton: TPanelBtn);
begin
  if CheckSaveKey(true) then
  begin
    MacroModified := false;
    KeyModified := false;

    if IsKeyLoaded then
    begin
      activePnlBtn.SetButtonColor(clBlack);
      UpdatePnlButtonKey(activeKbKey, activePnlBtn, true);
    end;

    if (pnlbutton = activePnlBtn) or (pnlbutton = nil) then
    begin
      activePnlBtn := nil;
      activeKbKey := nil;
    end
    else
    begin
      activeKbKey := keyService.GetKbKeyByIndex(activeLayer, pnlbutton.Index);
      if (activeKbKey <> nil) and (activeKbKey.CanEdit) then
      begin
        activePnlBtn := pnlbutton;
        activePnlBtn.SetButtonColor(clBtnFace);
        keyService.BackupKbKey(activeKbKey);
      end
      else if (activeKbKey <> nil) then
      begin
        activeKbKey := nil;
        ShowDialog('Select key', 'You cannot edit this key', mtInformation, [mbOK], DEFAULT_DIAG_HEIGHT, backColor, fontColor);
      end;
    end;

    btnDoneKey.Enabled := IsKeyLoaded;
    btnCancelKey.Enabled := IsKeyLoaded;
    //jm btnSpecialActionsRemap.Enabled := IsKeyLoaded;
    btnResetKey.Enabled := IsKeyLoaded;

    UpdatePnlButtonKey(activeKbKey, activePnlBtn);

    SetMacroMode(IsKeyLoaded);
  end;
end;

procedure TFormMain.SetPnlButtonText(pnlbutton: TPanelBtn; panelText: string);
begin
  pnlbutton.Text := panelText;
end;

procedure TFormMain.SetSaveState(Value: TSaveState);
begin
  SaveState := Value;
  btnSave.Enabled := SaveState = ssModifed;
end;

function TFormMain.LoadStateSettings: boolean;
var
  errorMsg: string;
//const
//  TitleStateFile = 'Load State.txt File';
begin
  Result := False;
  loadingSettings := true;

  errorMsg := fileService.LoadStateSettings;

  if (errorMsg = '') then
  begin
    currentLayoutFile := GPedalsFilePath + fileService.StateSettings.StartupFile;
    slStatusReport.Position := fileService.StateSettings.StatusPlaySpeed;
    swAutoVDrive.Checked := fileService.StateSettings.VDriveStartup;
    slMacroSpeed.Position := fileService.StateSettings.MacroSpeed;
    swKeyClicks.Checked := fileService.StateSettings.KeyClickTone;
    swKeyTones.Checked := fileService.StateSettings.ToggleTone;
    Result := true;
  end;
  //else
  //  ShowDialog(TitleStateFile, errorMsg, mtError, [mbOK]);

  loadingSettings := false;
end;

procedure TFormMain.SaveStateSettings;
var
  errorMsg: string;
const
  TitleStateFile = 'Save State.txt File';
begin
  errorMsg := fileService.SaveStateSettings;

  if (errorMsg <> '') then
    ShowDialog(TitleStateFile, errorMsg, mtError, [mbOK], DEFAULT_DIAG_HEIGHT, backColor, fontColor);
end;

function TFormMain.LoadKeyboardLayout(layoutFile: string): boolean;
var
  errorMsg: string;
const
  TitleStateFile = 'Load Layout File';
begin
  Result := False;

  errorMsg := fileService.LoadLayoutFile(layoutFile);

  if (Pos(QWERTY_LAYOUT_TEXT, LowerCase(layoutFile)) <> 0) then
    keyService.LoadLayerList(LAYER_QWERTY)
  else if (Pos(DVORAK_LAYOUT_TEXT, LowerCase(layoutFile)) <> 0) then
    keyService.LoadLayerList(LAYER_DVORAK)
  else
    errorMsg := 'The file you have chosen is not a layout file. You must select a valid qwerty or dvorak layout file from the Active subfolder.';

  if (errorMsg = '') then
  begin
    activeLayer := nil;
    swLayerSwitch.Checked := true;
    lblLayoutFile.Caption := ExtractFileName(layoutFile);
    keyService.ConvertFromTextFileFmtAdv2(fileService.LayoutContent);
    SetActiveLayer(TOPLAYER_IDX);
    SetActivePnlButton(nil);
    RefreshRemapInfo;
    Result := true;
  end
  else
    ShowDialog(TitleStateFile, errorMsg, mtError, [mbOK], DEFAULT_DIAG_HEIGHT, backColor, fontColor);
end;

function TFormMain.CheckToSave: boolean;
var
  dialogResult: integer;
begin
  result := true;
  if SaveState = ssModifed then
  begin
    dialogResult := ShowDialog('Save',
      'Do you want to save changes?',
      mtConfirmation, [mbYes, mbNo], DEFAULT_DIAG_HEIGHT, backColor, fontColor);

    if dialogResult = mrYes then
      btnSave.Click
    else if dialogResult = mrNo then
      SetSaveState(ssNone)
    else
      result := false;
  end;
end;

function TFormMain.Save(allowNew: boolean = false; showSaveDialog: boolean = true): boolean;
var
  errorMsg: string;
  layoutContent: TStringList;
begin
  result := false;
  errorMsg := '';

  if (CheckSaveKey(true)) then
  begin
    layoutContent := keyService.ConvertToTextFileFmtAdv2;

    if fileService.SaveFile(currentLayoutFile, layoutContent, allowNew, errorMsg) then
    begin
      if (showSaveDialog) then
        ShowDialog('Save', 'Save done!' + #10 + 'Your changes will be implemented once the v-Drive has been closed. Before closing the v-Drive, exit the App and then right-click the “Kinesis-KB” drive in Windows Explorer and “Eject” it.', mtConfirmation, [mbOK]);
      SetSaveState(ssNone);
      result := true;
      SaveStateSettings;
    end
    else
      ShowDialog('Save', 'Error saving file: ' + errorMsg + #10 + 'Confirm that the v-drive is still open.',
        mtError, [mbOK]);
  end;
end;

//procedure TFormMain.SaveAs;
//begin
//  NeedInput := True;
//  SaveDialog.InitialDir := GPedalsFilePath;
//  if SaveDialog.Execute then
//  begin
//    currentLayoutFile := SaveDialog.FileName;
//    Save(true);
//    LoadKeyboardLayout(currentLayoutFile);
//  end;
//  NeedInput := False;
//end;

procedure TFormMain.LoadLayer(layer: TKBLayer);
var
  i: integer;
  pnlButton: TPanelBtn;
  aKbKey: TKBKey;
begin
  if (layer <> nil) then
  begin
    SetActivePnlButton(nil);
    for i := 0 to layer.KBKeyList.Count - 1 do
    begin
      aKbKey := layer.KBKeyList[i];
      pnlButton := GetPnlButtonByIndex(aKbKey.Index);
      UpdatePnlButtonKey(aKbKey, pnlButton);
    end;
  end;
end;

function TFormMain.GetPnlButtonByIndex(index: integer): TPanelBtn;
var
  i: integer;
  pnlButton: TPanelBtn;
  found: boolean;
begin
  i := 0;
  result := nil;
  found := false;
  While (i < panelBtnList.Count) and (not found) do
  begin
    if (panelBtnList[i] is TPanelBtn) then
    begin
      pnlButton := (panelBtnList[i] as TPanelBtn);
      if (pnlButton.Index = index) then
      begin
        result := pnlButton;
        found := true;
      end;
    end;
    inc(i);
  end;
end;

function TFormMain.GetCursorNextKey(cursorPos: integer): integer;
var
  i:integer;
  keyIdx: integer;
  newKeyIdx: integer;
begin
  result := -1;
  if (IsKeyLoaded and (cursorPos < LengthUTF8(memoMacro.Text))) then
  begin
    keyIdx := keyService.GetKeyAtPosition(activeKbKey.ActiveMacro, cursorPos);
    for i := cursorPos + 1 to LengthUTF8(memoMacro.Text) do
    begin
      newKeyIdx := keyService.GetKeyAtPosition(activeKbKey.ActiveMacro, i);
      if (keyIdx <> newKeyIdx) then
      begin
        result := i - 1;
        break;
      end;
    end;
  end;
end;

function TFormMain.GetCursorPrevKey(cursorPos: integer): integer;
var
  i:integer;
  keyIdx: integer;
  prevKeyIdx: integer;
begin
  result := -1; //cursorPos - 1;
  if (IsKeyLoaded and (cursorPos > 1)) then
  begin
    keyIdx := keyService.GetKeyAtPosition(activeKbKey.ActiveMacro, cursorPos);
    for i := cursorPos - 1 downto 0 do
    begin
      prevKeyIdx := keyService.GetKeyAtPosition(activeKbKey.ActiveMacro, i);
      if (keyIdx <> prevKeyIdx) then
      begin
        result := i;
        break;
      end;
    end;
  end;
end;

procedure TFormMain.SetModifiedKey(key: word; Modifiers: string; isMacro: boolean; bothLayers: boolean = false);
var
  aKbKeyOtherLayer: TKBKey;
  cursorPos: integer;
  cursorNext: integer;
  keyIdx: integer;
  isDiffKey: boolean;
  lastPos: boolean;
  keyAdded: TKey;
  textKey: string;
  isLongKey: boolean;
  macroText: string;
  aKeysPos: TKeysPos;
  nbKeystrokes: integer;
begin
  if IsKeyLoaded then
  begin
    if (isMacro) then
    begin
      nbKeystrokes := keyService.CountKeystrokes(activeKbKey.ActiveMacro);
      inc(nbKeystrokes);
      nbKeystrokes := nbKeystrokes + (keyService.CountModifiers(Modifiers) * 2);
      if (nbKeystrokes > MAX_KEYSTROKES_FS) then
        ShowDialog('Maximum Length Reached', 'Macros are limited to approximately 300 characters.',
          mtError, [mbOK], DEFAULT_DIAG_HEIGHT, backColor, fontColor)
      else if (not activeKbKey.CanAssignMacro) then
      begin
        ShowDialog('Macro', 'You cannot assign a macro to a modifier key', mtError, [mbOK]);
      end
      else
      begin
        activeKbKey.IsMacro := true;
        cursorPos := memoMacro.SelStart;
        cursorNext := GetCursorNextKey(cursorPos);
        lastPos := (cursorPos >= LengthUTF8(memoMacro.Text));
        if (cursorNext <> -1) and (cursorPos <> 0) then
          keyIdx := keyService.GetKeyAtPosition(activeKbKey.ActiveMacro, cursorNext + 1)
        else
          keyIdx := keyService.GetKeyAtPosition(activeKbKey.ActiveMacro, cursorPos + 1);
        keyAdded := keyService.AddKey(activeKbKey, key, Modifiers, keyIdx);
        if (keyAdded <> nil) then
        begin
          textKey := keyService.GetSingleKeyText(keyAdded, isLongKey);
          textKey := StringReplace(textKey, ' ', AnsiToUtf8(#$e2#$90#$a3), [rfReplaceAll]);
          MacroModified := true;

          //Insert key in memo
          if (cursorPos <> 0) then
          begin
            if (lastPos) then
              cursorPos := LengthUTF8(memoMacro.Text)
            else
            begin
              cursorPos := cursorNext;

              //Must insert at end
              if (cursorPos = -1) then
              begin
                cursorPos := LengthUTF8(memoMacro.Text);
                lastPos := true;
              end;
            end;
          end;
          memoMacro.SelStart := cursorPos;
          memoMacro.SelText := textKey;
          memoMacro.SelLength := 0;

          //Set cursor at end if inserted at the end
          if (lastPos) then
            memoMacro.SelStart := LengthUTF8(memoMacro.Text)
          else //Set cursor + length text otherwise
            memoMacro.SelStart := memoMacro.SelStart + LengthUTF8(textKey);

          //Color key text if is long key
          if (isLongKey) then
            memoMacro.SetRangeColor(cursorPos, LengthUTF8(textKey), clRed)
          else
            memoMacro.SetRangeColor(cursorPos, LengthUTF8(textKey), clDefault);

          textMacroInput.Visible := activeKbKey.ActiveMacro.Count = 0;
        end;
      end;
    end
    else if (not MacroModified) then
    begin
      isDiffKey := (activeKbKey.IsModified and (activeKbKey.ModifiedKey.Key <> key)) or
        ((not activeKbKey.IsModified) and (activeKbKey.OriginalKey.Key <> key));

      //Checks if key is really modified
      if (isDiffKey) then
      begin
        KeyModified := true;
        SetSaveState(ssModifed);
        keyService.SetKBKey(activeKbKey, key);
        if (bothLayers) then
        begin
          aKbKeyOtherLayer := GetKeyOtherLayer(activePnlBtn.Index);
          if aKbKeyOtherLayer <> nil then
            keyService.SetKBKey(aKbKeyOtherLayer, key);
        end;
        UpdatePnlButtonKey(activeKbKey, activePnlBtn);
      end;
    end;
  end;
end;

procedure TFormMain.SetMacroText(pushCursorToEnd: boolean; cursorPos: integer = -1);
var
  aKeysPos: TKeysPos;
begin
  if (IsKeyLoaded) and (activeKbKey.IsMacro) then
  begin
    //Disable visual effects on Macro before assigning text
    {$ifdef Win32}
    SendMessage(memoMacro.Handle, WM_SETREDRAW, WPARAM(False), 0);
    {$endif}
    {$ifdef Darwin}
    //jm todo SendMessage(memoMacro.Handle, LM_SETREDRAW, WPARAM(False), 0);
    {$endif}

    memoMacro.Text := keyService.GetMacroText(activeKbKey.ActiveMacro, aKeysPos);

    //Replace empty space with special space character
    memoMacro.Text := StringReplace(memoMacro.Text, ' ', AnsiToUtf8(#$e2#$90#$a3), [rfReplaceAll]);
    SetMemoTextColor(memoMacro, aKeysPos);

    //To show the cursor at the end
    if pushCursorToEnd then
    begin
      memoMacro.SelStart := Length(memoMacro.Text);
    end
    else if (cursorPos <> -1) then
    begin
      cursorPos := GetCursorNextKey(cursorPos);
      memoMacro.SelStart := cursorPos;
    end;

    textMacroInput.Visible := activeKbKey.ActiveMacro.Count = 0;
    //{$ifdef Darwin}  //MacOS
    //memoConfig.SetFocus;
    //{$endif}

    //Enable visual effects on Macro after assigning text
    {$ifdef Win32}
    SendMessage(memoMacro.Handle, WM_SETREDRAW, WPARAM(True), 0);
    {$endif}
    {$ifdef Darwin}
    //jm todo SendMessage(memoMacro.Handle, LM_SETREDRAW, WPARAM(True), 0);
    {$endif}

    memoMacro.Repaint;
  end;
end;

//Load macro from key
procedure TFormMain.LoadMacro;
begin
  loadingMacro := true;
  ResetMacroCoTriggers;
  lastKeyDown := 0; //Resets last key down
  lastKeyPressed := 0; //Resets last key pressed
  keyService.ClearModifiers; //Removes all modifiers from memory

  if (IsKeyLoaded) and (activeKbKey.IsMacro) then
  begin
    if (rgMacro1.Checked) then
      activeKbKey.ActiveMacro := activeKbKey.Macro1
    else if (rgMacro2.Checked) then
      activeKbKey.ActiveMacro := activeKbKey.Macro2
    else if (rgMacro3.Checked) then
      activeKbKey.ActiveMacro := activeKbKey.Macro3;

    if (activeKbKey.ActiveMacro.MacroSpeed >= MACRO_SPEED_MIN) and (activeKbKey.ActiveMacro.MacroSpeed <= MACRO_SPEED_MAX) then
    begin
      //tbSpeed.Position := activeKbKey.ActiveMacro.MacroSpeed;
      slPlaybackSpeed.Position := activeKbKey.ActiveMacro.MacroSpeed
    end
    else
    begin
      //tbSpeed.Position := DEFAULT_MACRO_SPEED;
      slPlaybackSpeed.Position := DEFAULT_MACRO_SPEED;
    end;

    if activeKbKey.Macro1.Count > 0 then
      lblMacro1.Font.Color := blueColor
    else
      lblMacro1.Font.Color := fontColor;
    if activeKbKey.Macro2.Count > 0 then
      lblMacro2.Font.Color := blueColor
    else
      lblMacro2.Font.Color := fontColor;
    if activeKbKey.Macro3.Count > 0 then
      lblMacro3.Font.Color := blueColor
    else
      lblMacro3.Font.Color := fontColor;

    SetCoTrigger(activeKbKey.ActiveMacro.CoTrigger1);
    SetCoTrigger(activeKbKey.ActiveMacro.CoTrigger2);
    SetCoTrigger(activeKbKey.ActiveMacro.CoTrigger3);
  end;
  loadingMacro := false;
end;

procedure TFormMain.SetCoTrigger(aKey: TKey);
begin
  if (aKey <> nil) then
  begin
    if (aKey.Key = VK_LSHIFT) then
      ActivateCoTrigger(bLShiftMacro);
    if (aKey.Key = VK_LCONTROL) then
      ActivateCoTrigger(bLCtrlMacro);
    if (aKey.Key = VK_LMENU) then
      ActivateCoTrigger(bLAltMacro);
    if (aKey.Key = VK_RSHIFT) then
      ActivateCoTrigger(bRShiftMacro);
    if (aKey.Key = VK_RCONTROL) then
      ActivateCoTrigger(bRCtrlMacro);
    if (aKey.Key = VK_RMENU) then
      ActivateCoTrigger(bRAltMacro);
  end;
end;

function TFormMain.IsKeyLoaded: boolean;
begin
  result := (activePnlBtn <> nil) and (activeKbKey <> nil);
end;

//Resets co-trigger buttons to default values
procedure TFormMain.ResetMacroCoTriggers;
begin
  ResetCoTrigger(bLShiftMacro);
  ResetCoTrigger(bLCtrlMacro);
  ResetCoTrigger(bLAltMacro);
  ResetCoTrigger(bRShiftMacro);
  ResetCoTrigger(bRCtrlMacro);
  ResetCoTrigger(bRAltMacro);
end;

procedure TFormMain.ActivateCoTrigger(coTriggerBtn: THSSpeedButton);
begin
  coTriggerBtn.Down := true;
  coTriggerBtn.Font.Bold := true;
  coTriggerBtn.Font.Color := KINESIS_BLUE;
end;

procedure TFormMain.ResetCoTrigger(coTriggerBtn: THSSpeedButton);
begin
  coTriggerBtn.Down := false;
  coTriggerBtn.Font.Bold := true;
  coTriggerBtn.Font.Color := clBlack;
end;

procedure TFormMain.SetMemoTextColor(aMemo: TRichMemo; aKeysPos: TKeysPos);
var
  i: integer;
begin
  //Reset to default color before setting red (MacOS bug)
  aMemo.SetRangeColor(0, Length(aMemo.Text), clDefault);

  if (aKeysPos <> nil) then
  begin
    for i := 0 to Length(aKeysPos) - 1 do
      aMemo.SetRangeColor(aKeysPos[i].iStart, aKeysPos[i].iEnd - aKeysPos[i].iStart, clRed);
  end;
end;

procedure TFormMain.SetActiveLayer(layerIdx: integer);
begin
  activeLayer := keyService.GetLayer(layerIdx);
  LoadLayer(activeLayer);
end;

procedure TFormMain.UpdatePnlButtonKey(kbKey: TKBKey; pnlButton: TPanelBtn; unselectKey: boolean = false);
var
  fontSize:integer;
  fontName: string;
begin
  fontSize := 0;
  fontName := '';

  if (kbKey <> nil) and (pnlButton <> nil) then
  begin
    if (kbKey.TapAndHold) then
    begin
      pnlButton.Text := kbKey.TapAction.OtherDisplayText + #10 + kbKey.HoldAction.OtherDisplayText;
      if (kbKey.TapAction.DisplaySize <> 0) then
        fontSize := kbKey.TapAction.DisplaySize
      else if (kbKey.HoldAction.DisplaySize <> 0) then
        fontSize := kbKey.HoldAction.DisplaySize;
      pnlButton.SetMainBorderColor(clSilver);
      pnlButton.BorderWidth := 1;
      pnlButton.SetFontColor(KINESIS_BLUE);
    end
    else if (kbKey.IsModified) and (not kbKey.IsMacro) then
    begin
      pnlButton.Text := kbKey.ModifiedKey.DisplayText;
      fontSize := kbKey.ModifiedKey.DisplaySize;
      fontName := kbKey.ModifiedKey.FontName;
      pnlButton.SetMainBorderColor(clSilver);
      pnlButton.BorderWidth := 1;
      pnlButton.SetFontColor(KINESIS_BLUE);
    end
    else if (not kbKey.IsModified) and (kbKey.IsMacro) then
    begin
      pnlButton.Text := kbKey.OriginalKey.DisplayText;
      fontSize := kbKey.OriginalKey.DisplaySize;
      fontName := kbKey.OriginalKey.FontName;
      pnlButton.SetMainBorderColor(KINESIS_BLUE);
      pnlButton.BorderWidth := 2;
      if (pnlButton <> activePnlBtn) or unselectKey then
        pnlButton.SetFontColor(clWhite)
      else
        pnlButton.SetFontColor(clBlack);
    end
    else if (kbKey.IsModified) and (kbKey.IsMacro) then
    begin
      pnlButton.Text := kbKey.ModifiedKey.DisplayText;
      fontSize := kbKey.ModifiedKey.DisplaySize;
      fontName := kbKey.ModifiedKey.FontName;
      pnlButton.SetMainBorderColor(KINESIS_BLUE);
      pnlButton.BorderWidth := 2;
      pnlButton.SetFontColor(KINESIS_BLUE);
    end
    else
    begin
      pnlButton.Text := kbKey.OriginalKey.DisplayText;
      fontSize := kbKey.OriginalKey.DisplaySize;
      fontName := kbKey.OriginalKey.FontName;
      pnlButton.SetMainBorderColor(clSilver);
      pnlButton.BorderWidth := 1;
      if (pnlButton <> activePnlBtn) or unselectKey then
        pnlButton.SetFontColor(clWhite)
      else
        pnlButton.SetFontColor(clBlack);
    end;

    //if (pnlButton <> activePnlBtn) and not(unselectKey) then
    //    pnlButton.SetFontColor(clWhite);

    if (fontSize > 0) then
      pnlButton.Font.Size := fontSize
    else
      pnlButton.Font.Size := pnlButton.DefaultFontSize;

    if (fontName <> '') then
      pnlButton.Font.Name := fontName
    else
      pnlButton.Font.Name := pnlButton.DefaultFontName;

    pnlButton.Repaint;
  end;
end;

procedure TFormMain.InitPnlButtons(container: TWinControl);
var
  i: integer;
  pnlButton: TPanelBtn;
begin
  for i := 0 to container.ControlCount - 1 do
  begin
    if (container.Controls[i] is TPanelBtn) then
    begin
      pnlButton := (container.Controls[i] as TPanelBtn);
      pnlButton.DefaultFontSize := pnlButton.Font.Size;
      pnlButton.DefaultFontName := pnlButton.Font.Name;
      pnlButton.OnClick := @PnlButtonClick;
      pnlButton.OnMouseDown := @PnlButtonMouseDown;
      panelBtnList.Add(pnlButton);
    end
    else if (container.Controls[i] is TGroupBox) then
      InitPnlButtons(container.Controls[i] as TGroupBox);
  end;
end;

procedure TFormMain.btnNewClick(Sender: TObject);
var
  fileName: string;
  layoutPosition: string;
  layoutType: string;
  loadAfterSave: boolean;
begin
  NeedInput := True;
  fileName := ShowNewFile(backColor, fontColor, fileService.AllowEditSettings, layoutType, layoutPosition, loadAfterSave);
  if (fileName <> '') then
  begin
    keyService.ResetLayout;
    currentLayoutFile := GPedalsFilePath + fileName;
    if (loadAfterSave) then
    begin
      fileService.SetStatupFile(fileName);
      SetSaveState(ssModifed);
    end;

    Save(true, false);
    LoadKeyboardLayout(currentLayoutFile);

    filename := ExtractFileNameWithoutExt(ExtractFileName(fileName));

    ShowDialog('New Layout', 'This layout has been saved to layout ' + layoutType + ', position: ' + layoutPosition,
        mtInformation, [mbOK], DEFAULT_DIAG_HEIGHT, backColor, fontColor);
  end;
  NeedInput := False;
end;

//function TFormMain.CheckSaveMacro(canSave: boolean): boolean;
//var
//  msgResult: integer;
//begin
//  result := true;
//
//  if IsKeyLoaded and MacroMode and MacroModified then
//  begin
//    if (canSave) then
//    begin
//      msgResult := ShowDialog('Apply changes',
//      'Macro editing in progress, apply changes?', mtConfirmation,
//      [mbYes, mbNo, mbCancel]);
//    if msgResult = mrYes then
//      btnDoneMacro.Click
//     else if msgResult = mrNo then
//      btnCancelMacro.Click
//    else
//      result := false;
//    end
//    else
//    begin
//      ShowDialog('Macro', 'You must finish editing macro before proceeding', mtWarning, [mbOK]);
//      result := false;
//    end;
//  end;
//end;

function TFormMain.LoadFirwareVersion: boolean;
var
  errorMsg: string;
//const
//  TitleStateFile = 'Load version.txt File';
begin
  Result := False;

  errorMsg := fileService.LoadFirmwareVersion;

  if (errorMsg = '') then
  begin
    //eFirmware.Text := fileService.FirmwareVersion;
    Result := true;
  end;
  //else
  //  ShowDialog(TitleStateFile, errorMsg, mtError, [mbOK]);
end;



end.

