//CreoSource Inc. (info@creosource.com)
//Constants and utility fonctions
unit u_const;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef Win32}Windows, {$endif}
  {$ifdef Darwin}LCLIntf, {$endif}
  lcltype, Classes, SysUtils, FileUtil, Controls, Graphics, character, LazUTF8, U_Keys, Buttons,
  HSSpeedButton;

type
  TPedal = (pNone, pLeft, pMiddle, pRight, pJack1, pJack2, pJack3, pJack4);

  TKeyState = (ksNone, ksDown, ksUp);

  TSaveState = (ssNone, ssModifed);

  TModifiers = (moShift, moAlt, moCtrl, moWin);

  TMouseEvent = (meNone, meLeftClick, meMiddleClick, meRightClick);

  TKeyMode = (kmSingle, kmMulti);

  TMsgDlgTypeApp  = (mtWarning, mtError, mtInformation, mtConfirmation,
                    mtCustom, mtFSEdge, mtFSPro);

  TTransitionState = (tsPressed, tsReleased); //KeySate Down or Up
  TExtendedState = (esNormal, esExtended);
  //Extended key, Normal or Extended (ex: Alt or Left Alt)
  PKeystrokeData = ^TKeystrokeData;
  TRestoreType = (rtAll, rtKey, rtMacro);

  TKeystrokeData = record
    VirtualKey: WPARAM;
    KeyStroke: LPARAM;
    KeyState: TKeyboardState;
  end;

  TLogFile = record
    FileName: string;
    FileDate: TDateTime;
  end;

  TLogFiles = array of TLogFile;

  TPedalText = record
    PedalText: string;
    MultiKey: boolean;
  end;

  TKeyPos = record
    iStart: integer;
    iEnd: integer;
  end;

  TStateSettings = record
    StartupFile: string;
    ThumbMode: string;
    KeyClickTone: boolean;
    ToggleTone: boolean;
    MacroDisable: boolean;
    MacroSpeed: integer;
    StatusPlaySpeed: integer;
    PowerUser: boolean;
    VDriveStartup: boolean;
    GameMode: boolean;
    ProgramKeyLock: boolean;
    LedMode: string;
  end;

  TAppSettings = record
    AppIntroMsg: boolean;
    SaveAsMsg: boolean;
    SaveMsg: boolean;
    MultiplayMsg: boolean;
    SpeedMsg: boolean;
    CopyMacroMsg: boolean;
    ResetKeyMsg: boolean;
  end;

  TCustomButton = record
    Caption: string;
    Width: integer;
    OnClick: TNotifyEvent;
    Kind: TBitBtnKind;
  end;

  TCustomButtons = array of TCustomButton;

  TKeysPos = array of TKeyPos;


var
  GApplication: integer; //Id of application
  GApplicationName: string; //Name of application
  GApplicationTitle: string; //Title of application
  GApplicationPath: string; //Application start path
  GPedalsFile: string; //Location of Pedals.txt file
  GPedalsFilePath: string; //Location of Pedals.txt file folder
  GLayoutFilePath: string; //Location of layout file folder (FS Edge)
  GSettingsFilePath: string; //Location of settings files (FS Edge)
  GFirmwareFilePath: string; //Location of settings files (FS Edge)
  GVersionFile: string; //Location of the version.txt file
  GStateFile: string; //Location of state.txt file
  GAppSettingsFile: string; //Location of appsettings.txt file
  KINESIS_BLUE: TColor; //Kinesis blue color
  KINESIS_BLUE_EDGE: TColor; //Kinesis blue color for FS app
  KINESIS_ORANGE: TColor; //Kinesis orange color
  KINESIS_LIGHT_GRAY_FS: TColor; //Kinesis kight gray color for FS app
  KINESIS_DARK_GRAY_FS: TColor; //Kinesis kight gray color for FS app

function MapShiftToVirutalKey(sShift: TShiftStateEnum): word;
function IsModifier(Key: word): boolean;
//function GetCharFromVirtualKey(Key: word): string;
procedure SortArry(var aLogFiles: TLogFiles);
procedure SetFont(aObject : TWinControl; fontName: string);
procedure SetFontColor(aObject: TWinControl; fontColor: TColor);
//function GetCharFromVKey(vkey: Word): string;
//function VKeytoWideString (Key : Word) : WideString;
function LengthUTF8(value: string): integer;
function KeyToUnicode(Key: Word; Shift: boolean = false; AltGr: boolean = false) : UnicodeString;
function ConvertToEnUS(Key: Word): integer;
function ConvertToLayout(Key: Word): integer;
function GetCurrentKeyoardLayout: string;
function GetConfigKeys: TKeyList;
function CanUseUnicode: boolean;
function ConvertToInt(value: string; defaultVal: integer = -1): integer;
function IsLeftShift(key: word): boolean;
function IsLeftCtrl(key: word): boolean;
function IsLeftAlt(key: word): boolean;
function IsLeftWin(key: word): boolean;
function IsRightShift(key: word): boolean;
function IsRightCtrl(key: word): boolean;
function IsRightAlt(key: word): boolean;
function GetIndexOfString(value: string; fileContent: TStringList): integer;
function GetPosOfNthString(value: string; search: string; nth: integer): integer;
function GetIndexOfNumber(value: string): integer;
function IsPreWin8: boolean;

const
  VK_NUMPADENTER = 10000; //User-defined keypad enter
  VK_MOUSE_LEFT = 10001; //User-defined left mouse
  VK_MOUSE_MIDDLE = 10002; //User-defined middle mouse
  VK_MOUSE_RIGHT = 10003; //User-defined right mouse
  VK_SPEED1 = 10005; //User-defined slow output (speed1)
  VK_SPEED3 = 10006; //User-defined default output (speed3)
  VK_125MS = 10007; //User-defined 125ms delay
  VK_500MS = 10008; //User-defined 500ms delay
  VK_CALC = 10009; //User-defined calculator
  VK_SHUTDOWN = 10010; //User-defined shutdown
  VK_DIF_PRESS_REL = 10011; //User-defined different press and release
  VK_SPEED5 = 10012; //User-defined fast output (speed5)
  VK_PROGRAM = 10013; //User-defined Program button (keyboard)
  VK_KEYPAD = 10014; //User-defined Keypad button (keyboard)
  VK_KEYPAD_SHIFT = 10016; //User-defined Keypad Shift button (keyboard)
  VK_FUNCTION = 10017; //User-defined Function/Fn button (keyboard)
  //NOT USED, VK_BRIGHTNESS = 10018; //User-defined Brightness button (keyboard)
  VK_NULL = 10019; //User-defined Null button (keyboard)
  VK_FN_TOGGLE = 10020; //User-defined Fn Toggle button (FS Edge)
  VK_FN_SHIFT = 10021; //User-defined Fn Shift button (FS Edge)
  VK_LED = 10022; //User-defined Led button (FS Edge)
  VK_MIC_MUTE = 10023; //User-defined Led button (FS Edge)
  VK_HK1 = 10024; //User-defined HK button (FS Edge)
  VK_HK2 = 10025; //User-defined HK button (FS Edge)
  VK_HK3 = 10026; //User-defined HK button (FS Edge)
  VK_HK4 = 10027; //User-defined HK button (FS Edge)
  VK_HK5 = 10028; //User-defined HK button (FS Edge)
  VK_HK6 = 10029; //User-defined HK button (FS Edge)
  VK_HK7 = 10030; //User-defined HK button (FS Edge)
  VK_HK8 = 10031; //User-defined HK button (FS Edge)
  VK_HK9 = 10032; //User-defined HK button (FS Edge)
  VK_HK10 = 10033; //User-defined HK button (FS Edge)
  VK_LSPACE = 10034; //User-defined Left Space (FS Edge)
  VK_RSPACE = 10035; //User-defined Left Space (FS Edge)
  VK_MOUSE_BTN4 = 10036; //User-defined mouse button 4
  VK_MOUSE_BTN5 = 10037; //User-defined mouse button 5
  VK_LCMD_MAC = 10038; //Left Cmd button (FS Edge / Mac Only)
  VK_LPEDAL = 10039; //Left pedal Adv2
  VK_MPEDAL = 10040; //Middle pedal Adv2
  VK_RPEDAL = 10041; //Right pedal Adv2
  VK_KEYPAD_TOGGLE = 10042; //User-defined Keypad Toggle button (keyboard)

  //FOR ADVANTAGE 2
  VK_KP_MENU = 10043;
  VK_KP_PLAY = 10044;
  VK_KP_PREV = 10045;
  VK_KP_NEXT = 10046;
  VK_KP_CALC = 10047;
  VK_KP_KPSHIFT = 10048;
  VK_KP_MUTE = 10049;
  VK_KP_VOLDOWN = 10050;
  VK_KP_VOLUP = 10051;

  VK_KP_NUMLCK = 10052;
  VK_KP_EQUAL = 10053;
  VK_KP_DIVIDE = 10054;
  VK_KP_MULT = 10055;

  VK_KP_0 = 10056;
  VK_KP_1 = 10057;
  VK_KP_2 = 10058;
  VK_KP_3 = 10059;
  VK_KP_4 = 10060;
  VK_KP_5 = 10061;
  VK_KP_6 = 10062;
  VK_KP_7 = 10063;
  VK_KP_8 = 10064;
  VK_KP_9 = 10065;
  VK_KP_MIN = 10066;
  VK_KP_PLUS = 10067;

  VK_KP_ENTER1 = 10068;
  VK_KP_ENTER2 = 10069;
  VK_KP_PERI = 10070;
  //FOR ADVANTAGE 2

  VK_HYPER = 10071; //Multimodifier
  VK_MEH = 10072; //Multimodifier

  MAPVK_VK_TO_VSC = 0;
  MAPVK_VSC_TO_VK = 1;
  MAPVK_VK_TO_CHAR = 2;
  MAPVK_VSC_TO_VK_EX = 3;

  TAG_MULTI_KEY = 1; //Tag for menu items that can only be used in multi keys
  TAG_SINGLE_KEY = 2; //Tag for menu items that can only be used in single keys
  TAG_BOTH_KEY = 3; //Tag for menu items that can be used in multi keys and single keys
  LOG_FILES_TO_KEEP = 100; //Number of log files to keep
  DOUBLECLICK_TEXT = 'lmouse-dblclick'; //text for left mouse double click

  //List of MenuItem names
  miLeftMouse = 'miLeftMouse';
  miMiddleMouse = 'miMiddleMouse';
  miRightMouse = 'miRightMouse';
  miMouseDblClick = 'miMouseDblClick';
  miCut = 'miCut';
  miCopy = 'miCopy';
  miPaste = 'miPaste';
  miSelectAll = 'miSelectAll';
  miUndo = 'miUndo';
  miWebFwd = 'miWebFwd';
  miWebBack = 'miWebBack';
  miAltTab = 'miAltTab';
  miCtrlAltDel = 'miCtrlAltDel';
  miWinKey = 'miWinKey';
  miCmdTab = 'miCmdTab';
  miSnipClip = 'miSnipClip';
  miSnipFile = 'miSnipFile';
  miForceQuit = 'miForceQuit';
  miVolMute = 'miVolMute';
  miVolMin = 'miVolMin';
  miVolPlus = 'miVolPlus';
  miPlay = 'miPlay';
  miPrev = 'miPrev';
  miNext = 'miNext';
  miCalc = 'miCalc';
  miSpeed1 = 'miSpeed1';
  miSpeed3 = 'miSpeed3';
  miSpeed5 = 'miSpeed5';
  mi125 = 'mi125';
  mi500 = 'mi500';
  miDifPressRel = 'miDifPressRel';
  TEXT_SEPARATOR = '-'; //For popupmenu separator

  //Modifier short values
  SHIFT_MOD = 'S ';
  L_SHIFT_MOD = 'LS';
  R_SHIFT_MOD = 'RS';
  CTRL_MOD = 'C ';
  L_CTRL_MOD = 'LC';
  R_CTRL_MOD = 'RC';
  ALT_MOD = 'A ';
  L_ALT_MOD = 'LA';
  R_ALT_MOD = 'RA';
  WIN_MOD = 'W ';
  L_WIN_MOD = 'LW';
  R_WIN_MOD = 'RW';

  //Values for keys from teKeyboardHookProcxt file
  LEFT_PEDAL_TEXT = 'lpedal';
  MIDDLE_PEDAL_TEXT = 'mpedal';
  RIGHT_PEDAL_TEXT = 'rpedal';
  JACK1_PEDAL_TEXT = 'jack1';
  JACK2_PEDAL_TEXT = 'jack2';
  JACK3_PEDAL_TEXT = 'jack3';
  JACK4_PEDAL_TEXT = 'jack4';

  LEFT_PEDAL_TEXT_SINGLE = '[lpedal]';
  MIDDLE_PEDAL_TEXT_SINGLE = '[mpedal]';
  RIGHT_PEDAL_TEXT_SINGLE = '[rpedal]';
  JACK1_PEDAL_TEXT_SINGLE = '[jack1]';
  JACK2_PEDAL_TEXT_SINGLE = '[jack2]';
  JACK3_PEDAL_TEXT_SINGLE = '[jack3]';
  JACK4_PEDAL_TEXT_SINGLE = '[jack4]';

  LEFT_PEDAL_TEXT_MULTI = '{lpedal}';
  MIDDLE_PEDAL_TEXT_MULTI = '{mpedal}';
  RIGHT_PEDAL_TEXT_MULTI = '{rpedal}';
  JACK1_PEDAL_TEXT_MULTI = '{jack1}';
  JACK2_PEDAL_TEXT_MULTI = '{jack2}';
  JACK3_PEDAL_TEXT_MULTI = '{jack3}';
  JACK4_PEDAL_TEXT_MULTI = '{jack4}';

  //Constants for loading/saving
  SK_START = '[';
  SK_END = ']';
  MK_START = '{';
  MK_END = '}';
  KEYPAD_KEY = 'kp-';
  KEYPAD_KEY_EDGE = 'fn ';
  KP_PREFIX_LENGTH = 3;

  //Various constants
  DIFF_PRESS_REL_TEXT = '{ }';
  MACRO_SPEED_TEXT = 'speed';
  MACRO_SPEED_TEXT_EDGE = 's';
  MACRO_REPEAT_EDGE = 'x';
  GLOBAL_SPEED = 'Global';
  MACRO_FREQ_MIN = 0;
  MACRO_FREQ_MAX = 9;
  DEFAULT_MACRO_FREQ = 0;
  MACRO_SPEED_MIN = 0;
  MACRO_SPEED_MAX = 9;
  DEFAULT_MACRO_SPEED = 0;
  QWERTY_LAYOUT_TEXT = 'qwerty';
  DVORAK_LAYOUT_TEXT = 'dvorak';
  UNICODE_FONT = 'Cambria Math';
  DEFAULT_DIAG_HEIGHT = 175;

  //Keyboard layouts
  ENGLISH_US_LAYOUT_NAME = '00000409';
  //ENGLISH_US_VAL = 67699721;
  ENGLISH_US_LAYOUT_VALUE = 1033;

  //Keyboard layers
  TOPLAYER_IDX = 0;
  BOTLAYER_IDX = 1;
  LAYER_QWERTY = 0;
  LAYER_DVORAK = 1;

  //Application IDs
  APPL_PEDAL = 0;
  APPL_ADV2 = 1;
  APPL_FSEDGE = 2;
  APPL_FSPRO = 3;

  //Misc constants
  USER_MANUAL_FSEDGE = 'User Manual-SmartSet App.pdf';
  USER_MANUAL_ADV2 = 'SmartSet App Help.pdf';
  KB_SETTINGS_FILE = 'kbd_settings.txt';
  APP_SETTINGS_FILE = 'app_settings.txt';
  VERSION_FILE = 'version.txt';
  MACRO_COUNT_FS = 3;
  MAX_MACRO_FS = 24;
  DISABLE_NOTIF = 100;
  FS_FILENAME = 'layout';
  PITCH_BLACK = 'P';
  BREATHE = 'B';
  FSEDGE_TUTORIAL = 'https://www.youtube.com/playlist?list=PLJql6LYXw-uOcHFihFhnZhJGb854SRy7Z';
  FSPRO_TUTORIAL = 'https://www.youtube.com/playlist?list=PLcsFMh_3_h0Z7Gx0T5N7TTzceorPHXJr5';
  ADV2_TUTORIAL = 'https://www.youtube.com/playlist?list=PLcsFMh_3_h0aNmELoR6kakcNf7AInoEfW';
  ADV2_MANUAL = 'https://kinesis-ergo.com/support/advantage2/#manuals';
  MODEL_NAME_FSPRO = 'FS PRO';
  MODEL_NAME_FSEDGE = 'FS EDGE';
  MAX_KEYSTROKES_FS = 300;
  ADV2_2MB = '2MB';
  ADV2_4MB = '4MB';
  MIN_TIMING_DELAY = 1;
  MAX_TIMING_DELAY = 999;
  TAP_AND_HOLD = 't&h';
  DEFAULT_SPEED_TAP_HOLD = 250;
  MAX_TAP_HOLD = 10;

implementation

//Maps a Shift state to a virtual key
function MapShiftToVirutalKey(sShift: TShiftStateEnum): word;
begin
  if sShift = ssShift then
    Result := LCLType.VK_SHIFT
  else if sShift = ssCtrl then
    Result := LCLType.VK_CONTROL
  else if sShift = ssAlt then
    Result := LCLType.VK_MENU
  else if sShift = ssMeta then
    Result := LCLType.VK_LWIN
  else
    Result := 0;
end;

//Returns true if modifier key is pressed (left or right alt, shift, windows, ctrl)
function IsModifier(Key: word): boolean;
begin
  Result := False;

  if (Key = VK_MENU) or (Key = VK_LMENU) or (Key = VK_RMENU) or
    (Key = VK_SHIFT) or (Key = VK_LSHIFT) or (Key = VK_RSHIFT) or
    (Key = VK_CONTROL) or (Key = VK_LCONTROL) or (Key = VK_RCONTROL) or
    (Key = VK_LWIN) or (Key = VK_RWIN) then
    Result := True;
end;

//Sorts an array of log files oldest files first
procedure SortArry(var aLogFiles: TLogFiles);
var
  tempLogFile: TLogFile;
  Current, Next, Records: integer;
begin
  Records := Length(aLogFiles);
  tempLogFile := aLogFiles[0];
  for Current := 0 to Records - 1 do
  begin
    tempLogFile := aLogFiles[Current];
    for Next := Current + 1 to Records - 1 do
    begin
      if (aLogFiles[Next].FileDate < aLogFiles[Current].FileDate) then
      begin
        aLogFiles[Current] := aLogFiles[Next];
        aLogFiles[Next] := tempLogFile;
      end;
    end;
  end;
end;

procedure SetFont(aObject: TWinControl; fontName: string);
var
  i:integer;
begin
  aObject.Font.Name := fontName;
  for i := 0 to aObject.ComponentCount - 1 do
  begin
    if aObject.Components[i].InheritsFrom(TWinControl) then
       TWinControl(aObject.Components[i]).Font.Name := fontName;
    if aObject.Components[i].InheritsFrom(TGraphicControl) then
       TGraphicControl(aObject.Components[i]).Font.Name := fontName;
  end;
end;

procedure SetFontColor(aObject: TWinControl; fontColor: TColor);
var
  i:integer;
begin
  aObject.Font.Color := fontColor;
  for i := 0 to aObject.ComponentCount - 1 do
  begin
    if aObject.Components[i].InheritsFrom(TWinControl) then
       TWinControl(aObject.Components[i]).Font.Color := fontColor;
    if aObject.Components[i].InheritsFrom(TGraphicControl) and not(aObject.Components[i].InheritsFrom(THSSpeedButton)) then
       TGraphicControl(aObject.Components[i]).Font.Color := fontColor;
  end;
end;

//Decodes UTF8 string to get real length (UTF8 caracters are 2 bytes in length)
function LengthUTF8(value: string): integer;
begin
  result := Length(UTF8Decode(value));
end;

//Converts key to unicode value
function KeyToUnicode(Key: Word; Shift: boolean = false; AltGr: boolean = false): UnicodeString;
var
   keyboardState: array[0..256] of Byte;
   UnicodeKeys : array[0..256] of WideChar;
   i:integer;
begin
  //Reset arrays
  for i := 0 to 256 do
      keyboardState[i] := 0;

  for i := 0 to 256 do
      UnicodeKeys[i] := #0;

  //Set Shift pressed
  if Shift then
    keyboardState[VK_SHIFT] := 128;

  //Set AltGr or Alt+Ctrl pressed
  if AltGr then
  begin
    keyboardState[VK_CONTROL] := 128;
    keyboardState[VK_MENU] := 128;
  end;

  //Call Windows ToUnicode function
  //Must call twice to eliminate Dead-Keys
  {$ifdef Win32}
  ToUnicode(Key, 0, keyboardState, UnicodeKeys, 256, 0);
  ToUnicode(Key, 0, keyboardState, UnicodeKeys, 256, 0);
  {$endif}

  //Return lower case char converted to utf-8
  try
    if (UnicodeKeys[0] = #0) then
       result := ''
    else
       result := LazUTF8.SysToUTF8(UnicodeKeys[0]);
  except
    result := ''
  end;
end;

//Converts a key to English US equivalent
function ConvertToEnUS(Key: Word): integer;
var
   scanCode: word;
begin
  {$ifdef Win32}
  //Gets currenty keyboard scan Code
  scanCode := MapVirtualKey(Key, 0);

  //Converts to English US virtual code
  result := MapVirtualKeyEx(scanCode, 1, ENGLISH_US_LAYOUT_VALUE);
  {$endif}
end;

//Converts an English US key to current keyboard layout equivalent
function ConvertToLayout(Key: Word): integer;
var
   scanCode: word;
begin
  {$ifdef Win32}
  //Gets English US Scan Code
  scanCode := MapVirtualKeyEx(Key, 0, ENGLISH_US_LAYOUT_VALUE);

  //Converts to current keyboard virtual code
  result := MapVirtualKey(scanCode, 1);
  {$endif}
end;

//Return KeyboardLayout for the user
function GetCurrentKeyoardLayout: string;
var
  {$ifdef Win32}LayoutName: array [0 .. KL_NAMELENGTH + 1] of Char; {$endif}
  layout: string;
begin
  layout := ENGLISH_US_LAYOUT_NAME;
  {$ifdef Win32}
  try
    if (GetKeyboardLayoutName(@LayoutName)) then
      layout := StrPas(LayoutName);
  except
  end;
  {$endif}
  result := layout;
end;

//Checks if we can use Unicode characters
function CanUseUnicode: boolean;
begin
  result := false;
  {$ifdef Win32}
  result := (Win32MajorVersion >= 10); //Windows 10 and up
  {$endif}
end;

//Checks if we can use Unicode characters
function IsPreWin8: boolean;
begin
  result := false;
  {$ifdef Win32}
  result := (Win32MajorVersion <= 6) and (Win32MinorVersion < 2);
  {$endif}
end;

//Converts a string to Int, if error returns -1
function ConvertToInt(value: string; defaultVal: integer = -1): integer;
var
  tempVal, code: integer;
begin
  Val(value, tempVal, code);
  if (code = 0) then
    result := tempVal
  else
    result := defaultVal;
end;

function IsLeftShift(key: word): boolean;
begin
  result := (key = VK_LSHIFT);
end;

function IsLeftCtrl(key: word): boolean;
begin
  result := (key = VK_LCONTROL);
end;

function IsLeftAlt(key: word): boolean;
begin
  result := (key = VK_LMENU);
end;

function IsLeftWin(key: word): boolean;
begin
  result := (key = VK_LWIN);
end;

function IsRightShift(key: word): boolean;
begin
  result := (key = VK_RSHIFT);
end;

function IsRightCtrl(key: word): boolean;
begin
  result := (key = VK_RCONTROL);
end;

function IsRightAlt(key: word): boolean;
begin
  result := (key = VK_RMENU);
end;

function GetIndexOfString(value: string; fileContent: TStringList): integer;
var
  i:integer;
begin
  result := -1;
  if (fileContent <> nil) then
  begin
    for i := 0 to fileContent.Count - 1 do
    begin
      if (Pos(AnsiLowerCase(value), AnsiLowerCase(fileContent[i])) > 0) then
      begin
        result := i;
        break;
      end;
    end;
  end;
end;

function GetPosOfNthString(value: string; search: string; nth: integer): integer;
var
  strLength: integer;
  idx: integer;
  searchCount: integer;
begin
  result := -1;

  searchCount := 0;
  strLength := Length(value);
  idx := 1;
  while ((idx <= strLength) and (searchCount < nth)) do
  begin
    if (AnsiUpperCase(Copy(value, idx, Length(search))) = AnsiUpperCase(search)) then
      inc(searchCount);

    if (searchCount = nth) then
      result := idx;

    inc(idx);
  end;
end;

function GetIndexOfNumber(value: string): integer;
var
  i:integer;
begin
  result := -1;
  for i := 1 to Length(value) do
  begin
    if IsNumber(value, i) then
    begin
      result := i;
      break;
    end;
  end;
end;

//Initializes and returns list of Configurable Keys
function GetConfigKeys: TKeyList;
var
  i: integer;
  ConfigKeys: TKeyList;
  mediaFontSize: integer;
  smallFontSize: integer;
begin
  ConfigKeys := TKeyList.Create;

  smallFontSize := 8;
  //{$ifdef Darwin}smallFontSize := 8;{$endif}

  ConfigKeys.Clear;
  //Control keys
  if (GApplication in [APPL_FSEDGE, APPL_FSPRO]) then
    ConfigKeys.Add(TKey.Create(VK_ESCAPE, 'esc', 'Esc'))
  else
    ConfigKeys.Add(TKey.Create(VK_ESCAPE, 'escape', 'Esc'));
  ConfigKeys.Add(TKey.Create(VK_PAUSE, 'pause', 'Pause' + #10 + 'Break', '', '', '', false, false, '', true, false, smallFontSize));
  ConfigKeys.Add(TKey.Create(VK_PRINT, 'prtscr', 'Print' + #10 + 'Scrn', '', '', '', false, false, '', true, false, smallFontSize)); //Old print key...
  if (GApplication in [APPL_FSEDGE, APPL_FSPRO]) then
    ConfigKeys.Add(TKey.Create(VK_SNAPSHOT, 'prnt', 'Print' + #10 + 'Scrn', '', '', '', false, false, '', true, false, smallFontSize)) //Print screen key
  else
    ConfigKeys.Add(TKey.Create(VK_SNAPSHOT, 'prtscr', 'Print' + #10 + 'Scrn', '', '', '', false, false, '', true, false, smallFontSize)); //Print screen key
  if (GApplication in [APPL_FSEDGE, APPL_FSPRO]) then
    ConfigKeys.Add(TKey.Create(VK_SCROLL, 'scrlk', 'Scroll' + #10 + 'Lock', '', '', '', false, false, '', true, false, smallFontSize))
  else
    ConfigKeys.Add(TKey.Create(VK_SCROLL, 'scroll', 'Scroll' + #10 + 'Lock', '', '', '', false, false, '', true, false, smallFontSize));
  ConfigKeys.Add(TKey.Create(VK_TAB, 'tab', 'Tab'));
  ConfigKeys.Add(TKey.Create(VK_CAPITAL, 'caps', 'Caps' + #10 + 'Lock', '', '', '', false, false, '', true, false, smallFontSize));
  //When multi-key and no modifiers, show empty space
  if (GApplication in [APPL_FSEDGE, APPL_FSPRO]) then
    ConfigKeys.Add(TKey.Create(VK_SPACE, 'spc', 'Space', 'spc', ' '))
  else
    ConfigKeys.Add(TKey.Create(VK_SPACE, 'space', 'Space', 'space', ' '));///UTFString(#$e2#$90#$a3)));
  ConfigKeys.Add(TKey.Create(VK_LSPACE, 'lspc', 'Space', 'lspc', ' ')); //User-Defined key for FSEdge
  ConfigKeys.Add(TKey.Create(VK_RSPACE, 'rspc', 'Space', 'rspc', ' ')); //User-Defined key for FSEdge
  if (GApplication in [APPL_FSEDGE, APPL_FSPRO]) then
    ConfigKeys.Add(TKey.Create(VK_INSERT, 'insert', 'Insert', 'ins'))
  else if (GApplication in [APPL_ADV2]) then
    ConfigKeys.Add(TKey.Create(VK_INSERT, 'insert', 'Insert', 'kp-insert'))
  else
    ConfigKeys.Add(TKey.Create(VK_INSERT, 'insert', 'Insert'));
  ConfigKeys.Add(TKey.Create(VK_HOME, 'home', 'Home'));
  ConfigKeys.Add(TKey.Create(VK_END, 'end', 'End'));
  if (GApplication in [APPL_FSEDGE, APPL_FSPRO]) then
    ConfigKeys.Add(TKey.Create(VK_NEXT, 'pdn', 'Page' + #10 + 'Down'))
  else
    ConfigKeys.Add(TKey.Create(VK_NEXT, 'pdown', 'Page' + #10 + 'Down'));
  ConfigKeys.Add(TKey.Create(VK_PRIOR, 'pup', 'Page' + #10 + 'Up'));

  if (GApplication in [APPL_FSEDGE, APPL_FSPRO]) then
    ConfigKeys.Add(TKey.Create(VK_RIGHT, 'rght', UnicodeToUTF8(8594), '', '', '', false, false, '', true, false, 10, UNICODE_FONT))
  else
    ConfigKeys.Add(TKey.Create(VK_RIGHT, 'right', UnicodeToUTF8(8594), '', '', '', false, false, '', true, false, 16, UNICODE_FONT));

  if (GApplication in [APPL_FSEDGE, APPL_FSPRO]) then
    ConfigKeys.Add(TKey.Create(VK_LEFT, 'lft', UnicodeToUTF8(8592), '', '', '', false, false, '', true, false, 10, UNICODE_FONT))
  else
    ConfigKeys.Add(TKey.Create(VK_LEFT, 'left', UnicodeToUTF8(8592), '', '', '', false, false, '', true, false, 16, UNICODE_FONT));

  if (GApplication in [APPL_FSEDGE, APPL_FSPRO]) then
    ConfigKeys.Add(TKey.Create(VK_UP, 'up', UnicodeToUTF8(8593), '', '', '', false, false, '', true, false, 10, UNICODE_FONT))
  else
    ConfigKeys.Add(TKey.Create(VK_UP, 'up', UnicodeToUTF8(8593), '', '', '', false, false, '', true, false, 16, UNICODE_FONT));

  if (GApplication in [APPL_FSEDGE, APPL_FSPRO]) then
    ConfigKeys.Add(TKey.Create(VK_DOWN, 'dwn', UnicodeToUTF8(8595), '', '', '', false, false, '', true, false, 10, UNICODE_FONT))
  else
    ConfigKeys.Add(TKey.Create(VK_DOWN, 'down', UnicodeToUTF8(8595), '', '', '', false, false, '', true, false, 16, UNICODE_FONT));

  ConfigKeys.Add(TKey.Create(VK_SHIFT, 'Shift', 'Shift', 'shift'));
  if (GApplication in [APPL_FSEDGE, APPL_FSPRO]) then
  begin
    ConfigKeys.Add(TKey.Create(VK_LSHIFT, 'lshift', 'Left' + #10 + 'Shift', 'lshft'));
    ConfigKeys.Add(TKey.Create(VK_RSHIFT, 'rshift', 'Right' + #10 + 'Shift', 'rshft'));
  end
  else
  begin
    ConfigKeys.Add(TKey.Create(VK_LSHIFT, 'lshift', 'Left' + #10 + 'Shift'));
    ConfigKeys.Add(TKey.Create(VK_RSHIFT, 'rshift', 'Right' + #10 + 'Shift'));
  end;
  ConfigKeys.Add(TKey.Create(VK_CONTROL, 'Ctrl', 'Ctrl', 'ctrl'));
  //if (GApplication in [APPL_FSEDGE, APPL_FSPRO]) then
  //begin
  //  ConfigKeys.Add(TKey.Create(VK_LCONTROL, 'lctrl', 'Ctrl'));
  //  ConfigKeys.Add(TKey.Create(VK_RCONTROL, 'rctrl', 'Ctrl'));
  //end
  //else
  //begin
    ConfigKeys.Add(TKey.Create(VK_LCONTROL, 'lctrl', 'Left' + #10 + 'Ctrl'{$ifdef Darwin}, '', '', '', false, false, '', true, False, 0, '', 'Left Control' {$endif} ));
    ConfigKeys.Add(TKey.Create(VK_RCONTROL, 'rctrl', 'Right' + #10 + 'Ctrl'{$ifdef Darwin}, '', '', '', false, false, '', true, False, 0, '', 'Right Control' {$endif} ));
  //end;
  ConfigKeys.Add(TKey.Create(VK_NUMLOCK, 'numlk', 'Num' + #10 + 'Lock'));
  ConfigKeys.Add(TKey.Create(VK_KP_NUMLCK, 'numlk', 'Num' + #10 + 'Lock'));
  ConfigKeys.Add(TKey.Create(VK_APPS, 'menu', 'PC' + #10 + 'Menu'));
  ConfigKeys.Add(TKey.Create(VK_KP_MENU, 'menu', 'PC' + #10 + 'Menu'));
  //Windows
  {$ifdef Win32}

  if (GApplication in [APPL_FSEDGE, APPL_FSPRO]) then
    ConfigKeys.Add(TKey.Create(VK_RETURN, 'ent', ' Enter'))
  else
    ConfigKeys.Add(TKey.Create(VK_RETURN, 'enter', ' Enter'));
  if (GApplication in [APPL_FSEDGE, APPL_FSPRO]) then
    ConfigKeys.Add(TKey.Create(VK_BACK, 'bspc',  'Back' + #10 + 'Space'))
  else
    ConfigKeys.Add(TKey.Create(VK_BACK, 'bspace',  'Back' + #10 + 'Space'));
  if (GApplication in [APPL_FSEDGE, APPL_FSPRO]) then
    ConfigKeys.Add(TKey.Create(VK_DELETE, 'del', 'Delete'))
  else
    ConfigKeys.Add(TKey.Create(VK_DELETE, 'delete', 'Delete'));

  ConfigKeys.Add(TKey.Create(VK_MENU, 'alt', 'Alt'));
  //if (GApplication in [APPL_FSEDGE, APPL_FSPRO]) then
  //begin
  //  ConfigKeys.Add(TKey.Create(VK_LMENU, 'lalt', 'Alt'));
  //  ConfigKeys.Add(TKey.Create(VK_RMENU, 'ralt', 'Alt'));
  //end
  //else
  //begin
    ConfigKeys.Add(TKey.Create(VK_LMENU, 'lalt', 'Left' + #10 + 'Alt'));
    ConfigKeys.Add(TKey.Create(VK_RMENU, 'ralt', 'Right' + #10 + 'Alt'));
  //end;
  if (GApplication = APPL_PEDAL) then
  begin
    ConfigKeys.Add(TKey.Create(VK_LWIN, 'win', 'Left' + #10 + 'Win'));
    ConfigKeys.Add(TKey.Create(VK_RWIN, 'win', 'Right' + #10 + 'Win'));
  end
  //else if (GApplication in [APPL_FSEDGE, APPL_FSPRO]) then
  //begin
  //  ConfigKeys.Add(TKey.Create(VK_LWIN, 'lwin', 'Win'));
  //  ConfigKeys.Add(TKey.Create(VK_RWIN, 'rwin', 'Win'));
  //end
  else
  begin
    ConfigKeys.Add(TKey.Create(VK_LWIN, 'lwin', 'Left' + #10 + 'Win'));
    ConfigKeys.Add(TKey.Create(VK_RWIN, 'rwin', 'Right' + #10 + 'Win'));
  end;
  {$endif}

  //MacOS
  {$ifdef Darwin}
  if (GApplication in [APPL_FSEDGE, APPL_FSPRO]) then
    ConfigKeys.Add(TKey.Create(VK_RETURN, 'ent', 'return'))
  else
    ConfigKeys.Add(TKey.Create(VK_RETURN, 'enter', 'return'));
  if (GApplication in [APPL_FSEDGE, APPL_FSPRO]) then
    ConfigKeys.Add(TKey.Create(VK_BACK, 'bspc',  'delete'))
  else
    ConfigKeys.Add(TKey.Create(VK_BACK, 'bspace',  'delete'));
  if (GApplication in [APPL_FSEDGE, APPL_FSPRO]) then
    ConfigKeys.Add(TKey.Create(VK_DELETE, 'del', 'fwd-' + #10 + 'delete'))
  else
    ConfigKeys.Add(TKey.Create(VK_DELETE, 'delete', 'fwd-' + #10 + 'delete'));
  ConfigKeys.Add(TKey.Create(VK_MENU, 'Opt', 'Opt', 'alt'));
  ConfigKeys.Add(TKey.Create(VK_LMENU, 'lalt', 'Left' + #10 + 'Opt', '', '', '', false, false, '', true, False, 0, '', 'Left Option'));
  ConfigKeys.Add(TKey.Create(VK_RMENU, 'ralt', 'Right' + #10 + 'Opt', '', '', '', false, false, '', true, False, 0, '', 'Right Option'));
  ConfigKeys.Add(TKey.Create(VK_LWIN, 'Cmd', 'Cmd', 'lwin'));
  ConfigKeys.Add(TKey.Create(VK_LCMD_MAC, 'lwin', 'Left' + #10 + 'Cmd'));
  ConfigKeys.Add(TKey.Create(VK_RWIN, 'rwin', 'Right' + #10 + 'Cmd'));
  {$endif}

  //F1 to F24
  ConfigKeys.Add(TKey.Create(VK_F1, 'F1'));
  ConfigKeys.Add(TKey.Create(VK_F2, 'F2'));
  ConfigKeys.Add(TKey.Create(VK_F3, 'F3'));
  ConfigKeys.Add(TKey.Create(VK_F4, 'F4'));
  ConfigKeys.Add(TKey.Create(VK_F5, 'F5'));
  ConfigKeys.Add(TKey.Create(VK_F6, 'F6'));
  ConfigKeys.Add(TKey.Create(VK_F7, 'F7'));
  ConfigKeys.Add(TKey.Create(VK_F8, 'F8'));
  ConfigKeys.Add(TKey.Create(VK_F9, 'F9'));
  ConfigKeys.Add(TKey.Create(VK_F10, 'F10'));
  ConfigKeys.Add(TKey.Create(VK_F11, 'F11'));
  ConfigKeys.Add(TKey.Create(VK_F12, 'F12'));
  ConfigKeys.Add(TKey.Create(VK_F13, 'F13'));
  ConfigKeys.Add(TKey.Create(VK_F14, 'F14'));
  ConfigKeys.Add(TKey.Create(VK_F15, 'F15'));
  ConfigKeys.Add(TKey.Create(VK_F16, 'F16'));
  ConfigKeys.Add(TKey.Create(VK_F17, 'F17'));
  ConfigKeys.Add(TKey.Create(VK_F18, 'F18'));
  ConfigKeys.Add(TKey.Create(VK_F19, 'F19'));
  ConfigKeys.Add(TKey.Create(VK_F20, 'F20'));
  ConfigKeys.Add(TKey.Create(VK_F21, 'F21'));
  ConfigKeys.Add(TKey.Create(VK_F22, 'F22'));
  ConfigKeys.Add(TKey.Create(VK_F23, 'F23'));
  ConfigKeys.Add(TKey.Create(VK_F24, 'F24'));

  //Character keys
  if (GApplication in [APPL_FSEDGE, APPL_FSPRO]) then
  begin
    ConfigKeys.Add(TKey.Create(VK_LCL_EQUAL, '=', '= +', '=', '=', '+', true, true));
    ConfigKeys.Add(TKey.Create(VK_LCL_MINUS, '-', '- _', 'hyph', '-', '_', true, true));
    ConfigKeys.Add(TKey.Create(VK_LCL_SLASH, '/', '/ ?', '/', '/', '?', true, true));
    ConfigKeys.Add(TKey.Create(VK_LCL_BACKSLASH, '\', '\ |', '\', '\', '|', true, true));
    ConfigKeys.Add(TKey.Create(VK_LCL_QUOTE, '''', ''' "', 'apos', '''', '"', true, true));
    ConfigKeys.Add(TKey.Create(VK_LCL_TILDE, '`', '` ~', 'tilde', '`', '~', true, true));
    ConfigKeys.Add(TKey.Create(VK_LCL_SEMI_COMMA, ';', '; :', 'colon', ';', ':', true, true));
    ConfigKeys.Add(TKey.Create(VK_LCL_COMMA, ',', ', <', 'com', ',', '<', true, true));
    ConfigKeys.Add(TKey.Create(VK_LCL_POINT, '.', '. >', 'per', '.', '>', true, true));
    ConfigKeys.Add(TKey.Create(VK_LCL_OPEN_BRAKET, '[', '[ {', 'obrk', '[', '{', true, true));
    ConfigKeys.Add(TKey.Create(VK_LCL_CLOSE_BRAKET, ']', '] }', 'cbrk', ']', '}', true, true));
    ConfigKeys.Add(TKey.Create(VK_OEM_102, 'intl-\', '', 'intl\', 'intl-\', 'intl-\', true, true)); //International <> key between Left Shift and Z
  end
  else
  begin
    ConfigKeys.Add(TKey.Create(VK_LCL_EQUAL, '=', '+' + #10 + '=', '=', '=', '+', true, true));
    ConfigKeys.Add(TKey.Create(VK_LCL_MINUS, '-', '_' + #10 + '-', 'hyphen', '-', '_', true, true));
    ConfigKeys.Add(TKey.Create(VK_LCL_SLASH, '/', '?' + #10 + '/', '/', '/', '?', true, true));
    ConfigKeys.Add(TKey.Create(VK_LCL_BACKSLASH, '\', '|' + #10 + '\', '\', '\', '|', true, true));
    ConfigKeys.Add(TKey.Create(VK_LCL_QUOTE, '''', '"' + #10 + '''', '''', '''', '"', true, true));
    ConfigKeys.Add(TKey.Create(VK_LCL_TILDE, '`', '~' + #10 + '`', '`', '`', '~', true, true));
    ConfigKeys.Add(TKey.Create(VK_LCL_SEMI_COMMA, ';', ':' + #10 + ';', ';', ';', ':', true, true));
    ConfigKeys.Add(TKey.Create(VK_LCL_COMMA, ',', '<' + #10 + ',', ',', ',', '<', true, true));
    ConfigKeys.Add(TKey.Create(VK_LCL_POINT, '.', '>' + #10 + '.', '.', '.', '>', true, true));
    ConfigKeys.Add(TKey.Create(VK_LCL_OPEN_BRAKET, '[', '{' + #10 + '[', 'obrack', '[', '{', true, true));
    ConfigKeys.Add(TKey.Create(VK_LCL_CLOSE_BRAKET, ']', '}' + #10 + ']', 'cbrack', ']', '}', true, true));
    ConfigKeys.Add(TKey.Create(VK_OEM_102, 'intl-\', '', 'intl-\', 'intl-\', 'intl-\', true, true)); //International <> key between Left Shift and Z
  end;

  //Numpad keys
  if (GApplication in [APPL_FSEDGE, APPL_FSPRO]) then
  begin
    ConfigKeys.Add(TKey.Create(VK_NUMPAD0, 'kp0', '0'));
    ConfigKeys.Add(TKey.Create(VK_NUMPAD1, 'kp1', '1'));
    ConfigKeys.Add(TKey.Create(VK_NUMPAD2, 'kp2', '2'));
    ConfigKeys.Add(TKey.Create(VK_NUMPAD3, 'kp3', '3'));
    ConfigKeys.Add(TKey.Create(VK_NUMPAD4, 'kp4', '4'));
    ConfigKeys.Add(TKey.Create(VK_NUMPAD5, 'kp5', '5'));
    ConfigKeys.Add(TKey.Create(VK_NUMPAD6, 'kp6', '6'));
    ConfigKeys.Add(TKey.Create(VK_NUMPAD7, 'kp7', '7'));
    ConfigKeys.Add(TKey.Create(VK_NUMPAD8, 'kp8', '8'));
    ConfigKeys.Add(TKey.Create(VK_NUMPAD9, 'kp9', '9'));
    ConfigKeys.Add(TKey.Create(VK_DIVIDE, 'kp/', '/', 'kp/'));
    ConfigKeys.Add(TKey.Create(VK_MULTIPLY, 'kp*', '*', 'kp*'));
    ConfigKeys.Add(TKey.Create(VK_SUBTRACT, 'kp-', '-', 'kp-'));
    ConfigKeys.Add(TKey.Create(VK_ADD, 'kp+', '+', 'kp+'));
    ConfigKeys.Add(TKey.Create(VK_DECIMAL, 'kp.', '.'));
    ConfigKeys.Add(TKey.Create(VK_NUMPADENTER, 'kpenter', 'Kp' + #10 + 'Enter', 'kpent'));
    //ConfigKeys.Add(TKey.Create(VK_OEM_NEC_EQUAL, 'kp=', '='));
  end
  else
  begin
    ConfigKeys.Add(TKey.Create(VK_NUMPAD0, 'kp0', '0'));
    ConfigKeys.Add(TKey.Create(VK_KP_0, 'kp0', '0'));
    ConfigKeys.Add(TKey.Create(VK_NUMPAD1, 'kp1', '1'));
    ConfigKeys.Add(TKey.Create(VK_KP_1, 'kp1', '1'));
    ConfigKeys.Add(TKey.Create(VK_NUMPAD2, 'kp2', '2'));
    ConfigKeys.Add(TKey.Create(VK_KP_2, 'kp2', '2'));
    ConfigKeys.Add(TKey.Create(VK_NUMPAD3, 'kp3', '3'));
    ConfigKeys.Add(TKey.Create(VK_KP_3, 'kp3', '3'));
    ConfigKeys.Add(TKey.Create(VK_NUMPAD4, 'kp4', '4'));
    ConfigKeys.Add(TKey.Create(VK_KP_4, 'kp4', '4'));
    ConfigKeys.Add(TKey.Create(VK_NUMPAD5, 'kp5', '5'));
    ConfigKeys.Add(TKey.Create(VK_KP_5, 'kp5', '5'));
    ConfigKeys.Add(TKey.Create(VK_NUMPAD6, 'kp6', '6'));
    ConfigKeys.Add(TKey.Create(VK_KP_6, 'kp6', '6'));
    ConfigKeys.Add(TKey.Create(VK_NUMPAD7, 'kp7', '7'));
    ConfigKeys.Add(TKey.Create(VK_KP_7, 'kp7', '7'));
    ConfigKeys.Add(TKey.Create(VK_NUMPAD8, 'kp8', '8'));
    ConfigKeys.Add(TKey.Create(VK_KP_8, 'kp8', '8'));
    ConfigKeys.Add(TKey.Create(VK_NUMPAD9, 'kp9', '9'));
    ConfigKeys.Add(TKey.Create(VK_KP_9, 'kp9', '9'));
    ConfigKeys.Add(TKey.Create(VK_DIVIDE, 'kp/', '/', 'kpdiv'));
    ConfigKeys.Add(TKey.Create(VK_KP_DIVIDE, 'kp/', '/', 'kpdiv'));
    ConfigKeys.Add(TKey.Create(VK_MULTIPLY, 'kp*', '*', 'kpmult'));
    ConfigKeys.Add(TKey.Create(VK_KP_MULT, 'kp*', '*', 'kpmult'));
    ConfigKeys.Add(TKey.Create(VK_SUBTRACT, 'kp-', '-', 'kpmin'));
    ConfigKeys.Add(TKey.Create(VK_KP_MIN, 'kp-', '-', 'kpmin'));
    ConfigKeys.Add(TKey.Create(VK_ADD, 'kp+', '+', 'kpplus'));
    ConfigKeys.Add(TKey.Create(VK_KP_PLUS, 'kp+', '+', 'kpplus'));
    ConfigKeys.Add(TKey.Create(VK_DECIMAL, 'kp.', '.'));
    ConfigKeys.Add(TKey.Create(VK_KP_PERI, 'kp.', '.'));
    ConfigKeys.Add(TKey.Create(VK_NUMPADENTER, 'kpenter', 'Kp' + #10 + 'Enter', 'kpenter'));
    //ConfigKeys.Add(TKey.Create(VK_OEM_NEC_EQUAL, 'kp=', '='));
    ConfigKeys.Add(TKey.Create(VK_KP_EQUAL, 'kp=', '='));
  end;

  //0 to 9
  if (GApplication in [APPL_FSEDGE, APPL_FSPRO]) then
  begin
    ConfigKeys.Add(TKey.Create(VK_0, '0', '0 )', '0', '0', ')', true, true));
    ConfigKeys.Add(TKey.Create(VK_1, '1', '1 !', '1', '1', '!', true, true));
    ConfigKeys.Add(TKey.Create(VK_2, '2', '2 @', '2', '2', '@', true, true));
    ConfigKeys.Add(TKey.Create(VK_3, '3', '3 #', '3', '3', '#', true, true));
    ConfigKeys.Add(TKey.Create(VK_4, '4', '4 $', '4', '4', '$', true, true));
    ConfigKeys.Add(TKey.Create(VK_5, '5', '5 %', '5', '5', '%', true, true));
    ConfigKeys.Add(TKey.Create(VK_6, '6', '6 ^', '6', '6', '^', true, true));
    ConfigKeys.Add(TKey.Create(VK_7, '7', '7 &', '7', '7', '&', true, true));
    ConfigKeys.Add(TKey.Create(VK_8, '8', '8 *', '8', '8', '*', true, true));
    ConfigKeys.Add(TKey.Create(VK_9, '9', '9 (', '9', '9', '(', true, true));
  end
  else
  begin
    ConfigKeys.Add(TKey.Create(VK_0, '0', ')' + #10 + '0', '0', '0', ')', true, true));
    ConfigKeys.Add(TKey.Create(VK_1, '1', '!' + #10 + '1', '1', '1', '!', true, true));
    ConfigKeys.Add(TKey.Create(VK_2, '2', '@' + #10 + '2', '2', '2', '@', true, true));
    ConfigKeys.Add(TKey.Create(VK_3, '3', '#' + #10 + '3', '3', '3', '#', true, true));
    ConfigKeys.Add(TKey.Create(VK_4, '4', '$' + #10 + '4', '4', '4', '$', true, true));
    ConfigKeys.Add(TKey.Create(VK_5, '5', '%' + #10 + '5', '5', '5', '%', true, true));
    ConfigKeys.Add(TKey.Create(VK_6, '6', '^' + #10 + '6', '6', '6', '^', true, true));
    ConfigKeys.Add(TKey.Create(VK_7, '7', '&' + #10 + '7', '7', '7', '&', true, true));
    ConfigKeys.Add(TKey.Create(VK_8, '8', '*' + #10 + '8', '8', '8', '*', true, true));
    ConfigKeys.Add(TKey.Create(VK_9, '9', '(' + #10 + '9', '9', '9', '(', true, true));
  end;

  //a to z
  for i := VK_A to VK_Z do
    ConfigKeys.Add(TKey.Create(i, LowerCase(Chr(i)), UpperCase(Chr(i)), LowerCase(Chr(i)), LowerCase(Chr(i)), Chr(i),
      true, true));

  //Custom for special events
  if (GApplication in [APPL_FSEDGE, APPL_FSPRO]) then
  begin
    ConfigKeys.Add(TKey.Create(VK_MOUSE_LEFT, 'lmous', 'Left' + #10 + 'Mouse'));
    ConfigKeys.Add(TKey.Create(VK_MOUSE_MIDDLE, 'mmous', 'Middle' + #10 + 'Mouse'));
    ConfigKeys.Add(TKey.Create(VK_MOUSE_RIGHT, 'rmous', 'Right' + #10 + 'Mouse'));
    ConfigKeys.Add(TKey.Create(VK_MOUSE_BTN4, 'mous4', 'Mouse' + #10 + 'Btn 4'));
    ConfigKeys.Add(TKey.Create(VK_MOUSE_BTN5, 'mous5', 'Mouse' + #10 + 'Btn 5'));
  end
  else if (GApplication in [APPL_ADV2]) then
  begin
    ConfigKeys.Add(TKey.Create(VK_MOUSE_LEFT, 'lmouse', 'Left' + #10 + 'Mouse', '', '', '', false, false, '', true, false, smallFontSize));
    ConfigKeys.Add(TKey.Create(VK_MOUSE_MIDDLE, 'mmouse', 'Middle' + #10 + 'Mouse', '', '', '', false, false, '', true, false, smallFontSize));
    ConfigKeys.Add(TKey.Create(VK_MOUSE_RIGHT, 'rmouse', 'Right' + #10 + 'Mouse', '', '', '', false, false, '', true, false, smallFontSize));
  end
  else
  begin
    ConfigKeys.Add(TKey.Create(VK_MOUSE_LEFT, 'lmouse'));
    ConfigKeys.Add(TKey.Create(VK_MOUSE_MIDDLE, 'mmouse'));
    ConfigKeys.Add(TKey.Create(VK_MOUSE_RIGHT, 'rmouse'));
  end;
  ConfigKeys.Add(TKey.Create(VK_SPEED1, 'speed1', '', 'speed1', '', '', false, false, '', False));
  ConfigKeys.Add(TKey.Create(VK_SPEED3, 'speed3', '', 'speed3', '', '', false, false, '', False));
  ConfigKeys.Add(TKey.Create(VK_SPEED5, 'speed5', '', 'speed5', '', '', false, false, '', False));
  ConfigKeys.Add(TKey.Create(VK_125MS, 'd125', '', 'd125', '', '', false, false, '', False));
  ConfigKeys.Add(TKey.Create(VK_500MS, 'd500', '', 'd500', '', '', false, false, '', False));

  //Media keys
  mediaFontSize := 4;
  if (CanUseUnicode) then
  begin
    ConfigKeys.Add(TKey.Create(VK_VOLUME_MUTE, 'mute', UnicodeToUTF8(128360), '', '', '', false, false, '', true, false, mediaFontSize, UNICODE_FONT));
    ConfigKeys.Add(TKey.Create(VK_VOLUME_DOWN, 'vol-', UnicodeToUTF8(128361), '', '', '', false, false, '', true, false, mediaFontSize, UNICODE_FONT));
    ConfigKeys.Add(TKey.Create(VK_VOLUME_UP, 'vol+', UnicodeToUTF8(128362), '', '', '', false, false, '', true, false, mediaFontSize, UNICODE_FONT));
    ConfigKeys.Add(TKey.Create(VK_MEDIA_PLAY_PAUSE, 'play', UnicodeToUTF8(9199), '', '', '', false, false, '', true, false, mediaFontSize, UNICODE_FONT));
    ConfigKeys.Add(TKey.Create(VK_MEDIA_PREV_TRACK, 'prev', UnicodeToUTF8(9198), '', '', '', false, false, '', true, false, mediaFontSize, UNICODE_FONT));
    ConfigKeys.Add(TKey.Create(VK_MEDIA_NEXT_TRACK, 'next', UnicodeToUTF8(9197), '', '', '', false, false, '', true, false, mediaFontSize, UNICODE_FONT));
    ConfigKeys.Add(TKey.Create(VK_KP_PLAY, 'play', UnicodeToUTF8(9199), '', '', '', false, false, '', true, false, mediaFontSize, UNICODE_FONT));
    ConfigKeys.Add(TKey.Create(VK_KP_PREV, 'prev', UnicodeToUTF8(9198), '', '', '', false, false, '', true, false, mediaFontSize, UNICODE_FONT));
    ConfigKeys.Add(TKey.Create(VK_KP_NEXT, 'next', UnicodeToUTF8(9197), '', '', '', false, false, '', true, false, mediaFontSize, UNICODE_FONT));
    ConfigKeys.Add(TKey.Create(VK_KP_MUTE, 'mute', UnicodeToUTF8(128360), '', '', '', false, false, '', true, false, mediaFontSize, UNICODE_FONT));
    ConfigKeys.Add(TKey.Create(VK_KP_VOLDOWN, 'vol-', UnicodeToUTF8(128361), '', '', '', false, false, '', true, false, mediaFontSize, UNICODE_FONT));
    ConfigKeys.Add(TKey.Create(VK_KP_VOLUP, 'vol+', UnicodeToUTF8(128362), '', '', '', false, false, '', true, false, mediaFontSize, UNICODE_FONT));
  end
  else
  begin
    ConfigKeys.Add(TKey.Create(VK_VOLUME_MUTE, 'mute', 'Mute'));
    ConfigKeys.Add(TKey.Create(VK_VOLUME_DOWN, 'vol-', 'Vol-'));
    ConfigKeys.Add(TKey.Create(VK_VOLUME_UP, 'vol+', 'Vol+'));
    ConfigKeys.Add(TKey.Create(VK_MEDIA_PLAY_PAUSE, 'play', 'Play'));
    ConfigKeys.Add(TKey.Create(VK_MEDIA_PREV_TRACK, 'prev', 'Prev'));
    ConfigKeys.Add(TKey.Create(VK_MEDIA_NEXT_TRACK, 'next', 'Next'));
    ConfigKeys.Add(TKey.Create(VK_KP_PLAY, 'play', 'Play'));
    ConfigKeys.Add(TKey.Create(VK_KP_PREV, 'prev', 'Prev'));
    ConfigKeys.Add(TKey.Create(VK_KP_NEXT, 'next', 'Next'));
    ConfigKeys.Add(TKey.Create(VK_KP_MUTE, 'mute', 'Mute'));
    ConfigKeys.Add(TKey.Create(VK_KP_VOLDOWN, 'vol-', 'Vol-'));
    ConfigKeys.Add(TKey.Create(VK_KP_VOLUP, 'vol+', 'Vol+'));
  end;
  ConfigKeys.Add(TKey.Create(VK_CALC, 'calc', 'Calc'));
  ConfigKeys.Add(TKey.Create(VK_KP_CALC, 'calc', 'Calc'));
  if (GApplication in [APPL_FSEDGE, APPL_FSPRO]) then
    ConfigKeys.Add(TKey.Create(VK_SHUTDOWN, 'shutdn', 'Shut' + #10 + 'down', 'shtdn'))
  else
    ConfigKeys.Add(TKey.Create(VK_SHUTDOWN, 'shutdn', 'Shut' + #10 + 'down'));
  ConfigKeys.Add(TKey.Create(VK_DIF_PRESS_REL, '{ }', '', ' '));
  ConfigKeys.Add(TKey.Create(VK_KEYPAD, '', 'Key-' + #10 + 'pad', '', '', '', false, false, '', true, false, smallFontSize));
  ConfigKeys.Add(TKey.Create(VK_PROGRAM, '', 'Pro-' + #10 + 'gram', '', '', '', false, false, '', true, false, smallFontSize));
  ConfigKeys.Add(TKey.Create(VK_KEYPAD_SHIFT, 'kpshft', 'Kp' + #10 + 'Shift', '', '', '', false, false, '', true, false, smallFontSize));
  ConfigKeys.Add(TKey.Create(VK_KEYPAD_TOGGLE, 'kptoggle', 'Key-' + #10 + 'pad', '', '', '', false, false, '', true, false, smallFontSize));
  if (GApplication in [APPL_FSEDGE, APPL_FSPRO]) then
  begin
    ConfigKeys.Add(TKey.Create(VK_HK1, 'hk1', ' ', '', '', '', false, false, '', true, false, 0, '', 'hotkey 1'));
    ConfigKeys.Add(TKey.Create(VK_HK2, 'hk2', ' ', '', '', '', false, false, '', true, false, 0, '', 'hotkey 2'));
    ConfigKeys.Add(TKey.Create(VK_HK3, 'hk3', ' ', '', '', '', false, false, '', true, false, 0, '', 'hotkey 3'));
    ConfigKeys.Add(TKey.Create(VK_HK4, 'hk4', ' ', '', '', '', false, false, '', true, false, 0, '', 'hotkey 4'));
    ConfigKeys.Add(TKey.Create(VK_HK5, 'hk5', ' ', '', '', '', false, false, '', true, false, 0, '', 'hotkey 5'));
    ConfigKeys.Add(TKey.Create(VK_HK6, 'hk6', ' ', '', '', '', false, false, '', true, false, 0, '', 'hotkey 6'));
    ConfigKeys.Add(TKey.Create(VK_HK7, 'hk7', ' ', '', '', '', false, false, '', true, false, 0, '', 'hotkey 7'));
    ConfigKeys.Add(TKey.Create(VK_HK8, 'hk8', ' ', '', '', '', false, false, '', true, false, 0, '', 'hotkey 8'));
    ConfigKeys.Add(TKey.Create(VK_HK9, 'hk9', 'Fn' + #10 + 'Toggle', '', '', '', false, false, '', true, false, smallFontSize, '', 'hotkey 9'));
    if (GApplication in [APPL_FSEDGE]) then
    begin
      if (CanUseUnicode) then
        ConfigKeys.Add(TKey.Create(VK_HK10, 'hk10', UnicodeToUTF8(9728), '', '', '', false, false, '', true, false, mediaFontSize, UNICODE_FONT, 'hotkey 10'))
      else
        ConfigKeys.Add(TKey.Create(VK_HK10, 'hk10', 'LED', '', '', '', false, false, '', true, false, 0, '', 'hotkey 10'));
    end
    else
      ConfigKeys.Add(TKey.Create(VK_HK10, 'hk10', 'PC' + #10 + 'Menu', '', '', '', false, false, '', true, false, 0, '', 'hotkey 10'));
  end;
  ConfigKeys.Add(TKey.Create(VK_FUNCTION, 'Fn', 'Fn'));
  ConfigKeys.Add(TKey.Create(VK_NULL, 'null', ' '));
  if (GApplication in [APPL_FSEDGE, APPL_FSPRO]) then
    ConfigKeys.Add(TKey.Create(VK_FN_TOGGLE, 'fntog', 'Fn' + #10 + 'Toggle', '', '', '', false, false, '', true, false, smallFontSize))
  else
    ConfigKeys.Add(TKey.Create(VK_FN_TOGGLE, 'fntoggle', 'Fn' + #10 + 'Toggle', '', '', '', false, false, '', true, false, smallFontSize));
  if (GApplication in [APPL_FSEDGE, APPL_FSPRO]) then
    ConfigKeys.Add(TKey.Create(VK_FN_SHIFT, 'fnshf', 'Fn' + #10 + 'Shift', '', '', '', false, false, '', true, false, smallFontSize))
  else
    ConfigKeys.Add(TKey.Create(VK_FN_SHIFT, 'fnshift', 'Fn' + #10 + 'Shift', '', '', '', false, false, '', true, false, smallFontSize));
  if (CanUseUnicode) then
      ConfigKeys.Add(TKey.Create(VK_LED, 'LED', UnicodeToUTF8(9728), '', '', '', false, false, '', true, false, mediaFontSize, UNICODE_FONT))
    else
      ConfigKeys.Add(TKey.Create(VK_LED, 'LED', 'LED'));
  ConfigKeys.Add(TKey.Create(VK_MIC_MUTE, 'micmute', 'Mic' + #10 + 'Mute', '', '', '', false, false, '', true, false, smallFontSize));
  ConfigKeys.Add(TKey.Create(VK_LPEDAL, 'lp-tab', 'Tab'));
  ConfigKeys.Add(TKey.Create(VK_MPEDAL, 'mp-kpshf', 'Kp' + #10 + 'Shift'));
  ConfigKeys.Add(TKey.Create(VK_RPEDAL, 'rp-kpent', 'Kp' + #10 + 'Enter'));

  //FOR ADVANTAGE 2 KEYBOARD
  ConfigKeys.Add(TKey.Create(VK_KP_KPSHIFT, 'kpshft', 'Kp' + #10 + 'Shift', '', '', '', false, false, '', true, false, smallFontSize));
  ConfigKeys.Add(TKey.Create(VK_KP_ENTER1, 'kpenter1', 'Kp' + #10 + 'Enter'));
  ConfigKeys.Add(TKey.Create(VK_KP_ENTER2, 'kpenter2', 'Kp' + #10 + 'Enter'));
  ConfigKeys.Add(TKey.Create(VK_HYPER, 'hyper', 'Hyper'));
  ConfigKeys.Add(TKey.Create(VK_MEH, 'meh', 'Meh'));

  result := ConfigKeys;
end;

initialization
  KINESIS_BLUE := RGB(0, 114, 206);
  KINESIS_BLUE_EDGE := RGB(30, 154, 214);
  KINESIS_ORANGE := RGB(255, 134, 58);
  KINESIS_LIGHT_GRAY_FS := RGB(47, 56, 61);
  KINESIS_DARK_GRAY_FS := RGB(38, 38, 38);
  GApplicationTitle := 'Savant Elite2 SmartSet App';
  //Windows
  {$ifdef Win32}
  GApplicationName := 'Savant Elite2 SmartSet App (Win)';
  GApplicationPath := IncludeTrailingBackslash(ExtractFileDir(ParamStr(0)));
  {$endif}

  //MacOS
  {$ifdef Darwin}
  GApplicationName := 'Savant Elite2 SmartSet App (Mac)';

  //Try to get the executable path from ParamStr(0)
  GApplicationPath := IncludeTrailingBackslash(ExtractFilePath(ParamStr(0)));
  if GApplicationPath <> '' then
  begin
    //Go back 3 folders to get application bundle path
    GApplicationPath := Copy(GApplicationPath, 0, LastDelimiter('/', GApplicationPath) - 1);
    GApplicationPath := Copy(GApplicationPath, 0, LastDelimiter('/', GApplicationPath) - 1);
    GApplicationPath := Copy(GApplicationPath, 0, LastDelimiter('/', GApplicationPath) - 1);
    GApplicationPath := IncludeTrailingBackslash(Copy(GApplicationPath, 0, LastDelimiter('/', GApplicationPath) - 1));
  end;
  {$endif}

  //Warning ParamStr(0) might not work correctly on Mac OS
  GPedalsFilePath := IncludeTrailingBackslash(GApplicationPath + 'active');
  GLayoutFilePath := IncludeTrailingBackslash(GApplicationPath + 'layouts');
  GSettingsFilePath := IncludeTrailingBackslash(GApplicationPath + 'settings');
  GFirmwareFilePath := IncludeTrailingBackslash(GApplicationPath + 'firmware');
  GPedalsFile := GPedalsFilePath + 'pedals.txt';
  GVersionFile := GPedalsFilePath + 'version.txt';
  GStateFile := GPedalsFilePath + 'state.txt';

end.
