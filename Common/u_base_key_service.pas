unit u_base_key_service;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, u_keys, LCLType, u_const, u_key_layer, character;

type
  { TBaseKeyService }
  TBaseKeyService = class
  private
    FKBLayers: TKBLayerList;
    FActiveKbKey: TKBKey;

    FCurrentKBLayout: string;

    //List of supported keys from programming guide
    FConfigKeys: TKeyList;
    FActiveModifiers: TKeyList;
    FBackupKey: TKBKey;
    FBackupMacro: TKBKey;

    //List of supported keyboard layouts
    //FKeyboardLayouts: TKeyboardLayoutList;

    function IsNumericKey(aKey: TKey): boolean;
    function IsAlphaKey(aKey: TKey): boolean;

    procedure FillModifiersFromValues(aKeyList: TKeyList; sModifiers: string);
    function GetKeyText(aKey: TKey; defaultValue: string = ''; checkAltGr: boolean = false): string;
    function IsAltGr(aKey: TKey): boolean;
    function GetQwertyTopLayerAdv2: TKBLayer;
    function GetQwertyBotLayerAdv2: TKBLayer;
    function GetDvorakTopLayerAdv2: TKBLayer;
    function GetDvorakBotLayerAdv2: TKBLayer;
    function GetOutputText(aKeyList: TKeyList; var aKeysPos: TKeysPos; keyIdxToFind: integer; var keyIdxPos: integer): string;
    function KeyPadException(value: string): boolean;
    function GetKeyPadException(value: string): integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function FindKeyConfig(iKey: word): TKey; overload;
    function FindKeyConfig(sKey: string): TKey; overload;
    function GetKeyConfig(iKey: word): TKey;
    procedure AddModifier(key: word);
    procedure RemoveModifier(key: word);
    procedure ClearModifiers;
    function IsWinKeyDown: boolean;
    function AddKey(kbKey: TKBKey; iKey: word; modifiers: string; insertAtPos: integer): TKey;
    function RemoveKey(kbKey: TKBKey; index: integer): boolean;
    function GetModifierText: string;
    function BackupMacro(aKbKey: TKbKey): boolean;
    function RestoreMacro(aKbKey: TKbKey): boolean;
    function BackupKbKey(aKbKey: TKbKey): boolean;
    function RestoreKbKey(aKbKey: TKbKey): boolean;
    function GetMacroText(aKeyList: TKeyList; var aKeysPos: TKeysPos): string;
    function GetKeyAtPosition(aKeyList: TKeyList; cursorPos: integer): integer;
    procedure ConvertFromTextFileFmtAdv2(aLayoutContent: TStringList);
    function ConvertToTextFileFmtAdv2: TStringList;
    function IsShiftDown(aKey: TKey): boolean;
    //procedure LoadKeyConfig;
    procedure LoadLayerList(layerType: integer);
    //procedure LoadKeyboardLayouts;
    procedure UpdateCurrentKeyboardLayout;
    function GetReplacementKey(aKey: word; saving: boolean): string;
    procedure ResetLayout;
    procedure ResetLayer(aLayer: TKBLayer);
    function GetLayer(layerIdx: integer): TKBLayer;
    procedure SetKBKey(aKBKey: TKBKey; key: word);
    procedure SetKBKeyIdx(aLayer: TKBLayer; index: integer; key: word);
    function GetKBKey(key: word; layerIdx: integer): TKBKey;
    function GetKbKeyByIndex(aLayer: TKBLayer; index: integer): TKBKey;
    function CopyMacro(aMacro: TKeyList): TKeyList;
    function GetSingleKeyText(aKey: TKey; var isLongKey: boolean): string;
    function ValidateMacros(aKey: TKbKey; var errorMsg: string; var errorMsgTitle: string): boolean;
    function CountModifiers(modifiers: string): integer;
    function CountKeystrokes(aKeyList: TKeyList): integer;
    procedure LoadConfigKeys;
    function GetModifierValues(aKey: TKey): string;
    procedure SetTapAndHold(aKBKey: TKBKey; tapAction: integer;
      holdAction: integer; delay: integer);

    property ActiveKbKey: TKBKey read FActiveKbKey write FActiveKbKey;

    property ConfigKeys: TKeyList read FConfigKeys write FConfigKeys;
    property ActiveModifiers: TKeyList read FActiveModifiers write FActiveModifiers;
    //property BackupKey: TKBKey read FBackupKey write FBackupKey;
    //property KeyboardLayouts: TKeyboardLayoutList read FKeyboardLayouts write FKeyboardLayouts;

    property KBLayers: TKBLayerList read FKBLayers write FKBLayers;
  end;

implementation

{ TBaseKeyService }

constructor TBaseKeyService.Create;
begin
  inherited Create;
  FKBLayers := TKBLayerList.Create;
  FActiveKbKey := nil;
  FCurrentKBLayout := '';
  FConfigKeys := nil;
  FActiveModifiers := TKeyList.Create;
  FBackupKey := TKBKey.Create;
  FBackupMacro := TKBKey.Create;
  //KeyboardLayouts := TKeyboardLayoutList.Create;

  //LoadKeyConfig;
  //LoadKeyboardLayouts;
  UpdateCurrentKeyboardLayout;
end;

destructor TBaseKeyService.Destroy;
begin
  FreeAndNil(FConfigKeys);
  FreeAndNil(FActiveModifiers);
  FreeAndNil(FBackupKey);
  FreeAndNil(FBackupMacro);
  inherited Destroy;
end;

function TBaseKeyService.GetQwertyTopLayerAdv2: TKBLayer;
var
  aKBLayer: TKBLayer;
begin
  aKBLayer := TKBLayer.Create;
  aKBLayer.LayerIndex := TOPLAYER_IDX;
  aKBLayer.LayerName := 'Qwerty-top';
  aKBLayer.LayerType := LAYER_QWERTY;

  //Put Keys in order needed...
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_ESCAPE), 0)); //First button
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_F1), 1));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_F2), 2));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_F3), 3));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_F4), 4));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_F5), 5));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_F6), 6));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_F7), 7));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_F8), 8));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_F9), 9));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_F10), 10));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_F11), 11));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_F12), 12));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_PRINT), 13));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_SCROLL), 14));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_PAUSE), 15));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KEYPAD), 16, false));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_PROGRAM), 17, false));

  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LCL_EQUAL), 18));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_1), 19));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_2), 20));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_3), 21));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_4), 22));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_5), 23));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_6), 24));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_7), 25));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_8), 26));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_9), 27));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_0), 28));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LCL_MINUS), 29));

  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_TAB), 30));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_Q), 31));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_W), 32));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_E), 33));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_R), 34));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_T), 35));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_Y), 36));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_U), 37));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_I), 38));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_O), 39));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_P), 40));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LCL_BACKSLASH), 41));

  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_CAPITAL), 42));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_A), 43));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_S), 44));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_D), 45));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_F), 46));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_G), 47));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LCONTROL), 48, true, false));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LMENU), 49, true, false));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_RWIN), 50, true, false));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_RCONTROL), 51, true, false));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_H), 52));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_J), 53));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_K), 54));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_L), 55));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LCL_SEMI_COMMA), 56));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LCL_QUOTE), 57));

  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LSHIFT), 58, true, false));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_Z), 59));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_X), 60));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_C), 61));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_V), 62));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_B), 63));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_BACK), 64));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_DELETE), 65));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_HOME), 66));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_PRIOR), 67));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_RETURN), 68));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_SPACE), 69));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_N), 70));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_M), 71));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LCL_COMMA), 72));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LCL_POINT), 73));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LCL_SLASH), 74));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_RSHIFT), 75, true, false));

  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LCL_TILDE), 76));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_OEM_102), 77));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LEFT), 78));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_RIGHT), 79));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_END), 80));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_NEXT), 81));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_UP), 82));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_DOWN), 83));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LCL_OPEN_BRAKET), 84));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LCL_CLOSE_BRAKET), 85));

  //For Pedals
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LPEDAL), 86));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_MPEDAL), 87));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_RPEDAL), 88));

  result := aKBLayer;
end;

function TBaseKeyService.GetQwertyBotLayerAdv2: TKBLayer;
var
  aKBLayer: TKBLayer;
begin
  aKBLayer := TKBLayer.Create;
  aKBLayer.LayerIndex := BOTLAYER_IDX;
  aKBLayer.LayerName := 'Qwerty-keypad';
  aKBLayer.LayerType := LAYER_QWERTY;

  //Put Keys in order needed...
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_ESCAPE), 0)); //First button
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LWIN), 1));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_RMENU), 2));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KP_MENU), 3));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KP_PLAY), 4));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KP_PREV), 5));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KP_NEXT), 6));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KP_CALC), 7));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KP_KPSHIFT), 8));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_F9), 9));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_F10), 10));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_F11), 11));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_F12), 12));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KP_MUTE), 13));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KP_VOLDOWN), 14));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KP_VOLUP), 15));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KEYPAD), 16, false));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_PROGRAM), 17, false));

  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LCL_EQUAL), 18));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_1), 19));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_2), 20));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_3), 21));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_4), 22));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_5), 23));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_6), 24));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KP_NUMLCK), 25));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KP_EQUAL), 26));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KP_DIVIDE), 27));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KP_MULT), 28));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LCL_MINUS), 29));

  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_TAB), 30));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_Q), 31));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_W), 32));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_E), 33));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_R), 34));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_T), 35));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_Y), 36));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KP_7), 37));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KP_8), 38));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KP_9), 39));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KP_MIN), 40));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LCL_BACKSLASH), 41));

  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_CAPITAL), 42));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_A), 43));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_S), 44));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_D), 45));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_F), 46));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_G), 47));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LCONTROL), 48, true, false));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LMENU), 49, true, false));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_RWIN), 50, true, false));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_RCONTROL), 51, true, false));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_H), 52));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KP_4), 53));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KP_5), 54));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KP_6), 55));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KP_PLUS), 56));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LCL_QUOTE), 57));

  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LSHIFT), 58, true, false));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_Z), 59));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_X), 60));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_C), 61));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_V), 62));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_B), 63));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_BACK), 64));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_DELETE), 65));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_HOME), 66));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_PRIOR), 67));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_RETURN), 68));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KP_0), 69));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_N), 70));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KP_1), 71));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KP_2), 72));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KP_3), 73));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KP_ENTER1), 74));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_RSHIFT), 75, true, false));

  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LCL_TILDE), 76));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_INSERT), 77));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LEFT), 78));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_RIGHT), 79));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_END), 80));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_NEXT), 81));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_UP), 82));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_DOWN), 83));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KP_PERI), 84));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KP_ENTER2), 85));

  //For Pedals
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LPEDAL), 86));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_MPEDAL), 87));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_RPEDAL), 88));

  result := aKBLayer;
end;

function TBaseKeyService.GetDvorakTopLayerAdv2: TKBLayer;
var
  aKBLayer: TKBLayer;
begin
  aKBLayer := TKBLayer.Create;
  aKBLayer.LayerIndex := TOPLAYER_IDX;
  aKBLayer.LayerName := 'Dvorak-top';
  aKBLayer.LayerType := LAYER_DVORAK;

  //Put Keys in order needed...
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_ESCAPE), 0)); //First button
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_F1), 1));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_F2), 2));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_F3), 3));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_F4), 4));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_F5), 5));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_F6), 6));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_F7), 7));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_F8), 8));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_F9), 9));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_F10), 10));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_F11), 11));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_F12), 12));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_PRINT), 13));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_SCROLL), 14));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_PAUSE), 15));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KEYPAD), 16, false));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_PROGRAM), 17, false));

  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LCL_EQUAL), 18));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_1), 19));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_2), 20));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_3), 21));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_4), 22));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_5), 23));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_6), 24));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_7), 25));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_8), 26));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_9), 27));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_0), 28));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LCL_MINUS), 29));

  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_TAB), 30));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LCL_QUOTE), 31));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LCL_COMMA), 32));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LCL_POINT), 33));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_P), 34));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_Y), 35));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_F), 36));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_G), 37));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_C), 38));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_R), 39));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_L), 40));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LCL_SLASH), 41));

  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_CAPITAL), 42));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_A), 43));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_O), 44));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_E), 45));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_U), 46));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_I), 47));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LCONTROL), 48, true, false));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LMENU), 49, true, false));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_RWIN), 50, true, false));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_RCONTROL), 51, true, false));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_D), 52));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_H), 53));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_T), 54));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_N), 55));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_S), 56));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LCL_BACKSLASH), 57));

  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LSHIFT), 58, true, false));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LCL_SEMI_COMMA), 59));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_Q), 60));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_J), 61));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_K), 62));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_X), 63));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_BACK), 64));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_DELETE), 65));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_HOME), 66));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_PRIOR), 67));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_RETURN), 68));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_SPACE), 69));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_B), 70));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_M), 71));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_W), 72));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_V), 73));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_Z), 74));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_RSHIFT), 75, true, false));

  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LCL_TILDE), 76));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_OEM_102), 77));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LEFT), 78));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_RIGHT), 79));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_END), 80));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_NEXT), 81));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_UP), 82));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_DOWN), 83));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LCL_OPEN_BRAKET), 84));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LCL_CLOSE_BRAKET), 85));

  //For Pedals
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LPEDAL), 86));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_MPEDAL), 87));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_RPEDAL), 88));

  result := aKBLayer;
end;

function TBaseKeyService.GetDvorakBotLayerAdv2: TKBLayer;
var
  aKBLayer: TKBLayer;
begin
  aKBLayer := TKBLayer.Create;
  aKBLayer.LayerIndex := BOTLAYER_IDX;
  aKBLayer.LayerName := 'Dvorak-keypad';
  aKBLayer.LayerType := LAYER_DVORAK;

  //Put Keys in order needed...
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_ESCAPE), 0)); //First button
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LWIN), 1));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_RMENU), 2));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KP_MENU), 3));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KP_PLAY), 4));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KP_PREV), 5));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KP_NEXT), 6));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KP_CALC), 7));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KP_KPSHIFT), 8));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_F9), 9));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_F10), 10));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_F11), 11));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_F12), 12));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KP_MUTE), 13));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KP_VOLDOWN), 14));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KP_VOLUP), 15));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KEYPAD), 16, false));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_PROGRAM), 17, false));

  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LCL_EQUAL), 18));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_1), 19));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_2), 20));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_3), 21));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_4), 22));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_5), 23));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_6), 24));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KP_NUMLCK), 25));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KP_EQUAL), 26));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KP_DIVIDE), 27));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KP_MULT), 28));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LCL_MINUS), 29));

  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_TAB), 30));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LCL_QUOTE), 31));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LCL_COMMA), 32));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LCL_POINT), 33));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_P), 34));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_Y), 35));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_F), 36));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KP_7), 37));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KP_8), 38));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KP_9), 39));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KP_MIN), 40));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LCL_SLASH), 41));

  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_CAPITAL), 42));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_A), 43));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_O), 44));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_E), 45));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_U), 46));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_I), 47));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LCONTROL), 48, true, false));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LMENU), 49, true, false));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_RWIN), 50, true, false));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_RCONTROL), 51, true, false));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_D), 52));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KP_4), 53));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KP_5), 54));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KP_6), 55));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KP_PLUS), 56));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LCL_BACKSLASH), 57));

  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LSHIFT), 58, true, false));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LCL_SEMI_COMMA), 59));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_Q), 60));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_J), 61));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_K), 62));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_X), 63));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_BACK), 64));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_DELETE), 65));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_HOME), 66));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_PRIOR), 67));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_RETURN), 68));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KP_0), 69));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_B), 70));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KP_1), 71));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KP_2), 72));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KP_3), 73));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KP_ENTER1), 74));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_RSHIFT), 75, true, false));

  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LCL_TILDE), 76));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_INSERT), 77));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LEFT), 78));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_RIGHT), 79));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_END), 80));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_NEXT), 81));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_UP), 82));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_DOWN), 83));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KP_PERI), 84));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_KP_ENTER2), 85));

  //For Pedals
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_LPEDAL), 86));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_MPEDAL), 87));
  aKBLayer.KBKeyList.Add(TKBKey.Create(GetKeyConfig(VK_RPEDAL), 88));
  result := aKBLayer;

end;

procedure TBaseKeyService.LoadLayerList(layerType: integer);
begin
  FKBLayers.Clear;
  if (layerType = LAYER_QWERTY) then
  begin
    if (GApplication = APPL_ADV2) then
    begin
      FKBLayers.Add(GetQwertyTopLayerAdv2);
      FKBLayers.Add(GetQwertyBotLayerAdv2);
    end;
  end
  else if (layerType = LAYER_DVORAK) then
  begin
    if (GApplication = APPL_ADV2) then
    begin
      FKBLayers.Add(GetDvorakTopLayerAdv2);
      FKBLayers.Add(GetDvorakBotLayerAdv2);
    end;
  end
end;

procedure TBaseKeyService.UpdateCurrentKeyboardLayout;
begin
  FCurrentKBLayout := GetCurrentKeyoardLayout;
end;

//Checks if key is numeric (ascii 48 to 57)
function TBaseKeyService.IsNumericKey(aKey: TKey): boolean;
begin
  Result := (aKey.Key >= VK_0) and (aKey.Key <= VK_9);
end;

//Checks if key is alphabetic (ascii lowercase 65 to 90 or uppercase 97 to 122)
function TBaseKeyService.IsAlphaKey(aKey: TKey): boolean;
begin
  Result := (aKey.Key >= VK_A) and (aKey.Key <= VK_Z);
end;

//Takes a list of modifiers in string and fills a list of keys with the values
procedure TBaseKeyService.FillModifiersFromValues(aKeyList: TKeyList;
  sModifiers: string);
begin
  if sModifiers <> '' then
  begin
    if Pos(SHIFT_MOD, string(sModifiers)) <> 0 then
      aKeyList.Add(FindKeyConfig(VK_SHIFT).CopyKey);
    if Pos(L_SHIFT_MOD, string(sModifiers)) <> 0 then
      aKeyList.Add(FindKeyConfig(VK_LSHIFT).CopyKey);
    if Pos(R_SHIFT_MOD, string(sModifiers)) <> 0 then
      aKeyList.Add(FindKeyConfig(VK_RSHIFT).CopyKey);
    if Pos(CTRL_MOD, string(sModifiers)) <> 0 then
      aKeyList.Add(FindKeyConfig(VK_CONTROL).CopyKey);
    if Pos(L_CTRL_MOD, string(sModifiers)) <> 0 then
      aKeyList.Add(FindKeyConfig(VK_LCONTROL).CopyKey);
    if Pos(R_CTRL_MOD, string(sModifiers)) <> 0 then
      aKeyList.Add(FindKeyConfig(VK_RCONTROL).CopyKey);
    if Pos(ALT_MOD, string(sModifiers)) <> 0 then
      aKeyList.Add(FindKeyConfig(VK_MENU).CopyKey);
    if Pos(L_ALT_MOD, string(sModifiers)) <> 0 then
      aKeyList.Add(FindKeyConfig(VK_LMENU).CopyKey);
    if Pos(R_ALT_MOD, string(sModifiers)) <> 0 then
      aKeyList.Add(FindKeyConfig(VK_RMENU).CopyKey);
    if Pos(WIN_MOD, string(sModifiers)) <> 0 then
      aKeyList.Add(FindKeyConfig(VK_LWIN).CopyKey);
    if Pos(L_WIN_MOD, string(sModifiers)) <> 0 then
      aKeyList.Add(FindKeyConfig(VK_LWIN).CopyKey);
    if Pos(R_WIN_MOD, string(sModifiers)) <> 0 then
      aKeyList.Add(FindKeyConfig(VK_RWIN).CopyKey);
  end;
end;

function TBaseKeyService.GetKeyText(aKey: TKey; defaultValue: string;
  checkAltGr: boolean): string;
begin
{$ifdef Win32}
  if (aKey.ConvertToUnicode) then
    result := KeyToUnicode(aKey.Key, (IsShiftDown(aKey)) and (aKey.ShowShiftedValue), checkAltGr)
  else if (defaultValue <> '') then
    result := defaultValue
  else
    result := aKey.Value;
{$endif}
{$ifdef Darwin}
if (IsShiftDown(aKey) and (aKey.ShowShiftedValue)) then
  result := aKey.ShiftedValue
else if (defaultValue <> '') then
  result := defaultValue
else
  result := aKey.Value;
{$endif}
end;

//Checks if AltGr pressed (Ctrl + Alt)
function TBaseKeyService.IsAltGr(aKey: TKey): boolean;
begin
  if (Pos(CTRL_MOD, aKey.Modifiers) <> 0) and
    (Pos(ALT_MOD, aKey.Modifiers) <> 0) and
    (Length(aKey.Modifiers) = 3) then
    result := true
  else
    result := false;
end;

//Internal function to get output text
function TBaseKeyService.GetOutputText(aKeyList: TKeyList;
  var aKeysPos: TKeysPos; keyIdxToFind: integer; var keyIdxPos: integer
  ): string;
var
  i: integer;
  aKey: TKey;
  keyCount: integer;
  initLength: integer;
  mustAddKeyPos: boolean;

  procedure AddKeyPos(iStart, iEnd: integer);
  begin
    SetLength(aKeysPos, Length(aKeysPos) + 1);
    aKeysPos[Length(aKeysPos) - 1].iStart := iStart;
    aKeysPos[Length(aKeysPos) - 1].iEnd := iEnd;
  end;

  function GetKeyIdxPos(keyPos: integer; text: string): boolean;
  begin
    result := false;
    if (keyIdxToFind >= 0) and (keyIdxPos = -1) and (LengthUTF8(text) >= keyIdxToFind) then
    begin
      keyIdxPos := keyPos;
      result := true;
    end;
  end;

begin
  Result := '';
  aKeysPos := nil;
  keyIdxPos := -1;

  if aKeyList <> nil then
  begin
    keyCount := aKeyList.Count - 1;
    for i := 0 to keyCount do
    begin
      aKey := aKeyList.Items[i];
      initLength := LengthUTF8(Result);

      Result := Result + GetSingleKeyText(aKey, mustAddKeyPos);
      if (mustAddKeyPos) then
        AddKeyPos(initLength, LengthUTF8(Result));

      //Checks for keyIdxPos
      if GetKeyIdxPos(i, Result) then
        break;
    end;
  end;
end;

//Finds and returns the key in list of configurable keys using virtual key
function TBaseKeyService.FindKeyConfig(iKey: word): TKey;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to ConfigKeys.Count - 1 do
  begin
    if ConfigKeys[i] <> nil then
      if ConfigKeys[i].Key = iKey then
      begin
        Result := ConfigKeys[i];
        break;
      end;
  end;
end;

//Finds and returns the key in list of configurable keys using string key
function TBaseKeyService.FindKeyConfig(sKey: string): TKey;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to ConfigKeys.Count - 1 do
  begin
    if ConfigKeys[i] <> nil then
      if AnsiLowerCase(ConfigKeys[i].SaveValue) = AnsiLowerCase(sKey) then
      begin
        Result := ConfigKeys[i];
        break;
      end;
  end;

end;

function TBaseKeyService.GetKeyConfig(iKey: word): TKey;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to ConfigKeys.Count - 1 do
  begin
    if ConfigKeys[i] <> nil then
      if ConfigKeys[i].Key = iKey then
      begin
        Result := ConfigKeys[i].CopyKey;
        break;
      end;
  end;
end;

//Adds modifier to list of active modifiers
procedure TBaseKeyService.AddModifier(key: word);
var
  i: integer;
  found: boolean;
  aKey: TKey;
  newKey: TKey;
begin
  found := False;
  aKey := FindKeyConfig(key);
  if aKey <> nil then
  begin
    for i := 0 to ActiveModifiers.Count - 1 do
    begin
      if ActiveModifiers[i] <> nil then
        if ActiveModifiers[i].Key = aKey.Key then
        begin
          found := True; //already exists
          break;
        end;
    end;

    if not (found) then
    begin
      newKey := aKey.CopyKey;
      ActiveModifiers.Add(newKey);
    end;
  end;
end;

//Removes modifier from list of active modifiers
procedure TBaseKeyService.RemoveModifier(key: word);
var
  i: integer;
  aKey: TKey;
begin
  aKey := FindKeyConfig(key);
  if aKey <> nil then
  begin
    for i := ActiveModifiers.Count - 1 downto 0 do
    begin
      if ActiveModifiers[i] <> nil then
        if ActiveModifiers[i].Key = aKey.Key then
          ActiveModifiers.Delete(i);
    end;
  end;
end;

procedure TBaseKeyService.ClearModifiers;
begin
  ActiveModifiers.Clear;
end;

function TBaseKeyService.IsWinKeyDown: boolean;
var
  i:integer;
begin
  result := false;

  for i := 0 to ActiveModifiers.Count - 1 do
    if ActiveModifiers.Items[i].Key in [VK_LWIN, VK_RWIN] then
      result := true;
end;

function TBaseKeyService.AddKey(kbKey: TKBKey; iKey: word; modifiers: string; insertAtPos: integer): TKey;
var
  aKey: TKey;
  newKey: TKey;
begin
  Result := nil;
  if (kbKey <> nil) then
  begin
    aKey := FindKeyConfig(iKey);
    if (aKey <> nil) then
    begin
      newKey := aKey.CopyKey;

      newKey.Modifiers := modifiers;

      //Add keypress to active pedal
      if (insertAtPos >= 0) then
        kbKey.ActiveMacro.Insert(insertAtPos, newKey)
      else
        kbKey.ActiveMacro.Add(newKey);

      //Returns complet output text
      Result := newKey;
    end;
  end;

end;

function TBaseKeyService.RemoveKey(kbKey: TKBKey; index: integer): boolean;
begin
  Result := false;
  if (kbKey <> nil) and (kbKey.IsMacro) then
  begin
    if (kbKey.ActiveMacro.Count >= index) and (index >= 0) then
    begin;
      kbKey.ActiveMacro.Remove(kbKey.ActiveMacro.Items[index]);
      result := true;
    end;
  end;
end;

function TBaseKeyService.GetModifierText: string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to FActiveModifiers.Count - 1 do
  begin
    if Result <> '' then
      Result := Result + ',';
    if FActiveModifiers.Items[i].Key = VK_SHIFT then
      Result := Result + SHIFT_MOD
    else if FActiveModifiers.Items[i].Key = VK_LSHIFT then
      Result := Result + L_SHIFT_MOD
    else if FActiveModifiers.Items[i].Key = VK_RSHIFT then
      Result := Result + R_SHIFT_MOD
    else if FActiveModifiers.Items[i].Key = VK_CONTROL then
      Result := Result + CTRL_MOD
    else if FActiveModifiers.Items[i].Key = VK_LCONTROL then
      Result := Result + L_CTRL_MOD
    else if FActiveModifiers.Items[i].Key = VK_RCONTROL then
      Result := Result + R_CTRL_MOD
    else if FActiveModifiers.Items[i].Key = VK_MENU then
      Result := Result + ALT_MOD
    else if FActiveModifiers.Items[i].Key = VK_LMENU then
      Result := Result + L_ALT_MOD
    else if FActiveModifiers.Items[i].Key = VK_RMENU then
      Result := Result + R_ALT_MOD
    else if (FActiveModifiers.Items[i].Key = VK_LWIN) then
      Result := Result + L_WIN_MOD
    else if (FActiveModifiers.Items[i].Key = VK_RWIN) then
      Result := Result + R_WIN_MOD;
  end;
end;

function TBaseKeyService.BackupMacro(aKbKey: TKbKey): boolean;
begin
  result := false;
  if (aKbKey <> nil) then
  begin
    FBackupMacro.Assign(aKbKey, rtAll);
    result := true;
  end;
end;

function TBaseKeyService.RestoreMacro(aKbKey: TKbKey): boolean;
begin
  result := false;
  if (aKbKey <> nil) and (FBackupMacro <> nil) then
  begin
    aKbKey.Assign(FBackupMacro, rtMacro);
    result := true;
  end;
end;

function TBaseKeyService.BackupKbKey(aKbKey: TKbKey): boolean;
begin
  result := false;
  if (aKbKey <> nil) then
  begin
    FBackupKey.Assign(aKbKey, rtAll);
    result := true;
  end;
end;

function TBaseKeyService.RestoreKbKey(aKbKey: TKbKey): boolean;
begin
  result := false;
  if (aKbKey <> nil) and (FBackupKey <> nil) then
  begin
    aKbKey.Assign(FBackupKey, rtKey);
    result := true;
  end;
end;

//Returns macro text
function TBaseKeyService.GetMacroText(aKeyList: TKeyList; var aKeysPos: TKeysPos
  ): string;
var
  tmpInt: integer;
begin
  result := GetOutputText(aKeyList, aKeysPos, -1, tmpInt);
end;

//Returns key at specific position
function TBaseKeyService.GetKeyAtPosition(aKeyList: TKeyList; cursorPos: integer
  ): integer;
var
  keyPos: integer;
  tmpKeysPos: TKeysPos;
begin
  keyPos := -1;
  GetOutputText(aKeyList, tmpKeysPos, cursorPos, keyPos);
  result := keyPos;
end;

//Checks if value is a keypad exception (Adv2)
function TBaseKeyService.KeyPadException(value: string): boolean;
begin
  result := (value = 'menu') or
    (value = 'play') or
    (value = 'prev') or
    (value = 'next') or
    (value = 'calc') or
    (value = 'kpshft') or
    (value = 'mute') or
    (value = 'vol-') or
    (value = 'vol+') or
    (value = 'kp0') or (value = 'kp1') or (value = 'kp2') or
    (value = 'kp3') or (value = 'kp4') or (value = 'kp5') or
    (value = 'kp6') or (value = 'kp7') or (value = 'kp8') or (value = 'kp9') or
    (value = 'numlk') or
    (value = 'kp=') or
    (value = 'kpdiv') or
    (value = 'kpmult') or
    (value = 'kpmin') or
    (value = 'kpplus') or
    (value = 'kpenter1') or (value = 'kpenter2') or
    (value = 'kp.') or
    (value = 'kp-insert');
end;

//Checks if value is a keypad exception (Adv2)
function TBaseKeyService.GetKeyPadException(value: string): integer;
begin
  case value of
    'menu': result := VK_KP_MENU;
    'play': result := VK_KP_PLAY;
    'prev': result := VK_KP_PREV;
    'next': result := VK_KP_NEXT;
    'calc': result := VK_KP_CALC;
    'kpshft': result := VK_KP_KPSHIFT;
    'mute': result := VK_KP_MUTE;
    'vol-': result := VK_KP_VOLDOWN;
    'vol+': result := VK_KP_VOLUP;
    'numlk': result := VK_KP_NUMLCK;
    'kp=': result := VK_KP_EQUAL;
    'kpdiv': result := VK_KP_DIVIDE;
    'kpmult': result := VK_KP_MULT;
    'kp0': result := VK_KP_0;
    'kp1': result := VK_KP_1;
    'kp2': result := VK_KP_2;
    'kp3': result := VK_KP_3;
    'kp4': result := VK_KP_4;
    'kp5': result := VK_KP_5;
    'kp6': result := VK_KP_6;
    'kp7': result := VK_KP_7;
    'kp8': result := VK_KP_8;
    'kp9': result := VK_KP_9;
    'kpmin': result := VK_KP_MIN;
    'kpplus': result := VK_KP_PLUS;
    'kpenter1': result := VK_KP_ENTER1;
    'kpenter2': result := VK_KP_ENTER2;
    'kp.': result := VK_KP_PERI;
    'insert': result := VK_INSERT;
    else
      result := -1;
  end;
end;

//Converts text file content to layer/key values
procedure TBaseKeyService.ConvertFromTextFileFmtAdv2(aLayoutContent: TStringList);
var
  aKey, newKey: TKey;
  sKey: string;
  keyState: TKeyState;
  keyStart, keyEnd: integer;
  lastKey: integer;
  previousKey: TKey;
  replacementKey: string;

  i: integer;
  currentLine: string;
  posSep: integer;
  isSingleKey: boolean;
  isMacro: boolean;
  isTapHold: boolean;
  isKeypadLayer: boolean;
  configText: string;
  valueText: string;
  aKBKey: TKBKey;
  layerIdx: integer;
  aCoTriggers: TKeyList;
  activeMacro: TKeyList;
  vkException: integer;
  tapHold: integer;
begin
  lastKey := 0;
  aCoTriggers := TKeyList.Create;

  ResetLayout;

  if (aLayoutContent.Count > 0) then
  begin
    for i := 0 to aLayoutContent.Count - 1 do
    begin
      currentLine := AnsiLowerCase(aLayoutContent.Strings[i]);

      //Reset values
      isKeypadLayer := false;
      aCoTriggers.Clear;
      aKBKey := nil;
      activeMacro := nil;
      ClearModifiers;

      posSep := Pos('>', currentLine);
      isSingleKey := Copy(currentLine, 1, 1) = SK_START;
      isMacro := Copy(currentLine, 1, 1) = MK_START;
      isTapHold := isSingleKey and (Pos('[' + TAP_AND_HOLD, currentLine) > 0);

      //Check if it's a valid line
      if (posSep <> 0) and (isSingleKey or isMacro or isTapHold) then
      begin
        configText := Copy(currentLine, 1, posSep - 1);
        valueText := Copy(currentLine, posSep + 1, Length(currentLine));

        if (isTapHold) then
        begin
          //Load configured key
          keyStart := Pos(SK_START, configText);
          keyEnd := Pos(SK_END, configText);
          sKey := Copy(configText, keyStart + 1, keyEnd - 2);
          isKeypadLayer := (Pos(KEYPAD_KEY, sKey) <> 0) or (KeyPadException(sKey));
          if (isKeypadLayer) then
          begin
            layerIdx := BOTLAYER_IDX;
            if (Pos(KEYPAD_KEY, sKey) <> 0) then
              Delete(sKey, 1, length(KEYPAD_KEY)); //removes kp- text
          end
          else
            layerIdx := TOPLAYER_IDX;
          vkException := GetKeyPadException(sKey);
          if (vkException > 0) then
            aKey := FindKeyConfig(vkException)
          else
            aKey := FindKeyConfig(sKey);

          //Gets key from layer
          if aKey <> nil then
            aKBKey := GetKBKey(aKey.Key, layerIdx);

          if (aKBKey <> nil) then
          begin
            aKBKey.TapAndHold := true;
            //Load values for tap and hold
            for tapHold := 1 to 3 do
            begin
              keyStart := Pos(SK_START, valueText);
              keyEnd := Pos(SK_END, valueText);
              sKey := Copy(valueText, keyStart + 1, keyEnd - 2);
              Delete(valueText, 1, keyEnd); //removes currentkey

              //Sets modified key
              if (tapHold = 1) or (tapHold = 3) then
              begin
                aKey := FindKeyConfig(sKey);
                if aKey <> nil then
                begin
                  if (tapHold = 1) then
                    aKBKey.TapAction := aKey.CopyKey
                  else
                    aKBKey.HoldAction := aKey.CopyKey;
                end;
              end
              else
              begin
                sKey := Copy(sKey, Length(TAP_AND_HOLD) + 1, Length(sKey));
                aKBKey.TimingDelay := ConvertToInt(sKey, DEFAULT_SPEED_TAP_HOLD);
              end;
            end;
          end;
        end
        else if (isSingleKey) then
        begin
          //Load configured key
          keyStart := Pos(SK_START, configText);
          keyEnd := Pos(SK_END, configText);
          sKey := Copy(configText, keyStart + 1, keyEnd - 2);
          isKeypadLayer := (Pos(KEYPAD_KEY, sKey) <> 0) or (KeyPadException(sKey));
          if (isKeypadLayer) then
          begin
            layerIdx := BOTLAYER_IDX;
            if (Pos(KEYPAD_KEY, sKey) <> 0) then
              Delete(sKey, 1, length(KEYPAD_KEY)); //removes kp- text
          end
          else
            layerIdx := TOPLAYER_IDX;
          vkException := GetKeyPadException(sKey);
          if (vkException > 0) then
            aKey := FindKeyConfig(vkException)
          else
            aKey := FindKeyConfig(sKey);

          //Gets key from layer
          if aKey <> nil then
            aKBKey := GetKBKey(aKey.Key, layerIdx);

          if (aKBKey <> nil) then
          begin
            //Load value key
            isKeypadLayer := Pos(KEYPAD_KEY, valueText) <> 0;
            keyStart := Pos(SK_START, valueText);
            keyEnd := Pos(SK_END, valueText);
            sKey := Copy(valueText, keyStart + 1, keyEnd - 2);
            if (isKeypadLayer) then
              Delete(sKey, 1, length(KEYPAD_KEY)); //removes kp- text

            //Sets modified key
            vkException := GetKeyPadException(sKey);
            if (vkException > 0) then
              aKey := FindKeyConfig(vkException)
            else
              aKey := FindKeyConfig(sKey);
            if aKey <> nil then
              SetKBKey(aKBKey, aKey.Key);
          end;
        end
        else if (isMacro) then
        begin
          //Loads key and co-triggers
          while (configText <> '') do
          begin
            aKey := nil;
            keyStart := Pos(MK_START, configText);
            keyEnd := Pos(MK_END, configText);
            sKey := Copy(configText, keyStart + 1, keyEnd - 2);
            isKeypadLayer := (Pos(KEYPAD_KEY, sKey) <> 0) or (KeyPadException(sKey));
            if (isKeypadLayer) then
            begin
              layerIdx := BOTLAYER_IDX;
              if (Pos(KEYPAD_KEY, sKey) <> 0) then
                Delete(sKey, 1, length(KEYPAD_KEY)); //removes kp- text
            end
            else
              layerIdx := TOPLAYER_IDX;
            Delete(configText, 1, keyEnd); //remove currentkey

            //Finds key in config
            vkException := GetKeyPadException(sKey);
            if (vkException > 0) then
              aKey := FindKeyConfig(vkException)
            else
              aKey := FindKeyConfig(sKey);

            if (aKey <> nil) then
            begin
              if (IsModifier(aKey.Key)) and (configText <> '') then
                aCoTriggers.Add(aKey.CopyKey)
              else
                aKBKey := GetKBKey(aKey.Key, layerIdx);
            end;
          end;

          //Loads active Macro (max of 3)
          if (aKBKey <> nil) then
          begin
            if (aKBKey.Macro1.Count = 0) then
              activeMacro := aKBKey.Macro1
            else if (aKBKey.Macro2.Count = 0) then
              activeMacro := aKBKey.Macro2
            else if (aKBKey.Macro3.Count = 0) then
              activeMacro := aKBKey.Macro3;
          end;

          //If kbKey and activeMacro, load values
          if (aKBKey <> nil) and (activeMacro <> nil) then
          begin
            aKBKey.IsMacro := true;

            if (aCoTriggers.Count >= 1) then
              activeMacro.CoTrigger1 := aCoTriggers[0].CopyKey;
            if (aCoTriggers.Count >= 2) then
              activeMacro.CoTrigger2 := aCoTriggers[1].CopyKey;
            if (aCoTriggers.Count >= 3) then
              activeMacro.CoTrigger3 := aCoTriggers[2].CopyKey;

            //Get Macro text
            while (valueText <> '') do
            begin
              aKey := nil;
              keyStart := Pos(MK_START, valueText);
              keyEnd := Pos(MK_END, valueText);
              sKey := Copy(valueText, keyStart + 1, keyEnd - 2);
              if copy(sKey, 1, 1) = '-' then  //Checks for keyup or keydown
                keyState := ksDown
              else if copy(sKey, 1, 1) = '+' then
                keyState := ksUp
              else
                keyState := ksNone;
              if keyState <> ksNone then
                Delete(sKey, 1, 1); //removes - or +
              if (Pos(KEYPAD_KEY, sKey) <> 0) then
                Delete(sKey, 1, length(KEYPAD_KEY)); //removes kp- text
              Delete(valueText, 1, keyEnd); //removes currentkey

              if (activeMacro.Count = 0) and (Copy(sKey, 1, Length(MACRO_SPEED_TEXT)) = MACRO_SPEED_TEXT) then
              begin
                activeMacro.MacroSpeed := ConvertToInt(Copy(sKey, Length(MACRO_SPEED_TEXT) + 1, Length(MACRO_SPEED_TEXT) + 2));
              end
              else
              begin
                //Finds key in config
                vkException := GetKeyPadException(sKey);
                if (vkException > 0) then
                  aKey := FindKeyConfig(vkException)
                else
                  aKey := FindKeyConfig(sKey);

                //Checks for replacement key values (US English)
                if aKey <> nil then
                begin
                  replacementKey := GetReplacementKey(aKey.Key, false);
                  if (replacementKey <> '') then
                    aKey := FindKeyConfig(replacementKey);
                end;
              end;

              if aKey <> nil then
              begin
                if IsModifier(aKey.Key) then
                begin
                  //Adds to list of active modifiers
                  if (keyState = ksDown) then
                    AddModifier(aKey.key)
                  else if (keyState = ksUp) then
                  begin
                    //If last key is the same key, then adds modifier as single key down
                    if lastKey = aKey.Key then
                      activeMacro.Add(aKey.CopyKey);
                    RemoveModifier(aKey.Key);
                  end
                  else //if no keyState (+ or -) and modifier add as single key
                    activeMacro.Add(aKey.CopyKey);
                end
                else if (keyState in [ksNone, ksDown]) then //Only add key on key down or key none
                begin
                  //Get the previous key
                  if activeMacro.Count > 0 then
                    previousKey := activeMacro.Items[activeMacro.Count - 1];

                  //If there are modifiers and we find Different Press and Release, we assign it to previous key
                  if (ActiveModifiers.Count > 0) and (aKey.Key = VK_DIF_PRESS_REL) and (previousKey <> nil) then
                  begin
                    previousKey.DiffPressRel := true;
                  end
                  else //Add the key
                  begin
                    newKey := akey.CopyKey;
                    newKey.Modifiers := GetModifierText; //Gets modifier values
                    activeMacro.Add(newKey); //Adds key
                  end;
                end;
                lastKey := aKey.Key;
              end;
            end; //end while loop valueText
          end;
        end;
      end;
    end;
  end;
  ClearModifiers;
end;

//Converts keys from program to format for the text file
function TBaseKeyService.ConvertToTextFileFmtAdv2: TStringList;
var
  i, j: integer;
  lIdx, kIdx, mIdx: integer;
  lineText: string;
  saveValue: string;
  layoutContent: TStringList;
  aLayer: TKBLayer;
  aKbKey: TKBKey;
  aKey: TKey;
  aMacro: TKeyList;
  layerPrefix: string;
  prevModifiers: string;
  curKeyModifiers: TKeyList;
  prevKeyModifiers: TKeyList;

  function GetLayerPrefix(saveValue: string): string;
  begin
    if not KeyPadException(saveValue) then
      result := layerPrefix
    else
      result := '';
  end;

begin
  layoutContent := TStringList.Create;

  for lIdx := 0 to FKBLayers.Count - 1 do
  begin
    layerPrefix := '';
    aLayer := FKBLayers[lIdx];
    if (aLayer.LayerIndex = BOTLAYER_IDX) then
      layerPrefix := 'kp-';

    for kIdx := 0 to aLayer.KBKeyList.Count - 1 do
    begin
      lineText := '';
      aKbKey := aLayer.KBKeyList[kIdx];

      //If key is tap and hold
      if (aKbKey.TapAndHold) and (aKbKey.TapAction <> nil) and (aKbKey.HoldAction <> nil) then
      begin
        lineText := '[' + GetLayerPrefix(aKbKey.OriginalKey.SaveValue) + aKbKey.OriginalKey.SaveValue + ']>[' + aKbKey.TapAction.SaveValue + ']' +
          '[' + TAP_AND_HOLD + IntToStr(aKbKey.TimingDelay) + ']' + '[' + aKbKey.HoldAction.SaveValue + ']';
        layoutContent.Add(lineText);
      end
      //If key is modified / remapped
      else if (aKbKey.IsModified) then
      begin
        lineText := '[' + GetLayerPrefix(aKbKey.OriginalKey.SaveValue) + aKbKey.OriginalKey.SaveValue + ']>[' + aKbKey.ModifiedKey.SaveValue + ']';
        layoutContent.Add(lineText);
      end;

      //jm todo Can be modified and have macro??
      //jm todo Do we put kp- for macro values??
      if (aKbKey.IsMacro) then
      begin
        curKeyModifiers := TKeyList.Create;
        prevKeyModifiers := TKeyList.Create;

        //Loop through the 3 macros
        for mIdx := 1 to 3 do
        begin
          lineText := '';

          //Select the correct macro
          if (mIdx = 1) then
            aMacro := aKbKey.Macro1
          else if (mIdx = 2) then
            aMacro := aKbKey.Macro2
          else
            aMacro := aKbKey.Macro3;

          //Skip if macro has no keys
          if (aMacro.Count <= 0) then
            continue;

          //Add the co-triggers first
          if (aMacro.CoTrigger1 <> nil) then
            lineText := '{' + GetLayerPrefix(aMacro.CoTrigger1.SaveValue) + aMacro.CoTrigger1.SaveValue + '}';
          if (aMacro.CoTrigger2 <> nil) then
            lineText := lineText + '{' + GetLayerPrefix(aMacro.CoTrigger2.SaveValue) + aMacro.CoTrigger2.SaveValue + '}';
          if (aMacro.CoTrigger3 <> nil) then
            lineText := lineText + '{' + GetLayerPrefix(aMacro.CoTrigger3.SaveValue) + aMacro.CoTrigger3.SaveValue + '}';

          //Add the modified key
          lineText := lineText + '{' + GetLayerPrefix(aKbKey.OriginalKey.SaveValue) + aKbKey.OriginalKey.SaveValue + '}';

          //Add the character separating config and value keys
          lineText := lineText + '>';

          //jm todo: compare with global macro speed...
          if (aMacro.MacroSpeed >= 1) and (aMacro.MacroSpeed <= MACRO_SPEED_MAX) then
          begin
            lineText := lineText + '{' + MACRO_SPEED_TEXT + IntToStr(aMacro.MacroSpeed) + '}';
          end;

          for i := 0 to aMacro.Count - 1 do
          begin
            prevModifiers := '';
            curKeyModifiers.Clear;
            prevKeyModifiers.Clear;;

            aKey := aMacro[i];

            //Gets the key save value
            saveValue := aKey.SaveValue;

            //Fills list of modifiers
            FillModifiersFromValues(curKeyModifiers, aKey.Modifiers);

            //Loads previous key modifiers
            if (i >= 1) then
            begin
              prevModifiers := aMacro[i - 1].Modifiers;
              FillModifiersFromValues(prevKeyModifiers, prevModifiers);
            end;

            //Checks any change in modifiers (add or remove any)
            if (prevModifiers <> aKey.Modifiers) then
            begin
              //Add text + when modifier is released
              for j := 0 to prevKeyModifiers.Count - 1 do
              begin
                if not curKeyModifiers.ContrainsKey(prevKeyModifiers[j]) then
                  lineText := lineText + '{+' + prevKeyModifiers.Items[j].SaveValue + '}';
              end;

              //Add text - when modifier is pressed
              for j := 0 to curKeyModifiers.Count - 1 do
              begin
                if not prevKeyModifiers.ContrainsKey(curKeyModifiers[j]) then
                  lineText := lineText + '{-' + curKeyModifiers.Items[j].SaveValue + '}';
              end;
            end;

            //If different press & release with combination, write using the old method with up and down value
            if (aKey.DiffPressRel) then
            begin
              //Writes the key - and + if WriteDownUp is enabled, else writes only the value
              if aKey.WriteDownUp then
                lineText := lineText + '{-' + saveValue + '}' + DIFF_PRESS_REL_TEXT + '{+' + saveValue + '}'
              else
                lineText := lineText + '{' + saveValue + '}';
            end
            else  //Write the key value, only need the - / + for modifiers
              lineText := lineText + '{' + saveValue + '}';

            //If last key set modifiers +
            if (i = aMacro.Count - 1) then
            begin
              for j := 0 to curKeyModifiers.Count - 1 do
                lineText := lineText + '{+' + curKeyModifiers[j].SaveValue + '}';
            end;
          end;

          //Add line to text file
          layoutContent.Add(lineText);
        end;

        FreeAndNil(curKeyModifiers);
        FreeAndNil(prevKeyModifiers);
      end;
    end;
  end;

  Result := layoutContent;
end;

function TBaseKeyService.IsShiftDown(aKey: TKey): boolean;
begin
  result := (aKey.Modifiers = SHIFT_MOD) or (aKey.Modifiers = L_SHIFT_MOD) or (aKey.Modifiers = R_SHIFT_MOD);
end;

//Checks for replacement key values (non US English drivers)
function TBaseKeyService.GetReplacementKey(aKey: word; saving: boolean): string;
var
  returnKey: word;
  key: TKey;
begin
  result := '';
  {$ifdef Win32}
  if (FCurrentKBLayout <> ENGLISH_US_LAYOUT_NAME) then
  begin
    if (saving) then
      returnKey := ConvertToEnUS(aKey)
    else
      returnKey := ConvertToLayout(aKey);
    key := FindKeyConfig(returnKey);
    if key <> nil then
      result := key.SaveValue;
  end;
  {$endif}

end;

procedure TBaseKeyService.ResetLayout;
var
  i: integer;
begin
  for i := 0 to KBLayers.Count - 1 do
    ResetLayer(KBLayers[i]);
end;

procedure TBaseKeyService.ResetLayer(aLayer: TKBLayer);
var
  i:integer;
  kbKey: TKBKey;
begin
  if (aLayer <> nil) then
  begin
    for i := 0 to aLayer.KBKeyList.Count - 1 do
    begin
      kbKey := aLayer.KBKeyList[i];
      kbKey.ResetKey;
    end;
  end;
end;

function TBaseKeyService.GetLayer(layerIdx: integer): TKBLayer;
var
  i:integer;
begin
  result := nil;

  for i := 0 to FKBLayers.Count - 1 do
  begin
    if (FKBLayers[i].LayerIndex = layerIdx) then
      result := FKBLayers[i];
  end;
end;

procedure TBaseKeyService.SetKBKey(aKBKey: TKBKey; key: word);
var
  aKey: TKey;
begin
  aKey := GetKeyConfig(key);
  if (aKey <> nil) then
  begin
    if (aKBKey.OriginalKey.Key <> aKey.Key) and (aKBKey.CanEdit) then
    begin
      aKBKey.ModifiedKey := aKey;
      aKBKey.IsModified := true;
    end
    else
    begin
      aKBKey.ModifiedKey := nil;
      aKBKey.IsModified := false;
    end;
  end;
end;

procedure TBaseKeyService.SetKBKeyIdx(aLayer: TKBLayer; index: integer;
  key: word);
var
  aKey: TKey;
  aKbKey: TKBKey;
begin
  aKey := GetKeyConfig(key);
  aKbKey := GetKbKeyByIndex(aLayer, index);
  if (aKey <> nil) and (aKbKey <> nil) and (aLayer <> nil) then
  begin
    if (aKBKey.OriginalKey.Key <> aKey.Key) and (aKBKey.CanEdit) then
    begin
      aKBKey.ModifiedKey := aKey;
      aKBKey.IsModified := true;
    end
    else
    begin
      aKBKey.ModifiedKey := nil;
      aKBKey.IsModified := false;
    end;
  end;
end;

function TBaseKeyService.GetKBKey(key: word; layerIdx: integer): TKBKey;
var
  keyIdx: integer;
  aLayer: TKBLayer;
  aKey: TKBKey;
begin
  Result := nil;

  aLayer := GetLayer(layerIdx);

  if (aLayer <> nil) then
    for keyIdx := 0 to aLayer.KBKeyList.Count - 1 do
    begin
      aKey := aLayer.KBKeyList[keyIdx];
      if (aKey.OriginalKey.Key = key) then
      begin
        result := aKey;
        break;
      end;
    end;
end;

function TBaseKeyService.GetKbKeyByIndex(aLayer: TKBLayer; index: integer
  ): TKBKey;
var
  i: integer;
  found: boolean;
  aKbKey: TKBKey;
begin
  i := 0;
  result := nil;
  found := false;
  if (aLayer <> nil) then
  begin
    While (i < aLayer.KBKeyList.Count) and (not found) do
    begin
      aKbKey := aLayer.KBKeyList[i];
      if (aKbKey.Index = index) then
      begin
        result := aKbKey;
        found := true;
      end;
      inc(i);
    end;
  end;
end;

function TBaseKeyService.CopyMacro(aMacro: TKeyList): TKeyList;
begin
  result := nil;
  if (aMacro <> nil) then
  begin
    result := TKeyList.Create;
    result.Assign(aMacro);
  end;
end;

function TBaseKeyService.GetSingleKeyText(aKey: TKey; var isLongKey: boolean
  ): string;
var
  keyText: string;
  keyTextAltGr: string;
begin
  Result := '';
  keyText := GetKeyText(aKey);
  keyTextAltGr := '';
  isLongKey := false;

  //Returns shifted value if shift + key pressed (and shifted value available)
  if (IsShiftDown(aKey)) and (aKey.ShowShiftedValue) then
  begin
    Result := keyText;
  end
  //Returns modifier value (Ctrl, Shfit, Alt, Win) + key value
  else if (aKey.Modifiers <> '') and not (IsModifier(aKey.Key)) then
  begin
    {$ifdef Win32}
    if (IsAltGr(aKey)) then
      keyTextAltGr := GetKeyText(aKey, '', true);
    {$endif}

    //Shows AltGr value if keyboard layout supports it
    if (keyTextAltGr <> '') then
    begin
      Result := keyTextAltGr;
    end
    else
    begin
      Result := '{' + GetModifierValues(aKey) + keyText + '}';
      isLongKey := true;
    end;
  end
  //If has MultiValue and multikey, returns the multivalue
  else if (aKey.MultiValue <> '') then
  begin
    Result := GetKeyText(aKey, aKey.MultiValue);
  end
  //Returns value in square braquets for single key
  else
  begin
    Result := '{' + keyText + '}';

    if not(IsAlphaKey(aKey)) and not(IsNumericKey(aKey)) then
      isLongKey := true;
  end;
end;

//Validate if Macros are ok, not same co-trigger for all macros
function TBaseKeyService.ValidateMacros(aKey: TKbKey; var errorMsg: string;
  var errorMsgTitle: string): boolean;
var
  i:integer;
  coTriggersOk: boolean;

  function CompareKey(key1: word; aKeyList: TKeyList): boolean;
  var
    i: integer;
  begin
    result := false;

    for i := 1 to 3 do
    begin
      case i of
       1 : if (aKeyList.CoTrigger1 <> nil) then result := key1 = aKeyList.CoTrigger1.Key;
       2 : if (aKeyList.CoTrigger2 <> nil) then result := key1 = aKeyList.CoTrigger2.Key;
       3 : if (aKeyList.CoTrigger3 <> nil) then result := key1 = aKeyList.CoTrigger3.Key;
      end;
    end;
  end;

  function CompareTriggers(aKeyList1: TKeyList; aKeyList2: TKeyList): boolean;
  var
    i: integer;
  begin
    result := false;

    if (aKeyList1.Count > 0) and (aKeyList2.Count > 0) then
    begin
      for i := 1 to 3 do
      begin
        case i of
         1 : if (aKeyList1.CoTrigger1 <> nil) then result := CompareKey(aKeyList1.CoTrigger1.Key, aKeyList2);
         2 : if (aKeyList1.CoTrigger2 <> nil) then result := CompareKey(aKeyList1.CoTrigger2.Key, aKeyList2);
         3 : if (aKeyList1.CoTrigger3 <> nil) then result := CompareKey(aKeyList1.CoTrigger3.Key, aKeyList2);
        end;
      end;

      if (not result) then
      begin
        result := (aKeyList1.CoTrigger1 = nil) and (aKeyList1.CoTrigger2 = nil) and (aKeyList1.CoTrigger3 = nil) and
          (aKeyList2.CoTrigger1 = nil) and (aKeyList2.CoTrigger2 = nil) and (aKeyList2.CoTrigger3 = nil);
      end;
    end;
  end;

begin
  errorMsg := '';
  errorMsgTitle := '';
  coTriggersOk := true;
  if (aKey <> nil) then
  begin
    coTriggersOk := (not CompareTriggers(aKey.Macro1, aKey.Macro2)) and
      (not CompareTriggers(aKey.Macro1, aKey.Macro3)) and
      (not CompareTriggers(aKey.Macro2, aKey.Macro3));

    if (not coTriggersOk) then
    begin
      errorMsg := 'You cannot assign two different macros to the same trigger key combination. Choose a co-trigger that is not already in use.';
      errorMsgTitle := 'Duplicate Macro Trigger';
    end;

    //maxMacroOk := (aKey.Macro1.Count <= MAX_MACRO_FS) and
    //  (aKey.Macro2.Count <= MAX_MACRO_FS) and
    //  (aKey.Macro3.Count <= MAX_MACRO_FS);
    //
    //if (not maxMacroOk) then
    //  errorMsg := 'Only ' + IntToStr(MAX_MACRO_FS) + ' macros can be saved to a layout. Delete a macro to proceed.';
  end;

  result := coTriggersOk; // and maxMacroOk;
end;

function TBaseKeyService.CountModifiers(modifiers: string): integer;
begin
  Result := 0;
  if modifiers <> '' then
  begin
    if Pos(SHIFT_MOD, string(modifiers)) <> 0 then
      inc(result);
    if Pos(L_SHIFT_MOD, string(modifiers)) <> 0 then
      inc(result);
    if Pos(R_SHIFT_MOD, string(modifiers)) <> 0 then
      inc(result);
    if Pos(CTRL_MOD, string(modifiers)) <> 0 then
      inc(result);
    if Pos(L_CTRL_MOD, string(modifiers)) <> 0 then
      inc(result);
    if Pos(R_CTRL_MOD, string(modifiers)) <> 0 then
      inc(result);
    if Pos(ALT_MOD, string(modifiers)) <> 0 then
      inc(result);
    if Pos(L_ALT_MOD, string(modifiers)) <> 0 then
      inc(result);
    if Pos(R_ALT_MOD, string(modifiers)) <> 0 then
      inc(result);
    if Pos(WIN_MOD, string(modifiers)) <> 0 then
      inc(result);
    if Pos(L_WIN_MOD, string(modifiers)) <> 0 then
      inc(result);
  end;
end;

function TBaseKeyService.CountKeystrokes(aKeyList: TKeyList): integer;
var
  i:integer;
  aKey: TKey;
  modifiers: integer;
begin
  result := 0;
  if (aKeyList <> nil) then
  begin
    for i := 0 to aKeyList.Count - 1 do
    begin
      aKey := aKeyList[i];
      if (aKey <> nil) then
      begin
        inc(result);
        modifiers := CountModifiers(aKey.Modifiers);
        result := result + (modifiers * 2);
      end;
    end;
  end;
end;

procedure TBaseKeyService.LoadConfigKeys;
begin
  FConfigKeys := GetConfigKeys;
end;

function TBaseKeyService.GetModifierValues(aKey: TKey): string;
begin
  Result := '';
  if aKey <> nil then
  begin
    if aKey.Modifiers <> '' then
    begin
        if Pos(SHIFT_MOD, string(aKey.Modifiers)) <> 0 then
          Result := Result + FindKeyConfig(VK_SHIFT).Value + '+';
        if Pos(L_SHIFT_MOD, string(aKey.Modifiers)) <> 0 then
          Result := Result + FindKeyConfig(VK_LSHIFT).Value + '+';
        if Pos(R_SHIFT_MOD, string(aKey.Modifiers)) <> 0 then
          Result := Result + FindKeyConfig(VK_RSHIFT).Value + '+';
        if Pos(CTRL_MOD, string(aKey.Modifiers)) <> 0 then
          Result := Result + FindKeyConfig(VK_CONTROL).Value + '+';
        if Pos(L_CTRL_MOD, string(aKey.Modifiers)) <> 0 then
          Result := Result + FindKeyConfig(VK_LCONTROL).Value + '+';
        if Pos(R_CTRL_MOD, string(aKey.Modifiers)) <> 0 then
          Result := Result + FindKeyConfig(VK_RCONTROL).Value + '+';
        if Pos(ALT_MOD, string(aKey.Modifiers)) <> 0 then
          Result := Result + FindKeyConfig(VK_MENU).Value + '+';
        if Pos(L_ALT_MOD, string(aKey.Modifiers)) <> 0 then
          Result := Result + FindKeyConfig(VK_LMENU).Value + '+';
        if Pos(R_ALT_MOD, string(aKey.Modifiers)) <> 0 then
          Result := Result + FindKeyConfig(VK_RMENU).Value + '+';
        if Pos(WIN_MOD, string(aKey.Modifiers)) <> 0 then
          Result := Result + FindKeyConfig(VK_LWIN).Value + '+';
        if Pos(L_WIN_MOD, string(aKey.Modifiers)) <> 0 then
          Result := Result + FindKeyConfig(VK_LWIN).Value + '+';
    end;
  end;
end;

procedure TBaseKeyService.SetTapAndHold(aKBKey: TKBKey; tapAction: integer;
  holdAction: integer; delay: integer);
var
  aTapAction: TKey;
  aHoldAction: TKey;
begin
  aTapAction := GetKeyConfig(tapAction);
  aHoldAction := GetKeyConfig(holdAction);
  if (aTapAction <> nil) and (aHoldAction <> nil) then
  begin
    aKBKey.TapAction := aTapAction;
    aKBKey.HoldAction := aHoldAction;
    aKBKey.TimingDelay := delay;
    aKbKey.TapAndHold := true;
  end
  else
  begin
    aKBKey.TapAction := nil;
    aKBKey.HoldAction := nil;
    aKBKey.TimingDelay := 0;
    aKbKey.TapAndHold := false;
  end;
end;

end.

