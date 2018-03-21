{**************************************************************************************************
 This file is part of the Eye Candy Controls (EC-C)

  Copyright (C) 2013-2016 Vojtěch Čihák, Czech Republic

  This library is free software; you can redistribute it and/or modify it under the terms of the
  GNU Library General Public License as published by the Free Software Foundation; either version
  2 of the License, or (at your option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you permission to link this
  library with independent modules to produce an executable, regardless of the license terms of
  these independent modules,and to copy and distribute the resulting executable under terms of
  your choice, provided that you also meet, for each linked independent module, the terms and
  conditions of the license of that module. An independent module is a module which is not derived
  from or based on this library. If you modify this library, you may extend this exception to your
  version of the library, but you are not obligated to do so. If you do not wish to do so, delete
  this exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
  without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See
  the GNU Library General Public License for more details.

  You should have received a copy of the GNU Library General Public License along with this
  library; if not, write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
  Boston, MA 02111-1307, USA.

**************************************************************************************************}

unit ECSpinCtrls;
{$mode objfpc}{$H+}  

//{$DEFINE DBGSPINS}  {don't remove, just comment}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, CustomTimer, Math, Graphics, ImgList, ECTypes,
  LCLIntf, LMessages, {$IFDEF DBGSPINS} LCLProc, {$ENDIF} LCLType, Themes;

type
  {$PACKENUM 1}
  TBtnKind = (ebkMin, ebkBigDec, ebkDec, ebkMiddle, ebkDrag, ebkMenu, ebkInc, ebkBigInc, ebkMax);
  {$PACKENUM 2}
  TButtonStyle = (ebsSeparated, ebsSplittedBlock, ebsClearBlock);
  TDragOrientation = (edoVertical, edoHorizontal, edoBoth);
  TExtraMouseButtons = set of mbRight..high(TMouseButton); 
  TGlyphStyle = (egsArrowsA, egsArrowsB, egsArrowsC, egsComparison, egsMath, egsPlayer);
  TModifierEnter = (emeNoAction, emeMenuClick, emeMiddleClick);  { Alt, Ctrl/Meta or Shift + Enter }
  TSEOption = ({ Arrows, Home/End & PgUp/PgDn can exceed Max/Min and reach MaxInEdit/MinInEdit }
               esoArrowKeysExceed, 
               { Editing immediately changes value, it does not wait for EditingDone }
               esoEditingChangesValue,
               { Modifiers + Home/End for Max/Min }
               esoHomeEndAlt, esoHomeEndCtrl,
               { smart spinning months and years }
               esoSmartDate,
               { Modifiers + Space clicks Middle, otherwise it opens Menu }
               esoSpaceClicksMiddle, 
               { ArrowKeys for spinning }
               esoUpDownOnly, esoUpDownAlt, esoUpDownCtrl, esoUpDownShift);  
  TSEOptions = set of TSEOption; 
  TValueFormat = (evfRound, evfExponent, evfExponential, evfMantissa, evfHexadecimal,
                  evfMarkHexadec, evfOctal, evfMarkOctal, evfBinary, evfDate, evfTime, evfText,
                  evfCombined);
  { Event }
  TOnDrawGlyph = procedure(Sender: TObject; AKind: TBtnKind; AState: TItemState) of object;

const
  cDefActAltEnter = emeNoAction;
  cDefActCtrlEnter = emeMenuClick; 
  cDefActShiftEnter = emeNoAction;
  cDefCTDelay = 650; {miliseconds}
  cDefCTRepeat = 75; {miliseconds}
  cDefSSBWidth = 15; {pixels}
  cDefSEOptions = [esoEditingChangesValue, esoHomeEndCtrl, esoSpaceClicksMiddle, esoSmartDate,
                   esoUpDownOnly, esoUpDownAlt, esoUpDownCtrl, esoUpDownShift];
  cMouseModifier = [ssCtrl, ssMeta];
  
type  
  { TCustomECTimer }
  TCustomECTimer = class(TCustomTimer)
  private
    FControl: TComponent;
    FCounter: Integer;
    FDelay: Integer;
    FMaxCount: Integer;
    FRepeating: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    procedure DoOnTimer; override;
    procedure SetEnabled(Value: Boolean); override;
    property Control: TComponent read FControl write FControl;
    property Counter: Integer read FCounter;
    property Delay: Integer read FDelay write FDelay default cDefCTDelay;
    property MaxCount: Integer read FMaxCount write FMaxCount default 0;
    property Repeating: Integer read FRepeating write FRepeating default cDefCTRepeat;
  end;

  { TECTimer }
  TECTimer = class(TCustomECTimer)
  published
    property Counter;
    property Delay;
    property Enabled default False;
    property MaxCount;
    property Repeating;
    property OnStartTimer;
    property OnStopTimer;
    property OnTimer;
  end;

  TCustomSpinBtns = class;
  TECSpinEdit = class;

  { TECSpinController }
  TECSpinController = class(TComponent)
  private
    FActionAltEnter: TModifierEnter;
    FActionCtrlEnter: TModifierEnter;
    FActionShiftEnter: TModifierEnter;       
    FBtnBigDecWidth: Integer;
    FBtnBigIncWidth: Integer;
    FBtnDecWidth: Integer;
    FBtnDragWidth: Integer;
    FBtnIncWidth: Integer;
    FBtnMaxWidth: Integer;
    FBtnMenuWidth: Integer;
    FBtnMiddleWidth: Integer;
    FBtnMinWidth: Integer;
    FGlyphStyle: TGlyphStyle;
    FIndent: SmallInt;
    FOptions: TSEOptions;
    FReversed: Boolean;
    FSpacing: SmallInt;
    FStyle: TButtonStyle;
    FTimerDelay: Integer;
    FTimerRepeating: Integer;
    procedure SetActionAltEnter(AValue: TModifierEnter);
    procedure SetActionCtrlEnter(AValue: TModifierEnter);
    procedure SetActionShiftEnter(AValue: TModifierEnter);    
    procedure SetBtnBigDecWidth(AValue: Integer);
    procedure SetBtnBigIncWidth(AValue: Integer);
    procedure SetBtnDecWidth(AValue: Integer);
    procedure SetBtnDragWidth(AValue: Integer);
    procedure SetBtnIncWidth(AValue: Integer);
    procedure SetBtnMaxWidth(AValue: Integer);
    procedure SetBtnMenuWidth(AValue: Integer);
    procedure SetBtnMiddleWidth(AValue: Integer);
    procedure SetBtnMinWidth(AValue: Integer);
    procedure SetGlyphStyle(AValue: TGlyphStyle);
    procedure SetIndent(AValue: SmallInt);
    procedure SetOptions(AValue: TSEOptions);
    procedure SetReversed(AValue: Boolean);
    procedure SetSpacing(AValue: SmallInt);
    procedure SetStyle(AValue: TButtonStyle);
    procedure SetTimerDelay(AValue: Integer);
    procedure SetTimerRepeating(AValue: Integer);
  protected
    ClientList: TFPList;
    function IsClientButtons(AClient: Pointer; out ABtns: TCustomSpinBtns): Boolean;
    procedure SetupButtons(AButtons: TCustomSpinBtns);
    procedure SetupSpinEdit(ASpinEdit: TECSpinEdit);
    procedure RegisterClient(AClient: TControl);
    procedure UnRegisterClient(AClient: TControl);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ActionAltEnter: TModifierEnter read FActionAltEnter write SetActionAltEnter default cDefActAltEnter;
    property ActionCtrlEnter: TModifierEnter read FActionCtrlEnter write SetActionCtrlEnter default cDefActCtrlEnter;
    property ActionShiftEnter: TModifierEnter read FActionShiftEnter write SetActionShiftEnter default cDefActShiftEnter;
    property BtnBigDecWidth: Integer read FBtnBigDecWidth write SetBtnBigDecWidth default cDefSSBWidth;
    property BtnBigIncWidth: Integer read FBtnBigIncWidth write SetBtnBigIncWidth default cDefSSBWidth;
    property BtnDecWidth: Integer read FBtnDecWidth write SetBtnDecWidth default cDefSSBWidth;
    property BtnDragWidth: Integer read FBtnDragWidth write SetBtnDragWidth default cDefSSBWidth;
    property BtnIncWidth: Integer read FBtnIncWidth write SetBtnIncWidth default cDefSSBWidth;
    property BtnMaxWidth: Integer read FBtnMaxWidth write SetBtnMaxWidth default cDefSSBWidth;
    property BtnMenuWidth: Integer read FBtnMenuWidth write SetBtnMenuWidth default cDefSSBWidth;
    property BtnMiddleWidth: Integer read FBtnMiddleWidth write SetBtnMiddleWidth default cDefSSBWidth;
    property BtnMinWidth: Integer read FBtnMinWidth write SetBtnMinWidth default cDefSSBWidth;
    property GlyphStyle: TGlyphStyle read FGlyphStyle write SetGlyphStyle default egsArrowsA;
    property Indent: SmallInt read FIndent write SetIndent default 0;
    property Options: TSEOptions read FOptions write SetOptions default cDefSEOptions;
    property Reversed: Boolean read FReversed write SetReversed default False;
    property Spacing: SmallInt read FSpacing write SetSpacing default 0;
    property Style: TButtonStyle read FStyle write SetStyle default ebsSeparated;
    property TimerDelay: Integer read FTimerDelay write SetTimerDelay default cDefCTDelay;
    property TimerRepeating: Integer read FTimerRepeating write SetTimerRepeating default cDefCTRepeat;
  end;
  
  { TSingleSpinBtn }
  TSingleSpinBtn = class(TPersistent)
  private
    FBtnOrder: Word;
    FCaption: string;
    FGlyphColor: TColor;
    FImageIndex: TImageIndex;
    FLeft: Integer;
    FVisible: Boolean;
    FWidth: SmallInt;
    procedure SetBtnOrder(AValue: Word);
    procedure SetCaption(const AValue: string);
    procedure SetGlyphColor(AValue: TColor);
    procedure SetImageIndex(AValue: TImageIndex);
    procedure SetVisible(AValue: Boolean);
    procedure SetWidth(AValue: SmallInt);
  protected
    Click: TObjectMethod; 
    FEnabled: Boolean;
    FKind: TBtnKind;
    procedure CreateBitmaps;
    procedure FreeBitmaps;
    procedure Resize;
  public
    BtnBitmaps: array[low(TItemState)..eisPushed] of TBitmap;
    Parent: TCustomSpinBtns;
    constructor Create(AParent: TCustomSpinBtns);
    destructor Destroy; override;
    property Enabled: Boolean read FEnabled;
    property Kind: TBtnKind read FKind;
  published
    property BtnOrder: Word read FBtnOrder write SetBtnOrder;
    property Caption: string read FCaption write SetCaption;
    property GlyphColor: TColor read FGlyphColor write SetGlyphColor default clDefault;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property Left: Integer read FLeft;
    property Visible: Boolean read FVisible write SetVisible default True;
    property Width: SmallInt read FWidth write SetWidth default cDefSSBWidth;
  end;

  { TCustomSpinBtns }
  TCustomSpinBtns = class(TGraphicControl)
  private
    FBackgroundColor: TColor;
    FDiscreteChange: Double;
    FDragControl: TExtraMouseButtons;
    FDragOrientation: TDragOrientation;
    FGlyphStyle: TGlyphStyle;
    FImages: TCustomImageList;
    FIncrement: Double;
    FMax: Double;
    FMenuControl: TExtraMouseButtons;
    FMiddle: Double;
    FMin: Double;
    FMode: TIncrementalMode;
    FMouseFromMiddle: Boolean;
    FMouseIncrementX: Double;
    FMouseIncrementY: Double;
    FMouseStepPixelsX: Word;
    FMouseStepPixelsY: Word;
    FOnChange: TNotifyEvent;
    FOnDrawGlyph: TOnDrawGlyph;
    FOnMenuClick: TNotifyEvent;
    FPageSize: Double;
    FReversed: Boolean;
    FSpacing: SmallInt;
    FStyle: TButtonStyle;
    FTimerDelay: Integer;
    FTimerRepeating: Integer;
    FValue: Double;
    procedure SetBackgroundColor(AValue: TColor);
    procedure SetDiscreteChange(AValue: Double);
    procedure SetDragOrientation(AValue: TDragOrientation);
    procedure SetGlyphStyle(AValue: TGlyphStyle);
    procedure SetImages(AValue: TCustomImageList);
    procedure SetMax(AValue: Double); virtual;
    procedure SetMiddle(AValue: Double);
    procedure SetMin(AValue: Double); virtual;
    procedure SetMode(AValue: TIncrementalMode);
    procedure SetMouseStepPixelsX(AValue: Word);
    procedure SetMouseStepPixelsY(AValue: Word);
    procedure SetReversed(AValue: Boolean);
    procedure SetSpacing(AValue: SmallInt);
    procedure SetStyle(AValue: TButtonStyle);
    procedure SetTimerDelay(AValue: Integer);
    procedure SetTimerRepeating(AValue: Integer);
  protected
    class var ControlTimer: TCustomECTimer;
    class constructor CreateTimer;
    class destructor DestroyTimer;
  protected
    BtnPositions: array of Integer;
    CursorSwap: TCursor;
    FController: TECSpinController;
    InitValue: Double;  { initial Value for BtnDrag click & drag }
    InitX: Integer;   { initial X coord for BtnDrag click & drag }
    InitY: Integer;   { initial Y coord for BtnDrag click & drag }
    NeedCalcHoveredBtnInPaint: Boolean;
    PrevCTRLDown: Boolean;  { wheter CTRL was pressed in previous MouseMove }
    PrevHeight: Integer;
    PushedBtn: SmallInt;  { ommits invisible } 
    RedrawMode: TRedrawMode;
    FWidth: Integer;
    CustomChange: TObjectMethod;
    CustomMouseUp: TObjectMethod;
    TimerEvent: TObjectMethod;
    procedure AdjustWidth;
    function CalcDiscreteMode(AValue: Double): Double; inline;
    function CalcHoveredButton(X: Integer): Boolean;
    procedure CalcInternalGeometry;
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
                                     {%H-}WithThemeSpace: Boolean); override;
    procedure CMBiDiModeChanged(var Message: TLMessage); message CM_BIDIMODECHANGED;
    procedure CMColorChanged(var {%H-}Message: TLMessage); message CM_COLORCHANGED;
    procedure CMParentColorChanged(var Message: TLMessage); message CM_PARENTCOLORCHANGED;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
    procedure DoBtnBigDecClick; virtual;
    procedure DoBtnBigIncClick; virtual;
    procedure DoBtnDecClick; virtual;
    procedure DoBtnIncClick; virtual;
    procedure DoTimerRepeatingMode(Sender: TObject);
    procedure DrawButtons;
    procedure EndMouseDrag;                   
    procedure FontChanged(Sender: TObject); override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure RecalcRedraw; virtual;
    procedure Resize; override;
    procedure SetBtnsSorted;
    procedure SetDecBtnsEnabled(AEnabled: Boolean);
    procedure SetIncBtnsEnabled(AEnabled: Boolean);
    procedure SetParent(NewParent: TWinControl); override;
    procedure SetValue(AValue: Double);
    procedure SortSpeedBtns(TheKind: TBtnKind; NewValue, OldValue: Word);
    procedure StopTimer;
    procedure VisibleChanged; override;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor;
  public
    BtnsSorted: array [0..Byte(high(TBtnKind))] of TSingleSpinBtn;
    BtnsUnsorted: array [low(TBtnKind)..high(TBtnKind)] of TSingleSpinBtn;
    HoveredBtn: SmallInt;      { ommits invisible } 
    HoveredBtnReal: SmallInt;  { includes invisible }
    UpdateCount: SmallInt;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;         
    procedure BtnBigDecClick;
    procedure BtnBigIncClick;
    procedure BtnDecClick;
    procedure BtnIncClick;
    procedure BtnMaxClick;
    procedure BtnMenuClick;
    procedure BtnMiddleClick;
    procedure BtnMinClick;
    procedure EndUpdate;
    procedure Redraw;
    property BtnBigDec: TSingleSpinBtn read BtnsUnsorted[ebkBigDec] write BtnsUnsorted[ebkBigDec];
    property BtnBigInc: TSingleSpinBtn read BtnsUnsorted[ebkBigInc] write BtnsUnsorted[ebkBigInc];
    property BtnDec: TSingleSpinBtn read BtnsUnsorted[ebkDec] write BtnsUnsorted[ebkDec];
    property BtnDrag: TSingleSpinBtn read BtnsUnsorted[ebkDrag] write BtnsUnsorted[ebkDrag];
    property BtnInc: TSingleSpinBtn read BtnsUnsorted[ebkInc] write BtnsUnsorted[ebkInc];
    property BtnMax: TSingleSpinBtn read BtnsUnsorted[ebkMax] write BtnsUnsorted[ebkMax];
    property BtnMenu: TSingleSpinBtn read BtnsUnsorted[ebkMenu] write BtnsUnsorted[ebkMenu];
    property BtnMiddle: TSingleSpinBtn read BtnsUnsorted[ebkMiddle] write BtnsUnsorted[ebkMiddle];
    property BtnMin: TSingleSpinBtn read BtnsUnsorted[ebkMin] write BtnsUnsorted[ebkMin];
    property DiscreteChange: Double read FDiscreteChange write SetDiscreteChange;
    property DragControl: TExtraMouseButtons read FDragControl write FDragControl default [];
    property DragOrientation: TDragOrientation read FDragOrientation write SetDragOrientation default edoVertical;
    property GlyphStyle: TGlyphStyle read FGlyphStyle write SetGlyphStyle default egsArrowsA;
    property Images: TCustomImageList read FImages write SetImages;
    property Increment: Double read FIncrement write FIncrement;
    property Max: Double read FMax write SetMax;
    property MenuControl: TExtraMouseButtons read FMenuControl write FMenuControl default [];
    property Min: Double read FMin write SetMin;
    property Middle: Double read FMiddle write SetMiddle;  {don't change order, it's intently after Max & Min}
    property Mode: TIncrementalMode read FMode write SetMode default eimContinuous;
    property MouseFromMiddle: Boolean read FMouseFromMiddle write FMouseFromMiddle default False;
    property MouseIncrementX: Double read FMouseIncrementX write FMouseIncrementX;
    property MouseIncrementY: Double read FMouseIncrementY write FMouseIncrementY;
    property MouseStepPixelsX: Word read FMouseStepPixelsX write SetMouseStepPixelsX default 1;
    property MouseStepPixelsY: Word read FMouseStepPixelsY write SetMouseStepPixelsY default 1;
    property PageSize: Double read FPageSize write FPageSize;
    property Reversed: Boolean read FReversed write SetReversed default False;
    property Spacing: SmallInt read FSpacing write SetSpacing default 0;
    property Style: TButtonStyle read FStyle write SetStyle default ebsSeparated;
    property TimerDelay: Integer read FTimerDelay write SetTimerDelay default cDefCTDelay;
    property TimerRepeating: Integer read FTimerRepeating write SetTimerRepeating default cDefCTRepeat;
    property Value: Double read FValue write SetValue;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDrawGlyph: TOnDrawGlyph read FOnDrawGlyph write FOnDrawGlyph;
    property OnMenuClick: TNotifyEvent read FOnMenuClick write FOnMenuClick;
  end;

  { TECSpinBtns }
  TECSpinBtns = class(TCustomSpinBtns)
  private
    procedure SetController(AValue: TECSpinController);
  public
    destructor Destroy; override;
  published
    property Align;
    property Anchors;
    property BiDiMode;
    property BtnBigDec;
    property BtnBigInc;
    property BtnDec;
    property BtnDrag;
    property BtnInc;
    property BtnMax;
    property BtnMenu;
    property BtnMiddle;
    property BtnMin;
    {property Color;}  {does nothing ATM}
    property Controller: TECSpinController read FController write SetController;
    property DiscreteChange;
    property DragControl;
    property DragOrientation;
    property Enabled;
    property Font;
    property GlyphStyle;
    property Images;
    property Increment;
    property Max;
    property MenuControl;
    property Min;
    property Middle;
    property Mode;
    property MouseFromMiddle;
    property MouseIncrementX;
    property MouseIncrementY;
    property MouseStepPixelsX;
    property MouseStepPixelsY;
    property PageSize;
    property ParentBiDiMode;
    {property ParentColor;}  {does nothing ATM}
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Reversed;
    property ShowHint;
    property Spacing;
    property Style;
    property TimerDelay;
    property TimerRepeating;
    property Value;
    property Visible;
    property Width stored False;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDrawGlyph;
    property OnMenuClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
  end;
  
  { TECSpinBtnsPlus }
  TECSpinBtnsPlus = class(TCustomSpinBtns)
  private
    FMaxInEdit: Double;
    FMinInEdit: Double;
    procedure SetMaxInEdit(AValue: Double);
    procedure SetMax(AValue: Double); override;
    procedure SetMinInEdit(AValue: Double);
    procedure SetMin(AValue: Double); override;
  protected
    procedure DoBtnBigDecClick; override;
    procedure DoBtnBigIncClick; override;
    procedure DoBtnDecClick; override;
    procedure DoBtnIncClick; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure RecalcRedraw; override;
    procedure SetValue(AValue: Double; RaiseCustomChange, ExceedLimit: Boolean);
  public
    constructor Create(AOwner: TComponent); override; 
  published
    property AnchorSideLeft stored False;
    property AnchorSideTop stored False;
    property AnchorSideRight stored False;
    property AnchorSideBottom stored False;
    property BtnBigDec;
    property BtnBigInc;
    property BtnDec;
    property BtnDrag;
    property BtnInc;
    property BtnMax;
    property BtnMenu;
    property BtnMiddle;
    property BtnMin;
    property DiscreteChange;
    property DragControl;
    property DragOrientation;
    property Font; 
    property GlyphStyle;
    property Height stored False;
    property Images;
    property Increment;
    property Left stored False;
    property Max;
    property MenuControl;
    property Min;    
    property MaxInEdit: Double read FMaxInEdit write SetMaxInEdit;
    property MinInEdit: Double read FMinInEdit write SetMinInEdit;
    property Middle;
    property Mode;
    property MouseFromMiddle;
    property MouseIncrementX;
    property MouseIncrementY;
    property MouseStepPixelsX;
    property MouseStepPixelsY;
    property PageSize; 
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Reversed;
    property ShowHint;
    property Spacing;
    property Style;
    property TimerDelay;
    property TimerRepeating;
    property Top stored False;
    property Width stored False;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDrawGlyph;
    property OnMenuClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
  end;                           
    
  { TECSpinEditSpacing }
  TECSpinEditSpacing = class(TControlBorderSpacing)
  public
    function GetSpace(Kind: TAnchorKind): Integer; override;
    procedure GetSpaceAround(var SpaceAround: TRect); override;
  end;       
    
  { TECSpinEdit }
  TECSpinEdit = class(TCustomEdit)
  private
    FActionAltEnter: TModifierEnter;
    FActionCtrlEnter: TModifierEnter;
    FActionShiftEnter: TModifierEnter; 
    FDateTimeFormat: string;
    FDigits: Word;
    FIndent: SmallInt;
    FItems: TStrings;
    FMantissaExp: Double;
    FOnVisibleChanged: TOnVisibleChanged;
    FOptions: TSEOptions;
    FSpinBtns: TECSpinBtnsPlus;
    FValueFormat: TValueFormat;
    function GetController: TECSpinController;
    function GetValue: Double;
    function GetWidthInclBtns: Integer;
    procedure SetController(AValue: TECSpinController);
    procedure SetDateTimeFormat(const AValue: string);
    procedure SetDigits(AValue: Word);
    procedure SetIndent(AValue: SmallInt);
    procedure SetItems(AValue: TStrings);
    procedure SetMantissaExp(AValue: Double);
    procedure SetValue(AValue: Double);
    procedure SetValueFormat(AValue: TValueFormat);
    procedure SetWidthInclBtns(AValue: Integer);
  protected
    TextEdited: Boolean;
    procedure Change; override;
    function ChildClassAllowed(ChildClass: TClass): boolean; override;     
    procedure CMBiDiModeChanged(var Message: TLMessage); message CM_BIDIMODECHANGED;
    function CreateControlBorderSpacing: TControlBorderSpacing; override;      
    procedure DoOnChangeBounds; override;
    function GetBigDecreasedValue: Double;
    function GetBigIncreasedValue: Double;
    function GetDecreasedValue: Double;
    function GetIncreasedValue: Double;
    procedure InitializeWnd; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure RewriteText;
    procedure SetEnabled(Value: Boolean); override;
    procedure SetParent(NewParent: TWinControl); override;
    procedure SetSpinBtnsPosition;
    procedure VisibleChanged; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EditingDone; override;
    procedure EndUpdate;
    function GetText(AValue: Double; ARound: Integer): string;
    function GetText: string;
    procedure GetValueFromString(const AString: string);
    procedure SetRealBoundRect(ARect: TRect);
    procedure SetRealBounds(ALeft, ATop, AWidth, AHeight: Integer);
    procedure SwitchOption(AOption: TSEOption; AOn: Boolean);
    function TryGetValueFromString(AString: string; out AValue: Double): Boolean;
  published
    property ActionAltEnter: TModifierEnter read FActionAltEnter write FActionAltEnter default cDefActAltEnter;
    property ActionCtrlEnter: TModifierEnter read FActionCtrlEnter write FActionCtrlEnter default cDefActCtrlEnter;
    property ActionShiftEnter: TModifierEnter read FActionShiftEnter write FActionShiftEnter default cDefActShiftEnter;
    property Align;
    property Alignment;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BiDiMode;
    property BorderSpacing;
    property BorderStyle;    
    property Buttons: TECSpinBtnsPlus read FSpinBtns write FSpinBtns;
    property Color;
    property Constraints;
    property Controller: TECSpinController read GetController write SetController;
    property DateTimeFormat: string read FDateTimeFormat write SetDateTimeFormat;
    property Digits: Word read FDigits write SetDigits default 0;
    property DragCursor;
    property DragKind;
    property DragMode;   
    property Enabled;
    property Font;
    property HideSelection;     
    property Indent: SmallInt read FIndent write SetIndent default 0;
    property Items: TStrings read FItems write SetItems;
    property MantissaExp: Double read FMantissaExp write SetMantissaExp;
    property Options: TSEOptions read FOptions write FOptions default cDefSEOptions;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;  
    property ReadOnly;
    property ShowHint; 
    property TabOrder;
    property TabStop;
    property Value: Double read GetValue write SetValue;
    property ValueFormat: TValueFormat read FValueFormat write SetValueFormat default evfRound;
    property Visible;
    property WidthInclBtns: Integer read GetWidthInclBtns write SetWidthInclBtns stored False;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property OnVisibleChanged: TOnVisibleChanged read FOnVisibleChanged write FOnVisibleChanged;
  end;

implementation

{ TCustomECTimer }

constructor TCustomECTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Enabled := False;
  FDelay := cDefCTDelay;
  Interval := cDefCTDelay;
  FMaxCount := 0;
  FRepeating := cDefCTRepeat;
end;

procedure TCustomECTimer.DoOnTimer;
var aCounter: Integer;
    aStartTimer, aStopTimer: TNotifyEvent;
begin
  {$IFDEF DBGSPINS} DebugLn('TCustomECTimer.DoOnTimer'); {$ENDIF}
  aCounter := Counter;
  if aCounter = 0 then
    begin
      aStartTimer:=OnStartTimer;
      aStopTimer:=OnStopTimer;
      OnStartTimer:=nil;
      OnStopTimer:=nil;
      Interval := FRepeating;
      OnStartTimer:=aStartTimer;
      OnStopTimer:=aStopTimer;
    end;
  inherited DoOnTimer;
  inc(aCounter); 
  if (MaxCount > 0) and (aCounter >= MaxCount) 
    then Enabled := False
    else FCounter := aCounter;
end;

procedure TCustomECTimer.SetEnabled(Value: Boolean);
begin
  {$IFDEF DBGSPINS} DebugLn('TCustomECTimer.SetEnabled ' + BoolToStr(Value)); {$ENDIF}
  if Value then
    begin
      FCounter := 0;
      Interval := FDelay;      
    end;
  inherited SetEnabled(Value);
end;

{ TECSpinController }

constructor TECSpinController.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActionAltEnter := cDefActAltEnter;
  FActionCtrlEnter := cDefActCtrlEnter;
  FActionShiftEnter := cDefActShiftEnter;   
  ClientList := TFPList.Create;
  FBtnBigDecWidth := cDefSSBWidth;
  FBtnBigIncWidth := cDefSSBWidth;
  FBtnDecWidth := cDefSSBWidth;
  FBtnDragWidth := cDefSSBWidth;
  FBtnIncWidth := cDefSSBWidth;
  FBtnMaxWidth := cDefSSBWidth;
  FBtnMenuWidth := cDefSSBWidth;
  FBtnMiddleWidth := cDefSSBWidth;
  FBtnMinWidth := cDefSSBWidth;
  FGlyphStyle := egsArrowsA;
  FOptions := cDefSEOptions;
  FTimerDelay := cDefCTDelay;
  FTimerRepeating := cDefCTRepeat;
end;

destructor TECSpinController.Destroy;
var i: Integer;
begin
  {$IFDEF DBGSPINS} DebugLn('TECSpinController.Destroy'); {$ENDIF}
  for i := 0 to ClientList.Count - 1 do
    if TControl(ClientList[i]) is TCustomSpinBtns
      then (TControl(ClientList[i]) as TCustomSpinBtns).FController := nil
      else if TControl(ClientList[i]) is TECSpinBtns then (TControl(ClientList[i]) as TECSpinBtns).FController := nil;
  FreeAndNil(ClientList);
  inherited Destroy;
end;

function TECSpinController.IsClientButtons(AClient: Pointer; out ABtns: TCustomSpinBtns): Boolean;
begin
  if TControl(AClient) is TCustomSpinBtns then ABtns := TControl(AClient) as TCustomSpinBtns;
  if TControl(AClient) is TECSpinEdit then ABtns := (TControl(AClient) as TECSpinEdit).Buttons;
  Result := (ABtns is TCustomSpinBtns);
end;

procedure TECSpinController.RegisterClient(AClient: TControl);
begin
  if (AClient is TECSpinEdit) or (AClient is TCustomSpinBtns) then
    begin
      ClientList.Add(AClient);
      if AClient is TECSpinEdit then
        begin
          SetupButtons((AClient as TECSpinEdit).Buttons);
          SetupSpinEdit(AClient as TECSpinEdit);
        end else
        if AClient is TCustomSpinBtns then SetupButtons(AClient as TCustomSpinBtns);
    end;
end;

procedure TECSpinController.SetupButtons(AButtons: TCustomSpinBtns);
begin
  with AButtons do
    begin
      BtnBigDec.Width := BtnBigDecWidth;
      BtnBigInc.Width := BtnBigIncWidth;
      BtnDec.Width := BtnDecWidth;
      BtnDrag.Width := BtnDragWidth;
      BtnInc.Width := BtnIncWidth;
      BtnMax.Width := BtnMaxWidth;
      BtnMenu.Width := BtnMenuWidth;
      BtnMiddle.Width := BtnMiddleWidth;
      BtnMin.Width := BtnMinWidth;
      GlyphStyle := self.GlyphStyle;
      Reversed := self.Reversed;
      Spacing := self.Spacing;
      Style := self.Style;
      TimerDelay := self.TimerDelay;
      TimerRepeating := self.TimerRepeating;
    end;
end;

procedure TECSpinController.SetupSpinEdit(ASpinEdit: TECSpinEdit);
begin
  with ASpinEdit do
    begin
      ActionAltEnter := self.ActionAltEnter;
      ActionCtrlEnter := self.ActionCtrlEnter;
      ActionShiftEnter := self.ActionShiftEnter;
      Indent := self.Indent;
      Options := self.Options;
    end;
end;


procedure TECSpinController.UnRegisterClient(AClient: TControl);
var i: Integer;
begin
  for i := 0 to ClientList.Count - 1 do
    if TCustomSpinBtns(ClientList[i]) = AClient then
      begin
        ClientList.Delete(i);
        break;
      end;
end;

{TECSpinController.Setters}

procedure TECSpinController.SetActionAltEnter(AValue: TModifierEnter);
var i: Integer;
begin
  if FActionAltEnter = AValue then exit;
  FActionAltEnter := AValue;
  for i := 0 to ClientList.Count -1 do
    if assigned(ClientList[i]) and (TControl(ClientList[i]) is TECSpinEdit) then
      (TControl(ClientList[i]) as TECSpinEdit).ActionAltEnter := AValue;
end;

procedure TECSpinController.SetActionCtrlEnter(AValue: TModifierEnter);
var i: Integer;
begin
  if FActionCtrlEnter = AValue then exit;
  FActionCtrlEnter := AValue;
  for i := 0 to ClientList.Count -1 do
    if assigned(ClientList[i]) and (TControl(ClientList[i]) is TECSpinEdit) then
      (TControl(ClientList[i]) as TECSpinEdit).ActionCtrlEnter := AValue;
end;

procedure TECSpinController.SetActionShiftEnter(AValue: TModifierEnter);
var i: Integer;
begin
  if FActionShiftEnter = AValue then exit;
  FActionShiftEnter := AValue;
  for i := 0 to ClientList.Count -1 do
    if assigned(ClientList[i]) and (TControl(ClientList[i]) is TECSpinEdit) then
      (TControl(ClientList[i]) as TECSpinEdit).ActionShiftEnter := AValue;
end;   

procedure TECSpinController.SetBtnBigDecWidth(AValue: Integer);
var i: Integer;
    aBtns: TCustomSpinBtns;
begin
  if FBtnBigDecWidth = AValue then exit;
  FBtnBigDecWidth := AValue;
  for i := 0 to ClientList.Count - 1 do
    if IsClientButtons(ClientList[i], aBtns) then aBtns.BtnBigDec.Width := AValue;
end;

procedure TECSpinController.SetBtnBigIncWidth(AValue: Integer);
var i: Integer;
    aBtns: TCustomSpinBtns;
begin
  if FBtnBigIncWidth = AValue then exit;
  FBtnBigIncWidth := AValue;
  for i := 0 to ClientList.Count - 1 do
    if IsClientButtons(ClientList[i], aBtns) then aBtns.BtnBigInc.Width := AValue;
end;

procedure TECSpinController.SetBtnDecWidth(AValue: Integer);
var i: Integer;
    aBtns: TCustomSpinBtns;
begin
  if FBtnDecWidth = AValue then exit;
  FBtnDecWidth := AValue;
  for i := 0 to ClientList.Count - 1 do
    if IsClientButtons(ClientList[i], aBtns) then aBtns.BtnDec.Width := AValue;
end;

procedure TECSpinController.SetBtnDragWidth(AValue: Integer);
var i: Integer;
    aBtns: TCustomSpinBtns;
begin
  if FBtnDragWidth = AValue then exit;
  FBtnDragWidth := AValue;
  for i := 0 to ClientList.Count - 1 do
    if IsClientButtons(ClientList[i], aBtns) then aBtns.BtnDrag.Width := AValue;
end;

procedure TECSpinController.SetBtnIncWidth(AValue: Integer);
var i: Integer;
    aBtns: TCustomSpinBtns;
begin
  if FBtnIncWidth = AValue then exit;
  FBtnIncWidth := AValue;
  for i := 0 to ClientList.Count - 1 do
    if IsClientButtons(ClientList[i], aBtns) then aBtns.BtnInc.Width := AValue;
end;

procedure TECSpinController.SetBtnMaxWidth(AValue: Integer);
var i: Integer;
    aBtns: TCustomSpinBtns;
begin
  if FBtnMaxWidth = AValue then exit;
  FBtnMaxWidth := AValue;
  for i := 0 to ClientList.Count - 1 do
    if IsClientButtons(ClientList[i], aBtns) then aBtns.BtnMax.Width := AValue;
end;

procedure TECSpinController.SetBtnMenuWidth(AValue: Integer);
var i: Integer;
    aBtns: TCustomSpinBtns;
begin
  if FBtnMenuWidth = AValue then exit;
  FBtnMenuWidth := AValue;
  for i := 0 to ClientList.Count - 1 do
    if IsClientButtons(ClientList[i], aBtns) then aBtns.BtnMenu.Width := AValue;
end;

procedure TECSpinController.SetBtnMiddleWidth(AValue: Integer);
var i: Integer;
    aBtns: TCustomSpinBtns;
begin
  if FBtnMiddleWidth = AValue then exit;
  FBtnMiddleWidth := AValue;
  for i := 0 to ClientList.Count - 1 do
    if IsClientButtons(ClientList[i], aBtns) then aBtns.BtnMiddle.Width := AValue;
end;

procedure TECSpinController.SetBtnMinWidth(AValue: Integer);
var i: Integer;
    aBtns: TCustomSpinBtns;
begin
  if FBtnMinWidth = AValue then exit;
  FBtnMinWidth := AValue;
  for i := 0 to ClientList.Count - 1 do
    if IsClientButtons(ClientList[i], aBtns) then aBtns.BtnMin.Width := AValue;
end;

procedure TECSpinController.SetGlyphStyle(AValue: TGlyphStyle);
var i: Integer;
    aBtns: TCustomSpinBtns;
begin
  if FGlyphStyle = AValue then exit;
  FGlyphStyle := AValue;
  for i := 0 to ClientList.Count - 1 do
    if IsClientButtons(ClientList[i], aBtns) then aBtns.GlyphStyle := AValue;
end;

procedure TECSpinController.SetIndent(AValue: SmallInt);
var i: Integer;
begin
  if FIndent = AValue then exit;
  FIndent := AValue;
  for i := 0 to ClientList.Count -1 do
    if assigned(ClientList[i]) and (TControl(ClientList[i]) is TECSpinEdit) then
      (TControl(ClientList[i]) as TECSpinEdit).Indent := AValue;
end;

procedure TECSpinController.SetOptions(AValue: TSEOptions);
var i: Integer;
begin
  if FOptions = AValue then exit;
  FOptions := AValue;
  for i := 0 to ClientList.Count - 1 do
    if assigned(ClientList[i]) and (TControl(ClientList[i]) is TECSpinEdit) then
      (TControl(ClientList[i]) as TECSpinEdit).Options := AValue;
end;

procedure TECSpinController.SetReversed(AValue: Boolean);
var i: Integer;
    aBtns: TCustomSpinBtns;
begin
  if FReversed = AValue then exit;
  FReversed := AValue;
  for i := 0 to ClientList.Count - 1 do
    if IsClientButtons(ClientList[i], aBtns) then aBtns.Reversed := AValue;
end;

procedure TECSpinController.SetSpacing(AValue: SmallInt);
var i: Integer;
    aBtns: TCustomSpinBtns;
begin
  if FSpacing = AValue then exit;
  FSpacing := AValue;
  for i := 0 to ClientList.Count - 1 do
    if IsClientButtons(ClientList[i], aBtns) then aBtns.Spacing := AValue;
end;

procedure TECSpinController.SetStyle(AValue: TButtonStyle);
var i: Integer;
    aBtns: TCustomSpinBtns;
begin
  if FStyle = AValue then exit;
  FStyle := AValue;
  for i := 0 to ClientList.Count - 1 do
    if IsClientButtons(ClientList[i], aBtns) then aBtns.Style := AValue;
end;

procedure TECSpinController.SetTimerDelay(AValue: Integer);
var i: Integer;
    aBtns: TCustomSpinBtns;
begin
  if FTimerDelay = AValue then exit;
  FTimerDelay := AValue;
  for i := 0 to ClientList.Count - 1 do
    if IsClientButtons(ClientList[i], aBtns) then aBtns.TimerDelay := AValue;
end;

procedure TECSpinController.SetTimerRepeating(AValue: Integer);
var i: Integer;
    aBtns: TCustomSpinBtns;
begin
  if FTimerRepeating = AValue then exit;
  FTimerRepeating := AValue;
  for i := 0 to ClientList.Count - 1 do
    if IsClientButtons(ClientList[i], aBtns) then aBtns.TimerRepeating := AValue;
end;

{ TSingleSpinBtn }

constructor TSingleSpinBtn.Create(AParent: TCustomSpinBtns);
begin
  inherited Create;
  Parent := AParent;  {don't change order}
  FEnabled := True;
  FGlyphColor := clDefault;
  FImageIndex := -1;
  FWidth := cDefSSBWidth;
  FVisible := True;
end;

destructor TSingleSpinBtn.Destroy;
begin
  if FVisible then FreeBitmaps;
  inherited Destroy;
end;

procedure TSingleSpinBtn.CreateBitmaps;
var aState: TItemState;
begin
  for aState := low(TItemState) to eisPushed do
    begin
      BtnBitmaps[aState] := TBitmap.Create;
      BtnBitmaps[aState].SetProperties(self.Width, Parent.Height);
    end;
end;

procedure TSingleSpinBtn.FreeBitmaps;
var aState: TItemState;
begin
  for aState := low(TItemState) to eisPushed do
    FreeAndNil(BtnBitmaps[aState]);
end;

procedure TSingleSpinBtn.Resize;
var aState: TItemState;
    h, w: Integer;
begin
  if FVisible{ and Parent.HandleAllocated }then
    begin
      w := Width;
      h := Parent.Height;
      for aState:=low(TItemState) to eisPushed do
        BtnBitmaps[aState].SetSize(w, h);
    end;
end;

{ TSingleSpinBtn.Setters }

procedure TSingleSpinBtn.SetBtnOrder(AValue: Word);
var oldBtnOrder: Word;
begin
  oldBtnOrder := BtnOrder;
  if (oldBtnOrder = AValue) or (AValue > Byte(high(TBtnKind))) then exit;
  FBtnOrder := AValue;
  Parent.SortSpeedBtns(Kind, AValue, oldBtnOrder);
  if Parent.UpdateCount = 0 then
    begin
      Parent.CalcInternalGeometry;
      with Parent do
        if Style > ebsSeparated then DrawButtons;
      Parent.Invalidate;
    end else Parent.RedrawMode := ermRecalcRedraw;
end;

procedure TSingleSpinBtn.SetCaption(const AValue: string);
begin
  if FCaption = AValue then exit;
  FCaption := AValue;
  Parent.Redraw;
end;

procedure TSingleSpinBtn.SetGlyphColor(AValue: TColor);
begin
  if FGlyphColor = AValue then exit;
  FGlyphColor := AValue;
  Parent.Redraw;
end;

procedure TSingleSpinBtn.SetImageIndex(AValue: TImageIndex);
begin
  if FImageIndex = AValue then exit;
  FImageIndex := AValue;
  if assigned(Parent.FImages) then Parent.Redraw;
end;

procedure TSingleSpinBtn.SetVisible(AValue: Boolean);
begin
  if FVisible = AValue then exit;
  FVisible := AValue;
  if AValue 
    then CreateBitmaps
    else FreeBitmaps;
  Parent.RecalcRedraw;
end;

procedure TSingleSpinBtn.SetWidth(AValue: SmallInt);
begin
  if FWidth = AValue then exit;
  FWidth := AValue;
  Resize;
  Parent.RecalcRedraw;
end;

{ TCustomSpinBtns }

constructor TCustomSpinBtns.Create(AOwner: TComponent);
var aKind: TBtnKind;

  function CreateAndSetBtn(AMouseClickMethod: TObjectMethod; AKind: TBtnKind): TSingleSpinBtn;
  begin
    Result := TSingleSpinBtn.Create(self);
    with Result do
      begin
        Parent := self;
        Click := AMouseClickMethod;
        FKind := AKind;
      end;
  end;

begin
  inherited Create(AOwner);
  AutoSize := True;
  ControlStyle:=ControlStyle + [csCaptureMouse, csNoFocus, csParentBackground, csReplicatable] 
                             - csMultiClicks - [csOpaque, csSetCaption];
  BtnMin := CreateAndSetBtn(@BtnMinClick, ebkMin);
  BtnBigDec := CreateAndSetBtn(@BtnBigDecClick, ebkBigDec);
  BtnDec := CreateAndSetBtn(@BtnDecClick, ebkDec);
  BtnMiddle := CreateAndSetBtn(@BtnMiddleClick, ebkMiddle);
  BtnDrag := CreateAndSetBtn(nil, ebkDrag);
  BtnMenu := CreateAndSetBtn(@BtnMenuClick, ebkMenu);
  BtnInc := CreateAndSetBtn(@BtnIncClick, ebkInc);
  BtnBigInc := CreateAndSetBtn(@BtnBigIncClick, ebkBigInc);
  BtnMax := CreateAndSetBtn(@BtnMaxClick, ebkMax);
  for aKind := low(TBtnKind) to high(TBtnKind) do
    BtnsUnsorted[aKind].FBtnOrder := Byte(aKind);
  SetBtnsSorted;
  FDiscreteChange := 1;
  FDragOrientation := edoVertical;
  FGlyphStyle := egsArrowsA;
  FIncrement := 1;
  FMax := 100;
  FMin := -100;
  FMode:=eimContinuous;
  FMouseIncrementX := 0.1;
  FMouseIncrementY := 1;
  FMouseStepPixelsX := 1;
  FMouseStepPixelsY := 1;
  FPageSize := 10;
  FTimerDelay := cDefCTDelay;
  FTimerRepeating := cDefCTRepeat;
  FValue := 0;
  HoveredBtn := -1;
  InitX := high(Integer);
  InitY := high(Integer);
  PrevHeight := -1;
  PushedBtn := -1;
  CalcInternalGeometry;
  RedrawMode := ermRedrawBkgnd;
  if PrevHeight = -1 then
    begin
      for aKind := low(TBtnKind) to high(TBtnKind) do
        if BtnsUnsorted[aKind].Visible then BtnsUnsorted[aKind].CreateBitmaps;
      inc(PrevHeight);
    end;
  SetInitialBounds(0, 0, length(BtnsUnsorted)*cDefSSBWidth, 23);
  AccessibleRole := larSpinner;
end;

destructor TCustomSpinBtns.Destroy;
var aBtnKind: TBtnKind;
begin
  for aBtnKind := low(TBtnKind) to high(TBtnKind) do
    FreeAndNil(BtnsUnsorted[aBtnKind]);
  inherited Destroy;
end;

class constructor TCustomSpinBtns.CreateTimer;
begin
  ControlTimer := TCustomECTimer.Create(nil);
end;

class destructor TCustomSpinBtns.DestroyTimer;
begin
  FreeAndNil(ControlTimer);
end;

procedure TCustomSpinBtns.AdjustWidth;
begin
  if AutoSize then
    begin
      InvalidatePreferredSize;
      AdjustSize;
    end;
end;

procedure TCustomSpinBtns.BeginUpdate;
begin
  inc(UpdateCount);
end;

procedure TCustomSpinBtns.BtnBigDecClick;
begin
  if cMouseModifier*GetKeyShiftState = []
    then DoBtnBigDecClick
    else DoBtnDecClick;
end;

procedure TCustomSpinBtns.BtnBigIncClick;
begin
  if cMouseModifier*GetKeyShiftState = []
  then DoBtnBigIncClick
  else DoBtnIncClick;
end;

procedure TCustomSpinBtns.BtnDecClick;
begin
  if cMouseModifier*GetKeyShiftState = []
    then DoBtnDecClick
    else DoBtnBigDecClick;
end;

procedure TCustomSpinBtns.BtnIncClick;
begin
  if cMouseModifier*GetKeyShiftState = []
    then DoBtnIncClick
    else DoBtnBigIncClick;
end;

procedure TCustomSpinBtns.BtnMaxClick;
begin
  Value := FMax;
end;

procedure TCustomSpinBtns.BtnMenuClick;
begin
  {$IFDEF DBGSPINS} DebugLn('TCustomSpinBtns.BtnMenuClick'); {$ENDIF}
  if assigned(FOnMenuClick) then FOnMenuClick(self);
end;

procedure TCustomSpinBtns.BtnMiddleClick;
begin
  Value := FMiddle;
end;

procedure TCustomSpinBtns.BtnMinClick;
begin
  Value := FMin;
end;

function TCustomSpinBtns.CalcDiscreteMode(AValue: Double): Double;
begin
  Result := DiscreteChange*round(AValue/DiscreteChange);
end;

function TCustomSpinBtns.CalcHoveredButton(X: Integer): Boolean;
var aBtnPosLength, aHoveredBtnReal, aPrevHoveredBtn, aPrevHoveredBtnReal, i: Integer; 
begin  { returns True if needs Invalidate (repaint) }
  Result := False;
  aBtnPosLength := length(BtnPositions);
  aPrevHoveredBtn := HoveredBtn;
  i := 0;
  while (i < aBtnPosLength) and (BtnPositions[i] < X) do
    inc(i);
  HoveredBtn := i;
  if (aPrevHoveredBtn <> i) and (PushedBtn = -1) then
    begin
      aHoveredBtnReal := i;
      i := 0;
      while i <= aHoveredBtnReal do
        begin
          if not BtnsSorted[i].Visible then inc(aHoveredBtnReal);
          inc(i);
        end;
      aPrevHoveredBtnReal := HoveredBtnReal;
      HoveredBtnReal := aHoveredBtnReal;
      if BtnsSorted[aHoveredBtnReal].Enabled or BtnsSorted[aPrevHoveredBtnReal].Enabled 
        then Result := True;
    end; 
end;

procedure TCustomSpinBtns.CalcInternalGeometry;
var aCount, aIndex, aPos, i: Integer;
begin
  aCount := 0;
  for i := 0 to Byte(high(TBtnKind)) do
    if BtnsSorted[i].Visible then inc(aCount);
  aIndex := 0;
  aPos := 0;
  SetLength(BtnPositions, aCount);
  for i := 0 to Byte(high(TBtnKind)) do
    if BtnsSorted[i].Visible then
      begin
        BtnsSorted[i].FLeft := aPos + aIndex*FSpacing;
        inc(aPos, BtnsSorted[i].Width);
        if aIndex < aCount 
          then BtnPositions[aIndex] := aPos + ((2*aIndex + 1)*FSpacing) div 2
          else break;
        inc(aIndex);
      end; 
  dec(aCount);
  FWidth := aPos + aCount*FSpacing;
end;

procedure TCustomSpinBtns.CalculatePreferredSize(var PreferredWidth,
            PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  {$IFDEF DBGSPINS} DebugLn('TCustomSpinBtns.CalculatePreferredSize'); {$ENDIF}
  PreferredHeight := 0;
  PreferredWidth := FWidth;
end;

procedure TCustomSpinBtns.CMBiDiModeChanged(var Message: TLMessage);
var aKind: TBtnKind;
    aWidth: Integer;
begin
  inherited CMBidiModeChanged(Message);
  SetBtnsSorted;
  aWidth := Width;
  for aKind := ebkMin to ebkMax do
    BtnsUnsorted[aKind].FLeft := aWidth - BtnsUnsorted[aKind].FLeft - BtnsUnsorted[aKind].Width; 
end;

procedure TCustomSpinBtns.CMColorChanged(var Message: TLMessage);
begin
  BackgroundColor := GetColorResolvingDefault(Color, Parent.Brush.Color);
end;

procedure TCustomSpinBtns.CMParentColorChanged(var Message: TLMessage);
begin
  inherited CMParentColorChanged(Message);
  if not ParentColor then BackgroundColor := GetColorResolvingDefault(Color, Parent.Brush.Color);
end;

procedure TCustomSpinBtns.DoBtnBigDecClick;
begin
  SetValue(FValue - PageSize);
end;

procedure TCustomSpinBtns.DoBtnBigIncClick;
begin
  SetValue(FValue + PageSize)
end;

procedure TCustomSpinBtns.DoBtnDecClick;
begin
  SetValue(FValue - Increment);
end;

procedure TCustomSpinBtns.DoBtnIncClick;
begin
  SetValue(FValue + Increment);
end;

procedure TCustomSpinBtns.DoContextPopup(MousePos: TPoint; var Handled: Boolean);
begin
  Handled := (InitX <> high(Integer));  { Do NOT allow PopupMenu when dragging }
  inherited DoContextPopup(MousePos, Handled);
end;

procedure TCustomSpinBtns.DoTimerRepeatingMode(Sender: TObject);
begin
  TimerEvent;
  ControlTimer.OnTimer := TNotifyEvent(TimerEvent);
end;

procedure TCustomSpinBtns.DrawButtons;
var aBlockBMP: TBitmap;
    aBtnKind: TBtnKind;
    aFlags: Cardinal;
    aTransColor: TColor;
    aGlyphDesign: TGlyphDesign;
    aGlyphStyle: TGlyphStyle;
    aPoint: TPoint;
    aRect, aSrcRect: TRect;
    aState: TItemState;
    bReversed, bSplittedBlock: Boolean;
    i, k: Integer;
  
  procedure DrawBlockButton(AItemState: TItemState);
  var i: SmallInt;
  begin
    aRect.Right := Width;
    aBlockBMP.TransparentClear;
    aBlockBMP.Canvas.DrawButtonBackGround(aRect, AItemState);
    aSrcRect := Rect(0, 0, 0, Height);
    bSplittedBlock := (Style=ebsSplittedBlock);
    for i := 0 to Byte(high(TBtnKind)) do
      if BtnsSorted[i].FVisible then 
        begin
          k := BtnsSorted[i].Width;
          aRect.Right := k;
          aSrcRect.Right := aSrcRect.Right+k;
          BtnsSorted[i].BtnBitmaps[AItemState].TransparentColor := aTransColor;
          BtnsSorted[i].BtnBitmaps[AItemState].TransparentClear;
          BtnsSorted[i].BtnBitmaps[AItemState].Canvas.CopyRect(aRect, aBlockBMP.Canvas, aSrcRect);
          if bSplittedBlock then
            begin
              if aSrcRect.Right < Width then
                begin
                  BtnsSorted[i].BtnBitmaps[AItemState].Canvas.Pen.Color := cl3DShadow;            
                  BtnsSorted[i].BtnBitmaps[AItemState].Canvas.Line
                    (BtnsSorted[i].BtnBitmaps[AItemState].Width - 1, 4, 
                     BtnsSorted[i].BtnBitmaps[AItemState].Width - 1, Height - 4);
                end;
              if aSrcRect.Left > 0 then
                begin
                  BtnsSorted[i].BtnBitmaps[AItemState].Canvas.Pen.Color := cl3DHiLight; 
                  BtnsSorted[i].BtnBitmaps[AItemState].Canvas.Line(0, 4, 0, Height - 4);
                end;
            end;
          aSrcRect.Left := aSrcRect.Left + k;
        end;                             
  end;          
  
begin
  {$IFDEF DBGSPINS} DebugLn('TCustomSpinBtns.DrawButtons'); {$ENDIF}
  aTransColor := ColorToRGB(BackgroundColor) and $FDFBF9 + $010203;  
  aRect.TopLeft := Point(0, 0);
  aRect.Bottom := Height;
  aGlyphStyle := GlyphStyle;
  bReversed := Reversed;
  if Style > ebsSeparated then
    begin
      aBlockBMP := TBitmap.Create;
      with aBlockBMP do
        begin  
          SetSize(self.Width, self.Height);
          Transparent := True;
          TransparentMode := tmFixed;  
          TransparentColor := aTransColor;
        end;
      aRect.Right := Width;
      for aState := low(TItemState) to eisPushed do
        DrawBlockButton(aState);
      FreeAndNil(aBlockBMP);
    end else 
    for i := 0 to Byte(high(TBtnKind)) do
      if BtnsSorted[i].Visible then 
        begin
          aRect.Right := BtnsSorted[i].Width;
          for aState := low(TItemState) to eisPushed do
            begin
              BtnsSorted[i].BtnBitmaps[aState].TransparentColor := aTransColor;
              BtnsSorted[i].BtnBitmaps[aState].TransparentClear;
              BtnsSorted[i].BtnBitmaps[aState].Canvas.DrawButtonBackground(aRect, aState);
            end;   
        end;
  if assigned(OnDrawGlyph) then
    begin
      for aBtnKind := low(TBtnKind) to high(TBtnKind) do
        if BtnsUnsorted[aBtnKind].Visible then
          for aState := low(TItemState) to eisPushed do
            OnDrawGlyph(self, aBtnKind, aState);
    end else
    for i := 0 to Byte(high(TBtnKind)) do  { Draw Glyphs }
      with BtnsSorted[i] do
        if Visible then
          begin
            if Caption <> '' then
              begin  { Draw Caption }
                aRect := Rect(0, 0, FWidth, Height);
                aFlags := DT_NOPREFIX or DT_SINGLELINE or DT_VCENTER or DT_CENTER; 
                for aState := low(TItemState) to eisPushed do
                  begin
                    BtnBitmaps[aState].Canvas.Font.Assign(Parent.Font);
                    BtnBitmaps[aState].Canvas.Font.Color := GetColorResolvingDefault(GlyphColor, clBtnText);
                    with ThemeServices do 
                      DrawText(BtnBitmaps[aState].Canvas, GetElementDetails(caThemedContent[aState]), 
                        Caption, aRect, aFlags, 0);
                  end;
              end else
              begin  { Draw Image from ImageList }
                k := FImageIndex;
                if (k > -1) and assigned(Parent.FImages) and (k < Parent.FImages.Count) then
                  begin
                    aPoint.X := (FWidth-Parent.FImages.Width) div 2;
                    aPoint.Y := (Parent.Height-Parent.FImages.Height) div 2;
                    for aState := low(TItemState) to eisPushed do
                      begin
                        with ThemeServices do 
                          DrawIcon(BtnBitmaps[aState].Canvas, GetElementDetails(caThemedContent[aState]), 
                            aPoint, Parent.FImages, k);
                      end;
                  end else
                  begin  { Draw built-in Glyphs }
                    aBtnKind := Kind;
                    if bReversed and (aGlyphStyle <= egsArrowsC) then
                      case aBtnKind of
                        ebkMin: aBtnKind := ebkMax;
                        ebkBigDec: aBtnKind := ebkBigInc;
                        ebkDec: aBtnKind := ebkInc;
                        ebkInc: aBtnKind := ebkDec;
                        ebkBigInc: aBtnKind := ebkBigDec;
                        ebkMax: aBtnKind := ebkMin;
                      end;
                    case aGlyphStyle of
                      egsArrowsA: 
                        case aBtnKind of
                          ebkMin: aGlyphDesign := egdArrowsMin;
                          ebkBigDec: aGlyphDesign := egdArrowsDD;
                          ebkDec: aGlyphDesign := egdArrowDec;
                          ebkMiddle: aGlyphDesign := egdArrowsMiddle;
                          ebkDrag: 
                            case DragOrientation of
                              edoVertical: aGlyphDesign := egdArrowsUD;
                              edoHorizontal: aGlyphDesign := egdArrowsLR;
                              edoBoth: aGlyphDesign := egdArrowsURDL_S;                                  
                            end;
                          ebkMenu: aGlyphDesign := egdWindowRect;
                          ebkInc: aGlyphDesign := egdArrowInc;
                          ebkBigInc: aGlyphDesign := egdArrowsUU;
                          ebkMax: aGlyphDesign := egdArrowsMax;
                        end;
                      egsArrowsB: 
                        case aBtnKind of
                          ebkMin: aGlyphDesign := egdArrsB_Min;
                          ebkBigDec: aGlyphDesign := egdArrsB_DD;
                          ebkDec: aGlyphDesign := egdArrB_Down;
                          ebkMiddle: aGlyphDesign := egdArrsB_Middle;
                          ebkDrag: 
                            if DragOrientation <> edoHorizontal
                              then aGlyphDesign := egdArrsB_UD
                              else aGlyphDesign := egdArrsB_LR;
                          ebkMenu: aGlyphDesign := egdWindowRound;
                          ebkInc: aGlyphDesign := egdArrB_Up;
                          ebkBigInc: aGlyphDesign := egdArrsB_UU;
                          ebkMax: aGlyphDesign := egdArrsB_Max;    
                        end;
                      egsArrowsC: 
                        case aBtnKind of
                          ebkMin: aGlyphDesign := egdArrC_Min;
                          ebkBigDec: aGlyphDesign := egdArrC_DD;
                          ebkDec: aGlyphDesign := egdArrC_Down;
                          ebkMiddle: aGlyphDesign := egdArrC_Middle;
                          ebkDrag: 
                            if DragOrientation <> edoHorizontal
                              then aGlyphDesign := egdArrC_UD
                              else aGlyphDesign := egdArrC_LR;
                          ebkMenu: aGlyphDesign := egdWindowRect;
                          ebkInc: aGlyphDesign := egdArrC_Up;
                          ebkBigInc: aGlyphDesign := egdArrC_UU;
                          ebkMax: aGlyphDesign := egdArrC_Max;    
                        end;
                      egsComparison: 
                        case aBtnKind of
                          ebkMin: aGlyphDesign := egdArrsB_HMin;
                          ebkBigDec: aGlyphDesign := egdArrsB_LL;
                          ebkDec: aGlyphDesign := egdArrB_Left;
                          ebkMiddle: aGlyphDesign := egdArrsB_HMiddle;
                          ebkDrag: 
                            if DragOrientation <> edoHorizontal
                              then aGlyphDesign := egdArrsB_UD
                              else aGlyphDesign := egdArrsB_LR;
                          ebkMenu: aGlyphDesign := egdWindowRound;
                          ebkInc: aGlyphDesign := egdArrB_Right;
                          ebkBigInc: aGlyphDesign := egdArrsB_RR;
                          ebkMax: aGlyphDesign := egdArrsB_HMax;    
                        end;
                      egsMath: 
                        case aBtnKind of
                          ebkMin: aGlyphDesign := egdArrB_HMin;
                          ebkBigDec: aGlyphDesign := egdMathBigMinus;
                          ebkDec: aGlyphDesign := egdMathMinus;
                          ebkMiddle: aGlyphDesign := egdMathEqual; 
                          ebkDrag: 
                            begin
                              if DragOrientation <> edoHorizontal
                                then aGlyphDesign := egdMathPlusMinus
                                else aGlyphDesign := egdArrsB_LR;
                            end;
                          ebkMenu: aGlyphDesign := egdWindowRound;
                          ebkInc: aGlyphDesign := egdMathPlus;
                          ebkBigInc: aGlyphDesign := egdMathBigPlus;
                          ebkMax: aGlyphDesign := egdArrB_HMax;    
                        end;
                      egsPlayer: 
                        case aBtnKind of
                          ebkMin: aGlyphDesign := egdArrowHMin;
                          ebkBigDec: aGlyphDesign := egdArrowsLL;
                          ebkDec: aGlyphDesign := egdArrowLeft;
                          ebkMiddle: aGlyphDesign := egdPlayPause;
                          ebkDrag: 
                            if DragOrientation <> edoHorizontal
                              then aGlyphDesign := egdPlayUpDown
                              else aGlyphDesign := egdArrowsLR;
                          ebkMenu: aGlyphDesign := egdPlayStop;
                          ebkInc: aGlyphDesign := egdArrowRight;
                          ebkBigInc: aGlyphDesign := egdArrowsRR;
                          ebkMax: aGlyphDesign := egdArrowHMax;    
                        end;
                    end;  {case}
                    aRect := Rect(0, 0, Width, Height);
                    if aGlyphDesign >= egdGrid then InflateRect(aRect, -3, -4);
                    for aState := low(TItemState) to eisPushed do 
                      begin     
                        BtnBitmaps[aState].Canvas.SetRealGlyphColor(GlyphColor, aState);
                        BtnBitmaps[aState].Canvas.DrawGlyph(aRect, aGlyphDesign, aState)
                      end;          
                   end;
              end;
          end;                        
end;

procedure TCustomSpinBtns.EndMouseDrag;
begin
  if InitX <> high(Integer) then
    begin
      Cursor := CursorSwap;
      InitX := high(Integer);
      InitY := high(Integer);
    end; 
end;

procedure TCustomSpinBtns.EndUpdate;
begin
  dec(UpdateCount);
  if UpdateCount = 0 then
    begin
      DrawButtons;
      Invalidate;
    end;
end;

procedure TCustomSpinBtns.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);
  DrawButtons;
end;

procedure TCustomSpinBtns.Loaded;
begin
  {$IFDEF DBGSPINS} DebugLn('TCustomSpinBtns.Loaded'); {$ENDIF}
  inherited Loaded;
  Width := FWidth;  { for the case when buttons are invisible and used as cell editor of grid }
  RecalcRedraw;
end;

procedure TCustomSpinBtns.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var aHoveredBtnReal: Integer;
    
  procedure StartDragging;
  begin
    InitValue := Value;
    InitX := X;
    InitY := Y;
    PrevCTRLDown := (ssCtrl in Shift);
    CursorSwap := Cursor;
    case DragOrientation of
      edoVertical: Cursor := crSizeNS;
      edoHorizontal: Cursor := crSizeWE;
      edoBoth: Cursor := crSizeAll;
    end;
  end;

begin
  {$IFDEF DBGSPINS} DebugLn('TCustomSpinBtns.MouseDown Hovered,Pushed '+inttostr(HoveredBtn)+', '+inttostr(PushedBtn)); {$ENDIF}
  inherited MouseDown(Button, Shift, X, Y);
  if (PushedBtn = -1) and (HoveredBtn >= 0) and (InitX = high(Integer)) then
    begin
      aHoveredBtnReal := HoveredBtnReal;
      if Button = mbLeft then  
        begin
          if BtnsSorted[aHoveredBtnReal].Enabled then
            begin
              PushedBtn := HoveredBtn;
              case BtnsSorted[aHoveredBtnReal].Kind of
                ebkBigDec, ebkDec, ebkInc, ebkBigInc:
                  begin
                    BtnsSorted[aHoveredBtnReal].Click;
                    TimerEvent := BtnsSorted[aHoveredBtnReal].Click;
                    ControlTimer.Delay := TimerDelay;
                    ControlTimer.Repeating := TimerRepeating;
                    ControlTimer.OnTimer := @DoTimerRepeatingMode;
                    ControlTimer.Control := self;
                    ControlTimer.Enabled := True;
                  end;
                ebkDrag: StartDragging;
                otherwise
                  BtnsSorted[aHoveredBtnReal].Click;
              end;
              Invalidate;
            end;
        end else  
        if ([Button]*DragControl <> []) and ((Button <> mbRight) or not assigned(PopupMenu)) then
          begin
            MouseCapture := True;
            StartDragging;
            Invalidate;
          end;
    end;
end;

procedure TCustomSpinBtns.MouseLeave;
begin
  {$IFDEF DBGSPINS} DebugLn('TCustomSpinBtns.MouseLeave'); {$ENDIF}
  inherited MouseLeave;
  if (HoveredBtn >= 0) and (PushedBtn = -1) and BtnsSorted[HoveredBtnReal].Enabled then
    begin
      HoveredBtn := -1;
      Invalidate;
    end;
end;

procedure TCustomSpinBtns.MouseMove(Shift: TShiftState; X, Y: Integer);
var i, j: Integer;
    aMouseIncrement, aValue: Double;
    bCTRLDown: Boolean;
begin
  inherited MouseMove(Shift, X, Y);
  if InitX <> high(Integer) then
    begin  { Drag state }
      bCTRLDown := (ssCtrl in Shift);
      if bCTRLDown <> PrevCTRLDown then
        begin
          InitValue := Value;
          InitX := X;
          InitY := Y;
        end; 
      j := Y - InitY;
      i := X - InitX;  
      if not Reversed then j := -j;
      if IsRightToLeft then i := -i;
      if not MouseFromMiddle 
        then aValue := InitValue
        else aValue := Middle;   
      if DragOrientation <> edoHorizontal then 
        begin
          if not bCTRLDown
            then aMouseIncrement := MouseIncrementY
            else aMouseIncrement := MouseIncrementX;  
          aValue := aValue + aMouseIncrement*(j div MouseStepPixelsY);
        end;
      if DragOrientation <> edoVertical then 
        begin
          if not bCTRLDown
            then aMouseIncrement := MouseIncrementX
            else aMouseIncrement := MouseIncrementY;          
          aValue := aValue + aMouseIncrement*(i div MouseStepPixelsX);
        end;
      Value := EnsureRange(aValue, FMin, FMax);
      PrevCTRLDown := bCTRLDown;
    end else
    begin  { Normal state }
      if CalcHoveredButton(X) then 
        if not MouseCapture then Invalidate;
    end;
end;

procedure TCustomSpinBtns.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var aPrevPushedBtn: SmallInt;
    bMouseEntered: Boolean;
begin         
  { GTK2 bug report no. 21982 }     
  {$IFDEF DBGSPINS} Debugln('TCustomSpinBtns.MouseUp; Hovered, PushedBtn '+inttostr(HoveredBtn)+' '+inttostr(PushedBtn)); {$ENDIF}
  inherited MouseUp(Button, Shift, X, Y);
  if InitX <> high(Integer) then  { any Button stops dragging }
    begin
      EndMouseDrag;
      aPrevPushedBtn := PushedBtn;
      if Button = mbLeft
        then PushedBtn := -1
        else MouseCapture := False;
      if PtInRect(ClientRect, Point(X, Y)) then 
        begin
          CalcHoveredButton(X);
          Invalidate;
        end else
        if (aPrevPushedBtn >= 0) then 
          begin
            HoveredBtn := -1;
            Invalidate;
          end;
    end else
    begin
      if Button = mbLeft then
        begin
          bMouseEntered := PtInRect(ClientRect, Point(X, Y));  { MouseEntered is always True here }
          if not bMouseEntered then HoveredBtn := -1;
          if PushedBtn >= 0 then
            begin
              StopTimer;
              PushedBtn := -1;
              Invalidate;
              if assigned(CustomMouseUp) then CustomMouseUp;
            end else
            if bMouseEntered then Invalidate;
        end else
        begin
          if not (Button in DragControl) then
            if [Button]*MenuControl <> [] 
              then BtnMenuClick
              else if Button = mbMiddle then BtnMiddleClick;
        end; 
    end;
end;

procedure TCustomSpinBtns.Paint;
var aState: TItemState;
    aIndex, aLeft, i: Integer;
    aMousePoint: TPoint;
    bEnabled: Boolean;
begin
  {$IFDEF DBGSPINS} DebugLn('TCustomSpinBtns.Paint, HoveredBtn '+inttostr(HoveredBtn)); {$ENDIF}
  inherited Paint;
  if RedrawMode = ermRecalcRedraw then CalcInternalGeometry; 
  if RedrawMode >= ermRedrawBkgnd then DrawButtons;
  if RedrawMode >= ermFreeRedraw then
    begin
      if NeedCalcHoveredBtnInPaint then  { when Buttons are a CellEditor }
        begin
          aMousePoint := ScreenToClient(Mouse.CursorPos);
          if PtInRect(ClientRect, aMousePoint)
            then CalcHoveredButton(aMousePoint.X)
            else HoveredBtn := -1;
          NeedCalcHoveredBtnInPaint := False;
        end else
        if not MouseCapture then  { solves when virtual desktop is switched during drag }
          begin
            if not MouseEntered then HoveredBtn := -1;
            PushedBtn := -1;
            if ControlTimer.Control = self then StopTimer;
            EndMouseDrag; 
          end; 
      bEnabled := IsEnabled;
      aLeft := 0;
      aIndex := 0;
      for i := 0 to Byte(high(TBtnKind)) do
        if BtnsSorted[i].Visible then
          begin
            aState := eisEnabled;
            if not (bEnabled and BtnsSorted[i].Enabled) 
              then aState := eisDisabled
              else if PushedBtn = -1 then
                     begin
                       if (aIndex = HoveredBtn) and not MouseCapture then aState := eisHighlighted
                     end else
                     if aIndex = PushedBtn then aState := eisPushed;
            Canvas.Draw(aLeft, 0, BtnsSorted[i].BtnBitmaps[aState]);
            aLeft := aLeft + BtnsSorted[i].Width + Spacing;
            inc(aIndex);
          end;
    end;
  RedrawMode := ermFreeRedraw;
end;

procedure TCustomSpinBtns.RecalcRedraw;
begin
  if UpdateCount = 0 then
    begin
      CalcInternalGeometry;
      AdjustWidth;
      RedrawMode := ermRedrawBkgnd;
      Invalidate;
    end;
end;

procedure TCustomSpinBtns.Redraw;
begin
  if RedrawMode < ermRedrawBkgnd then RedrawMode := ermRedrawBkgnd;
  if UpdateCount = 0 then Invalidate;
end;

procedure TCustomSpinBtns.Resize;
var i: Integer;
begin
  {$IFDEF DBGSPINS} DebugLn('TCustomSpinBtns.WMSize W, H '+inttostr(Width)+', '+inttostr(Height)); {$ENDIF}
  inherited Resize;
  if Height<>PrevHeight then
    begin
      for i:=0 to Byte(high(TBtnKind)) do
        BtnsSorted[i].Resize;
      PrevHeight:= Height;
      RedrawMode:=ermRedrawBkgnd;
    end;
end;

procedure TCustomSpinBtns.SetBtnsSorted;
var aKind: TBtnKind;
begin
  {$IFDEF DBGSPINS} DebugLn('TCustomSpinBtns.SetBtnsSorted'); {$ENDIF}
  if not IsRightToLeft 
    then for aKind := low(TBtnKind) to high(TBtnKind) do
           BtnsSorted[BtnsUnsorted[aKind].BtnOrder] := BtnsUnsorted[aKind]
    else for aKind := low(TBtnKind) to high(TBtnKind) do
           BtnsSorted[8-BtnsUnsorted[aKind].BtnOrder] := BtnsUnsorted[aKind];
end;

procedure TCustomSpinBtns.SetDecBtnsEnabled(AEnabled: Boolean);
begin
  BtnMin.FEnabled := aEnabled;
  BtnBigDec.FEnabled := aEnabled;
  BtnDec.FEnabled := aEnabled; 
end;

procedure TCustomSpinBtns.SetIncBtnsEnabled(AEnabled: Boolean);
begin
  BtnInc.FEnabled := aEnabled;
  BtnBigInc.FEnabled := aEnabled;
  BtnMax.FEnabled := aEnabled; 
end;

procedure TCustomSpinBtns.SetParent(NewParent: TWinControl);
begin
  inherited SetParent(NewParent);
  if assigned(NewParent) then BackgroundColor := GetColorResolvingDefault(Color, NewParent.Brush.Color);
end;   

procedure TCustomSpinBtns.SortSpeedBtns(TheKind: TBtnKind; NewValue, OldValue: Word);
var aKind: TBtnKind;
begin
  {$IFDEF DBGSPINS} DebugLn('TCustomSpinBtns.SortSpeenButtons'); {$ENDIF}
  if NewValue < OldValue then
    begin
      if TheKind > low(TBtnKind) then  {pred(ebkMin) = 255 (or 65535 etc.), enums are unsigned}
        for aKind := low(TBtnKind) to pred(TheKind) do
          if (BtnsUnsorted[aKind].BtnOrder >= NewValue) and (BtnsUnsorted[aKind].BtnOrder < OldValue) then
            BtnsUnsorted[aKind].FBtnOrder := BtnsUnsorted[aKind].BtnOrder + 1;
      if TheKind < high(TBtnKind) then
        for aKind := succ(TheKind) to high(TBtnKind) do  {avoids range checks}
          if (BtnsUnsorted[aKind].BtnOrder >= NewValue) and (BtnsUnsorted[aKind].BtnOrder < OldValue) then
            BtnsUnsorted[aKind].FBtnOrder := BtnsUnsorted[aKind].BtnOrder + 1;
    end else
    begin
      if TheKind > low(TBtnKind) then
        for aKind := low(TBtnKind) to pred(TheKind) do
          if (BtnsUnsorted[aKind].BtnOrder <= NewValue) and (BtnsUnsorted[aKind].BtnOrder > OldValue) then
            BtnsUnsorted[aKind].FBtnOrder := BtnsUnsorted[aKind].BtnOrder - 1;
      if TheKind < high(TBtnKind) then
        for aKind := succ(TheKind) to high(TBtnKind) do
          if (BtnsUnsorted[aKind].BtnOrder <= NewValue) and (BtnsUnsorted[aKind].BtnOrder > OldValue) then
            BtnsUnsorted[aKind].FBtnOrder := BtnsUnsorted[aKind].BtnOrder - 1;
    end;
  SetBtnsSorted;
end;

procedure TCustomSpinBtns.StopTimer;
begin
  ControlTimer.Enabled := False;
  ControlTimer.Control := nil;
end;

procedure TCustomSpinBtns.VisibleChanged;
begin
  inherited VisibleChanged;
  if Visible then NeedCalcHoveredBtnInPaint := True;
end;

{ TCustomSpinBtns.Getters + Setters }

procedure TCustomSpinBtns.SetBackgroundColor(AValue: TColor);
begin
  if FBackgroundColor = AValue then exit;
  FBackgroundColor := AValue;
  RedrawMode := ermRedrawBkgnd;   
end;

procedure TCustomSpinBtns.SetDiscreteChange(AValue: Double);
begin
  if FDiscreteChange = AValue then exit;
  FDiscreteChange := AValue;
  if Mode = eimDiscrete then SetValue(FValue);
end;

procedure TCustomSpinBtns.SetDragOrientation(AValue: TDragOrientation);
begin
  if FDragOrientation = AValue then exit;
  FDragOrientation := AValue;
  Redraw;
end;

procedure TCustomSpinBtns.SetGlyphStyle(AValue: TGlyphStyle);
begin
  if FGlyphStyle = AValue then exit;
  FGlyphStyle := AValue;
  Redraw;
end;

procedure TCustomSpinBtns.SetImages(AValue: TCustomImageList);
begin
  if FImages = AValue then exit;
  FImages := AValue;
  Redraw;
end;

procedure TCustomSpinBtns.SetMax(AValue: Double);
var b: Boolean;
begin
  if (FMin < AValue) or (csLoading in ComponentState) or (UpdateCount >  0) then
    begin
      FMax := AValue;
      b := BtnInc.Enabled;  
      if FValue >= AValue then
        begin
          SetIncBtnsEnabled(False);
          if b then 
            if UpdateCount = 0 then Invalidate;
        end else
        begin
          SetIncBtnsEnabled(True);
          if not b then
            if UpdateCount = 0 then Invalidate;
        end;         
    end;
end;

procedure TCustomSpinBtns.SetMiddle(AValue: Double);
begin                                                   
  if ((FMin < AValue) and (AValue < FMax)) or (csLoading in ComponentState) or
    (UpdateCount > 0) then FMiddle := AValue;
end;

procedure TCustomSpinBtns.SetMin(AValue: Double);
var b: Boolean;
begin
  if (AValue < FMax) or (csLoading in ComponentState) or (UpdateCount > 0) then
    begin
      FMin := AValue;
      b := BtnDec.Enabled;
      if FValue <= AValue then 
        begin
          SetDecBtnsEnabled(False);
          if b then 
            if UpdateCount = 0 then Invalidate;
        end else
        begin
          SetDecBtnsEnabled(True);
          if not b then 
            if UpdateCount = 0 then Invalidate;
        end;  
    end;
end;

procedure TCustomSpinBtns.SetMode(AValue: TIncrementalMode);
begin
  if FMode = AValue then exit;
  FMode := AValue;
  if AValue = eimDiscrete then SetValue(FValue);
end;

procedure TCustomSpinBtns.SetMouseStepPixelsX(AValue: Word);
begin
  if AValue > 0 
    then FMouseStepPixelsX := AValue
    else FMouseStepPixelsX := 1;
end;

procedure TCustomSpinBtns.SetMouseStepPixelsY(AValue: Word);
begin
  if AValue > 0 
    then FMouseStepPixelsY := AValue
    else FMouseStepPixelsY := 1; 
end; 

procedure TCustomSpinBtns.SetReversed(AValue: Boolean);
begin
  if FReversed = AValue then exit;
  FReversed := AValue;
  Redraw;
end;

procedure TCustomSpinBtns.SetSpacing(AValue: SmallInt);
begin
  if FSpacing = AValue then exit;
  FSpacing := AValue;
  CalcInternalGeometry;
  AdjustWidth;
  if UpdateCount = 0 then Invalidate;
end;

procedure TCustomSpinBtns.SetStyle(AValue: TButtonStyle);
begin
  if FStyle = AValue then exit;
  FStyle := AValue;
  Redraw; 
end;   

procedure TCustomSpinBtns.SetTimerDelay(AValue: Integer);
begin
  if FTimerDelay = AValue then exit;
  FTimerDelay := AValue;
end;

procedure TCustomSpinBtns.SetTimerRepeating(AValue: Integer);
begin
  if FTimerRepeating = AValue then exit;
  FTimerRepeating := AValue;
end;

procedure TCustomSpinBtns.SetValue(AValue: Double);
var b: Boolean;
begin
  if FMode = eimDiscrete then AValue := CalcDiscreteMode(AValue);
  if FValue = AValue then exit;
  b := BtnDec.Enabled;
  AValue := Math.Max(AValue, FMin);
  if AValue <= FMin then 
    begin
      SetDecBtnsEnabled(False);
      StopTimer;
      if b then 
        if UpdateCount = 0 then Invalidate;
    end else
    begin
      SetDecBtnsEnabled(True);
      if not b then 
        if UpdateCount = 0 then Invalidate;
    end;
  b := BtnInc.Enabled;  
  AValue := Math.Min(AValue, FMax);  
  if AValue >= FMax then
    begin
      SetIncBtnsEnabled(False);
      StopTimer;
      if b then 
        if UpdateCount = 0 then Invalidate;
    end else
    begin
      SetIncBtnsEnabled(True);                                    
      if not b then 
        if UpdateCount = 0 then Invalidate;
    end;
  FValue := AValue;
  if assigned(CustomChange) then CustomChange;
  if assigned(FOnChange) then FOnChange(self);
end;

{ TECSpinBtns }

procedure TECSpinBtns.SetController(AValue: TECSpinController);
begin
  {$IFDEF DBGSPINS} DebugLn('TECSpinBtns.SetController'); {$ENDIF}
  if FController = AValue then exit;
  if assigned(FController) then FController.UnRegisterClient(self);
  FController := AValue;
  if assigned(AValue) then AValue.RegisterClient(self);
end;

destructor TECSpinBtns.Destroy;
begin
  {$IFDEF DBGSPINS} DebugLn('TECSpinBtns.Destroy'); {$ENDIF}
  if assigned(FController) then FController.UnRegisterClient(self);
  inherited Destroy;
end;

{ TECSpinBtnsPlus }

constructor TECSpinBtnsPlus.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF DBGSPINS} if not (Owner is TECSpinEdit) then
    DebugLn(DbgSName(self), ' Warning! Owner is not TECSpinEdit!'); {$ENDIF} 
  ControlStyle := ControlStyle + [csNoDesignSelectable]; 
  MaxInEdit := FMax;
  MinInEdit := FMin;
  SetSubComponent(True);
end;    

procedure TECSpinBtnsPlus.DoBtnBigDecClick;
begin   
  Value := (Owner as TECSpinEdit).GetBigDecreasedValue;
end;

procedure TECSpinBtnsPlus.DoBtnBigIncClick;
begin
  Value := (Owner as TECSpinEdit).GetBigIncreasedValue;
end;

procedure TECSpinBtnsPlus.DoBtnDecClick;
begin
  Value := (Owner as TECSpinEdit).GetDecreasedValue;
end;

procedure TECSpinBtnsPlus.DoBtnIncClick;
begin
  Value := (Owner as TECSpinEdit).GetIncreasedValue;
end;    

procedure TECSpinBtnsPlus.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  with Owner as TWinControl do SetFocus;
end;            

procedure TECSpinBtnsPlus.RecalcRedraw;
begin
  if UpdateCount = 0 then
    begin
      CalcInternalGeometry;
      AdjustWidth;
      (Owner as TECSpinEdit).SetSpinBtnsPosition;
      RedrawMode := ermRedrawBkgnd;
      Invalidate;
    end;   
end;            

procedure TECSpinBtnsPlus.SetMaxInEdit(AValue: Double);
begin
  if  (FMinInEdit < AValue) or (csLoading in ComponentState) or (UpdateCount > 0) then
    begin
      FMaxInEdit := AValue;
      if AValue < FMax then Max := AValue;
      if FValue > AValue then SetValue(AValue, True, True); 
    end;     
end;

procedure TECSpinBtnsPlus.SetMax(AValue: Double);
begin
  if (csLoading in ComponentState) or (UpdateCount > 0)
    then FMaxInEdit := AValue
    else if FMaxInEdit < AValue then MaxInEdit := AValue; 
  inherited SetMax(AValue);
end;

procedure TECSpinBtnsPlus.SetMinInEdit(AValue: Double);
begin
  if (AValue < FMaxInEdit) or (csLoading in ComponentState) or (UpdateCount > 0) then
    begin
      FMinInEdit := AValue;
      if FMin < AValue then Min := AValue;
      if FValue < AValue then SetValue(AValue, True, True);
    end;       
end;

procedure TECSpinBtnsPlus.SetMin(AValue: Double);
begin
  if (csLoading in ComponentState) or (UpdateCount > 0)
    then FMinInEdit := AValue
    else if FMinInEdit > AValue then MinInEdit := AValue; 
  inherited SetMin(AValue);
end;

procedure TECSpinBtnsPlus.SetValue(AValue: Double; RaiseCustomChange, ExceedLimit: Boolean);
var b: Boolean;
begin
  if Mode = eimDiscrete then AValue := CalcDiscreteMode(AValue);
  if FValue = AValue then exit;
  b := BtnDec.Enabled;
  if ExceedLimit 
    then AValue := Math.Max(AValue, FMinInEdit)
    else AValue := Math.Max(AValue, FMin);
  if AValue <= FMin then 
    begin
      SetDecBtnsEnabled(False);
      StopTimer;
      if b then Invalidate;
    end else
    begin
      SetDecBtnsEnabled(True);
      if not b then Invalidate;
    end;
  b := BtnInc.Enabled;  
  if ExceedLimit 
    then AValue := Math.Min(AValue, FMaxInEdit)
    else AValue := Math.Min(AValue, FMax);
  if AValue >= FMax then
    begin
      SetIncBtnsEnabled(False);
      StopTimer;
      if b then Invalidate;
    end else
    begin
      SetIncBtnsEnabled(True);
      if not b then Invalidate;
    end;
  FValue := AValue;
  if RaiseCustomChange and assigned(CustomChange) then CustomChange;
  if assigned(FOnChange) then FOnChange(self);  
end;       

{ TECSpinEditSpacing }

function TECSpinEditSpacing.GetSpace(Kind: TAnchorKind): Integer;
begin
  {$IFDEF DBGSPINS} DebugLn('Spacing.GetSpace'); {$ENDIF}  
  Result:=inherited GetSpace(Kind);
  case Kind of
    akLeft: if Control.IsRightToLeft then
              inc(Result, TECSpinEdit(Control).Buttons.Width + TECSpinEdit(Control).Indent);
    akRight: if not Control.IsRightToLeft then
               inc(Result, TECSpinEdit(Control).Buttons.Width + TECSpinEdit(Control).Indent);
  end;  
end;

procedure TECSpinEditSpacing.GetSpaceAround(var SpaceAround: TRect);
var aIndentButtonWidth: Integer;
begin
  {$IFDEF DBGSPINS} DebugLn('Spacing.GetSpaceAround'); {$ENDIF}
  inherited GetSpaceAround(SpaceAround);
  with TECSpinEdit(Control) do
    aIndentButtonWidth := Buttons.Width + Indent;
  if not Control.IsRightToLeft 
    then inc(SpaceAround.Right, aIndentButtonWidth)
    else inc(SpaceAround.Left, aIndentButtonWidth);
end;
      
{ TECSpinEdit }

constructor TECSpinEdit.Create(AOwner: TComponent);
begin
  FSpinBtns := TECSpinBtnsPlus.Create(self);
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption];
  with FSpinBtns do
    begin                         
      CustomChange := @RewriteText;
      Name := 'ECSpinEditButtons';
      AnchorParallel(akTop, 0, self);
      AnchorParallel(akBottom, 0, self);
    end;         
  FActionAltEnter := cDefActAltEnter;
  FActionCtrlEnter := cDefActCtrlEnter;
  FActionShiftEnter := cDefActShiftEnter;   
  FDateTimeFormat := 'hh:nn:ss';
  FMantissaExp := 2;
  FValueFormat := evfRound;
  FItems := TStringList.Create;
  RewriteText;
  Options := cDefSEOptions;
  AccessibleRole := larSpinner;
end;

destructor TECSpinEdit.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TECSpinEdit.BeginUpdate;
begin
  Buttons.BeginUpdate;
end;

procedure TECSpinEdit.Change;
var aValue: Double;
begin
  if esoEditingChangesValue in Options then 
    if TryGetValueFromString(Text, aValue) then FSpinBtns.SetValue(aValue, False, True);
  inherited Change;
  {$IFDEF DBGSPINS} DebugLn('TECSpinEdit.Change '+BoolToStr(esoEditingChangesValue in Options, 
    'EditChangeVal ', 'EditingNOTChange ') +floattostr(Value)); {$ENDIF}
end;      

function TECSpinEdit.ChildClassAllowed(ChildClass: TClass): boolean;
begin
  Result := (ChildClass = TECSpinBtnsPlus);
end;      

procedure TECSpinEdit.CMBiDiModeChanged(var Message: TLMessage);
begin
  inherited CMBiDiModeChanged(Message);
  FSpinBtns.BiDiMode := BiDiMode;
  SetSpinBtnsPosition; 
end;      

function TECSpinEdit.CreateControlBorderSpacing: TControlBorderSpacing;
begin
  {$IFDEF DBGSPINS}  DebugLn('CreateControlBorderSpacing'); {$ENDIF}
  Result := TECSpinEditSpacing.Create(self);
end; 

procedure TECSpinEdit.DoOnChangeBounds;
begin
  {$IFDEF DBGSPINS} DebugLn('DoOnChangeBounds');  {$ENDIF}
  inherited DoOnChangeBounds;
  if assigned(Buttons) then SetSpinBtnsPosition;
end;

procedure TECSpinEdit.EditingDone;
var aValue: Double;
begin
  {$IFDEF DBGSPINS} DebugLn('TECSpinEdit.EditingDone'); {$ENDIF}
  if not ReadOnly then
    begin
      if TryGetValueFromString(Text, aValue) then SetValue(aValue);
      RewriteText;
    end;
  inherited EditingDone;
end;

procedure TECSpinEdit.EndUpdate;
begin
  Buttons.EndUpdate;
end;

function TECSpinEdit.GetBigDecreasedValue: Double;
var i: Integer;
begin
  if (ValueFormat = evfDate) and (esoSmartDate in Options) then
    begin
      i := trunc(FSpinBtns.PageSize);
      if (i mod 31) = 0 then
        begin
          i := i div 31 ;
          Result := IncMonth(Value, -i);
          exit;  { Exit! }
        end;
      if (i mod 365) = 0 then
        begin
          i := i div 365;
          Result := IncMonth(Value, -12*i);
          exit;  { Exit! }
        end;   
    end;        
  Result := Value - FSpinBtns.PageSize;
end;

function TECSpinEdit.GetBigIncreasedValue: Double;
var i: Integer;
begin
  if (ValueFormat = evfDate) and (esoSmartDate in Options) then
    begin
      i := trunc(FSpinBtns.PageSize);
      if (i mod 31) = 0 then
        begin
          i := i div 31 ;
          Result := IncMonth(Value, i);
          exit;  { Exit! }
        end;
      if (i mod 365) = 0 then
        begin
          i := i div 365;
          Result := IncMonth(Value, 12*i);
          exit;  { Exit! }
        end;   
    end;                   
  Result := Value + FSpinBtns.PageSize;
end;

function TECSpinEdit.GetDecreasedValue: Double;
var i: Integer;
begin
  if (ValueFormat = evfDate) and (esoSmartDate in Options) then
    begin
      i := trunc(FSpinBtns.Increment);
      if (i mod 31) = 0 then
        begin
          i := i div 31 ;
          Result := IncMonth(Value, -i);
          exit;  { Exit! }
        end;
      if (i mod 365) = 0 then
        begin
          i := i div 365;
          Result := IncMonth(Value, -12*i);
          exit;  { Exit! }
        end;   
    end;                   
  Result := Value - FSpinBtns.Increment;
end;

function TECSpinEdit.GetIncreasedValue: Double;
var i: Integer;
begin
  if (ValueFormat = evfDate) and (esoSmartDate in Options) then
    begin
      i := trunc(FSpinBtns.Increment);
      if (i mod 31) = 0 then
        begin
          i := i div 31 ;
          Result := IncMonth(Value, i);
          exit;  { Exit! }
        end;
      if (i mod 365) = 0 then
        begin
          i := i div 365;
          Result := IncMonth(Value, 12*i);
          exit;  { Exit! }
        end;   
    end;         
  Result := Value + FSpinBtns.Increment;
end;
    
function TECSpinEdit.GetText: string;
begin
  Result := GetText(FSpinBtns.Value, FDigits);
end;
   
function TECSpinEdit.GetText(AValue: Double; ARound: Integer): string;
var aFS: TFormatSettings;
    i: Integer;
begin
  case FValueFormat of
    evfRound: Result := floattostrF(AValue, ffFixed, 1, ARound);
    evfExponent: Result := floattostrF(power(FMantissaExp, AValue), ffFixed, 1, ARound);
    evfExponential: Result := floattostrF(AValue, ffExponent, ARound, ARound);
    evfMantissa: Result := floattostrF(power(AValue, FMantissaExp), ffFixed, 1, ARound);
    evfHexadecimal: Result := hexStr(round(AValue), ARound);
    evfMarkHexadec: Result := '$' + hexStr(round(AValue), ARound);
    evfOctal: Result := octStr(round(AValue), ARound);
    evfMarkOctal: Result := '&' + octStr(round(AValue), ARound);
    evfBinary: Result := binStr(round(AValue), ARound);
    evfDate: 
      begin
        aFS := DefaultFormatSettings;
        aFS.LongDateFormat := FDateTimeFormat;
        Result := datetostr(AValue, aFS);
      end;
    evfTime: 
      begin
        aFS := DefaultFormatSettings;
        aFS.LongTimeFormat := FDateTimeFormat;
        Result := timetostr(AValue, aFS);
      end;
    evfText: 
      begin
        i := round(AValue);
        if assigned(FItems) and (i >= 0) and (i < FItems.Count) 
          then Result := FItems[i]
          else Result := '';
      end;
    evfCombined: 
      begin
        i := round(AValue/FSpinBtns.Increment);
        if assigned(FItems) and (i >= 0) and (FItems.Count > 0) then
          begin
            if i = 0
              then Result := FItems[0]
              else
              begin
                Result := floattostrF(AValue, ffFixed, 1, ARound) + ' ';
                if i < FItems.Count
                  then Result := Result + FItems[i]
                  else Result := Result + FItems[FItems.Count - 1];
              end;
          end else
          Result := floattostrF(AValue, ffFixed, 1, ARound);
      end;
  end; {case}
end;      

procedure TECSpinEdit.GetValueFromString(const AString: string);
var aValue: Double;
begin
  if TryGetValueFromString(AString, aValue) 
    then Value := aValue
    else Value := FSpinBtns.Middle;
end;

procedure TECSpinEdit.InitializeWnd;
begin
  inherited InitializeWnd;
  SetSpinBtnsPosition;
end;

procedure TECSpinEdit.KeyDown(var Key: Word; Shift: TShiftState);
var aKey: Word;
    bKeyUsed: Boolean;
    
  procedure ResetActualValue;
  var aValue: Double;
  begin
    {$IFDEF DBGSPINS} DebugLn('TextEdited='+BoolToStr(TextEdited, 'True', 'False')); {$ENDIF}
    if TextEdited and not (esoEditingChangesValue in FOptions) then
      if TryGetValueFromString(Text, aValue) then FSpinBtns.SetValue(aValue, False, True); 
  end;

begin
  {$IFDEF DBGSPINS} DebugLn('TECSpinEdit.KeyDown'); {$ENDIF}
  if FSpinBtns.Enabled then
    begin
      case Key of
        VK_RETURN:  {Enter + Ctrl opens Menu}
          if ((ActionAltEnter = emeMenuClick) and (ssAlt in Shift)) or
            ((ActionCtrlEnter = emeMenuClick) and (ssModifier in Shift)) or
            ((ActionShiftEnter = emeMenuClick) and (ssShift in Shift)) then
            begin
              FSpinBtns.BtnMenuClick;
              bKeyUsed := True;
            end else
            if ((ActionAltEnter = emeMiddleClick) and (ssAlt in Shift)) or
              ((ActionCtrlEnter = emeMiddleClick) and (ssModifier in Shift)) or
              ((ActionShiftEnter = emeMiddleClick) and (ssShift in Shift)) then
              begin
                FSpinBtns.BtnMiddleClick;
                bKeyUsed := True;
              end;
        VK_SPACE:  {(CTRL +) Space clicks Menu or Middle} 
          if (ssModifier in Shift) or ReadOnly then
            begin  
              if esoSpaceClicksMiddle in Options 
                then FSpinBtns.BtnMiddleClick
                else FSpinBtns.BtnMenuClick;
              bKeyUsed := True;
            end;
        otherwise bKeyUsed := False;
      end;
      if not bKeyUsed then
        begin
          aKey := Key;
          if FSpinBtns.Reversed then
            case aKey of  {mirror keys in Y-axis}
              VK_PRIOR: aKey := VK_NEXT;
              VK_NEXT: aKey := VK_PRIOR;
              VK_END: aKey := VK_HOME;
              VK_HOME: aKey := VK_END;
              VK_UP: aKey := VK_DOWN;
              VK_DOWN: aKey := VK_UP;
            end; {case}
          if not (ssShift in Shift) and
             (((ssModifier in Shift) and (esoHomeEndCtrl in Options)) or
            ((ssAlt in Shift) and (esoHomeEndAlt in Options)) or ReadOnly) then     
            case aKey of
              VK_END:
                begin
                  if esoArrowKeysExceed in Options 
                    then Value := FSpinBtns.MinInEdit
                    else Value := FSpinBtns.Min; 
                  bKeyUsed := True;
                end;
              VK_HOME: 
                begin
                  if esoArrowKeysExceed in Options 
                    then Value := FSpinBtns.MaxInEdit
                    else Value := FSpinBtns.Max;
                  bKeyUsed := True;
                end;
            end; {case}                  
          if not bKeyUsed then   
            if ((esoUpDownOnly in Options) and (([ssShift, ssAlt, ssModifier]*Shift) = [])) or
              ((ssModifier in Shift) and (esoUpDownCtrl in Options)) or
              ((ssAlt in Shift) and (esoUpDownAlt in Options)) or
              ((ssShift in Shift) and (esoUpDownShift in Options)) then                                          
              case aKey of
                VK_PRIOR: 
                  begin
                    ResetActualValue;
                    with FSpinBtns do
                      SetValue(GetBigIncreasedValue, True, esoArrowKeysExceed in FOptions);
                    bKeyUsed := True;
                  end;
                VK_NEXT: 
                  begin
                    ResetActualValue;
                    with FSpinBtns do
                      SetValue(GetBigDecreasedValue, True, esoArrowKeysExceed in Options);
                    bKeyUsed := True;
                  end;            
                VK_UP: 
                  begin
                    ResetActualValue;
                    with FSpinBtns do
                      SetValue(GetIncreasedValue, True, esoArrowKeysExceed in Options);
                    bKeyUsed := True;
                  end;
                VK_DOWN: 
                  begin
                    ResetActualValue;
                    with FSpinBtns do
                      SetValue(GetDecreasedValue, True, esoArrowKeysExceed in Options);
                    bKeyUsed := True;
                  end;
              end; {case}
        end;  
      if not bKeyUsed then TextEdited := True;
    end;
  inherited KeyDown(Key, Shift);
end;                          

procedure TECSpinEdit.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if ssModifier in GetKeyShiftState then exit;  
  if (Key <> Char(VK_BACK)) and (ord(Key) <> 127) then {127 is Delete} 
    case FValueFormat of
      evfRound, evfExponent, evfMantissa:
        if not ((Key in ['0'..'9', '-']) or ((FDigits > 0)
          and (Key = DefaultFormatSettings.DecimalSeparator))) then Key := #0;
      evfExponential:
        if not (Key in ['0'..'9', '-', 'e', 'E', DefaultFormatSettings.DecimalSeparator])
          then Key := #0;
      evfHexadecimal, evfMarkHexadec:
        if not (Key in ['$', '0'..'9', 'a'..'f', 'A'..'F']) then Key := #0;
      evfOctal, evfMarkOctal: if not (Key in ['&', '0'..'7']) then Key := #0;
      evfBinary: if (Key <> '0') and (Key <> '1') then Key := #0;
      evfDate: if not (Key in ['0'..'9', DefaultFormatSettings.DateSeparator]) then Key := #0;
      evfTime: if not (Key in ['0'..'9', DefaultFormatSettings.TimeSeparator, '.']) then Key := #0;
    end;      
end;                          

procedure TECSpinEdit.RewriteText;
begin
  {$IFDEF DBGSPINS} DebugLn('TECSpinEdit.RewriteText'); {$ENDIF}
  Text := GetText(FSpinBtns.FValue, FDigits); 
  TextEdited := False;
end;

procedure TECSpinEdit.SetEnabled(Value: Boolean);
begin
  inherited SetEnabled(Value);
  FSpinBtns.Enabled := Value;
end;      

procedure TECSpinEdit.SetParent(NewParent: TWinControl);
begin
  FSpinBtns.Anchors := [];
  inherited SetParent(NewParent);
  FSpinBtns.Parent := Parent;
  FSpinBtns.Anchors := [akTop, akBottom];
end;

procedure TECSpinEdit.SetRealBoundRect(ARect: TRect);
begin
  if BiDiMode = bdLeftToRight 
    then dec(ARect.Right, Indent + FSpinBtns.Width)
    else inc(ARect.Left, Indent + FSpinBtns.Width);
  BoundsRect := ARect;
end; 
          
procedure TECSpinEdit.SetRealBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if BiDiMode <> bdLeftToRight then ALeft := ALeft + Indent + FSpinBtns.Width;
  SetBounds(ALeft, ATop, AWidth - Indent - FSpinBtns.Width, AHeight);
end;    

procedure TECSpinEdit.SetSpinBtnsPosition;
begin
  if not IsRightToLeft
    then FSpinBtns.Left := Left + Width + Indent
    else FSpinBtns.Left := Left - Indent - FSpinBtns.FWidth;
end;
         
procedure TECSpinEdit.SwitchOption(AOption: TSEOption; AOn: Boolean);
var aOptions: TSEOptions;
begin
  aOptions := FOptions;
  if AOn 
    then Include(aOptions, AOption)
    else Exclude(aOptions, AOption);
  Options := aOptions;
end;

function TECSpinEdit.TryGetValueFromString(AString: string; out AValue: Double): Boolean;
var aFS: TFormatSettings;
    d: Double;
    i: Int64;
    j: Integer;
begin
  case FValueFormat of
    evfRound, evfExponential: 
      begin
        Result := TryStrToFloat(AString, d);
        if Result then AValue := d;
      end;
    evfExponent: 
      begin
        Result := TryStrToFloat(AString, d);
        if Result then AValue := logn(FMantissaExp, d);
      end;
    evfMantissa: 
      begin
        Result := TryStrToFloat(AString, d);
        if Result then AValue := power(d, 1/FMantissaExp);
      end;
    evfHexadecimal, evfMarkHexadec: 
      begin
        if (AString <> '') and (AString[1] <> '$') then AString := '$' + AString;
        Result := TryStrToInt64(AString, i);
        if Result then AValue := i;
      end;
    evfOctal, evfMarkOctal:
      begin
        if (AString <> '') and (AString[1] <> '&') then AString := '&' + AString;
        Result := TryStrToInt64(AString, i);
        if Result then AValue := i;
      end;
    evfBinary: 
      begin
        if (AString<>'') and (AString[1] <> '%') then AString := '%' + AString;
        Result := TryStrToInt64(AString, i);
        if Result then AValue := i;
      end;
    evfDate: 
      begin
        aFS := DefaultFormatSettings;
        aFS.LongDateFormat := FDateTimeFormat;
        Result := TryStrToDate(AString, d, aFS);
        if Result then AValue := d;
      end;
    evfTime: 
      begin
        aFS := DefaultFormatSettings;
        aFS.LongTimeFormat := FDateTimeFormat;
        Result := TryStrToDateTime(AString, d, aFS);
        if Result then AValue := d;
      end;
    evfText: 
      begin
        AValue := -1;
        if assigned(FItems) then AValue := FItems.IndexOf(trim(AString));
        Result := (AValue > -1);
      end;
    evfCombined: 
      begin
        AString := trim(AString);
        for j := 1 to length(AString) do
          if not (AString[j] in ['0'..'9', DefaultFormatSettings.DecimalSeparator, '-', 'e', 'E']) then
            begin
              AString := LeftStr(AString, j-1);
              break;
            end;
        Result := TryStrToFloat(AString, d);
        if Result then AValue := d;
      end;
  end;  {case}
end;

procedure TECSpinEdit.VisibleChanged;
begin
  inherited VisibleChanged;
  FSpinBtns.Visible := Visible;
  if assigned(OnVisibleChanged) then OnVisibleChanged(self, Visible);
end;                  

{ TECSpinEdit.Getters + Setters }

function TECSpinEdit.GetController: TECSpinController;
begin
  Result := Buttons.FController;
end;

function TECSpinEdit.GetValue: Double;
begin
  Result := FSpinBtns.Value;
end;

function TECSpinEdit.GetWidthInclBtns: Integer;
begin
  Result := Width + Indent + FSpinBtns.Width;
end;

procedure TECSpinEdit.SetController(AValue: TECSpinController);
begin
  {$IFDEF DBGSPINS} DebugLn('TECSpinEdit.SetController'); {$ENDIF}
  if Buttons.FController = AValue then exit;
  if assigned(Buttons.FController) then Buttons.FController.UnRegisterClient(self);
  Buttons.FController := AValue;
  if assigned(AValue) then AValue.RegisterClient(self);
end;

procedure TECSpinEdit.SetDateTimeFormat(const AValue: string);
begin
  if FDateTimeFormat = AValue then exit;
  FDateTimeFormat := AValue;
  if FValueFormat in [evfDate, evfTime] then RewriteText;        
end;

procedure TECSpinEdit.SetDigits(AValue: Word);
begin
  if FDigits = AValue then exit;
  FDigits := AValue;
  RewriteText;
end;      

procedure TECSpinEdit.SetIndent(AValue: SmallInt);
begin
  if FIndent = AValue then exit;
  FIndent := AValue;
  SetSpinBtnsPosition;
end;

procedure TECSpinEdit.SetItems(AValue: TStrings);
begin
  if AValue <> FItems then FItems.Assign(AValue); 
end; 

procedure TECSpinEdit.SetMantissaExp(AValue: Double);
begin
  if FMantissaExp = AValue then exit;
  FMantissaExp := AValue;
  if FValueFormat in [evfExponent, evfMantissa] then RewriteText;
end;

procedure TECSpinEdit.SetValue(AValue: Double);
begin
  FSpinBtns.SetValue(AValue, True, True); 
end;

procedure TECSpinEdit.SetValueFormat(AValue: TValueFormat);
begin
  if FValueFormat = AValue then exit;
  FValueFormat := AValue;
  RewriteText; 
end;

procedure TECSpinEdit.SetWidthInclBtns(AValue: Integer);
begin
  Width := AValue - Indent - FSpinBtns.Width;
end;

end.
    
                                                 
