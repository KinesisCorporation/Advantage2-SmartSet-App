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

unit ECEditBtns;
{$mode objfpc}{$H+}  

//{$DEFINE DBGCTRLS}  {don't remove, just comment}

interface

uses
  Classes, SysUtils, Controls, Graphics, Math, Themes, Types, StdCtrls, ActnList,
  Dialogs, Forms, ImgList, LCLIntf, LCLProc, LCLType, LMessages, ECSpinCtrls, ECTypes;

type       
  {$PACKENUM 2}
  TButtonMode = (ebmSpeedBtn, ebmToggleBox, ebmDelayBtn);
  TDropDownGlyph = (edgNone, edgMiddle, edgDown);
  TEBOption = (eboClickAltEnter, eboClickCtrlEnter, eboClickShiftEnter);
  TEBOptions = set of TEBOption;   
  TItemOrder = (eioFixed, eioHistory, eioSorted);
  { Event }
  TOnDrawGlyph = procedure(Sender: TObject; AState: TItemState) of object;
  
const
  cDefEBOptions = [eboClickAltEnter, eboClickCtrlEnter, eboClickShiftEnter];

type 
  TCustomECSpeedBtn = class;

  { TECSpeedBtnActionLink }
  TECSpeedBtnActionLink = class(TWinControlActionLink)
  protected
    FClientSpeedBtn: TCustomECSpeedBtn;
    procedure AssignClient(AClient: TObject); override;
    procedure SetChecked(Value: Boolean); override;
  public
    function IsCheckedLinked: Boolean; override;
  end;

  TECSpeedBtnActionLinkClass = class of TECSpeedBtnActionLink;    
  
  { TCustomECSpeedBtn }
  TCustomECSpeedBtn = class(TGraphicControl)
  private
    FAllowAllUp: Boolean;
    FChecked: Boolean;
    FCheckedFontColor: TColor;
    FCheckedFontStyles: TFontStyles;
    FCheckFromAction: Boolean;
    FDelay: Integer;
    FDropDownGlyph: TDropDownGlyph;
    FFlat: Boolean;
    FGlyphColor: TColor;
    FGlyphDesignChecked: TGlyphDesign;
    FGlyphDesign: TGlyphDesign;
    FGroupIndex: Integer;
    FImageIndex: TImageIndex;
    FImageIndexChecked: TImageIndex;
    FImages: TCustomImageList;
    FLayout: TObjectPos;
    FMargin: SmallInt;
    FMode: TButtonMode;
    FRepeating: Integer;
    FShowCaption: Boolean;
    FSpacing: SmallInt;
    FOnChange: TNotifyEvent;
    FOnDrawGlyph: TOnDrawGlyph;
    FOnHoldDown: TNotifyEvent;
    FOnRelease: TNotifyEvent;
    FOnRepeating: TNotifyEvent;
    FTransparent: Boolean;
    procedure SetAllowAllUp(AValue: Boolean);
    procedure SetChecked(AValue: Boolean);
    procedure SetCheckedFontColor(AValue: TColor);
    procedure SetCheckedFontStyles(AValue: TFontStyles);
    procedure SetDelay(AValue: Integer);
    procedure SetDropDownGlyph(AValue: TDropDownGlyph);
    procedure SetFlat(AValue: Boolean);
    procedure SetGlyphColor(AValue: TColor);
    procedure SetGlyphDesign(AValue: TGlyphDesign);
    procedure SetGlyphDesignChecked(AValue: TGlyphDesign);
    procedure SetGroupIndex(AValue: Integer);
    procedure SetImageIndex(AValue: TImageIndex);
    procedure SetImageIndexChecked(AValue: TImageIndex);
    procedure SetImages(AValue: TCustomImageList);
    procedure SetLayout(AValue: TObjectPos);
    procedure SetMargin(AValue: SmallInt);
    procedure SetMode(AValue: TButtonMode);
    procedure SetRepeating(AValue: Integer);
    procedure SetShowCaption(AValue: Boolean);
    procedure SetSpacing(AValue: SmallInt);
    procedure SetTransparent(AValue: Boolean);
  protected const
    cDefCheckedFontStyles = [];
    cDefHeight = 23;
    cDefSpacing = 6;
    cDefWidth = 21;
    cDropDownGlyphIndent: SmallInt = 4;
    cMargin: SmallInt = 6;  
  protected
    BtnBitmapPushedDisabled: TBitmap;
    BtnDrawnPushed: Boolean;
    BtnPushed: Boolean;
    NeedRedraw: Boolean;
    PrevSize: TSize;
    RealLayout: TObjectPos;
    ECTimer: TCustomECTimer;
    ValidStates: TItemStates;
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: Integer; 
                                     {%H-}WithThemeSpace: Boolean); override;
    procedure Click; override;
    procedure CMBiDiModeChanged(var {%H-}Message: TLMessage); message CM_BIDIMODECHANGED;
    procedure CMButtonPressed(var Message: TLMessage); message CM_BUTTONPRESSED;
    procedure CMColorChanged(var {%H-}Message: TLMessage); message CM_COLORCHANGED;
    procedure CMParentColorChanged(var Message: TLMessage); message CM_PARENTCOLORCHANGED;
    procedure CreateTimer;
    procedure CreateValidBitmaps(AEnabled: Boolean);
    function DialogChar(var Message: TLMKey): Boolean; override;
    procedure DrawButtonBMPs;
    procedure FontChanged(Sender: TObject); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    function HasValidImages: Boolean;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint;  override;
    procedure Redraw;
    procedure Resize; override;
    procedure ResizeInvalidate;
    procedure SetAction(Value: TBasicAction); override;
    procedure SetAutoSize(Value: Boolean); override;
    procedure SetParent(NewParent: TWinControl); override;
    procedure SetTimerEvent;
    procedure TextChanged; override;
    procedure TimerOnTimerDelay(Sender: TObject);
    procedure TimerOnTimerHold(Sender: TObject);
    procedure TimerOnTimerRepeating(Sender: TObject);
    procedure UpdateGroup;
    property CheckFromAction: Boolean read FCheckFromAction write FCheckFromAction;
  public
    BtnBitmaps: array [low(TItemState)..high(TItemState)] of TBitmap;
    UpdateCount: SmallInt;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    property AllowAllUp: Boolean read FAllowAllUp write SetAllowAllUp default False;
    property Checked: Boolean read FChecked write SetChecked default False;
    property CheckedFontColor: TColor read FCheckedFontColor write SetCheckedFontColor default clDefault;
    property CheckedFontStyles: TFontStyles read FCheckedFontStyles write SetCheckedFontStyles default cDefCheckedFontStyles;
    property Delay: Integer read FDelay write SetDelay default 0;
    property DropDownGlyph: TDropDownGlyph read FDropDownGlyph write SetDropDownGlyph default edgNone;
    property Flat: Boolean read FFlat write SetFlat default False;
    property GlyphColor: TColor read FGlyphColor write SetGlyphColor default clDefault;
    property GlyphDesign: TGlyphDesign read FGlyphDesign write SetGlyphDesign;  { set default in descendants }
    property GlyphDesignChecked: TGlyphDesign read FGlyphDesignChecked write SetGlyphDesignChecked default egdNone;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property ImageIndexChecked: TImageIndex read FImageIndexChecked write SetImageIndexChecked default -1;
    property Images: TCustomImageList read FImages write SetImages;
    property Layout: TObjectPos read FLayout write SetLayout default eopLeft;
    property Margin: SmallInt read FMargin write SetMargin default -1;
    property Mode: TButtonMode read FMode write SetMode default ebmSpeedBtn;
    property Repeating: Integer read FRepeating write SetRepeating default 0;
    property ShowCaption: Boolean read FShowCaption write SetShowCaption default True;
    property Spacing: SmallInt read FSpacing write SetSpacing default cDefSpacing;
    property Transparent: Boolean read FTransparent write SetTransparent;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDrawGlyph: TOnDrawGlyph read FOnDrawGlyph write FOnDrawGlyph;
    property OnHoldDown: TNotifyEvent read FOnHoldDown write FOnHoldDown;
    property OnRelease: TNotifyEvent read FOnRelease write FOnRelease;
    property OnRepeating: TNotifyEvent read FOnRepeating write FOnRepeating;
  end;

  { TECSpeedBtn }
  TECSpeedBtn = class(TCustomECSpeedBtn)
  published
    property Action;
    property Align;
    property AllowAllUp;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property BorderSpacing;
    property Caption;
    property Checked;
    property CheckedFontColor;
    property CheckedFontStyles;
    {property Color;}  {does nothing ATM}
    property Constraints;
    property Delay;
    property DropDownGlyph;
    property Enabled;
    property Flat;
    property Font;
    property GlyphColor;
    property GlyphDesign default egdNone;
    property GlyphDesignChecked;
    property GroupIndex;
    property ImageIndex;
    property ImageIndexChecked;
    property Images;
    property Layout;
    property Margin;
    property Mode;
    property PopupMenu;
    property ParentBiDiMode;
    {property ParentColor;}  {does nothing ATM}
    property ParentFont;
    property ParentShowHint;
    property Repeating;
    property ShowCaption;
    property ShowHint;
    property Spacing;
    property Visible;
    property OnChangeBounds;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawGlyph;
    property OnHoldDown;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnRelease;
    property OnRepeating;
    property OnResize;    
  end;

  { TCustomECSpeedBtnPlus }    
  TCustomECSpeedBtnPlus = class(TCustomECSpeedBtn)
  protected
    CustomClick: TObjectMethod;
    CustomMouseDown: TMouseMethod;
    CustomMouseUp: TMouseMethod;
    CustomResize: TObjectMethod;
    procedure Click; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override; 
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Resize; override;
  public 
    constructor Create(AOwner: TComponent); override;
  published
    property AnchorSideLeft stored False;
    property AnchorSideTop stored False;
    property AnchorSideRight stored False;
    property AnchorSideBottom stored False;     
    property Height stored False;
    property Left stored False;
    property Top stored False;
  end;
  
  { TECSpeedBtnPlus }
  TECSpeedBtnPlus = class(TCustomECSpeedBtnPlus)
  published
    property Caption;
    property Flat;
    property GlyphColor;
    property GlyphDesign;
    property ImageIndex;
    property Images;
    property Layout;
    property Margin;
    property PopupMenu;
    property ParentShowHint;
    property ShowHint;
    property Spacing;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;  
  end;
  
  { TECEditBtnSpacing }
  TECEditBtnSpacing = class(TControlBorderSpacing)
  public
    function GetSpace(Kind: TAnchorKind): Integer; override;
    procedure GetSpaceAround(var SpaceAround: TRect); override;
  end;    
  
  { TBaseECEditBtn }
  TBaseECEditBtn = class(TCustomEdit)
  private
    FIndent: SmallInt;
    FOnVisibleChanged: TOnVisibleChanged;
    FOptions: TEBOptions;
    function GetWidthInclBtn: Integer;
    procedure SetIndent(AValue: SmallInt);
    procedure SetWidthInclBtn(AValue: Integer);
  protected
    FAnyButton: TCustomECSpeedBtnPlus;
    function ChildClassAllowed(ChildClass: TClass): boolean; override;
    procedure CMBiDiModeChanged(var Message: TLMessage); message CM_BIDIMODECHANGED;
    function CreateControlBorderSpacing: TControlBorderSpacing; override;
    procedure DoOnChangeBounds; override;
    procedure InitializeWnd; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure SetButtonPosition;
    procedure SetEnabled(Value: Boolean); override;
    procedure SetParent(NewParent: TWinControl); override;
    procedure SetVisible(Value: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetRealBoundRect(ARect: TRect);
    procedure SetRealBounds(ALeft, ATop, AWidth, AHeight: Integer);
    procedure SwitchOption(AOption: TEBOption; AOn: Boolean);
    property Indent: SmallInt read FIndent write SetIndent default 0;
    property Options: TEBOptions read FOptions write FOptions default cDefEBOptions;
    property WidthInclBtn: Integer read GetWidthInclBtn write SetWidthInclBtn stored False;
    property OnVisibleChanged: TOnVisibleChanged read FOnVisibleChanged write FOnVisibleChanged;
  end;

  { TECEditBtn }
  TECEditBtn = class(TBaseECEditBtn)
  private
    FButton: TECSpeedBtnPlus;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Button: TECSpeedBtnPlus read FButton write FButton;
    property Indent;
    property Options;
    property WidthInclBtn;
  published  
    property Align;
    property Alignment;
    property Anchors;
    property AutoSelect;
    property AutoSize;       
    property BiDiMode;
    property BorderSpacing;
    property BorderStyle;
    property CharCase;
    property Color; 
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property EchoMode;
    property Enabled;
    property Font;
    property HideSelection;  
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;      
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
    property OnVisibleChanged;
  end;

const 
  cDefColorLayout = eclRGBColor;
  cDefCustomColor = clBlack;
    
type     
  { TECSpeedBtnColor }
  TECSpeedBtnColor = class(TCustomECSpeedBtnPlus)
  protected const
    cDefGlyphDesign = egdWinRectClr;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Caption;
    property GlyphDesign default cDefGlyphDesign;
    property ImageIndex;
    property Images;
    property Layout;
    property Margin;
    property PopupMenu;
    property ParentShowHint;
    property ShowHint;
    property Spacing;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;               
  end;
  
  { TECColorBtn }
  TECColorBtn = class(TBaseECEditBtn)    
  private
    FButton: TECSpeedBtnColor;
    FColorLayout: TColorLayout;
    FCustomColor: TColor;
    FOnCustomColorChanged: TNotifyEvent;
    FPrefix: string;
    procedure SetColorLayout(AValue: TColorLayout);
    procedure SetCustomColor(AValue: TColor);
    procedure SetPrefix(const AValue: string);
  protected
    procedure DoButtonClick;
    procedure Redraw;
  public
    constructor Create(AOwner: TComponent); override;
    procedure EditingDone; override;
  published
    property Button: TECSpeedBtnColor read FButton write FButton;
    property ColorLayout: TColorLayout read FColorLayout write SetColorLayout default cDefColorLayout;
    property CustomColor: TColor read FCustomColor write SetCustomColor default cDefCustomColor;
    property Indent;
    property Options;
    property Prefix: string read FPrefix write SetPrefix;
    property WidthInclBtn;
    property OnCustomColorChanged: TNotifyEvent read FOnCustomColorChanged write FOnCustomColorChanged;
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSelect;
    property AutoSize;       
    property BiDiMode;
    property BorderSpacing;
    property BorderStyle;  
    property CharCase;
    property Color;   
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly default True;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
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
    property OnVisibleChanged;
  end;

  { TECComboBtnSpacing }
  TECComboBtnSpacing = class(TControlBorderSpacing)
  public
    function GetSpace(Kind: TAnchorKind): Integer; override;
    procedure GetSpaceAround(var SpaceAround: TRect); override;
  end;       
  
  { TBaseECComboBtn }
  TBaseECComboBtn = class(TCustomComboBox)
  private
    FIndent: SmallInt;
    FItemOrder: TItemOrder;
    FMaxCount: Integer;
    FOnVisibleChanged: TOnVisibleChanged;
    FOptions: TEBOptions;
    function GetWidthInclBtn: Integer;
    procedure SetIndent(AValue: SmallInt);
    procedure SetItemOrder(AValue: TItemOrder);
    procedure SetMaxCount(AValue: Integer);
    procedure SetOptions(AValue: TEBOptions);
    procedure SetWidthInclBtn(AValue: Integer);
  protected 
    FAnyButton: TCustomECSpeedBtnPlus;
    function ChildClassAllowed(ChildClass: TClass): boolean; override;
    procedure CMBiDiModeChanged(var Message: TLMessage); message CM_BIDIMODECHANGED;
    function CreateControlBorderSpacing: TControlBorderSpacing; override;
    procedure DoOnChangeBounds; override;
    procedure InitializeWnd; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure SetButtonPosition;
    procedure SetEnabled(Value: Boolean); override;
    procedure SetParent(NewParent: TWinControl); override;
    procedure SetSorted(Val: boolean); override;
    procedure SetVisible(Value: Boolean); override;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Add(const AItem: string);
    procedure AddItemHistory(const AItem: string; ACaseSensitive: Boolean);
    procedure AddItemLimit(const AItem: string; ACaseSensitive: Boolean);
    procedure SetRealBoundRect(ARect: TRect);
    procedure SetRealBounds(ALeft, ATop, AWidth, AHeight: Integer);     
    property Indent: SmallInt read FIndent write SetIndent default 0;
    property ItemOrder: TItemOrder read FItemOrder write SetItemOrder; 
    property MaxCount: Integer read FMaxCount write SetMaxCount default 0;
    property Options: TEBOptions read FOptions write SetOptions default cDefEBOptions;
    property WidthInclBtn: Integer read GetWidthInclBtn write SetWidthInclBtn stored False;
    property OnVisibleChanged: TOnVisibleChanged read FOnVisibleChanged write FOnVisibleChanged;
  end;
  
  { TECComboBtn }      
  TECComboBtn = class(TBaseECComboBtn)
  private
    FButton: TECSpeedBtnPlus;
  protected const
    cDefItemOrder = eioFixed;
  public
    constructor Create(TheOwner: TComponent); override;
  published
    property Button: TECSpeedBtnPlus read FButton write FButton;
    property Indent;
    property ItemOrder default eioFixed;
    property Options;
    property WidthInclBtn;
  published
    property Align;
    property Anchors;
    property ArrowKeysTraverseList;
    property AutoComplete;
    property AutoCompleteText;
    property AutoDropDown;
    property AutoSelect;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property ItemHeight;
    property ItemIndex;
    property Items;
    property ItemWidth;
    property MaxCount;
    property MaxLength;  
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property Style;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;  
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEndDrag;
    property OnDropDown;
    property OnEditingDone;
    property OnEnter;
    property OnExit;
    property OnGetItems;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnSelect;
    property OnUTF8KeyPress;
    property OnVisibleChanged;
  end;
    
  { TECColorCombo }
  TECColorCombo = class(TBaseECComboBtn)
  private
    FButton: TECSpeedBtnColor;
    FColorLayout: TColorLayout;
    FCustomColor: TColor;
    FPrefix: string;
    FOnCustomColorChanged: TNotifyEvent;
    procedure SetColorLayout(AValue: TColorLayout);
    procedure SetCustomColor(AValue: TColor);
    procedure SetPrefix(const AValue: string);
  protected const
    cDefColorOrder = eioHistory;    
  protected
    FNeedMeasure: Boolean;
    FSelectedFromList: Boolean;
    FTextExtent: TSize;
    FUpdatingCustomColor: Boolean;
    procedure DoButtonClick;
    procedure DrawItem(Index: Integer; ARect: TRect; State: TOwnerDrawState); override;
    procedure Select; override;  { only when ItemIndex changes by mouse }
    procedure SetItemIndex(const Val: Integer); override;  { only when ItemIndex changes by code }
    procedure SetItemHeight(const AValue: Integer); override;
    procedure SetItems(const Value: TStrings); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AddColor(const AColor: string); overload;
    procedure AddColor(AColor: TColor); overload;
    procedure EditingDone; override;
    procedure SetColorText(const AColor: string);
    procedure Validate;
  published           				 
    property Button: TECSpeedBtnColor read FButton write FButton;
    property ColorLayout: TColorLayout read FColorLayout write SetColorLayout default cDefColorLayout;
    property CustomColor: TColor read FCustomColor write SetCustomColor default cDefCustomColor;
    property Indent;
    property ItemOrder default cDefColorOrder;
    property MaxCount;
    property Options;
    property Prefix: string read FPrefix write SetPrefix;
    property WidthInclBtn;
    property OnCustomColorChanged: TNotifyEvent read FOnCustomColorChanged write FOnCustomColorChanged;
  published
    property Align;
    property Anchors;
    property ArrowKeysTraverseList;
    property AutoDropDown;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property CharCase;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property ItemHeight;
    property ItemIndex;
    property Items;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;  
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnDropDown;
    property OnEditingDone;
    property OnEnter;
    property OnExit;
    property OnGetItems;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnSelect;
    property OnUTF8KeyPress;
    property OnVisibleChanged;
  end;
  
implementation

{ TECSpeedBtnActionLink }

procedure TECSpeedBtnActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);
  FClientSpeedBtn := AClient as TCustomECSpeedBtn;
end;

function TECSpeedBtnActionLink.IsCheckedLinked: Boolean;
begin
  Result := inherited IsCheckedLinked and
            (FClientSpeedBtn.Checked = (Action as TCustomAction).Checked);   
end;

procedure TECSpeedBtnActionLink.SetChecked(Value: Boolean);
begin
  if IsCheckedLinked then
    begin
      FClientSpeedBtn.CheckFromAction := True;
      try
        FClientSpeedBtn.Checked := Value;
      finally
        FClientSpeedBtn.CheckFromAction := False;
      end;
    end;    
end;

{ TCustomECSpeedBtn }

constructor TCustomECSpeedBtn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csNoFocus, csParentBackground, csReplicatable] 
                               - csMultiClicks - [csCaptureMouse, csOpaque, csSetCaption];
  FCheckedFontColor := clDefault;
  FGlyphColor := clDefault;
  FImageIndex := -1;
  FImageIndexChecked:=-1;
  FLayout := eopLeft;
  RealLayout := eopLeft;
  FMargin := -1;
  FShowCaption := True;
  FSpacing := cDefSpacing;
  FTransparent := True;
  PrevSize.cx := cDefWidth;
  PrevSize.cy := cDefHeight;
  SetInitialBounds(0, 0, cDefWidth, cDefHeight);
  AccessibleRole := larButton;
end;

destructor TCustomECSpeedBtn.Destroy;
var aState: TItemState;
begin
  for aState := low(TItemState) to high(TItemState) do
    FreeAndNil(BtnBitmaps[aState]);
  inherited Destroy;
end;

procedure TCustomECSpeedBtn.BeginUpdate;
begin
  inc(UpdateCount);
end;

procedure TCustomECSpeedBtn.CalculatePreferredSize(var PreferredWidth, 
            PreferredHeight: Integer; WithThemeSpace: Boolean);
var aCaption: string;
    aCaptionSize, aGlyphSize: TSize;
    aMargin: SmallInt;
begin
  aCaption := Caption;
  if aCaption <> '' then
    begin
      DeleteAmpersands(aCaption);
      aCaptionSize := Canvas.TextExtent(aCaption);
    end else
    aCaptionSize := Size(0, 0);
  if (ImageIndex >= 0) and assigned(Images) and (ImageIndex < Images.Count)
    then aGlyphSize := Size(Images.Width, Images.Height)
    else aGlyphSize := Canvas.GlyphExtent(GlyphDesign);     
  if RealLayout in [eopRight, eopLeft] then
    begin
      if aGlyphSize.cx*aCaptionSize.cx > 0 then inc(aGlyphSize.cx, Spacing);
      inc(aGlyphSize.cx, aCaptionSize.cx);
      if aCaptionSize.cx > 0 then inc(aGlyphSize.cx, 2*Spacing);
      aGlyphSize.cy := Math.max(aGlyphSize.cy, aCaptionSize.cy);
    end else
    begin
      aGlyphSize.cx := Math.max(aGlyphSize.cx, aCaptionSize.cx);
      if aGlyphSize.cy*aCaptionSize.cy > 0 then inc(aGlyphSize.cy, Spacing);
      inc(aGlyphSize.cy, aCaptionSize.cy);
    end;
  aMargin := Margin;
  if aMargin < 0 then aMargin := cMargin;
  inc(aGlyphSize.cx, 2*aMargin);
  inc(aGlyphSize.cy, 2*aMargin);
  aGlyphSize.cx := aGlyphSize.cx or 1;  { Odd size glyphs look better }
  aGlyphSize.cy := aGlyphSize.cy or 1;
  PreferredWidth := aGlyphSize.cx;
  PreferredHeight := aGlyphSize.cy;
end;

procedure TCustomECSpeedBtn.Click;
begin
  case Mode of
    ebmToggleBox: if not assigned(Action) then Checked := not Checked;
    ebmDelayBtn: if Delay > 0 then
                   begin
                     if not Checked then Checked := True
                       else
                       begin
                         ECTimer.Enabled := False;
                         ECTimer.Enabled := True;
                       end;
                   end;
  end;
  inherited Click;
end;   

procedure TCustomECSpeedBtn.CMBiDiModeChanged(var Message: TLMessage);
var aRealLayout: TObjectPos;
begin
  aRealLayout := Layout;
  if IsRightToLeft then
    case aRealLayout of
      eopRight: aRealLayout := eopLeft;
      eopLeft: aRealLayout := eopRight;
    end;
  RealLayout := aRealLayout;
  Redraw;
end;   

procedure TCustomECSpeedBtn.CMButtonPressed(var Message: TLMessage);
var aSender: TCustomECSpeedBtn;
begin
  if csDestroying in ComponentState then exit;
  if Message.WParam = WParam(FGroupIndex) then
    begin
      aSender := TCustomECSpeedBtn(Message.LParam);
      if aSender <> self then
        begin
          if aSender.Checked and FChecked then
            begin
              FChecked := False;
              Invalidate;
            end;
          FAllowAllUp := aSender.AllowAllUp;
        end;
    end;         
end;   

procedure TCustomECSpeedBtn.CMColorChanged(var Message: TLMessage);
begin
  NeedRedraw := True;
end;   

procedure TCustomECSpeedBtn.CMParentColorChanged(var Message: TLMessage);
begin
  inherited CMParentColorChanged(Message);
  if not ParentColor then NeedRedraw := True;
end;

procedure TCustomECSpeedBtn.CreateTimer;
begin
  ECTimer := TCustomECTimer.Create(self);
  ECTimer.Enabled := False;
end;

procedure TCustomECSpeedBtn.CreateValidBitmaps(AEnabled: Boolean);
var aIState: TItemState;
    aWidth, aHeight: Integer;
begin
  for aIState in ValidStates do  
    FreeAndNil(BtnBitmaps[aIState]);
  if AEnabled 
    then ValidStates := caEnabledStates 
    else ValidStates := caDisabledStates;
  aWidth := Width;
  aHeight := Height;    
  for aIState in ValidStates do 
    begin
      BtnBitmaps[aIState] := TBitmap.Create;
      BtnBitmaps[aIState].SetProperties(aWidth, aHeight, Transparent);
    end;          
end;                 
  
function TCustomECSpeedBtn.DialogChar(var Message: TLMKey): Boolean;
begin
  Result := False;
  if Message.Msg = LM_SYSCHAR then
    begin
      if IsEnabled and IsVisible then
        begin
          if IsAccel(Message.CharCode, Caption) then
            begin
              Click;
              Result := True;      
            end else
            Result := inherited DialogChar(Message);     
        end;
    end;
end;   

procedure TCustomECSpeedBtn.DrawButtonBMPs;
var aImageIndex, aImgIdxChckd, aLength: Integer;
    aCaption: string;
    aFlags: Cardinal;
    aGlyphDesign, aGlyphDsgnChckd: TGlyphDesign;
    aGlyphSize, aTextSize: TSize;
    aGlyphPoint: TPoint;
    aRect: TRect;
    aState: TItemState;
    aValidStates: TItemStates;
    
  procedure AdjustRect(AWidthLimit, AHeightLimit: SmallInt);
  var i, j, aLimit: SmallInt;
  begin
    i := -4;
    aLimit := aRect.Right - aRect.Left;
    if aLimit <= 21 then inc(i);      
    aLimit := aLimit - AWidthLimit;
    if aLimit > 0 then dec(i, aLimit div 5);
    j := -4;                                   
    aLimit := aRect.Bottom - aRect.Top - AHeightLimit;
    if aLimit > 0 then dec(j, aLimit div 5);			
    InflateRect(aRect, i, j);
  end;            
    
begin
  {$IFDEF DBGCTRLS} DebugLn('TCustomECSpeedBtn.DrawButton'); {$ENDIF}
  if assigned(Parent) then
    begin
      aRect := ClientRect;
      aValidStates := ValidStates;
      for aState in aValidStates do
        begin
          BtnBitmaps[aState].TransparentColor := GetColorResolvingDefault(Color, Parent.Brush.Color);
          BtnBitmaps[aState].TransparentClear;
        end;
      if (Mode = ebmSpeedBtn) and Flat then aValidStates := aValidStates - [eisDisabled, eisEnabled];
      for aState in aValidStates do
        BtnBitmaps[aState].Canvas.DrawButtonBackground(aRect, aState);
      aValidStates := ValidStates;
      if assigned(OnDrawGlyph) then 
        for aState in aValidStates do
          OnDrawGlyph(self, aState)
        else
        begin
          aCaption := Caption;
          if (aCaption <> '') and ShowCaption then
            begin
              DeleteAmpersands(aCaption);
              aFlags := DT_CENTER or DT_VCENTER;
              aState := eisEnabled;
              with ThemeServices do
                aRect := GetTextExtent(Canvas.Handle, GetElementDetails(caThemedContent[aState]),
                  aCaption, aFlags, nil);
              aTextSize.cx := aRect.Right - aRect.Left + 1;
              aTextSize.cy := aRect.Bottom - aRect.Top + 1;
            end else
            aTextSize := Size(0, 0);
          if DropDownGlyph > edgNone then
            begin
              aGlyphSize := Canvas.GlyphExtent(egdArrowDown);
              if not IsRightToLeft
                then aGlyphPoint.X := Width - aGlyphSize.cx - cDropDownGlyphIndent
                else aGlyphPoint.X := cDropDownGlyphIndent;
              if DropDownGlyph = edgMiddle
                then aGlyphPoint.Y := (Height - aGlyphSize.cy + 2) div 2
                else aGlyphPoint.Y := Height - aGlyphSize.cy - cDropDownGlyphIndent;
              aRect := Rect(aGlyphPoint.X, aGlyphPoint.Y, aGlyphPoint.X + aGlyphSize.cx,
                aGlyphPoint.Y + aGlyphSize.cy);
              for aState in aValidStates do
                begin
                  BtnBitmaps[aState].Canvas.SetRealGlyphColor(GlyphColor, aState);
                  BtnBitmaps[aState].Canvas.DrawGlyph(aRect, egdArrowDown, aState);
                end;
            end;
          if assigned(Images) then
	          begin
              aImageIndex := ImageIndex;
              aImgIdxChckd := ImageIndexChecked;
              aGlyphSize := Size(Images.Width, Images.Height);
              if (aImageIndex >= 0) and (aImageIndex < Images.Count) then  
                begin
                  if ((aImgIdxChckd < 0) or (aImgIdxChckd >= Images.Count)) and (Mode <> ebmSpeedBtn)
                    then aImgIdxChckd := aImageIndex;
                end else
                if (aImgIdxChckd >= 0) and (aImgIdxChckd < Images.Count) and (Mode = ebmSpeedBtn)
                  then aImageIndex := aImgIdxChckd
                  else aImageIndex := -1;
            end else
            begin
              aImageIndex := -1;
              aImgIdxChckd := -1;
            end;
          if (aImageIndex < 0) and (aImgIdxChckd < 0) then
            begin
              aGlyphDesign := GlyphDesign;
              aGlyphDsgnChckd := GlyphDesignChecked;
              if aGlyphDsgnChckd = egdNone then aGlyphDsgnChckd := aGlyphDesign;
              if (aGlyphDesign = egdNone) and (Mode = ebmSpeedBtn) then aGlyphDesign := GlyphDesignChecked;
              aGlyphSize := Canvas.GlyphExtent(aGlyphDesign);
              if aGlyphDesign > egdNone then
                begin
                  aGlyphSize.cx := aGlyphSize.cx or 1;
                  aGlyphSize.cy := aGlyphSize.cy or 1;
                end;             
            end;
          aRect := ClientRect;
          if (aTextSize.cx > 0) and not AutoSize and (Margin >= 0) and (Layout in [eopRight, eopLeft]) then
            if not IsRightToLeft xor (Layout = eopRight)
              then aRect.Right := aTextSize.cx + 2 * Margin
              else aRect.Left := aRect.Right - aTextSize.cx - 2 * Margin;
          if aGlyphSize.cx*aTextSize.cx <> 0 then
            begin
              if RealLayout in [eopTop, eopBottom] then
                begin
                  aLength := aGlyphSize.cy + aTextSize.cy;
                  aGlyphPoint.X := (Width - aGlyphSize.cx) div 2;
                end else 
                begin
                  aLength := aGlyphSize.cx + aTextSize.cx;
                  aGlyphPoint.Y := (Height - aGlyphSize.cy) div 2;
                end;
              inc(aLength, Spacing);
              case RealLayout of
                eopTop: 
                  begin
                    aGlyphPoint.Y := (Height - aLength) div 2;
                    aRect.Top := aGlyphPoint.Y + aGlyphSize.cy + Spacing;    
                  end;
                eopRight: 
                  begin
                    if Margin < 0
                      then aRect.Left := (Width - aLength) div 2
                      else aRect.Left := Width - aLength - Margin;
                    aGlyphPoint.X := aRect.Left + aTextSize.cx + Spacing;
                  end;
                eopBottom:
                  begin
                    aRect.Top := (Height - aLength) div 2;
                    aGlyphPoint.Y := aRect.Top + aTextSize.cy + Spacing; 
                  end;
                eopLeft: 
                  begin
                    if Margin < 0
                      then aGlyphPoint.X := (Width - aLength) div 2
                      else aGlyphPoint.X := Margin;
                    aRect.Left := aGlyphPoint.X + aGlyphSize.cx + Spacing;  
                  end;
              end;
              if RealLayout in [eopRight, eopLeft]
                then aRect.Right := aRect.Left + aTextSize.cx
                else aRect.Bottom := aRect.Top + aTextSize.cy;
            end else
            aGlyphPoint := Point((Width - aGlyphSize.cx) div 2, (Height - aGlyphSize.cy) div 2);  
          if aTextSize.cx > 0 then
            begin              
              if UseRightToLeftReading then aFlags := aFlags or DT_RTLREADING;     
              for aState in aValidStates do
                begin
                  BtnBitmaps[aState].Canvas.Font.Assign(Font);
                  if aState >= eisPushed then
                    begin
                      BtnBitmaps[aState].Canvas.Font.Style := CheckedFontStyles;
                      BtnBitmaps[aState].Canvas.Font.Color := CheckedFontColor;
                    end;
                  if BtnBitmaps[aState].Canvas.Font.Color = clDefault
                    then BtnBitmaps[aState].Canvas.Font.Color := clBtnText;  
                  with ThemeServices do 
                    DrawText(BtnBitmaps[aState].Canvas, GetElementDetails(caThemedContent[aState]), 
                      Caption, aRect, aFlags, 0);
                end;
            end;
          if (aImageIndex >= 0) or (aImgIdxChckd >= 0) then
            begin
              if Mode = ebmSpeedBtn then
                begin
                  for aState in aValidStates do
                    ThemeServices.DrawIcon(BtnBitmaps[aState].Canvas, 
                      ThemeServices.GetElementDetails(caThemedContent[aState]), 
                      aGlyphPoint, FImages, aImageIndex);
                end else
                begin
                  if aImageIndex >= 0 then
                    for aState in aValidStates*[eisDisabled, eisHighlighted, eisEnabled] do
                      ThemeServices.DrawIcon(BtnBitmaps[aState].Canvas,
                        ThemeServices.GetElementDetails(caThemedContent[aState]),
                        aGlyphPoint, FImages, aImageIndex);
                  if aImgIdxChckd >= 0 then
                    for aState in aValidStates*[eisPushed, eisPushedHihlighted, eisPushedDisabled] do
                      ThemeServices.DrawIcon(BtnBitmaps[aState].Canvas,
                        ThemeServices.GetElementDetails(caThemedContent[aState]),
                        aGlyphPoint, FImages, aImgIdxChckd);
                end;
            end else
            begin
              if aTextSize.cx = 0 then 
                begin
                  case aGlyphDesign of
                    egdGrid..egdSizeArrLeft: AdjustRect(30, 30);
                    egdRectBeveled..high(TGlyphDesign): AdjustRect(85, 30);
                  end;
                end else
                aRect := Rect(aGlyphPoint.X, aGlyphPoint.Y, aGlyphPoint.X + aGlyphSize.cx, aGlyphPoint.Y + aGlyphSize.cy);
              if Mode = ebmSpeedBtn then
                begin
                  if aGlyphDesign > egdNone then
                    for aState in aValidStates do
                      begin
                        BtnBitmaps[aState].Canvas.SetRealGlyphColor(GlyphColor, aState);
                        BtnBitmaps[aState].Canvas.DrawGlyph(aRect, aGlyphDesign, aState);
                      end;
                end else
                begin
                  if aGlyphDesign > egdNone then    
                    for aState in aValidStates*[eisDisabled, eisHighlighted, eisEnabled] do
                      begin
                        BtnBitmaps[aState].Canvas.SetRealGlyphColor(GlyphColor, aState);
                        BtnBitmaps[aState].Canvas.DrawGlyph(aRect, aGlyphDesign, aState)
                      end;   
                  if aGlyphDsgnChckd > egdNone then         
                    for aState in aValidStates*[eisPushed, eisPushedHihlighted, eisPushedDisabled] do
                      begin
                        BtnBitmaps[aState].Canvas.SetRealGlyphColor(GlyphColor, aState);
                        BtnBitmaps[aState].Canvas.DrawGlyph(aRect, aGlyphDsgnChckd, aState)
                      end;
                end;
            end;
        end;
    end;
  NeedRedraw := False;
end;

procedure TCustomECSpeedBtn.EndUpdate;
begin
  dec(UpdateCount);
  if UpdateCount = 0 then
    begin
      NeedRedraw := True;
      if AutoSize then
        begin
          InvalidatePreferredSize;
          AdjustSize;
        end;
      Invalidate;
    end;
end;

procedure TCustomECSpeedBtn.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);
  NeedRedraw := True;  { Invalidate not necessary here }  
end;

function TCustomECSpeedBtn.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TECSpeedBtnActionLink;
end;

function TCustomECSpeedBtn.HasValidImages: Boolean;
begin
  Result := (assigned(Images) and ((ImageIndex >= 0) or (ImageIndexChecked >= 0))
    and ((ImageIndex < Images.Count) or (ImageIndexChecked < Images.Count)));
end;

procedure TCustomECSpeedBtn.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
    begin
      BtnPushed := True;
      if (Mode = ebmSpeedBtn) and assigned(ECTimer) then
        if (Repeating = 0) or assigned(OnRepeating) then ECTimer.Enabled := True;
    end;
  if not Checked then Invalidate;
end;

procedure TCustomECSpeedBtn.MouseLeave;
begin
  {$IFDEF DBGCTRLS} DebugLn('TBaseECSpeedBtn.MouseLeave'); {$ENDIF}
  inherited MouseLeave;
  if BtnPushed then MouseUp(mbLeft, [ssLeft], 0, 0);
  Invalidate;  
end;

procedure TCustomECSpeedBtn.MouseEnter;
begin
  inherited MouseEnter;
  Invalidate;  
end;

procedure TCustomECSpeedBtn.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);      
  BtnPushed := False;
  if Mode <> ebmDelayBtn then
    begin
      if assigned(ECTimer) then ECTimer.Enabled := False;
      if Mode = ebmSpeedBtn then Invalidate;  { when Delay <> 0 SetChecked cares of Invalidate; }
    end else
    if Delay = 0 then Invalidate;
end;

procedure TCustomECSpeedBtn.Paint;
var aState: TItemState;
    bEnabled: Boolean;
begin
  {$IFDEF DBGCTRLS} DebugLn('TCustomECSpeedBtn.Paint'); {$ENDIF}
  inherited Paint;
  bEnabled := IsEnabled;
  if (bEnabled xor (eisEnabled in ValidStates)) or (ValidStates = []) then
    begin
      NeedRedraw := True;
      CreateValidBitmaps(bEnabled);
    end;    
  if NeedRedraw then DrawButtonBMPs;
  aState := eisEnabled;
  if bEnabled then
    begin
      if BtnPushed or Checked or (assigned(ECTimer) and ECTimer.Enabled) then
        begin
          if not MouseEntered 
            then aState := eisPushed
            else aState := eisPushedHihlighted;
        end else 
        if MouseEntered then aState := eisHighlighted;    
    end else
    begin
      if Checked 
        then aState := eisPushedDisabled
        else aState := eisDisabled;
    end;    
  Canvas.Draw(0, 0, BtnBitmaps[aState]);
  BtnDrawnPushed := (aState in [eisPushed, eisPushedHihlighted]);
end;

procedure TCustomECSpeedBtn.Redraw;
begin
  NeedRedraw := True;
  if UpdateCount = 0 then Invalidate;
end;

procedure TCustomECSpeedBtn.Resize;
var aState: TItemState;
    aWidth, aHeight: Integer;
begin
  inherited Resize;
  aWidth := Width;
  aHeight := Height;
  if (aWidth <> PrevSize.cx) or (aHeight <> PrevSize.cy) then
    begin
      for aState in ValidStates do
        BtnBitmaps[aState].SetSize(aWidth, aHeight);
      Redraw;
      PrevSize.cx := aWidth;
      PrevSize.cy := aHeight;
    end;  
end;

procedure TCustomECSpeedBtn.ResizeInvalidate;
begin
  if UpdateCount = 0 then
    begin;
      if AutoSize then
        begin
          InvalidatePreferredSize;
          AdjustSize;
        end;    
      Invalidate;
    end;
end;            

procedure TCustomECSpeedBtn.SetAction(Value: TBasicAction);
begin
  inherited SetAction(Value);
  if assigned(Value) and (Value is TCustomAction) and
    (Value as TCustomAction).AutoCheck then Delay := -1;
end;            

procedure TCustomECSpeedBtn.SetAutoSize(Value: Boolean);
begin
  inherited SetAutoSize(Value);
  if Value then
    begin
      InvalidatePreferredSize;
      AdjustSize;
      NeedRedraw := True;
      Invalidate;   
    end; 
end;                        

procedure TCustomECSpeedBtn.SetParent(NewParent: TWinControl);
begin
  inherited SetParent(NewParent);
  NeedRedraw := True;
end;

procedure TCustomECSpeedBtn.SetTimerEvent;
begin
  if Mode = ebmDelayBtn
    then ECTimer.OnTimer := @TimerOnTimerDelay
    else if Repeating = 0
           then ECTimer.OnTimer := @TimerOnTimerHold
           else ECTimer.OnTimer := @TimerOnTimerRepeating;
end;

procedure TCustomECSpeedBtn.TextChanged;
begin
  inherited TextChanged;
  NeedRedraw := True;
  ResizeInvalidate; 
end;             

procedure TCustomECSpeedBtn.TimerOnTimerDelay(Sender: TObject);
begin
  ECTimer.Enabled := False;
  Checked := False;
  Invalidate;
end;

procedure TCustomECSpeedBtn.TimerOnTimerHold(Sender: TObject);
begin
  ECTimer.Enabled := False;
  if assigned(OnHoldDown) then
    begin
      OnHoldDown(self);
      ControlState := ControlState - [csClicked];
    end;
end;

procedure TCustomECSpeedBtn.TimerOnTimerRepeating(Sender: TObject);
begin
  if assigned(OnRepeating) then
    begin
      OnRepeating(self);
      ControlState := ControlState - [csClicked];
    end;
end;

procedure TCustomECSpeedBtn.UpdateGroup;
var aMsg : TLMessage;
begin
  if (FGroupIndex <> 0) and (Parent <> nil) and (not (csLoading in ComponentState)) then
  begin
    aMsg.Msg := CM_ButtonPressed;
    aMsg.WParam := FGroupIndex;
    aMsg.LParam := PtrInt(self);
    aMsg.Result := 0;
    Parent.Broadcast(aMsg);
  end;    
end;                 

{ TCustomECSpeedBtn.Setters }

procedure TCustomECSpeedBtn.SetAllowAllUp(AValue: Boolean);
begin
  if FAllowAllUp = AValue then exit;
  FAllowAllUp := AValue;
  UpdateGroup;
end; 

procedure TCustomECSpeedBtn.SetChecked(AValue: Boolean);
begin                                                  
  if (FChecked = AValue) or (not AValue and (GroupIndex <> 0) and (not AllowAllUp)) then exit;
  FChecked := AValue;
  if [csLoading, csDestroying]*ComponentState = [] then
    begin
      if GroupIndex <> 0 then UpdateGroup;
      if AValue then 
        begin
          if Mode = ebmSpeedBtn then Mode := ebmToggleBox;
          if Mode = ebmDelayBtn then
            begin
              if not (csDesigning in ComponentState)
                then ECTimer.Enabled := True
                else Mode := ebmToggleBox;
            end;
        end;
      if assigned(OnChange) then OnChange(self);
      if not AValue and assigned(OnRelease) then OnRelease(self);
    end; 
  if not AValue or not BtnDrawnPushed then Invalidate;
end;

procedure TCustomECSpeedBtn.SetCheckedFontColor(AValue: TColor);
begin
  if FCheckedFontColor=AValue then exit;
  FCheckedFontColor:=AValue;
  Redraw;
end;

procedure TCustomECSpeedBtn.SetCheckedFontStyles(AValue: TFontStyles);
begin
  if FCheckedFontStyles=AValue then exit;
  FCheckedFontStyles:=AValue;
  Redraw;
end;

procedure TCustomECSpeedBtn.SetDelay(AValue: Integer);
begin
  if FDelay = AValue then exit;       
  FDelay := AValue;
  if not (csDesigning in ComponentState) then
    begin
      if (AValue > 0) and (Mode <> ebmToggleBox) then
        begin
          if not assigned(ECTimer) then
            begin
              CreateTimer;
              SetTimerEvent;
            end;
          ECTimer.Delay := AValue;
          ECTimer.Repeating := Repeating;
        end else
        FreeAndNil(ECTimer);
    end;
end;

procedure TCustomECSpeedBtn.SetDropDownGlyph(AValue: TDropDownGlyph);
begin
  if FDropDownGlyph = AValue then exit;
  FDropDownGlyph := AValue;
  Redraw;
end;

procedure TCustomECSpeedBtn.SetFlat(AValue: Boolean);
begin
  if FFlat=AValue then exit;
  FFlat:=AValue;
  Redraw;
end;

procedure TCustomECSpeedBtn.SetGlyphColor(AValue: TColor);
begin
  if FGlyphColor = AValue then exit;
  FGlyphColor := AValue;
  if not HasValidImages then Redraw;
end;

procedure TCustomECSpeedBtn.SetGlyphDesign(AValue: TGlyphDesign);
begin
  if FGlyphDesign=AValue then exit;
  FGlyphDesign:=AValue;
  if not HasValidImages then
    begin
      NeedRedraw := True;
      ResizeInvalidate;
    end;
end;

procedure TCustomECSpeedBtn.SetGlyphDesignChecked(AValue: TGlyphDesign);
begin
  if FGlyphDesignChecked = AValue then exit;
  FGlyphDesignChecked := AValue;
  if not HasValidImages and ((Mode <> ebmSpeedBtn) or (GlyphDesign = egdNone)) then Redraw;
end;

procedure TCustomECSpeedBtn.SetGroupIndex(AValue: Integer);
begin
  if FGroupIndex = AValue then exit;
  FGroupIndex := AValue;
  if AValue <> 0 then 
    begin
      FDelay := -1;  { only checkable button makes sense in group }
      UpdateGroup;
    end;
end;   

procedure TCustomECSpeedBtn.SetImageIndex(AValue: TImageIndex);
begin
  if FImageIndex = AValue then exit;
  FImageIndex := AValue;
  NeedRedraw := True;
  ResizeInvalidate;
end;

procedure TCustomECSpeedBtn.SetImageIndexChecked(AValue: TImageIndex);
begin
  if FImageIndexChecked = AValue then exit;
  FImageIndexChecked := AValue;
  if (Mode <> ebmSpeedBtn) or (ImageIndex < 0) then Redraw;
end;

procedure TCustomECSpeedBtn.SetImages(AValue: TCustomImageList);
begin
  if FImages = AValue then exit; 
  FImages := AValue;
  NeedRedraw := True;
  ResizeInvalidate;
end;

procedure TCustomECSpeedBtn.SetLayout(AValue: TObjectPos);
begin
  if FLayout = AValue then exit;
  FLayout := AValue;
  if IsRightToLeft then
    case AValue of
      eopRight: AValue := eopLeft;
      eopLeft: AValue := eopRight;
    end;
  RealLayout := AValue;                           
  NeedRedraw := True;
  ResizeInvalidate;
end;

procedure TCustomECSpeedBtn.SetMargin(AValue: SmallInt);
begin
  if FMargin = AValue then exit;
  FMargin := AValue;    
  NeedRedraw := True;
  ResizeInvalidate;
end;

procedure TCustomECSpeedBtn.SetMode(AValue: TButtonMode);
begin
  if FMode = AValue then exit;
  if (AValue = ebmSpeedBtn) or (FMode = ebmSpeedBtn) and ((ImageIndex <> ImageIndexChecked)
    or (GlyphDesign <> GlyphDesignChecked)) then NeedRedraw := True;
  FMode := AValue;
  if AValue <> ebmToggleBox then
    begin
      FGroupIndex := 0;
      Checked := False;
      if Delay > 0 then
        begin
          if not assigned(ECTimer) then CreateTimer;
          ECTimer.Interval := Delay;
        end;
    end else
    FreeAndNil(ECTimer);
  if assigned(ECTimer) then SetTimerEvent;
end;

procedure TCustomECSpeedBtn.SetRepeating(AValue: Integer);
begin
  if FRepeating = AValue then exit;
  FRepeating := AValue;
  if assigned(ECTimer) then
    begin
      ECTimer.Repeating := AValue;
      SetTimerEvent;
    end;
end;

procedure TCustomECSpeedBtn.SetShowCaption(AValue: Boolean);
begin
  if FShowCaption = AValue then exit;
  FShowCaption := AValue;
  if Caption <> '' then
    begin
      NeedRedraw := True;
      ResizeInvalidate;
    end;
end;

procedure TCustomECSpeedBtn.SetSpacing(AValue: SmallInt);
begin
  if FSpacing = AValue then exit;
  FSpacing := AValue;
  NeedRedraw := True;
  ResizeInvalidate;  
end;

procedure TCustomECSpeedBtn.SetTransparent(AValue: Boolean);
var aState: TItemState;
begin
  if FTransparent=AValue then exit;
  FTransparent:=AValue;
  for aState in ValidStates do
    BtnBitmaps[aState].Transparent := AValue;
  Redraw;
end;

{ TCustomECSpeedBtnPlus }

constructor TCustomECSpeedBtnPlus.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csNoDesignSelectable];
  SetSubComponent(True);
end;

procedure TCustomECSpeedBtnPlus.Click;
begin
  inherited Click;
  if assigned(CustomClick) then CustomClick;
end;     

procedure TCustomECSpeedBtnPlus.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if assigned(CustomMouseDown) then CustomMouseDown(Button, Shift);
  if assigned(Owner) and (Owner is TWinControl) and (Owner as TWinControl).CanFocus 
    then (Owner as TWinControl).SetFocus;   
end;    

procedure TCustomECSpeedBtnPlus.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if assigned(CustomMouseUp) then CustomMouseUp(Button, Shift);
end;    

procedure TCustomECSpeedBtnPlus.Resize;
begin
  inherited Resize;
  if assigned(CustomResize) then CustomResize;
end;    

{ TECEditBtnSpacing }

function TECEditBtnSpacing.GetSpace(Kind: TAnchorKind): Integer;
begin
  Result:=inherited GetSpace(Kind);
  case Kind of
    akLeft: if Control.IsRightToLeft then
              inc(Result, TBaseECEditBtn(Control).FAnyButton.Width + TBaseECEditBtn(Control).Indent);
    akRight: if not Control.IsRightToLeft then
               inc(Result, TBaseECEditBtn(Control).FAnyButton.Width + TBaseECEditBtn(Control).Indent);
  end;              
end;

procedure TECEditBtnSpacing.GetSpaceAround(var SpaceAround: TRect);
var aIndentButtonWidth: Integer;
begin
  inherited GetSpaceAround(SpaceAround);
  with TBaseECEditBtn(Control) do
    aIndentButtonWidth := FAnyButton.Width + Indent;
  if not Control.IsRightToLeft 
    then inc(SpaceAround.Right, aIndentButtonWidth)
    else inc(SpaceAround.Left, aIndentButtonWidth);
end;
    
{ TBaseECEditBtn }

constructor TBaseECEditBtn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption];
  with FAnyButton do
    begin
      AnchorParallel(akTop, 0, self);
      AnchorParallel(akBottom, 0, self);   
      CustomResize := @SetButtonPosition;
    end;
  FOptions := cDefEBOptions;
  AccessibleRole := larTextEditorSingleline;
end;

function TBaseECEditBtn.ChildClassAllowed(ChildClass: TClass): boolean;
begin
  Result := (ChildClass = TCustomECSpeedBtnPlus);
end;

procedure TBaseECEditBtn.CMBiDiModeChanged(var Message: TLMessage);
begin
  inherited CMBiDiModeChanged(Message);
  SetButtonPosition;
end;

function TBaseECEditBtn.CreateControlBorderSpacing: TControlBorderSpacing;
begin
  Result := TECEditBtnSpacing.Create(self);
end;

procedure TBaseECEditBtn.DoOnChangeBounds;
begin
  inherited DoOnChangeBounds;
  SetButtonPosition;
end;

procedure TBaseECEditBtn.InitializeWnd;
begin
  inherited InitializeWnd;
  SetButtonPosition;
end; 
      
procedure TBaseECEditBtn.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_RETURN: 
      if ((ssModifier in Shift) and (eboClickCtrlEnter in FOptions)) or 
        ((ssAlt in Shift) and (eboClickAltEnter in FOptions)) or
        ((ssShift in Shift) and (eboClickShiftEnter in FOptions)) 
        then FAnyButton.Click;
    VK_SPACE: 
      if (ssModifier in Shift) or ReadOnly then FAnyButton.Click;
  end;
  inherited KeyDown(Key, Shift);
end;

procedure TBaseECEditBtn.SetButtonPosition;
begin
  if not IsRightToLeft
    then FAnyButton.Left := Left + Width + Indent
    else FAnyButton.Left := Left - Indent - FAnyButton.Width;         
end;    

procedure TBaseECEditBtn.SetEnabled(Value: Boolean);
begin
  inherited SetEnabled(Value);        
  FAnyButton.Enabled := Value;
end;    

procedure TBaseECEditBtn.SetParent(NewParent: TWinControl);
begin
  FAnyButton.Anchors := [];
  inherited SetParent(NewParent);
  FAnyButton.Parent := Parent;
  FAnyButton.Anchors := [akTop, akBottom];
end;

procedure TBaseECEditBtn.SetRealBoundRect(ARect: TRect);
begin
  if BiDiMode = bdLeftToRight 
    then dec(ARect.Right, Indent + FAnyButton.Width)
    else inc(ARect.Left, Indent + FAnyButton.Width);
  BoundsRect := ARect;
end; 
          
procedure TBaseECEditBtn.SetRealBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if BiDiMode <> bdLeftToRight then ALeft := ALeft + Indent + FAnyButton.Width;
  SetBounds(ALeft, ATop, AWidth - Indent - FAnyButton.Width, AHeight);
end;                  

procedure TBaseECEditBtn.SetVisible(Value: Boolean);
begin
  inherited SetVisible(Value);
  FAnyButton.Visible := Value;
  if assigned(OnVisibleChanged) then OnVisibleChanged(self, Value);
end;     

procedure TBaseECEditBtn.SwitchOption(AOption: TEBOption; AOn: Boolean);
var aOptions: TEBOptions;
begin
  aOptions := FOptions;
  if AOn 
    then Include(aOptions, AOption)
    else Exclude(aOptions, AOption);
  Options := aOptions;
end;       

{ Setters }

function TBaseECEditBtn.GetWidthInclBtn: Integer;
begin
  Result := Width + Indent + FAnyButton.Width;
end;     

procedure TBaseECEditBtn.SetIndent(AValue: SmallInt);
begin
  if FIndent = AValue then exit;
  FIndent := AValue;
  SetButtonPosition;
end;

procedure TBaseECEditBtn.SetWidthInclBtn(AValue: Integer);
begin
  Width := AValue - Indent - FAnyButton.Width;
end;

{ TECEditBtn }

constructor TECEditBtn.Create(AOwner: TComponent);
begin
  FButton := TECSpeedBtnPlus.Create(self);
  FAnyButton := FButton;
  FButton.Name := 'ECEditBtnButton';
  inherited Create(AOwner);    
end;   

{ TECSpeedBtnColor }

constructor TECSpeedBtnColor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGlyphDesign := cDefGlyphDesign;
end;        

{ TECColorBtn }

constructor TECColorBtn.Create(AOwner: TComponent);
begin
  FButton := TECSpeedBtnColor.Create(self);
  FAnyButton := FButton;
  with FButton do
    begin  
      CustomClick := @DoButtonClick;
      Name := 'ECCBSpeedBtn';
      Width := 27;
    end;     
  inherited Create(AOwner);
  ReadOnly := True;
  FCustomColor := cDefCustomColor;
  FPrefix := '$';
  Redraw;
end;

procedure TECColorBtn.DoButtonClick;
var aAlpha: Integer;
    aColorDialog: TColorDialog;
begin
  aAlpha := CustomColor and $FF000000;
  aColorDialog := TColorDialog.Create(self);
  aColorDialog.Color := CustomColor and $FFFFFF;
  if aColorDialog.Execute then CustomColor := aAlpha + aColorDialog.Color;
  aColorDialog.Free;
  SetFocus;
end;        

procedure TECColorBtn.EditingDone;
var aColor: TColor;
begin   
  inherited EditingDone;
  if TryStrToColorLayouted(Text, ColorLayout, aColor) then CustomColor := aColor; 
end;

procedure TECColorBtn.Redraw;
var aPrefix: string;
begin
  if ColorLayout <> eclSystemBGR 
    then aPrefix := Prefix
    else aPrefix := '';
  Text := aPrefix + ColorToStrLayouted(CustomColor, ColorLayout);
end;

{ Setters }

procedure TECColorBtn.SetCustomColor(AValue: TColor);
begin
  if FCustomColor = AValue then exit;
  FCustomColor := AValue;
  FButton.GlyphColor := AValue and $FFFFFF;
  Redraw;
  if assigned(OnCustomColorChanged) then OnCustomColorChanged(self);
end;

procedure TECColorBtn.SetColorLayout(AValue: TColorLayout);
begin
  if FColorLayout = AValue then exit;
  FColorLayout := AValue;
  Redraw;
end;

procedure TECColorBtn.SetPrefix(const AValue: string);
begin
  if FPrefix = AValue then exit;
  FPrefix := AValue;
  Redraw;
end;

{ TECComboBtnSpacing }

function TECComboBtnSpacing.GetSpace(Kind: TAnchorKind): Integer;
begin
  Result:=inherited GetSpace(Kind);
  case Kind of
    akLeft: if Control.IsRightToLeft then
              inc(Result, TBaseECComboBtn(Control).FAnyButton.Width + TBaseECComboBtn(Control).Indent);
    akRight: if not Control.IsRightToLeft then
               inc(Result, TBaseECComboBtn(Control).FAnyButton.Width + TBaseECComboBtn(Control).Indent);
  end;  
end;

procedure TECComboBtnSpacing.GetSpaceAround(var SpaceAround: TRect);
var aIndentButtonWidth: Integer;
begin
  inherited GetSpaceAround(SpaceAround);
  with TBaseECComboBtn(Control) do
    aIndentButtonWidth := FAnyButton.Width + Indent;
  if not Control.IsRightToLeft 
    then inc(SpaceAround.Right, aIndentButtonWidth)
    else inc(SpaceAround.Left, aIndentButtonWidth);
end;   

{ TBaseECComboBtn }

constructor TBaseECComboBtn.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ControlStyle := ControlStyle - [csSetCaption];
  with FAnyButton do
    begin
      AnchorParallel(akTop, 0, self);
      AnchorParallel(akBottom, 0, self);   
      CustomResize := @SetButtonPosition;
    end;
  FOptions := cDefEBOptions;
end;

procedure TBaseECComboBtn.Add(const AItem: string);
begin
  case ItemOrder of
    eioFixed: AddItemLimit(AItem, False);
    eioHistory: AddItemHistory(AItem, False);
    eioSorted: AddItemLimit(AItem, False);
  end;
end;

procedure TBaseECComboBtn.AddItemHistory(const AItem: string; ACaseSensitive: Boolean);
var aMaxCount: Integer;
begin
  aMaxCount := MaxCount;
  if aMaxCount <= 0 then aMaxCount := high(Integer);
  AddHistoryItem(aItem, aMaxCount, True, ACaseSensitive);
end;

procedure TBaseECComboBtn.AddItemLimit(const AItem: string; ACaseSensitive: Boolean);
var i, aCount: Integer;
    aDuplicates: TDuplicates;
begin
  aDuplicates := TStringList(Items).Duplicates;
  Items.BeginUpdate;
  if (aDuplicates <> dupAccept) and not TStringList(Items).Sorted then    
    begin
      if not ACaseSensitive then
        begin
          for i := Items.Count -1 downto 0 do
            if AnsiCompareText(Items[i], AItem) = 0 then 
              case aDuplicates of
                dupIgnore: Items.Delete(i);
                dupError: 
                  begin
                    Items.EndUpdate;
                    exit;  { Exit! }
                  end;
              end;
        end else
        begin
          for i := Items.Count -1 downto 0 do
            if Items[i] = AItem then 
              case aDuplicates of
                dupIgnore: Items.Delete(i);
                dupError: 
                  begin
                    Items.EndUpdate;
                    exit;  { Exit! }
                  end;
              end;               
        end;
    end;
  if not TStringList(Items).Sorted 
    then Items.Insert(Items.Count, AItem)  { add new item to the end }
    else Items.Add(AItem);                 { or somewhere ...}
  { remove overflow item from the beginning; it works on sorted list too, so beware }
  aCount := MaxCount;
  if aCount <= 0 then aCount := high(Integer);    
  for i := 1 to Items.Count - aCount do
    Items.Delete(0);
  Items.EndUpdate;
end;

function TBaseECComboBtn.ChildClassAllowed(ChildClass: TClass): boolean;
begin
  Result := (ChildClass = TCustomECSpeedBtnPlus);
end;

procedure TBaseECComboBtn.CMBiDiModeChanged(var Message: TLMessage);
begin
  inherited CMBiDiModeChanged(Message);
  SetButtonPosition;
end;  

function TBaseECComboBtn.CreateControlBorderSpacing: TControlBorderSpacing;
begin
  Result := TECComboBtnSpacing.Create(self);
end;

procedure TBaseECComboBtn.DoOnChangeBounds;
begin
  inherited DoOnChangeBounds;
  SetButtonPosition;
end;

procedure TBaseECComboBtn.InitializeWnd;
begin
  inherited InitializeWnd;
  SetButtonPosition;
end;

procedure TBaseECComboBtn.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of 
    VK_RETURN:
      begin
        if ((ssModifier in Shift) and (eboClickCtrlEnter in Options)) or
          ((ssAlt in Shift) and (eboClickAltEnter in Options)) or
          ((ssShift in Shift) and (eboClickShiftEnter in Options)) 
          then FAnyButton.Click;
      end;
    VK_SPACE:
      if (ssModifier in Shift) or ReadOnly then FAnyButton.Click;
  end;
  inherited KeyDown(Key, Shift);
end;

procedure TBaseECComboBtn.SetButtonPosition;
begin
  if not IsRightToLeft
    then FAnyButton.Left := Left + Width + Indent
    else FAnyButton.Left := Left - Indent - FAnyButton.Width;
end;

procedure TBaseECComboBtn.SetEnabled(Value: Boolean);
begin
  inherited SetEnabled(Value); 
  FAnyButton.Enabled := Value;
end;

procedure TBaseECComboBtn.SetParent(NewParent: TWinControl);
begin
  FAnyButton.Anchors := [];
  inherited SetParent(NewParent);
  FAnyButton.Parent := NewParent;
  FAnyButton.Anchors := [akTop, akBottom];
end;        

procedure TBaseECComboBtn.SetRealBoundRect(ARect: TRect);
begin
  if BiDiMode = bdLeftToRight 
    then dec(ARect.Right, Indent + FAnyButton.Width)
    else inc(ARect.Left, Indent + FAnyButton.Width);
  BoundsRect := ARect;    
end;

procedure TBaseECComboBtn.SetRealBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if BiDiMode <> bdLeftToRight then ALeft := ALeft + Indent + FAnyButton.Width;
  SetBounds(ALeft, ATop, AWidth - Indent - FAnyButton.Width, AHeight);  
end;        

procedure TBaseECComboBtn.SetSorted(Val: boolean);
begin
  if Val then FItemOrder := eioSorted;
  inherited SetSorted(Val);
end;         

procedure TBaseECComboBtn.SetVisible(Value: Boolean);
begin
  inherited SetVisible(Value);
  FAnyButton.Visible := Value;
  if assigned(OnVisibleChanged) then OnVisibleChanged(self, Value);
end;        

{ TBaseECComboBtn.Setters }

function TBaseECComboBtn.GetWidthInclBtn: Integer;
begin
  Result := Width + Indent + FAnyButton.Width;
end;       

procedure TBaseECComboBtn.SetIndent(AValue: SmallInt);
begin
  if FIndent = AValue then exit;
  FIndent := AValue;
  SetButtonPosition;
end;

procedure TBaseECComboBtn.SetItemOrder(AValue: TItemOrder);
begin
  if FItemOrder = AValue then exit;
  FItemOrder := AValue;
  Sorted := (AValue = eioSorted);
end;         

procedure TBaseECComboBtn.SetMaxCount(AValue: Integer);
var i: Integer;
begin
  if FMaxCount=AValue then exit;
  FMaxCount:=AValue;
  if (AValue > 0) and (AValue < Items.Count) then 
    begin
      Items.BeginUpdate;
      for i := Items.Count - 1 downto AValue do 
        Items.Delete(i);
      Items.EndUpdate;
    end;
end;     

procedure TBaseECComboBtn.SetOptions(AValue: TEBOptions);
begin
  if FOptions = AValue then exit;
  FOptions := AValue;
end;

procedure TBaseECComboBtn.SetWidthInclBtn(AValue: Integer);
begin
  Width := AValue - Indent - FAnyButton.Width;
end;

{ TECComboBtn }

constructor TECComboBtn.Create(TheOwner: TComponent);
begin
  FButton := TECSpeedBtnPlus.Create(self);
  FAnyButton := FButton;
  FButton.Name := 'ECCSpeedBtn';
  FItemOrder := cDefItemOrder;
  inherited Create(TheOwner); 
end;

{ TECColorCombo }

constructor TECColorCombo.Create(AOwner: TComponent);
begin
  FButton := TECSpeedBtnColor.Create(self);
  FAnyButton := FButton;
  with FButton do
    begin  
      CustomClick := @DoButtonClick;
      Name := 'ECCCSpeedBtn';
      Width := 27;
    end;  
  inherited Create(AOwner);
  TStringList(Items).Duplicates := dupIgnore;
  ReadOnly := True;
  FItemOrder := cDefColorOrder;
  FCustomColor := cDefCustomColor;
  FPrefix := '$';
  Style := csOwnerDrawFixed;
  FNeedMeasure := True;
end;         

procedure TECColorCombo.AddColor(const AColor: string);
var anyColor: TColor;
begin
  if TryStrToColorLayouted(AColor, ColorLayout, anyColor) then AddColor(anyColor);       
end;        

procedure TECColorCombo.AddColor(AColor: TColor);
var aPrefix: string;
begin
  if ColorLayout <> eclSystemBGR
    then aPrefix := Prefix
    else aPrefix := '';      
  case ItemOrder of
    eioFixed: AddItemLimit(aPrefix + ColorToStrLayouted(AColor, ColorLayout), False);
    eioHistory: 
      begin
        AddItemHistory(aPrefix + ColorToStrLayouted(AColor, ColorLayout), False);
        ItemIndex := 0;  
      end;
    eioSorted: Items.Add(aPrefix + ColorToStrLayouted(AColor, ColorLayout)); 
  end;      
end;

procedure TECColorCombo.DoButtonClick;
var aAlpha: Integer;
    aColorDialog: TColorDialog;
begin
  aAlpha := CustomColor and $FF000000;
  aColorDialog := TColorDialog.Create(self);
  aColorDialog.Color := CustomColor and $FFFFFF;
  if aColorDialog.Execute then
    begin
      FSelectedFromList := False;
      CustomColor := aAlpha + aColorDialog.Color;
    end;
  aColorDialog.Free;
  SetFocus;         
end;         

procedure TECColorCombo.DrawItem(Index: Integer; ARect: TRect; State: TOwnerDrawState);
var aColor: TColor;
    bFocusedEditableMainItemNoDD: Boolean;  { combo has edit-like line edit in csDropDownList (Win) and is closed (not DroppedDown }
begin  { do not call inherited ! }  
  {$IFDEF DBGCTRLS} DebugLn('DrawItem ', ColorToString(Canvas.Brush.Color)); {$ENDIF}
  {$IF DEFINED(LCLWin32) or DEFINED(LCLWin64)}
  bFocusedEditableMainItemNoDD := (Focused and (ARect.Left > 0) and not DroppedDown);
  {$ELSE}
  bFocusedEditableMainItemNoDD := False;
  {$ENDIF}
  if not (odSelected in State) then Canvas.Brush.Color := clWindow;
  if (ARect.Left = 0) or bFocusedEditableMainItemNoDD then Canvas.FillRect(ARect);
  if FNeedMeasure then 
    begin
      if ColorLayout in [eclRGBColor, eclBGRColor, eclCMYColor, eclYMCColor]
        then FTextExtent := Canvas.TextExtent(Prefix + 'F9CDEB')
        else FTextExtent := Canvas.TextExtent(Prefix + 'F9CDEBA8');
      FNeedMeasure := False;    
    end;
  Canvas.Brush.Style := bsClear;
  if (not (odSelected in State) or (ARect.Left > 0)) and not bFocusedEditableMainItemNoDD
    then Canvas.Font.Color := GetColorResolvingDefault(Font.Color, clWindowText)
    else Canvas.Font.Color := clHighlightText;
  if bFocusedEditableMainItemNoDD then
    begin
      LCLIntf.SetBkColor(Canvas.Handle, ColorToRGB(clBtnFace));
      LCLIntf.DrawFocusRect(Canvas.Handle, aRect);
    end;
  inc(ARect.Left, 3);
  Canvas.TextOut(ARect.Left, (ARect.Top + ARect.Bottom - FTextExtent.cy) div 2, Items[Index]);
  if TryStrToColorLayouted(Items[Index], ColorLayout, aColor) then 
    with Canvas do
      begin
        Canvas.Pen.Color := clWindowText;
        Brush.Color := aColor;
        Brush.Style := bsSolid;
        Rectangle(ARect.Left + FTextExtent.cx + 2, ARect.Top + 1,
                  ARect.Right - 3, ARect.Bottom - 1);
     //   if (Index = 0) and (Items.Count <= 1) then CustomColor := aColor;       
      end;
end;

procedure TECColorCombo.EditingDone;
begin
  {$IFDEF DBGCTRLS} DebugLn('TECColorCombo.EditingDone');  {$ENDIF}
  if (ItemOrder = eioHistory) and (ItemIndex > 0) then
    begin
      Items.Move(ItemIndex, 0);
      ItemIndex := 0;
    end;
  inherited EditingDone;
end;

procedure TECColorCombo.Select;  { only when ItemIndex changes by mouse }
var aColor: TColor;
begin
  {$IFDEF DBGCTRLS} DebugLn('Select ', inttostr(ItemIndex));  {$ENDIF}
  inherited Select;
  if TryStrToColorLayouted(Items[ItemIndex], ColorLayout, aColor) then
    begin
      FSelectedFromList := True;
      CustomColor := aColor;
    end;
end;

procedure TECColorCombo.SetColorText(const AColor: string);
begin
  Text := AColor;
  Validate;
end;

procedure TECColorCombo.SetItemIndex(const Val: Integer);  { only when ItemIndex changes by code }
var aColor: TColor;
    aLoading: Boolean;
    aValue: Integer;
    aText: string;
begin
  {$IFDEF DBGCTRLS} DebugLn('TECColorCombo.SetItemIndex ' + inttostr(Val)); {$ENDIF}
  aValue := Val;
  aLoading := (csLoading in ComponentState);
  if not aLoading and (ItemOrder = eioHistory) then
    begin
      if (aValue > 0) and (aValue < Items.Count) then
        begin
          Items.Move(aValue, 0);
          aValue:=0;
        end;
    end;
  inherited SetItemIndex(aValue);
  if not aLoading then
    begin
      if aValue >= 0
        then aText := Items[aValue]
        else aText := Text;
      if TryStrToColorLayouted(aText, ColorLayout, aColor) then
        if not FUpdatingCustomColor
          then CustomColor := aColor
          else FCustomColor := aColor;
    end;
end;

procedure TECColorCombo.SetItems(const Value: TStrings);
begin
  inherited SetItems(Value);
  Validate;
end;

procedure TECColorCombo.Validate;
var aColor: TColor;
begin
  if TryStrToColorLayouted(Text, ColorLayout, aColor) then CustomColor := aColor;
end;   

{ TECColorCombo.Setters }

procedure TECColorCombo.SetColorLayout(AValue: TColorLayout);
var aColor: TColor;
    i: Integer;
    aOldLayout: TColorLayout;
    aPrefix: string;					
begin                        
  if FColorLayout = AValue then exit;
  aOldLayout := FColorLayout;
  FColorLayout := AValue;
  FNeedMeasure := True;
  if not (AValue = eclSystemBGR) 
    then aPrefix := Prefix
    else aPrefix := '';
  for i := 0 to Items.Count - 1 do
    if TryStrToColorLayouted(Items[i], aOldLayout, aColor) then
      Items[i] := aPrefix + ColorToStrLayouted(aColor, AValue);
end;

procedure TECColorCombo.SetCustomColor(AValue: TColor);
var aColorString: string;
    aIndex: Integer;
begin
  {$IFDEF DBGCTRLS} DebugLn('SetCustomColor ', ColorToString(AValue)); {$ENDIF}
  if FCustomColor = AValue then exit;
  FCustomColor := AValue;
  FButton.GlyphColor := AValue and $FFFFFF;
  if not (csLoading in ComponentState) then
    begin
      FUpdatingCustomColor := True;
      if not FSelectedFromList then
        begin
          aColorString := ColorToStrLayouted(AValue, ColorLayout);
          if ColorLayout <> eclSystemBGR then aColorString := Prefix + aColorString;
          aIndex := Items.IndexOf(aColorString);
          case ItemOrder of
            eioFixed:
              begin
                if aIndex = -1 then
                  begin
                    aIndex := Items.Add(aColorString);
                    ItemIndex := aIndex;
                  end else
                  ItemIndex := aIndex;
              end;
            eioHistory:
              begin
                if aIndex = -1
                  then Items.Insert(0, aColorString)
		              else Items.Move(aIndex, 0);
                ItemIndex := 0;
              end;
            eioSorted:
              begin
                if aIndex = -1 then
                  begin
                    Items.Add(aColorString);
                    TStringList(Items).Sort;
                    ItemIndex := TStringList(Items).IndexOf(aColorString);
                  end else
                  ItemIndex := aIndex;
              end;
          end;
          FSelectedFromList:=False;
        end;
      if assigned(OnCustomColorChanged) then OnCustomColorChanged(self);
      FUpdatingCustomColor:=False;
    end;
end;

procedure TECColorCombo.SetItemHeight(const AValue: Integer);
begin
  inherited SetItemHeight(AValue);
  FNeedMeasure := True;
end;

procedure TECColorCombo.SetPrefix(const AValue: string);
var i: Integer;          
begin
  if FPrefix = AValue then exit;
  FPrefix := AValue;                          
  FNeedMeasure := True;
  if ColorLayout <> eclSystemBGR then
    for i := 0 to Items.Count - 1 do
      Items[i] := AValue + TrimColorString(Items[i]);
end;            

end.


