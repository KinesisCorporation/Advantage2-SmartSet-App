{**************************************************************************************************
 This file is part of the Eye Candy Controls (EC-C)

  Copyright (C) 2015-2016 Vojtěch Čihák, Czech Republic

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

unit ECTabCtrl;
{$mode objfpc}{$H+}

//{$DEFINE DBGTAB}  {don't remove, just comment}

interface

uses
  Classes, SysUtils, LMessages, LResources, LCLIntf, LCLType, Controls, ComCtrls,
  Forms, Graphics, ImgList, {$IFDEF DBGTAB} LCLProc, {$ENDIF} LCLStrConsts, Menus,
  Math, Themes, Types, ECEditBtns, ECTypes;

type
  {$PACKENUM 2}
  TECTabFlag = (etfFolded,         { tab is folded }
                etfHasCloseBtn,    { tab has Close button }
                etfHasImage,       { tab has valid image; (ImageIndex>=0) and assigned(ImageList) }
                etfParentFont);    { FontOptions of tab was not yet changed }
  TECTabFlags = set of TECTabFlag;
  TECTabOption = (etoCanBeFolded,    { tab can be folded by other tabs }
                  etoCanFold,        { tab can fold other tabs }
                  etoCloseable,      { tab can be closed; Close btn closes tab and Close item appears in pop-menu }
                  etoCloseBtn,       { tab has Close button }
                  etoDontShrink,     { tab does not shrink even if etcoShrinkTabtoMin is in Options }
                  etoVisible);       { tab is visible }
  TECTabOptions = set of TECTabOption;
  TECTabCtrlFlag = (ectfCloseBtnDown,     { Close button of any tab is pushed down }
                    ectfDragFolding,      { Dragged tab can be folded }
                    ectfInvPrefSize,      { reduce InvalidatePreferredSize calls }
                    ectfKeepActiveVisi,   { keeps active tab visible, when any tab is added/closed }
                    ectfLockHint,         { lock when changing hint }
                    ectfRealignButtons);  { reduce buttons alinging (less AlignControls and Resize) }
  TECTabCtrlFlags = set of TECTabCtrlFlag;
  TECTabCtrlOption = (etcoAddTabButton,         { Add button is shown }
                      etcoAutoDragTabs,         { automatic mouse dragging }
                      etcoAutoSizeTabsHeight,   { tabs are vertically autosized }
                      etcoAutoSizeTabsWidth,    { tabs are horizontally autosized }
                      etcoCloseBtnActiveOnly,   { Close button is shown only on the active tab }
                      etcoDontEnlargeTopRow,    { top-row (when RowCount>1) is not enlarged }
                      etcoDontShrinkActiveTab,  { active tab is never shrinked }
                      etcoDropDownMenu,         { build-in drop down menu }
                      etcoDropDownFoldingOnly,  { drop-down menu contains item related to folding only }
                      etcoEnlargeTabsToFit,     { tabs are enlarged to fill the whole row; max. is 0.5*Width }
                      etcoFixedPosition,        { tabs cannot be moved to a new position }
                      etcoFoldingPriority,      { folding is prior to moving }
                      etcoKeepOrigPosFoldTab,   { folded tab is unfolded to its original position }
                      etcoLoopTabSelection,     { next tab of the last tab is the first tab and v.v. }
                      etcoMiddleButtonClose,    { middle mouse button closes tab; otherwise moves/folds }
                      etcoNewTabActive,         { tab added by Add btn is activated }
                      etcoReversed,             { tabs are aligned oppositely; this is NOT BiDi-mode! }
                      etcoShrinkTabsToMin);     { tabs are shrinked to TabMinWidth before Left-Right btns appears }
  TECTabCtrlOptions = set of TECTabCtrlOption;
  TECTabHovered = (ethTabCaption, ethDropDownGlyph, ethCloseButton);
  TECTabCtrlMode = (etmLeftRightBtns, etmMultiLine);
  TECTabAdding = (etaFirst, etaLast, etaBeside);
  TECTabClosing = (etcLeftNear, etcRightNear, etcFirst, etcLast, etcPrevious);
  TECTCMaxRows = 1..32767;

  TCloseQueryEvent = procedure(Sender: TObject; AIndex: Integer; var CanClose: Boolean) of object;
  TDrawECTabEvent = procedure(Sender: TObject; AIndex: Integer; Rect: TRect; AActive: Boolean) of object;
  TFoldECTabEvent = procedure(Sender: TObject; AFolded, AOwner: Integer) of object;

  TECTab = class;

  { TECTab }
  TECTab = class(TCollectionItem)
  private
    FColor: TColor;
    FControl: TControl;
    FFoldedTabs: TFPList;
    FFontOptions: TFontOptions;
    FHint: TTranslateString;
    FImageIndex: SmallInt;
    FOptions: TECTabOptions;
    FPopupMenu: TPopupMenu;
    FTag: PtrInt;
    FText: TCaption;
    FWidth: Integer;
    procedure SetColor(AValue: TColor);
    procedure SetImageIndex(AValue: SmallInt);
    procedure SetOptions(AValue: TECTabOptions);
    procedure SetText(AValue: TCaption);
  protected const
    cDefOptions = [etoVisible];
    cDefText = 'Tab';
  protected
    BoundRect: TRect;
    CaptionRect: TRect;
    CloseBtnRect: TRect;
    DropDownRect: TRect;
    Flags: TECTabFlags;
    ImagePoint: TPoint;
    PaintRect: TRect;
    PreferredWidth: SmallInt;
    PreviousID: Integer;
    function GetDisplayName: string; override;
    procedure RecalcRedraw;
    procedure Redraw;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    function IsFolded: Boolean;
    property FoldedTabs: TFPList read FFoldedTabs write FFoldedTabs;
  published
    property Color: TColor read FColor write SetColor default clDefault;
    property Control: TControl read FControl write FControl;
    property FontOptions: TFontOptions read FFontOptions write FFontOptions;
    property Hint: TTranslateString read FHint write FHint;
    property ImageIndex: SmallInt read FImageIndex write SetImageIndex default -1;
    property Options: TECTabOptions read FOptions write SetOptions default cDefOptions;
    property PopupMenu: TPopupMenu read FPopupMenu write FPopupMenu;
    property Tag: PtrInt read FTag write FTag default 0;
    property Text: TCaption read FText write SetText;
    property Width: Integer read FWidth;
  end;

  TECTabIndex = record
    Tab: TECTab;
    Index: Integer;
  end;
  TECTabArray = array of TECTabIndex;

  TCustomECTabCtrl = class;

  { TECTabs }
  TECTabs = class(TCollection)
  private
    function GetItems(Index: Integer): TECTab;
    procedure SetItems(Index: Integer; AValue: TECTab);
  protected
    FECTabCtrl: TCustomECTabCtrl;
    OnClick: TIntegerMethod;
    function GetOwner: TPersistent; override;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AECTabCtrl: TCustomECTabCtrl);
    function Add: TECTab;
    procedure Clear; reintroduce;
    procedure Delete(Index: Integer); reintroduce;
    function IDToIndex(AID: Integer): Integer;
    property Items[Index: Integer]: TECTab read GetItems write SetItems; default;
  end;

  { TCustomECTabCtrl }
  TCustomECTabCtrl = class(TCustomControl)
  private
    FAlignment: TAlignment;
    FBottomSize: SmallInt;
    FColorActiveTab: TColor;
    FColorHighlighted: TColor;
    FHovered: Integer;
    FImages: TCustomImageList;
    FMaxRowCount: TECTCMaxRows;
    FNewTabPosition: TECTabAdding;
    FOnAdding: TIntegerEvent;
    FOnChange: TNotifyEvent;
    FOnChanging: TTabChangingEvent;
    FOnCloseQuery: TCloseQueryEvent;
    FOnCloseTabClicked: TNotifyEvent;
    FOnDrawTab: TDrawECTabEvent;
    FOnFold: TFoldECTabEvent;
    FOptions: TECTabCtrlOptions;
    FPosLeftRight: Integer;
    FPosLRMax: Integer;
    FRowCount: SmallInt;
    FStyle: TObjectStyle;
    FTabActiveRise: Integer;
    FTabClosing: TECTabClosing;
    FTabFixedWidth: Integer;
    FTabHeight: Integer;
    FTabHovered: TECTabHovered;
    FTabIndex: Integer;
    FTabMaxWidth: Integer;
    FTabMinWidth: Integer;
    FTabs: TECTabs;
    FTabPosition: TTabPosition;
    FVisibleTabIndex: Integer;
    FVisiTabs: TECTabArray;
    procedure SetAlignment(AValue: TAlignment);
    procedure SetBottomSize(AValue: SmallInt);
    procedure SetColorActiveTab(AValue: TColor);
    procedure SetColorHighlighted(AValue: TColor);
    procedure SetHovered(AValue: Integer);
    procedure SetImages(AValue: TCustomImageList);
    procedure SetMaxRowCount(AValue: TECTCMaxRows);
    procedure SetOptions(AValue: TECTabCtrlOptions);
    procedure SetPosLeftRight(AValue: Integer);
    procedure SetPosLRMax(AValue: Integer);
    procedure SetStyle(AValue: TObjectStyle);
    procedure SetTabActiveRise(AValue: Integer);
    procedure SetTabFixedWidth(AValue: Integer);
    procedure SetTabHeight(AValue: Integer);
    procedure SetTabHovered(AValue: TECTabHovered);
    procedure SetTabIndex(AValue: Integer);
    procedure SetTabMaxWidth(AValue: Integer);
    procedure SetTabMinWidth(AValue: Integer);
    procedure SetTabs(AValue: TECTabs);
    procedure SetTabPosition(AValue: TTabPosition);
  protected const
    cBottomBevel: SmallInt = 2;
    cCloseBtnBorder: SmallInt = 3;
    cContentIndent: SmallInt = 6;
    cDefMaxRowCount = 1;
    cDefNewTabPos = etaLast;
    cDefOptions = [etcoAutoDragTabs, etcoDontEnlargeTopRow, etcoDropDownMenu, etcoNewTabActive];
    cDefTabActiveRise = 3;
    cDefTabFixedWidth = 84;
    cDefTabClosing = etcLeftNear;
    cDefMaxWidth = 140;
    cDefMinWidth = 40;
    cDefTabHeight = 20;
    cDropDownGlyph: array[TTabPosition] of TGlyphDesign = (egdArrowDown, egdArrowsUD, egdArrowRight, egdArrowLeft);
    cMouseWheelMax: SmallInt = 7;
  protected type
    TTabRow = record
      Width: Integer;
      From: Integer;
      InactRect: TRect;
      Count: SmallInt;
      InactBkgnd: Boolean;
    end;
  protected
    BtnAdd: TCustomECSpeedBtn;
    BtnLeftUp, BtnRightDown: TCustomECSpeedBtn;
    ClickedTabIndex: Integer;
    DefHint: string;
    Flags: TECTabCtrlFlags;
    FontHeight: SmallInt;
    MouseWheelCnt: SmallInt;
    PrefRowWidth: Integer;
    RowIndex: Integer;
    RowWidth: Integer;
    VisiTabsInRow: array of TTabRow;
    class var GlyphAdd, GlyphClose, GlyphCloseInact, GlyphCloseDis, GlyphCloseHigh: TPortableNetworkGraphic;
    class var DropDownMenu: TPopupMenu;
    procedure AlignButtons;
    procedure BtnAddMouseDown(Sender: TObject; Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure BtnLeftUpRepeat(Sender: TObject);
    procedure BtnLeftUpMouseUp(Sender: TObject; {%H-}Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure BtnRightDownRepeat(Sender: TObject);
    procedure BtnRightDownMouseUp(Sender: TObject; {%H-}Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure CalcHoveredTab(X, Y: Integer);
    procedure CalcLayoutAndTabSizes;
    procedure Calculate;
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: Integer;
                                     {%H-}WithThemeSpace: Boolean); override;
    procedure CalcVisibleTabsAnFlags;
    function CalcVisiTabsInRow(ARow, AFrom, ARowWidth: Integer; ALRBtns: Boolean): Boolean;
    function CalcVisiTabsInRows(ARowWidth: Integer; ALRBtns: Boolean; var arows: smallint): Boolean;
    procedure ChangeHint(ATabIndex: Integer);
    procedure CMBiDiModeChanged(var {%H-}Message: TLMessage); message CM_BIDIMODECHANGED;
    procedure DeleteFromFoldedTabs(AFolder, ATabID: Integer);
    procedure DesignLeftRightBtns;
    function DialogChar(var Message: TLMKey): Boolean; override;
    procedure DoBtnsEnabled;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    procedure DoSwitchFoldedTab(AIndex, AFolder: Integer);
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;
    procedure DropDownMenuClose(Sender: TObject);
    class destructor FinalizeClass;
    procedure FontChanged(Sender: TObject); override;
    function GetButtonsWidth: SmallInt;
    class function GetControlClassDefaultSize: TSize; override;
    class constructor InitializeClass;
    function IsSameVisiRow(AVisiIndex, BVisiIndex: Integer): Boolean;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MIAddTab(Sender: TObject);
    procedure MICloseAllClick(Sender: TObject);
    procedure MICloseFoldedTabClick(Sender: TObject);
    procedure MICloseTabClick(Sender: TObject);
    procedure MIFoldedTabClick(Sender: TObject);
    procedure MIFoldToClick(Sender: TObject);
    procedure MIMoveLeftmostClick(Sender: TObject);
    procedure MIMoveLeftClick(Sender: TObject);
    procedure MIMoveRightClick(Sender: TObject);
    procedure MIMoveRightmostClick(Sender: TObject);
    procedure MIUnfoldClick(Sender: TObject);
    procedure MIUnfoldAllClick(Sender: TObject);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure RecalcInvalidate;
    procedure Redraw;
    procedure SetHint(const Value: TTranslateString); override;
    procedure ShowDropDownMenu(AIndex: Integer);
    procedure SwitchFoldedTab(AIndex, AFolder: Integer);
    procedure TabChanged(ARecalculate: Boolean);
    procedure UnfoldAllTabs(AFolder: Integer);
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
    property Hovered: Integer read FHovered write SetHovered;
    property PosLeftRight: Integer read FPosLeftRight write SetPosLeftRight;
    property PosLRMax: Integer read FPosLRMax write SetPosLRMax;
    property TabHovered: TECTabHovered read FTabHovered write SetTabHovered;
  public
    UpdateCount: Integer;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure ActivateTab(AIndex: Integer);
    function AddTab(APosition: TECTabAdding; AActivate: Boolean): TECTab;
    procedure BeginUpdate;
    procedure CloseAllFoldedTabs(AFolder: Integer);
    procedure DeleteTab(AIndex: Integer);
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    procedure EndUpdate;
    function FindFolderOfTab(AIndex: Integer): Integer;
    procedure FoldTab(AIndex, AFolder: Integer);
    function IndexOfTabAt(X, Y: Integer): Integer;
    function IsTabVisible(AIndex: Integer): Boolean;
    procedure MakeTabAvailable(AIndex: Integer; AActivate: Boolean);
    procedure MoveNext(AKeepVisible: Boolean = True);
    procedure MovePrevious(AKeepVisible: Boolean = True);
    procedure MoveTab(AFrom, ATo: Integer);
    procedure ScrollLeft;
    procedure ScrollRight;
    procedure SelectNext;
    procedure SelectPrevious;
    procedure SelectRowDown;
    procedure SelectRowUp;
    procedure UnfoldTab(AID: Integer; AFolder: Integer=-1);
  public
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property BottomSize: SmallInt read FBottomSize write SetBottomSize default 0;
    property ColorActiveTab: TColor read FColorActiveTab write SetColorActiveTab default clDefault;
    property ColorHighlighted: TColor read FColorHighlighted write SetColorHighlighted default clDefault;
    property Highlighted: Integer read FHovered;
    property Images: TCustomImageList read FImages write SetImages;
    property MaxRowCount: TECTCMaxRows read FMaxRowCount write SetMaxRowCount default cDefMaxRowCount;
    property NewTabPosition: TECTabAdding read FNewTabPosition write FNewTabPosition default cDefNewTabPos;
    property Options: TECTabCtrlOptions read FOptions write SetOptions default cDefOptions;
    property RowCount: SmallInt read FRowCount;
    property Style: TObjectStyle read FStyle write SetStyle default eosButton;
    property TabActiveRise: Integer read FTabActiveRise write SetTabActiveRise default cDefTabActiveRise;
    property TabClosing: TECTabClosing read FTabClosing write FTabClosing default cDefTabClosing;
    property TabFixedWidth: Integer read FTabFixedWidth write SetTabFixedWidth default cDefTabFixedWidth;
    property TabHeight: Integer read FTabHeight write SetTabHeight default cDefTabHeight;
    property TabIndex: Integer read FTabIndex write SetTabIndex default -1;
    property TabMaxWidth: Integer read FTabMaxWidth write SetTabMaxWidth default cDefMaxWidth;
    property TabMinWidth: Integer read FTabMinWidth write SetTabMinWidth default cDefMinWidth;
    property TabPosition: TTabPosition read FTabPosition write SetTabPosition default tpTop;
    property Tabs: TECTabs read FTabs write SetTabs;
    property VisibleTabIndex: Integer read FVisibleTabIndex;
    property VisibleTabs: TECTabArray read FVisiTabs;
  public
    property OnAdd: TIntegerEvent read FOnAdding write FOnAdding;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TTabChangingEvent read FOnChanging write FOnChanging;
    property OnCloseQuery: TCloseQueryEvent read FOnCloseQuery write FOnCloseQuery;
    property OnCloseTabClicked: TNotifyEvent read FOnCloseTabClicked write FOnCloseTabClicked;
    property OnDrawTab: TDrawECTabEvent read FOnDrawTab write FOnDrawTab;
    property OnFold: TFoldECTabEvent read FOnFold write FOnFold;
  end;

  { TCECTabCtrl }
  TECTabCtrl = class(TCustomECTabCtrl)
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize default True;
    property BiDiMode;
    property BorderSpacing;
    property BottomSize;
    {property Color;}  {does nothing ATM}
    property ColorActiveTab;
    property ColorHighlighted;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Images;
    property MaxRowCount;
    property NewTabPosition;
    property OnAdd;
    property OnChange;
    property OnChangeBounds;
    property OnChanging;
    property OnCloseQuery;
    property OnCloseTabClicked;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawTab;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnFold;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    property Options;
    property ParentBiDiMode;
    {property ParentColor;}  {does nothing ATM}
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RowCount;
    property ShowHint;
    property Style;
    property TabActiveRise;
    property TabClosing;
    property TabFixedWidth;
    property TabHeight;
    property TabIndex;
    property TabMaxWidth;
    property TabMinWidth;
    property TabOrder;
    property TabPosition;
    property TabStop default True;
    property Tabs;
    property Visible;
  end;

const cECNoTabHovered: SmallInt = -1;
      cECNoBotomBevel: SmallInt = -2;

implementation

resourcestring
  rsETCAdd = 'Add a New Tab';
  rsETCBottmomost = 'Bottommost';
  rsETCClose = 'Close';
  rsETCCloseAll = 'Close All';
  rsETCCloseTab = 'Close Tab';
  rsETCDown = 'Down';
  rsETCFoldTo = 'Fold to';
  rsETCLeft = 'Left';
  rsETCLeftmost = 'Leftmost';
  rsETCMoveTab = 'Move Tab';
  rsETCRight = 'Right';
  rsETCRightmost = 'Rightmost';
  rsETCTopmost = 'Topmost';
  rsETCUnfold = 'Unfold Tab';
  rsETCUnfoldAll = 'Unfold All';
  rsETCUp = 'Up';

{ TECTab }

constructor TECTab.Create(ACollection: TCollection);
begin
  FColor:=clDefault;
  Flags:=[etfParentFont];
  FFoldedTabs:=TFPList.Create;
  FFontOptions:=TFontOptions.Create(nil);
  with FFontOptions do
    begin
      if assigned(ACollection) then
        begin
          FontSize:=TECTabs(ACollection).FECTabCtrl.Font.Size;
          FontStyles:=TECTabs(ACollection).FECTabCtrl.Font.Style;
        end else
        begin
          FontSize:=0;
          FontStyles:=[];
        end;
      OnRecalcRedraw:=@RecalcRedraw;
      OnRedraw:=@Redraw;
    end;
  FImageIndex:=-1;
  FOptions:=cDefOptions;
  PreviousID:=-1;
  inherited Create(ACollection);
end;

destructor TECTab.Destroy;
begin
  FreeAndNil(FFoldedTabs);
  FreeAndNil(FFontOptions);
  inherited Destroy;
end;

function TECTab.IsFolded: Boolean;
begin
  Result:= (etfFolded in Flags);
end;

function TECTab.GetDisplayName: string;
begin
  Result:=Text;
  if Result='' then Result:=cDefText+inttostr(ID);
end;

procedure TECTab.RecalcRedraw;
begin
  Changed(True);
end;

procedure TECTab.Redraw;
begin
  Changed(False);
end;

{ TECTab.Setters }

procedure TECTab.SetColor(AValue: TColor);
begin
  if FColor=AValue then exit;
  FColor:=AValue;
  if (etoVisible in Options) and not (etfFolded in Flags) then Changed(True);
end;

procedure TECTab.SetImageIndex(AValue: SmallInt);
begin
  if FImageIndex=AValue then exit;
  FImageIndex:=AValue;
  if (etoVisible in Options) and not (etfFolded in Flags) then Changed(True);
end;

procedure TECTab.SetOptions(AValue: TECTabOptions);
const cRecalcOpts = [etoCloseable, etoCloseBtn, etoDontShrink, etoVisible];
var bRecalc: Boolean;
begin
  bRecalc:= ((cRecalcOpts*FOptions)<>(cRecalcOpts*AValue));
  if FOptions=AValue then exit;
  FOptions:=AValue;
  Changed(bRecalc);
end;

procedure TECTab.SetText(AValue: TCaption);
begin
  if FText=AValue then exit;
  FText:=AValue;
  if (etoVisible in Options) and not (etfFolded in Flags) then Changed(True);
end;

{ TECTabs }

constructor TECTabs.Create(AECTabCtrl: TCustomECTabCtrl);
begin
  inherited Create(TECTab);
  FECTabCtrl:=AECTabCtrl;
end;

function TECTabs.Add: TECTab;
begin
  Result:=TECTab(inherited Add);
end;

procedure TECTabs.Clear;
begin
  FECTabCtrl.TabIndex:=-1;
  inherited Clear;
end;

procedure TECTabs.Delete(Index: Integer);
begin
  FECTabCtrl.DeleteTab(Index);
end;

function TECTabs.GetOwner: TPersistent;
begin
  Result:=FECTabCtrl;
end;

function TECTabs.IDToIndex(AID: Integer): Integer;
var i: Integer;
begin
  Result:=-1;
  for i:=0 to Count-1 do
    if Items[i].ID=AID then
      begin
        Result:=i;
        break;
      end;
end;

procedure TECTabs.Notify(Item: TCollectionItem; Action: TCollectionNotification);
begin
  inherited Notify(Item, Action);
  if Action=cnAdded then
    if not (csLoading in (Owner as TCustomECTabCtrl).ComponentState)
      then TECTab(Item).FText:=TECTab.cDefText+inttostr(Item.ID);
end;

procedure TECTabs.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  FECTabCtrl.TabChanged(Item=nil);
end;

{ TECTabs.G/Setters }

function TECTabs.GetItems(Index: Integer): TECTab;
begin
  Result:=TECTab(inherited Items[Index]);
end;

procedure TECTabs.SetItems(Index: Integer; AValue: TECTab);
begin
  Items[Index].Assign(AValue);
end;

{ TCustomECTabCtrl }

constructor TCustomECTabCtrl.Create(TheOwner: TComponent);
const cDelay: SmallInt = 500;
      cRepeating: SmallInt = 125;
begin
  inherited Create(TheOwner);
  FCompStyle:=csNotebook;
  AutoSize:=True;
  TabStop:=True;
  ControlStyle:=ControlStyle+[csAcceptsControls, csParentBackground, csReplicatable]+csMultiClicks
                            -[csCaptureMouse, csNoFocus, csOpaque, csSetCaption];
  FColorActiveTab:=clDefault;
  FColorHighlighted:=clDefault;
  FHovered:=cECNoTabHovered;
  FMaxRowCount:=cDefMaxRowCount;
  SetLength(VisiTabsInRow, cDefMaxRowCount);
  VisiTabsInRow[0].From:=0;
  VisiTabsInRow[0].Count:=0;
  ClickedTabIndex:=-1;
  FNewTabPosition:=cDefNewTabPos;
  FOptions:=cDefOptions;
  FTabActiveRise:=cDefTabActiveRise;
  FTabFixedWidth:=cDefTabFixedWidth;
  FTabHeight:=cDefTabHeight;
  FTabIndex:=-1;
  FTabMaxWidth:=cDefMaxWidth;
  FTabMinWidth:=cDefMinWidth;
  FTabs:=TECTabs.Create(self);
  BtnLeftUp:=TCustomECSpeedBtn.Create(self);
  BtnRightDown:=TCustomECSpeedBtn.Create(self);
  DesignLeftRightBtns;
  with BtnLeftUp do
    begin
      ControlStyle:=ControlStyle+[csNoDesignVisible];
      OnMouseUp:=@BtnLeftUpMouseUp;
      OnRepeating:=@BtnLeftUpRepeat;
      Delay:=cDelay;
      Repeating:=cRepeating;
      Transparent:=False;
      Parent:=self;
    end;
  with BtnRightDown do
    begin
      ControlStyle:=ControlStyle+[csNoDesignVisible];
      OnMouseUp:=@BtnRightDownMouseUp;
      OnRepeating:=@BtnRightDownRepeat;
      Delay:=cDelay;
      Repeating:=cRepeating;
      Transparent:=False;
      Parent:=self;
    end;
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, cx, cy);
  if not assigned(DropDownMenu) then DropDownMenu:=TPopupMenu.Create(nil);
  AccessibleRole:=larTabControl;
  AccessibleDescription:=rsTCustomTabControlAccessibilityDescription;
end;

destructor TCustomECTabCtrl.Destroy;
begin
  FreeAndNil(FTabs);
  FreeAndNil(BtnLeftUp);
  FreeAndNil(BtnRightDown);
  inherited Destroy;
end;

procedure TCustomECTabCtrl.ActivateTab(AIndex: Integer);
begin
  if (AIndex<Tabs.Count) and (etoVisible in Tabs[AIndex].Options) then
    begin
      if (AIndex<0) or not (etfFolded in Tabs[AIndex].Flags)
        then TabIndex:=AIndex
        else SwitchFoldedTab(AIndex, FindFolderOfTab(AIndex));
    end;
end;

function TCustomECTabCtrl.AddTab(APosition: TECTabAdding; AActivate: Boolean): TECTab;
begin
  BeginUpdate;
  case APosition of
    etaFirst: Result:=TECTab(Tabs.Insert(0));
    etaLast: Result:=Tabs.Add;
    etaBeside: Result:=TECTab(Tabs.Insert(TabIndex+1));
  end;
  if AActivate then
    begin
      include(Flags, ectfKeepActiveVisi);
      case APosition of
        etaFirst: TabIndex:=0;
        etaLast: TabIndex:=Tabs.Count-1;
        etaBeside: TabIndex:=TabIndex+1;
      end;
    end;
  EndUpdate;
  if assigned(OnAdd) then OnAdd(self, Tabs.IDToIndex(Result.ID));
end;

procedure TCustomECTabCtrl.AlignButtons;

  function GetBottomSpacing: SmallInt;
  begin
    Result:=cBottomBevel+BottomSize+1;
  end;

var aRowCount, aWidth, aHeight: SmallInt;
    aTabPos: TTabPosition;
    bRev: Boolean;
const
    cHorAnchors: array[Boolean] of TAnchorKind = (akBottom, akTop);
    cVerAnchors: array[Boolean] of TAnchorKind = (akRight, akLeft);
begin
  if ectfRealignButtons in Flags then
    begin
      aTabPos:=TabPosition;
      aWidth:=GetButtonsWidth;
      aHeight:=aWidth;
      aRowCount:=RowCount;
      if (aRowCount=1) and (TabActiveRise=0) then dec(aHeight);
      bRev:= (etcoReversed in Options);
      if aTabPos in [tpTop, tpBottom] then bRev:= (bRev xor IsRightToLeft);
      if BtnLeftUp.Visible then
        begin
          BtnRightDown.Anchors:=[];
          BtnRightDown.BorderSpacing.Top:=0;
          BtnRightDown.BorderSpacing.Bottom:=0;
          BtnRightDown.BorderSpacing.Left:=0;
          BtnRightDown.BorderSpacing.Right:=0;
          if aRowCount>=2 then dec(aHeight, TabActiveRise div 2 +1);
          if aTabPos in [tpTop, tpBottom] then
            begin  { tpTop, tpBottom }
              BtnRightDown.AnchorParallel(cVerAnchors[bRev], 0, self);
              BtnRightDown.AnchorParallel(cHorAnchors[aTabPos=tpBottom], GetBottomSpacing, self);
              BtnRightDown.Height:=aHeight;
              BtnRightDown.Width:=aWidth;
              if aRowCount=1 then
                begin
                  BtnLeftUp.AnchorToCompanion(cVerAnchors[bRev], 0, BtnRightDown);
                  BtnLeftUp.Width:=aWidth;
                end else
                begin
                  BtnLeftUp.AnchorToCompanion(cHorAnchors[aTabPos=tpBottom], 0, BtnRightDown);
                  BtnLeftUp.Height:=BtnRightDown.Height;
                end;
              if etcoAddTabButton in Options then
                if aRowCount=1 then
                  begin
                    BtnAdd.Width:=aWidth;
                    BtnAdd.Anchors:=[akTop, akBottom];
                    BtnAdd.AnchorToCompanion(cVerAnchors[bRev], 0, BtnLeftUp);
                  end else
                  begin
                    if aRowCount=2 then
                      begin
                        BtnAdd.Width:=aWidth;
                        BtnAdd.AnchorToCompanion(cVerAnchors[bRev], 0, BtnLeftUp);
                      end else
                      begin
                        BtnAdd.Height:=aHeight+1;
                        BtnAdd.AnchorToCompanion(cHorAnchors[aTabPos=tpBottom], 0, BtnLeftUp);
                      end;
                    BtnLeftUp.BorderSpacing.Top:=0;
                    BtnLeftUp.BorderSpacing.Bottom:=0;
                  end;
            end else
            begin  { tpLeft, tpRight }
              BtnRightDown.AnchorParallel(cHorAnchors[bRev], 0, self);
              BtnRightDown.Height:=aWidth;
              BtnRightDown.AnchorParallel(cVerAnchors[aTabPos=tpRight], GetBottomSpacing, self);
              BtnRightDown.Width:=aHeight;
              if aRowCount=1 then
                begin
                  BtnLeftUp.AnchorToCompanion(cHorAnchors[bRev], 0, BtnRightDown);
                  BtnLeftUp.Height:=aWidth;
                end else
                begin
                  BtnLeftUp.Width:=BtnRightDown.Width;
                  BtnLeftUp.AnchorToCompanion(cVerAnchors[aTabPos=tpRight], 0, BtnRightDown);
                end;
              if etcoAddTabButton in Options then
                if aRowCount=1 then
                  begin
                    BtnAdd.Height:=aWidth;
                    BtnAdd.Anchors:=[akLeft, akRight];
                    BtnAdd.AnchorToCompanion(cHorAnchors[bRev], 0, BtnLeftUp);
                  end else
                  begin
                    if aRowCount=2 then
                      begin
                        BtnAdd.Height:=aWidth;
                        BtnAdd.AnchorToCompanion(cHorAnchors[bRev], 0, BtnLeftUp);
                      end else
                      begin
                        BtnAdd.Width:=aHeight+1;
                        BtnAdd.AnchorToCompanion(cVerAnchors[aTabPos=tpRight], 0, BtnLeftUp);
                      end;
                    BtnLeftUp.BorderSpacing.Left:=0;
                    BtnLeftUp.BorderSpacing.Right:=0;
                  end;
            end;
        end else
        begin  { L/R buttons not visible }
          if etcoAddTabButton in Options then
            begin
              BtnAdd.Anchors:=[];
              if aTabPos in [tpTop, tpBottom] then
                begin  { tpTop, tpBottom }
                  BtnAdd.Width:=aWidth;
                  BtnAdd.AnchorParallel(cVerAnchors[bRev], 0, self);
                  BtnAdd.Height:=aHeight;
                  BtnAdd.AnchorParallel(cHorAnchors[aTabPos=tpBottom], GetBottomSpacing, self);
                end else
                begin  { tpLeft, tpRight }
                  BtnAdd.Height:=aWidth;
                  BtnAdd.AnchorParallel(cHorAnchors[bRev], 0, self);
                  BtnAdd.Width:=aHeight;
                  BtnAdd.AnchorParallel(cVerAnchors[aTabPos=tpRight], GetBottomSpacing, self);
                end;
            end;
        end;
      exclude(Flags, ectfRealignButtons);
    end;
end;

procedure TCustomECTabCtrl.BeginUpdate;
begin
  inc(UpdateCount);
end;

procedure TCustomECTabCtrl.BtnAddMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button=mbLeft then AddTab(NewTabPosition, etcoNewTabActive in Options);
end;

procedure TCustomECTabCtrl.BtnLeftUpRepeat(Sender: TObject);
begin
  ScrollLeft;
end;

procedure TCustomECTabCtrl.BtnLeftUpMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  case Button of
    mbLeft: ScrollLeft;
    mbMiddle: PosLeftRight:=0;
  end;
end;

procedure TCustomECTabCtrl.BtnRightDownRepeat(Sender: TObject);
begin
  ScrollRight;
end;

procedure TCustomECTabCtrl.BtnRightDownMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  case Button of
    mbLeft: ScrollRight;
    mbMiddle: PosLeftRight:=PosLRMax;
  end;
end;

procedure TCustomECTabCtrl.CalcHoveredTab(X, Y: Integer);
var aHovered: Integer;
begin
  aHovered:=IndexOfTabAt(X, Y);
  if aHovered>=0 then
    begin
      if (etfHasCloseBtn in Tabs[aHovered].Flags) and PtInRect(Tabs[aHovered].CloseBtnRect, Point(X, Y))
        then TabHovered:=ethCloseButton
        else
        begin
          exclude(Flags, ectfCloseBtnDown);
          if (Tabs[aHovered].FoldedTabs.Count>0) and (((aHovered=TabIndex) and
            not (etcoAutoDragTabs in Options)) or PtInRect(Tabs[aHovered].DropDownRect, Point(X, Y)))
            then TabHovered:=ethDropDownGlyph
            else TabHovered:=ethTabCaption;
        end;
    end else
    begin
      exclude(Flags, ectfCloseBtnDown);
      TabHovered:=ethTabCaption;
    end;
  Hovered:=aHovered;
end;

procedure TCustomECTabCtrl.CalcLayoutAndTabSizes;
var aRows: SmallInt;
    aRowWidth: Integer;

  procedure AdjustRowWidth;
  var aBtnWidth: SmallInt;
  begin
    aBtnWidth:=GetButtonsWidth;
    if aRows=1 then
      begin
        dec(aRowWidth, 2*aBtnWidth);
        if not (etcoAddTabButton in Options) then dec(aRowWidth, 2);
      end else
      begin
        if not (etcoAddTabButton in Options)
          then dec(aRowWidth, aBtnWidth+2)
          else if (aRows=2) then dec(aRowWidth, aBtnWidth);
      end;
    RowWidth:=aRowWidth;
  end;

  procedure EnlargeTabsInRow(ARowWidth: Integer; ARow: SmallInt);
  var i, aFrom, aMaxTabWidth, aRealTabWidth, aTotalWidth: Integer;
      aDiff, aTabWidth, aRatio: Single;
  begin
    aMaxTabWidth:=ARowWidth div 2;
    aTotalWidth:=0;
    aFrom:=VisiTabsInRow[ARow].From;
    for i:=aFrom to aFrom+VisiTabsInRow[ARow].Count-1 do
      inc(aTotalWidth, VisibleTabs[i].Tab.PreferredWidth);
    aDiff:=0;
    aRatio:=ARowWidth/aTotalWidth;
    aTotalWidth:=0;
    for i:=aFrom to aFrom+VisiTabsInRow[ARow].Count-1 do
      begin
        aTabWidth:=VisibleTabs[i].Tab.PreferredWidth*ARatio+aDiff;
        aRealTabWidth:=Math.min(round(aTabWidth), aMaxTabWidth);
        VisibleTabs[i].Tab.FWidth:=aRealTabWidth;
        inc(aTotalWidth, aRealTabWidth);
        if aMaxTabWidth<>aRealTabWidth then aDiff:=aTabWidth-aRealTabWidth;
      end;
    VisiTabsInRow[ARow].Width:=aTotalWidth;
  end;

  procedure EnlargeTabs(ARowWidth: Integer);
  var i, aRow, aMax: SmallInt;
  begin
    aMax:=aRows-1;
    if (aMax>0) and (etcoDontEnlargeTopRow in Options) then
      begin
        for i:=VisiTabsInRow[aMax].From to VisiTabsInRow[aMax].From+VisiTabsInRow[aMax].Count-1 do
          VisibleTabs[i].Tab.FWidth:=VisibleTabs[i].Tab.PreferredWidth;
        dec(aMax);
      end;
    for aRow:=0 to aMax do
      EnlargeTabsInRow(ARowWidth, aRow);
  end;

  procedure RearrangeVisibleTabs;
  var i, aHelp: Integer;
  begin
    aHelp:=length(VisibleTabs) div aRows;
    for i:=aRows-1 downto 0 do
      begin
        VisiTabsInRow[i].Count:=aHelp;
        VisiTabsInRow[i].From:=length(VisibleTabs)-(aRows-i)*aHelp;
      end;
    aHelp:=VisiTabsInRow[0].From;
    for i:=0 to aHelp-1 do
      begin
        dec(VisiTabsInRow[i].From, aHelp-i);
        inc(VisiTabsInRow[i].Count);
      end;
  end;

  function ShrinkTabsInRow(ARow: SmallInt): Boolean;
  var i, aFrom, aPrefWidth, aRowWidthHelp: Integer;
      aDiff, aTabWidth, aRatio: Single;
  begin
    aPrefWidth:=0;
    aFrom:=VisiTabsInRow[ARow].From;
    aRowWidthHelp:=aRowWidth;
    for i:=aFrom to aFrom+VisiTabsInRow[ARow].Count-1 do
      if not (etoDontShrink in VisibleTabs[i].Tab.Options) and
        (not (etcoDontShrinkActiveTab in Options) or (i<>VisibleTabIndex))
        then inc(aPrefWidth, VisibleTabs[i].Tab.PreferredWidth)
        else dec(aRowWidthHelp, VisibleTabs[i].Tab.PreferredWidth);
    aDiff:=0;
    Result:=False;
    if aPrefWidth>aRowWidth then
      begin
        aRatio:=aRowWidthHelp/aPrefWidth;
        aPrefWidth:=0;
        for i:=aFrom to aFrom+VisiTabsInRow[ARow].Count-1 do
          if not (etoDontShrink in VisibleTabs[i].Tab.Options) and
            (not (etcoDontShrinkActiveTab in Options) or (i<>VisibleTabIndex)) then
            begin
              aTabWidth:=VisibleTabs[i].Tab.PreferredWidth*ARatio+aDiff;
              VisibleTabs[i].Tab.FWidth:=Math.max(round(aTabWidth), TabMinWidth);
              inc(aPrefWidth, VisibleTabs[i].Tab.FWidth);
              aDiff:=aTabWidth-round(aTabWidth);
            end else
            begin
              VisibleTabs[i].Tab.FWidth:=VisibleTabs[i].Tab.PreferredWidth;
              inc(aPrefWidth, VisibleTabs[i].Tab.FWidth);
            end;
        VisiTabsInRow[ARow].Width:=aPrefWidth;
        if aPrefWidth>aRowWidth then Result:=True;
      end else
      for i:=aFrom to aFrom+VisiTabsInRow[ARow].Count-1 do
        VisibleTabs[i].Tab.FWidth:=VisibleTabs[i].Tab.PreferredWidth;
  end;

var i, aHelpSize, aPrefWidth: Integer;
    aSize: TSize;
    bLRBtns: Boolean;
begin
  CalcVisibleTabsAnFlags;
  if ([etcoAutoSizeTabsHeight, etcoAutoSizeTabsWidth]*Options)<>[] then
    begin
      for i:=0 to length(VisibleTabs)-1 do
        begin
          if etfParentFont in VisibleTabs[i].Tab.Flags then
            begin
              Canvas.Font.Size:=Font.Size;
              Canvas.Font.Style:=Font.Style;
            end else
            begin
              Canvas.Font.Size:=VisibleTabs[i].Tab.FontOptions.FontSize;
              Canvas.Font.Style:=VisibleTabs[i].Tab.FontOptions.FontStyles;
            end;
          aSize:=Canvas.TextExtent(VisibleTabs[i].Tab.Text);
          VisibleTabs[i].Tab.CaptionRect:=Rect(0, 0, aSize.cx, aSize.cy);
        end;
    end;
  if etcoAutoSizeTabsHeight in Options then
    begin
      aHelpSize:=0;
      for i:=0 to length(VisibleTabs)-1 do
        aHelpSize:=Math.max(aHelpSize, VisibleTabs[i].Tab.CaptionRect.Bottom);
      if assigned(Images) then aHelpSize:=Math.max(aHelpSize, Images.Height);
      i:=aHelpSize+cContentIndent;
      if i<>FTabHeight then include(Flags, ectfRealignButtons);
      FTabHeight:=i;
    end;
  if etcoAutoSizeTabsWidth in Options then
    begin
      aPrefWidth:=0;
      for i:=0 to length(VisibleTabs)-1 do
        begin
          aHelpSize:=2*cContentIndent;
          inc(aHelpSize, VisibleTabs[i].Tab.CaptionRect.Right);
          if etfHasImage in VisibleTabs[i].Tab.Flags then inc(aHelpSize, Images.Width+cContentIndent);
          if VisibleTabs[i].Tab.FoldedTabs.Count>0 then
            inc(aHelpSize, Canvas.GlyphExtent(cDropDownGlyph[TabPosition]).cx+2*cContentIndent);
          if etfHasCloseBtn in VisibleTabs[i].Tab.Flags then inc(aHelpSize, GlyphClose.Width+2*cCloseBtnBorder);
          aHelpSize:=Math.max(Math.min(TabMaxWidth, aHelpSize), TabMinWidth);
          VisibleTabs[i].Tab.PreferredWidth:=aHelpSize;
          inc(aPrefWidth, aHelpSize);
        end;
    end else
    begin
      aHelpSize:=TabFixedWidth;
      for i:=0 to length(VisibleTabs)-1 do
        VisibleTabs[i].Tab.PreferredWidth:=aHelpSize;
      aPrefWidth:=length(VisibleTabs)*aHelpSize;
    end;
  if TabPosition in [tpTop, tpBottom]
    then aRowWidth:=Width
    else aRowWidth:=Height;
  if etcoAddTabButton in Options then dec(aRowWidth, GetButtonsWidth+2);
  aRows:=1;
  bLRBtns:=False;
  if aPrefWidth>0 then
    begin
      if aPrefWidth>aRowWidth then
        begin  { tabs don't fit to single row; shrink or multiline needeed }
          if not (etcoShrinkTabsToMin in Options) then
            begin  { shrink not possible }
              bLRBtns:=CalcVisiTabsInRows(aRowWidth, False, aRows);
              if not bLRBtns then
                begin  { tabs fit }
                  if not (etcoEnlargeTabsToFit in Options)
                    then for i:=0 to length(VisibleTabs)-1 do  { do NOT enlarge }
                           VisibleTabs[i].Tab.FWidth:=VisibleTabs[i].Tab.PreferredWidth
                    else
                    begin  { rearrange tabs and enlarge }
                      RearrangeVisibleTabs;
                      EnlargeTabs(aRowWidth);
                    end;
                end else
                begin  { tabs don't fit, Left-Right btns needed }
                  aRows:=MaxRowCount;
                  AdjustRowWidth;
                  i:=aPrefWidth div aRows;
                  CalcVisiTabsInRows(i, bLRBtns, aRows);
                  PrefRowWidth:=i;
                  if not (etcoEnlargeTabsToFit in Options)
                    then for i:=0 to length(VisibleTabs)-1 do  { do NOT enlarge }
                           VisibleTabs[i].Tab.FWidth:=VisibleTabs[i].Tab.PreferredWidth
                    else
                    begin  { enlarge tabs to the widest row }
                      aHelpSize:=0;
                      for i:=0 to aRows-1 do
                        if VisiTabsInRow[i].Width>aHelpSize then aHelpSize:=VisiTabsInRow[i].Width;
                      RearrangeVisibleTabs;
                      EnlargeTabs(aHelpSize);
                    end;
                end;
            end else
            begin  { shrink possible }
              bLRBtns:=CalcVisiTabsInRows(aRowWidth, False, aRows);
              if not bLRBtns then
                begin  { tabs fit }
                  if not (etcoEnlargeTabsToFit in Options)
                    then for i:=0 to length(VisibleTabs)-1 do  { do NOT enlarge }
                           VisibleTabs[i].Tab.FWidth:=VisibleTabs[i].Tab.PreferredWidth
                    else
                    begin  { rearrange tabs and enlarge }
                      RearrangeVisibleTabs;
                      EnlargeTabs(aRowWidth);
                    end;
                end else
                begin  { tabs do NOT fit, attempt to shrink }
                  aRows:=MaxRowCount;
                  PrefRowWidth:=aPrefWidth div aRows;
                  RearrangeVisibleTabs;
                  {$BOOLEVAL ON}
                  bLRBtns:=False;
                  for i:=0 to aRows-1 do
                    bLRBtns:= bLRBtns or ShrinkTabsInRow(i);
                  {$BOOLEVAL OFF}
                  if bLRBtns then AdjustRowWidth;
                end;
            end;
        end else
        begin  { tabs fit to single row }
          CalcVisiTabsInRow(0, 0, aRowWidth, False);
          if not (etcoEnlargeTabsToFit in Options)
            then for i:=0 to length(VisibleTabs)-1 do  { tabs have their preffered width }
                   VisibleTabs[i].Tab.FWidth:=VisibleTabs[i].Tab.PreferredWidth
            else EnlargeTabsInRow(aRowWidth, 0);  { enlarge tabs }
        end;
    end else
    VisiTabsInRow[0].Count:=0;
  if FRowCount<>aRows then
    begin
      include(Flags, ectfRealignButtons);
      FRowCount:=aRows;
    end;
  RowWidth:=aRowWidth;
  if bLRBtns then
    begin  { calc. PosLRMax }
      if aRows=1 then
        begin
          VisiTabsInRow[0].From:=0;
          VisiTabsInRow[0].Count:=length(VisibleTabs);
        end;
      aHelpSize:=0;
      for i:=VisiTabsInRow[0].From+VisiTabsInRow[0].Count-1 downto 0 do
        begin
          inc(aHelpSize, VisibleTabs[i].Tab.Width);
          if aHelpSize>=aRowWidth then
            begin
              PosLRMax:=i+1;
              break;
            end;
        end;
    end else
    PosLRMax:=0;
  if bLRBtns<>BtnLeftUp.Visible then
    begin
      if bLRBtns then include(Flags, ectfRealignButtons);
      BtnLeftUp.Visible:=bLRBtns;
      BtnRightDown.Visible:=bLRBtns;
    end;
end;

procedure TCustomECTabCtrl.Calculate;
var aDDGlyphSize: SmallInt;
    aRect: TRect;
    bR2L, bReversed: Boolean;
const caOverlay: array[TObjectStyle] of SmallInt = (4, 1, 3);
      caSideLap: array[TObjectStyle] of SmallInt = (6, 3, 4);
      caBottomLap: array[TObjectStyle] of SmallInt = (8, 3, 5);

  procedure CalcContentHor(AIndex: Integer; AHasSideLap: Boolean);
  var aTab: TECTab;
      bTop: Boolean;
      y: SmallInt;
  begin
    bTop:= (TabPosition=tpTop);
    aTab:=VisibleTabs[AIndex].Tab;
    if AIndex=VisibleTabIndex then
      if bTop
        then dec(aRect.Top, TabActiveRise)
        else inc(aRect.Bottom, TabActiveRise);
    aTab.BoundRect:=aRect;
    aTab.PaintRect:=aRect;
    if bTop
      then inc(aTab.PaintRect.Bottom, caBottomLap[Style])
      else dec(aTab.PaintRect.Top, caBottomLap[Style]);
    if (AIndex<>VisibleTabIndex) and AHasSideLap then
      if not (bReversed xor bR2L)
        then inc(aTab.PaintRect.Right, caSideLap[Style])
        else
        begin
          dec(aTab.PaintRect.Left, caSideLap[Style]);
          inc(aRect.Left, caOverlay[Style]-1);
        end;
    if bTop then inc(aRect.Top);
    inc(aRect.Left, cContentIndent);
    dec(aRect.Right, cContentIndent);
    if not bR2L then
      begin
        if etfHasImage in aTab.Flags then
          begin
            aTab.ImagePoint:=Point(aRect.Left, (aRect.Top+aRect.Bottom-Images.Height) div 2);
            inc(aRect.Left, Images.Width+cContentIndent);
          end;
        if aTab.FoldedTabs.Count>0 then
          begin;
            aTab.DropDownRect:=Rect(aRect.Right-aDDGlyphSize-cContentIndent, aRect.Top,
                                    aRect.Right+cContentIndent, aRect.Bottom);
            aRect.Right:=aTab.DropDownRect.Left-cContentIndent;
          end;
        if etfHasCloseBtn in aTab.Flags then
          begin;
            y:=(aRect.Top+aRect.Bottom-GlyphClose.Height) div 2;
            aTab.CloseBtnRect:=Rect(aRect.Right-GlyphClose.Width-cCloseBtnBorder, y-cCloseBtnBorder,
                                    aRect.Right+cCloseBtnBorder, y+GlyphClose.Height+cCloseBtnBorder);
            dec(aRect.Right, GlyphClose.Width+2*cCloseBtnBorder);
          end;
      end else
      begin
        if etfHasImage in aTab.Flags then
          begin
            dec(aRect.Right, Images.Width);
            aTab.ImagePoint:=Point(aRect.Right, (aRect.Top+aRect.Bottom-Images.Height) div 2);
            dec(aRect.Right, cContentIndent);
          end;
        if aTab.FoldedTabs.Count>0 then
          begin;
            aTab.DropDownRect:=Rect(aRect.Left-cContentIndent, aRect.Top,
                                    aRect.Left+aDDGlyphSize+cContentIndent, aRect.Bottom);
            aRect.Left:=aTab.DropDownRect.Right+cContentIndent;
          end;
        if etfHasCloseBtn in aTab.Flags then
          begin
            y:=(aRect.Top+aRect.Bottom-GlyphClose.Height) div 2;
            aTab.CloseBtnRect:=Rect(aRect.Left-cCloseBtnBorder, y-cCloseBtnBorder,
                                    aRect.Left+GlyphClose.Width+cCloseBtnBorder, y+GlyphClose.Height+cCloseBtnBorder);
            inc(aRect.Left, GlyphClose.Width+2*cCloseBtnBorder);
          end;
      end;
    inc(aRect.Top);
    aTab.CaptionRect:=aRect;
  end;

  procedure CalcContentVer(AIndex: Integer; AHasSideLap: Boolean);
  var aTab: TECTab;
      bLeft: Boolean;
      x: SmallInt;
  begin
    bLeft:= (TabPosition=tpLeft);
    aTab:=VisibleTabs[AIndex].Tab;
    if AIndex=VisibleTabIndex then
      if bLeft
        then dec(aRect.Left, TabActiveRise)
        else inc(aRect.Right, TabActiveRise);
    aTab.BoundRect:=aRect;
    aTab.PaintRect:=aRect;
    if bLeft
      then inc(aTab.PaintRect.Right, caBottomLap[Style])
      else dec(aTab.PaintRect.Left, caBottomLap[Style]);
    if (AIndex<>VisibleTabIndex) and AHasSideLap then
      if not bReversed then
        begin
          inc(aTab.PaintRect.Bottom, caSideLap[Style]);
          dec(aRect.Bottom, caOverlay[Style]-1);
        end else
        dec(aTab.PaintRect.Top, caSideLap[Style]);
    if bLeft then inc(aRect.Left);
    inc(aRect.Top, cContentIndent);
    dec(aRect.Bottom, cContentIndent);
    if etfHasImage in aTab.Flags then
      begin
        aTab.ImagePoint:=Point((aRect.Left+aRect.Right-Images.Width) div 2, aRect.Top);
        inc(aRect.Top, Images.Height+cContentIndent);
      end;
    if aTab.FoldedTabs.Count>0 then
      begin
        aTab.DropDownRect:=Rect(aRect.Left, aRect.Bottom-aDDGlyphSize-cContentIndent,
                                aRect.Right, aRect.Bottom+cContentIndent);
        aRect.Bottom:=aTab.DropDownRect.Top-cContentIndent;
      end;
    if etfHasCloseBtn in aTab.Flags then
      begin
        x:=(aRect.Left+aRect.Right-GlyphClose.Width) div 2;
        aTab.CloseBtnRect:=Rect(x-cCloseBtnBorder, aRect.Bottom-GlyphClose.Height-cCloseBtnBorder,
                                x+GlyphClose.Width+cCloseBtnBorder, aRect.Bottom+cCloseBtnBorder);
        dec(aRect.Bottom, GlyphClose.Height+2*cCloseBtnBorder);
      end;
    aRect.Left:=(aRect.Left+aRect.Right-FontHeight) div 2;
    x:=Canvas.TextWidth(aTab.Text);
    case Alignment of
      taRightJustify: aRect.Bottom:=Math.min(aRect.Bottom, aRect.Top+x);
      taCenter: aRect.Bottom:=Math.min(aRect.Bottom, (aRect.Bottom+aRect.Top+x) div 2);
    end;
    aTab.CaptionRect:=aRect;
  end;

var aTopLeft, aBttmRight: Integer;

  procedure CalcBoundRectsHor(ALeft, AFrom, ATo: Integer);
  var aIndex: Integer;
  begin
    for aIndex:=AFrom to ATo do
      begin
        aRect.Left:=ALeft;
        aRect.Top:=aTopLeft;
        aRect.Bottom:=aBttmRight;
        inc(ALeft, VisibleTabs[aIndex].Tab.Width);
        aRect.Right:=ALeft;
        if aRect.Right<Width then
          if aIndex<ATo
            then inc(aRect.Right, caOverlay[Style])
            else inc(aRect.Right);
        CalcContentHor(aIndex, (aIndex<ATo));
      end;
  end;

  procedure CalcBoundRectsHorRev(ARight, AFrom, ATo: Integer);
  var aIndex: Integer;
  begin
    for aIndex:=AFrom to ATo do
      begin
        aRect.Right:=ARight;
        aRect.Top:=aTopLeft;
        aRect.Bottom:=aBttmRight;
        dec(ARight, VisibleTabs[aIndex].Tab.Width);
        aRect.Left:=ARight;
        if aRect.Left>0 then
          if aIndex<ATo
            then dec(aRect.Left, caOverlay[Style])
            else dec(aRect.Left);
        CalcContentHor(aIndex, (aIndex<ATo));
      end;
  end;

  procedure CalcBoundRectsHorLimit(ARight, AFrom, ADownTo: Integer);
  var aIndex: Integer;
  begin
    for aIndex:=AFrom downto ADownTo do
      begin
        aRect.Right:=ARight+1;
        aRect.Top:=aTopLeft;
        aRect.Bottom:=aBttmRight;
        dec(ARight, VisibleTabs[aIndex].Tab.Width);
        aRect.Left:=ARight;
        if aIndex<AFrom then inc(aRect.Right, caOverlay[Style]);
        CalcContentHor(aIndex, (aIndex<AFrom));
      end;
  end;

  procedure CalcBoundRectsHorRevLimit(ALeft, AFrom, ADownTo: Integer);
  var aIndex: Integer;
  begin
    for aIndex:=AFrom downto ADownTo do
      begin
        aRect.Left:=ALeft-1;
        aRect.Top:=aTopLeft;
        aRect.Bottom:=aBttmRight;
        inc(ALeft, VisibleTabs[aIndex].Tab.Width);
        aRect.Right:=ALeft;
        if aIndex<AFrom then dec(aRect.Left, caOverlay[Style]);
        CalcContentHor(aIndex, (aIndex<AFrom));
      end;
  end;

  procedure CalcBoundRectsVer(ATop, AFrom, ATo: Integer);
  var aIndex: Integer;
  begin
    for aIndex:=AFrom to ATo do
      begin
        aRect.Left:=aTopLeft;
        aRect.Top:=ATop;
        aRect.Right:=aBttmRight;
        inc(ATop, VisibleTabs[aIndex].Tab.Width);
        aRect.Bottom:=ATop;
        if aRect.Bottom<Height then
          if aIndex<ATo
            then inc(aRect.Bottom, caOverlay[Style])
            else inc(aRect.Bottom);
        CalcContentVer(aIndex, (aIndex<ATo));
      end;
  end;

  procedure CalcBoundRectsVerRev(ABottom, AFrom, ATo: Integer);
  var aIndex: Integer;
  begin
    for aIndex:=AFrom to ATo do
      begin
        aRect.Left:=aTopLeft;
        aRect.Bottom:=ABottom;
        aRect.Right:=aBttmRight;
        dec(ABottom, VisibleTabs[aIndex].Tab.Width);
        aRect.Top:=ABottom;
        if aRect.Top>0 then
          if aIndex<ATo
            then dec(aRect.Top, caOverlay[Style])
            else dec(aRect.Top);
        CalcContentVer(aIndex, (aIndex<ATo));
      end;
  end;

  procedure CalcBoundRectsVerLimit(ABottom, AFrom, ADownTo: Integer);
  var aIndex: Integer;
  begin
    for aIndex:=AFrom downto ADownTo do
      begin
        aRect.Left:=aTopLeft;
        aRect.Right:=aBttmRight;
        aRect.Bottom:=ABottom+1;
        dec(ABottom, VisibleTabs[aIndex].Tab.Width);
        aRect.Top:=ABottom;
        if aIndex<AFrom then inc(aRect.Bottom, caOverlay[Style]);
        CalcContentVer(aIndex, (aIndex<AFrom));
      end;
  end;

  procedure CalcBoundRectsVerRevLimit(ATop, AFrom, ADownTo: Integer);
  var aIndex: Integer;
  begin
    for aIndex:=AFrom downto ADownTo do
      begin
        aRect.Top:=ATop-1;
        aRect.Left:=aTopLeft;
        aRect.Right:=aBttmRight;
        inc(ATop, VisibleTabs[aIndex].Tab.Width);
        aRect.Bottom:=ATop;
        if aIndex<AFrom then dec(aRect.Top, caOverlay[Style]);
        CalcContentVer(aIndex, (aIndex<AFrom));
      end;
  end;

var i, aBound, aFrom, aIndex, aPosLRBtns, aRow, aWidest: Integer;
    aTabInRow: TTabRow;
const cOuterRect: TRect = (Left: -100; Top: -100; Right: -80; Bottom: -80);
begin
  {$IFDEF DBGTAB} DebugLn('TCustomECTabCtrl.Calculate'); {$ENDIF}
  if TabPosition in [tpTop, tpBottom]
    then aDDGlyphSize:=Canvas.GlyphExtent(cDropDownGlyph[TabPosition]).cx
    else aDDGlyphSize:=Canvas.GlyphExtent(cDropDownGlyph[TabPosition]).cy;
  FontHeight:=Canvas.TextHeight('Šj|9');
  bR2L:=IsRightToLeft;
  bReversed:= (etcoReversed in Options);
  i:=-1;  { find row with TabIndex }
  for aRow:=0 to RowCount-1 do
    begin
      aFrom:=VisiTabsInRow[aRow].From;
      if (aFrom<=VisibleTabIndex) and (VisibleTabIndex<(aFrom+VisiTabsInRow[aRow].Count)) then
        begin
          i:=aRow;
          RowIndex:=aRow;
          break;
        end;
    end;
  if i>0 then
    begin  { move row with TabIndex to the bottom }
      aTabInRow:=VisiTabsInRow[i];
      for aIndex:=i-1 downto 0 do
        VisiTabsInRow[aIndex+1]:=VisiTabsInRow[aIndex];
      VisiTabsInRow[0]:=aTabInRow;
    end;
  if (ectfKeepActiveVisi in Flags) and (PosLRMax>0) then
    begin  { make TabIndex visible }
      i:=VisibleTabIndex;
      for aRow:=0 to RowCount-1 do
        begin
          aFrom:=VisiTabsInRow[aRow].From;
          if (i>=aFrom) and (i<(aFrom+VisiTabsInRow[aRow].Count)) then
            begin
              FPosLeftRight:=Math.min(i-aFrom, PosLRMax);
              break;
            end;
        end;
      DoBtnsEnabled;
      exclude(Flags, ectfKeepActiveVisi);
    end;
  aPosLRBtns:=PosLeftRight;
  if RowCount>1 then
    begin  { calc. shift[px] of the base row }
      aBound:=0;
      for aIndex:=0 to aPosLRBtns-1 do
        inc(aBound, VisibleTabs[aIndex].Tab.Width);
    end;
  if PosLRMax>0 then
    begin
      aPosLRBtns:=PosLeftRight;
      if aPosLRBtns=PosLRMax then
        begin
          aWidest:=VisiTabsInRow[0].Width;
          for i:=1 to MaxRowCount-1 do
            if aWidest<VisiTabsInRow[i].Width then aWidest:=VisiTabsInRow[i].Width;
        end;
    end else
    aPosLRBtns:=0;
  for aIndex:=0 to length(VisibleTabs)-1 do  { clear Tab bound rects }
    VisibleTabs[aIndex].Tab.BoundRect:=cOuterRect;
  if TabPosition in [tpTop, tpBottom] then  { calc. Tab bound rects }
    begin
      for aRow:=0 to RowCount-1 do
        begin
          aFrom:=VisiTabsInRow[aRow].From;
          case TabPosition of
            tpTop:
              begin
                aBttmRight:=Height-cBottomBevel-BottomSize-aRow*TabHeight;
                aTopLeft:=aBttmRight-TabHeight;
              end;
            tpBottom:
              begin
                aTopLeft:=cBottomBevel+BottomSize+aRow*TabHeight;
                aBttmRight:=aTopLeft+TabHeight;
              end;
          end;  { case }
          if (PosLRMax=0) or (aPosLRBtns<PosLRMax) then
            begin
              if not (bR2L xor bReversed) then
                begin
                  if aFrom=0
                    then CalcBoundRectsHor(0, aFrom+aPosLRBtns, aFrom+VisiTabsInRow[aRow].Count-1)
                    else CalcBoundRectsHor(-aBound, aFrom, aFrom+VisiTabsInRow[aRow].Count-1);
                end else
                begin
                  if aFrom=0
                    then CalcBoundRectsHorRev(Width, aFrom+aPosLRBtns, aFrom+VisiTabsInRow[aRow].Count-1)
                    else CalcBoundRectsHorRev(Width+aBound, aFrom, aFrom+VisiTabsInRow[aRow].Count-1);
                end;
            end else
            begin
              if not (bR2L xor bReversed)
                then CalcBoundRectsHorLimit(RowWidth+VisiTabsInRow[aRow].Width-aWidest,
                       aFrom+VisiTabsInRow[aRow].Count-1, Math.max(aFrom+aPosLRBtns-1, 0))
                else CalcBoundRectsHorRevLimit(Width-RowWidth-VisiTabsInRow[aRow].Width+aWidest,
                       aFrom+VisiTabsInRow[aRow].Count-1, Math.max(aFrom+aPosLRBtns-1, 0));
            end;
        end;
    end else
    begin  { tpLeft, tpRight }
      for aRow:=0 to RowCount-1 do
        begin
          aFrom:=VisiTabsInRow[aRow].From;
          case TabPosition of
            tpLeft:
              begin
                aBttmRight:=Width-cBottomBevel-BottomSize-aRow*TabHeight;
                aTopLeft:=aBttmRight-TabHeight;
              end;
            tpRight:
              begin
                aTopLeft:=cBottomBevel+BottomSize+aRow*TabHeight;
                aBttmRight:=aTopLeft+TabHeight;
              end;
          end;  { case }
          if (PosLRMax=0) or (aPosLRBtns<PosLRMax) then
            begin
              if not bReversed then
                begin
                  if aFrom=0
                    then CalcBoundRectsVer(0, aFrom+aPosLRBtns, aFrom+VisiTabsInRow[aRow].Count-1)
                    else CalcBoundRectsVer(-aBound, aFrom, aFrom+VisiTabsInRow[aRow].Count-1);
                end else
                begin
                  if aFrom=0
                    then CalcBoundRectsVerRev(Height, aFrom+aPosLRBtns, aFrom+VisiTabsInRow[aRow].Count-1)
                    else CalcBoundRectsVerRev(Height+aBound, aFrom, aFrom+VisiTabsInRow[aRow].Count-1);
                end;
            end else
            begin
              if not bReversed
                then CalcBoundRectsVerLimit(RowWidth+VisiTabsInRow[aRow].Width-aWidest,
                       aFrom+VisiTabsInRow[aRow].Count-1, Math.max(aFrom+aPosLRBtns-1, 0))
                else CalcBoundRectsVerRevLimit(Height-RowWidth-VisiTabsInRow[aRow].Width+aWidest,
                       aFrom+VisiTabsInRow[aRow].Count-1, Math.max(aFrom+aPosLRBtns-1, 0));
            end;
        end;
    end;
  for aRow:=0 to RowCount-2 do
    begin  { calc inactive backgrounds }
      if TabPosition in [tpTop, tpBottom] then
        begin  { tpTop, tpBottom }
          if not (bR2L xor bReversed) then
            begin
              aBound:=0;
              for i:=1 to RowCount-1 do
                aBound:=Math.max(aBound, VisibleTabs[VisiTabsInRow[i].From+VisiTabsInRow[i].Count-1].Tab.PaintRect.Right);
              i:=VisiTabsInRow[aRow].From+VisiTabsInRow[aRow].Count-1;
              VisiTabsInRow[aRow].InactBkgnd:= ((VisibleTabs[i].Tab.PaintRect.Right)<aBound);
              if VisiTabsInRow[aRow].InactBkgnd then
                begin
                  aRect.Left:=VisibleTabs[i].Tab.PaintRect.Right-caSideLap[Style];
                  aRect.Right:=aBound;
                  if TabPosition=tpTop then
                    begin
                      aRect.Bottom:=VisibleTabs[i].Tab.PaintRect.Bottom;
                      aRect.Top:=aRect.Bottom-TabHeight-caBottomLap[Style];
                    end else
                    begin
                      aRect.Top:=VisibleTabs[i].Tab.PaintRect.Top;
                      aRect.Bottom:=aRect.Top+TabHeight+caBottomLap[Style];
                    end;
                  VisiTabsInRow[aRow].InactRect:=aRect;
                end;
            end else
            begin  { reversed }
              aBound:=Width;
              for i:=1 to RowCount-1 do
                aBound:=Math.min(aBound, VisibleTabs[VisiTabsInRow[i].From+VisiTabsInRow[i].Count-1].Tab.PaintRect.Left);
              i:=VisiTabsInRow[aRow].From+VisiTabsInRow[aRow].Count-1;
              VisiTabsInRow[aRow].InactBkgnd:= ((VisibleTabs[i].Tab.PaintRect.Left)>aBound);
              if VisiTabsInRow[aRow].InactBkgnd then
                begin
                  aRect.Right:=VisibleTabs[i].Tab.PaintRect.Left+caSideLap[Style];
                  aRect.Left:=aBound;
                  if TabPosition=tpTop then
                    begin
                      aRect.Bottom:=VisibleTabs[i].Tab.PaintRect.Bottom;
                      aRect.Top:=aRect.Bottom-TabHeight-caBottomLap[Style];
                    end else
                    begin
                      aRect.Top:=VisibleTabs[i].Tab.PaintRect.Top;
                      aRect.Bottom:=aRect.Top+TabHeight+caBottomLap[Style];
                    end;
                  VisiTabsInRow[aRow].InactRect:=aRect;
                end;
            end;
        end else
        begin  { tpLeft, tpRight }
          if not bReversed then
            begin
              aBound:=0;
              for i:=1 to RowCount-1 do
                aBound:=Math.max(aBound, VisibleTabs[VisiTabsInRow[i].From+VisiTabsInRow[i].Count-1].Tab.PaintRect.Bottom);
              i:=VisiTabsInRow[aRow].From+VisiTabsInRow[aRow].Count-1;
              VisiTabsInRow[aRow].InactBkgnd:= ((VisibleTabs[i].Tab.PaintRect.Bottom)<aBound);
              if VisiTabsInRow[aRow].InactBkgnd then
                begin
                  aRect.Top:=VisibleTabs[i].Tab.PaintRect.Bottom-caSideLap[Style];
                  aRect.Bottom:=aBound;
                  if TabPosition=tpLeft then
                    begin
                      aRect.Right:=VisibleTabs[i].Tab.PaintRect.Right;
                      aRect.Left:=aRect.Right-TabHeight-caBottomLap[Style];
                    end else
                    begin
                      aRect.Left:=VisibleTabs[i].Tab.PaintRect.Left;
                      aRect.Right:=aRect.Left+TabHeight+caBottomLap[Style];
                    end;
                  VisiTabsInRow[aRow].InactRect:=aRect;
                end;
            end else
            begin  { reversed }
              aBound:=Height;
              for i:=1 to RowCount-1 do
                aBound:=Math.min(aBound, VisibleTabs[VisiTabsInRow[i].From+VisiTabsInRow[i].Count-1].Tab.PaintRect.Top);
              i:=VisiTabsInRow[aRow].From+VisiTabsInRow[aRow].Count-1;
              VisiTabsInRow[aRow].InactBkgnd:= ((VisibleTabs[i].Tab.PaintRect.Top)>aBound);
              if VisiTabsInRow[aRow].InactBkgnd then
                begin
                  aRect.Bottom:=VisibleTabs[i].Tab.PaintRect.Top+caSideLap[Style];
                  aRect.Top:=aBound;
                  if TabPosition=tpLeft then
                    begin
                      aRect.Right:=VisibleTabs[i].Tab.PaintRect.Right;
                      aRect.Left:=aRect.Right-TabHeight-caBottomLap[Style];
                    end else
                    begin
                      aRect.Left:=VisibleTabs[i].Tab.PaintRect.Left;
                      aRect.Right:=aRect.Left+TabHeight+caBottomLap[Style];
                    end;
                  VisiTabsInRow[aRow].InactRect:=aRect;
                end;
            end;
        end;
    end;
  VisiTabsInRow[RowCount-1].InactBkgnd:=False;
end;

procedure TCustomECTabCtrl.CalculatePreferredSize(var PreferredWidth, PreferredHeight: Integer; WithThemeSpace: Boolean);
var aHeight, aRows: Integer;
begin
  aRows:=RowCount;
  aHeight:=cBottomBevel+BottomSize+aRows*TabHeight;
  if aRows=1 then inc(aHeight, TabActiveRise);
  if TabPosition in [tpTop, tpBottom] then
    begin
      PreferredHeight:=aHeight;
      PreferredWidth:=0;
    end else
    begin
      PreferredHeight:=0;
      PreferredWidth:=aHeight;
    end;
end;

procedure TCustomECTabCtrl.CalcVisibleTabsAnFlags;
var i, j, aTabIndex: Integer;
const cCloseBtn = [etoCloseable, etoCloseBtn];
begin
  j:=0;
  for i:=0 to Tabs.Count-1 do
    if (etoVisible in Tabs[i].Options) and not (etfFolded in Tabs[i].Flags) then inc(j);
  SetLength(FVisiTabs, j);
  aTabIndex:=TabIndex;
  FVisibleTabIndex:=-1;
  j:=0;
  for i:=0 to Tabs.Count-1  do
    begin
      if (etoVisible in Tabs[i].Options) and not (etfFolded in Tabs[i].Flags) then
        begin
          if i=aTabIndex then FVisibleTabIndex:=j;
          FVisiTabs[j].Tab:=Tabs[i];
          FVisiTabs[j].Index:=i;
          Tabs[i].Flags:=[];
          if Tabs[i].FontOptions.IsIdentical(Font) then include(Tabs[i].Flags, etfParentFont);
          if assigned(Images) and (Tabs[i].ImageIndex>=0) then include(Tabs[i].Flags, etfHasImage);
          if (not (etcoCloseBtnActiveOnly in Options) or (i=aTabIndex))
            and ((cCloseBtn*Tabs[i].Options)=cCloseBtn)
            then include(Tabs[i].Flags, etfHasCloseBtn);
          inc(j);
        end;
    end;
end;

function TCustomECTabCtrl.CalcVisiTabsInRow(ARow, AFrom, ARowWidth: Integer; ALRBtns: Boolean): Boolean;
var i, aSum: Integer;  { returns True when all tabs are placed and nothing remains to the next row(s) }
begin
  aSum:=0;
  VisiTabsInRow[ARow].From:=AFrom;
  for i:=AFrom to length(VisibleTabs)-1 do
    begin
      inc(aSum, VisibleTabs[i].Tab.PreferredWidth);
      if aSum>ARowWidth then
        begin
          if ALRBtns then dec(AFrom);
          VisiTabsInRow[ARow].Count:=i-AFrom;
          VisiTabsInRow[ARow].Width:=aSum;
          Result:=False;
          exit;  { Exit! }
        end;
    end;
  VisiTabsInRow[ARow].Count:=length(VisibleTabs)-AFrom;
  VisiTabsInRow[ARow].Width:=aSum;
  Result:=True;
end;

function TCustomECTabCtrl.CalcVisiTabsInRows(ARowWidth: Integer; ALRBtns: Boolean; var arows: smallint): Boolean;
var i: Integer;
begin
  if ALRBtns then dec(ARowWidth);
  if not CalcVisiTabsInRow(0, 0, ARowWidth, ALRBtns) then
    for i:=1 to MaxRowCount-1 do
      if CalcVisiTabsInRow(i, VisiTabsInRow[i-1].From+VisiTabsInRow[i-1].Count, ARowWidth, ALRBtns) then
        begin
          aRows:=i+1;
          Result:=False;
          exit;  { Exit! }
        end;
  Result:=True;
end;

procedure TCustomECTabCtrl.ChangeHint(ATabIndex: Integer);
begin
  include(Flags, ectfLockHint);
  if (ATabIndex>=0) and (Tabs[ATabIndex].Hint<>'')
    then Hint:=Tabs[ATabIndex].Hint
    else Hint:=DefHint;
  exclude(Flags, ectfLockHint);
end;

procedure TCustomECTabCtrl.CloseAllFoldedTabs(AFolder: Integer);
var i, aID: Integer;
begin
  for i:=0 to Tabs[AFolder].FoldedTabs.Count-1 do
    if etoCloseable in TECTab(Tabs[AFolder].FoldedTabs[i]).Options then
      begin
        aID:=TECTab(Tabs[AFolder].FoldedTabs[i]).ID;
        Tabs[AFolder].FoldedTabs.Delete(i);
        Tabs.Delete(Tabs.IDToIndex(aID));
      end;
end;

procedure TCustomECTabCtrl.CMBiDiModeChanged(var Message: TLMessage);
begin
  DesignLeftRightBtns;
  include(Flags, ectfRealignButtons);
  RecalcInvalidate;
end;

procedure TCustomECTabCtrl.DeleteFromFoldedTabs(AFolder, ATabID: Integer);
var i: Integer;
begin
  for i:=0 to Tabs[AFolder].FoldedTabs.Count-1 do
    if TECTab(Tabs[AFolder].FoldedTabs[i]).ID=ATabID then
      begin
        Tabs[AFolder].FoldedTabs.Delete(i);
        break;
      end;
end;

procedure TCustomECTabCtrl.DeleteTab(AIndex: Integer);

  procedure ActivateLeftNear;
  begin
    TabIndex:=Math.min(Math.max(AIndex-1, 0), Tabs.Count-1)
  end;

var i, j, aPrevID, aPrevIndex: Integer;
    bCanClose: Boolean;
begin
  bCanClose:=True;
  if assigned(OnCloseQuery) then OnCloseQuery(self, AIndex, bCanClose);
  if bCanClose then
    begin
      BeginUpdate;
      if etfFolded in Tabs[AIndex].Flags then
        begin
          for i:=0 to Tabs.Count-1 do
            for j:=0 to Tabs[i].FoldedTabs.Count-1 do
              if TECTab(Tabs[i].FoldedTabs[j])=Tabs[AIndex] then
                begin
                  Tabs[i].FoldedTabs.Delete(j);
                  break;
                end;
        end;
      aPrevID:=Tabs[AIndex].PreviousID;
      TCollection(Tabs).Delete(AIndex);
      if AIndex=TabIndex then
        begin
          include(Flags, ectfKeepActiveVisi);
          case TabClosing of
            etcLeftNear: ActivateLeftNear;
            etcRightNear: TabIndex:=Math.min(AIndex, Tabs.Count-1);
            etcFirst: TabIndex:=Math.min(0, Tabs.Count-1);
            etcLast: TabIndex:=Tabs.Count-1;
            etcPrevious:
              begin
                if aPrevID>=0 then
                  begin
                    aPrevIndex:=Tabs.IDToIndex(aPrevID);
                    if aPrevIndex>=0 then
                      begin
                        aPrevID:=Tabs[aPrevIndex].PreviousID;
                        TabIndex:=aPrevIndex;
                        Tabs[aPrevIndex].PreviousID:=aPrevID;
                      end else
                      ActivateLeftNear;
                  end;
              end;
          end;  { case }
        end else
        if AIndex<TabIndex then TabIndex:=TabIndex-1;
      EndUpdate;
    end;
end;

procedure TCustomECTabCtrl.DesignLeftRightBtns;
begin
  if TabPosition in [tpTop, tpBottom] then
    begin
      if not (IsRightToLeft xor (etcoReversed in Options)) then
        begin
          BtnLeftUp.GlyphDesign:=egdArrowLeft;
          BtnRightDown.GlyphDesign:=egdArrowRight;
        end else
        begin
          BtnLeftUp.GlyphDesign:=egdArrowRight;
          BtnRightDown.GlyphDesign:=egdArrowLeft;
        end;
    end else
    begin  { reversed }
      if not (etcoReversed in Options) then
        begin
          BtnLeftUp.GlyphDesign:=egdArrowUp;
          BtnRightDown.GlyphDesign:=egdArrowDown;
        end else
        begin
          BtnLeftUp.GlyphDesign:=egdArrowDown;
          BtnRightDown.GlyphDesign:=egdArrowUp;
        end;
    end;
end;

function TCustomECTabCtrl.DialogChar(var Message: TLMKey): Boolean;
var i: Integer;
begin
  Result:=False;
  if Message.Msg=LM_SYSCHAR then
    begin
      if IsEnabled and IsVisible then
        begin
          for i:=0 to length(VisibleTabs)-1 do
            if IsAccel(Message.CharCode, VisibleTabs[i].Tab.Text) then
              begin
                ActivateTab(VisibleTabs[i].Index);
                Result:=True;
                exit;  { Exit! }
              end;
          Result:=inherited DialogChar(Message);
        end;
    end;
end;

procedure TCustomECTabCtrl.DoBtnsEnabled;
begin
  BtnLeftUp.Enabled:= (PosLeftRight>0);
  BtnRightDown.Enabled:= (PosLeftRight<PosLRMax);
end;

procedure TCustomECTabCtrl.DoContextPopup(MousePos: TPoint; var Handled: Boolean);
begin
  if IsEnabled and (Hovered>cECNoTabHovered) and (ClickedTabIndex=-1) then
    begin
      if assigned(Tabs[Hovered].PopupMenu) then
        begin
          Tabs[Hovered].PopupMenu.PopUp;
          Handled:=True;
        end else
        if (etcoDropDownMenu in Options) and ((etoCloseable in Tabs[Hovered].Options) or
          ([etcoAddTabButton, etcoFixedPosition]*Options<>[etcoFixedPosition]) or
          (([etoCanBeFolded, etoCanFold]*Tabs[Hovered].Options)<>[])) then
          begin
            ShowDropDownMenu(Hovered);
            Handled:=True;
          end;
    end;
  inherited DoContextPopup(MousePos, Handled);
end;

function TCustomECTabCtrl.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  if not Dragging then
    begin
      inc(MouseWheelCnt);
      if MouseWheelCnt>=cMouseWheelMax then
        begin
          if TabPosition in [tpTop, tpBottom]
            then SelectNext
            else if not (etcoReversed in Options)
                   then SelectNext
                   else SelectPrevious;
          MouseWheelCnt:=0;
        end;
    end;
  Result:=inherited DoMouseWheelDown(Shift, MousePos);
end;

function TCustomECTabCtrl.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  if not Dragging then
    begin
      dec(MouseWheelCnt);
      if MouseWheelCnt<=0 then
        begin
          if TabPosition in [tpTop, tpBottom]
            then SelectPrevious
            else if not (etcoReversed in Options)
                   then SelectPrevious
                   else SelectNext;
          MouseWheelCnt:=cMouseWheelMax;
        end;
    end;
  Result:=inherited DoMouseWheelUp(Shift, MousePos);
end;

procedure TCustomECTabCtrl.DoSwitchFoldedTab(AIndex, AFolder: Integer);
var i: Integer;
begin
  exclude(Tabs[AIndex].Flags, etfFolded);
  Tabs[AIndex].FoldedTabs.Add(Tabs[AFolder]);
  include(Tabs[AFolder].Flags, etfFolded);
  for i:=0 to Tabs[AFolder].FoldedTabs.Count-1 do
    if TECTab(Tabs[AFolder].FoldedTabs[i])<>Tabs[AIndex] then
      Tabs[AIndex].FoldedTabs.Add(Tabs[AFolder].FoldedTabs[i]);
  Tabs[AFolder].FoldedTabs.Clear;
end;

procedure TCustomECTabCtrl.DragDrop(Source: TObject; X, Y: Integer);
var aHoveredTab: Integer;
begin
  inherited DragDrop(Source, X, Y);
  if ClickedTabIndex>=0 then
    begin
      aHoveredTab:=Hovered;
      if not (ectfDragFolding in Flags) then
        begin
          if aHoveredTab<0 then aHoveredTab:=Tabs.Count-1;
          Tabs[ClickedTabIndex].Index:=aHoveredTab;
          TabIndex:=aHoveredTab;
        end else
        FoldTab(ClickedTabIndex, aHoveredTab);
    end;
end;

procedure TCustomECTabCtrl.DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var aHoveredTab: Integer;
begin
  inherited DragOver(Source, X, Y, State, Accept);
  aHoveredTab:=Hovered;
  if not (ectfDragFolding in Flags) then
    begin
      if aHoveredTab>=0
        then Accept:= (aHoveredTab<>ClickedTabIndex)
        else Accept:= ((ClickedTabIndex>=0) and (ClickedTabIndex<(Tabs.Count-1)));
    end else
    Accept:= (aHoveredTab>=0) and (aHoveredTab<>ClickedTabIndex) and (etoCanFold in Tabs[aHoveredTab].Options);
end;

procedure TCustomECTabCtrl.DropDownMenuClose(Sender: TObject);
var aPoint: TPoint;
begin
  { ClickedTabIndex:=-1; }
  aPoint:=ScreenToClient(Mouse.CursorPos);
  CalcHoveredTab(aPoint.X, aPoint.Y);
end;

procedure TCustomECTabCtrl.EndUpdate;
begin
  dec(UpdateCount);
  if UpdateCount=0 then
    begin
      Flags:=Flags+[ectfInvPrefSize, ectfRealignButtons];
      RecalcInvalidate;
    end;
end;

class destructor TCustomECTabCtrl.FinalizeClass;
begin
  FreeAndNil(DropDownMenu);
  FreeAndNil(GlyphAdd);
  FreeAndNil(GlyphClose);
  FreeAndNil(GlyphCloseInact);
  FreeAndNil(GlyphCloseDis);
  FreeAndNil(GlyphCloseHigh);
end;

function TCustomECTabCtrl.FindFolderOfTab(AIndex: Integer): Integer;
var i, j: Integer;
begin
  Result:=-1;
  for i:=0 to Tabs.Count-1 do
    for j:=0 to Tabs[i].FoldedTabs.Count-1 do
      if Tabs[AIndex]=TECTab(Tabs[i].FoldedTabs[j]) then
        begin
          Result:=i;
          exit;  { Exit! }
        end;
end;

procedure TCustomECTabCtrl.FoldTab(AIndex, AFolder: Integer);
var i: Integer;
begin
  include(Tabs[AIndex].Flags, etfFolded);
  Tabs[AFolder].FoldedTabs.Insert(0, Tabs[AIndex]);
  for i:=0 to Tabs[AIndex].FoldedTabs.Count-1 do
    Tabs[AFolder].FoldedTabs.Add(Tabs[AIndex].FoldedTabs[i]);
  Tabs[AIndex].FoldedTabs.Clear;
  if TabIndex=AIndex
    then TabIndex:=AFolder
    else RecalcInvalidate;
  if assigned(OnFold) then OnFold(self, AIndex, AFolder);
end;

procedure TCustomECTabCtrl.FontChanged(Sender: TObject);
var i: Integer;
begin
  inherited FontChanged(Sender);
  BeginUpdate;
  for i:=0 to Tabs.Count-1 do
    if etfParentFont in Tabs[i].Flags then
      begin
        Tabs[i].FontOptions.FontColor:=Font.Color;
        Tabs[i].FontOptions.FontSize:=Font.Size;
        Tabs[i].FontOptions.FontStyles:=Font.Style;
      end;
  EndUpdate;
end;

function TCustomECTabCtrl.GetButtonsWidth: SmallInt;
begin
  Result:=TabHeight+TabActiveRise div 2;
end;

class function TCustomECTabCtrl.GetControlClassDefaultSize: TSize;
begin
  Result.cx:=200;
  Result.cy:=30;
end;

function TCustomECTabCtrl.IndexOfTabAt(X, Y: Integer): Integer;
var aPoint: TPoint;
    i, aTabIndex: Integer;
begin
  Result:=cECNoTabHovered;
  aTabIndex:=TabIndex;
  aPoint:=Point(X, Y);
  if (aTabIndex>=0) and PtInRect(Tabs[aTabIndex].BoundRect, aPoint)
    then Result:=aTabIndex
    else for i:=0 to length(VisibleTabs)-1 do
           if PtInRect(VisibleTabs[i].Tab.BoundRect, aPoint) then
             begin
               Result:=VisibleTabs[i].Index;
               break;
             end;
end;

class constructor TCustomECTabCtrl.InitializeClass;
begin
  {$I ectabctrl.lrs}
  GlyphAdd:=TPortableNetworkGraphic.Create;
  GlyphAdd.LoadFromLazarusResource('add');
  GlyphClose:=TPortableNetworkGraphic.Create;
  GlyphClose.LoadFromLazarusResource('close');
  GlyphCloseInact:=TPortableNetworkGraphic.Create;
  GlyphCloseInact.LoadFromLazarusResource('closeinact');
  GlyphCloseDis:=TPortableNetworkGraphic.Create;
  GlyphCloseDis.LoadFromLazarusResource('closedis');
  GlyphCloseHigh:=TPortableNetworkGraphic.Create;
  GlyphCloseHigh.LoadFromLazarusResource('closehigh');
end;

function TCustomECTabCtrl.IsSameVisiRow(AVisiIndex, BVisiIndex: Integer): Boolean;
var i, aLimit: Integer;
begin
  Result:= (RowCount=1);
  if not Result then
    for i:=0 to length(VisiTabsInRow)-1 do
      begin
        aLimit:=VisiTabsInRow[i].From;
        if (AVisiIndex>=aLimit) and (BVisiIndex>=aLimit) then
          begin
            inc(aLimit, VisiTabsInRow[i].Count);
            Result:= ((AVisiIndex<aLimit) and (BVisiIndex<aLimit));
            if Result then exit;
          end;
      end;
end;

function TCustomECTabCtrl.IsTabVisible(AIndex: Integer): Boolean;
var aTabsRect: TRect;
begin
  if (etoVisible in Tabs[AIndex].Options) and not (etfFolded in Tabs[AIndex].Flags) then
    begin
      aTabsRect:=ClientRect;
      if TabPosition in [tpTop, tpBottom] then
        begin  { tpTop, tpBottom }
          if not ((etcoReversed in Options) xor IsRightToLeft) then
            begin
              if assigned(BtnAdd) then aTabsRect.Right:=BtnAdd.Left;
              if BtnLeftUp.Visible then aTabsRect.Right:=Math.min(aTabsRect.Right, BtnLeftUp.Left);
            end else
            begin  { reversed or R2L }
              if assigned(BtnAdd) then aTabsRect.Left:=BtnAdd.Left+BtnAdd.Width;
              if BtnLeftUp.Visible then aTabsRect.Left:=Math.max(aTabsRect.Left, BtnLeftUp.Left+BtnLeftUp.Width);
            end;
        end else
        begin  { tpLeft, tpRight }
          if not (etcoReversed in Options) then
            begin
              if assigned(BtnAdd) then aTabsRect.Bottom:=BtnAdd.Top;
              if BtnLeftUp.Visible then aTabsRect.Bottom:=Math.min(aTabsRect.Bottom, BtnLeftUp.Top);
            end else
            begin  { reversed }
              if assigned(BtnAdd) then aTabsRect.Top:=BtnAdd.Top+BtnAdd.Height;
              if BtnLeftUp.Visible then aTabsRect.Top:=Math.max(aTabsRect.Top, BtnLeftUp.Top+BtnLeftUp.Height);
            end;
        end;
      {$IFDEF DBGTAB} DebugLn('aTabsRect '+inttostr(aTabsRect.Left)+' '+inttostr(aTabsRect.Top)+
                              ' '+inttostr(aTabsRect.Right)+' '+inttostr(aTabsRect.Bottom)); {$ENDIF}
      Result:=((aTabsRect.Left<=Tabs[AIndex].BoundRect.Left) and (aTabsRect.Top<=Tabs[AIndex].BoundRect.Top) and
               (aTabsRect.Right>=Tabs[AIndex].BoundRect.Right) and (aTabsRect.Bottom>=Tabs[AIndex].BoundRect.Bottom));
    end else
    Result:=False;
  {$IFDEF DBGTAB} DebugLn('TCustomECTabCtrl.IsTabVisible '+booltostr(Result, 'T', 'F')); {$ENDIF}
end;

procedure TCustomECTabCtrl.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  case Key of
    VK_RETURN, VK_SPACE: if TabIndex>=0 then ShowDropDownMenu(TabIndex);
    VK_LEFT:
      begin
        if TabPosition in [tpTop, tpBottom] then
          begin
            if not (ssCtrl in Shift) or (etcoFixedPosition in Options) then
              begin
                if not ((etcoReversed in Options) xor IsRightToLeft)
                  then SelectPrevious
                  else SelectNext;
              end else
              begin
                if not ((etcoReversed in Options) xor IsRightToLeft)
                  then MovePrevious
                  else MoveNext;
              end;
          end else
          begin
            if RowCount>1 then
              if TabPosition=tpLeft
                then SelectRowUp
                else SelectRowDown;
          end;
        Key:=0;
      end;
    VK_UP:
      begin
        if TabPosition in [tpLeft, tpRight] then
          begin
            if not (ssCtrl in Shift) or (etcoFixedPosition in Options) then
              begin
                if not (etcoReversed in Options)
                  then SelectPrevious
                  else SelectNext;
              end else
              begin
                if not (etcoReversed in Options)
                  then MovePrevious
                  else MoveNext;
              end;
          end else
          begin
            if RowCount>1 then
              if TabPosition=tpTop
                then SelectRowUp
                else SelectRowDown;
          end;
        Key:=0;
      end;
    VK_RIGHT:
      begin
        if TabPosition in [tpTop, tpBottom] then
          begin
            if not (ssCtrl in Shift) or (etcoFixedPosition in Options) then
              begin
                if not ((etcoReversed in Options) xor IsRightToLeft)
                  then SelectNext
                  else SelectPrevious;
              end else
              begin
                if not ((etcoReversed in Options) xor IsRightToLeft)
                  then MoveNext
                  else MovePrevious;
              end;
          end else
          begin
            if RowCount>1 then
              if TabPosition=tpLeft
                then SelectRowDown
                else SelectRowUp;
          end;
        Key:=0;
      end;
    VK_DOWN:
      begin
        if TabPosition in [tpLeft, tpRight] then
          begin
            if not (ssCtrl in Shift) or (etcoFixedPosition in Options) then
              begin
                if not (etcoReversed in Options)
                  then SelectNext
                  else SelectPrevious;
              end else
              begin
                if not (etcoReversed in Options)
                  then MoveNext
                  else MovePrevious;
              end;
          end else
          begin
            if RowCount>1 then
              if TabPosition=tpTop
                then SelectRowDown
                else SelectRowUp;
          end;
        Key:=0;
      end;
  end;
end;

procedure TCustomECTabCtrl.MakeTabAvailable(AIndex: Integer; AActivate: Boolean);
var i, j, aFolder, aFrom, aTo: Integer;
    aRows: SmallInt;
begin
  if etoVisible in Tabs[AIndex].Options then
    begin
      BeginUpdate;
      if etfFolded in Tabs[AIndex].Flags then
        begin
          aFolder:=FindFolderOfTab(AIndex);
          if TabIndex=aFolder then AActivate:=True;  { when folder was active }
          DoSwitchFoldedTab(AIndex, aFolder);
          if not (etcoKeepOrigPosFoldTab in Options) then
            begin
              Tabs[AIndex].Index:=aFolder;
              AIndex:=aFolder;
            end;
          aRows:=1;
          CalcVisibleTabsAnFlags;
          CalcVisiTabsInRows(PrefRowWidth, BtnLeftUp.Visible, aRows);
        end;
      if BtnLeftUp.Visible then
        begin
          for i:=0 to RowCount-1 do
            begin
              aFrom:=VisiTabsInRow[i].From;
              aTo:=VisibleTabs[aFrom+VisiTabsInRow[i].Count-1].Index;
              aFrom:=VisibleTabs[aFrom].Index;
              if (aFrom<=AIndex) and (AIndex<=aTo) then
                begin
                  for j:=aFrom to aTo do
                    if AIndex=VisibleTabs[j].Index then
                      begin
                        FPosLeftRight:=Math.min(PosLRMax, j-VisiTabsInRow[i].From);
                        break;
                      end;
                  break;
                end;
            end;
          DoBtnsEnabled;
        end;
      if AActivate then TabIndex:=AIndex;
      EndUpdate;
    end;
end;

procedure TCustomECTabCtrl.MIAddTab(Sender: TObject);
begin
  AddTab(NewTabPosition, etcoNewTabActive in Options);
end;

procedure TCustomECTabCtrl.MICloseAllClick(Sender: TObject);
begin
  CloseAllFoldedTabs(Tabs.IDToIndex(TMenuItem(Sender).Tag));
end;

procedure TCustomECTabCtrl.MICloseFoldedTabClick(Sender: TObject);
begin
  DeleteFromFoldedTabs(DropDownMenu.Tag, TMenuItem(Sender).Tag);
  DeleteTab(TMenuItem(Sender).Tag);
end;

procedure TCustomECTabCtrl.MICloseTabClick(Sender: TObject);
begin
  DeleteTab(TMenuItem(Sender).Tag);
end;

procedure TCustomECTabCtrl.MIFoldedTabClick(Sender: TObject);
begin
  SwitchFoldedTab(Tabs.IDToIndex(TMenuItem(Sender).Tag), DropDownMenu.Tag);
end;

procedure TCustomECTabCtrl.MIFoldToClick(Sender: TObject);
begin
  FoldTab(DropDownMenu.Tag, Tabs.IDToIndex(TMenuItem(Sender).Tag));
end;

procedure TCustomECTabCtrl.MIMoveLeftmostClick(Sender: TObject);
begin
  MoveTab(DropDownMenu.Tag, 0);
end;

procedure TCustomECTabCtrl.MIMoveLeftClick(Sender: TObject);
var i: Integer;
begin
  i:=DropDownMenu.Tag-1;
  while (i>0) and not (etoVisible in Tabs[i].Options) do
    dec(i);
  MoveTab(DropDownMenu.Tag, i);
end;

procedure TCustomECTabCtrl.MIMoveRightClick(Sender: TObject);
var i: Integer;
begin
  i:=DropDownMenu.Tag+1;
  while (i<(Tabs.Count-1)) and not (etoVisible in Tabs[i].Options) do
    inc(i);
  MoveTab(DropDownMenu.Tag, i);
end;

procedure TCustomECTabCtrl.MIMoveRightmostClick(Sender: TObject);
begin
  MoveTab(DropDownMenu.Tag, Tabs.Count-1);
end;

procedure TCustomECTabCtrl.MIUnfoldClick(Sender: TObject);
begin
  UnfoldTab(TMenuItem(Sender).Tag, DropDownMenu.Tag);
end;

procedure TCustomECTabCtrl.MIUnfoldAllClick(Sender: TObject);
begin
  UnfoldAllTabs(DropDownMenu.Tag);
end;

procedure TCustomECTabCtrl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var aTab: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button in [mbLeft, mbMiddle] then
    begin
      aTab:=IndexOfTabAt(X, Y);
      if Button=mbLeft then
        begin
          if PosLRMax>0 then
            if (not BtnLeftUp.Enabled and PtInRect(BtnLeftUp.BoundsRect, Point(X, Y))) or
              (not BtnRightDown.Enabled and PtInRect(BtnRightDown.BoundsRect, Point(X, Y))) then exit;  { Exit! }
          if Hovered>=0 then
            case TabHovered of
              ethTabCaption: TabIndex:=Hovered;
              ethCloseButton:
                begin
                  include(Flags, ectfCloseBtnDown);
                  Invalidate;
                  exit;  { Exit! }
                end;
              ethDropDownGlyph:
                begin
                  ShowDropDownMenu(Hovered);
                  exit;  { Exit! }
                end;
            end;  { case }
        end else
        if (etcoMiddleButtonClose in Options) and (Hovered>=0) and (etoCloseable in Tabs[Hovered].Options) then
          begin
            DeleteTab(Hovered);
            CalcHoveredTab(X, Y);
            exit;  { Exit! }
          end;
      if (etcoAutoDragTabs in Options) and (length(VisibleTabs)>1) then
        begin
          if aTab>=0 then
            begin
              if not (etcoFixedPosition in Options) then
                begin  { movable }
                  ClickedTabIndex:=aTab;
                  if etoCanBeFolded in Tabs[aTab].Options then
                    begin
                      if (etcoFoldingPriority in Options) xor ((Button=mbMiddle) xor (ssModifier in Shift))
                        then include(Flags, ectfDragFolding)
                        else exclude(Flags, ectfDragFolding);
                    end else
                    exclude(Flags, ectfDragFolding);
                end else
                begin  { not movable }
                  if etoCanBeFolded in Tabs[aTab].Options
                    then ClickedTabIndex:=aTab
                    else ClickedTabIndex:=-1;
                  include(Flags, ectfDragFolding);
                end;
              if ClickedTabIndex>=0 then BeginDrag(False, 8);
            end;
        end else
        ClickedTabIndex:=-1;
    end;
end;

procedure TCustomECTabCtrl.MouseLeave;
begin
  inherited MouseLeave;
  TabHovered:=ethTabCaption;
  Hovered:=-1;
end;

procedure TCustomECTabCtrl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if ClickedTabIndex=-1
    then CalcHoveredTab(X, Y)
    else Hovered:=IndexOfTabAt(X, Y);
end;

procedure TCustomECTabCtrl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Button=mbLeft then
    begin
      if ectfCloseBtnDown in Flags then
        begin
          if assigned(OnCloseTabClicked) then OnCloseTabClicked(self);
          if etoCloseable in Tabs[Hovered].Options then
            begin
              DeleteTab(Hovered);
              CalcHoveredTab(X, Y);
            end;
          exclude(Flags, ectfCloseBtnDown);
        end else
        if RowCount>1 then CalcHoveredTab(X, Y);
      ClickedTabIndex:=-1;
    end else
    if (Button=mbMiddle) and (ClickedTabIndex>=0) then EndDrag(True);
end;

procedure TCustomECTabCtrl.MoveNext(AKeepVisible: Boolean);
var aNext: Integer;
begin
  if VisibleTabIndex<(length(VisibleTabs)-1) then
    begin
      aNext:=VisibleTabs[VisibleTabIndex+1].Index;
      if AKeepVisible then
        begin
          BeginUpdate;
          if IsTabVisible(TabIndex) then
            if IsSameVisiRow(VisibleTabIndex, VisibleTabIndex+1) then
              begin
                {$IFDEF DBGTAB} DebugLn('TCustomECTabCtrl.MoveNext/IsSameVisiRow'); {$ENDIF}
                if (aNext<Tabs.Count) and not IsTabVisible(aNext) then ScrollRight;
              end else
              MakeTabAvailable(aNext, False);
        end;
      MoveTab(TabIndex, aNext);
      if AKeepVisible then EndUpdate;
    end;
end;

procedure TCustomECTabCtrl.MovePrevious(AKeepVisible: Boolean);
var aPrevious: Integer;
begin
  if VisibleTabIndex>0 then
    begin
      aPrevious:=VisibleTabs[VisibleTabIndex-1].Index;
      if AKeepVisible then
        begin
          BeginUpdate;
          if IsTabVisible(TabIndex) then
            if IsSameVisiRow(VisibleTabIndex, VisibleTabIndex-1) then
              begin
                {$IFDEF DBGTAB} DebugLn('TCustomECTabCtrl.MovePrevious/IsSameVisiRow'); {$ENDIF}
                if (aPrevious>=0) and not IsTabVisible(aPrevious) then ScrollLeft;
              end else
              MakeTabAvailable(aPrevious, False);
        end;
      MoveTab(TabIndex, aPrevious);
      if AKeepVisible then EndUpdate;
    end;
end;

procedure TCustomECTabCtrl.MoveTab(AFrom, ATo: Integer);
var aTabIndex: Integer;
begin
  Tabs[AFrom].Index:=ATo;
  aTabIndex:=TabIndex;
  if AFrom=aTabIndex
    then TabIndex:=ATo
    else if (aTabIndex>AFrom) and (aTabIndex<=ATo)
           then TabIndex:=aTabIndex-1
           else if (aTabIndex>=ATo) and (aTabIndex<AFrom)
                  then TabIndex:=aTabIndex+1;
end;

procedure TCustomECTabCtrl.Paint;
var aDetails: TThemedElementDetails;
    aFlags: Cardinal;
    bEnabled, bR2L: Boolean;

  procedure PaintContent(AIndex: Integer);
  var aPos: SmallInt;
      aTab: TECTab;
  begin
    aTab:=VisibleTabs[AIndex].Tab;
    if etfHasImage in aTab.Flags then
      ThemeServices.DrawIcon(Canvas, aDetails, aTab.ImagePoint, Images, aTab.ImageIndex);
    if aTab.FoldedTabs.Count>0 then
      begin
        Canvas.Brush.Style:=bsSolid;
        if bEnabled then
          begin
            if (VisibleTabs[AIndex].Index<>Hovered) or (TabHovered<>ethDropDownGlyph)
              then Canvas.Pen.Color:=clBtnText
              else Canvas.Pen.Color:=clActiveCaption;
          end else
          Canvas.Pen.Color:=GetMergedColor(clBtnText, clBtnFace, 0.67);
        Canvas.DrawGlyph(aTab.DropDownRect, cDropDownGlyph[TabPosition], eisEnabled);
        Canvas.Pen.Color:=clBtnShadow;
        if TabPosition in [tpTop, tpBottom] then
          begin
            if not bR2L
              then aPos:=aTab.DropDownRect.Left
              else aPos:=aTab.DropDownRect.Right-1;
            Canvas.Line(aPos, aTab.BoundRect.Top+cContentIndent, aPos, aTab.BoundRect.Bottom-cCloseBtnBorder);
          end else
          begin
            aPos:=aTab.DropDownRect.Top;
            Canvas.Line(aTab.BoundRect.Left+cContentIndent, aPos, aTab.BoundRect.Right-cContentIndent, aPos);
          end;
        if AIndex=VisibleTabIndex
          then Canvas.Pen.Color:=clBtnHighlight
          else Canvas.Pen.Color:=clBtnFace;
        if TabPosition in [tpTop, tpBottom]
          then Canvas.Line(aPos+1, aTab.BoundRect.Top+cContentIndent, aPos+1, aTab.BoundRect.Bottom-cCloseBtnBorder)
          else Canvas.Line(aTab.BoundRect.Left+cContentIndent, aPos+1, aTab.BoundRect.Right-cContentIndent, aPos+1);
      end;
    if etfHasCloseBtn in aTab.Flags then
      begin;
        aPos:=aTab.CloseBtnRect.Left+cCloseBtnBorder;
        if bEnabled then
          begin
            if (VisibleTabs[AIndex].Index=Hovered) and (TabHovered=ethCloseButton) then
              begin
                if not (ectfCloseBtnDown in Flags)
                  then Canvas.Draw(aPos, aTab.CloseBtnRect.Top+cCloseBtnBorder, GlyphCloseHigh)
                  else Canvas.Draw(aPos, aTab.CloseBtnRect.Top+cCloseBtnBorder, GlyphClose);
              end else
              begin
                if AIndex<>VisibleTabIndex
                  then Canvas.Draw(aPos, aTab.CloseBtnRect.Top+cCloseBtnBorder, GlyphCloseInact)
                  else Canvas.Draw(aPos, aTab.CloseBtnRect.Top+cCloseBtnBorder, GlyphClose);
              end;
          end else
          Canvas.Draw(aPos, aTab.CloseBtnRect.Top+cCloseBtnBorder, GlyphCloseDis);
      end;
    if etfParentFont in aTab.Flags then
      begin
        Canvas.Font.Color:=Font.Color;
        Canvas.Font.Size:=Font.Size;
        Canvas.Font.Style:=Font.Style;
      end else
      begin
        Canvas.Font.Color:=aTab.FontOptions.FontColor;
        Canvas.Font.Size:=aTab.FontOptions.FontSize;
        Canvas.Font.Style:=aTab.FontOptions.FontStyles;
      end;
    Canvas.Brush.Style:=bsClear;
    if TabPosition in [tpTop, tpBottom]
      then ThemeServices.DrawText(Canvas, aDetails, aTab.Text, aTab.CaptionRect, aFlags, 0)
      else
      begin
        if not bEnabled then
          Canvas.Font.Color:=GetMergedColor(ColorToRGB(clBtnText), ColorToRGB(clBtnFace), 0.6);
        Canvas.TextOut(aTab.CaptionRect.Left, aTab.CaptionRect.Bottom, aTab.Text);
      end;
  end;

var aRect: TRect;

  procedure DeflateFocusRect(ALeft, ATop, ARight, ABottom: Integer);
  begin
    inc(aRect.Left, ALeft);
    inc(aRect.Top, ATop);
    dec(aRect.Right, ARight);
    dec(aRect.Bottom, ABottom);
  end;

var i, aFrom, aRowCountM1: Integer;
    aColor: TColor;
    aRow: SmallInt;
    b: Boolean;
const caAlignment: array [Boolean, TAlignment] of Cardinal = ((DT_LEFT, DT_RIGHT, DT_CENTER),
         (DT_RIGHT or DT_RTLREADING, DT_LEFT or DT_RTLREADING, DT_CENTER or DT_RTLREADING));
      cFocusIndent: SmallInt = 3;
begin
  {$IFDEF DBGTAB} DebugLn('TCustomECTabCtrl.Paint'); {$ENDIF}
  inherited Paint;
  Canvas.Font.Assign(Font);
  if TabPosition in [tpTop, tpBottom]
    then Canvas.Font.Orientation:=0
    else Canvas.Font.Orientation:=900;
  Canvas.Pen.Width:=1;
  Canvas.Pen.Style:=psSolid;
  bR2L:=IsRightToLeft;
  bEnabled:=IsEnabled;
  if bEnabled
    then aDetails:=ThemeServices.GetElementDetails(tbPushButtonNormal)
    else aDetails:=ThemeServices.GetElementDetails(tbPushButtonDisabled);
  aFlags:= DT_VCENTER or DT_SINGLELINE or DT_END_ELLIPSIS or caAlignment[bR2L, Alignment];
  Canvas.Clipping:=True;
  case TabPosition of
    tpTop: Canvas.ClipRect:=Rect(0, 0, Width, Height-BottomSize-1);
    tpBottom: Canvas.ClipRect:=Rect(0, BottomSize+1, Width, Height);
    tpLeft: Canvas.ClipRect:=Rect(0, 0, Width-BottomSize-1, Height);
    tpRight: Canvas.ClipRect:=Rect(BottomSize+1, 0, Width, Height);
  end;
  aRowCountM1:=RowCount-1;
  for aRow:=aRowCountM1 downto 0 do
    begin
      if VisiTabsInRow[aRow].InactBkgnd then
        begin
          aRect:=VisiTabsInRow[aRow].InactRect;
          i:=-2;
          case Style of
            eosButton: Canvas.DrawButtonBackground(aRect, eisPushed);
            eosPanel:
              begin
                Canvas.DrawPanelBackground(aRect, bvLowered, bvNone, 0, 1, clDefault, clDefault, clForm);
                i:=-1;
              end;
            eosThemedPanel: Canvas.DrawThemedPanelBkgnd(aRect);
          end;
          InflateRect(aRect, i, i);
          Canvas.Brush.Style:=bsDiagCross;
          Canvas.Brush.Color:=clBtnShadow;
          Canvas.FillRect(aRect);
        end;
      Canvas.Brush.Style:=bsSolid;
      aFrom:=VisiTabsInRow[aRow].From;
      for i:=aFrom to aFrom+VisiTabsInRow[aRow].Count-1 do
        if i<>VisibleTabIndex then
          begin
            aRect:=VisibleTabs[i].Tab.BoundRect;
            if TabPosition in [tpTop, tpBottom]
              then b:= ((aRect.Right>0) and (aRect.Left<Width))
              else b:= ((aRect.Bottom>0) and (aRect.Top<Height));
            if b then
              begin
                case Style of
                  eosButton: Canvas.DrawButtonBackground(VisibleTabs[i].Tab.PaintRect, eisPushed);
                  eosPanel:
                    begin
                      if VisibleTabs[i].Index<>Hovered
                        then aColor:=GetColorResolvingDefault(VisibleTabs[i].Tab.Color, clBtnFace)
                        else aColor:=GetColorResolvingDefault(ColorHighlighted, cl3DLight);
                      if not bEnabled then aColor:=GetMonochromaticColor(aColor);
                      Canvas.DrawPanelBackground(VisibleTabs[i].Tab.PaintRect,
                        bvNone, bvLowered, 0, 1, clDefault, clDefault, aColor);
                    end;
                  eosThemedPanel: Canvas.DrawThemedPanelBkgnd(VisibleTabs[i].Tab.PaintRect);
                end;
                PaintContent(i);
              end;
          end;
    end;
  Canvas.Pen.Color:=clBtnShadow;
  if VisibleTabIndex>=0 then
    begin
      aRect:=Tabs[TabIndex].BoundRect;
      if TabPosition in [tpTop, tpBottom]
        then b:= ((aRect.Right>0) and (aRect.Left<Width))
        else b:= ((aRect.Bottom>0) and (aRect.Top<Height));
      if b then
        begin
          case Style of
            eosButton: Canvas.DrawButtonBackground(Tabs[TabIndex].PaintRect, eisEnabled);
            eosPanel:
              begin
                aColor:=GetColorResolvingDefault(ColorActiveTab, cl3DLight);
                if not bEnabled then aColor:=GetMonochromaticColor(aColor);
                Canvas.DrawPanelBackground(Tabs[TabIndex].PaintRect,
                  bvNone, bvRaised, 0, 1, clDefault, clDefault, aColor);
              end;
           eosThemedPanel: Canvas.DrawThemedPanelBkgnd(Tabs[TabIndex].PaintRect);
          end;
          if Focused then
            begin
              case TabPosition of
               tpTop: DeflateFocusRect(cFocusIndent, cFocusIndent, cFocusIndent, 0);
               tpBottom: DeflateFocusRect(cFocusIndent, 0, cFocusIndent, cFocusIndent);
               tpLeft: DeflateFocusRect(cFocusIndent, cFocusIndent, 0, cFocusIndent);
               tpRight: DeflateFocusRect(0, cFocusIndent, cFocusIndent, cFocusIndent);
              end;
              LCLIntf.SetBkColor(Canvas.Handle, ColorToRGB(clBtnFace));
              LCLIntf.DrawFocusRect(Canvas.Handle, aRect);
              aRect:=Tabs[TabIndex].BoundRect;
            end;
        end;
      Canvas.Clipping:=False;
      if BottomSize>=cECNoBotomBevel then
        case TabPosition of
          tpTop:
            begin
              i:=Height-2-BottomSize;
              Canvas.Line(0, i, aRect.Left, i);
              Canvas.Line(aRect.Right, i, Width, i);
            end;
          tpBottom:
            begin
              i:=1+BottomSize;
              Canvas.Line(0, i, aRect.Left, i);
              Canvas.Line(aRect.Right, i, Width, i);
            end;
          tpLeft:
            begin
              i:=Width-2-BottomSize;
              Canvas.Line(i, 0, i, aRect.Top);
              Canvas.Line(i, aRect.Bottom, i, Height);
            end;
          tpRight:
            begin
              i:=1+BottomSize;
              Canvas.Line(i, 0, i, aRect.Top);
              Canvas.Line(i, aRect.Bottom, i, Height);
            end;
        end;  { case }
      if b then PaintContent(VisibleTabIndex);
    end else
    begin
      Canvas.Clipping:=False;
      if BottomSize>=-1 then
        case TabPosition of
          tpTop:
            begin
              i:=Height-2-BottomSize;
              Canvas.Line(0, i, Width, i);
            end;
          tpBottom:
            begin
              i:=1+BottomSize;
              Canvas.Line(0, i, Width, i);
            end;
          tpLeft:
            begin
              i:=Width-2-BottomSize;
              Canvas.Line(i, 0, i, Height);
            end;
          tpRight:
            begin
              i:=1+BottomSize;
              Canvas.Line(i, 0, i, Height);
            end;
        end;  { case }
    end;
  if BottomSize>=0 then
    begin
      Canvas.Pen.Color:=clBtnHiLight;
      case TabPosition of
        tpTop:
          begin
            i:=Height-1;
            Canvas.Line(0, i, Width, i);
          end;
        tpBottom: Canvas.Line(0, 0, Width, 0);
        tpLeft:
          begin
            i:=Width-1;
            Canvas.Line(i, 0, i, Height);
          end;
        tpRight: Canvas.Line(0, 0, 0, Height);
      end;  { case }
    end;
  Canvas.Clipping:=True;
end;

procedure TCustomECTabCtrl.RecalcInvalidate;
var bLoading: Boolean;
begin
  {$IFDEF DBGTAB} DebugLn('RecalcInv, UC=', inttostr(UpdateCount)); {$ENDIF}
  if UpdateCount=0 then
    begin
      bLoading:= (csLoading in ComponentState);
      if not bLoading then CalcLayoutAndTabSizes;
      if AutoSize and (ectfInvPrefSize in Flags) then
        begin
          InvalidatePreferredSize;
          AdjustSize;
        end;
      include(Flags, ectfInvPrefSize);
      if not bLoading then
        begin
          AlignButtons;
          Calculate;
        end;
      Invalidate;
    end;
end;

procedure TCustomECTabCtrl.Redraw;
begin
  if UpdateCount=0 then Invalidate;
end;

procedure TCustomECTabCtrl.SetHint(const Value: TTranslateString);
begin
  inherited SetHint(Value);
  if not (ectfLockHint in Flags) then DefHint:=Value;
end;

procedure TCustomECTabCtrl.ScrollLeft;
begin
  PosLeftRight:=PosLeftRight-1;
end;

procedure TCustomECTabCtrl.ScrollRight;
begin
  PosLeftRight:=PosLeftRight+1;
end;

procedure TCustomECTabCtrl.SelectNext;
var aNext: Integer;
    bLoop: Boolean;
begin
  aNext:=-1;
  bLoop:=False;
  if VisibleTabIndex<(length(VisibleTabs)-1)
    then aNext:=VisibleTabs[VisibleTabIndex+1].Index
    else if etcoLoopTabSelection in Options then
      begin
        aNext:=VisibleTabs[0].Index;
        bLoop:=True;
      end;
  if aNext>=0 then
    begin
      if BtnLeftUp.Visible then
        begin
          if IsTabVisible(TabIndex) and IsSameVisiRow(VisibleTabIndex, aNext) then
            begin
              if not IsTabVisible(aNext) then
                begin
                  if not bLoop
                    then FPosLeftRight:=Math.min(FPosLeftRight+1, PosLRMax)
                    else FPosLeftRight:=0;
                  DoBtnsEnabled;
                end;
            end else
            begin
              MakeTabAvailable(aNext, True);
              exit;  { Exit! }
            end;
        end;
      TabIndex:=aNext;
    end;
end;

procedure TCustomECTabCtrl.SelectPrevious;
var aPrevious: Integer;
    bLoop: Boolean;
begin
  aPrevious:=-1;
  bLoop:=False;
  if VisibleTabIndex>0
    then aPrevious:=VisibleTabs[VisibleTabIndex-1].Index
    else if etcoLoopTabSelection in Options then
      begin
        aPrevious:=VisibleTabs[length(VisibleTabs)-1].Index;
        bLoop:=True;
      end;
  if aPrevious>=0 then
    begin
      if BtnLeftUp.Visible then
        begin
          if IsTabVisible(TabIndex) and IsSameVisiRow(VisibleTabIndex, aPrevious) then
            begin
              if not IsTabVisible(aPrevious) then
                begin
                  if not bLoop
                    then FPosLeftRight:=Math.max(FPosLeftRight-1, 0)
                    else FPosLeftRight:=PosLRMax;
                  DoBtnsEnabled;
                end;
            end else
            begin
              MakeTabAvailable(aPrevious, True);
              exit;  { Exit! }
            end;
        end;
      TabIndex:=aPrevious;
    end;
end;

procedure TCustomECTabCtrl.SelectRowDown;
begin
  if RowIndex>0 then TabIndex:=Math.max(VisibleTabIndex-VisiTabsInRow[RowIndex].Count, VisibleTabs[0].Index);
end;

procedure TCustomECTabCtrl.SelectRowUp;
begin
  if RowIndex<(length(VisiTabsInRow)-1) then
    TabIndex:=Math.min(VisibleTabIndex+VisiTabsInRow[RowIndex].Count, VisibleTabs[length(VisibleTabs)-1].Index);
end;

procedure TCustomECTabCtrl.ShowDropDownMenu(AIndex: Integer);

  function CreateMIParams(const AText: string; AImgIdx: Integer; ATag: PtrInt; AVisible: Boolean; AOnClick: TNotifyEvent): TMenuItem;
  begin
    Result:=TMenuItem.Create(DropDownMenu);
    Result.Caption:=AText;
    if not AVisible then Result.Caption:=Result.Caption+' #';
    Result.ImageIndex:=AImgIdx;
    Result.Tag:=ATag;
    Result.OnClick:=AOnClick;
  end;

var bRev, bVert: Boolean;

  function CreateMIMoveParams(const AText, ATextVert: string; AOnClick, AOnClickRev: TNotifyEvent; AEnabled: Boolean): TMenuItem;
  begin
    Result:=TMenuItem.Create(DropDownMenu);
    Result.Enabled:=AEnabled;
    if not bVert
      then Result.Caption:=AText
      else Result.Caption:=ATextVert;
    if not bRev
      then Result.OnClick:=AOnClick
      else Result.OnClick:=AOnClickRev;
  end;

  procedure AddSeparator;
  var aMI: TMenuItem;
  begin
    aMI:=TMenuItem.Create(DropDownMenu);
    aMI.Caption:='-';
    DropDownMenu.Items.Add(aMI);
  end;

var aMI: TMenuItem;
    i, aVisiCountM1: Integer;
    bSep: Boolean;
    aDDPoint: TPoint;
begin
  DropDownMenu.Items.Clear;
  DropDownMenu.BidiMode:=BiDiMode;
  DropDownMenu.Images:=Images;
  DropDownMenu.OnClose:=@DropDownMenuClose;
  DropDownMenu.Tag:=AIndex;
  bSep:=False;
  if etoCanFold in Tabs[AIndex].Options then
    for i:=0 to Tabs[AIndex].FoldedTabs.Count-1 do
      if etoVisible in TECTab(Tabs[AIndex].FoldedTabs[i]).Options then
        with TECTab(Tabs[AIndex].FoldedTabs[i]) do
          DropDownMenu.Items.Add(CreateMIParams(Text, ImageIndex, ID, True, @MIFoldedTabClick));
  if DropDownMenu.Items.Count>0 then AddSeparator;
  if Tabs[AIndex].FoldedTabs.Count>0 then
    begin
      aMI:=TMenuItem.Create(DropDownMenu);
      with aMI do
        begin
          Caption:=rsETCUnfold;
          SubMenuImages:=Images;
          for i:=0 to Tabs[AIndex].FoldedTabs.Count-1 do
            Add(CreateMIParams(TECTab(Tabs[AIndex].FoldedTabs[i]).Text,
                               TECTab(Tabs[AIndex].FoldedTabs[i]).ImageIndex,
                               TECTab(Tabs[AIndex].FoldedTabs[i]).ID,
                               etoVisible in TECTab(Tabs[AIndex].FoldedTabs[i]).Options, @MIUnfoldClick));
        end;
      DropDownMenu.Items.Add(aMI);
      aMI:=TMenuItem.Create(DropDownMenu);
      with aMI do
        begin
          Caption:=rsETCUnfoldAll;
          OnClick:=@MIUnfoldAllClick;
        end;
      DropDownMenu.Items.Add(aMI);
      if not (etcoDropDownFoldingOnly in Options) then
        begin
          aMI:=TMenuItem.Create(DropDownMenu);
          with aMI do
            begin
              Caption:=rsETCCloseTab;
              SubMenuImages:=Images;
              for i:=0 to Tabs[AIndex].FoldedTabs.Count-1 do
                if etoCloseable in TECTab(Tabs[AIndex].FoldedTabs[i]).Options then
                  Add(CreateMIParams(TECTab(Tabs[AIndex].FoldedTabs[i]).Text,
                                     TECTab(Tabs[AIndex].FoldedTabs[i]).ImageIndex,
                                     TECTab(Tabs[AIndex].FoldedTabs[i]).ID,
                                     etoVisible in TECTab(Tabs[AIndex].FoldedTabs[i]).Options, @MICloseFoldedTabClick));
            end;
          if aMI.Count>0 then
            begin
              AddSeparator;
              DropDownMenu.Items.Add(aMI);
              if aMI.Count>=2 then
                DropDownMenu.Items.Add(CreateMIParams(rsETCCloseAll, -1, Tabs[AIndex].ID, True, @MICloseAllClick));
              bSep:=True;
            end else
            aMI.Free;
        end;
    end;
  if etoCanBeFolded in Tabs[AIndex].Options then
    begin
      aMI:=TMenuItem.Create(DropDownMenu);
      with aMI do
        begin
          Caption:=rsETCFoldTo;
          SubMenuImages:=Images;
          for i:=0 to length(VisibleTabs)-1 do
            if (VisibleTabs[i].Index<>AIndex) and (etoCanFold in VisibleTabs[i].Tab.Options) then
              Add(CreateMIParams(VisibleTabs[i].Tab.Text, VisibleTabs[i].Tab.ImageIndex,
                                 VisibleTabs[i].Tab.ID, True, @MIFoldToClick));
        end;
      if aMI.Count>0 then
        begin
          if bSep then AddSeparator;
          DropDownMenu.Items.Add(aMI);
          bSep:=True;
        end else
        aMI.Free;
    end;
  if not (etcoDropDownFoldingOnly in Options) then
    begin
      if etcoAddTabButton in Options then
        begin
          if bSep then AddSeparator;
          aMI:=TMenuItem.Create(DropDownMenu);
          with aMI do
            begin
              Caption:=rsETCAdd;
              Bitmap.Assign(GlyphAdd);
              OnClick:=@MIAddTab;
            end;
          DropDownMenu.Items.Add(aMI);
          bSep:=True;
        end;
      aVisiCountM1:=length(VisibleTabs)-1;
      if (aVisiCountM1>=1) and not (etcoFixedPosition in Options) then
        begin
          if bSep then AddSeparator;
          i:=0;
          while AIndex<>VisibleTabs[i].Index do
            inc(i);
          bVert:= (TabPosition in [tpLeft, tpRight]);
          bRev:= (etcoReversed in Options);
          if not bVert then bRev:= (bRev xor IsRightToLeft);
          aMI:=TMenuItem.Create(DropDownMenu);
          with aMI do
            begin
              Caption:=rsETCMoveTab;
              Add(CreateMIMoveParams(rsETCLeftmost, rsETCTopmost, @MIMoveLeftmostClick, @MIMoveRightmostClick, (i>0)));
              Add(CreateMIMoveParams(rsETCLeft, rsETCUp, @MIMoveLeftClick, @MIMoveRightClick, (i>0)));
              Add(CreateMIMoveParams(rsETCRight, rsETCDown, @MIMoveRightClick, @MIMoveLeftClick, (i<aVisiCountM1)));
              Add(CreateMIMoveParams(rsETCRightmost, rsETCBottmomost, @MIMoveRightmostClick, @MIMoveLeftmostClick, (i<aVisiCountM1)));
            end;
          DropDownMenu.Items.Add(aMI);
          bSep:=True;
        end;
      if etoCloseable in Tabs[AIndex].Options then
        begin
          if bSep then AddSeparator;
          aMI:=TMenuItem.Create(DropDownMenu);
          with aMI do
            begin
              Caption:=rsETCClose;
              Bitmap.Assign(GlyphClose);
              Tag:=AIndex;
              OnClick:=@MICloseTabClick;
            end;
          DropDownMenu.Items.Add(aMI);
        end;
    end;
  if TabPosition in [tpTop, tpBottom] then
    begin
      DropDownMenu.Alignment:=paLeft;
      if not IsRightToLeft
        then aDDPoint.X:=Tabs[AIndex].BoundRect.Left
        else aDDPoint.X:=Tabs[AIndex].BoundRect.Right;
      aDDPoint.Y:=Tabs[AIndex].BoundRect.Bottom;
    end else
    begin  { tpLeft, tpRight }
      if (TabPosition=tpLeft) xor DropDownMenu.IsRightToLeft
        then DropDownMenu.Alignment:=paLeft
        else DropDownMenu.Alignment:=paRight;
      if TabPosition=tpLeft
        then aDDPoint.X:=Tabs[AIndex].BoundRect.Right
        else aDDPoint.X:=Tabs[AIndex].BoundRect.Left;
      aDDPoint.Y:=Tabs[AIndex].BoundRect.Top;
    end;
  aDDPoint:=ClientToScreen(aDDPoint);
  DropDownMenu.PopUp(aDDPoint.X, aDDPoint.Y);
end;

procedure TCustomECTabCtrl.SwitchFoldedTab(AIndex, AFolder: Integer);
begin
  BeginUpdate;
  DoSwitchFoldedTab(AIndex, AFolder);
  if not (etcoKeepOrigPosFoldTab in Options) then
    begin
      Tabs[AIndex].Index:=AFolder;
      TabIndex:=AFolder;
    end else
    TabIndex:=AIndex;
  EndUpdate;
end;

procedure TCustomECTabCtrl.TabChanged(ARecalculate: Boolean);
var i: Integer;
begin
  {$IFDEF DBGTAB} DebugLn('TECTabCtrl.TabChanged '+booltostr(ARecalculate, 'T', 'F')); {$ENDIF}
  if not ARecalculate then
    begin
      for i:=0 to length(VisibleTabs)-1 do
        if (etfParentFont in VisibleTabs[i].Tab.Flags) and
          (VisibleTabs[i].Tab.FontOptions.FontColor<>Font.Color)
          then exclude(VisibleTabs[i].Tab.Flags, etfParentFont);
      Redraw;
    end else
    RecalcInvalidate;
end;

procedure TCustomECTabCtrl.UnfoldAllTabs(AFolder: Integer);
var i: Integer;
begin
  for i:=0 to Tabs[AFolder].FoldedTabs.Count-1 do
    exclude(TECTab(Tabs[AFolder].FoldedTabs[i]).Flags, etfFolded);
  Tabs[AFolder].FoldedTabs.Clear;
  RecalcInvalidate;
end;

procedure TCustomECTabCtrl.UnfoldTab(AID: Integer; AFolder: Integer = -1);
var i, j, aIndex: Integer;
begin
  aIndex:=Tabs.IDToIndex(AID);
  if etfFolded in Tabs[aIndex].Flags then
    begin
      if AFolder=-1 then
        begin
          for i:=0 to Tabs.Count-1 do
            for j:=0 to Tabs[i].FoldedTabs.Count-1 do
              if TECTab(Tabs[i].FoldedTabs[j])=Tabs[aIndex] then
                begin
                  Tabs[i].FoldedTabs.Delete(j);
                  exclude(Tabs[aIndex].Flags, etfFolded);
                  RecalcInvalidate;
                  exit;  { Exit! }
                end;
        end else
        begin
          for j:=0 to Tabs[AFolder].FoldedTabs.Count-1 do
            if TECTab(Tabs[AFolder].FoldedTabs[j])=Tabs[aIndex] then
              begin
                Tabs[AFolder].FoldedTabs.Delete(j);
                exclude(Tabs[aIndex].Flags, etfFolded);
                RecalcInvalidate;
                exit;  { Exit! }
              end;
        end;
    end;
end;

procedure TCustomECTabCtrl.WMSize(var Message: TLMSize);
begin
  inherited WMSize(Message);
  RecalcInvalidate;
end;

{ TCustomECTabCtrl.G/Setters }

procedure TCustomECTabCtrl.SetAlignment(AValue: TAlignment);
begin
  if FAlignment=AValue then exit;
  FAlignment:=AValue;
  RecalcInvalidate;
end;

procedure TCustomECTabCtrl.SetBottomSize(AValue: SmallInt);
begin
  if AValue<cECNoBotomBevel then AValue:=cECNoBotomBevel;
  if FBottomSize=AValue then exit;
  FBottomSize:=AValue;
  include(Flags, ectfRealignButtons);
  InvalidatePreferredSize;
  RecalcInvalidate;
end;

procedure TCustomECTabCtrl.SetColorActiveTab(AValue: TColor);
begin
  if FColorActiveTab=AValue then exit;
  FColorActiveTab:=AValue;
  if Style=eosPanel then Redraw;
end;

procedure TCustomECTabCtrl.SetColorHighlighted(AValue: TColor);
begin
  if FColorHighlighted=AValue then exit;
  FColorHighlighted:=AValue;
  if Style=eosPanel then Redraw;
end;

procedure TCustomECTabCtrl.SetHovered(AValue: Integer);
begin
  if FHovered=AValue then exit;
  FHovered:=AValue;
  {$IFDEF DBGTAB} DebugLn('TECTabCtrl.SetHovered: '+inttostr(AValue)); {$ENDIF}
  if ShowHint then
    begin
      Application.CancelHint;
      ChangeHint(AValue);
      Application.ActivateHint(Mouse.CursorPos);
    end;
  if (TabHovered<>ethTabCaption) or (Style=eosPanel) then Redraw;
end;

procedure TCustomECTabCtrl.SetImages(AValue: TCustomImageList);
begin
  if FImages=AValue then exit;
  FImages:=AValue;
  if length(VisibleTabs)>0 then RecalcInvalidate;
end;

procedure TCustomECTabCtrl.SetMaxRowCount(AValue: TECTCMaxRows);
begin
  if FMaxRowCount=AValue then exit;
  FMaxRowCount:=AValue;
  SetLength(VisiTabsInRow, AValue);
  if length(VisibleTabs)>0 then RecalcInvalidate;
end;

procedure TCustomECTabCtrl.SetOptions(AValue: TECTabCtrlOptions);
var aChangedOpts: TECTabCtrlOptions;
const cRecalcOpts = [etcoAddTabButton, etcoAutoSizeTabsHeight, etcoAutoSizeTabsWidth,
                     etcoCloseBtnActiveOnly, etcoDontEnlargeTopRow, etcoDontShrinkActiveTab,
                     etcoEnlargeTabsToFit, etcoReversed, etcoShrinkTabsToMin];
begin
  aChangedOpts:= FOptions><AValue;
  if FOptions=AValue then exit;
  FOptions:=AValue;
  if etcoAddTabButton in aChangedOpts then
    begin
      if etcoAddTabButton in AValue then
        begin
          BtnAdd:=TCustomECSpeedBtn.Create(self);
          with BtnAdd do
            begin
              GlyphDesign:=egdMathBigPlus;
              Transparent:=False;
              OnMouseDown:=@BtnAddMouseDown;
              Parent:=self;
            end;
        end else
        FreeAndNil(BtnAdd);
    end;
  if (aChangedOpts*cRecalcOpts)<>[] then
    begin
      DesignLeftRightBtns;
      include(Flags, ectfRealignButtons);
      RecalcInvalidate;
    end;
end;

procedure TCustomECTabCtrl.SetPosLeftRight(AValue: Integer);
begin
  if AValue<0
    then AValue:=0
    else if AValue>PosLRMax then AValue:=PosLRMax;
  if FPosLeftRight=AValue then exit;
  FPosLeftRight:=AValue;
  exclude(Flags, ectfInvPrefSize);
  RecalcInvalidate;
  DoBtnsEnabled;
end;

procedure TCustomECTabCtrl.SetPosLRMax(AValue: Integer);
begin
  if PosLeftRight>AValue then FPosLeftRight:=AValue;
  if FPosLRMax=AValue then exit;
  FPosLRMax:=AValue;
  DoBtnsEnabled;
end;

procedure TCustomECTabCtrl.SetStyle(AValue: TObjectStyle);
begin
  if FStyle=AValue then exit;
  FStyle:=AValue;
  RecalcInvalidate;
end;

procedure TCustomECTabCtrl.SetTabActiveRise(AValue: Integer);
begin
  if FTabActiveRise=AValue then exit;
  FTabActiveRise:=AValue;
  include(Flags, ectfRealignButtons);
  RecalcInvalidate;
end;

procedure TCustomECTabCtrl.SetTabFixedWidth(AValue: Integer);
begin
  if FTabFixedWidth=AValue then exit;
  FTabFixedWidth:=AValue;
  if not (etcoAutoSizeTabsWidth in Options) then RecalcInvalidate;
end;

procedure TCustomECTabCtrl.SetTabHeight(AValue: Integer);
begin
  if (FTabHeight=AValue) or (etcoAutoSizeTabsHeight in Options) then exit;
  FTabHeight:=AValue;
  include(Flags, ectfRealignButtons);
  RecalcInvalidate;
end;

procedure TCustomECTabCtrl.SetTabHovered(AValue: TECTabHovered);
begin
  if FTabHovered=AValue then exit;
  FTabHovered:=AValue;
  {$IFDEF DBGTAB} DebugLn('TECTabCtrl.SetTabHovered '+inttostr(Integer(AValue))); {$ENDIF}
  Redraw;
end;

procedure TCustomECTabCtrl.SetTabIndex(AValue: Integer);
var bAllow: Boolean;
begin
  if FTabIndex=AValue then exit;
  bAllow:=True;
  if assigned(OnChanging) then OnChanging(self, bAllow);
  if bAllow then
    begin
      if (AValue>=0) and (FTabIndex>=0) and (FTabIndex<Tabs.Count-1)
        then Tabs[AValue].PreviousID:=Tabs[FTabIndex].ID;
      FTabIndex:=AValue;
      if assigned(OnChange) then OnChange(self);
      exclude(Flags, ectfInvPrefSize);
      RecalcInvalidate;
    end;
end;

procedure TCustomECTabCtrl.SetTabMaxWidth(AValue: Integer);
begin
  if FTabMaxWidth=AValue then exit;
  FTabMaxWidth:=AValue;
  if etcoAutoSizeTabsWidth in Options then RecalcInvalidate;
end;

procedure TCustomECTabCtrl.SetTabMinWidth(AValue: Integer);
begin
  if FTabMinWidth=AValue then exit;
  FTabMinWidth:=AValue;
  if ([etcoAutoSizeTabsWidth, etcoShrinkTabsToMin]*Options)<>[] then RecalcInvalidate;
end;

procedure TCustomECTabCtrl.SetTabPosition(AValue: TTabPosition);
begin
  if FTabPosition=AValue then exit;
  FTabPosition:=AValue;
  DesignLeftRightBtns;
  include(Flags, ectfRealignButtons);
  RecalcInvalidate;
end;

procedure TCustomECTabCtrl.SetTabs(AValue: TECTabs);
begin
  if AValue=FTabs then exit;
  FTabs.Assign(AValue);
  RecalcInvalidate;
end;

end.


