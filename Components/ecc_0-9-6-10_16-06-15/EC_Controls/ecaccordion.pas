{**************************************************************************************************
 This file is part of the Eye Candy Controls (EC-C)

  Copyright (C) 2016 Vojtěch Čihák, Czech Republic

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

unit ECAccordion;
{$mode objfpc}{$H+}

//{$DEFINE DBGACC}  {don't remove, just comment}

interface

uses
  Classes, SysUtils, Controls, Forms, Graphics, LMessages, LCLIntf, LCLType,
  {$IFDEF DBGACC} LCLProc, {$ENDIF} ImgList, Math, Themes, Types, ECTypes;

type
  {$PACKENUM 2}
  TAccordionStyle = (easHeader, easButton, easGradient);
  { event }
  TDrawAccordionItem = procedure (Sender: TObject; AIndex: Integer; ARect: TRect; var AHandled: Boolean) of object;

  TCustomECAccordion = class;

  { TAccordionItem }
  TAccordionItem = class(TWinControl)
  private
    FAccordion: TCustomECAccordion;
    FImageIndex: TImageIndex;
    FOrder: Word;
    FPreferredHeight: Integer;
    procedure SetAccordion(AValue: TCustomECAccordion);
    procedure SetImageIndex(AValue: TImageIndex);
    procedure SetOrder(AValue: Word);
    procedure SetPreferredHeight(AValue: Integer);
  protected const
    cDefPrefHeight = 100;
  protected
    ChangingParentFlag: Boolean;
    procedure IntegrateToAccordion(AAccordion: TCustomECAccordion);
    procedure SetParent(NewParent: TWinControl); override;
    procedure TextChanged; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property Accordion: TCustomECAccordion read FAccordion write SetAccordion;
  published
    property Caption;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property Order: Word read FOrder write SetOrder nodefault;
    property PreferredHeight: Integer read FPreferredHeight write SetPreferredHeight default cDefPrefHeight;
  end;

  { TCustomECAccordion }
  TCustomECAccordion = class(TCustomControl)
  private
    FAlignment: TAlignment;
    FColorGradBottom: TColor;
    FColorGradTop: TColor;
    FFullExpand: Boolean;
    FGlyphCollapse: TGlyphDesign;
    FGlyphExpand: TGlyphDesign;
    FImages: TCustomImageList;
    FItemHeight: SmallInt;
    FItemIndex: SmallInt;
    FItems: TFPList;
    FOnAdding: TIntegerEvent;
    FOnChange: TNotifyEvent;
    FOnDrawItem: TDrawAccordionItem;
    FSelectedFontOptions: TFontOptions;
    FSpacing: SmallInt;
    FStyle: TAccordionStyle;
    function GetActiveItem: TAccordionItem;
    function GetItem(AIndex: Integer): TAccordionItem;
    procedure SetActiveItem(AValue: TAccordionItem);
    procedure SetAlignment(AValue: TAlignment);
    procedure SetColorGradBottom(AValue: TColor);
    procedure SetColorGradTop(AValue: TColor);
    procedure SetFullExpand(AValue: Boolean);
    procedure SetGlyphCollapse(AValue: TGlyphDesign);
    procedure SetGlyphExpand(AValue: TGlyphDesign);
    procedure SetImages(AValue: TCustomImageList);
    procedure SetItemHeight(AValue: SmallInt);
    procedure SetItemIndex(AValue: SmallInt);
    procedure SetItems(AValue: TFPList);
    procedure SetSelectedFontOptions(AValue: TFontOptions);
    procedure SetSpacing(AValue: SmallInt);
    procedure SetStyle(AValue: TAccordionStyle);
  protected const
    cDefGlyphCollapse = egdArrowUp;
    cDefGlyphExpand = egdArrowDown;
    cDefItemHeight = 21;
    cDefSpacing = 5;
  protected
    Hovered: Integer;
    ItemIndexLFM: Integer;
    ExpandedHeight: Integer;
    procedure AddToList(AItem: TAccordionItem);
    procedure AnchorAccordionItem;
    procedure ChangeOrder(ACurrentPos: Word; var ANewPos: Word);
    function ChildClassAllowed(ChildClass: TClass): Boolean; override;
    procedure DeleteFromList(AItem: TAccordionItem);
    function DialogChar(var Message: TLMKey): Boolean; override;
    procedure DoOnAddEvent(AIndex: Integer);
    procedure InvalidateNonUpdated;
    class function GetControlClassDefaultSize: TSize; override;
    procedure Loaded; override;
    procedure MakeNewOrder;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
  public
    UpdateCount: SmallInt;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function AddItem(AOwner: TComponent; AActivate: Boolean = True): TAccordionItem;
    procedure BeginUpdate;
    procedure ClearItems;
    procedure DeleteItem(AIndex: Integer); overload;
    procedure DeleteItem(AItem: TAccordionItem); overload;
    procedure EndUpdate;
    procedure FindNextItem;
    procedure FindPreviousItem;
    function InsertItem(AOwner: TComponent; AIndex: Integer; AActivate: Boolean=True): TAccordionItem;
    procedure MoveItemDown;
    procedure MoveItemUp;
    property ActiveItem: TAccordionItem read GetActiveItem write SetActiveItem;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property ColorGradBottom: TColor read FColorGradBottom write SetColorGradBottom default clDefault;
    property ColorGradTop: TColor read FColorGradTop write SetColorGradTop default clDefault;
    property FullExpand: Boolean read FFullExpand write SetFullExpand default False;
    property GlyphCollapse: TGlyphDesign read FGlyphCollapse write SetGlyphCollapse default cDefGlyphCollapse;
    property GlyphExpand: TGlyphDesign read FGlyphExpand write SetGlyphExpand default cDefGlyphExpand;
    property Images: TCustomImageList read FImages write SetImages;
    property ItemHeight: SmallInt read FItemHeight write SetItemHeight default cDefItemHeight;
    property ItemIndex: SmallInt read FItemIndex write SetItemIndex default -1;
    property Item[AIndex: Integer]: TAccordionItem read GetItem;
    property Items: TFPList read FItems write SetItems;
    property OnAdd: TIntegerEvent read FOnAdding write FOnAdding;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDrawItem: TDrawAccordionItem read FOnDrawItem write FOnDrawItem;
    property SelectedFontOptions: TFontOptions read FSelectedFontOptions write SetSelectedFontOptions;
    property Spacing: SmallInt read FSpacing write SetSpacing default cDefSpacing;
    property Style: TAccordionStyle read FStyle write SetStyle default easHeader;
  end;

  { TECAccordion }
  TECAccordion = class(TCustomECAccordion)
  published
    property Align;
    property Alignment;
    property Anchors;
    property BiDiMode;
    property BorderSpacing;
    property Color;
    property ColorGradBottom;
    property ColorGradTop;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FullExpand;
    property GlyphCollapse;
    property GlyphExpand;
    property Images;
    property ItemHeight;
    property ItemIndex;
    property OnAdd;
    property OnChange;
    property OnChangeBounds;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
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
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property SelectedFontOptions;
    property ShowHint;
    property Spacing;
    property Style;
    property Visible;
  end;

implementation

{ TAccordionItem }

constructor TAccordionItem.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ControlStyle:=ControlStyle+[csAcceptsControls, csNoFocus, csOpaque, csReplicatable, csSetCaption]
                            -[csNoDesignSelectable];
  SetSubComponent(True);
  PreferredHeight:=cDefPrefHeight;
  FImageIndex:=-1;
end;

destructor TAccordionItem.Destroy;
begin
  Accordion:=nil;
  inherited Destroy;
end;

procedure TAccordionItem.IntegrateToAccordion(AAccordion: TCustomECAccordion);
begin
  AnchorParallel(akLeft, 0, AAccordion);
  AnchorParallel(akRight, 0, AAccordion);
  AAccordion.AddToList(self);
end;

procedure TAccordionItem.SetParent(NewParent: TWinControl);
begin
  if (NewParent=nil) or (NewParent.InheritsFrom(TCustomECAccordion)) then
    begin
      inherited SetParent(NewParent);
      if not ChangingParentFlag then Accordion:=TCustomECAccordion(NewParent);
    end;
end;

procedure TAccordionItem.TextChanged;
begin
  inherited TextChanged;
  if assigned(Accordion) then Accordion.InvalidateNonUpdated;
end;

{ TAccordionItem.Setters }

procedure TAccordionItem.SetAccordion(AValue: TCustomECAccordion);
begin
  if FAccordion=AValue then exit;
  if assigned(FAccordion) then FAccordion.DeleteFromList(self);
  FAccordion:=AValue;
  ChangingParentFlag:=True;
  Parent:=AValue;
  ChangingParentFlag:=False;
  if assigned(AValue) then IntegrateToAccordion(AValue);
end;

procedure TAccordionItem.SetImageIndex(AValue: TImageIndex);
begin
  if FImageIndex=AValue then exit;
  FImageIndex:=AValue;
  if assigned(Accordion) then Accordion.InvalidateNonUpdated;
end;

procedure TAccordionItem.SetOrder(AValue: Word);
begin
  if FOrder=AValue then exit;
  if not (csLoading in ComponentState) then
    if assigned(Accordion) then Accordion.ChangeOrder(FOrder, AValue);
  FOrder:=AValue;
end;

procedure TAccordionItem.SetPreferredHeight(AValue: Integer);
begin
  if FPreferredHeight=AValue then exit;
  FPreferredHeight:=AValue;
  if assigned(Accordion) then Accordion.InvalidateNonUpdated;
end;

{ TCustomECAccordion }

constructor TCustomECAccordion.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ControlStyle:=ControlStyle+[csAcceptsControls, csNoFocus]-[csOwnedChildrenNotSelectable];
  FColorGradBottom:=clDefault;
  FColorGradTop:=clDefault;
  FGlyphCollapse:=cDefGlyphCollapse;
  FGlyphExpand:=cDefGlyphExpand;
  Hovered:=-1;
  FItemHeight:=cDefItemHeight;
  FItemIndex:=-1;
  ItemIndexLFM:=-1;
  FItems:=TFPList.Create;
  FSelectedFontOptions:=TFontOptions.Create(self);
  with FSelectedFontOptions do
    begin
      FontStyles:=[fsBold];
      OnRecalcRedraw:=@InvalidateNonUpdated;
      OnRedraw:=@InvalidateNonUpdated;
    end;
  FSpacing:=cDefSpacing;
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, cx, cy);
  AccessibleRole:=larTabControl;
end;

destructor TCustomECAccordion.Destroy;
begin
  FreeAndNil(FItems);
  FreeAndNil(FSelectedFontOptions);
  inherited Destroy;
end;

function TCustomECAccordion.AddItem(AOwner: TComponent; AActivate: Boolean = True): TAccordionItem;
begin
  Result:=TAccordionItem.Create(AOwner);
  Result.Accordion:=self;
  if AActivate
    then ItemIndex:=Items.Count-1
    else AnchorAccordionItem;
end;

procedure TCustomECAccordion.AddToList(AItem: TAccordionItem);
begin
  Items.Add(AItem);
  AItem.FOrder:=Items.Count-1;
  DoOnAddEvent(AItem.Order);
end;

procedure TCustomECAccordion.AnchorAccordionItem;
var aExpHeight, aTop: Integer;
begin
  if ItemIndex>=0 then
    begin
      aExpHeight:=Height-Items.Count*ItemHeight;
      if not FullExpand then aExpHeight:=Math.min(Item[ItemIndex].PreferredHeight, aExpHeight);
      ExpandedHeight:=aExpHeight;
      aTop:=(ItemIndex+1)*ItemHeight;
      Item[ItemIndex].AnchorParallel(akTop, aTop, self);
      Item[ItemIndex].AnchorParallel(akBottom, Height-aExpHeight-aTop, self);
    end else
    ExpandedHeight:=0;
  InvalidateNonUpdated;
end;

procedure TCustomECAccordion.BeginUpdate;
begin
  inc(UpdateCount);
end;

procedure TCustomECAccordion.ClearItems;
var i: Integer;
begin
  ItemIndex:=-1;
  BeginUpdate;
  for i:=Items.Count-1 downto 0 do
    DeleteItem(i);
  EndUpdate;
end;

procedure TCustomECAccordion.ChangeOrder(ACurrentPos: Word; var ANewPos: Word);
var aActiveItem: TAccordionItem;
begin
  if ANewPos>Items.Count-1 then ANewPos:=Items.Count-1;
  if ANewPos=ACurrentPos then exit;  { Exit! }
  aActiveItem:=ActiveItem;
  Items.Move(ACurrentPos, ANewPos);
  MakeNewOrder;
  BeginUpdate;
  ItemIndex:=-1;
  EndUpdate;
  ItemIndex:=Items.IndexOf(aActiveItem);
end;

function TCustomECAccordion.ChildClassAllowed(ChildClass: TClass): Boolean;
begin
  Result:= (ChildClass=TAccordionItem);
end;

procedure TCustomECAccordion.DeleteFromList(AItem: TAccordionItem);
begin
  Items.Delete(Items.IndexOf(AItem));
  MakeNewOrder;
end;

procedure TCustomECAccordion.DeleteItem(AIndex: Integer);
var aItem: TAccordionItem;
begin
  if AIndex=ItemIndex
    then ItemIndex:=-1
    else if AIndex<ItemIndex then dec(FItemIndex);
  aItem:=Item[AIndex];
  FreeAndNil(aItem);
  AnchorAccordionItem;
end;

procedure TCustomECAccordion.DeleteItem(AItem: TAccordionItem);
begin
  DeleteItem(Items.IndexOf(AItem));
end;

function TCustomECAccordion.DialogChar(var Message: TLMKey): Boolean;
var i: SmallInt;
begin
  Result:=False;
  if Message.Msg=LM_SYSCHAR then
    begin
      if IsEnabled and IsVisible then
        begin
          for i:=0 to Items.Count-1 do
            if IsAccel(Message.CharCode, TAccordionItem(Items[i]).Caption) then
              begin
                if i<>ItemIndex
                  then ItemIndex:=i
                  else ItemIndex:=-1;
                SetFocus;
                Result:=True;
                exit;  { Exit! }
              end;
          Result:=inherited DialogChar(Message);
        end;
    end;
end;

procedure TCustomECAccordion.DoOnAddEvent(AIndex: Integer);
begin
  if (UpdateCount=0) and not (csLoading in ComponentState) and assigned(OnAdd) then OnAdd(self, AIndex);
end;

procedure TCustomECAccordion.EndUpdate;
begin
  dec(UpdateCount);
  if UpdateCount=0 then Invalidate;
end;

procedure TCustomECAccordion.FindNextItem;
var aItemIndex: Integer;
begin
  aItemIndex:=ItemIndex;
  if aItemIndex<(Items.Count-1) then ItemIndex:=aItemIndex+1;
end;

procedure TCustomECAccordion.FindPreviousItem;
var aItemIndex: Integer;
begin
  aItemIndex:=ItemIndex;
  if aItemIndex>0 then ItemIndex:=aItemIndex-1;
end;

function TCustomECAccordion.InsertItem(AOwner: TComponent; AIndex: Integer; AActivate: Boolean = True): TAccordionItem;
var aOrder: Word;
begin
  BeginUpdate;
  Result:=TAccordionItem.Create(AOwner);
  Result.Accordion:=self;
  if AActivate then ItemIndex:=-1;
  aOrder:=Item[Items.Count-1].FOrder;
  Item[Items.Count-1].FOrder:=Item[AIndex].Order;
  Item[AIndex].FOrder:=aOrder;
  Items.Move(Items.Count-1, AIndex);
  EndUpdate;
  DoOnAddEvent(AIndex);
  if not AActivate then
    begin
      if AIndex<ItemIndex then inc(FItemIndex);
      AnchorAccordionItem;
    end else
    ItemIndex:=AIndex;
end;

procedure TCustomECAccordion.InvalidateNonUpdated;
begin
  if UpdateCount=0 then Invalidate;
end;

class function TCustomECAccordion.GetControlClassDefaultSize: TSize;
begin
  Result.cx:=180;
  Result.cy:=300;
end;

procedure TCustomECAccordion.Loaded;
var i, j: Integer;
begin
  inherited Loaded;
  for j:=0 to Items.Count-1 do
    for i:=j to Items.Count-1 do
      if j=Item[i].Order then
        begin
          Items.Move(i, j);
          break;
        end;
  FItemIndex:=ItemIndexLFM;
  AnchorAccordionItem;
end;

procedure TCustomECAccordion.MakeNewOrder;
var i: Integer;
begin
  for i:=0 to Items.Count-1 do
    Item[i].FOrder:=i;
end;

procedure TCustomECAccordion.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button=mbLeft then
    begin
      if (Hovered>=0) then
        begin
          if ItemIndex=Hovered
            then ItemIndex:=-1        { collapse }
            else ItemIndex:=Hovered;  { expand }
        end;
    end;
end;

procedure TCustomECAccordion.MouseMove(Shift: TShiftState; X, Y: Integer);
var aBottom, aHovered, aItemHeight: Integer;
    aItemIndex, j: SmallInt;
begin
  inherited MouseMove(Shift, X, Y);
  aItemHeight:=ItemHeight;
  aItemIndex:=ItemIndex;
  aHovered:=-1;
  if (Y>=0) and (Y<Height) then
    begin
      aBottom:=aItemHeight;
      for j:=0 to Items.Count-1 do
        begin
          if Y<aBottom then
            begin
              aHovered:=j;
              break;
            end;
          if j=aItemIndex then inc(aBottom, ExpandedHeight);
          inc(aBottom, aItemHeight);
        end;
    end;
  Hovered:=aHovered;
end;

procedure TCustomECAccordion.MoveItemDown;
var aItemIndex: Integer;
begin
  aItemIndex:=ItemIndex;
  if aItemIndex<(Items.Count-1) then
    begin
      Items.Move(aItemIndex, aItemIndex+1);
      ItemIndex:=aItemIndex+1;
      MakeNewOrder;
    end;
end;

procedure TCustomECAccordion.MoveItemUp;
var aItemIndex: Integer;
begin
  aItemIndex:=ItemIndex;
  if aItemIndex>0 then
    begin
      Items.Move(aItemIndex, aItemIndex-1);
      ItemIndex:=aItemIndex-1;
      MakeNewOrder;
    end;
end;

procedure TCustomECAccordion.Paint;
var aFlags: Cardinal;
    aImagePoint: TPoint;
    aGlyphRect, aTextRect: TRect;
    aIndex, aTop: Integer;
    bEnabled, bR2L: Boolean;

  procedure DrawItem;
  var aColorTop, aColorBottom: TColor;
      aDetails: TThemedElementDetails;
      aGlyphDesign: TGlyphDesign;
      aHandled: Boolean;
      aRect: TRect;
      aState: TItemState;
  begin
    aRect:=Rect(0, aTop, Width, aTop+ItemHeight);
    aHandled:=False;
    if assigned(OnDrawItem) then OnDrawItem(self, aIndex, aRect, aHandled);
    if aHandled then exit;  { Exit! }
    case Style of
      easHeader:
        begin
          if bEnabled then
            begin
              if aIndex<>ItemIndex
                then aDetails:=ThemeServices.GetElementDetails(thHeaderItemNormal)
                else aDetails:=ThemeServices.GetElementDetails(thHeaderItemPressed);
            end else
            aDetails:=ThemeServices.GetElementDetails(thHeaderItemNormal);
          ThemeServices.DrawElement(Canvas.Handle, aDetails, aRect);
        end;
      easButton:
        begin
          if bEnabled then
            begin
              if aIndex<>ItemIndex
                then aDetails:=ThemeServices.GetElementDetails(tbPushButtonNormal)
                else aDetails:=ThemeServices.GetElementDetails(tbPushButtonPressed);
            end else
            aDetails:=ThemeServices.GetElementDetails(tbPushButtonDisabled);
          ThemeServices.DrawElement(Canvas.Handle, aDetails, aRect);
        end;
      easGradient:
        begin
          aColorTop:=GetColorResolvingDefault(ColorGradTop, clHighlight);
          aColorBottom:=GetColorResolvingDefault(ColorGradBottom, clForm);
          if bEnabled then
            begin
              if aIndex=ItemIndex then
                begin
                  aColorTop:=GetMergedColor(aColorTop, clBlack, 0.8);
                  aColorBottom:=GetMergedColor(aColorBottom, clWhite, 0.4);
                end;
            end else
            begin
              aColorTop:=GetMonochromaticColor(aColorTop);
              aColorBottom:=GetMonochromaticColor(aColorBottom);
            end;
          Canvas.GradientFill(aRect, aColorTop, aColorBottom, gdVertical);
        end;
    end;
    aTextRect.Top:=aRect.Top;
    aTextRect.Bottom:=aRect.Bottom;
    if aIndex<>ItemIndex then
      begin
        Canvas.Font.Color:=Font.Color;
        Canvas.Font.Size:=Font.Size;
        Canvas.Font.Style:=Font.Style;
        if bEnabled
          then aDetails:=ThemeServices.GetElementDetails(tbPushButtonNormal)
          else aDetails:=ThemeServices.GetElementDetails(tbPushButtonDisabled);
      end else
      begin
        Canvas.Font.Color:=SelectedFontOptions.FontColor;
        Canvas.Font.Size:=SelectedFontOptions.FontSize;
        Canvas.Font.Style:=SelectedFontOptions.FontStyles;
        if bEnabled
          then aDetails:=ThemeServices.GetElementDetails(tbPushButtonPressed)
          else aDetails:=ThemeServices.GetElementDetails(tbPushButtonDisabled);
      end;
    ThemeServices.DrawText(Canvas, aDetails, Item[aIndex].Caption, aTextRect, aFlags, 0);
    if assigned(Images) then
      ThemeServices.DrawIcon(Canvas, aDetails, aImagePoint, Images, Item[aIndex].ImageIndex);
    if bEnabled then
      begin
        if aIndex<>ItemIndex
          then aState:=eisEnabled
          else aState:=eisPushed;
      end else
      aState:=eisDisabled;
    if aIndex<>ItemIndex
      then aGlyphDesign:=GlyphExpand
      else aGlyphDesign:=GlyphCollapse;
    Canvas.Pen.Color:=clBtnText;
    Canvas.DrawGlyph(aGlyphRect, aGlyphDesign, aState);
  end;

var aGlyphSize: TSize;
begin
  inherited Paint;
  Canvas.Font.Assign(Font);
  bEnabled:=IsEnabled;
  bR2L:=IsRightToLeft;
  aGlyphSize:=Canvas.GlyphExtent(GlyphExpand);
  if aGlyphSize.cx>0 then
    begin
      inc(aGlyphSize.cx, 2*Spacing);
      if not bR2L
        then aGlyphRect.Left:=Width-aGlyphSize.cx
        else aGlyphRect.Left:=0;
      aGlyphRect.Right:=aGlyphRect.Left+aGlyphSize.cx;
      aGlyphRect.Top:=0;
      aGlyphRect.Bottom:=ItemHeight;
    end else
    aGlyphSize.cx:=Spacing;
  if not bR2L
    then aTextRect.Right:=Width-aGlyphSize.cx
    else aTextRect.Left:=aGlyphSize.cx;
  if assigned(Images) then
    begin
      if not bR2L
        then aImagePoint.X:=Spacing
        else aImagePoint.X:=Width-Images.Width-Spacing;
      aImagePoint.Y:=(ItemHeight-Images.Height) div 2;
      aGlyphSize.cx:=Images.Width+2*Spacing
    end else
    aGlyphSize.cx:=Spacing;
  if not bR2L
    then aTextRect.Left:=aGlyphSize.cx
    else aTextRect.Right:=Width-aGlyphSize.cx;
  aFlags:=DT_VCENTER+DT_SINGLELINE+DT_END_ELLIPSIS;
  if not bR2L then
    case Alignment of
      taRightJustify: aFlags:=aFlags+DT_RIGHT;
      taCenter: aFlags:=aFlags+DT_CENTER;
    end else
    begin
      aFlags:=aFlags+DT_RTLREADING;
      case Alignment of
        taLeftJustify: aFlags:=aFlags+DT_RIGHT;
        taCenter: aFlags:=aFlags+DT_CENTER;
      end;
    end;
  aTop:=0;
  for aIndex:=0 to Items.Count-1 do
    begin
      DrawItem;
      inc(aTop, ItemHeight);
      if aIndex=ItemIndex then
        begin
          inc(aTop, ExpandedHeight);
          inc(aGlyphRect.Top, ExpandedHeight);
          inc(aGlyphRect.Bottom, ExpandedHeight);
          inc(aImagePoint.Y, ExpandedHeight);
        end;
      inc(aGlyphRect.Top, ItemHeight);
      inc(aGlyphRect.Bottom, ItemHeight);
      inc(aImagePoint.Y, ItemHeight);
    end;
end;

procedure TCustomECAccordion.WMSize(var Message: TLMSize);
begin
  inherited WMSize(Message);
  AnchorAccordionItem;
end;

{ TCustomECAccordion.Setters }

function TCustomECAccordion.GetActiveItem: TAccordionItem;
begin
  if FItemIndex>=0
    then Result:=TAccordionItem(FItems[FItemIndex])
    else Result:=nil;
end;

function TCustomECAccordion.GetItem(AIndex: Integer): TAccordionItem;
begin
  Result:=TAccordionItem(Items[AIndex]);
end;

procedure TCustomECAccordion.SetActiveItem(AValue: TAccordionItem);
var aIndex: Integer;
begin
  if assigned(AValue) and (AValue.Accordion<>self) then exit;  { Exit! }
  aIndex:=Items.IndexOf(AValue);
  ItemIndex:=aIndex;
end;

procedure TCustomECAccordion.SetAlignment(AValue: TAlignment);
begin
  if FAlignment=AValue then exit;
  FAlignment:=AValue;
  InvalidateNonUpdated;
end;

procedure TCustomECAccordion.SetColorGradBottom(AValue: TColor);
begin
  if FColorGradBottom=AValue then exit;
  FColorGradBottom:=AValue;
  InvalidateNonUpdated;
end;

procedure TCustomECAccordion.SetColorGradTop(AValue: TColor);
begin
  if FColorGradTop=AValue then exit;
  FColorGradTop:=AValue;
  InvalidateNonUpdated;
end;

procedure TCustomECAccordion.SetFullExpand(AValue: Boolean);
begin
  if FFullExpand=AValue then exit;
  FFullExpand:=AValue;
  AnchorAccordionItem;
end;

procedure TCustomECAccordion.SetGlyphCollapse(AValue: TGlyphDesign);
begin
  if FGlyphCollapse=AValue then exit;
  FGlyphCollapse:=AValue;
  InvalidateNonUpdated;
end;

procedure TCustomECAccordion.SetGlyphExpand(AValue: TGlyphDesign);
begin
  if FGlyphExpand=AValue then exit;
  FGlyphExpand:=AValue;
  InvalidateNonUpdated;
end;

procedure TCustomECAccordion.SetImages(AValue: TCustomImageList);
begin
  if FImages=AValue then exit;
  FImages:=AValue;
  InvalidateNonUpdated;
end;

procedure TCustomECAccordion.SetItemHeight(AValue: SmallInt);
begin
  if FItemHeight=AValue then exit;
  FItemHeight:=AValue;
  AnchorAccordionItem;
end;

procedure TCustomECAccordion.SetItemIndex(AValue: SmallInt);
begin  { do not change csNoDesignVisible }
  if FItemIndex=AValue then exit;
  if csLoading in ComponentState then
    begin
      ItemIndexLFM:=AValue;
      exit;  { Exit! }
    end;
  if FItemIndex>=0 then
    with Item[FItemIndex] do
      begin
        Anchors:=[akLeft, akRight];
        Height:=0;
        Visible:=False;
      end;
  FItemIndex:=AValue;
  if UpdateCount=0 then if assigned(OnChange) then OnChange(self);
  if AValue>=0 then Item[AValue].Visible:=True;
  AnchorAccordionItem;
end;

procedure TCustomECAccordion.SetItems(AValue: TFPList);
begin
  if FItems=AValue then exit;
  FItems:=AValue;
  InvalidateNonUpdated;
end;

procedure TCustomECAccordion.SetSelectedFontOptions(AValue: TFontOptions);
begin
  if FSelectedFontOptions=AValue then exit;
  FSelectedFontOptions:=AValue;
  InvalidateNonUpdated;
end;

procedure TCustomECAccordion.SetSpacing(AValue: SmallInt);
begin
  if FSpacing=AValue then exit;
  FSpacing:=AValue;
  InvalidateNonUpdated;
end;

procedure TCustomECAccordion.SetStyle(AValue: TAccordionStyle);
begin
  if FStyle=AValue then exit;
  FStyle:=AValue;
  InvalidateNonUpdated;
end;

end.


