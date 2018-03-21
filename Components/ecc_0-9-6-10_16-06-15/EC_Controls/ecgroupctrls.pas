{**************************************************************************************************
 This file is part of the Eye Candy Controls (EC-C)

  Copyright (C) 2014-2016 Vojtěch Čihák, Czech Republic

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

unit ECGroupCtrls;
{$mode objfpc}{$H+}

//{$DEFINE DBGGRP}  {don't remove, just comment}

interface

uses
  Classes, SysUtils, Controls, Forms, Graphics, ImgList, LCLIntf,
  LCLProc, LCLType, LMessages, Math, Themes, Types, ECTypes;

type 
  {$PACKENUM 2}
  TGCOption = (egoAllowAllUp, egoCaptionBy, egoCentered, 
               egoColumnThenRow, egoNativeGlyphs, egoSplitted);
  TGCOptions = set of TGCOption;
    
  { TGroupCtrlItem }   
  TGroupCtrlItem = class(TCollectionItem)
  private
    FCaption: TTranslateString;
    FChecked: Boolean;
    FImageIndex: SmallInt;
    FImageIndexChecked: SmallInt;
    procedure SetCaption(const AValue: TTranslateString);
    procedure SetChecked(AValue: Boolean);
    procedure SetImageIndex(AValue: SmallInt);
    procedure SetImageIndexChecked(AValue: SmallInt);
  protected const
    cDefCaption = 'Item';
  protected
    FItemRect: TRect;
    function GetDisplayName: string; override;
  public 
    constructor Create(ACollection: TCollection); override;
  published
    property Caption: TTranslateString read FCaption write SetCaption;
    property Checked: Boolean read FChecked write SetChecked default False;
    property ImageIndex: SmallInt read FImageIndex write SetImageIndex default -1;
    property ImageIndexChecked: SmallInt read FImageIndexChecked write SetImageIndexChecked default -1;
  end; 
  
  TBaseECGroupCtrl = class;
  
  { TGroupCtrlItems }
  TGroupCtrlItems = class(TCollection)
  private
    function GetItems(Index: Integer): TGroupCtrlItem;
    procedure SetItems(Index: Integer; AValue: TGroupCtrlItem);
  protected
    FGroupCtrl: TBaseECGroupCtrl;
    OnCheckedChange: TIntegerMethod;
    OnClick: TIntegerMethod;
    function GetOwner: TPersistent; override;
    procedure InvalidateItemRect(AIndex: Integer);
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AGroupCtrl: TBaseECGroupCtrl);
    function Add: TGroupCtrlItem;
    procedure Click(AIndex: Integer);
    procedure Reset;
    property Items[Index: Integer]: TGroupCtrlItem read GetItems write SetItems; default;
  end;   
  
  { TBaseECGroupCtrl }
  TBaseECGroupCtrl = class abstract(TECBaseControl)
  private
    FBlockColor: TColor;
    FCheckedFontColor: TColor;
    FCheckedFontStyles: TFontStyles;
    FImages: TCustomImageList;
    FIndent: SmallInt;
    FItems: TGroupCtrlItems;
    FHighlighted: SmallInt;
    FOptions: TGCOptions;
    FRowCount: SmallInt; 
    FSpacing: SmallInt;
    FUncheckedFontColor: TColor;
    FUncheckedFontStyles: TFontStyles;
    procedure SetBlockColor(AValue: TColor);
    procedure SetCheckedFontColor(AValue: TColor);
    procedure SetCheckedFontStyles(AValue: TFontStyles);
    procedure SetHighlighted(AValue: SmallInt);
    procedure SetImages(AValue: TCustomImageList);
    procedure SetIndent(AValue: SmallInt);
    procedure SetItems(AValue: TGroupCtrlItems);
    procedure SetOptions(AValue: TGCOptions);
    procedure SetRowCount(AValue: SmallInt);  
    procedure SetSpacing(AValue: SmallInt);
    procedure SetUncheckedFontColor(AValue: TColor);
    procedure SetUncheckedFontStyles(AValue: TFontStyles);
  protected const
    cDefCheckedFontStyles = [fsBold];
    cDefIndent = 3;
    cDefSpacing = 5;
    cFocusRectIndent: SmallInt = 2;
  protected
    Bitmaps: array [low(TItemState)..high(TItemState)] of TBitmap;
    FBlockRect: TRect;
    FCaptionRect: TRect;
    FColWidth, FRowHeight: Single;
    FPushedBtn: SmallInt;
    FRealColCount, FRealRowCount:  SmallInt;
    ValidStates: TItemStates;
    procedure Calculate(AEnabled: Boolean);
    procedure CMBiDiModeChanged(var {%H-}Message: TLMessage); message CM_BIDIMODECHANGED;
    procedure CMColorChanged(var {%H-}Message: TLMessage); message CM_COLORCHANGED;
    procedure CMEnabledChanged(var Message: TLMessage); message CM_ENABLEDCHANGED;
    procedure CMParentColorChanged(var Message: TLMessage); message CM_PARENTCOLORCHANGED;
    procedure CreateValidBitmaps(AEnabled: Boolean);
    function DialogChar(var Message: TLMKey): Boolean; override;
    procedure DrawBlocks(AEnabled: Boolean);
    class function GetControlClassDefaultSize: TSize; override;
    function GetElementDetails(AIState: TItemState): TThemedElementDetails; virtual; abstract;
    procedure InvalidateCustomRect({%H-}AMove: Boolean); override;
    procedure InvalidateItem(AIndex: Integer);
    procedure ItemsChanged(ARecalculate: Boolean);	
    procedure KeyPress(var Key: Char); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure OrientationChanged(AValue: TObjectOrientation); override;
    procedure Paint; override;                              
    procedure RecalcInvalidate;
    procedure RecalcRedraw; override;
    procedure Redraw3DColorAreas; override;    
    procedure TextChanged; override;
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
    property Highlighted: SmallInt read FHighlighted write SetHighlighted;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Redraw; override;
    property BlockColor: TColor read FBlockColor write SetBlockColor default clDefault;
    property CheckedFontColor: TColor read FCheckedFontColor write SetCheckedFontColor default clDefault;
    property CheckedFontStyles: TFontStyles read FCheckedFontStyles write SetCheckedFontStyles default cDefCheckedFontStyles;
    property Images: TCustomImageList read FImages write SetImages;
    property Indent: SmallInt read FIndent write SetIndent default cDefIndent;
    property Items: TGroupCtrlItems read FItems write SetItems;
    property Options: TGCOptions read FOptions write SetOptions;
    property RowCount: SmallInt read FRowCount write SetRowCount default 1;
    property Spacing: SmallInt read FSpacing write SetSpacing default cDefSpacing;
    property UncheckedFontColor: TColor read FUncheckedFontColor write SetUncheckedFontColor default clDefault;
    property UncheckedFontStyles: TFontStyles read FUncheckedFontStyles write SetUncheckedFontStyles default [];
  end;
  
  { TCustomECRadioGroup }
  TCustomECRadioGroup = class(TBaseECGroupCtrl)
  private
    FItemIndex: SmallInt;
    FOnSelectionChange: TNotifyEvent;
    procedure SetItemIndex(AValue: SmallInt);
  protected const
    cDefRGOptions = [egoCentered, egoSplitted];
  protected
    function GetElementDetails(AIState: TItemState): TThemedElementDetails; override;
    procedure GroupCtrlItemCheckedChanged(AIndex: Integer);
    procedure GroupCtrlItemClick(AIndex: Integer); 
  public
    constructor Create(AOwner: TComponent); override;
    property ItemIndex: SmallInt read FItemIndex write SetItemIndex default -1;
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
  end;
  
  { TCustomECCheckGroup }
  TCustomECCheckGroup = class(TBaseECGroupCtrl)
  private
    FOnItemClick: TIntegerEvent;
    function GetChecked(Index: SmallInt): Boolean;
    procedure SetChecked(Index: SmallInt; AValue: Boolean);
  protected const
    cDefCGOptions = [egoAllowAllUp, egoCentered, egoSplitted];   
  protected
    function GetElementDetails(AIState: TItemState): TThemedElementDetails; override;
    procedure GroupCtrlItemCheckedChanged(AIndex: Integer);
    procedure GroupCtrlItemClick(AIndex: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    property Checked[Index: SmallInt]: Boolean read GetChecked write SetChecked; 
    property OnItemClick: TIntegerEvent read FOnItemClick write FOnItemClick;
  end;
  
  { TECRadioGroup }    
  TECRadioGroup = class(TCustomECRadioGroup)
  published
    property Align;
    property Anchors;
    property BevelInner;
    property BevelOuter;
    property BevelSpace;
    property BevelWidth;
    property BiDiMode;    
    property BlockColor;
    property BorderSpacing;
    property Caption;   
    property CheckedFontColor;
    property CheckedFontStyles;
    {property Color;}  { not needed }
    property Color3DDark;
    property Color3DLight;
    property Constraints;
    property Cursor;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;    
    property Height;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Hint; 
    property Images;
    property Indent;
    property ItemIndex;
    property Items;  
    property Left;
    property Name;
    property Options  default cDefRGOptions;
    property Orientation default eooHorizontal; 
    property ParentBiDiMode;
    {property ParentColor;}  { not needed }
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RowCount;
    property ShowHint;                           
    property Spacing;
    property Style default eosButton;
    property TabOrder;
    property TabStop;
    property Tag;
    property Top;
    property UncheckedFontColor;
    property UncheckedFontStyles;
    property Visible;
    property Width;    
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
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
    property OnSelectionChange;
    property OnStartDrag;
    property OnUTF8KeyPress;   
  end;

  { TECCheckGroup }     
  TECCheckGroup = class(TCustomECCheckGroup)
  published
    property Align;
    property Anchors;
    property BevelInner;
    property BevelOuter;
    property BevelSpace;
    property BevelWidth;
    property BiDiMode;    
    property BlockColor;
    property BorderSpacing;
    property Caption;      
    property CheckedFontColor;
    property CheckedFontStyles;  
    {property Color;}  { not needed }
    property Color3DDark;
    property Color3DLight;
    property Constraints;
    property Cursor;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;   
    property Height;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Hint;  
    property Images;
    property Indent;
    property Items;  
    property Left;
    property Name;
    property Options default cDefCGOptions;
    property Orientation default eooHorizontal;
    property ParentBiDiMode;
    {property ParentColor;}  { not needed }
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RowCount;
    property ShowHint;  
    property Spacing;
    property Style default eosButton;
    property TabOrder;
    property TabStop;
    property Tag;
    property Top;
    property UncheckedFontColor;
    property UncheckedFontStyles;  
    property Visible;
    property Width;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnItemClick;
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
  end;
  
implementation

{ TGroupCtrlItem }

constructor TGroupCtrlItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FChecked:=False;
  FImageIndex:=-1;
  FImageIndexChecked:=-1;  
end;

function TGroupCtrlItem.GetDisplayName: string;
begin
  Result:=Caption;
  if Result='' then Result:=cDefCaption+inttostr(ID);
end;

{ TGroupCtrlItem.Setters }

procedure TGroupCtrlItem.SetCaption(const AValue: TTranslateString);
begin
  if FCaption=AValue then exit;
  FCaption:=AValue;
  Changed(False)  
end;

procedure TGroupCtrlItem.SetChecked(AValue: Boolean);
begin
  if FChecked=AValue then exit;  
  FChecked:=AValue;
  if assigned(Collection) then
    with Collection as TGroupCtrlItems do
      begin
        if assigned(OnCheckedChange) then OnCheckedChange(Index);
        InvalidateItemRect(Index); 
      end;
end;
         
procedure TGroupCtrlItem.SetImageIndex(AValue: SmallInt);
begin
  if FImageIndex=AValue then exit;
  FImageIndex:=AValue;
  Changed(False);    
end;

procedure TGroupCtrlItem.SetImageIndexChecked(AValue: SmallInt);
begin
  if FImageIndexChecked=AValue then exit;
  FImageIndexChecked:=AValue;
  Changed(False);
end;

{ TGroupCtrlItems }

constructor TGroupCtrlItems.Create(AGroupCtrl: TBaseECGroupCtrl);
begin
  inherited Create(TGroupCtrlItem);
  FGroupCtrl:=AGroupCtrl;
end;

function TGroupCtrlItems.Add: TGroupCtrlItem;
begin
  Result:=TGroupCtrlItem(inherited Add);
end;   

procedure TGroupCtrlItems.Click(AIndex: Integer);
begin
  if assigned(OnClick) then OnClick(AIndex);
end;   

function TGroupCtrlItems.GetOwner: TPersistent;
begin
  Result:=FGroupCtrl;
end;

procedure TGroupCtrlItems.InvalidateItemRect(AIndex: Integer);
begin
  if assigned(Owner) then
    with Owner as TBaseECGroupCtrl do
      InvalidateItem(AIndex);
end;

procedure TGroupCtrlItems.Notify(Item: TCollectionItem; Action: TCollectionNotification);
begin
  inherited Notify(Item, Action);
  if Action=cnAdded then
    if not (csLoading in (Owner as TBaseECGroupCtrl).ComponentState) then
      TGroupCtrlItem(Item).FCaption:=TGroupCtrlItem.cDefCaption+inttostr(Item.ID);  
end;

procedure TGroupCtrlItems.Reset;
var i: Integer;
begin
  for i:=0 to Count-1 do
    Items[i].Checked:=False;   
end;

procedure TGroupCtrlItems.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  FGroupCtrl.ItemsChanged(Item=nil);
end;   

{ TGroupCtrlItems.Setters } 

function TGroupCtrlItems.GetItems(Index: Integer): TGroupCtrlItem;
begin
  Result:=TGroupCtrlItem(inherited Items[Index]);
end;  

procedure TGroupCtrlItems.SetItems(Index: Integer; AValue: TGroupCtrlItem);
begin
  Items[Index].Assign(AValue);
end;

{ TBaseECGroupCtrl }

constructor TBaseECGroupCtrl.Create(AOwner: TComponent);
begin                                   
  inherited Create(AOwner);
  ControlStyle:=ControlStyle+[csParentBackground, csReplicatable, csSetCaption] 
                            -csMultiClicks-[csCaptureMouse, csNoFocus, csOpaque];
  FBlockColor:=clDefault;
  FCheckedFontColor:=clDefault;
  FCheckedFontStyles:=cDefCheckedFontStyles;
  FHighlighted:=-1;
  FIndent:=cDefIndent;
  FItems:=TGroupCtrlItems.Create(self);   
  FOrientation:=eooHorizontal;
  FRowCount:=1;
  FSpacing:=cDefSpacing;
  FUncheckedFontColor:=clDefault;
  FUncheckedFontStyles:=[];
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, cx, cy); 
  RedrawMode:=ermRecalcRedraw;
end;

destructor TBaseECGroupCtrl.Destroy;
var aIState: TItemState;
begin
  for aIState:=low(TItemState) to high(TItemState) do
    if assigned(Bitmaps[aIState]) then FreeAndNil(Bitmaps[aIState]);
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TBaseECGroupCtrl.Calculate(AEnabled: Boolean);
var aIState: TItemState;
    aExtent: TSize;
    i, aCount: Integer;
begin
  if HasCaption then
    begin  
      aExtent:=Canvas.TextExtent(Caption);
      i:=2*cFocusRectIndent;
      inc(aExtent.cx, i);
      inc(aExtent.cy, i);
      if Orientation=eooHorizontal then
        begin
          if not IsRightToLeft then
            begin  { Horizontal }
              FCaptionRect:=Rect(0, 0, aExtent.cx, aExtent.cy);
              if not (egoCaptionBy in Options)
                then FBlockRect:=Rect(0, aExtent.cy+Indent-cFocusRectIndent, Width, Height)
                else FBlockRect:=Rect(aExtent.cx+Indent-cFocusRectIndent, 0, Width, Height);
            end else
            begin
              FCaptionRect:=Rect(Width-aExtent.cx+1, 0, Width, aExtent.cy);
              if not (egoCaptionBy in Options)
                then FBlockRect:=Rect(0, aExtent.cy+Indent-cFocusRectIndent, Width, Height)
                else FBlockRect:=Rect(0, 0, Width-aExtent.cx-Indent+cFocusRectIndent+2, Height);
            end;
        end else
        begin  { Vertical }
          if not IsRightToLeft then
            begin
              FCaptionRect:=Rect(0, 0, aExtent.cy, aExtent.cx);
              if not (egoCaptionBy in Options)
                then FBlockRect:=Rect(aExtent.cy+Indent-cFocusRectIndent, 0, Width, Height)
                else FBlockRect:=Rect(0, aExtent.cx+Indent-cFocusRectIndent, Width, Height);
            end else
            begin
              FCaptionRect:=Rect(Width-aExtent.cy, 0, Width, aExtent.cx);
              if not (egoCaptionBy in Options)
                then FBlockRect:=Rect(0, 0, Width-aExtent.cy-Indent+cFocusRectIndent, Height)
                else FBlockRect:=Rect(0, aExtent.cx+Indent-cFocusRectIndent, Width, Height); 
            end;
        end;
    end else
    FBlockRect:=ClientRect;
  aExtent.cx:=FBlockRect.Right-FBlockRect.Left;  
  aExtent.cy:=FBlockRect.Bottom-FBlockRect.Top;
  if AEnabled then
    for aIState:=eisHighlighted to eisPushedHihlighted do
      Bitmaps[aIState].SetSize(aExtent.cx, aExtent.cy)
    else
    begin
      Bitmaps[eisDisabled].SetSize(aExtent.cx, aExtent.cy);
      Bitmaps[eisPushedDisabled].SetSize(aExtent.cx, aExtent.cy);
    end;
  aCount:=Items.Count;
  aExtent.cy:=Math.max(1, Math.min(FRowCount, aCount));
  aExtent.cx:=aCount div aExtent.cy;
  if (aExtent.cx=0) or ((aCount  mod aExtent.cy)>0) then inc(aExtent.cx);
  i:=aExtent.cx*aExtent.cy-aCount;
  if i>=0 then dec(aExtent.cy, i div aExtent.cx);
  FRealColCount:=aExtent.cx;
  FRealRowCount:=aExtent.cy;
  if aCount>0 then 
    begin
      if Orientation=eooHorizontal then
        begin  { Horizontal }
          FColWidth:=(Bitmaps[caItemState[AEnabled]].Width)/aExtent.cx;
          FRowHeight:=(Bitmaps[caItemState[AEnabled]].Height)/aExtent.cy;
        end else
        begin  { Vertical }
          FColWidth:=(Bitmaps[caItemState[AEnabled]].Height)/aExtent.cx;
          FRowHeight:=(Bitmaps[caItemState[AEnabled]].Width)/aExtent.cy;   
        end;
    end;
end;

procedure TBaseECGroupCtrl.CMBiDiModeChanged(var Message: TLMessage);
begin
  RecalcInvalidate;
end;

procedure TBaseECGroupCtrl.CMColorChanged(var Message: TLMessage);
begin
  Redraw;
end;

procedure TBaseECGroupCtrl.CMEnabledChanged(var Message: TLMessage);
begin
  if IsEnabled then Highlighted:=-1;
  inherited CMEnabledChanged(Message);
end;

procedure TBaseECGroupCtrl.CMParentColorChanged(var Message: TLMessage);
begin
  inherited CMParentColorChanged(Message);
  Redraw;
end;

procedure TBaseECGroupCtrl.CreateValidBitmaps(AEnabled: Boolean);
var aIState: TItemState;
    aWidth, aHeight: Integer;
begin
  if assigned(Bitmaps[eisEnabled]) then 
    begin
      aWidth:=Bitmaps[eisEnabled].Width;
      aHeight:=Bitmaps[eisEnabled].Height; 
    end else
    if assigned(Bitmaps[eisDisabled]) then
      begin
        aWidth:=Bitmaps[eisDisabled].Width;
        aHeight:=Bitmaps[eisDisabled].Height;  
      end else
      begin
        aWidth:=0;
        aHeight:=0;        
      end;
  for aIState in ValidStates do  
    FreeAndNil(Bitmaps[aIState]);
  if AEnabled 
    then ValidStates:=caEnabledStates 
    else ValidStates:=caDisabledStates;       
  for aIState in ValidStates do 
    begin
      Bitmaps[aIState]:=TBitmap.Create;
      Bitmaps[aIState].SetProperties(aWidth, aHeight);
    end;    
end;   

function TBaseECGroupCtrl.DialogChar(var Message: TLMKey): Boolean;
var i: Integer;
begin
  Result:=False;  
  if Message.Msg=LM_SYSCHAR then 
    begin
      if IsEnabled and IsVisible then
        begin
          for i:=0 to Items.Count-1 do
            if IsAccel(Message.CharCode, Items[i].Caption) then
              begin
                Items.Click(i); 
                SetFocus; 
                Result:=True;  
                exit;  { Exit! }
              end;
          Result:=inherited DialogChar(Message);  
        end; 
    end; 
end;    

procedure TBaseECGroupCtrl.DrawBlocks(AEnabled: Boolean);
var i, j, aBorder, aCount, aIndex, aLeft, aTop: Integer;
    aColor: TColor;
    aColWidth, aRowHeight, aText1Top: Single;
    aFlags: Cardinal;
    aGlyphSize, aImageSize: TSize;
    aRect: TRect;
    aIState: TItemState;
    bGlyphs, bHasImages, bHorizontal, bR2L: Boolean;
    
  procedure IncIndent(var AResult: Integer; AIncrement: Integer);
  begin
    if AResult<>0 then 
      begin
        inc(AResult, AIncrement);
        if AIncrement<>0 then inc(AResult, Spacing);
      end
      else AResult:=AIncrement
  end;         

  procedure DrawGlyphImageText;
  var aCaption: string;
      aElementRect: TRect;
      aImage, aImgCh: Integer;
      aPoint: TPoint;
      aTextWidth, w, x, y: Integer;
      bImages: Boolean;
  begin
    aImage:=Items[aIndex].ImageIndex;
    aImgCh:=Items[aIndex].ImageIndexChecked;
    if not (bHasImages and (aImage>=0) and (aImage<Images.Count)) then aImage:=-1;
    if not (bHasImages and (aImgCh>=0) and (aImgCh<Images.Count)) then
      if aImage>=0
        then aImgCh:=aImage
        else aImgCh:=-1;
    if bGlyphs 
      then w:=aGlyphSize.cx
      else w:=0;
    bImages:= ((aImgCh>=0) and ((aIState>=eisPushed) or not (egoCentered in Options))) 
                 or ((aImage>=0) and (aIState<=eisEnabled)); 
    if bImages then IncIndent(w, aImageSize.cx); 
    aCaption:=Items[aIndex].Caption;
    if bHorizontal then DeleteAmpersands(aCaption);
    aTextWidth:=Bitmaps[aIState].Canvas.TextWidth(aCaption);
    IncIndent(w, aTextWidth);
    aLeft:=round(i*aColWidth); 
    if bHorizontal then
      begin
        Items[aIndex].FItemRect:=Rect(FBlockRect.Left+aLeft, FBlockRect.Top+round(j*aRowHeight),
          FBlockRect.Left+round((i+1)*aColWidth), FBlockRect.Top+round((j+1)*aRowHeight));
      end else
      begin
        Items[aIndex].FItemRect:=Rect(FBlockRect.Left+round(j*aRowHeight), FBlockRect.Top+aLeft,
          FBlockRect.Left+round((j+1)*aRowHeight), FBlockRect.Top+round((i+1)*aColWidth));  
      end;
    if not (egoCentered in Options) then
      begin
        x:=aBorder+Spacing;
        if bR2L and bHorizontal then x:=round(FColWidth-w-x);        
      end else
      x:=round(0.5*(aColWidth-w));
    if bGlyphs then
      begin
        if bHorizontal then
          begin
            y:=round(0.5*(aRowHeight-aGlyphSize.cy)+j*aRowHeight);
            if not bR2L then
              begin
                aElementRect:=Rect(aLeft+x, y, aLeft+x+aGlyphSize.cx, y+aGlyphSize.cy);
                inc(x, aGlyphSize.cx+Spacing);
              end else
              begin
                aElementRect:=Rect(aLeft+x+w-aGlyphSize.cx, y, aLeft+x+w, y+aGlyphSize.cy);
                dec(x, aGlyphSize.cx+Spacing);  
              end;
          end else
          begin
            y:=round(0.5*(aRowHeight-aGlyphSize.cx)+j*aRowHeight);
            aElementRect:=Rect(y, aLeft+x, y+aGlyphSize.cx, aLeft+x+aGlyphSize.cy);
            inc(x, aGlyphSize.cy+Spacing);                             
          end;    
        ThemeServices.DrawElement(Bitmaps[aIState].Canvas.Handle, 
          GetElementDetails(aIState), aElementRect, nil);
      end;
    if bImages then
      begin
        y:=round(0.5*(FRowHeight-aImageSize.cy)+j*aRowHeight);
        if not bR2L or not bHorizontal then
          begin
            aPoint:=Point(aLeft+x, y);
            inc(x, aImageSize.cx+Spacing);
          end else
          begin
            aPoint:=Point(aLeft+x+w-aImageSize.cx, y);
            dec(x, aImageSize.cx+Spacing);
          end;
        if not bHorizontal then
          begin
            w:=aPoint.X;
            aPoint.X:=aPoint.Y;
            aPoint.Y:=w;
          end;  
        if aIState>=eisPushed then aImage:=aImgCh;
        if aImage>=0 then       
          ThemeServices.DrawIcon(Bitmaps[aIState].Canvas, GetElementDetails(aIState), 
            aPoint, Images, aImage); 
      end;
    aElementRect.Top:=round(aText1Top+j*aRowHeight);
    aElementRect.Right:=Bitmaps[aIState].Width;
    aElementRect.Bottom:=Bitmaps[aIState].Height;
    if not bR2L or not bHorizontal
      then aElementRect.Left:=aLeft+x
      else aElementRect.Left:=aLeft+x+w-aTextWidth;
    if bHorizontal then
      begin
        Bitmaps[aIState].Canvas.Font.Orientation:=0;
        ThemeServices.DrawText(Bitmaps[aIState].Canvas,
          ThemeServices.GetElementDetails(caThemedContent[aIState]),
          Items[aIndex].Caption, aElementRect, aFlags, 0);
      end else
      begin
        Bitmaps[aIState].Canvas.Brush.Style:=bsClear;
        Bitmaps[aIState].Canvas.Font.Orientation:=900;
        Bitmaps[aIState].Canvas.TextOut(aElementRect.Top, 
          aElementRect.Left+aTextWidth, Items[aIndex].Caption);
      end;      
  end;
  
begin
  {$IFDEF DBGGRP} DebugLn('TCustomECGroupCtrl.Draw'); {$ENDIF}
  aColWidth:=FColWidth;
  aCount:=Items.Count;
  aRowHeight:=FRowHeight;
  aBorder:=GetBorderWidth;
  aText1Top:=round(0.5*(aRowHeight-Bitmaps[caItemState[AEnabled]].Canvas.TextHeight('É,9')));   
  aRect:=Rect(0, 0, Bitmaps[caItemState[AEnabled]].Width, Bitmaps[caItemState[AEnabled]].Height);
  bGlyphs:= egoNativeGlyphs in Options;
  bHasImages:= assigned(Images);
  bHorizontal:= Orientation=eooHorizontal;
  bR2L:= IsRightToLeft;
  aFlags:=DT_SINGLELINE;
  if bR2L then aFlags:=aFlags+DT_RTLREADING;
  { set Glyphs size if needed; assumes that disabled/enables/pushed/hot have the same size }
  if bGlyphs then aGlyphSize:=ThemeServices.GetDetailSize(GetElementDetails(caItemState[AEnabled]));
  if bHasImages then aImageSize:=Size(Images.Width, Images.Height);  { set Images size if needed }  
  aColor:=ColorToRGB(GetColorResolvingDefault(Color, Parent.Brush.Color));  
  if (aColor and $FF) > 0
    then dec(aColor)
    else inc(aColor); 
  for aIState in ValidStates do
    begin 
      { Draw Background }
      Bitmaps[aIState].TransparentColor:=aColor;
      Bitmaps[aIState].TransparentClear;
      case Style of
        eosButton: Bitmaps[aIState].Canvas.DrawButtonBackground(aRect, aIState);
        eosPanel: Bitmaps[aIState].Canvas.DrawPanelBackGround(aRect, BevelInner, BevelOuter,
                    BevelSpace, BevelWidth, Color3DDark, Color3DLight, 
                    GetColorResolvingDefault(BlockColor, Parent.Brush.Color));
        eosThemedPanel: Bitmaps[aIState].Canvas.DrawThemedPanelBkgnd(aRect);
      end;
      { Draw Spliters }
      if egoSplitted in Options then  
        if bHorizontal then
          begin  { Horizontal }
            for i:=1 to FRealColCount-1 do  
              begin
                aLeft:=round(i*aColWidth)-1;
                Bitmaps[aIState].Canvas.Pen.Color:=cl3DShadow;
                Bitmaps[aIState].Canvas.Line(aLeft, aBorder, aLeft, Bitmaps[aIState].Height-aBorder);
                if aIState<eisPushed then
                  begin
                    inc(aLeft);
                    Bitmaps[aIState].Canvas.Pen.Color:=cl3DHiLight;
                    Bitmaps[aIState].Canvas.Line(aLeft, aBorder, aLeft, Bitmaps[aIState].Height-aBorder);
                  end;
              end; 
            for j:=1 to FRealRowCount-1 do
              begin
                aTop:=round(j*aRowHeight)-1;
                Bitmaps[aIState].Canvas.Pen.Color:=cl3DShadow;
                Bitmaps[aIState].Canvas.Line(aBorder, aTop, Bitmaps[aIState].Width-aBorder, aTop);
                if aIState<eisPushed then
                  begin
                    inc(aTop);
                    Bitmaps[aIState].Canvas.Pen.Color:=cl3DHiLight;
                    Bitmaps[aIState].Canvas.Line(aBorder, aTop, Bitmaps[aIState].Width-aBorder, aTop);
                  end; 
              end;
          end else
          begin  { Vertical }
            for i:=1 to FRealColCount-1 do  
              begin
                aTop:=round(i*aColWidth)-1;
                Bitmaps[aIState].Canvas.Pen.Color:=cl3DShadow;
                Bitmaps[aIState].Canvas.Line(aBorder, aTop, Bitmaps[aIState].Width-aBorder, aTop);
                if aIState<eisPushed then
                  begin
                    inc(aTop);
                    Bitmaps[aIState].Canvas.Pen.Color:=cl3DHiLight;
                    Bitmaps[aIState].Canvas.Line(aBorder, aTop, Bitmaps[aIState].Width-aBorder, aTop);
                  end;
              end; 
            for j:=1 to FRealRowCount-1 do
              begin
                aLeft:=round(j*aRowHeight)-1;
                Bitmaps[aIState].Canvas.Pen.Color:=cl3DShadow;
                Bitmaps[aIState].Canvas.Line(aLeft, aBorder, aLeft, Bitmaps[aIState].Height-aBorder);
                if aIState<eisPushed then
                  begin
                    inc(aLeft);
                    Bitmaps[aIState].Canvas.Pen.Color:=cl3DHiLight;
                    Bitmaps[aIState].Canvas.Line(aLeft, aBorder, aLeft, Bitmaps[aIState].Height-aBorder);
                  end; 
              end;  
          end;
      { Draw Glyphs, Images and Captions }
      if aIState<eisPushed then  
        begin
          Bitmaps[aIState].Canvas.Font.Color:=GetColorResolvingDefault(UncheckedFontColor, clBtnText);
          Bitmaps[aIState].Canvas.Font.Style:=UncheckedFontStyles;
        end else 
        begin
          Bitmaps[aIState].Canvas.Font.Color:=GetColorResolvingDefault(CheckedFontColor, clBtnText);
          Bitmaps[aIState].Canvas.Font.Style:=CheckedFontStyles;     
        end;    
      if not (egoColumnThenRow in Options) then  
        begin  
          if not bR2L xor not bHorizontal then
            begin  { Horizontal then Vertical, Left to Right } 
              for j:=0 to FRealRowCount-1 do  
                for i:=0 to FRealColCount-1 do  
                  begin
                    aIndex:=i+j*FRealColCount;
                    if aIndex>=aCount then break;
                    DrawGlyphImageText;
                  end;
            end else
            begin  { Horizontal then Vertical, Right to Left }
              for j:=0 to FRealRowCount-1 do  
                for i:=FRealColCount-1 downto 0 do  
                  begin
                    aIndex:=FRealColCount-i-1+j*FRealColCount;
                    if aIndex>=aCount then break;
                    DrawGlyphImageText;
                  end;          
            end;
        end else
        begin
          if not bR2L xor not bHorizontal then
            begin  { Vertical then Horizontal, Left to Right }  
              for i:=0 to FRealColCount-1 do 
                for j:=0 to FRealRowCount-1 do  
                  begin
                    aIndex:=i*FRealRowCount+j;
                    if aIndex>=aCount then break;
                    DrawGlyphImageText;
                  end;   
            end else
            begin  { Vertical then Horizontal, Right to Left } 
              for i:=FRealColCount-1 downto 0 do 
                for j:=0 to FRealRowCount-1 do  
                  begin
                    aIndex:=(FRealColCount-i-1)*FRealRowCount+j;
                    if aIndex>=aCount then break;
                    DrawGlyphImageText;
                  end;              
            end;
        end;
    end;                          
end;                       

class function TBaseECGroupCtrl.GetControlClassDefaultSize: TSize;
begin
  Result.cx:=150;
  Result.cy:=39;
end;                                    

procedure TBaseECGroupCtrl.InvalidateCustomRect(AMove: Boolean);
begin
  if UpdateCount=0 then InvalidateRect(Handle, @FInvalidRect, False);
end;                                              

procedure TBaseECGroupCtrl.InvalidateItem(AIndex: Integer);
begin
  if RedrawMode<=ermFreeRedraw then
    begin
      RedrawMode:=ermMoveKnob;
      if FInvalidRect.Left<0 
        then FInvalidRect:=Items[AIndex].FItemRect
        else IncludeRectangle(FInvalidRect, Items[AIndex].FItemRect);
      InvalidateCustomRect(False);
    end else
    InvalidateNonUpdated;
end;                                              

procedure TBaseECGroupCtrl.ItemsChanged(ARecalculate: Boolean);
begin
  if not ARecalculate
    then Redraw
    else RecalcRedraw;
end;                       

procedure TBaseECGroupCtrl.KeyPress(var Key: Char);
var aIndex: Integer;
begin
  inherited KeyPress(Key);
  aIndex:=ord(Key)-VK_1;
  if (aIndex>=0) and (aIndex<=8) and (aIndex<Items.Count)  
    then Items.Click(aIndex)                                                  { VK_1..VK_9 click items}
    else if (ord(Key)=VK_0) and (egoAllowAllUp in Options) then Items.Reset;  { and VK_0 resets all }    
end;                                                            

procedure TBaseECGroupCtrl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button=mbLeft then FPushedBtn:=Highlighted;
  SetFocus;
end;                       

procedure TBaseECGroupCtrl.MouseLeave;
begin
  {$IFDEF DBGGRP} DebugLn('TCustomECGroupCtrl.MouseLeave'); {$ENDIF}
  inherited MouseLeave;
  Highlighted:=-1;
end;

procedure TBaseECGroupCtrl.MouseMove(Shift: TShiftState; X, Y: Integer);
var aCol, aCount, aIndex, aRow: Integer;
begin
  inherited MouseMove(Shift, X, Y);
  aCount:=Items.Count;
  if (aCount>0) and PtInRect(FBlockRect, Point(X, Y)) then
    begin
      if Orientation=eooHorizontal then
        begin
          X:=X-FBlockRect.Left;
          Y:=Y-FBlockRect.Top;  
        end else
        begin
          aRow:=Y;
          Y:=X-FBlockRect.Left;
          X:=aRow-FBlockRect.Top; 
        end;
      if not (egoColumnThenRow in Options) then
        begin
          aRow:=trunc(Y/FRowHeight);
          if not IsRightToLeft xor (Orientation=eooVertical)
            then aIndex:=aRow*FRealColCount+trunc(X/FColWidth)
            else aIndex:=aRow*FRealColCount+FRealColCount-trunc(X/FColWidth)-1;
        end else
        begin
          if not IsRightToLeft xor (Orientation=eooVertical)     
            then aCol:=trunc(X/FColWidth)
            else aCol:=FRealColCount-trunc(X/FColWidth)-1;
          aIndex:=aCol*FRealRowCount+trunc(Y/FRowHeight);
        end;
      if aIndex>=aCount then aIndex:=-1;
    end else
    aIndex:=-1;
  Highlighted:=aIndex;
end;                       

procedure TBaseECGroupCtrl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Button=mbLeft then
    if (FPushedBtn=Highlighted) and (Highlighted>=0) then Items.Click(Highlighted);
end;                       

procedure TBaseECGroupCtrl.OrientationChanged(AValue: TObjectOrientation);
begin
  if not (csLoading in ComponentState) then SetBounds(Left, Top, Height, Width);
  inherited OrientationChanged(AValue);         
end;                       

procedure TBaseECGroupCtrl.Paint;
var aCount, aIndex, i, j: Integer;
    aFlags: Cardinal;
    aRect, aDest: TRect;
    bEnabled: Boolean;   
    
  function GetItemState: TItemState;
  begin
    {$IFDEF DBGGRP} DebugLn('GroupCtrl.GetItemState, '+inttostr(aIndex)); {$ENDIF}
    if aIndex<aCount then
      begin
        if bEnabled then
          begin
            if not Items[aIndex].Checked then 
              begin
                if not (Highlighted=aIndex)
                  then Result:=eisEnabled
                  else Result:=eisHighlighted;
              end else 
              begin
                if not (Highlighted=aIndex)
                  then Result:=eisPushed
                  else Result:=eisPushedHihlighted;
              end                                  
          end else
          if not Items[aIndex].Checked 
            then Result:=eisDisabled
            else Result:=eisPushedDisabled;
      end else
      Result:=caItemState[bEnabled];
  end;
    
begin
  {$IFDEF DBGGRP} 
  DebugLn('RedrawMode '+inttostr(SmallInt(RedrawMode)));
  DebugLn(inttostr(FInvalidRect.Left)+', '+inttostr(FInvalidRect.Top)+', '+
          inttostr(FInvalidRect.Right)+', '+inttostr(FInvalidRect.Bottom));	 
  {$ENDIF}
  inherited Paint;
  bEnabled:=IsEnabled;
  if (bEnabled xor (eisEnabled in ValidStates)) or (ValidStates=[]) then 
    begin
      if RedrawMode<ermRedrawBkgnd then RedrawMode:=ermRedrawBkgnd;
      CreateValidBitmaps(bEnabled);
    end;  
  if RedrawMode=ermRecalcRedraw then Calculate(bEnabled);
  if RedrawMode>=ermRedrawBkgnd then DrawBlocks(bEnabled);
  { Paint Body }
  aCount:=Items.Count;
  if aCount=0 then
    begin
      aRect:=Rect(0, 0, Bitmaps[caItemState[bEnabled]].Width, Bitmaps[caItemState[bEnabled]].Height);
      Canvas.CopyRect(FBlockRect, Bitmaps[caItemState[bEnabled]].Canvas, aRect);
    end else
    begin
      if Orientation=eooHorizontal then
        begin  { Horizontal }
          if not IsRightToLeft then
            begin
              if not (egoColumnThenRow in Options) then
                begin  { Horizontal then Vertical, Left to Right }
                  for j:=0 to FRealRowCount-1 do
                    begin
                      aRect.Top:=round(j*FRowHeight);
                      aRect.Bottom:=round((j+1)*FRowHeight);
                      aDest.Top:=FBlockRect.Top+aRect.Top;
                      aDest.Bottom:=FBlockRect.Top+aRect.Bottom;
                      for i:=0 to FRealColCount-1 do
                        begin
                          aRect.Left:=round(i*FColWidth);
                          aRect.Right:=round((i+1)*FColWidth);
                          aDest.Left:=FBlockRect.Left+aRect.Left;
                          aDest.Right:=FBlockRect.Left+aRect.Right;
                          aIndex:=i+j*FRealColCount;
                          if (RedrawMode>=ermFreeRedraw) or 
                            PtInRect(FInvalidRect, Point(aDest.Left+1, aDest.Top+1)) 
                            then Canvas.CopyRect(aDest, Bitmaps[GetItemState].Canvas, aRect);
                        end;
                    end;
                end else
                begin  { Vertical then Horizontal, Left to Right } 
                  for i:=0 to FRealColCount-1 do
                    begin
                      aRect.Left:=round(i*FColWidth);
                      aRect.Right:=round((i+1)*FColWidth);
                      aDest.Left:=FBlockRect.Left+aRect.Left;
                      aDest.Right:=FBlockRect.Left+aRect.Right; 
                      for j:=0 to FRealRowCount-1 do
                        begin
                          aRect.Top:=round(j*FRowHeight);
                          aRect.Bottom:=round((j+1)*FRowHeight);
                          aDest.Top:=FBlockRect.Top+aRect.Top;
                          aDest.Bottom:=FBlockRect.Top+aRect.Bottom;
                          aIndex:=i*FRealRowCount+j;
                          if (RedrawMode>=ermFreeRedraw) or 
                            PtInRect(FInvalidRect, Point(aDest.Left+1, aDest.Top+1)) 
                            then Canvas.CopyRect(aDest, Bitmaps[GetItemState].Canvas, aRect);
                        end;
                    end;    
                end;
            end else
            begin
              if not (egoColumnThenRow in Options) then
                begin  { Horizontal then Vertical, Right to Left }
                  for j:=FRealRowCount-1 downto 0 do
                    begin
                      aRect.Top:=round(j*FRowHeight);
                      aRect.Bottom:=round((j+1)*FRowHeight);
                      aDest.Top:=FBlockRect.Top+aRect.Top;
                      aDest.Bottom:=FBlockRect.Top+aRect.Bottom;
                      for i:=0 to FRealColCount-1 do
                        begin
                          aRect.Left:=round(i*FColWidth);
                          aRect.Right:=round((i+1)*FColWidth);
                          aDest.Left:=FBlockRect.Left+aRect.Left;
                          aDest.Right:=FBlockRect.Left+aRect.Right;   
                          aIndex:=FRealColCount-i-1+j*FRealColCount;
                          if (RedrawMode>=ermFreeRedraw) or 
                            PtInRect(FInvalidRect, Point(aDest.Left+1, aDest.Top+1)) 
                            then Canvas.CopyRect(aDest, Bitmaps[GetItemState].Canvas, aRect);
                        end;
                    end;
                end else
                begin  { Vertical then Horizontal, Right to Left }
                  for i:=FRealColCount-1 downto 0 do
                    begin
                      aRect.Left:=round(i*FColWidth);
                      aRect.Right:=round((i+1)*FColWidth);
                      aDest.Left:=FBlockRect.Left+aRect.Left;
                      aDest.Right:=FBlockRect.Left+aRect.Right; 
                      for j:=0 to FRealRowCount-1 do
                        begin
                          aRect.Top:=round(j*FRowHeight);
                          aRect.Bottom:=round((j+1)*FRowHeight);
                          aDest.Top:=FBlockRect.Top+aRect.Top;
                          aDest.Bottom:=FBlockRect.Top+aRect.Bottom; 
                          aIndex:=(FRealColCount-i-1)*FRealRowCount+j;
                          if (RedrawMode>=ermFreeRedraw) or 
                            PtInRect(FInvalidRect, Point(aDest.Left+1, aDest.Top+1)) 
                            then Canvas.CopyRect(aDest, Bitmaps[GetItemState].Canvas, aRect);
                        end;
                    end;    
                end;                     
            end;  
       end else
       begin  { Vertical }
         if not IsRightToLeft then
            begin
              if not (egoColumnThenRow in Options) then
                begin  { Horizontal then Vertical, Left to Right }
                  for j:=0 to FRealRowCount-1 do
                    begin
                      aRect.Left:=round(j*FRowHeight);
                      aRect.Right:=round((j+1)*FRowHeight);
                      aDest.Left:=FBlockRect.Left+aRect.Left;
                      aDest.Right:=FBlockRect.Left+aRect.Right;
                      for i:=0 to FRealColCount-1 do
                        begin
                          aRect.Top:=round(i*FColWidth);
                          aRect.Bottom:=round((i+1)*FColWidth);
                          aDest.Top:=FBlockRect.Top+aRect.Top;
                          aDest.Bottom:=FBlockRect.Top+aRect.Bottom;          
                          aIndex:=FRealColCount-i-1+j*FRealColCount;
                          if (RedrawMode>=ermFreeRedraw) or 
                            PtInRect(FInvalidRect, Point(aDest.Left+1, aDest.Top+1)) 
                            then Canvas.CopyRect(aDest, Bitmaps[GetItemState].Canvas, aRect);
                        end;
                    end;
                end else
                begin  { Vertical then Horizontal, Left to Right } 
                  for i:=0 to FRealColCount-1 do
                    begin
                      aRect.Top:=round(i*FColWidth);
                      aRect.Bottom:=round((i+1)*FColWidth);
                      aDest.Top:=FBlockRect.Top+aRect.Top;
                      aDest.Bottom:=FBlockRect.Top+aRect.Bottom; 
                      for j:=0 to FRealRowCount-1 do
                        begin
                          aRect.Left:=round(j*FRowHeight);
                          aRect.Right:=round((j+1)*FRowHeight);
                          aDest.Left:=FBlockRect.Left+aRect.Left;
                          aDest.Right:=FBlockRect.Left+aRect.Right;  
                          aIndex:=(FRealColCount-i-1)*FRealRowCount+j;
                          if (RedrawMode>=ermFreeRedraw) or 
                            PtInRect(FInvalidRect, Point(aDest.Left+1, aDest.Top+1)) 
                            then Canvas.CopyRect(aDest, Bitmaps[GetItemState].Canvas, aRect);
                        end;
                    end;    
                end;
            end else
            begin
              if not (egoColumnThenRow in Options) then
                begin  { Horizontal then Vertical, Right to Left }
                  for j:=FRealRowCount-1 downto 0 do
                    begin
                      aRect.Left:=round(j*FRowHeight);
                      aRect.Right:=round((j+1)*FRowHeight);
                      aDest.Left:=FBlockRect.Left+aRect.Left;
                      aDest.Right:=FBlockRect.Left+aRect.Right;
                      for i:=0 to FRealColCount-1 do
                        begin
                          aRect.Top:=round(i*FColWidth);
                          aRect.Bottom:=round((i+1)*FColWidth);
                          aDest.Top:=FBlockRect.Top+aRect.Top;
                          aDest.Bottom:=FBlockRect.Top+aRect.Bottom;
                          aIndex:=i+j*FRealColCount;
                          if (RedrawMode>=ermFreeRedraw) or 
                            PtInRect(FInvalidRect, Point(aDest.Left+1, aDest.Top+1)) 
                            then Canvas.CopyRect(aDest, Bitmaps[GetItemState].Canvas, aRect);
                        end;
                    end;
                end else
                begin  { Vertical then Horizontal, Right to Left }
                  for i:=FRealColCount-1 downto 0 do
                    begin
                      aRect.Top:=round(i*FColWidth);
                      aRect.Bottom:=round((i+1)*FColWidth);
                      aDest.Top:=FBlockRect.Top+aRect.Top;
                      aDest.Bottom:=FBlockRect.Top+aRect.Bottom; 
                      for j:=0 to FRealRowCount-1 do
                        begin
                          aRect.Left:=round(j*FRowHeight);
                          aRect.Right:=round((j+1)*FRowHeight);
                          aDest.Left:=FBlockRect.Left+aRect.Left;
                          aDest.Right:=FBlockRect.Left+aRect.Right;
                          aIndex:=i*FRealRowCount+j;
                          if (RedrawMode>=ermFreeRedraw) or 
                            PtInRect(FInvalidRect, Point(aDest.Left+1, aDest.Top+1)) 
                            then Canvas.CopyRect(aDest, Bitmaps[GetItemState].Canvas, aRect);
                        end;
                    end;    
                end;                     
            end;       
       end;     
    end;
  { Paint Caption and FocusRect }
  if HasCaption then
    begin
      if RedrawMode>=ermFreeRedraw then
        begin
          if Focused then
            begin
              if Orientation=eooHorizontal then
                begin
                  LCLIntf.SetBkColor(Canvas.Handle, ColorToRGB(clForm));
                  LCLIntf.DrawFocusRect(Canvas.Handle, FCaptionRect);
                end else
                Canvas.DrawFocusRectNonThemed(FCaptionRect);
            end;
          aRect:=FCaptionRect;
          InflateRect(aRect, -cFocusRectIndent, -cFocusRectIndent);
          Canvas.Brush.Style:=bsClear;
          if Orientation=eooHorizontal then 
            begin
              Canvas.Font.Orientation:=0;
              aFlags:=DT_SINGLELINE+DT_NOPREFIX;
              if IsRightToLeft then aFlags:=aFlags+DT_RTLREADING;
              with ThemeServices do
                DrawText(Canvas, GetElementDetails(caThemedContent[caItemState[bEnabled]]), 
                  Caption, aRect, aFlags, 0)
            end else
            begin
              Canvas.Font.Orientation:=900;
              Canvas.TextOut(aRect.Left, aRect.Bottom, Caption);
            end;
        end;
    end else
    begin
      if Focused then
        begin
          aRect:=Rect(3, 3, ClientWidth-3, ClientHeight-3);
          Canvas.DrawFocusRectNonThemed(aRect);
        end
    end; 
  FInvalidRect:=Rect(-1, -1, -1, -1);
  RedrawMode:=ermFreeRedraw;
end;

procedure TBaseECGroupCtrl.RecalcInvalidate;
begin
  RedrawMode:=ermRecalcRedraw;
  Invalidate;
end;

procedure TBaseECGroupCtrl.RecalcRedraw;
begin
  {$IFDEF DBGGRP} DebugLn('TCustomECGroupCtrl.RecalcRedraw'); {$ENDIF}      
  RedrawMode:=ermRecalcRedraw;
  if UpdateCount=0 then
    begin
      if AutoSize then
        begin
          InvalidatePreferredSize;
          AdjustSize;
        end;                  
      Invalidate;
    end;   
end;

procedure TBaseECGroupCtrl.Redraw;
begin
  {$IFDEF DBGGRP} DebugLn('TCustomECGroupCtrl.Redraw'); {$ENDIF}
  if RedrawMode<ermRedrawBkgnd then RedrawMode:=ermRedrawBkgnd;
  if UpdateCount=0 then Invalidate;
end;

procedure TBaseECGroupCtrl.Redraw3DColorAreas;
begin
  if (Style=eosPanel) and (RedrawMode<ermRedrawBkgnd) then RedrawMode:=ermRedrawBkgnd; 
  if UpdateCount=0 then Invalidate;
end;

procedure TBaseECGroupCtrl.TextChanged;
begin
  inherited TextChanged;
  RecalcInvalidate;              
end;    

procedure TBaseECGroupCtrl.WMSize(var Message: TLMSize);
begin
  inherited WMSize(Message);
  RedrawMode:=ermRecalcRedraw;
  if UpdateCount=0 then Invalidate;  { because of GTK2 }      
end;    
      
{ TBaseECGroupCtrl.Setters }

procedure TBaseECGroupCtrl.SetBlockColor(AValue: TColor);
begin
  if FBlockColor=AValue then exit;
  FBlockColor:=AValue;
  Redraw;
end; 

procedure TBaseECGroupCtrl.SetCheckedFontColor(AValue: TColor);
begin
  if FCheckedFontColor=AValue then exit;
  FCheckedFontColor:=AValue;
  Redraw;
end;

procedure TBaseECGroupCtrl.SetCheckedFontStyles(AValue: TFontStyles);
begin
  if FCheckedFontStyles=AValue then exit;
  FCheckedFontStyles:=AValue;
  Redraw;   
end; 
         
procedure TBaseECGroupCtrl.SetHighlighted(AValue: SmallInt);
var aOldIndex: SmallInt;
begin
  if FHighlighted=AValue then exit;
  aOldIndex:=FHighlighted;
  FHighlighted:=AValue; 
  if RedrawMode<=ermFreeRedraw then
    begin
      RedrawMode:=ermHoverKnob;
		  if aOldIndex>=0 then 
        if FInvalidRect.Left<0 
          then FInvalidRect:=Items[aOldIndex].FItemRect
          else IncludeRectangle(FInvalidRect, Items[aOldIndex].FItemRect);
	    if AValue>=0 then 
        if FInvalidRect.Left<0 
          then FInvalidRect:=Items[AValue].FItemRect
          else IncludeRectangle(FInvalidRect, Items[AValue].FItemRect);
      InvalidateCustomRect(False);
    end else
    Invalidate;
end;

procedure TBaseECGroupCtrl.SetImages(AValue: TCustomImageList);
begin
  if FImages=AValue then exit;
  FImages:=AValue;
  RecalcInvalidate;
end;

procedure TBaseECGroupCtrl.SetIndent(AValue: SmallInt);
begin
  if FIndent=AValue then exit;
  FIndent:=AValue;
  if HasCaption then RecalcInvalidate;
end;

procedure TBaseECGroupCtrl.SetItems(AValue: TGroupCtrlItems);
begin
  if AValue<>FItems then 
    begin
      FItems.Assign(AValue);
      RecalcInvalidate;
    end;
end;

procedure TBaseECGroupCtrl.SetOptions(AValue: TGCOptions);
begin
  if FOptions=AValue then exit;
  FOptions:=AValue;
  RecalcInvalidate;
end;

procedure TBaseECGroupCtrl.SetRowCount(AValue: SmallInt);
begin
  if (FRowCount=AValue) or (AValue<1) then exit;
  FRowCount:=AValue;
  RecalcInvalidate;
end;

procedure TBaseECGroupCtrl.SetSpacing(AValue: SmallInt);
begin
  if FSpacing=AValue then exit;
  FSpacing:=AValue;
  Redraw;
end;

procedure TBaseECGroupCtrl.SetUncheckedFontColor(AValue: TColor);
begin
  if FUncheckedFontColor=AValue then exit;
  FUncheckedFontColor:=AValue;
  Redraw;
end;

procedure TBaseECGroupCtrl.SetUncheckedFontStyles(AValue: TFontStyles);
begin
  if FUncheckedFontStyles=AValue then exit;
  FUncheckedFontStyles:=AValue;
  Redraw;   
end;

{ TCustomECRadioGroup }

constructor TCustomECRadioGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItemIndex:=-1;
  Items.OnCheckedChange:=@GroupCtrlItemCheckedChanged;
  Items.OnClick:=@GroupCtrlItemClick;
  FOptions:=cDefRGOptions;
  AccessibleRole:=larGroup;
end;

function TCustomECRadioGroup.GetElementDetails(AIState: TItemState): TThemedElementDetails;
const caElements: array [low(TItemState)..high(TItemState)] of TThemedButton = 
  (tbRadioButtonUncheckedDisabled, tbRadioButtonUncheckedHot, tbRadioButtonUncheckedNormal,
   tbRadioButtonCheckedNormal, tbRadioButtonCheckedHot, tbRadioButtonCheckedDisabled);
begin
  Result:=ThemeServices.GetElementDetails(caElements[AIState]);    
end;

procedure TCustomECRadioGroup.GroupCtrlItemCheckedChanged(AIndex: Integer);
var i: Integer;
begin
  if Items[AIndex].Checked then
    begin
      for i:=0 to AIndex-1 do
        TGroupCtrlItem(Items[i]).FChecked:=False;    
      for i:=AIndex+1 to Items.Count-1 do
        TGroupCtrlItem(Items[i]).FChecked:=False;
      ItemIndex:=AIndex;
    end else
    ItemIndex:=-1;  
end;

procedure TCustomECRadioGroup.GroupCtrlItemClick(AIndex: Integer);
begin
  if not (egoAllowAllUp in Options)
    then Items[AIndex].Checked:=True  { default }
    else Items[AIndex].Checked:= not Items[AIndex].Checked;
end;

procedure TCustomECRadioGroup.SetItemIndex(AValue: SmallInt);
var anOldIndex: SmallInt;
begin
  anOldIndex:=FItemIndex;
  if (anOldIndex=AValue) or (AValue>=Items.Count) 
    or ((anOldIndex<0) and (AValue<0)) then exit;
  FItemIndex:=AValue;
  if anOldIndex>=0 then 
    begin
      Items[anOldIndex].FChecked:=False;
      if FInvalidRect.Left<0
        then FInvalidRect:=Items[anOldIndex].FItemRect
        else IncludeRectangle(FInvalidRect, Items[anOldIndex].FItemRect);
    end;
  if AValue>=0 then 
    begin
      Items[AValue].FChecked:=True;
      if FInvalidRect.Left<0
        then FInvalidRect:=Items[AValue].FItemRect
        else IncludeRectangle(FInvalidRect, Items[AValue].FItemRect);  
    end;
  if RedrawMode<=ermFreeRedraw then
    begin
      RedrawMode:=ermMoveKnob;
      InvalidateCustomRect(False);
    end else
    InvalidateNonUpdated;
  if assigned(OnSelectionChange) then OnSelectionChange(self);
end;
     
{ TCustomECCheckGroup }

constructor TCustomECCheckGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Items.OnCheckedChange:=@GroupCtrlItemCheckedChanged;
  Items.OnClick:=@GroupCtrlItemClick;
  FOptions:=cDefCGOptions;
  AccessibleRole:=larGroup;
end;

procedure TCustomECCheckGroup.GroupCtrlItemCheckedChanged(AIndex: Integer);
begin
  if assigned(OnItemClick) then OnItemClick(self, AIndex);
end;       

procedure TCustomECCheckGroup.GroupCtrlItemClick(AIndex: Integer);
var i: Integer;
    bFound, bValue: Boolean;
begin
  bValue:=Items[AIndex].Checked;
  if (egoAllowAllUp in Options) or not bValue
    then Items[AIndex].Checked:= not bValue  { default }
    else                                        
    begin  { at least one checkbox must stay Checked }
      bFound:=False;
      for i:=0 to AIndex-1 do
        if Items[i].Checked then
          begin
            bFound:=True;
            break;
          end;
      if not bFound then
        for i:=AIndex+1 to Items.Count-1 do
          if Items[i].Checked then 
            begin
              bFound:=True;
              break; 
            end;
      if bFound then Items[AIndex].Checked:= not bValue;
    end;    
end;       

function TCustomECCheckGroup.GetElementDetails(AIState: TItemState): TThemedElementDetails;
const caElements: array [low(TItemState)..high(TItemState)] of TThemedButton = 
  (tbCheckBoxUncheckedDisabled, tbCheckBoxUncheckedHot, tbCheckBoxUncheckedNormal,
   tbCheckBoxCheckedNormal, tbCheckBoxCheckedHot, tbCheckBoxCheckedDisabled);
begin
  Result:=ThemeServices.GetElementDetails(caElements[AIState]);
end;

function TCustomECCheckGroup.GetChecked(Index: SmallInt): Boolean;
begin
  Result:=Items[Index].Checked;
end;
       
procedure TCustomECCheckGroup.SetChecked(Index: SmallInt; AValue: Boolean);
begin
  Items[Index].Checked:=AValue;
end;

end.


