{**************************************************************************************************
 This file is part of the Eye Candy Controls (EC-C)

  Copyright (C) 2015-2016 Vojtěch Čihák, Czech Republic

  This library is free software; you can redistribute it and/or modify it under the terms of the
  GNU Library General Public Licenhse as published by the Free Software Foundation; either version
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

unit ECHeader;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, ImgList, LCLType, LMessages,
  Math, Themes, Types, ECTypes;

type
  {$PACKENUM 2}
  TECHeaderItemOption = (ehiEnabled, ehiSizable, ehiVisible);
  TECHeaderItemOptions = set of TECHeaderItemOption;
  TECHeaderOption = (ehoDropDownGlyph, ehoAscDescendant, ehoHighlightPush, ehoAutosizeBreakSection);
  TECHeaderOptions = set of TECHeaderOption;
  TSectionTrackEvent = procedure(Sender: TObject; AIndex, AWidth: Integer) of object;

  { TECHeaderItem }
  TECHeaderItem = class(TCollectionItem)
  private
    FAlignment: TAlignment;
    FImageIndex: SmallInt;
    FMinWidth: SmallInt;
    FOptions: TECHeaderItemOptions;
    FText: TCaption;
    FWidth: Integer;
    procedure SetAlignment(AValue: TAlignment);
    procedure SetImageIndex(AValue: SmallInt);
    procedure SetMinWidth(AValue: SmallInt);
    procedure SetOptions(AValue: TECHeaderItemOptions);
    procedure SetText(AValue: TCaption);
    procedure SetWidth(AValue: Integer);
  protected const
    cDefOptions = [ehiEnabled, ehiSizable, ehiVisible];
    cDefText = 'Item';
    cDefWidth = 75;
  protected
    FBoundLeft, FBoundRight: Integer;
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property ImageIndex: SmallInt read FImageIndex write SetImageIndex default -1;
    property MinWidth: SmallInt read FMinWidth write SetMinWidth default 0;
    property Options: TECHeaderItemOptions read FOptions write SetOptions default cDefOptions;
    property Text: TCaption read FText write SetText;
    property Width: Integer read FWidth write SetWidth;
  end;

  TCustomECHeader = class;

  { TECHeaderItems }
  TECHeaderItems = class(TCollection)
  private
    function GetItems(Index: Integer): TECHeaderItem;
    procedure SetItems(Index: Integer; AValue: TECHeaderItem);
  protected
    FECHeader: TCustomECHeader;
    OnClick: TIntegerMethod;
    function GetOwner: TPersistent; override;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AECHeader: TCustomECHeader);
    function Add: TECHeaderItem;
    procedure Click(AIndex: Integer);
    property Items[Index: Integer]: TECHeaderItem read GetItems write SetItems; default;
  end;

  { TCustomECHeader }
  TCustomECHeader = class(TGraphicControl)
  private
    FBreakIndex: SmallInt;
    FHighlighted: SmallInt;
    FImages: TCustomImageList;
    FOnSectionClick: TIntegerEvent;
    FOnSectionResize: TIntegerEvent;
    FOnSectionTrack: TSectionTrackEvent;
    FOptions: TECHeaderOptions;
    FPushed: SmallInt;
    FSections: TECHeaderItems;
    FSelected: SmallInt;
    procedure SetBreakIndex(AValue: SmallInt);
    procedure SetHighlighted(AValue: SmallInt);
    procedure SetImages(AValue: TCustomImageList);
    procedure SetOptions(AValue: TECHeaderOptions);
    procedure SetPushed(AValue: SmallInt);
    procedure SetSections(AValue: TECHeaderItems);
    procedure SetSelected(AValue: SmallInt);
  protected const
    cDefBreakIndex = 1000;
    cDefIndent: SmallInt = 4;
    cDefOptions = [];
  protected
    Ascendant: Boolean;
    Bitmaps: array [low(TItemState)..eisPushed] of TBitmap;
    DefCursor: TCursor;
    LockCursor: Boolean;
    RedrawMode: TRedrawMode;
    SizeBoundRight: Boolean;
    SizeInitX: Integer;
    SizeSection: Integer;
    Sizing: Boolean;
    procedure Calculate;
    procedure ChangeBounds(ALeft, ATop, AWidth, AHeight: integer; KeepBase: Boolean); override;
    procedure ChangeCursor(ASizing: Boolean);
    procedure CMBiDiModeChanged(var {%H-}Message: TLMessage); message CM_BIDIMODECHANGED;
    procedure DrawBitmaps;
    class function GetControlClassDefaultSize: TSize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure SectionChanged(ARecalculate: Boolean);
    procedure SetCursor(Value: TCursor); override;
    property Pushed: SmallInt read FPushed write SetPushed;
  public
    UpdateCount: SmallInt;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure RecalcRedraw;
    procedure Redraw;
    property BreakIndex: SmallInt read FBreakIndex write SetBreakIndex default 1000;
    property Highlighted: SmallInt read FHighlighted write SetHighlighted;
    property Images: TCustomImageList read FImages write SetImages;
    property Options: TECHeaderOptions read FOptions write SetOptions default cDefOptions;
    property Sections: TECHeaderItems read FSections write SetSections;
    property Selected: SmallInt read FSelected write SetSelected;
    property OnSectionClick: TIntegerEvent read FOnSectionClick write FOnSectionClick;
    property OnSectionResize: TIntegerEvent read FOnSectionResize write FOnSectionResize;
    property OnSectionTrack: TSectionTrackEvent read FOnSectionTrack write FOnSectionTrack;
  end;

  { TECHeader }
  TECHeader = class(TCustomECHeader)
  published
    property Align;
    property Anchors;
    {property AutoSize;}
    property BiDiMode;
    property BorderSpacing;
    property BreakIndex;
    {property Color;}  {does nothing ATM}
    property Constraints;
    property Enabled;
    property Font;
    property Images;
    property Options;
    property PopupMenu;
    property ParentBiDiMode;
    {property ParentColor;}  {does nothing ATM}
    property ParentFont;
    property ParentShowHint;
    property Sections;
    property ShowHint;
    property Visible;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnSectionClick;
    property OnSectionResize;
    property OnSectionTrack;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
  end;

implementation

{ TECHeaderItem }

constructor TECHeaderItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FImageIndex:=-1;
  FOptions:=cDefOptions;
  FWidth:=cDefWidth;
end;

function TECHeaderItem.GetDisplayName: string;
begin
  Result:=Text;
  if Result='' then Result:=cDefText+inttostr(ID);
end;

{ TECHeaderItem.G/Setters }

procedure TECHeaderItem.SetAlignment(AValue: TAlignment);
begin
  if FAlignment=AValue then exit;
  FAlignment:=AValue;
  Changed(False);
end;

procedure TECHeaderItem.SetImageIndex(AValue: SmallInt);
begin
  if FImageIndex=AValue then exit;
  FImageIndex:=AValue;
  Changed(False);
end;

procedure TECHeaderItem.SetMinWidth(AValue: SmallInt);
begin
  if FMinWidth=AValue then exit;
  FMinWidth:=AValue;
  Changed(True);
end;

procedure TECHeaderItem.SetOptions(AValue: TECHeaderItemOptions);
const cRecalcOpts = [ehiVisible];
var bRecalc: Boolean;
begin
  bRecalc:= ((cRecalcOpts*FOptions)<>(cRecalcOpts*AValue));
  if FOptions=AValue then exit;
  FOptions:=AValue;
  Changed(bRecalc);
end;

procedure TECHeaderItem.SetText(AValue: TCaption);
begin
  if FText=AValue then exit;
  FText:=AValue;
  Changed(False);
end;

procedure TECHeaderItem.SetWidth(AValue: Integer);
begin
  if FWidth=AValue then exit;
  FWidth:=AValue;
  Changed(True);
end;

{ TECHeaderItems }

constructor TECHeaderItems.Create(AECHeader: TCustomECHeader);
begin
  inherited Create(TECHeaderItem);
  FECHeader:=AECHeader;
end;

function TECHeaderItems.Add: TECHeaderItem;
begin
  Result:=TECHeaderItem(inherited Add);
end;

procedure TECHeaderItems.Click(AIndex: Integer);
begin
  if assigned(OnClick) then OnClick(AIndex);
end;

function TECHeaderItems.GetOwner: TPersistent;
begin
  Result:=FECHeader;
end;

procedure TECHeaderItems.Notify(Item: TCollectionItem; Action: TCollectionNotification);
begin
  inherited Notify(Item, Action);
  if Action=cnAdded then
    if not (csLoading in (Owner as TCustomECHeader).ComponentState) then
      TECHeaderItem(Item).FText:=TECHeaderItem.cDefText+inttostr(Item.ID);
end;

procedure TECHeaderItems.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  FECHeader.SectionChanged(Item=nil);
end;

{ TECHeaderItems.G/Setters }

function TECHeaderItems.GetItems(Index: Integer): TECHeaderItem;
begin
  Result:=TECHeaderItem(inherited Items[Index]);
end;

procedure TECHeaderItems.SetItems(Index: Integer; AValue: TECHeaderItem);
begin
  Items[Index].Assign(AValue);
end;

{ TCustomECHeader }

constructor TCustomECHeader.Create(AOwner: TComponent);
var aIState: TItemState;
begin
  inherited Create(AOwner);
  ControlStyle:=ControlStyle+[csNoFocus, csParentBackground, csReplicatable]
                            -csMultiClicks-[csCaptureMouse, csOpaque, csSetCaption];
  FBreakIndex:=cDefBreakIndex;
  FHighlighted:=-1;
  FOptions:=cDefOptions;
  FPushed:=-1;
  FSelected:=-1;
  FSections:=TECHeaderItems.Create(self);
  with GetControlClassDefaultSize do
    begin
      for aIState:=low(Bitmaps) to high(Bitmaps) do
        begin
          Bitmaps[aIState]:=TBitmap.Create;
          Bitmaps[aIState].SetProperties(cx, cy);
        end;
      SetInitialBounds(0, 0, cx, cy);
    end;
  DefCursor:=Cursor;
  SizeSection:=-1;
  RedrawMode:=ermRecalcRedraw;
  AccessibleRole:=larLabel;
end;

destructor TCustomECHeader.Destroy;
var aIState: TItemState;
begin
  for aIState:=low(Bitmaps) to high(Bitmaps) do
    if assigned(Bitmaps[aIState]) then FreeAndNil(Bitmaps[aIState]);
  FreeAndNil(FSections);
  inherited Destroy;
end;

procedure TCustomECHeader.BeginUpdate;
begin
  inc(UpdateCount);
end;

procedure TCustomECHeader.Calculate;
var i, aLeft: Integer;
begin
  aLeft:=0;
  for i:=0 to Math.min(BreakIndex, Sections.Count-1) do
    begin
      if ehiVisible in Sections[i].Options then
        begin
          Sections[i].FBoundLeft:=aLeft;
          inc(aLeft, Sections[i].Width);
          Sections[i].FBoundRight:=aLeft;
        end else
        begin
          if i>=1 then
            begin
              Sections[i].FBoundLeft:=Sections[i-1].FBoundLeft;
              Sections[i].FBoundRight:=Sections[i-1].FBoundRight;
            end else
            Sections[0].FBoundRight:=0;
        end;
    end;
  aLeft:=Width;
  for i:=Sections.Count-1 downto BreakIndex+1 do
    begin
      if ehiVisible in Sections[i].Options then
        begin
          Sections[i].FBoundRight:=aLeft;
          dec(aLeft, Sections[i].Width);
          Sections[i].FBoundLeft:=aLeft;
        end else
        begin
          if i<(Sections.Count-1) then
            begin
              Sections[i].FBoundRight:=Sections[i+1].FBoundRight;
              Sections[i].FBoundLeft:=Sections[i+1].FBoundLeft;
            end else
            Sections[i].FBoundLeft:=Width;
        end;
    end;
  i:=BreakIndex;
  if (ehoAutosizeBreakSection in Options) and (i<Sections.Count) then
    if i<(Sections.Count-1)
      then Sections[i].FBoundRight:=FSections[i+1].FBoundLeft
      else Sections[i].FBoundRight:=ClientWidth;
  if IsRightToLeft then
    begin
      for i:=0 to Sections.Count-1 do
        begin
          aLeft:=Sections[i].FBoundLeft;
          Sections[i].FBoundLeft:=Width-Sections[i].FBoundRight;
          Sections[i].FBoundRight:=Width-aLeft;
        end;
    end;
end;

procedure TCustomECHeader.ChangeBounds(ALeft, ATop, AWidth, AHeight: integer; KeepBase: Boolean);
var bNeedResize: Boolean;
    aIState: TItemState;
begin
  bNeedResize:= ((AWidth<>Width) or (AHeight<>Height));
  inherited ChangeBounds(ALeft, ATop, AWidth, AHeight, KeepBase);
  if bNeedResize then
    begin
      for aIState:=low(Bitmaps) to high(Bitmaps) do
        Bitmaps[aIState].SetSize(AWidth, AHeight);
      RedrawMode:=ermRecalcRedraw;
      if UpdateCount=0 then Invalidate;  { because of GTK2 }
    end;
end;

procedure TCustomECHeader.ChangeCursor(ASizing: Boolean);
begin
  LockCursor:=True;
  if not ASizing
    then Cursor:=DefCursor
    else Cursor:=crHSplit;
  LockCursor:=False;
end;

procedure TCustomECHeader.CMBiDiModeChanged(var Message: TLMessage);
begin
  RecalcRedraw;
end;

procedure TCustomECHeader.DrawBitmaps;
const cBkgndsLeft: array[low(TItemState)..eisPushed] of TThemedHeader =
       (thHeaderItemLeftNormal, thHeaderItemLeftHot, thHeaderItemLeftNormal, thHeaderItemLeftPressed);
      cBkgndsRight: array[low(TItemState)..eisPushed] of TThemedHeader =
        (thHeaderItemRightNormal, thHeaderItemRightHot, thHeaderItemRightNormal, thHeaderItemRightPressed);
      cContent: array[low(TItemState)..eisPushed] of TThemedButton =
        (tbPushButtonDisabled, tbPushButtonHot, tbPushButtonNormal, tbPushButtonPressed);
      cArFlags: array[TAlignment, Boolean] of Cardinal =
        ((DT_LEFT, DT_RIGHT), (DT_RIGHT, DT_LEFT), (DT_CENTER, DT_CENTER));
      cFlags = DT_VCENTER+DT_END_ELLIPSIS+DT_NOPREFIX;
var i, aArrowWidth, aImageLeft, aImageTop, aImageWidth: Integer;
    aDetailsBkgnd, aDetailsContent: TThemedElementDetails;
    aFlags: Cardinal;
    aRect: TRect;
    aIState: TItemState;
    bR2L: Boolean;
begin
  aRect.Top:=0;
  aRect.Bottom:=Height;
  bR2L:=IsRightToLeft;
  if ehoDropDownGlyph in Options
    then aArrowWidth:=Canvas.GlyphExtent(egdArrowDown).cx
    else aArrowWidth:=-1;
  if assigned(Images)
    then aImageWidth:=Images.Width
    else aImageWidth:=-1;
  if aImageWidth>=0 then aImageTop:=(Height-Images.Height) div 2;
  for aIState:=low(Bitmaps) to high(Bitmaps) do
    begin
      Bitmaps[aIState].TransparentClear;
      Bitmaps[aIState].Canvas.Font.Assign(Font);
      aDetailsContent:=ThemeServices.GetElementDetails(cContent[aIState]);
      aFlags:=DT_VCENTER;
      for i:=0 to Sections.Count-1 do
        if ehiVisible in Sections[i].Options then
          begin
            aRect.Left:=Sections[i].FBoundLeft;
            aRect.Right:=Sections[i].FBoundRight;
            if aRect.Right<Width
              then aDetailsBkgnd:=ThemeServices.GetElementDetails(cBkgndsLeft[aIState])
              else aDetailsBkgnd:=ThemeServices.GetElementDetails(cBkgndsRight[aIState]);
            ThemeServices.DrawElement(Bitmaps[aIState].Canvas.Handle, aDetailsBkgnd, aRect, nil);
            inc(aRect.Left, cDefIndent);
            dec(aRect.Right, cDefIndent);
            if aArrowWidth>0 then
              begin
                if not bR2L or (Sections[i].Alignment=taCenter)
                  then dec(aRect.Right, aArrowWidth+cDefIndent);
                if bR2L or (Sections[i].Alignment=taCenter)
                  then inc(aRect.Left, aArrowWidth+cDefIndent);
              end;
            if aImageWidth>0 then
              begin
                if not bR2L
                  then aImageLeft:=aRect.Left
                  else aImageLeft:=aRect.Right-aImageWidth;
                ThemeServices.DrawIcon(Bitmaps[aIState].Canvas, aDetailsContent,
                  Point(aImageLeft, aImageTop), Images, FSections[i].ImageIndex);
                if not bR2L or (Sections[i].Alignment=taCenter)
                  then inc(aRect.Left, aImageWidth+cDefIndent);
                if bR2L or (Sections[i].Alignment=taCenter)
                  then dec(aRect.Right, aImageWidth+cDefIndent);
              end;
            aFlags:=cArFlags[Sections[i].Alignment, bR2L] or cFlags;
            ThemeServices.DrawText(Bitmaps[aIState].Canvas, aDetailsContent,
                                   Sections[i].Text, aRect, aFlags, 0);
          end;
    end;
end;

procedure TCustomECHeader.EndUpdate;
begin
  dec(UpdateCount);
  if UpdateCount=0 then Invalidate;
end;

class function TCustomECHeader.GetControlClassDefaultSize: TSize;
begin
  Result.cx:=225;
  Result.cy:=27;
end;

procedure TCustomECHeader.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button=mbLeft then
    if not Sizing then
      if SizeSection<0
        then Pushed:=Highlighted
        else
        begin
          MouseCapture:=True;
          if SizeBoundRight
            then SizeInitX:=X-Sections[SizeSection].FBoundRight
            else SizeInitX:=Sections[SizeSection].FBoundLeft-X;
          Sizing:=True;
        end;
end;

procedure TCustomECHeader.MouseLeave;
begin
  inherited MouseLeave;
  Highlighted:=-1;
end;

procedure TCustomECHeader.MouseMove(Shift: TShiftState; X, Y: Integer);
var i, aBound, aHighlighted: Integer;
    bCanSize, bR2L: Boolean;
begin
  inherited MouseMove(Shift, X, Y);
  if Pushed=-1 then
    if not Sizing then
      begin
        aHighlighted:=-1;
        bCanSize:=False;
        bR2L:=IsRightToLeft;
        for i:=0 to Sections.Count-1 do
          begin
            if not bCanSize and (ehiVisible in Sections[i].Options) then
              begin
                if not ((i=BreakIndex) and (ehoAutosizeBreakSection in Options))
                  and (ehiSizable in Sections[i].Options) then
                  begin
                    if not bR2L xor (i>BreakIndex) then
                      begin
                        aBound:=Sections[i].FBoundRight;
                        SizeBoundRight:=True;
                      end else
                      begin
                        aBound:=Sections[i].FBoundLeft;
                        SizeBoundRight:=False;
                      end;
                    if (X>=(aBound-5)) and (X<=(aBound+2)) then
                      begin
                        SizeSection:=i;
                        bCanSize:=True;
                      end;
                  end;
                if (X>=Sections[i].FBoundLeft) and (X<Sections[i].FBoundRight) then
                  begin
                    if ehiEnabled in Sections[i].Options then aHighlighted:=i;
                  end;
              end;
          end;
        if not bCanSize then SizeSection:=-1;
        ChangeCursor(bCanSize);
        Highlighted:=aHighlighted;
      end else
      begin
        aBound:=Sections[SizeSection].Width;
        if SizeBoundRight then
          begin
            i:=X-Sections[SizeSection].FBoundLeft-SizeInitX;
            i:=Math.Max(i, Sections[SizeSection].MinWidth);
            Sections[SizeSection].Width:=i;
          end else
          begin
            i:=Sections[SizeSection].FBoundRight-X-SizeInitX;
            i:=Math.Max(i, Sections[SizeSection].MinWidth);
            Sections[SizeSection].Width:=i;
          end;
        if (aBound<>i) and assigned(OnSectionTrack) then OnSectionTrack(self, SizeSection, i);
      end;
end;

procedure TCustomECHeader.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Button=mbLeft then
    begin
      if (Highlighted>=0) and not Sizing and assigned(OnSectionClick) then OnSectionClick(self, Pushed);
      if Sizing and assigned(OnSectionResize) then OnSectionResize(self, SizeSection);
      Sizing:=False;
      ChangeCursor(False);
      MouseCapture:=False;
      SizeSection:=-1;
      Pushed:=-1;
    end;
end;

procedure TCustomECHeader.Paint;
var aRect: TRect;

  procedure PaintNonSectionArea(ALeft, ARight: Integer);
  var aDetails: TThemedElementDetails;
  begin
    aRect.Left:=ALeft;
    aRect.Right:=ARight;
    if ARight<Width
      then aDetails:=ThemeServices.GetElementDetails(thHeaderItemNormal)
      else aDetails:=ThemeServices.GetElementDetails(thHeaderItemRightNormal);
    ThemeServices.DrawElement(Canvas.Handle, aDetails, aRect);
  end;

const cArrows: array [Boolean] of TGlyphDesign = (egdArrowDown, egdArrowUp);
var aIState: TItemState;
    aSize: TSize;
    bEnabled: Boolean;
    i, aCountM1: Integer;
begin
  inherited Paint;
  if RedrawMode=ermRecalcRedraw then Calculate;
  bEnabled:=IsEnabled;
  if RedrawMode>=ermRedrawBkgnd then DrawBitmaps;
  if RedrawMode>=ermFreeRedraw then
    begin
      aCountM1:=Sections.Count-1;
      aRect.Top:=0;
      aRect.Bottom:=Height;
      if aCountM1>=0 then
        begin
          for i:=0 to aCountM1 do
            if ehiVisible in Sections[i].Options then
              begin
                aRect.Left:=Sections[i].FBoundLeft;
                aRect.Right:=Sections[i].FBoundRight;
                if bEnabled and (ehiEnabled in Sections[i].Options) then
                  begin
                    if ehoHighlightPush in Options then
                      begin
                        if Pushed=i
                          then aIState:=eisPushed
                          else if Highlighted=i
                                 then aIState:=eisHighlighted
                                 else aIState:=eisEnabled;
                      end else
                      aIState:=eisEnabled;
                  end else
                  aIState:=eisDisabled;
                Canvas.CopyRect(aRect, Bitmaps[aIState].Canvas, aRect);
              end;
          if not IsRightToLeft then
            begin
              if BreakIndex>=aCountM1
                then PaintNonSectionArea(Sections[aCountM1].FBoundRight, Width)
                else if BreakIndex<0
                       then PaintNonSectionArea(0, Sections[0].FBoundLeft)
                       else PaintNonSectionArea(Sections[BreakIndex].FBoundRight,
                                                Sections[BreakIndex+1].FBoundLeft);
            end else
            begin
              if BreakIndex>=aCountM1
                then PaintNonSectionArea(0, Sections[aCountM1].FBoundLeft)
                else if BreakIndex<0
                       then PaintNonSectionArea(Sections[0].FBoundRight, Width)
                       else PaintNonSectionArea(Sections[BreakIndex+1].FBoundRight,
                                                Sections[BreakIndex].FBoundLeft);
            end;
          if (ehoDropDownGlyph in Options) and (Selected>=0) and (Selected<=aCountM1) then
            if ehiVisible in Sections[Selected].Options then
              begin
                aSize:=Canvas.GlyphExtent(egdArrowDown);
                if not IsRightToLeft then
                  begin
                    aRect.Right:=Sections[Selected].FBoundRight-cDefIndent;
                    aRect.Left:=aRect.Right-aSize.cx;
                  end else
                  begin
                    aRect.Left:=Sections[Selected].FBoundLeft+cDefIndent;
                    aRect.Right:=aRect.Left+aSize.cx;
                  end;
                aRect.Top:=(Height-aSize.cy+2) div 2;
                aRect.Bottom:=aRect.Top+aSize.cy;
                Canvas.Pen.Color:=clBtnText;
                Canvas.DrawGlyph(aRect, cArrows[Ascendant], aIState);
              end;
        end else
        PaintNonSectionArea(0, Width);
    end;
  RedrawMode:=ermFreeRedraw;
end;

procedure TCustomECHeader.RecalcRedraw;
begin
  RedrawMode:=ermRecalcRedraw;
  if UpdateCount=0 then Invalidate;
end;

procedure TCustomECHeader.Redraw;
begin
  if RedrawMode<ermRedrawBkgnd then RedrawMode:=ermRedrawBkgnd;
  if UpdateCount=0 then Invalidate;
end;

procedure TCustomECHeader.SectionChanged(ARecalculate: Boolean);
begin
  if not ARecalculate
    then Redraw
    else RecalcRedraw;
end;

procedure TCustomECHeader.SetCursor(Value: TCursor);
begin
  inherited SetCursor(Value);
  if not LockCursor then DefCursor:=Value;
end;

{ TCustomECHeader.G/Setters}

procedure TCustomECHeader.SetBreakIndex(AValue: SmallInt);
begin
  if FBreakIndex=AValue then exit;
  FBreakIndex:=AValue;
  RecalcRedraw;
end;

procedure TCustomECHeader.SetHighlighted(AValue: SmallInt);
begin
  if FHighlighted=AValue then exit;
  FHighlighted:=AValue;
  if (UpdateCount=0) and (ehoHighlightPush in Options) then Invalidate;
end;

procedure TCustomECHeader.SetImages(AValue: TCustomImageList);
begin
  if FImages=AValue then exit;
  FImages:=AValue;
  RecalcRedraw;
end;

procedure TCustomECHeader.SetOptions(AValue: TECHeaderOptions);
const cRecalcOpts = [ehoDropDownGlyph, ehoAutosizeBreakSection];
var bRecalc: Boolean;
begin
  bRecalc:= (FOptions*cRecalcOpts)<>(AValue*cRecalcOpts);
  if FOptions=AValue then exit;
  FOptions:=AValue;
  if bRecalc then RecalcRedraw;
end;

procedure TCustomECHeader.SetPushed(AValue: SmallInt);
begin
  if FPushed=AValue then exit;
  FPushed:=AValue;
  if AValue>=0 then Selected:=AValue;
  if UpdateCount=0 then Invalidate;
end;

procedure TCustomECHeader.SetSections(AValue: TECHeaderItems);
begin
  if AValue<>FSections then
    begin
      FSections.Assign(AValue);
      RecalcRedraw;
    end;
end;

procedure TCustomECHeader.SetSelected(AValue: SmallInt);
begin
  if FSelected=AValue then
    begin
      if ehoAscDescendant in Options
        then Ascendant:= not Ascendant
        else exit;
    end else
    begin
      FSelected:=AValue;
      Ascendant:=False;
    end;
  if UpdateCount=0 then Invalidate;
end;

end.


