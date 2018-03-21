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

unit ECImageMenu;
{$mode objfpc}{$H+}

//{$DEFINE DBGIMGMENU}  {don't remove, just comment} 

interface

uses                                                                  
  Classes, SysUtils, Controls, StdCtrls, Graphics, Forms, ImgList, Math,
  LCLIntf, LCLProc, LCLType, LMessages, Themes, Types, ECTypes;

type    
  { TImageMenuItem }   
  TImageMenuItem = class(TCollectionItem)
  private
    FCaption: TTranslateString;
    FDescription: TTranslateString;
    FImageIndex: SmallInt;
    procedure SetCaption(const AValue: TTranslateString);
    procedure SetDescription(const AValue: TTranslateString);
    procedure SetImageIndex(AValue: SmallInt);
  protected const
    cDefCaption = 'MenuItem';   
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  published
    property Caption: TTranslateString read FCaption write SetCaption;
    property Description: TTranslateString read FDescription write SetDescription;
    property ImageIndex: SmallInt read FImageIndex write SetImageIndex default -1;
  end;
  
  TCustomECImageMenu = class;
  
  { TImageMenuItems }
  TImageMenuItems = class(TCollection)
  private
    function GetItems(Index: Integer): TImageMenuItem;
    procedure SetItems(Index: Integer; AValue: TImageMenuItem);
  protected
    FAddingOrDeletingItem: Boolean;  { Calculate is not needed on Add/Delete item }
    FImageMenu: TCustomECImageMenu;
    function GetOwner: TPersistent; override;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AImageMenu: TCustomECImageMenu);
    function Add: TImageMenuItem;
    property Items[Index: Integer]: TImageMenuItem read GetItems write SetItems; default;
  end;
  
  { TCustomECImageMenu }   
  TCustomECImageMenu = class(TCustomListBox)
  private
    FAlternate: Boolean;
    FCaptionAlign: SmallInt;                    
    FCaptionFontOptions: TFontOptions;
    FImages: TCustomImageList;
    FLayout: TObjectPos;
    FMenuItems: TImageMenuItems;
    FSpacing: SmallInt;
    procedure SetAlternate(AValue: Boolean);
    procedure SetCaptionAlign(AValue: SmallInt);
    procedure SetCaptionFontOptions(AValue: TFontOptions);
    procedure SetImages(AValue: TCustomImageList);
    procedure SetLayout(AValue: TObjectPos);
    procedure SetMenuItems(AValue: TImageMenuItems);
    procedure SetSpacing(AValue: SmallInt);
  protected const
    cDefSpacing = 5;
  protected
    AfterLoad: Boolean;
    CaptionYPos, DescYPos, ImageYPos: Integer;
    NeedCalculate: Boolean;
    procedure Calculate;
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: Integer; 
                                     {%H-}WithThemeSpace: Boolean); override;
    function DialogChar(var Message: TLMKey): boolean; override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    procedure DrawItem(Index: Integer; ARect: TRect; {%H-}State: TOwnerDrawState); override;
    procedure InitializeWnd; override;
    procedure InvalidateNonUpdated;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Loaded; override;
    procedure RecalcInvalidate;
    procedure SetAutoSize(Value: Boolean); override;
    procedure SetBorderStyle(NewStyle: TBorderStyle); override;  
    procedure SetParent(NewParent: TWinControl); override;
  public
    UpdateCount: SmallInt;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate(Recalculate: Boolean = True); 
    procedure Invalidate; override;
    procedure Add(const ACaption, ADescription: TTranslateString; AImageIndex: SmallInt);
    procedure Delete(AIndex: Integer);
    procedure Insert(AIndex: Integer; const ACaption, ADescription: TTranslateString;
      AImageIndex: SmallInt);
    property Alternate: Boolean read FAlternate write SetAlternate default False;
    property CaptionAlign: SmallInt read FCaptionAlign write SetCaptionAlign default 0;
    property CaptionFontOptions: TFontOptions read FCaptionFontOptions write SetCaptionFontOptions;
    property Images: TCustomImageList read FImages write SetImages;
    property Layout: TObjectPos read FLayout write SetLayout default eopTop;
    property MenuItems: TImageMenuItems read FMenuItems write SetMenuItems;
    property Spacing: SmallInt read FSpacing write SetSpacing default cDefSpacing;
  end;
    
  TECImageMenu = class(TCustomECImageMenu)
  published
    property Align;
    property Alternate;
    property Anchors;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property BorderStyle;
    property CaptionAlign;
    property CaptionFontOptions;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Images;
    property Layout;
    property MenuItems;  { do NOT change order MenuItems / ItemIndex }
    property ItemIndex;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEnter;
    property OnEndDrag;
    property OnExit;
    property OnKeyPress;
    property OnKeyDown;
    property OnKeyUp;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnSelectionChange;
    property OnShowHint;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property ParentBidiMode;
    property ParentColor;
    property ParentShowHint;
    property ParentFont;
    property PopupMenu;
    property ScrollWidth;
    property ShowHint;
    property Spacing;
    property TabOrder;
    property TabStop;
    property Visible;            
  end;   
  
implementation

{ TImageMenuItem }  

constructor TImageMenuItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FImageIndex := -1;       
end;

destructor TImageMenuItem.Destroy;
begin
  { normally, Items.Count should be already MenuItems.Count-1 ATM }
  { this solves case when item is not deleted via Collection.Delete(Index) }
  { but directly via Item.Free (exactly what Collection Editor of IDE does) }
  { therefore Notify must be called from here, so count of Items and MenuItems remains same }
  if assigned(Collection) and assigned(Collection.Owner) and
    not (csDestroying in (Collection.Owner as TCustomECImageMenu).ComponentState)
    and (Collection.Count <= (Collection.Owner as TCustomECImageMenu).Items.Count)
    then TImageMenuItems(Collection).Notify(self, cnDeleting);
  inherited Destroy;
end;

function TImageMenuItem.GetDisplayName: string;
begin
  Result := Caption;
  if Result = '' then Result := cDefCaption + inttostr(Index);
end;   

procedure TImageMenuItem.SetCaption(const AValue: TTranslateString);
begin
  if FCaption = AValue then exit;
  FCaption := AValue;
  Changed(Index = 0);  { Measurement is done on the first item } 
end;

procedure TImageMenuItem.SetDescription(const AValue: TTranslateString);
begin
  if FDescription = AValue then exit;
  FDescription := AValue;
  Changed(Index = 0);  { Measurement is done on the first item }
end;

procedure TImageMenuItem.SetImageIndex(AValue: SmallInt);
begin
  if FImageIndex = AValue then exit;
  FImageIndex := AValue;
  Changed(False);
end;              

{ TImageMenuItems }

constructor TImageMenuItems.Create(AImageMenu: TCustomECImageMenu);
begin
  inherited Create(TImageMenuItem);
  FImageMenu := AImageMenu;
end;              

function TImageMenuItems.Add: TImageMenuItem;
begin
  Result := TImageMenuItem(inherited Add);
end;               

function TImageMenuItems.GetOwner: TPersistent;
begin
  Result := FImageMenu; 
end;

procedure TImageMenuItems.Notify(Item: TCollectionItem; Action: TCollectionNotification);
var i: Integer;
begin
  {$IFDEF DBGIMGMENU} DebugLn('TImageMenuItems.Notify'); {$ENDIF}
  inherited Notify(Item, Action);
  case Action of 
    cnAdded:
      begin
        FAddingOrDeletingItem := True; 
        with Owner as TCustomECImageMenu do
          begin
            Items.Add('');   
            if not (csLoading in ComponentState) then 
              TImageMenuItem(Item).FCaption := TImageMenuItem.cDefCaption + inttostr(Item.ID);
          end;
      end;
    cnDeleting:
      begin
        FAddingOrDeletingItem := True;
        with Owner as TCustomECImageMenu do
          begin
            i := ItemIndex;      
            Items.Delete(Item.Index);
            if i < Count then ItemIndex := i
              else if i > 0 then ItemIndex := i - 1;
          end;
      end;
  end;
end;   

procedure TImageMenuItems.Update(Item: TCollectionItem);
begin
  {$IFDEF DBGIMGMENU} DebugLn('TImageMenuItems.Update ', BoolToStr(assigned(Item), 'Item', 'All')); {$ENDIF}
  inherited Update(Item);
  if not (csLoading in FImageMenu.ComponentState) and (assigned(Item) or FAddingOrDeletingItem)
    then FImageMenu.InvalidateNonUpdated
    else FImageMenu.RecalcInvalidate; 
  FAddingOrDeletingItem := False;
end;

{ TImageMenuItems.Setters }

function TImageMenuItems.GetItems(Index: Integer): TImageMenuItem;
begin
  Result := TImageMenuItem(inherited Items[Index]);  
end;

procedure TImageMenuItems.SetItems(Index: Integer; AValue: TImageMenuItem);
begin
  Items[Index].Assign(AValue); 
end;

{ TCustomECImageMenu }

constructor TCustomECImageMenu.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ClickOnSelChange := False;
  FMenuItems := TImageMenuItems.Create(self);
  FCaptionFontOptions := TFontOptions.Create(self);
  with FCaptionFontOptions do
    begin
      FontStyles := [fsBold];
      OnRecalcRedraw := @RecalcInvalidate;
      OnRedraw := @InvalidateNonUpdated;
    end;
  FSpacing := cDefSpacing;
  ExtendedSelect := False;
  MultiSelect := False;
  Style := lbOwnerDrawVariable;  { because of Win32 - it doesn't like lbOwnerDrawFixed }
  AccessibleRole := larMenuBar;
end;                                        

destructor TCustomECImageMenu.Destroy;
begin
  FreeAndNil(FCaptionFontOptions);
  FreeAndNil(FMenuItems);
  inherited Destroy;
end;  

procedure TCustomECImageMenu.Add(const ACaption, ADescription: TTranslateString; AImageIndex: SmallInt);
begin
  Insert(MenuItems.Count, ACaption, ADescription, AImageIndex);
end;

procedure TCustomECImageMenu.BeginUpdate;
begin
  inc(UpdateCount);
end;

procedure TCustomECImageMenu.Calculate;
var aCaptionHeight, aDescHeight, aImagesHeight, aLeftHeight, aRightHeight, aItemHeight: Integer;
    aCaption: string;
    aBMP: TBitmap;
begin
  {$IFDEF DBGIMGMENU} DebugLn('TCustomECImageMenu.Calculate'); {$ENDIF}
  if assigned(Images) 
    then aImagesHeight := Images.Height
    else aImagesHeight := 0;    
  aCaptionHeight := 0;
  aDescHeight := 0;
  if MenuItems.Count > 0 then
    begin
      aBMP := TBitmap.Create;
      aBMP.Canvas.Font.Assign(self.Font);  { Description is written with default Font }
      aCaption := MenuItems[0].Description;
      if aCaption <> '' then aDescHeight := aBMP.Canvas.TextHeight(aCaption);
      aBMP.Canvas.Font.Size := CaptionFontOptions.FontSize;
      aBMP.Canvas.Font.Style := CaptionFontOptions.FontStyles;
      aCaption := MenuItems[0].Caption;  
      if aCaption <> '' then aCaptionHeight := aBMP.Canvas.TextHeight(aCaption);
      FreeAndNil(aBMP);
    end;     
  case FLayout of
    eopTop: 
      begin
        ImageYPos := Spacing;
        if aImagesHeight > 0
          then CaptionYPos := ImageYPos + aImagesHeight + Spacing
          else CaptionYPos := ImageYPos;
        if aCaptionHeight > 0
          then DescYPos := CaptionYPos + aCaptionHeight + Spacing
          else DescYPos := CaptionYPos;
        if aDescHeight > 0
          then aItemHeight := DescYPos + aDescHeight + Spacing
          else aItemHeight := DescYPos;
      end;    
    eopBottom:
      begin
        CaptionYPos := Spacing;
        if aCaptionHeight > 0 
          then ImageYPos := CaptionYPos + aCaptionHeight + Spacing
          else ImageYPos := CaptionYPos;    
        if aImagesHeight > 0 
          then DescYPos := ImageYPos + aImagesHeight + Spacing
          else DescYPos := ImageYPos;
        if aDescHeight > 0
          then aItemHeight := DescYPos + aDescHeight + Spacing
          else aItemHeight := DescYPos;
      end; 
    otherwise  { eopRight, eopLeft }
      if aImagesHeight > 0
        then aLeftHeight := aImagesHeight + 2*Spacing
        else aLeftHeight := Spacing; 
      aRightHeight := Spacing;
      if aCaptionHeight > 0 then aRightHeight := aRightHeight + aCaptionHeight + Spacing;
      if aDescHeight > 0 then aRightHeight := aRightHeight + aDescHeight + Spacing;
      aItemHeight := Math.max(aLeftHeight, aRightHeight);
      ImageYPos := (aItemHeight - aImagesHeight) div 2;
      if (aCaptionHeight > 0) xor (aDescHeight > 0) then
        begin
          CaptionYPos := (aItemHeight - aCaptionHeight) div 2;
          DescYPos := CaptionYPos;
        end else
        begin
          CaptionYPos := (aItemHeight - aCaptionHeight - aDescHeight - Spacing) div 2;
          DescYPos := CaptionYPos + aCaptionHeight + Spacing;
        end;
  end; 
  inc(UpdateCount);  { this avoids calling Calculate twice }
  ItemHeight := aItemHeight;
  dec(UpdateCount);
  NeedCalculate := False;
  {$IFDEF DBGIMGMENU} DebugLn(DbgSName(self),'.Calc ', inttostr(aItemHeight)); {$ENDIF}
end;  

procedure TCustomECImageMenu.CalculatePreferredSize(var PreferredWidth, PreferredHeight: Integer; 
  WithThemeSpace: Boolean);
var aCaption: string;
    aImageWidth, aTextWidth, i: Integer; 
    aRect: TRect;
begin        
  PreferredHeight := 0;
  if assigned(Images)
    then aImageWidth := Images.Width
    else aImageWidth := 0;
  aTextWidth := 0;
  Canvas.Font.Assign(Font);
  for i := 0 to MenuItems.Count - 1 do
    aTextWidth := Math.max(aTextWidth, Canvas.TextWidth(MenuItems[i].Description));
  Canvas.Font.Size := CaptionFontOptions.FontSize;
  Canvas.Font.Style := CaptionFontOptions.FontStyles;
  for i := 0 to MenuItems.Count - 1 do 
    begin
      aCaption := MenuItems[i].Caption;
      DeleteAmpersands(aCaption);
      aTextWidth := Math.max(aTextWidth, Canvas.TextWidth(aCaption));
    end;
  LCLIntf.GetClientRect(Handle, {%H-}aRect);  { Calc. left + right border }
  i := Width - aRect.Right;
  if Layout in [eopRight, eopLeft] then
    begin
      if aImageWidth*aTextWidth > 0 then inc(aImageWidth, Spacing);
      PreferredWidth := aImageWidth + aTextWidth + 2*Spacing + i;
    end else 
    PreferredWidth := Math.max(aImageWidth, aTextWidth) + 2*Spacing + i;
end;  

procedure TCustomECImageMenu.Delete(AIndex: Integer);
begin
  BeginUpdate;
  MenuItems.Delete(AIndex);
  EndUpdate(False);
end;                                        

function TCustomECImageMenu.DialogChar(var Message: TLMKey): boolean;
var i: Integer;
begin
  Result := False;
  if Message.Msg = LM_SYSCHAR then 
    begin
      if IsEnabled and IsVisible then
        begin
          for i := 0 to MenuItems.Count - 1 do
            if IsAccel(Message.CharCode, MenuItems[i].Caption) then
              begin
                Selected[i] := True;
                SetFocus; 
                Result := True;  
                Click;
                exit;  { Exit! }
              end;
          Result := inherited DialogChar(Message);  
        end;
    end;
end;   

function TCustomECImageMenu.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelDown(Shift, MousePos);
  if ItemIndex < (Items.Count - 1) then ItemIndex := ItemIndex + 1;
end;

function TCustomECImageMenu.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelUp(Shift, MousePos);
  if ItemIndex > 0 then ItemIndex := ItemIndex - 1;
end;  

procedure TCustomECImageMenu.DrawItem(Index: Integer; ARect: TRect; State: TOwnerDrawState);
var aDetails: TThemedElementDetails;
    aFlags: Cardinal;
    aImagePoint: TPoint;
    aTextRect: TRect;
    bEnabled: Boolean;
begin  { do not call inherited ! }
  bEnabled := IsEnabled;                
  if odSelected in State then
    begin
      if not Focused then Canvas.Brush.Color :=
        GetMergedColor(Canvas.Brush.Color, GetColorResolvingDefault(Color, Brush.Color), 0.6);
    end else
    begin
      if ((Index and 1) = 1) and Alternate then Canvas.Brush.Color :=
        GetMergedColor(Canvas.Brush.Color, ColorToRGB(clForm), 0.5);
    end;
  if not bEnabled then
    Canvas.Brush.Color := GetMonochromaticColor(Canvas.Brush.Color);
  Canvas.FillRect(ARect);
  aDetails := ThemeServices.GetElementDetails(caThemedContent[caItemState[bEnabled]]);
  aTextRect.Left := ARect.Left + Spacing;
  aTextRect.Right := ARect.Right - Spacing;
  aTextRect.Bottom := ARect.Bottom; 
  if assigned(Images) then
    begin
      case Layout of
        eopRight: 
          begin
            aImagePoint.X := ARect.Right - Images.Width - Spacing;
            dec(aTextRect.Right, Images.Width + Spacing);
          end;
        eopLeft: 
          begin
            aImagePoint.X := ARect.Left + Spacing; 
            inc(aTextRect.Left, Images.Width + Spacing);
          end;
        otherwise aImagePoint.X := (ARect.Right - ARect.Left - Images.Width) div 2; 
      end;
      aImagePoint.Y := ARect.Top + ImageYPos;
      ThemeServices.DrawIcon(Canvas, aDetails, aImagePoint, Images, MenuItems[Index].ImageIndex);
    end;  
  aFlags := DT_END_ELLIPSIS or DT_SINGLELINE;
  if IsRightToLeft then aFlags := aFlags or DT_RTLREADING;
  if (Layout in [eopTop, eopBottom]) or (CaptionAlign = 0)
    then aFlags := aFlags or DT_CENTER
    else if (Layout = eopRight) xor (CaptionAlign > 0) then aFlags := aFlags or DT_RIGHT;
  if MenuItems[Index].Description <> '' then
    begin
      aTextRect.Top := ARect.Top + DescYPos;
      ARect.Left := aTextRect.Left;
      ARect.Right := aTextRect.Right; 
      if Layout in [eopTop, eopBottom] then
        begin
          if CaptionAlign > 0 then dec(aTextRect.Right, 2*CaptionAlign)
            else dec(aTextRect.Left, 2*CaptionAlign);         
        end;
      Canvas.Font.Assign(Font);
      if (odSelected in State) and (Font.Color = clDefault) then Canvas.Font.Color := clHighlightText;
      ThemeServices.DrawText(Canvas, aDetails, MenuItems[Index].Description, 
        aTextRect, aFlags or DT_NOPREFIX, 0);
      aTextRect.Left := ARect.Left;
      aTextRect.Right := ARect.Right;
    end;
  if MenuItems[Index].Caption <> '' then
    begin
      if CaptionFontOptions.FontColor <> clDefault
        then Canvas.Font.Color := CaptionFontOptions.FontColor
        else if odSelected in State then Canvas.Font.Color := clHighlightText;
      Canvas.Font.Size := CaptionFontOptions.FontSize;
      Canvas.Font.Style := CaptionFontOptions.FontStyles;
      aTextRect.Top := ARect.Top + CaptionYPos;
      if Layout in [eopTop, eopBottom] then
        begin
          if CaptionAlign > 0
            then inc(aTextRect.Left, 2*CaptionAlign)
            else inc(aTextRect.Right, 2*CaptionAlign);           
        end;  
      ThemeServices.DrawText(Canvas, aDetails, MenuItems[Index].Caption, aTextRect, aFlags, 0);
    end;  
end;        

procedure TCustomECImageMenu.EndUpdate(Recalculate: Boolean = True);
begin
  dec(UpdateCount);
  if UpdateCount = 0 then 
    if Recalculate
      then RecalcInvalidate
      else Invalidate;
end; 

procedure TCustomECImageMenu.InitializeWnd;
begin     
  {$IFDEF DBGIMGMENU} DebugLn('InitWnd ', inttostr(Width), ' ', inttostr(Height), ' Count ', inttostr(MenuItems.Count)); {$ENDIF}
  if AfterLoad and not NeedCalculate then
    begin
      AfterLoad := False;
      exit;
    end; 
  if (MenuItems.Count > 0) and (UpdateCount = 0) then Calculate;
  inherited InitializeWnd;
end; 

procedure TCustomECImageMenu.Insert(AIndex: Integer; const ACaption, ADescription: TTranslateString;
  AImageIndex: SmallInt);
var aItem: TCollectionItem;
begin
  if (AIndex >= 0) and (AIndex <= MenuItems.Count) then
    begin
      BeginUpdate;
      aItem := MenuItems.Insert(AIndex);
      with aItem as TImageMenuItem do
        begin
          Caption := ACaption;
          Description := ADescription;
          ImageIndex := AImageIndex;
        end;
      EndUpdate(False);
    end;
end;        

procedure TCustomECImageMenu.Invalidate;
begin
  {$IFDEF DBGIMGMENU} DebugLn('TCustomECImageMenu.Invalidate'); {$ENDIF}
  if NeedCalculate and (MenuItems.Count > 0) and HandleAllocated then Calculate;
  inherited Invalidate;
end;        

procedure TCustomECImageMenu.InvalidateNonUpdated;
begin
  {$IFDEF DBGIMGMENU} DebugLn('TCustomECIM.InvalidateNonUpdate'); {$ENDIF}
  if UpdateCount = 0 then Invalidate;
end; 

procedure TCustomECImageMenu.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if Key in [VK_RETURN, VK_SPACE] then Click;
end; 

procedure TCustomECImageMenu.Loaded;
begin
  {$IFDEF DBGIMGMENU} DebugLn('TCustomECImageMenu.Loaded'); {$ENDIF}
  inherited Loaded;
  AfterLoad := True;       
end; 

procedure TCustomECImageMenu.RecalcInvalidate;
begin
  {$IFDEF DBGIMGMENU} DebugLn('TCustomECImageMenu.Recalc'); {$ENDIF}
  if UpdateCount = 0 then
    begin  
      if AutoSize then
        begin
          InvalidatePreferredSize;
          BeginUpdateBounds;
          AdjustSize;
          EndUpdateBounds;
        end;   
      NeedCalculate := True;
      Invalidate;
    end;
end;  

procedure TCustomECImageMenu.SetAutoSize(Value: Boolean);
begin
  {$IFDEF DBGIMGMENU} DebugLn('TCustomECImageMenu.SetAutoSize'); {$ENDIF}
  inherited SetAutoSize(Value);
  if Value then 
    begin
      InvalidatePreferredSize;
      AdjustSize;
      NeedCalculate := True;
      Invalidate;   
    end;
end;  

procedure TCustomECImageMenu.SetBorderStyle(NewStyle: TBorderStyle);
begin
  inherited SetBorderStyle(NewStyle);
  if AutoSize then RecalcInvalidate;
end;                

procedure TCustomECImageMenu.SetParent(NewParent: TWinControl);
begin
  {$IFDEF DBGIMGMENU} DebugLn('TCustomECImageMenu.SetParent'); {$ENDIF}
  inc(UpdateCount);																
  inherited SetParent(NewParent);
  if assigned(NewParent) and (MenuItems.Count > 0) then Calculate;
  dec(UpdateCount);
end;         
       
{ Setters }

procedure TCustomECImageMenu.SetAlternate(AValue: Boolean);
begin
  if FAlternate = AValue then exit;
  FAlternate := AValue;
  InvalidateNonUpdated;
end; 

procedure TCustomECImageMenu.SetCaptionAlign(AValue: SmallInt);
begin
  if FCaptionAlign = AValue then exit;
  FCaptionAlign := AValue;
  InvalidateNonUpdated;
end;       

procedure TCustomECImageMenu.SetCaptionFontOptions(AValue: TFontOptions);
begin
  if FCaptionFontOptions = AValue then exit;
  FCaptionFontOptions := AValue;
  RecalcInvalidate;
end;

procedure TCustomECImageMenu.SetImages(AValue: TCustomImageList);
begin
  if FImages = AValue then exit;
  FImages := AValue;
  RecalcInvalidate;
end;         

procedure TCustomECImageMenu.SetLayout(AValue: TObjectPos);
begin
  if FLayout = AValue then exit;
  FLayout := AValue;
  RecalcInvalidate;
end;   

procedure TCustomECImageMenu.SetMenuItems(AValue: TImageMenuItems);
begin
  if FMenuItems <> AValue then
    begin
      FMenuItems.Assign(AValue);
      RecalcInvalidate;
    end;
end;         

procedure TCustomECImageMenu.SetSpacing(AValue: SmallInt);
begin
  if FSpacing = AValue then exit;
  FSpacing := AValue;
  RecalcInvalidate;
end;         

end.

