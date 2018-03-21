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

unit ECDesignTime;
{$mode objfpc}{$H+}

interface

uses
  Classes, ComponentEditors, PropEdits, Menus, Controls, LResources, ImgList,
  GraphPropEdits, typinfo, ECBevel, ECLink, ECImageMenu, ECSpinCtrls, ECSwitch,
  ECEditBtns, ECHeader, ECCheckListBox, ECSlider, ECProgressBar, ECRuler, ECGroupCtrls,
  ECConfCurve, ECScheme, ECTabCtrl, ECAccordion, ECTriangle;

type
  { TECTabCtrlEditor }
  TECTabCtrlEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure PrepareItem(Index: Integer; const AnItem: TMenuItem); override;
  end;

  { TECAccordionEditor }
  TECAccordionEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function FindUniqueName(const Name: string): string;
    function GetAccordion: TECAccordion;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure PrepareItem(Index: Integer; const AnItem: TMenuItem); override;
  end;

  { THookAccItemSelection }
  THookAccItemSelection = class
  protected
    procedure HookSelection(const ASelection: TPersistentSelectionList);
  end;

  { TECImageIndexPropEdit }

  TECImageIndexPropEdit = class(TImageIndexPropertyEditor)
  protected
    function GetImageList: TCustomImageList; override;
  end;

procedure Register;

implementation

var HookAccItemSelection: THookAccItemSelection;

resourcestring
  { TECTabCtrl }
  rsETCAddDT = 'Add Tab';
  rsETCInsertDT = 'Insert Tab';
  rsETCDeleteDT = 'Delete Tab';
  rsETCMoveLeftDT = 'Move Left';
  rsETCMoveRightDT = 'Move Right';
  { TECAccordion }
  rsEAccAdd = 'Add Item';
  rsEAccInsert = 'Insert Item';
  rsEAccDelete = 'Delete Item';
  rsEAccPrevious = 'Previous Item';
  rsEAccNext = 'Next Item';
  rsEAccMoveUp = 'Move Up';
  rsEAccMoveDown = 'Move Down';

{ TECTabCtrlEditor }

procedure TECTabCtrlEditor.ExecuteVerb(Index: Integer);
var aECTC: TECTabCtrl;
    aECTab: TECTab;
    aHook: TPropertyEditorHook;
begin
  if Component is TECTabCtrl
    then aECTC:=TECTabCtrl(Component)
    else exit;  { Exit! }
    begin
      aHook:=nil;
      if not GetHook(aHook) then exit;  { Exit! }
      case Index of
        0: begin
             aECTab:=aECTC.AddTab(etaLast, True);
             aHook.PersistentAdded(aECTab, True);
           end;
        1: begin
             aECTab:=aECTC.AddTab(etaBeside, True);
             aHook.PersistentAdded(aECTab, True);
           end;
        2: begin
             aECTab:=aECTC.Tabs[aECTC.TabIndex];
             aHook.PersistentDeleting(aECTab);
             aECTC.DeleteTab(aECTC.TabIndex);
             aHook.SelectOnlyThis(aECTC.Tabs);  { force the OI to refresh }
           end;
        3: aECTC.MovePrevious();
        4: aECTC.MoveNext();
      end;
      Modified;
    end;
end;

function TECTabCtrlEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result:=rsETCAddDT;
    1: Result:=rsETCInsertDT;
    2: Result:=rsETCDeleteDT;
    3: Result:=rsETCMoveLeftDT;
    4: Result:=rsETCMoveRightDT;
  end;
end;

function TECTabCtrlEditor.GetVerbCount: Integer;
begin
  Result:=5;
end;

procedure TECTabCtrlEditor.PrepareItem(Index: Integer; const AnItem: TMenuItem);
var aECTC: TECTabCtrl;
begin
  inherited PrepareItem(Index, AnItem);
  aECTC:=TECTabCtrl(GetComponent);
  case Index of
    1: AnItem.Enabled:= (aECTC.Tabs.Count>0);
    2: AnItem.Enabled:= (aECTC.TabIndex>=0);
    3: AnItem.Enabled:= (aECTC.TabIndex>0);
    4: AnItem.Enabled:= (aECTC.TabIndex<(aECTC.Tabs.Count-1));
  end;
end;

{ TECAccordionEditor }

procedure TECAccordionEditor.ExecuteVerb(Index: Integer);
var aECAcc: TECAccordion;
    aAccItem: TAccordionItem;
    aHook: TPropertyEditorHook;

  procedure AddHelper;
  begin
    aAccItem.Name:=FindUniqueName('AccordionItem');
    aAccItem.Caption:=aAccItem.Name;
    aHook.PersistentAdded(aAccItem, True);
  end;

begin
  aECAcc:=GetAccordion;
  aHook:=nil;
  if not GetHook(aHook) then exit;  { Exit! }
  case Index of
    0:
      begin
        aAccItem:=aECAcc.AddItem(Designer.Form);
        AddHelper;
      end;
    1:
      begin
        aAccItem:=aECAcc.InsertItem(Designer.Form, aECAcc.ItemIndex);
        AddHelper;
      end;
    2:
      begin
        aAccItem:=aECAcc.ActiveItem;
        aHook.PersistentDeleting(aAccItem);
        aECAcc.DeleteItem(aAccItem);
      end;
    3: aECAcc.FindPreviousItem;
    4: aECAcc.FindNextItem;
    5: aECAcc.MoveItemUp;
    6: aECAcc.MoveItemDown;
  end;
  Modified;
end;

function TECAccordionEditor.FindUniqueName(const Name: string): string;
var aFormDesigner: TComponentEditorDesigner;
begin
  aFormDesigner:=GetDesigner;
  Result:=aFormDesigner.UniqueName(Name);
end;

function TECAccordionEditor.GetAccordion: TECAccordion;
begin
  if Component is TECAccordion
    then Result:=TECAccordion(Component)
    else if Component is TAccordionItem
           then Result:=TECAccordion(TAccordionItem(Component).Accordion);
end;

function TECAccordionEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result:=rsEAccAdd;
    1: Result:=rsEAccInsert;
    2: Result:=rsEAccDelete;
    3: Result:=rsEAccPrevious;
    4: Result:=rsEAccNext;
    5: Result:=rsEAccMoveUp;
    6: Result:=rsEAccMoveDown;
  end;
end;

function TECAccordionEditor.GetVerbCount: Integer;
begin
  Result:=7;
end;

procedure TECAccordionEditor.PrepareItem(Index: Integer; const AnItem: TMenuItem);
var aECAcc: TECAccordion;
begin
  inherited PrepareItem(Index, AnItem);
  aECAcc:=GetAccordion;
  case Index of
    1, 2: AnItem.Enabled:= (aECAcc.ItemIndex>=0);
    3, 5: AnItem.Enabled:= (aECAcc.ItemIndex>0);
    4, 6: AnItem.Enabled:= (aECAcc.ItemIndex<(aECAcc.Items.Count-1));
  end;
end;

{ THookAccItemSelection }

procedure THookAccItemSelection.HookSelection(const ASelection: TPersistentSelectionList);
var i: Integer;
    aAccItem: TAccordionItem;

  procedure SearchAccItemInclParents(APersistent: TPersistent);
  begin
    if APersistent is TControl then
      begin
        if APersistent is TAccordionItem
          then aAccItem:=TAccordionItem(APersistent)
          else SearchAccItemInclParents(TControl(APersistent).Parent);
      end else
      aAccItem:=nil;
  end;

begin
  for i:=0 to ASelection.Count-1 do
    begin
      SearchAccItemInclParents(ASelection[i]);
      if assigned(aAccItem) then
        begin
          if assigned(aAccItem.Accordion) then
            begin
              aAccItem.Accordion.ItemIndex:=aAccItem.Accordion.Items.IndexOf(aAccItem);
              break;
            end;
        end;
    end;
end;

{ TECImageIndexPropEdit }

function TECImageIndexPropEdit.GetImageList: TCustomImageList;
var aPersistent: TPersistent;
    aPropInfo: PPropInfo;
    aObj: TObject;
begin
  Result:=nil;
  aPersistent:=GetComponent(0);
  if aPersistent=nil then exit;  { Exit! }
  if (aPersistent is TCustomECSpeedBtn) or (aPersistent is TBaseECSlider) then
    begin
      aPropInfo:=TypInfo.GetPropInfo(TComponent(aPersistent), 'Images');
      if aPropInfo=nil then exit;  { Exit! }
      aObj:=GetObjectProp(TComponent(aPersistent), aPropInfo);
      if aObj is TCustomImageList then Result:=TCustomImageList(aObj);
    end else
    if aPersistent is TSingleSpinBtn then
      begin
        aPropInfo:=TypInfo.GetPropInfo(TSingleSpinBtn(aPersistent).Parent, 'Images');
        if aPropInfo=nil then exit;  { Exit! }
        aObj:=GetObjectProp(TSingleSpinBtn(aPersistent).Parent, aPropInfo);
        if aObj is TCustomImageList then Result:=TCustomImageList(aObj);
      end;
end;

procedure Register;
begin
  {$I ecbevel.lrs}
  {$I eclink.lrs}
  {$I ecimagemenu.lrs}
  {$I ecspinctrls.lrs}
  {$I eceditbtns.lrs}
  {$I echeader.lrs}
  {$I ecchecklistbox.lrs}
  {$I ecslider.lrs}
  {$I ecprogressbar.lrs}
  {$I ecruler.lrs}
  {$I ecgroupctrls.lrs}
  {$I ecaccordion.lrs}
  {$I ectriangle.lrs}
  {$I ecconfcurve.lrs}
  {$I ecscheme.lrs}
  RegisterComponents('EC-C', [TECBevel, TECLink, TECImageMenu, TECSpinBtns, TECSpinEdit,
    TECSpinController, TECTimer, TECSwitch, TECSpeedBtn, TECEditBtn, TECColorBtn, TECComboBtn,
    TECColorCombo, TECHeader, TECCheckListBox, TECSlider, TECProgressBar, TECPositionBar,
    TECSpinPosition, TECRuler, TECRadioGroup, TECCheckGroup, TECTabCtrl, TECAccordion,
    TECTriangle, TECConfCurve, TECScheme]);
  RegisterPropertyEditor(TypeInfo(TCaption), TCustomECSpeedBtn, 'Caption', TStringMultilinePropertyEditor);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TSingleSpinBtn, 'ImageIndex', TECImageIndexPropEdit);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TBaseECSlider, 'ImageIndex', TECImageIndexPropEdit);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TCustomECSpeedBtn, 'ImageIndex', TECImageIndexPropEdit);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TCustomECSpeedBtn, 'ImageIndexChecked', TECImageIndexPropEdit);
  RegisterComponentEditor(TECTabCtrl, TECTabCtrlEditor);
  RegisterNoIcon([TAccordionItem]);
  RegisterComponentEditor(TECAccordion, TECAccordionEditor);
  GlobalDesignHook.AddHandlerSetSelection(@HookAccItemSelection.HookSelection);
  RegisterComponentEditor(TAccordionItem, TECAccordionEditor);
end;

initialization

  HookAccItemSelection:=THookAccItemSelection.Create;

finalization

  if assigned(HookAccItemSelection) then HookAccItemSelection.Free;

end.


