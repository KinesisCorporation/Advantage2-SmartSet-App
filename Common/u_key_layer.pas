unit u_key_layer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, u_const, u_keys;

type

  { TKBKey }

  TKBKey = class
  private
    FOriginalKey: TKey;
    FModifiedKey: TKey;
    FIndex: integer;
    //FButtonName: string;
    FIsModified: boolean;
    FIsMacro: boolean;
    FCanEdit: boolean;
    FCanAssignMacro: boolean;
    FMacro1: TKeyList;
    FMacro2: TKeyList;
    FMacro3: TKeyList;
    FActiveMacro: TKeyList;
    procedure Init;
  protected
    //function CopyKey: TKBKey;
    //function Compare(aKey: TKBKey): boolean;
    function GetActiveMacro: TKeyList;
    procedure SetActiveMacro(macro: TKeyList);
  public
    constructor Create;
    constructor Create(oOriginalKey: TKey; iIndex: integer; bCanEdit: boolean = true; bCanAssignMacro: boolean = true);
    procedure ResetKey;
    procedure Assign(aKbKey: TKBKey; restoreType: TRestoreType);
    function MacroCount: integer;

    property OriginalKey: TKey read FOriginalKey write FOriginalKey;
    property ModifiedKey: TKey read FModifiedKey write FModifiedKey;
    property Index: integer read FIndex write FIndex;
    //property ButtonName: string read FButtonName write FButtonName;
    property IsModified: boolean read FIsModified write FIsModified;
    property IsMacro: boolean read FIsMacro write FIsMacro;
    property CanEdit: boolean read FCanEdit write FCanEdit;
    property CanAssignMacro: boolean read FCanAssignMacro write FCanAssignMacro;
    property Macro1: TKeyList read FMacro1 write FMacro1;
    property Macro2: TKeyList read FMacro2 write FMacro2;
    property Macro3: TKeyList read FMacro3 write FMacro3;
    property ActiveMacro: TKeyList read GetActiveMacro write SetActiveMacro;
  end;

  { TKBKeyList }

  TKBKeyList = class(TObjectList)
  private
  protected
    function GetItem(index: integer): TKBKey;
    procedure SetItem(Index: integer; AObject: TKBKey);
  public
    constructor Create;
    function Add(AObject: TKBKey): integer; reintroduce; virtual;
    function Remove(AObject: TKBKey): integer; reintroduce; virtual;
    procedure Assign(aKBKeyList: TKBKeyList);
    //function Compare(aKBKeyList: TKBKeyList): boolean;
    property Items[index: integer]: TKBKey read GetItem write SetItem; default;
  end;

  { TKBLayer }

  TKBLayer = class
  private
    FKBKeyList: TKBKeyList;
    FLayerIndex: integer;
    FLayerName: string;
    FLayerType: integer;
  public
    constructor Create;
    property KBKeyList: TKBKeyList read FKBKeyList write FKBKeyList;
    property LayerIndex: integer read FLayerIndex write FLayerIndex;
    property LayerName: string read FLayerName write FLayerName;
    property LayerType: integer read FLayerType write FLayerType;
  end;

  { TKBKeyList }

  { TKBLayerList }

  TKBLayerList = class(TObjectList)
  private
  protected
    function GetItem(index: integer): TKBLayer;
    procedure SetItem(Index: integer; AObject: TKBLayer);
  public
    constructor Create;
    function Add(AObject: TKBLayer): integer; reintroduce; virtual;
    function Remove(AObject: TKBLayer): integer; reintroduce; virtual;
    property Items[index: integer]: TKBLayer read GetItem write SetItem; default;
  end;

implementation

{ TKBLayerList }

function TKBLayerList.GetItem(index: integer): TKBLayer;
begin
  Result := nil;
    try
      Result := inherited Items[Index] as TKBLayer
    except
      //do nothing
      //on E : Exception do HandleExcept(E, True, 'Error in TKey.GetItem. Index out of range.');
    end;
end;

procedure TKBLayerList.SetItem(Index: integer; AObject: TKBLayer);
begin
  try
    inherited Items[Index] := AObject
  except
    //do nothing
    //on E : Exception do HandleExcept(E, True, 'Error in TKeyList.SetItem. Index out of range.');
  end;
end;

constructor TKBLayerList.Create;
begin
  inherited Create;
  OwnsObjects := True;
end;

function TKBLayerList.Add(AObject: TKBLayer): integer;
begin
  Result := inherited Add(AObject);
end;

function TKBLayerList.Remove(AObject: TKBLayer): integer;
begin
  Result := inherited Remove(AObject);
end;

{ TKBKey }

//function TKBKey.CopyKey: TKBKey;
//begin
//  Result := nil;
//  if self <> nil then
//  begin
//    Result := TKBKey.Create(self.FOriginalKey, self.FIndex, self.CanEdit);
//    Result.FOriginalKey := self.FOriginalKey.CopyKey;
//    Result.FModifiedKey := self.FModifiedKey.CopyKey;
//    Result.FIndex := self.FIndex;
//    Result.FIsModified := self.FIsModified;
//    Result.FIsMacro := self.FIsMacro;
//    Result.FCanEdit := self.FCanEdit;
//    Result.FMacro := self.FMacro;
//  end;
//end;

//function TKBKey.Compare(aKey: TKBKey): boolean;
//begin
//  //JM todo...
//  result := false;
//end;

function TKBKey.GetActiveMacro: TKeyList;
begin
  result := FActiveMacro;
end;

procedure TKBKey.SetActiveMacro(macro: TKeyList);
begin
  FActiveMacro := macro;
end;

procedure TKBKey.ResetKey;
begin
  FModifiedKey := nil;
  FIsModified := false;
  FIsMacro := false;
  FMacro1.Clear;
  FMacro2.Clear;
  FMacro3.Clear;
  SetActiveMacro(FMacro1);
end;

procedure TKBKey.Assign(aKbKey: TKBKey; restoreType: TRestoreType);
begin
  if (self <> nil) and (aKbKey <> nil) then
  begin
    if (restoreType in [rtAll, rtKey]) then
    begin
      self.FOriginalKey := aKbKey.OriginalKey.CopyKey;
      self.FModifiedKey := aKbKey.ModifiedKey.CopyKey;
      self.FIndex := aKbKey.Index;
      self.IsModified := aKbKey.IsModified;
      self.FCanEdit := aKbKey.CanEdit;
      self.FCanAssignMacro := aKbKey.CanAssignMacro;
    end;
    if (restoreType in [rtAll, rtMacro]) then
    begin
      self.FIsMacro := aKbKey.IsMacro;
      self.FMacro1.Assign(aKbKey.Macro1);
      self.FMacro2.Assign(aKbKey.Macro2);
      self.FMacro3.Assign(aKbKey.Macro3);
    end;
  end;
end;

function TKBKey.MacroCount: integer;
begin
  result := 0;
  if (Macro1.Count > 0) then
    inc(result);
  if (Macro2.Count > 0) then
    inc(result);
  if (Macro3.Count > 0) then
    inc(result);
end;

constructor TKBKey.Create;
begin
  Init;
end;

procedure TKBKey.Init;
begin
  FOriginalKey := nil;
  FModifiedKey := nil;
  FCanEdit := false;
  FCanAssignMacro := true;
  FMacro1 := TKeyList.Create;
  FMacro2 := TKeyList.Create;
  FMacro3 := TKeyList.Create;
  FMacro1.MultiKey := true;
  FMacro1.MacroIdx := 1;
  FMacro2.MultiKey := true;
  FMacro2.MacroIdx := 2;
  FMacro3.MultiKey := true;
  FMacro3.MacroIdx := 3;
  SetActiveMacro(FMacro1);
end;

constructor TKBKey.Create(oOriginalKey: TKey; iIndex: integer; bCanEdit: boolean; bCanAssignMacro: boolean);
begin
  Init;
  FOriginalKey := oOriginalKey;
  FIndex := iIndex;
  FCanEdit := bCanEdit;
  FCanAssignMacro := bCanAssignMacro;
end;

{ TKBKeyList }

function TKBKeyList.GetItem(index: integer): TKBKey;
begin
  Result := nil;
  try
    Result := inherited Items[Index] as TKBKey
  except
    //do nothing
    //on E : Exception do HandleExcept(E, True, 'Error in TKey.GetItem. Index out of range.');
  end;
end;

procedure TKBKeyList.SetItem(Index: integer; AObject: TKBKey);
begin
  try
    inherited Items[Index] := AObject
  except
    //do nothing
    //on E : Exception do HandleExcept(E, True, 'Error in TKeyList.SetItem. Index out of range.');
  end;
end;

constructor TKBKeyList.Create;
begin
  inherited Create;
  OwnsObjects := True;
end;

function TKBKeyList.Add(AObject: TKBKey): integer;
begin
  Result := inherited Add(AObject);
end;

function TKBKeyList.Remove(AObject: TKBKey): integer;
begin
  Result := inherited Remove(AObject);
end;

procedure TKBKeyList.Assign(aKBKeyList: TKBKeyList);
var
  i: integer;
begin
  if (self <> nil) and (aKBKeyList <> nil) then
  begin
    self.Clear;
    for i := 0 to aKBKeyList.Count - 1 do
    begin
      //Jm is this necessary?
      //self.Add(aKBKeyList.Items[i].CopyKey);
    end;
  end;
end;

//function TKBKeyList.Compare(aKBKeyList: TKBKeyList): boolean;
//var
//  i:integer;
//begin
//  result := false;
//  if aKBKeyList <> nil then
//  begin
//    //Checks if they are both multikeys and same number of key items
//    result := (self.Count = aKBKeyList.Count);
//
//    if result then
//    begin
//      i := 0;
//      //Compares all keys to makre sure they are the same
//      While (i < self.Count) and (result) do
//      begin
//        result := self.Items[i].Compare(aKBKeyList.Items[i]);
//        inc(i);
//      end;
//    end;
//  end;
//end;

{ TKBLayer }

constructor TKBLayer.Create;
begin
  FLayerIndex := 0;
  FLayerType := -1;
  FKBKeyList := TKBKeyList.Create;
end;

end.

