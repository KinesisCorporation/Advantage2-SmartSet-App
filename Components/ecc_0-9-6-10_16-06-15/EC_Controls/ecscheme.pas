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

unit ECScheme;
{$mode objfpc}{$H+}

//{$DEFINE DEBUG}  {don't remove, just comment}

interface

uses
  Classes, SysUtils, Controls, FGL, Graphics, LCLIntf, LCLType, LMessages, Math, 
  {$IFDEF DEBUG} LCLProc, {$ENDIF} LazFileUtils, StdCtrls, Types, Themes,
  Forms, ImgList, Laz2_DOM, Laz2_XMLRead, Laz2_XMLWrite, ECTypes;

type
  {$PACKENUM 2}
  { enumerations TxxxOption must not have more than 32 elements }
  TBlockStyle = (ebsButton, ebsPanel, ebsBlock, ebsRounded, ebsRhombus, ebsEllipse);
  TConnectionOption = (ecoL_Connection,       { connection with L-shape }
                       ecoL_VertThenHor,      { L-shape conn. is vertical then horizontal }
                       ecoArrow,              { no arrow on connection }
                       ecoBold,               { connection line width is doubled }
                       ecoDashed, ecoDotted,  { solid, dash, dot or dash-dot }
                       ecoUser0 = 24, ecoUser1, ecoUser2, ecoUser3,
                       ecoUser4, ecoUser5, ecoUser6, ecoUser7);
  TConnectionOptions = set of TConnectionOption;
  TAddConnectionResult = (eacFailed, eacOccupied, eacAlreadyExists, eacCreated);
  TDeviceOption = (edoAutosized,    { block is autosized to fit caption }
                   edoFixed,        { block cannot be moved }
                   edoNoIcon,       { no icon on the block }
                   edoNoText,       { no text on the block }
                   edoUser0 = 24, edoUser1, edoUser2, edoUser3,
                   edoUser4, edoUser5, edoUser6, edoUser7);
  TDeviceOptions = set of TDeviceOption;
  TSchemeAction = (esaDefault, esaHovering, esaConnecting, esaDragging);
  TSchemeFlag = (esfCursorLock,    { locks cursor for safe internal changing }
                 esfHintLock,      { locks hint for safe internal changing }
                 esfNeedRecalc,    { blocks need recaclulation }
                 esfNeedRedraw,    { block needs redraw }
                 esfSelected,      { one or more blocks are selected }
                 esfSelecting,     { selecting in progress, i.e. left mouse button is down and selection rect. is redrawn on each mouse move }
                 esfWasEnabled     { state of IsEnabled from previous Paint }
                 );
  TSchemeFlags = set of TSchemeFlag;  
  TSchemeOption = (esoIdenticalBlocks,       { all blocks are same => optimized painting }
                   esoDescriptionOnBlock,    { Description appears on block, below Caption }
                   esoDescriptionToHint,     { Description is displayed as a Hint (tooltip) }
                   esoFullDragRepaint,       { scheme is fully repainted during dragging (instead of dashed lines) }
                   esoRectangularConnect,    { connectors are rectangular (instead of direct lines) }
                   esoReadOnly,              { blocks are NOT movable and connnectable by mouse }
                   esoShowGrid,              { grid (dots) is visible }
                   esoSnapToGrid             { block are snapped to grid }
                   );             
  TSchemeOptions = set of TSchemeOption;
  TXMLFlag = (exfCaption,     { Caption }
              exfConfig,      { Connections, MaxIn/Outputs }
              exfDescript,    { Description }
              exfGeometry,    { size, position }
              exfScheme,      { Style, Color, FontColor, Options defined in TECScheme }
              exfVisuals      { Style, Color, FontColor, Options defined in TECDevice(s) }
              );
  TXMLFlags = set of TXMLFlag;  { what will be loaded/saved from/to XML }
  { Events }
  TOnBlockClick = procedure(Sender: TObject; AIndex: Integer) of object;
  TOnDrawBlock = procedure(Sender: TObject; ABitmap: TBitmap) of object;  { for same customdrawn blocks }
  TOnPaintBlock = procedure(Sender: TObject; ARect: TRect) of object;  { for different blocks }
  TOnPaintConnection = procedure(Sender: TObject; ASource, ADestination: TPoint) of object;
  TOnPaintContent = procedure(Sender: TObject; AIndex: Integer; ARect: TRect) of object;

const cXMLFlagsAll = [exfCaption, exfDescript, exfConfig, exfGeometry, exfScheme, exfVisuals];

type  
  { TECConnection } 
  TECConnection = record
    Input: Integer;
    Color: TColor;
    Options: TConnectionOptions;
  end;

  PTECConnection = ^TECConnection;

  TECConnections = array of TECConnection;
  
  TECDevices = class;
  
  { TECDevice }
  TECDevice = class(TPersistent)
  private
    FCaption: string;
    FColor: TColor;
    FData: TObject;
    FDescription: string;
    FFontColor: TColor;
    FHeight: SmallInt;
    FImageIndex: SmallInt;
    FLeft: Integer;
    FMaxInputs: SmallInt;
    FMaxOutputs: SmallInt;  
    FOptions: TDeviceOptions;
    FStyle: TBlockStyle;
    FTag: Integer;
    FTop: Integer;
    FWidth: SmallInt;
    function GetBottom: Integer;
    function GetBoundsRect: TRect;
    function GetHorCenter: Integer;
    function GetRight: Integer;
    function GetVertCenter: Integer;
    procedure SetCaption(AValue: string);
    procedure SetColor(AValue: TColor);
    procedure SetDescription(AValue: string);
    procedure SetFontColor(AValue: TColor);
    procedure SetHeight(AValue: SmallInt);
    procedure SetImageIndex(AValue: SmallInt);
    procedure SetLeft(AValue: Integer);
    procedure SetMaxInputs(AValue: SmallInt);
    procedure SetMaxOutputs(AValue: SmallInt);
    procedure SetOptions(AValue: TDeviceOptions);
    procedure SetOutputs(AValue: TECConnections);
    procedure SetSelected(AValue: Boolean);
    procedure SetStyle(AValue: TBlockStyle);
    procedure SetTop(AValue: Integer);
    procedure SetWidth(AValue: SmallInt);
  protected const
    cDefCaption = 'Item';
    cDefOptions = [];
  protected
    FCaptionRect: TRect;
    FImagePoint: TSmallPoint;
    FOutputs: TECConnections;
    FOwner: TECDevices;
    FSelected: Boolean;
    procedure UpdateArea;
    procedure UpdateDevice;
    procedure UpdateOwner;
    procedure UpdateScheme;
    procedure UpdateSelection;
  public
    constructor Create; virtual;
    procedure ChangeConnectionProperties(AInput: Integer; AColor: TColor; AOptions: TConnectionOptions);
    function ClearOutputs: Integer;
    function DeleteConnIndex(AIndex: Integer): Boolean;
    function DeleteConnInput(AInput: Integer): Boolean;
    property Bottom: Integer read GetBottom;
    property BoundsRect: TRect read GetBoundsRect;
    property Caption: string read FCaption write SetCaption;
    property Color: TColor read FColor write SetColor;
    property Data: TObject read FData write FData;
    property Description: string read FDescription write SetDescription;
    property FontColor: TColor read FFontColor write SetFontColor;
    property Height: SmallInt read FHeight write SetHeight;
    property HorCenter: Integer read GetHorCenter;
    property ImageIndex: SmallInt read FImageIndex write SetImageIndex default -1;
    property Left: Integer read FLeft write SetLeft;
    property MaxInputs: SmallInt read FMaxInputs write SetMaxInputs default 1;
    property MaxOutputs: SmallInt read FMaxOutputs write SetMaxOutputs default 1;
    property Options: TDeviceOptions read FOptions write SetOptions default cDefOptions;
    property Outputs: TECConnections read FOutputs write SetOutputs;
    property Right: Integer read GetRight;
    property Selected: Boolean read FSelected write SetSelected default False;
    property Style: TBlockStyle read FStyle write SetStyle;
    property Tag: Integer read FTag write FTag default 0;
    property Top: Integer read FTop write SetTop;
    property VertCenter: Integer read GetVertCenter;
    property Width: SmallInt read FWidth write SetWidth;
  end;
  
  TCustomECScheme = class;
  
  { TECDevices }
  TECDevices = class(specialize TFPGObjectList<TECDevice>)
  protected
    FScheme: TCustomECScheme;
    procedure CalculateItem(AItem: TECDevice);
    function DoAddConnection(AInput, AOutput: Integer; AColor: TColor;
                AOptions: TConnectionOptions): TAddConnectionResult;
    procedure UpdateScheme;
    procedure UpdateSelection(ASelected: Boolean);
  public const
    cDefConnOpts = [ecoArrow];
  public
    function AddConnection(AInput, AOutput: Integer; AColor: TColor = clDefault;
               AOptions: TConnectionOptions = cDefConnOpts): TAddConnectionResult; virtual;
    function AddConnectionSafe(AInput, AOutput: Integer; AColor: TColor = clDefault;
               AOptions: TConnectionOptions = cDefConnOpts): TAddConnectionResult;
    function AddDevice: Integer; virtual; overload;
    function AddDevice(AItem: TECDevice): Integer; virtual; overload;
    function CalculateInputs(AIndex: Integer): Integer;
    function CalculateOutputs(AIndex: Integer): Integer;
    function CaptionToIndex(ACaption: string):Integer;
    function ClearAllConnections: Integer;
    procedure ClearAll;
    function ClearInputs(AIndex: Integer): Integer; overload;
    function ClearInputs(ACaption: string): Integer; overload;
    function ClearOutputs(AIndex: Integer): Integer; overload;
    function ClearOutputs(ACaption: string): Integer; overload;    
    function DeleteConnection(AInput, AOutput: Integer): Boolean;
    function DeleteConnectionSafe(AInput, AOutput: Integer): Boolean;
    function DeleteDevice(ACaption: string): Boolean; overload;
    function DeleteDevice(AIndex: Integer): Boolean; overload;
    function GetInputs(AIndex: Integer): TIntegerDynArray;
    function GetOutputs(AIndex: Integer): TIntegerDynArray;    
    function HasDirectOutput(AOutput, AInput: Integer): Boolean;
    function HasIndirectOutput(AOutput, AInput: Integer; out APath: TIntegerDynArray): Boolean;
    function IsConnectedDirectly(AIndex, BIndex: Integer): Boolean;
    function IsConnectedIndirectly(AIndex, BIndex: Integer; out APath: TIntegerDynArray): Boolean;
    function ReverseConnection(ADevice, AInput: Integer): Boolean;
  end;

  { TCustomECScheme }                   
  TCustomECScheme = class(TBaseScrollControl)
  private
    FBlockColor: TColor;
    FBlockFontSize: SmallInt;   
    FBlockHeight: SmallInt;
    FBlockStyle: TBlockStyle;
    FBlockWidth: SmallInt;
    FConnectorColor: TColor;
    FConnectorWidth: SmallInt;
    FGrid: SmallInt;
    FImages: TCustomImageList;
    FIndent: SmallInt;
    FLayout: TObjectPos;
    FOnBlockClick: TOnBlockClick;
    FOnDrawBlock: TOnDrawBlock;
    FOnPaintBlock: TOnPaintBlock;
    FOnPaintConnection: TOnPaintConnection;
    FOnPaintContent: TOnPaintContent;
    FOnSchemeChange: TNotifyEvent;
    FOnSelectionChanged: TNotifyEvent;
    FOptions: TSchemeOptions;
    procedure SetBlockColor(AValue: TColor);
    procedure SetBlockFontSize(AValue: SmallInt); 
    procedure SetBlockHeight(AValue: SmallInt);
    procedure SetBlockStyle(AValue: TBlockStyle);
    procedure SetBlockWidth(AValue: SmallInt);
    procedure SetConnectorColor(AValue: TColor);
    procedure SetConnectorWidth(AValue: SmallInt);
    procedure SetGrid(AValue: SmallInt);
    procedure SetHovering(AValue: Integer);
    procedure SetImages(AValue: TCustomImageList);
    procedure SetIndent(AValue: SmallInt);
    procedure SetLayout(AValue: TObjectPos);
    procedure SetOptions(AValue: TSchemeOptions);
  protected const
    cFlags: Cardinal = (DT_CENTER or DT_NOPREFIX or DT_VCENTER {or DT_END_ELLIPSIS});
    cIndentAround: SmallInt = 5;
    cIndentImageText: SmallInt = 4;
    cDefBlockFontSize = 7;
    cDefBlockHeight = 30;
    cDefBlockStyle = ebsButton;
    cDefBlockWidth = 40;
    cDefConnectWidth = 1;
    cDefGrid = 8;
    cDefIndent = 8;                   
    cDefOptions = [esoFullDragRepaint, esoIdenticalBlocks, esoRectangularConnect];
    cMinGrid: SmallInt = 8;
    cCount: DOMString = 'Count';
    cDevice: DOMString = 'Device';
    cID: DOMString = 'ID';
    cIndex: DOMString = 'Index';
    cRoot: DOMString = 'ROOT';
    cScheme: DOMString = 'Scheme';
    cValue: DOMString = 'Value';
    cCaption: DOMString = 'Caption';
    cID_Caption = 101;
    cColor: DOMString = 'Color';
    cID_Color = 106;
    cDescript: DOMString = 'Description';
    cID_Decript = 111;
    cFontColor: DOMString = 'FontColor';
    cID_FontColor = 116;
    cHeight: DOMString = 'Height';
    cID_Height = 121;
    cImageIndex: DOMString = 'ImageIndex';
    cID_ImageIndex = 123;
    cInput: DOMString = 'Input';
    cID_Input = 126;
    cLeft: DOMString = 'Left';
    cID_Left = 131;
    cMaxInputs: DOMString = 'MaxInputs';
    cID_MaxIn = 136;
    cMaxOutputs: DOMString = 'MaxOutputs';
    cID_MaxOut = 141;
    cOptions: DOMString = 'Options';
    cID_Options = 146;
    cOutput: DOMString = 'Output';
    cID_Output = 151;
    cStyle: DOMString = 'Style';
    cID_Style = 156;
    cTag: DOMString = 'Tag';
    cID_Tag = 159;
    cTop: DOMString = 'Top';
    cID_Top = 161;
    cWidth: DOMString = 'Width';
    cID_Width = 166;
    cBlockColor: DOMString = 'BlockColor';
    cID_BlockColor = 401;
    cBlockFontSize: DOMString = 'BlockFontSize';
    cID_BlockFontSize = 406;
    cBlockHeight: DOMString = 'BlockHeight';
    cID_BlockHeight = 411;
    cBlockStyle: DOMString = 'BlockStyle';
    cID_BlockStyle = 416;
    cBlockWidth: DOMString = 'BlockWidtht';
    cID_BlockWidth = 421;
    cConnColor: DOMString = 'ConnectColor';
    cID_ConnColor = 426;
    cConnWidth: DOMString = 'ConnectWidth';
    cID_ConnWidth = 431;
    cGrid: DOMString = 'GridSize';
    cID_Grid = 436;
    cIndent: DOMString = 'Indent';
    cID_Indent = 441;
    cLayout: DOMString = 'Layout';
    cID_Layout = 446;
    cConnects: DOMString = 'Connections';
    cID_Connects = 501;
  protected
    FBlock: TBitmap;
    FConnecting: Integer;
    FCursorBkgnd: TCursor;
  	FDragging: Integer;
    FHintBkgnd: string;
    FHovering: Integer;
    FInitPoint: TPoint;  { initial point for connecting and dragging }
    FFlags: TSchemeFlags;
    FSelStartPoint, FSelEndPoint: TPoint;  { bound points of Selection }
    FTextFlags: Cardinal;
    procedure CalcBlockSizesAndContent(ADevice: TECDevice = nil);
    function CalcFullArea: TPoint;
    procedure ChangeCursor(AAction: TSchemeAction);
    procedure CMBiDiModeChanged(var Message: TLMessage); message CM_BIDIMODECHANGED;
    procedure CMColorChanged(var {%H-}Message: TLMessage); message CM_COLORCHANGED;
    procedure CreateDevices; virtual;
    procedure CreateNode(AXMLDoc: TXMLDocument; AParent: TDOMNode; AIdent, AValue: string; AID: Word);
    procedure DrawBlock(ACanvas: TCanvas; ARect: TRect; ABlockStyle: TBlockStyle; AEnabled: Boolean);
    class function GetControlClassDefaultSize: TSize; override;
    function GetFullCaption(ADevice: TECDevice): string;
    procedure GetGridDelta(out DX, DY: SmallInt);
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure LoadDeviceFromXML(AIndex: Integer; ADeviceNode: TDOMNode;
               AXMLFlags: TXMLFlags = cXMLFlagsAll); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure PaintConnection(ADevice, AOutput: Integer; ADefColor: TColor;
                BFullRepaint: Boolean); virtual;
    procedure PaintContent(AIndex: Integer; ABlockRect: TRect); virtual;
    procedure Paint; override;
    procedure SaveDeviceToXML(AIndex: Integer; AXMLDoc: TXMLDocument;
                ADeviceNode: TDOMNode; AXMLFlags: TXMLFlags = cXMLFlagsAll); virtual;
    procedure SetCursor(Value: TCursor); override;
    procedure SetDefaultScrollParams; override;
    procedure SetHint(const Value: TTranslateString); override;
    procedure SetParent(NewParent: TWinControl); override;
    procedure UpdateDevice(ADevice: TECDevice);
    procedure UpdateRequiredAreaHeight; override; 
    procedure UpdateRequiredAreaWidth; override; 
    procedure UpdateScheme;
    procedure UpdateSelection;
  public
    Devices: TECDevices;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Deselect(AIndex: Integer);
    procedure DeselectAll;
    procedure GetAllSelected(out ACount: Integer; out ASelected: TIntegerDynArray);
    function GetFirstSelected: Integer;
    function GetNewDevicePos: TPoint;
    procedure LoadSchemeFromXML(ASchemeNode: TDOMNode; AXMLFlags: TXMLFlags = cXMLFlagsAll); overload;
    procedure LoadSchemeFromXML(AFileName: string; ASchemeNode: DOMString;
                AXMLFlags: TXMLFlags = cXMLFlagsAll); overload;
    procedure SaveSchemeToXML(AXMLDoc: TXMLDocument; ASchemeNode: TDOMNode;
                AXMLFlags: TXMLFlags = cXMLFlagsAll); overload;
    procedure SaveSchemeToXML(AFileName: string; ASchemeNode: DOMString;
                AXMLFlags: TXMLFlags = cXMLFlagsAll); overload;
    procedure Select(AIndex: Integer);
    procedure SelectAll;  
    procedure ShowDevice(AIndex: Integer); overload;
    procedure ShowDevice(ACaption: string); overload;
    procedure StopConnecting;
    property BlockColor: TColor read FBlockColor write SetBlockColor default clDefault;
    property BlockFontSize: SmallInt read FBlockFontSize write SetBlockFontSize default cDefBlockFontSize;   
    property BlockHeight: SmallInt read FBlockHeight write SetBlockHeight default cDefBlockHeight;
    property BlockStyle: TBlockStyle read FBlockStyle write SetBlockStyle default cDefBlockStyle;
    property BlockWidth: SmallInt read FBlockWidth write SetBlockWidth default cDefBlockWidth;
    property BorderStyle default bsSingle;
    property ConnectorColor: TColor read FConnectorColor write SetConnectorColor default clDefault;
    property ConnectorWidth: SmallInt read FConnectorWidth write SetConnectorWidth default cDefConnectWidth;
    property Grid: SmallInt read FGrid write SetGrid default cDefGrid;
    property Hovering: Integer read FHovering write SetHovering;
    property Images: TCustomImageList read FImages write SetImages;
    property Indent: SmallInt read FIndent write SetIndent default cDefIndent;
    property Layout: TObjectPos read FLayout write SetLayout default eopLeft;
    property Options: TSchemeOptions read FOptions write SetOptions default cDefOptions;
    property OnBlockClick: TOnBlockClick read FOnBlockClick write FOnBlockClick;
    property OnDrawBlock: TOnDrawBlock read FOnDrawBlock write FOnDrawBlock;
    property OnPaintBlock: TOnPaintBlock read FOnPaintBlock write FOnPaintBlock;
    property OnPaintConnection: TOnPaintConnection read FOnPaintConnection write FOnPaintConnection;
    property OnPaintContent: TOnPaintContent read FOnPaintContent write FOnPaintContent;
    property OnSchemeChange: TNotifyEvent read FOnSchemeChange write FOnSchemeChange;
    property OnSelectionChange: TNotifyEvent read FOnSelectionChanged write FOnSelectionChanged;
  end;
  
  { TECScheme }                
  TECScheme= class(TCustomECScheme)
  published 
    property Align;
    property Anchors;
    property AreaHeight;
    property AreaWidth;  
    property BlockColor;
    property BlockFontSize;
    property BlockHeight;
    property BlockStyle;
		property BlockWidth;
    property BorderSpacing;
		property BorderStyle;
    property ClientAreaLeft;
    property ClientAreaTop;
    property Color;
    property ConnectorColor;
    property ConnectorWidth;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FullAreaHeight;
    property FullAreaWidth;
    property Grid;
    property IncrementX default cDefBlockWidth;
    property IncrementY default cDefBlockHeight;  
    property Indent;
    property Layout;
	  property Options;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnBlockClick;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawBlock;
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
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPaintBlock;
    property OnPaintConnection;
    property OnPaintContent;
    property OnResize;
    property OnSchemeChange;
    property OnSelectionChange;
    property OnStartDrag;
    property OnUTF8KeyPress;
  end;
    
implementation

{ TECDevice }

constructor TECDevice.Create;
begin
  FImageIndex:=-1;
  FMaxInputs:=1;
  FMaxOutputs:=1;
end;

function TECDevice.DeleteConnIndex(AIndex: Integer): Boolean;
var i, aLength: Integer;
begin
  aLength:=length(Outputs);
  Result:= (AIndex>=0) and (AIndex<aLength);
  if Result then
    begin
      for i:=AIndex+1 to aLength-1 do
        Outputs[i-1]:=Outputs[i];
      SetLength(FOutputs, aLength-1);
      UpdateScheme;
    end;
end;

function TECDevice.DeleteConnInput(AInput: Integer): Boolean;
var i, aIndex, aLength: Integer;
begin
  Result:=False;
  aLength:=length(Outputs);
  for i:=0 to aLength-1 do
    if Outputs[i].Input=AInput then
      begin
        Result:=True;
        aIndex:=i;
        break;
      end;
  if Result then
    begin
      for i:=aIndex+1 to aLength-1 do
        Outputs[i-1]:=Outputs[i];
      SetLength(FOutputs, aLength-1);
      UpdateScheme;
    end;
end;

procedure TECDevice.ChangeConnectionProperties(AInput: Integer; AColor: TColor;
  AOptions: TConnectionOptions);
var i: Integer;
begin
  for i:=0 to length(Outputs)-1 do
    if Outputs[i].Input=AInput then
      begin
        Outputs[i].Color:=AColor;
        Outputs[i].Options:=AOptions;
        UpdateOwner;
      end;
end;

function TECDevice.ClearOutputs: Integer;
begin
  Result:=length(Outputs);
  if Result>0 then;
    begin
      SetLength(FOutputs, 0);
      UpdateScheme;
    end;
end;

procedure TECDevice.UpdateArea;
begin
  if assigned(FOwner) then
    with FOwner.FScheme do
      begin
        if AreaWidth<0 then UpdateRequiredAreaWidth;
        if AreaHeight<0 then UpdateRequiredAreaHeight;
        InvalidateNonUpdated;
      end;
end;

procedure TECDevice.UpdateDevice;
begin
  if assigned(FOwner) then
    with FOwner do FScheme.UpdateDevice(self);
end;
     
procedure TECDevice.UpdateOwner;
begin
  if assigned(FOwner) then
    with FOwner do FScheme.InvalidateNonUpdated;
end;

procedure TECDevice.UpdateScheme;
begin
  with FOwner do FScheme.UpdateScheme;
end;

procedure TECDevice.UpdateSelection;
begin
  with FOwner do FScheme.UpdateSelection;
end;

{ TECDevice.Setters }

function TECDevice.GetBottom: Integer;
begin
  Result:=Top+Height;
end;

function TECDevice.GetBoundsRect: TRect;
begin
  Result.Left:=Left;
  Result.Top:=Top;
  Result.Right:=Result.Left+Width;
  Result.Bottom:=Result.Top+Height;
end;

function TECDevice.GetHorCenter: Integer;
begin
  Result:=Left+Width div 2;
end;

function TECDevice.GetRight: Integer;
begin
  Result:=Left+Width;
end;

function TECDevice.GetVertCenter: Integer;
begin
  Result:=Top+Bottom div 2;
end;

procedure TECDevice.SetCaption(AValue: string);
begin
  if FCaption=AValue then exit;
  FCaption:=AValue;
  UpdateDevice;
end;

procedure TECDevice.SetColor(AValue: TColor);
begin
  if FColor=AValue then exit;
  FColor:=AValue;
  UpdateOwner;
end;

procedure TECDevice.SetDescription(AValue: string);
begin
  if FDescription=AValue then exit;
  FDescription:=AValue;
  if assigned(FOwner) then
    if esoDescriptionOnBlock in FOwner.FScheme.Options then
      UpdateDevice;
end;

procedure TECDevice.SetFontColor(AValue: TColor);
begin
  if FFontColor=AValue then exit;
  FFontColor:=AValue;
  UpdateOwner;
end;

procedure TECDevice.SetHeight(AValue: SmallInt);
begin
  if FHeight=AValue then exit;
  FHeight:=AValue;
  UpdateArea;
end;

procedure TECDevice.SetImageIndex(AValue: SmallInt);
begin
  if FImageIndex=AValue then exit;
  FImageIndex:=AValue;
  UpdateDevice;
end;

procedure TECDevice.SetLeft(AValue: Integer);
begin
  if FLeft=AValue then exit;
  if assigned(FOwner) then
    with FOwner.FScheme do
      begin
        if Indent>AValue 
          then AValue:=Indent
          else if (AreaWidth>=0) and (AValue>(FullAreaWidth-Indent))
                 then AValue:=FullAreaWidth-Indent;
      end;
  FLeft:=AValue;
  UpdateArea;
end;

procedure TECDevice.SetMaxInputs(AValue: SmallInt);
begin
  if FMaxInputs=AValue then exit;
  FMaxInputs:=AValue;
end;     

procedure TECDevice.SetMaxOutputs(AValue: SmallInt);
begin
  if FMaxOutputs=AValue then exit;
  FMaxOutputs:=AValue;
end;

procedure TECDevice.SetOptions(AValue: TDeviceOptions);
var bAutoSize, bUpdate: Boolean;
const cUpdateOpts = [edoNoIcon, edoNoText];
begin
  if FOptions=AValue then exit;
  bAutoSize:= ([edoAutosized]*AValue<>[edoAutosized]*FOptions);
  bUpdate:= (cUpdateOpts*AValue<>cUpdateOpts*FOptions);;
  FOptions:=AValue;
  if bUpdate or (bAutoSize and (edoAutosized in AValue)) then UpdateDevice;
end;

procedure TECDevice.SetOutputs(AValue: TECConnections);
begin
  if FOutputs=AValue then exit;
  FOutputs:=AValue;
end;

procedure TECDevice.SetSelected(AValue: Boolean);
begin
  if FSelected=AValue then exit;
  FSelected:=AValue;
  UpdateSelection;
end;

procedure TECDevice.SetStyle(AValue: TBlockStyle);
begin
  if FStyle=AValue then exit;
  FStyle:=AValue;
  UpdateDevice;
end;

procedure TECDevice.SetTop(AValue: Integer);
begin
  if FTop=AValue then exit;
  if assigned(FOwner) then
    with FOwner.FScheme do
      begin
        if Indent>AValue 
          then AValue:=Indent
          else if (AreaHeight>=0) and (AValue>(FullAreaHeight-Indent))
                 then AValue:=FullAreaHeight-Indent;
      end;
  FTop:=AValue;
  UpdateArea;
end;

procedure TECDevice.SetWidth(AValue: SmallInt);
begin
  if FWidth=AValue then exit;
  FWidth:=AValue;
  UpdateArea;
end;

{ TECDevices }

function TECDevices.AddConnection(AInput, AOutput: Integer; AColor: TColor;
  AOptions: TConnectionOptions): TAddConnectionResult;
begin
  if IsConnectedDirectly(AInput, AOutput)
    then Result:=eacAlreadyExists
    else
    begin
      Result:=DoAddConnection(AInput, AOutput, AColor, AOptions);
      FScheme.UpdateScheme;
    end;
end;

function TECDevices.AddConnectionSafe(AInput, AOutput: Integer; AColor: TColor;
  AOptions: TConnectionOptions): TAddConnectionResult;
begin
  Result:=eacFailed;
  if (AInput>=0) and (AOutput>=0) and (AInput<Count) and (AOutput<Count) and (AInput<>AOutput) 
    then Result:=AddConnection(AInput, AOutput, AColor, AOptions);
end;

function TECDevices.AddDevice: Integer;
var aItem: TECDevice;
    aPoint: TPoint;
begin
  aItem:=TECDevice.Create;
  aItem.Caption:=aItem.cDefCaption+inttostr(Count);
  aItem.FOwner:=self;
  aItem.FHeight:=FScheme.BlockHeight;
  aItem.FStyle:=FScheme.BlockStyle;
  aItem.FWidth:=FScheme.BlockWidth;
  aPoint:=FScheme.GetNewDevicePos;
  aItem.FLeft:=aPoint.X;
  aItem.FTop:=aPoint.Y;
  Result:=Add(AItem);
  if Result>=0 then
    begin
      CalculateItem(aItem);
      UpdateScheme;
    end;
end;

function TECDevices.AddDevice(AItem: TECDevice): Integer;
begin
  AItem.FOwner:=self;
  if AItem.FLeft<FScheme.Indent then AItem.FLeft:=FScheme.Indent;
  if AItem.FTop<FScheme.Indent then AItem.FTop:=FScheme.Indent;
  if (esoIdenticalBlocks in FScheme.Options) or (AItem.Height=0)
    then AItem.Height:=FScheme.BlockHeight;
  if (esoIdenticalBlocks in FScheme.Options) or (AItem.Width=0)
    then AItem.Width:=FScheme.BlockWidth;
  Result:=Add(AItem);
  if Result>=0 then
    begin
      CalculateItem(AItem);
      UpdateScheme;
    end;
end;

function TECDevices.CalculateInputs(AIndex: Integer): Integer;
var i, j: Integer;
begin
  Result:=0;
  for i:=0 to AIndex-1 do
    for j:=0 to length(Items[i].Outputs)-1 do
      if Items[i].Outputs[j].Input=AIndex then inc(Result);
  for i:=AIndex+1 to Count-1 do
    for j:=0 to length(Items[i].Outputs)-1 do
      if Items[i].Outputs[j].Input=AIndex then inc(Result);        
end;

procedure TECDevices.CalculateItem(AItem: TECDevice);
begin
  FScheme.CalcBlockSizesAndContent(AItem);
end;

function TECDevices.CalculateOutputs(AIndex: Integer): Integer;
begin
  Result:=length(Items[AIndex].Outputs);
end; 

function TECDevices.CaptionToIndex(ACaption: string): Integer;
var i: Integer;
begin
  Result:=-1;
  for i:=0 to Count-1 do
    if Items[i].Caption=ACaption then
      begin
        Result:=i;
        break;
      end;
end; 

function TECDevices.ClearAllConnections: Integer;
var i: Integer;
begin
  Result:=0;
  for i:=0 to Count-1 do
    begin
      inc(Result, length(Items[i].Outputs));
      SetLength(Items[i].FOutputs, 0);
    end;
  if Result>0 then FScheme.UpdateScheme;
end;

procedure TECDevices.ClearAll;
begin
  if Count>0 then
    begin
      Clear;
      FScheme.UpdateScheme;
    end;
end;

function TECDevices.ClearInputs(AIndex: Integer): Integer;
var i, j, k, aLength: Integer;
begin
  Result:=0;
  for i:=0 to AIndex-1 do
    begin
      aLength:=length(Items[i].Outputs);
      for j:=0 to aLength-1 do
        if Items[i].Outputs[j].Input=AIndex then
          begin
            for k:=j+1 to aLength-1 do
              Items[i].Outputs[k-1]:=Items[i].Outputs[k];
            SetLength(Items[i].FOutputs, aLength-1);
            inc(Result);
            break;  { can't have doubled connections }
          end;      
    end;
  for i:=AIndex+1 to Count-1 do
    begin
      aLength:=length(Items[i].Outputs);
      for j:=0 to aLength-1 do
        if Items[i].Outputs[j].Input=AIndex then
          begin
            for k:=j+1 to aLength-1 do
              Items[i].Outputs[k-1]:=Items[i].Outputs[k];
            SetLength(Items[i].FOutputs, aLength-1);
            inc(Result);
            break;  { can't have doubled connections }
          end;     
    end;
 if Result>0 then FScheme.UpdateScheme;
end;

function TECDevices.ClearInputs(ACaption: string): Integer;
begin
  Result:=ClearInputs(CaptionToIndex(ACaption));
end;

function TECDevices.ClearOutputs(AIndex: Integer): Integer;
begin
  Result:=length(Items[AIndex].Outputs);
  if Result>0 then
    begin
      SetLength(Items[AIndex].FOutputs, 0);
      FScheme.UpdateScheme;
    end;
end;

function TECDevices.ClearOutputs(ACaption: string): Integer;
begin
  Result:=ClearOutputs(CaptionToIndex(ACaption));
end;

function TECDevices.DeleteConnection(AInput, AOutput: Integer): Boolean;
var i, j, aLength: Integer;
begin
  Result:=False;
  aLength:=length(Items[AOutput].Outputs);
  for i:=0 to aLength-1 do
    if Items[AOutput].Outputs[i].Input=AInput then
      begin
        for j:=i+1 to aLength-1 do
          Items[AOutput].Outputs[j-1]:=Items[AOutput].Outputs[j];
        SetLength(Items[AOutput].FOutputs, aLength-1);
        Result:=True;
        break;  { can't have doubled connections }
      end;
  if Result then FScheme.UpdateScheme;
end;

function TECDevices.DeleteConnectionSafe(AInput, AOutput: Integer): Boolean;
begin
  Result:=False;
  if (AInput>=0) and (AOutput>=0) and (AInput<Count) and (AOutput<Count) and (AInput<>AOutput) 
    then Result:=DeleteConnection(AInput, AOutput); 
end;

function TECDevices.DeleteDevice(AIndex: Integer): Boolean;
begin
  if (AIndex<0) or (AIndex>=Count)
    then Result:=False
    else
    begin
      Delete(AIndex);
      FScheme.UpdateScheme;
      Result:=True;
    end;
end;

function TECDevices.DeleteDevice(ACaption: string): Boolean;
begin
  Result:=DeleteDevice(CaptionToIndex(ACaption));
end;

function TECDevices.DoAddConnection(AInput, AOutput: Integer; AColor: TColor;
  AOptions: TConnectionOptions): TAddConnectionResult;
var aInputs, aOutputs: Integer;
begin  { no checking for existing reversed conn., no scheme update }
  aInputs:=CalculateInputs(AInput);
  aOutputs:=length(Items[AOutput].Outputs);
  if (aInputs>=Items[AInput].MaxInputs) or (aOutputs>=Items[AOutput].MaxOutputs) then
    begin
      Result:=eacOccupied;
      exit
    end;
  SetLength(Items[AOutput].FOutputs, aOutputs+1);
  Items[AOutput].Outputs[aOutputs].Input:=AInput;
  Items[AOutput].Outputs[aOutputs].Color:=AColor;
  Items[AOutput].Outputs[aOutputs].Options:=AOptions;
  Result:=eacCreated;
end;

function TECDevices.GetInputs(AIndex: Integer): TIntegerDynArray;
var i, j: Integer;
begin
  SetLength(Result, 0);
  for i:=0 to AIndex-1 do                             
    for j:=0 to length(Items[i].Outputs)-1 do
      if Items[i].Outputs[j].Input=AIndex then
        begin
          SetLength(Result, length(Result)+1);
          Result[length(Result)-1]:=i;
          break;  { can't have doubled connections }
        end;
end;

function TECDevices.GetOutputs(AIndex: Integer): TIntegerDynArray;
var i: Integer;
begin
  SetLength(Result, length(Items[AIndex].Outputs));
  for i:=0 to length(Items[AIndex].Outputs)-1 do
    Result[i]:=Items[AIndex].Outputs[i].Input;
end; 

function TECDevices.HasDirectOutput(AOutput, AInput: Integer): Boolean;
var i: Integer;
begin
  Result:=False;
  for i:=0 to length(Items[AOutput].Outputs)-1 do
    if Items[AOutput].Outputs[i].Input=AInput then
      begin
        Result:=True;
        exit;
      end;          
end;

function TECDevices.HasIndirectOutput(AOutput, AInput: Integer; out APath: TIntegerDynArray): Boolean;
const cArrIncSize: SmallInt = 8;
var aLevel: Integer;

  procedure nHasDirectOutput(ADevice: Integer);
  var i: Integer;
  begin
    { detect loop }
    for i:=0 to aLevel-1 do
      if APath[i]=ADevice then exit;  { Exit! }
    { increase lenght of array }
    if length(APath)=(aLevel+1) then SetLength(APath, length(APath)+cArrIncSize);
    if aLevel>0 then APath[aLevel-1]:=ADevice;       
    inc(aLevel);  
    { loop and test all outputs of current device }
    for i:=0 to length(Items[ADevice].Outputs)-1 do
      begin
        if Items[ADevice].Outputs[i].Input=AInput then 
          begin
            Result:=True;
            SetLength(APath, aLevel-1);
          end;
        if not Result 
          then nHasDirectOutput(Items[ADevice].Outputs[i].Input)
          else exit;  { Exit! }
      end;
    dec(aLevel);
  end;
  
begin
  Result:=False;
  aLevel:=0;
  SetLength(APath, cArrIncSize);
  nHasDirectOutput(AOutput);
  if not Result then SetLength(APath, 0);
end; 

function TECDevices.IsConnectedDirectly(AIndex, BIndex: Integer): Boolean;
begin
  Result:= HasDirectOutput(AIndex, BIndex) or HasDirectOutput(BIndex, AIndex);
end;    

function TECDevices.IsConnectedIndirectly(AIndex, BIndex: Integer; out APath: TIntegerDynArray): Boolean;
begin
  Result:= HasIndirectOutput(AIndex, BIndex, APath) or
           HasIndirectOutput(BIndex, AIndex, APath);
end;

function TECDevices.ReverseConnection(ADevice, AInput: Integer): Boolean;
var i, aOutIndex: Integer;
begin
  Result:=False;
  inc(FScheme.UpdateCount);
  aOutIndex:=-1;
  for i:=0 to length(Items[ADevice].Outputs)-1 do
    if Items[ADevice].Outputs[i].Input=AInput then
      begin
        aOutIndex:=i;
        break;
      end;
  if aOutIndex>=0 then
    begin
      Result:= (DoAddConnection(ADevice, AInput, Items[ADevice].Outputs[aOutIndex].Color,
                                Items[ADevice].Outputs[aOutIndex].Options)=eacCreated);
      if Result then DeleteConnection(AInput, ADevice);
    end;
  dec(FScheme.UpdateCount);
  if Result then FScheme.UpdateScheme;
end;

procedure TECDevices.UpdateScheme;
begin
  FScheme.UpdateScheme;
end;

procedure TECDevices.UpdateSelection(ASelected: Boolean);
var i: Integer;
    b: Boolean;
begin
  b:=ASelected;
  if not b then
    for i:=0 to Count-1 do
      begin
        b:=Items[i].Selected;
        if b then break;
      end;
  if b 
    then include(FScheme.FFlags, esfSelected)
    else exclude(FScheme.FFlags, esfSelected);
  FScheme.UpdateSelection;
end;    

{ TCustomECScheme }

constructor TCustomECScheme.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, cx, cy);
  CreateDevices;
  { area }
  BorderStyle:=bsSingle;
  ControlStyle:=ControlStyle+[csCaptureMouse];
  Devices.FScheme:=self;
  FGrid:=cDefGrid;
  { block }
  FBlock:=TBitmap.Create;
  with FBlock do
    begin
      Canvas.Brush.Style:=bsSolid;
      Transparent:=True;
    end;
  FBlockColor:=clDefault;
  FBlockFontSize:=cDefBlockFontSize;
  FBlockHeight:=cDefBlockHeight;
  FBlockWidth:=cDefBlockWidth;
  { connectors }
  FConnectorColor:=clDefault;
  FConnectorWidth:=cDefConnectWidth;
  FCursorBkgnd:=Cursor;
  FIndent:=cDefIndent;
  FLayout:=eopLeft;
  FOptions:=cDefOptions;
  FConnecting:=-1;
  FDragging:=-1;
  FHovering:=-1;
  if not IsRightToLeft
    then FTextFlags:=cFlags
    else FTextFlags:=cFlags or DT_RTLREADING;
  include(FFlags, esfNeedRedraw);
end;

destructor TCustomECScheme.Destroy;
begin
  FreeAndNil(FBlock);  
  if assigned(Devices) then FreeAndNil(Devices);
  inherited Destroy;
end;

procedure TCustomECScheme.CreateDevices;
begin
  Devices:=TECDevices.Create(True);
end;

procedure TCustomECScheme.CreateNode(AXMLDoc: TXMLDocument; AParent: TDOMNode; AIdent,
  AValue: string; AID: Word);
var aNode: TDOMNode;
begin
  aNode:=AXMLDoc.CreateElement(AIdent);
  AParent.AppendChild(aNode);
  TDOMElement(aNode).SetAttribute(cValue, AValue);
  TDOMElement(aNode).SetAttribute(cID, inttostr(AID));
end;

procedure TCustomECScheme.CalcBlockSizesAndContent(ADevice: TECDevice = nil);
var bEnabled, bHasImage: Boolean;
    aImageCount: Integer;
    aExtent, aTextExtent: TSize;
    aLayout: TObjectPos;
    aRect: TRect;

  procedure Calculate;
  begin
    aTextExtent:=Size(0, 0);
    if ADevice.Caption<>'' then
      begin
        aRect:=ThemeServices.GetTextExtent(Canvas.Handle,
                 ThemeServices.GetElementDetails(caThemedContent[caItemState[bEnabled]]),
                 GetFullCaption(ADevice), FTextFlags, nil);
        aTextExtent:=Size(aRect.Right, aRect.Bottom);
      end;
    bHasImage:= (assigned(Images) and (ADevice.ImageIndex>=0) and (ADevice.ImageIndex<aImageCount));
    if bHasImage then
      case aLayout of
        eopTop, eopBottom:
          begin
            aExtent.cx:=Math.max(aTextExtent.cx, Images.Width);
            aExtent.cy:=aTextExtent.cy+Images.Height;
            if aTextExtent.cy>0 then inc(aExtent.cy, cIndentImageText);
          end;
        eopRight, eopLeft:
          begin
            aExtent.cy:=Math.max(aTextExtent.cy, Images.Height);
            aExtent.cx:=aTextExtent.cx+Images.Width;
            if aTextExtent.cx>0 then inc(aExtent.cx, cIndentImageText);
          end;
      end else aExtent:=aTextExtent;
    if not (esoIdenticalBlocks in Options) and (edoAutosized in ADevice.Options) then
      begin  { autosized blocks }
        ADevice.FWidth:=aExtent.cx+2*cIndentAround;
        ADevice.FHeight:=aExtent.cy+2*cIndentAround;
        case ADevice.Style of
          ebsRounded:
            begin
              inc(ADevice.FWidth, 2);
              inc(ADevice.FHeight, 2);
            end;
          ebsRhombus:
            begin
              inc(ADevice.FWidth, 5*aExtent.cx div 6);
              inc(ADevice.FHeight, 5*aExtent.cy div 6);
            end;
          ebsEllipse:
            begin
              inc(ADevice.FWidth, aExtent.cx div 3);
              inc(ADevice.FHeight, aExtent.cy div 3);
            end;
        end;
      end;
    { calc points of content }
    case aLayout of
      eopTop, eopBottom:
        begin
          ADevice.FCaptionRect.Left:=(ADevice.Width-aTextExtent.cx) div 2;
          if bHasImage then
            ADevice.FImagePoint.X:=(ADevice.Width-Images.Width) div 2;
        end;
      eopRight, eopLeft:
        begin
          ADevice.FCaptionRect.Top:=(ADevice.Height-aTextExtent.cy) div 2;
          if bHasImage then
            ADevice.FImagePoint.Y:=(ADevice.Height-Images.Height) div 2;
        end;
    end;
    if not bHasImage then
      begin  { no image }
        if aTextExtent.cx>0 then
          case aLayout of
            eopTop, eopBottom:
              ADevice.FCaptionRect.Top:=(ADevice.Height-aTextExtent.cy) div 2;
            eopRight, eopLeft:
              ADevice.FCaptionRect.Left:=(ADevice.Width-aTextExtent.cx) div 2;
          end;
      end else
      begin  { has image }
        if aTextExtent.cx>0 then
          begin  { has Caption }
            case aLayout of
              eopTop:
                begin
                  ADevice.FImagePoint.Y:=(ADevice.Height-aExtent.cy) div 2;
                  ADevice.FCaptionRect.Top:=ADevice.FImagePoint.Y+Images.Height+cIndentImageText;
                end;
              eopRight:
                begin
                  ADevice.FCaptionRect.Left:=(ADevice.Width-aExtent.cx) div 2;
                  ADevice.FImagePoint.X:=ADevice.FCaptionRect.Left+aTextExtent.cx+cIndentAround;
                end;
              eopBottom:
                begin
                  ADevice.FCaptionRect.Top:=(ADevice.Height-aExtent.cy) div 2;
                  ADevice.FImagePoint.Y:=ADevice.FCaptionRect.Top+aTextExtent.cy+cIndentAround;
                end;
              eopLeft:
                begin
                  ADevice.FImagePoint.X:=(ADevice.Width-aExtent.cx) div 2;
                  ADevice.FCaptionRect.Left:=ADevice.FImagePoint.X+Images.Width+cIndentImageText;
                end;
            end;
          end else
          begin  { only Image }
            case aLayout of
              eopTop, eopBottom:
                ADevice.FImagePoint.Y:=(ADevice.Height-Images.Height) div 2;
              eopRight, eopLeft:
                ADevice.FImagePoint.X:=(ADevice.Width-Images.Width) div 2;
            end;
          end;
      end;
    ADevice.FCaptionRect.Right:=ADevice.FCaptionRect.Left+aTextExtent.cx;
    ADevice.FCaptionRect.Bottom:=ADevice.FCaptionRect.Top+aTextExtent.cy;
  end;

begin
  bEnabled:=IsEnabled;
  if assigned(Images) then aImageCount:=Images.Count;
  aLayout:=Layout;
  if IsRightToLeft then
    begin
      case aLayout of
        eopRight: aLayout:=eopLeft;
        eopLeft: aLayout:=eopRight;
      end;
    end;
  Canvas.Font.Size:=BlockFontSize;
  if HandleAllocated then
    begin
      if ADevice=nil
        then for ADevice in Devices do
               Calculate
        else Calculate;
    end;
end;

function TCustomECScheme.CalcFullArea: TPoint;
var i, aHelp: Integer;
begin
  Result:=Point(0, 0);
  for i:=0 to Devices.Count-1 do
    begin
      aHelp:=Devices[i].Right;
      if aHelp>Result.X then Result.X:=aHelp;
      aHelp:=Devices[i].Bottom;
      if aHelp>Result.Y then Result.Y:=aHelp;
    end;
  inc(Result.X, Indent);
  inc(Result.Y, Indent);
  if Result.X<ClientWidth then Result.X:=ClientWidth;
  if Result.Y<ClientHeight then Result.Y:=ClientHeight;
end;     

procedure TCustomECScheme.ChangeCursor(AAction: TSchemeAction);
begin           
  include(FFlags, esfCursorLock);
  case AAction of 
    esaDefault: Cursor:=FCursorBkgnd;
    esaHovering: Cursor:=crHandPoint;
    esaConnecting: Cursor:=crCross;
    esaDragging: Cursor:=crDrag;
  end;                         
  exclude(FFlags, esfCursorLock);                    
end;

procedure TCustomECScheme.CMBiDiModeChanged(var Message: TLMessage);
begin
  inherited CMBiDiModeChanged(Message);
  if not IsRightToLeft
    then FTextFlags:=cFlags
    else FTextFlags:=cFlags or DT_RTLREADING;
end;

procedure TCustomECScheme.CMColorChanged(var Message: TLMessage);
begin
  if esoIdenticalBlocks in Options then include(FFlags, esfNeedRedraw);
  InvalidateNonUpdated;
end;     

procedure TCustomECScheme.Deselect(AIndex: Integer);
var bSelected: Boolean;
begin
  bSelected:=Devices[AIndex].Selected;
  Devices[AIndex].Selected:=False;
  if bSelected then InvalidateNonUpdated;
end; 

procedure TCustomECScheme.DeselectAll;
var i: Integer;
    bSelected: Boolean;
begin
  bSelected:=False;
  for i:=0 to Devices.Count-1 do
    begin
      bSelected:=bSelected or Devices[i].Selected;
      Devices[i].Selected:=False;
    end;
  exclude(FFlags, esfSelected);
  if bSelected then InvalidateNonUpdated;
end;           

procedure TCustomECScheme.DrawBlock(ACanvas: TCanvas; ARect: TRect; ABlockStyle: TBlockStyle;
  AEnabled: Boolean);
var h: Integer;
begin
  case ABlockStyle of
    ebsButton: ACanvas.DrawButtonBackGround(ARect, AEnabled);
    ebsPanel: ACanvas.DrawPanelBackGround(ARect, bvNone, bvRaised, 1, clBtnFace);
    otherwise
      ACanvas.Pen.Width:=2;
      ACanvas.Pen.Style:=psSolid;
      ACanvas.Brush.Color:=clBtnFace;
      inc(ARect.Left);
      inc(ARect.Top);
      case ABlockStyle of
        ebsBlock:
          begin
            ACanvas.FillRect(ARect);
            ACanvas.Frame(ARect);
          end;
        ebsRounded:
          begin
            ACanvas.AntialiasingMode:=amOn;
            ACanvas.RoundRect(ARect, 7, 7);
          end;
        ebsRhombus:
          begin
            ACanvas.AntialiasingMode:=amOn;
            dec(ARect.Right);
            dec(ARect.Bottom);
            h:=(ARect.Top+ARect.Bottom) div 2;
            ACanvas.Polygon([Point(ARect.Left, h),
                             Point((ARect.Left+ARect.Right) div 2, ARect.Top),
                             Point(ARect.Right, h),
                             Point((ARect.Left+ARect.Right) div 2, ARect.Bottom),
                             Point(ARect.Left, h)]);
          end;
        ebsEllipse:
          begin
            ACanvas.AntialiasingMode:=amOn;
            ACanvas.Ellipse(ARect);
          end;
      end;
      ACanvas.AntialiasingMode:=amOff;
  end;
end;

procedure TCustomECScheme.GetAllSelected(out ACount: Integer; out ASelected: TIntegerDynArray);
var i, j: Integer;
begin
  ACount:=0;
  for i:=0 to Devices.Count-1 do
    if Devices[i].Selected then inc(ACount);
  SetLength(ASelected, ACount);
  if ACount>0 then
    begin
      j:=0;
      for i:=0 to Devices.Count-1 do
        if Devices[i].Selected then
          begin
            ASelected[j]:=i;
            inc(j);
          end;
    end;
end;

class function TCustomECScheme.GetControlClassDefaultSize: TSize;
begin
  Result.cx:=200;
  Result.cy:=200;
end;

function TCustomECScheme.GetFirstSelected: Integer;
var i: Integer;
begin
  Result:=-1;
  for i:=0 to Devices.Count-1 do
    if Devices[i].Selected then
      begin
        Result:=i;
        break;
      end;
end;

function TCustomECScheme.GetFullCaption(ADevice: TECDevice): string;
begin
  Result:=ADevice.Caption;
  if (esoDescriptionOnBlock in Options) and (ADevice.Description<>'')
    then Result:=Result+LineEnding+ADevice.Description;
end;

procedure TCustomECScheme.GetGridDelta(out DX, DY: SmallInt);
var aGrid: SmallInt;
begin
  aGrid:=Grid;
  if aGrid>0 then
    begin
      DX:=ClientAreaLeft mod aGrid;
      DY:=ClientAreaTop mod aGrid;
    end else
    begin
      DX:=0;
      DY:=0;
    end;
end;

function TCustomECScheme.GetNewDevicePos: TPoint;
var i: Integer;
begin
  Result.X:=0;
  Result.Y:=Indent;
  for i:=0 to Devices.Count-1 do
    Result.X:=Math.max(Result.X, Devices[i].Right);
  inc(Result.X, Indent);
end;

procedure TCustomECScheme.KeyDown(var Key: Word; Shift: TShiftState);
var bUsed: Boolean;
begin
  inherited KeyDown(Key, Shift);
  bUsed:=True;
  case Key of
    VK_PRIOR: 
      begin
        if not (ssCtrl in Shift) 
          then ClientAreaTop:=ClientAreaTop-ClientHeight
          else ClientAreaLeft:=ClientAreaLeft-ClientWidth;
      end;
    VK_NEXT: 
      begin
        if not (ssCtrl in Shift) 
          then ClientAreaTop:=ClientAreaTop+ClientHeight
          else ClientAreaLeft:=ClientAreaLeft+ClientWidth;
      end;
    VK_END: 
      begin
        if not (ssCtrl in Shift)
          then ClientAreaTop:=FullAreaHeight-ClientHeight
          else ClientAreaLeft:=FullAreaWidth-ClientWidth;        
      end;
    VK_HOME: 
      begin
        if not (ssCtrl in Shift)
          then ClientAreaTop:=0
          else ClientAreaLeft:=0;
      end;
    VK_LEFT: ClientAreaLeft:=ClientAreaLeft-IncrementX;
    VK_UP: ClientAreaTop:=ClientAreaTop-IncrementY;
    VK_RIGHT: ClientAreaLeft:=ClientAreaLeft+IncrementX;
    VK_DOWN: ClientAreaTop:=ClientAreaTop+IncrementY;
    otherwise bUsed:=False;
  end;
  if bUsed then Key:=0;			 
end; 

procedure TCustomECScheme.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  if (((ssModifier=ssCtrl) and (Key=VK_CONTROL)) or
    ((ssModifier=ssMeta) and (Key=VK_LWIN))) and (FConnecting>-1) then
    StopConnecting;
end; 

procedure TCustomECScheme.LoadDeviceFromXML(AIndex: Integer; ADeviceNode: TDOMNode;
  AXMLFlags: TXMLFlags = cXMLFlagsAll);
var aNode, aOutputNode, aHelpNode: TDOMNode;
    id, aCount, aLength, aOutput: Integer;
    aStr: string;
begin
  aNode:=ADeviceNode.FirstChild;
  while assigned(aNode) do
    begin
      id:=strtoint(TDOMElement(aNode).GetAttribute(cID));
      aStr:=TDOMElement(aNode).GetAttribute(cValue);
      case id of
        cID_Caption: if exfCaption in AXMLFlags then Devices[AIndex].Caption:=aStr;
        cID_Color: if exfVisuals in AXMLFlags then Devices[AIndex].Color:=StringToColor(aStr);
        cID_Decript: if exfDescript in AXMLFlags then Devices[AIndex].Description:=aStr;
        cID_FontColor: if exfVisuals in AXMLFlags then Devices[AIndex].FontColor:=StringToColor(aStr);
        cID_Height: if exfGeometry in AXMLFlags then Devices[AIndex].Height:=strtoint(aStr);
        cID_ImageIndex: if exfVisuals in AXMLFlags then Devices[AIndex].ImageIndex:=strtoint(aStr);
        cID_Left: if exfGeometry in AXMLFlags then Devices[AIndex].Left:=strtoint(aStr);
        cID_MaxIn: if exfConfig in AXMLFlags then Devices[AIndex].MaxInputs:=strtoint(aStr);
        cID_MaxOut: if exfConfig in AXMLFlags then Devices[AIndex].MaxOutputs:=strtoint(aStr);
        cID_Options: if exfVisuals in AXMLFlags then Devices[AIndex].Options:=TDeviceOptions(LongWord(strtoint('%'+aStr)));
        cID_Style: if exfVisuals in AXMLFlags then Devices[AIndex].Style:=TBlockStyle(strtoint(aStr));
        cID_Tag: if exfConfig in AXMLFlags then Devices[AIndex].Tag:=strtoint(aStr);
        cID_Top: if exfGeometry in AXMLFlags then Devices[AIndex].Top:=strtoint(aStr);
        cID_Width: if exfGeometry in AXMLFlags then Devices[AIndex].Width:=strtoint(aStr);
        cID_Connects:  { keep this at the last pos }
          if [exfConfig, exfVisuals]*AXMLFlags<>[] then
            begin
              aCount:=strtoint(TDOMElement(aNode).GetAttribute(cCount));
              SetLength(Devices[AIndex].FOutputs, aCount);
              aCount:=length(cOutput);
              aOutputNode:=aNode.FirstChild;
              while assigned(aOutputNode) do
                begin
                  aLength:=length(TDOMElement(aOutputNode).NodeName);
                  aOutput:=strtoint(RightStr(TDOMElement(aOutputNode).NodeName, aLength-aCount));
                  aHelpNode:=aOutputNode.FirstChild;
                  while assigned(aHelpNode) do
                    begin
                      id:=strtoint(TDOMElement(aHelpNode).GetAttribute(cID));
                      aStr:=TDOMElement(aHelpNode).GetAttribute(cValue);
                      case id of
                        cID_Input: if exfConfig in AXMLFlags then
                                     Devices[AIndex].Outputs[aOutput].Input:=strtoint(aStr);
                        cID_Color: if exfVisuals in AXMLFlags then
                                     Devices[AIndex].Outputs[aOutput].Color:=StringToColor(aStr);
                        cID_Options: if exfVisuals in AXMLFlags then
                                       Devices[AIndex].Outputs[aOutput].Options:=
                                         TConnectionOptions(strtoint('%'+aStr));
                      end;  { case }
                      aHelpNode:=aHelpNode.NextSibling;
                    end;
                  aOutputNode:=aOutputNode.NextSibling;
                end;
            end;
      end;  { case }
      aNode:=aNode.NextSibling;
    end;
end; 

procedure TCustomECScheme.LoadSchemeFromXML(AFileName: string; ASchemeNode: DOMString;
            AXMLFlags: TXMLFlags = cXMLFlagsAll);
var XMLDoc: TXMLDocument;
    aNode: TDOMNode;
begin
  XMLDoc:=nil;
  if FileExistsUTF8(AFileName) then 
    ReadXMLFile(XMLDoc, AFileName, [xrfAllowSpecialCharsInAttributeValue]);
  if assigned(XMLDoc) then
    with XMLDoc do
      begin
        if assigned(DocumentElement) then 
          begin
            if ASchemeNode='' then ASchemeNode:=cScheme;
            aNode:=DocumentElement.FindNode(ASchemeNode); 
            if assigned(aNode) then LoadSchemeFromXML(aNode, AXMLFlags);
          end;
        Free;
      end;        
end;

procedure TCustomECScheme.LoadSchemeFromXML(ASchemeNode: TDOMNode;
            AXMLFlags: TXMLFlags = cXMLFlagsAll);
var aCount, aID: Integer;
    aNodeName: DOMString;
    aNode: TDOMNode;
    aStr: string;
begin
  inc(UpdateCount);
  Devices.Clear;
  { create devices from children }
  aCount:=strtoint(TDOMElement(ASchemeNode).GetAttribute(cCount));
  if aCount>0 then
    for aID:=0 to aCount-1 do
      Devices.AddDevice(TECDevice.Create);
  aNode:=ASchemeNode.FirstChild;
  while assigned(aNode) do
    begin
      aNodeName:=TDOMElement(aNode).NodeName;
      if aNodeName=cDevice
        then LoadDeviceFromXML(strtoint(TDOMElement(aNode).GetAttribute(cIndex)), aNode, AXMLFlags)
        else if exfScheme in AXMLFlags then
               begin
                 aID:=strtoint(TDOMElement(aNode).GetAttribute(cID));
                 aStr:=TDOMElement(aNode).GetAttribute(cValue);
                 case aID of
                   cID_Options: Options:=TSchemeOptions(LongWord(strtoint('%'+aStr)));
                   cID_BlockColor: BlockColor:=StringToColor(aStr);
                   cID_BlockFontSize: BlockFontSize:=strtoint(aStr);
                   cID_BlockHeight: BlockHeight:=strtoint(aStr);
                   cID_BlockStyle: BlockStyle:=TBlockStyle(strtoint(aStr));
                   cID_BlockWidth: BlockWidth:=strtoint(aStr);
                   cID_ConnColor: ConnectorColor:=StringToColor(aStr);
                   cID_ConnWidth: ConnectorWidth:=strtoint(aStr);
                   cID_Grid: Grid:=strtoint(aStr);
                   cID_Indent: Indent:=strtoint(aStr);
                   cID_Layout: Layout:=TObjectPos(strtoint(aStr));
                 end;
               end;
      aNode:=aNode.NextSibling;
    end;
  dec(UpdateCount);
  InvalidateNonUpdated;
end; 

procedure TCustomECScheme.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var i: Integer;
    bSelected, bChanged: Boolean;
begin
  inherited MouseDown(Button, Shift, X, Y);      
  if Button=mbLeft then 
    if not (ssShift in Shift) then
      begin
        bChanged:=False;
        for i:=0 to FHovering-1 do
          begin
            bChanged:= (bChanged or Devices[i].FSelected);
            Devices[i].FSelected:=False;
          end;
        for i:=FHovering+1 to Devices.Count-1 do  { if Hovering=-1 then this clears all }
          begin
            bChanged:=bChanged or Devices[i].FSelected;
            Devices[i].FSelected:=False;
          end;
        if FHovering>=0 then 
          begin
            bChanged:= (bChanged or not Devices[FHovering].FSelected);
            include(FFlags, esfSelected);
            Devices[FHovering].FSelected:=True;
          end else
          begin
            exclude(FFlags, esfSelected);
          end; 
        if bChanged then UpdateSelection;
      end else
      begin
        if FHovering>=0 then 
          begin
            if not Devices[FHovering].Selected
              then bSelected:=True
              else
              begin
                bSelected:=False;
                for i:=0 to Devices.Count-1 do
                  if Devices[i].FSelected then
                    begin
                      bSelected:=True;
                      break;
                    end;
              end;
            if bSelected
              then include(FFlags, esfSelected)
              else exclude(FFlags, esfSelected);
            Devices[FHovering].FSelected:= not Devices[FHovering].Selected;
            UpdateSelection;
          end;
      end;
  if ((Button=mbMiddle) or ((Button=mbLeft) and (ssModifier in Shift))) and (FConnecting=-1) 
    and (FHovering>=0) and not (esfSelecting in FFlags) then
    begin
      FConnecting:=FHovering;
      ChangeCursor(esaConnecting);
    end;
end;

procedure TCustomECScheme.MouseMove(Shift: TShiftState; X, Y: Integer);
var i, aAreaLeft, aAreaTop, aHovering, aLeft, aTop: Integer;
    aGrid: SmallInt;
begin
  inherited MouseMove(Shift, X, Y);
  if esfWasEnabled in FFlags then
    begin    
      if FDragging<0 then
        begin  { Normal mode }
          if (ssLeft in Shift) and (FConnecting<0) then
            begin
              if not (esoReadOnly in Options) and (FHovering>=0) and
                not (edoFixed in Devices[FHovering].Options) then 
                begin
                  FInitPoint:=Point(Devices[FHovering].Left-X, Devices[FHovering].Top-Y);
                  FDragging:=FHovering;
                  ChangeCursor(esaDragging);
                  exit;  { Exit! }
                end;      
              if not (ssShift in Shift) then
                begin
                  if not (esfSelecting in FFlags) then
                    begin
                      include(FFlags, esfSelecting);
                      FSelStartPoint:=Point(X, Y);
                    end else
                    Invalidate;
                end;
            end else
            begin
              aHovering:=-1;
              for i:=Devices.Count-1 downto 0 do
                begin
                  aLeft:=Devices[i].Left-ClientAreaLeft;
                  aTop:=Devices[i].Top-ClientAreaTop;
                  if PtInRect(Rect(aLeft, aTop, aLeft+Devices[i].Width, aTop+Devices[i].Height),
                              Point(X, Y)) then
                    begin
                      aHovering:=i;
                      break;
                    end;
                end;
              Hovering:=aHovering;
              if FConnecting>=0 then InvalidateNonUpdated;
              if (FConnecting<0) and (FDragging<0) then 
                if FHovering=-1
                  then ChangeCursor(esaDefault)
                  else ChangeCursor(esaHovering);
            end;
        end else
        begin  { DragMode }
          aAreaLeft:=ClientAreaLeft;
          aAreaTop:=ClientAreaTop;
          aLeft:=Math.max(Indent, X+FInitPoint.X);  
          aTop:=Math.max(Indent, Y+FInitPoint.Y);
          if AreaWidth<0 then 
            begin  { autosized area width }
              Devices[FDragging].FLeft:=aLeft;    
              UpdateRequiredAreaWidth;
            end else
            begin  { fixed area width }
              aLeft:=Math.min(aLeft, AreaWidth-Devices[FDragging].Width-Indent);
              Devices[FDragging].FLeft:=aLeft; 
            end;
          if AreaHeight<0 then  
            begin  { autosized area height }
              Devices[FDragging].FTop:=aTop;
              UpdateRequiredAreaHeight;
            end else
            begin  { fixed area height }
              aTop:=Math.min(aTop, AreaHeight-Devices[FDragging].Height-Indent);
              Devices[FDragging].FTop:=aTop;
            end;   
         { snap to grid }
          aGrid:=Grid;
          if (esoSnapToGrid in Options) and (aGrid>=cMinGrid) then
            begin
              Devices[FDragging].FLeft:=(aLeft div aGrid)*aGrid;
              Devices[FDragging].FTop:=(aTop div aGrid)*aGrid;
            end;
          { adjust ClientArea }
          if aLeft<(aAreaLeft+Indent)
            then aAreaLeft:=aLeft-Indent
            else
            begin
              i:=aLeft+Devices[FDragging].Width+Indent;
              if i>=FullAreaWidth
                then aAreaLeft:=FullAreaWidth-ClientWidth
                else if i>(aAreaLeft+ClientWidth)
                       then aAreaLeft:=i-ClientWidth;
            end;
          if aTop<(aAreaTop+Indent)
            then aAreaTop:=aTop-Indent
            else
            begin
              i:=aTop+Devices[FDragging].Height+Indent;
              if i>=FullAreaHeight
                then aAreaTop:=FullAreaHeight-ClientHeight
                else if i>(aAreaTop+ClientHeight)
                       then aAreaTop:=i-ClientHeight;
            end;
          if FRequiredArea.X<=ClientWidth then aAreaLeft:=0;
          if FRequiredArea.Y<=ClientHeight then aAreaTop:=0;
          ClientAreaLeft:=aAreaLeft;
          ClientAreaTop:=aAreaTop;
          
          Invalidate;
        end;
   end;        
end;

procedure TCustomECScheme.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var i: Integer;
    bSelected, bSelectionChange, bSelectNotEmpty: Boolean;
    aRect: TRect;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if ((Button=mbMiddle) or ((Button=mbLeft) and (ssModifier in Shift))) and (FConnecting>-1) then
    begin
      if (FHovering>-1) and (FConnecting<>FHovering) then 
        Devices.AddConnection(FHovering, FConnecting);  
      StopConnecting;
    end;
  if Button=mbLeft then
    begin
      if FDragging>=0 then
        begin
          FDragging:=-1;
          ChangeCursor(esaDefault);
          Invalidate;
        end;
      if esfSelecting in FFlags then
        begin
          bSelectNotEmpty:=False;
          bSelectionChange:=False;
          aRect:=NormalizeRectangle(X+ClientAreaLeft, Y+ClientAreaTop,
				           FSelStartPoint.X+ClientAreaLeft, FSelStartPoint.Y+ClientAreaTop);
          for i:=0 to Devices.Count-1 do
            begin
              bSelected:=IsRectIntersect(aRect, Devices[i].BoundsRect);
              bSelectionChange:= bSelectionChange or (Devices[i].FSelected<>bSelected);
         		  Devices[i].FSelected:=bSelected;
         	    bSelectNotEmpty:=bSelectNotEmpty or bSelected;
            end;   
          if bSelectNotEmpty then
            begin
              include(FFlags, esfSelected);
            end else
            begin
              exclude(FFlags, esfSelected);
            end;
          exclude(FFlags, esfSelecting);
          if bSelectionChange
            then UpdateSelection
            else Invalidate;
        end;
      if not (ssModifier in Shift) and (Hovering>=0) and assigned(FOnBlockClick)
        then FOnBlockClick(self, Hovering);
    end;
end;

procedure TCustomECScheme.PaintConnection(ADevice, AOutput: Integer; ADefColor: TColor;
  BFullRepaint: Boolean);
var aConnectWidth: SmallInt;
    aInput, aPenWidth: Integer;
    aPenStyle: TPenStyle;
    aSrcPoint: TPoint;
const cNonShiftShapes = [ebsRhombus, ebsEllipse];

  function CanShiftSourcePoint: Boolean;
  begin
    if esoIdenticalBlocks in Options
      then Result:= not (BlockStyle in cNonShiftShapes)
      else Result:= not (Devices[ADevice].Style in cNonShiftShapes);
  end;

  function CanShiftDestPoint: Boolean;
  begin
    if esoIdenticalBlocks in Options
      then Result:= not (BlockStyle in cNonShiftShapes)
      else Result:= not (Devices[aInput].Style in cNonShiftShapes);
  end;

  procedure DrawArrowRectConnect(ALeft, ATop, ARight, ABottom: Integer; AGlyph: TGlyphDesign);
  var aRect: TRect;
  begin
    aPenStyle:=Canvas.Pen.Style;
    aPenWidth:=Canvas.Pen.Width;
    aSrcPoint:=Canvas.PenPos;
    aRect:=Rect(aSrcPoint.X+ALeft-aConnectWidth, aSrcPoint.Y+ATop-aConnectWidth,
                aSrcPoint.X+ARight+aConnectWidth, aSrcPoint.Y+ABottom+aConnectWidth);
    Canvas.DrawGlyph(aRect, AGlyph, eisEnabled);
    Canvas.Pen.Style:=aPenStyle;
    Canvas.Pen.Width:=aPenWidth;
    Canvas.MoveTo(aSrcPoint);
  end;

var dx, dy, aShift: Integer;
    aArrowAlt: SmallInt;
    aCosAlpha, aSinAlpha: Single;
    aArrow1, aArrow2, aArrow3, aDestPoint: TPoint;
    b, bHorizontal, bL_Connect, bL_VertThenHor: Boolean;
const caArrowsLR: array [False..True] of TGlyphDesign = (egdSizeArrLeft, egdSizeArrRight);
      caArrowsUD: array [False..True] of TGlyphDesign = (egdSizeArrUp, egdSizeArrDown);
begin
  aInput:=Devices[ADevice].Outputs[AOutput].Input;
  dx:=Devices[ADevice].HorCenter-Devices[aInput].HorCenter;
  dy:=Devices[ADevice].VertCenter-Devices[aInput].VertCenter;
  bL_VertThenHor:= (ecoL_VertThenHor in Devices[ADevice].Outputs[AOutput].Options);
  bHorizontal:= (abs(dx)>abs(dy));
  bL_Connect:=False;
  if ecoL_Connection in Devices[ADevice].Outputs[AOutput].Options then
    begin
      if bHorizontal then
        begin
          if not bL_VertThenHor
            then bL_Connect:= ((Devices[ADevice].VertCenter<Devices[aInput].Top)
                           or (Devices[ADevice].VertCenter>Devices[aInput].Bottom))
            else bL_Connect:= ((Devices[ADevice].Bottom<Devices[aInput].VertCenter)
                           or (Devices[ADevice].Top>Devices[aInput].VertCenter));
        end else
        begin
          if not bL_VertThenHor
            then bL_Connect:= ((Devices[ADevice].Right<Devices[aInput].HorCenter)
                           or (Devices[ADevice].Left>Devices[aInput].HorCenter))
            else bL_Connect:= ((Devices[ADevice].HorCenter<Devices[aInput].Left)
                           or (Devices[ADevice].HorCenter>Devices[aInput].Right));
        end;
    end;
  if bHorizontal
    then b:= not bL_Connect or not bL_VertThenHor
    else b:= not (not bL_Connect or bL_VertThenHor);
  if b then
    begin
      if dx>=0
        then aSrcPoint.X:=Devices[ADevice].Left
        else aSrcPoint.X:=Devices[ADevice].Right;
      aSrcPoint.Y:=Devices[ADevice].Top+Devices[ADevice].Height div 2;
      if CanShiftSourcePoint then
        dec(aSrcPoint.Y, trunc(0.4*Devices[ADevice].Height*dy/FullAreaHeight));
    end else
    begin
      aSrcPoint.X:=Devices[ADevice].Left+Devices[ADevice].Width div 2;
      if CanShiftSourcePoint then
        dec(aSrcPoint.X, trunc(0.4*Devices[ADevice].Width*dx/FullAreaWidth));
      if dy>=0
        then aSrcPoint.Y:=Devices[ADevice].Top
        else aSrcPoint.Y:=Devices[ADevice].Bottom;
    end;
  dec(aSrcPoint.X, ClientAreaLeft);
  dec(aSrcPoint.Y, ClientAreaTop);
  if bHorizontal
    then b:= not bL_Connect or bL_VertThenHor
    else b:= bL_Connect and bL_VertThenHor;
  if b then
    begin
      if dx<0
        then aDestPoint.X:=Devices[aInput].Left
        else aDestPoint.X:=Devices[aInput].Right;
      aDestPoint.Y:=Devices[aInput].Top+Devices[aInput].Height div 2;
      if CanShiftDestPoint then
        inc(aDestPoint.Y, trunc(0.4*Devices[aInput].Height*dy/FullAreaHeight));
    end else
    begin
      aDestPoint.X:=Devices[aInput].Left+Devices[aInput].Width div 2;
      if CanShiftDestPoint then
        inc(aDestPoint.X, trunc(0.4*Devices[aInput].Width*dx/FullAreaWidth));
      if dy>=0
        then aDestPoint.Y:=Devices[aInput].Bottom
        else aDestPoint.Y:=Devices[aInput].Top;
    end;
  dec(aDestPoint.X, ClientAreaLeft);
  dec(aDestPoint.Y, ClientAreaTop);
  if not (ecoDashed in Devices[ADevice].Outputs[AOutput].Options)
    then if not (ecoDotted in Devices[ADevice].Outputs[AOutput].Options)
           then Canvas.Pen.Style:=psSolid
           else Canvas.Pen.Style:=psDot
    else if not (ecoDotted in Devices[ADevice].Outputs[AOutput].Options)
           then Canvas.Pen.Style:=psDash
           else Canvas.Pen.Style:=psDashDot;
  Canvas.Brush.Style:=bsClear;
  aConnectWidth:=ConnectorWidth;
  if ecoBold in Devices[ADevice].Outputs[AOutput].Options then inc(aConnectWidth);
  if not bFullRepaint
    then Canvas.Pen.Width:=1
    else Canvas.Pen.Width:=aConnectWidth;
  Canvas.Pen.Color:=GetColorResolvingDefAndEnabled(Devices[ADevice].Outputs[AOutput].Color, ADefColor, IsEnabled);
  if not (esoRectangularConnect in Options) then
    begin  { direct (slant) connections }
      { draw arrow }
      if bFullRepaint and (ecoArrow in Devices[ADevice].Outputs[AOutput].Options) then
        begin
          aArrowAlt:=Canvas.Pen.Width+4;  { arrow is 4 px larger than line }
          aArrow1.X:=(2*aSrcPoint.X+aDestPoint.X) div 3;
          aArrow1.Y:=(2*aSrcPoint.Y+aDestPoint.Y) div 3;
          aCosAlpha:=arctan((aDestPoint.Y-aSrcPoint.Y)/(aDestPoint.X-aSrcPoint.X));  { not cos yet! }
          aSinAlpha:=sin(aCosAlpha);
          aCosAlpha:=cos(aCosAlpha);  { now it is cos }
          if aSrcPoint.X>aDestPoint.X then aArrowAlt:=-aArrowAlt;
          aArrow2.X:=aArrow1.X-trunc(aArrowAlt*(aCosAlpha+aSinAlpha));
          aArrow2.Y:=aArrow1.Y-trunc(aArrowAlt*(aSinAlpha-aCosAlpha));
          aArrow3.X:=aArrow1.X-trunc(aArrowAlt*(aCosAlpha-aSinAlpha));
          aArrow3.Y:=aArrow1.Y-trunc(aArrowAlt*(aSinAlpha+aCosalpha));
          Canvas.Line(aArrow2, aArrow1);
          Canvas.Line(aArrow1, aArrow2);
          Canvas.Line(aArrow3, aArrow1);
          Canvas.Line(aArrow1, aArrow3);
        end;
      Canvas.Line(aSrcPoint, aDestPoint);
    end else  { |_  & _    conn. }
    begin     {   |    |_  }
      Canvas.MoveTo(aSrcPoint);
      if not bL_Connect then
        begin
          if bHorizontal then
            begin
              aShift:=abs(dy) div 10;
              Canvas.LineTo((Canvas.PenPos.X+aDestPoint.X) div 2 -aShift, Canvas.PenPos.Y);
              if bFullRepaint and (ecoArrow in Devices[ADevice].Outputs[AOutput].Options) then
                begin  { draw arrow }
                  aShift:=(aSrcPoint.X-Canvas.PenPos.X) div 2;
                  DrawArrowRectConnect(aShift-3, -3, aShift+3, 4, caArrowsLR[dx<0]);
                end;
              Canvas.LineTo(Canvas.PenPos.X, aDestPoint.Y);
            end else
            begin
              aShift:=abs(dx) div 10;
              Canvas.LineTo(Canvas.PenPos.X, (Canvas.PenPos.Y+aDestPoint.Y) div 2 -aShift);
              if bFullRepaint and (ecoArrow in Devices[ADevice].Outputs[AOutput].Options) then
                begin  { draw arrow }
                  aShift:=(aSrcPoint.Y-Canvas.PenPos.Y) div 2;
                  DrawArrowRectConnect(-3, aShift-3, 4, aShift+3, caArrowsUD[dy<0]);
                end;
              Canvas.LineTo(aDestPoint.X, Canvas.PenPos.Y);
            end;
        end else
        begin  { L-shape connections }
          if not bL_VertThenHor then
            begin  { Horizontal, then Vertical }
              Canvas.LineTo(aDestPoint.X, aSrcPoint.Y);
              if bFullRepaint and (ecoArrow in Devices[ADevice].Outputs[AOutput].Options) then
                begin  { draw arrow }
                  aShift:=(aSrcPoint.X-Canvas.PenPos.X) div 2;
                  DrawArrowRectConnect(aShift-3, -3, aShift+3, 4, caArrowsLR[dx<0]);
                end;
            end else
            begin  { Vertical, then Horizontal }
              Canvas.LineTo(aSrcPoint.X, aDestPoint.Y);
              if bFullRepaint and (ecoArrow in Devices[ADevice].Outputs[AOutput].Options) then
                begin  { draw arrow }
                  aShift:=(aSrcPoint.Y-Canvas.PenPos.Y) div 2;
                  DrawArrowRectConnect(-3, aShift-3, 4, aShift+3, caArrowsUD[dy<0]);
                end;
            end;
        end;
      Canvas.LineTo(aDestPoint);
    end;
  if assigned(OnPaintConnection) then OnPaintConnection(self, aSrcPoint, aDestPoint);
end;

procedure TCustomECScheme.PaintContent(AIndex: Integer; ABlockRect: TRect);
var aDetails: TThemedElementDetails;
    aPoint: TPoint;
    aRect: TRect;
begin
  if assigned(OnPaintContent)
    then OnPaintContent(self, AIndex, ABlockRect)
    else
    begin
      aDetails:=ThemeServices.GetElementDetails(caThemedContent[caItemState[IsEnabled]]);
      if not (edoNoText in Devices[AIndex].Options) then
        begin
          aRect:=Devices[AIndex].FCaptionRect;
          inc(aRect.Left, ABlockRect.Left);
          inc(aRect.Right, ABlockRect.Left);
          inc(aRect.Top, ABlockRect.Top);
          inc(aRect.Bottom, ABlockRect.Top);
          ThemeServices.DrawText(Canvas, aDetails, GetFullCaption(Devices[AIndex]),
            aRect, FTextFlags, 0);
        end;
      if not (edoNoIcon in Devices[AIndex].Options) and assigned(Images) then
        begin
          aPoint.X:=Devices[AIndex].FImagePoint.X+ABlockRect.Left;
          aPoint.Y:=Devices[AIndex].FImagePoint.Y+ABlockRect.Top;
          ThemeServices.DrawIcon(Canvas, aDetails, aPoint, Images, Devices[AIndex].ImageIndex);
        end;
    end;
end;

procedure TCustomECScheme.Paint;
  
  procedure CalcSelectionRectangle;
  var aRect: TRect;
      i, j: Integer;
  begin
    j:=-1;
    for i:=0 to Devices.Count-1 do
      if Devices[i].Selected then 
        begin
          j:=i;
          break;
        end;
    if j>=0 then
      begin
        aRect:=Devices[j].BoundsRect;
        for i:=j+1 to Devices.Count-1 do
          if Devices[i].Selected then IncludeRectangle(aRect, Devices[i].BoundsRect);
        InflateRect(aRect, 4, 4);     
        i:=ClientAreaLeft;
        dec(aRect.Left, i);
        dec(aRect.Right, i);
        j:=ClientAreaTop;
        dec(aRect.Top, j);
        dec(aRect.Bottom, j);
        FSelStartPoint:=aRect.TopLeft;
        FSelEndPoint:=aRect.BottomRight;
      end;
  end;    
  
  procedure DrawGrid;
  var i, j, dx, dy, aGrid: SmallInt;            
  begin
    Canvas.AntialiasingMode:=amOff;
    Canvas.Brush.Style:=bsClear;
    Canvas.Pen.Color:=GetMergedColor(clBtnText, clForm, 0.1);
    Canvas.Pen.Style:=psDot;
    Canvas.Pen.Width:=1;
    GetGridDelta(dx, dy);
    aGrid:=Grid;
    for i:=1 to (ClientWidth div Grid)+1 do
      begin
        j:=i*aGrid-dx;
        Canvas.Line(j, 0, j, ClientHeight);        
      end;
    for j:=1 to (ClientHeight div Grid)+1 do
      begin
        i:=j*aGrid-dy;
        Canvas.Line(0, i, ClientWidth, i);
      end;
  end;

var i, j: Integer;
    aColor: TColor;
    aDestPoint: TPoint;
    aFPEMask: TFPUExceptionMask;
    aRect: TRect;
    bEnabled, bFullRepaint: Boolean;
begin
  {$IFDEF DEBUG} DebugLn('Paint ', inttostr(ClientAreaOrigin.X)+' ', inttostr(ClientAreaOrigin.Y)); {$ENDIF}
  inherited Paint;
  bEnabled:=IsEnabled;
  if esfNeedRecalc in FFlags then CalcBlockSizesAndContent;
  if (esfNeedRedraw in FFlags) or ((esoIdenticalBlocks in Options)
    and ((esfWasEnabled in FFlags)<>bEnabled)) then
    begin
      if assigned(OnDrawBlock)
        then OnDrawBlock(self, FBlock)
        else
        begin
          FBlock.SetSize(BlockWidth, BlockHeight);
          FBlock.TransparentColor:=Brush.Color;
          FBlock.TransparentClear;
          aColor:=GetColorResolvingDefault(BlockColor, clBtnText);
          if not bEnabled then aColor:=GetMergedColor(aColor, clBtnFace, 0.5);
          FBlock.Canvas.Pen.Color:=aColor;
          DrawBlock(FBlock.Canvas, Rect(0, 0, FBlock.Width, FBlock.Height), BlockStyle, bEnabled);
          exclude(FFlags, esfNeedRedraw);
        end;
    end;

  bFullRepaint:= ((esoFullDragRepaint in Options) or ((FDragging<0) and (FConnecting<0)));

  { draw grid }
  if (bFullRepaint or (esoSnapToGrid in Options)) and
    (esoShowGrid in Options) and (Grid>=cMinGrid) then
      DrawGrid;

  if bFullRepaint and not (esoRectangularConnect in Options) then Canvas.AntialiasingMode:=amOn;

  { draw connections }
  aColor:=GetColorResolvingDefault(ConnectorColor, clBtnText);
  if not bEnabled then aColor:=GetMergedColor(GetMonochromaticColor(aColor), clBtnFace, 0.55);
  Canvas.Brush.Style:=bsClear;
  aFPEMask:=Math.GetExceptionMask;
  Math.SetExceptionMask(aFPEMask+[exZeroDivide]);
  for i:=0 to Devices.Count-1 do
    for j:=0 to length(Devices.Items[i].Outputs)-1 do
      PaintConnection(i, j, aColor, bFullRepaint);
  Math.SetExceptionMask(aFPEMask);

  { draw devices }
  Canvas.AntialiasingMode:=amOff;
  Canvas.Font.Size:=BlockFontSize;
  j:=0;
  for i:=0 to Devices.Count-1 do
    begin
      aDestPoint:=Point(Devices[i].Left, Devices[i].Top);
      dec(aDestPoint.X, ClientAreaLeft);
      dec(aDestPoint.Y, ClientAreaTop);
      aRect.TopLeft:=aDestPoint;
      if esoIdenticalBlocks in Options then
        begin
          aRect.Right:=aDestPoint.X+FBlockWidth;
          aRect.Bottom:=aDestPoint.Y+FBlockHeight;
        end else
        begin
          aRect.Right:=aDestPoint.X+Devices[i].Width;
          aRect.Bottom:=aDestPoint.Y+Devices[i].Height;
        end;
      if bFullRepaint then
        begin
          if esoIdenticalBlocks in Options
            then Canvas.Draw(aRect.Left, aRect.Top, FBlock)
            else
            begin
              aColor:=GetColorResolvingDefault(Devices[i].Color, clBtnText);
              if not bEnabled then aColor:=GetMergedColor(GetMonochromaticColor(aColor), clBtnFace, 0.55);
              Canvas.Pen.Color:=aColor;
              DrawBlock(Canvas, aRect, Devices[i].Style, bEnabled);
            end;
        end else
        Canvas.Frame(aRect);
      { Paint Caption }
      if bFullRepaint then
        begin
          if not (esoIdenticalBlocks in Options) then
            begin
              aColor:=GetColorResolvingDefault(Devices[i].FontColor, clBtnText);
              if not bEnabled then aColor:=GetMergedColor(GetMonochromaticColor(aColor), clBtnFace, 0.55);
              Canvas.Font.Color:=aColor;
            end else
            Canvas.Font.Color:=Font.Color;
          PaintContent(i, aRect);
        end;
      { Paint single FocusRect }
      if Devices[i].Selected and
        not (((FDragging>=0) or (FConnecting>=0)) and not (esoFullDragRepaint in Options)) then
        begin
          InflateRect(aRect, 2, 2);
          Canvas.DrawFocusRect(aRect);
          inc(j);
        end;
    end;

  { creating new connection with mouse }
  if FConnecting>=0 then
    begin
      Canvas.Pen.Width:=1;
      Canvas.Pen.Style:=psDash;
      Canvas.Pen.Color:=clHighlight;
      Canvas.Brush.Color:=clForm;
      aDestPoint:=ScreenToControl(Mouse.CursorPos);
      Canvas.Line(Devices[FConnecting].Left+Devices[FConnecting].Width div 2 -ClientAreaLeft,
                  Devices[FConnecting].Top+Devices[FConnecting].Height div 2 -ClientAreaTop,
                  aDestPoint.X, aDestPoint.Y);
    end;

  if bEnabled then
    begin
      { selecting more devices with mouse }
      if esfSelecting in FFlags then
        begin
          aDestPoint:=ScreenToControl(Mouse.CursorPos);
          Canvas.DrawFocusRect(Rect(FSelStartPoint.X, FSelStartPoint.Y, aDestPoint.X, aDestPoint.Y));
        end;
      { more than one device is selected }
      if (j>1) and (esfSelected in FFlags) and not ((FDragging>=0) and not (esoFullDragRepaint in Options)) then
        begin
          CalcSelectionRectangle;
          Canvas.DrawFocusRect(Rect(FSelStartPoint.X, FSelStartPoint.Y, FSelEndPoint.X, FSelEndPoint.Y));
        end;
    end;
  
  if bEnabled 
    then include(FFlags, esfWasEnabled)
    else exclude(FFlags, esfWasEnabled);   
end;

procedure TCustomECScheme.SaveDeviceToXML(AIndex: Integer; AXMLDoc: TXMLDocument;
  ADeviceNode: TDOMNode; AXMLFlags: TXMLFlags = cXMLFlagsAll);
var aConnNode, aOutputNode: TDOMNode;  
    i, aConnectCnt: Integer;  
begin
  if exfCaption in AXMLFlags then
    CreateNode(AXMLDoc, ADeviceNode, cCaption, Devices[AIndex].Caption, cID_Caption);
  if exfDescript in AXMLFlags then
    CreateNode(AXMLDoc, ADeviceNode, cDescript, Devices[AIndex].Description, cID_Decript);
  if exfGeometry in AXMLFlags then
    begin
      CreateNode(AXMLDoc, ADeviceNode, cHeight, inttostr(Devices[AIndex].Height), cID_Height);
      CreateNode(AXMLDoc, ADeviceNode, cLeft, inttostr(Devices[AIndex].Left), cID_Left);
      CreateNode(AXMLDoc, ADeviceNode, cTop, inttostr(Devices[AIndex].Top), cID_Top);
      CreateNode(AXMLDoc, ADeviceNode, cWidth, inttostr(Devices[AIndex].Width), cID_Width);
    end;
  if exfVisuals in AXMLFlags then
    begin
      CreateNode(AXMLDoc, ADeviceNode, cColor, ColorToString(Devices[AIndex].Color), cID_Color);
      CreateNode(AXMLDoc, ADeviceNode, cFontColor, ColorToString(Devices[AIndex].FontColor), cID_FontColor);
      CreateNode(AXMLDoc, ADeviceNode, cImageIndex, inttostr(Devices[AIndex].ImageIndex), cID_ImageIndex);
      CreateNode(AXMLDoc, ADeviceNode, cOptions, binStr(LongWord(Devices[AIndex].Options), 32), cID_Options);
      CreateNode(AXMLDoc, ADeviceNode, cStyle, inttostr(Word(Devices[AIndex].Style)), cID_Style);
    end;
  if exfConfig in AXMLFlags then
    begin
      CreateNode(AXMLDoc, ADeviceNode, cMaxInputs, inttostr(Devices[AIndex].MaxInputs), cID_MaxIn);
      CreateNode(AXMLDoc, ADeviceNode, cMaxOutputs, inttostr(Devices[AIndex].MaxOutputs), cID_MaxOut);
      CreateNode(AXMLDoc, ADeviceNode, cTag, inttostr(Devices[AIndex].Tag), cID_Tag);
    end;
  if [exfConfig, exfVisuals]*AXMLFlags<>[] then
    begin
      aConnectCnt:=length(Devices[AIndex].Outputs);
      if aConnectCnt>0 then
        begin
          aConnNode:=AXMLDoc.CreateElement(cConnects);
          ADeviceNode.AppendChild(aConnNode);
          TDOMElement(aConnNode).SetAttribute(cID, inttostr(cID_Connects));
          TDOMElement(aConnNode).SetAttribute(cCount, inttostr(aConnectCnt));
          for i:=0 to aConnectCnt-1 do
            begin
              aOutputNode:=AXMLDoc.CreateElement(cOutput+inttostr(i));
              aConnNode.AppendChild(aOutputNode);
              TDOMElement(aOutputNode).SetAttribute(cID, inttostr(cID_Output));
              if exfConfig in AXMLFlags then
                CreateNode(AXMLDoc, aOutputNode, cInput, inttostr(Devices[AIndex].Outputs[i].Input), cID_Input);
              if exfVisuals in AXMLFlags then
                begin
                  CreateNode(AXMLDoc, aOutputNode, cColor, ColorToString(Devices[AIndex].Outputs[i].Color), cID_Color);
                  CreateNode(AXMLDoc, aOutputNode, cOptions, binStr(LongWord(Devices[AIndex].Outputs[i].Options), 32), cID_Options);
                end;
            end;
        end;
    end;
end;

{ saves scheme to file, i.e. xml file contains scheme in <ROOT><ASchemeNode... }
procedure TCustomECScheme.SaveSchemeToXML(AFileName: string;
            ASchemeNode: DOMString; AXMLFlags: TXMLFlags = cXMLFlagsAll);
var XMLDoc: TXMLDocument;
    aNode: TDOMNode;
begin
  XMLDoc:=nil;
  if FileExistsUTF8(AFileName) then 
    ReadXMLFile(XMLDoc, AFileName, [xrfAllowSpecialCharsInAttributeValue]);
  if not assigned(XMLDoc) then XMLDoc:=TXMLDocument.Create;
  with XMLDoc do
    begin
      if not assigned(DocumentElement) then
        begin
          aNode:=CreateElement(cRoot);
          AppendChild(aNode); 
        end;    
      if ASchemeNode='' then ASchemeNode:=cScheme;
      aNode:=DocumentElement.FindNode(ASchemeNode); 
      if not assigned(aNode) then
        begin
          aNode:=CreateElement(ASchemeNode);
          DocumentElement.AppendChild(aNode);
        end;   
      SaveSchemeToXML(XMLDoc, aNode, AXMLFlags);
      WriteXMLFile(XMLDoc, AFileName);
      Free;
    end;
end;

procedure TCustomECScheme.SaveSchemeToXML(AXMLDoc: TXMLDocument;
            ASchemeNode: TDOMNode; AXMLFlags: TXMLFlags = cXMLFlagsAll);
var aNode, aDeviceNode: TDOMNode;
    i, aCount: Integer;
begin
  aDeviceNode:=ASchemeNode.FirstChild;
  { delete old children }
  while assigned(aDeviceNode) do
    begin
      aNode:=aDeviceNode;
      aDeviceNode:=aDeviceNode.NextSibling;          
      aNode:=ASchemeNode.RemoveChild(aNode);       
      aNode.Free;
    end;
  { create new children and fill them }
  aCount:=Devices.Count;
  TDOMElement(ASchemeNode).SetAttribute(cCount, inttostr(aCount));
  for i:=0 to aCount-1 do
    begin
      aDeviceNode:=AXMLDoc.CreateElement(cDevice);
      TDOMElement(aDeviceNode).SetAttribute(cIndex, inttostr(i));
      ASchemeNode.AppendChild(aDeviceNode);
      SaveDeviceToXML(i, AXMLDoc, aDeviceNode, AXMLFlags);
    end;
  if exfScheme in AXMLFlags then
    begin
      CreateNode(AXMLDoc, ASchemeNode, cBlockColor, ColorToString(BlockColor), cID_BlockColor);
      CreateNode(AXMLDoc, ASchemeNode, cBlockFontSize, inttostr(BlockFontSize), cID_BlockFontSize);
      CreateNode(AXMLDoc, ASchemeNode, cBlockHeight, inttostr(BlockHeight), cID_BlockHeight);
      CreateNode(AXMLDoc, ASchemeNode, cBlockStyle, inttostr(Word(BlockStyle)), cID_BlockStyle);
      CreateNode(AXMLDoc, ASchemeNode, cBlockWidth, inttostr(BlockWidth), cID_BlockWidth);
      CreateNode(AXMLDoc, ASchemeNode, cConnColor, ColorToString(ConnectorColor), cID_ConnColor);
      CreateNode(AXMLDoc, ASchemeNode, cConnWidth, inttostr(ConnectorWidth), cID_ConnWidth);
      CreateNode(AXMLDoc, ASchemeNode, cGrid, inttostr(Grid), cID_Grid);
      CreateNode(AXMLDoc, ASchemeNode, cIndent, inttostr(Indent), cID_Indent);
      CreateNode(AXMLDoc, ASchemeNode, cLayout, inttostr(Word(Layout)), cID_Layout);
      CreateNode(AXMLDoc, ASchemeNode, cOptions, binStr(LongWord(Options), 32), cID_Options);
    end;
end;

procedure TCustomECScheme.Select(AIndex: Integer);
begin
  Devices[AIndex].Selected:=True;
  include(FFlags, esfSelected);
end;

procedure TCustomECScheme.SelectAll;
var aDevice: TECDevice;
begin
  for aDevice in Devices do
    aDevice.Selected:=True; 
  include(FFlags, esfSelected);
  InvalidateNonUpdated;
end;            

procedure TCustomECScheme.SetCursor(Value: TCursor);
begin
  inherited SetCursor(Value);
  if not (esfCursorLock in FFlags) then FCursorBkgnd:=Value;   
end;           

procedure TCustomECScheme.SetDefaultScrollParams;
begin
  IncrementX:=cDefBlockWidth;
  IncrementY:=cDefBlockHeight; 
  FScrollBars:=ssAutoBoth;
end;     

procedure TCustomECScheme.SetHint(const Value: TTranslateString);
var aOldHint: string;
begin
  aOldHint:=Hint;  
  inherited SetHint(Value);
  if not (esfHintLock in FFlags)
    then FHintBkgnd:=Value
    else if aOldHint<>Value then 
           begin
             Application.CancelHint;
             Application.ActivateHint(Mouse.CursorPos);
           end;
end;

procedure TCustomECScheme.SetParent(NewParent: TWinControl);
begin
  inherited SetParent(NewParent);
  if not (csDestroying in ComponentState) then CalcBlockSizesAndContent(nil);
end;

procedure TCustomECScheme.ShowDevice(AIndex: Integer);
begin
  ClientAreaLeft:=Devices[AIndex].Left-(ClientWidth-Devices[AIndex].Width) div 2;
  ClientAreaTop:=Devices[AIndex].Top-(ClientHeight-Devices[AIndex].Height) div 2;
end;

procedure TCustomECScheme.ShowDevice(ACaption: string);
var i: Integer;
begin
  i:=Devices.CaptionToIndex(ACaption);
  if i>=0 then ShowDevice(i);
end;  

procedure TCustomECScheme.StopConnecting;
begin
  FConnecting:=-1;
  ChangeCursor(esaDefault);
  InvalidateNonUpdated;  
end;

procedure TCustomECScheme.UpdateDevice(ADevice: TECDevice);
begin
  CalcBlockSizesAndContent(ADevice);
  InvalidateNonUpdated;
end;

procedure TCustomECScheme.UpdateRequiredAreaHeight;
var i, h, aBottom: Integer;
begin
  if AreaHeight>=0 then 
    begin
      h:=AreaHeight-Indent;
      for i:=0 to Devices.Count-1 do
        if Devices[i].Bottom>h then Devices[i].Top:=h-Devices[i].Height;
      h:=AreaHeight;
    end else
    begin  { autosized }
      h:=0;
      for i:=0 to Devices.Count-1 do
        begin
          aBottom:=Devices[i].Bottom;
          if aBottom>h then h:=aBottom;
        end;
      inc(h, Indent);
    end;
  FRequiredArea.Y:=h;
  UpdateScrollInfoVert;
end;

procedure TCustomECScheme.UpdateRequiredAreaWidth;
var i, w, aRight: Integer;
begin
  if AreaWidth>=0 then 
    begin
      w:=AreaWidth-Indent;
      for i:=0 to Devices.Count-1 do
        if Devices[i].Right>w then Devices[i].Left:=w-Devices[i].Width;
      w:=AreaWidth;
    end else
    begin  { autosized }
      w:=0;
      for i:=0 to Devices.Count-1 do
        begin
          aRight:=Devices[i].Right;
          if aRight>w then w:=aRight;
        end;
      inc(w, Indent);
    end;
  FRequiredArea.X:=w;    
  UpdateScrollInfoHor;
end;
        
procedure TCustomECScheme.UpdateScheme;
begin
  if UpdateCount=0 then
    begin
      if assigned(OnSchemeChange) then OnSchemeChange(self);
      Invalidate;
    end;
end;

procedure TCustomECScheme.UpdateSelection;
begin
  if UpdateCount=0 then
    begin
      if assigned(OnSelectionChange) then OnSelectionChange(self);
      Invalidate;
    end;
end;             

{ TCustomECScheme.Setters } 

procedure TCustomECScheme.SetBlockColor(AValue: TColor);
begin
  if FBlockColor=AValue then exit;
  FBlockColor:=AValue;
  if esoIdenticalBlocks in Options then include(FFlags, esfNeedRedraw);
  InvalidateNonUpdated;
end;

procedure TCustomECScheme.SetBlockFontSize(AValue: SmallInt);
begin
  if FBlockFontSize=AValue then exit;
  FBlockFontSize:=AValue;
  if esoIdenticalBlocks in Options then FFlags:=FFlags+[esfNeedRedraw, esfNeedRecalc];
  InvalidateNonUpdated;
end;
         
procedure TCustomECScheme.SetBlockHeight(AValue: SmallInt);
var i: Integer;
begin
  if (FBlockHeight=AValue) or (AValue<0) then exit;
  FBlockHeight:=AValue;
  IncrementY:=AValue;
  if esoIdenticalBlocks in Options then
    begin
      inc(UpdateCount);
      for i:=0 to Devices.Count-1 do
        Devices[i].Height:=AValue;
      dec(UpdateCount);
    end;
  if esoIdenticalBlocks in Options then FFlags:=FFlags+[esfNeedRedraw, esfNeedRecalc];
  if AreaHeight<0 then UpdateRequiredAreaHeight;
  InvalidateNonUpdated;
end;

procedure TCustomECScheme.SetBlockStyle(AValue: TBlockStyle);
begin
  if FBlockStyle=AValue then exit;
  FBlockStyle:=AValue;
  if esoIdenticalBlocks in Options then FFlags:=FFlags+[esfNeedRedraw, esfNeedRecalc];
  InvalidateNonUpdated;
end;  

procedure TCustomECScheme.SetBlockWidth(AValue: SmallInt);
var i: Integer;
begin
  if (FBlockWidth=AValue) or (AValue<0) then exit;
  FBlockWidth:=AValue;
  IncrementX:=AValue;
  if esoIdenticalBlocks in Options then
    begin
      inc(UpdateCount);
      for i:=0 to Devices.Count-1 do
        Devices[i].Width:=AValue;
      dec(UpdateCount);
    end;
  if esoIdenticalBlocks in Options then FFlags:=FFlags+[esfNeedRedraw, esfNeedRecalc];
  if AreaWidth<0 then UpdateRequiredAreaWidth;
  InvalidateNonUpdated;
end;    

procedure TCustomECScheme.SetConnectorColor(AValue: TColor);
begin
  if FConnectorColor=AValue then exit;
  FConnectorColor:=AValue;
  InvalidateNonUpdated;
end;                            

procedure TCustomECScheme.SetConnectorWidth(AValue: SmallInt);
begin
  if AValue<1 then AValue:=1;
  if FConnectorWidth=AValue then exit;
  FConnectorWidth:=AValue;
  InvalidateNonUpdated;
end;

procedure TCustomECScheme.SetGrid(AValue: SmallInt);
begin
  if AValue<cMinGrid then AValue:=0;
  if FGrid=AValue then exit;
  FGrid:=AValue;
  InvalidateNonUpdated;
end;

procedure TCustomECScheme.SetHovering(AValue: Integer);
begin
  if FHovering=AValue then exit;
  FHovering:=AValue;
  if esoDescriptionToHint in Options then
    begin
      include(FFlags, esfHintLock);
      if (AValue<0) or (Devices[AValue].Description='')
        then Hint:=FHintBkgnd
        else Hint:=Devices[AValue].Description;
      exclude(FFlags, esfHintLock);
    end;
end;

procedure TCustomECScheme.SetImages(AValue: TCustomImageList);
begin
  if FImages=AValue then exit;
  FImages:=AValue;
  FFlags:=FFlags+[esfNeedRedraw, esfNeedRecalc];
  InvalidateNonUpdated;
end;

procedure TCustomECScheme.SetIndent(AValue: SmallInt);
var i, s: Integer;
begin
  if (FIndent=AValue) or (AValue<0) then exit;
  FIndent:=AValue;
  BeginUpdate;
  for i:=0 to Devices.Count-1 do
    begin
      if Devices[i].Left<AValue then Devices[i].FLeft:=AValue;
      if Devices[i].Top<AValue then Devices[i].FTop:=AValue;
    end;
  if AreaWidth>=0 then
    begin
      s:=AreaWidth-Indent;
      for i:=0 to Devices.Count-1 do
        if Devices[i].Right>s then Devices[i].Left:=s-Devices[i].Width;
    end else
    UpdateRequiredAreaWidth;
  if AreaHeight>=0 then
    begin
      s:=AreaHeight-Indent;
      for i:=0 to Devices.Count-1 do
        if Devices[i].Bottom>s then Devices[i].Top:=s-Devices[i].Height;
    end else
    UpdateRequiredAreaHeight;               
  EndUpdate;         
end;

procedure TCustomECScheme.SetLayout(AValue: TObjectPos);
begin
  if FLayout=AValue then exit;
  FLayout:=AValue;
  if assigned(Images) then
    begin
      FFlags:=FFlags+[esfNeedRedraw, esfNeedRecalc];
      InvalidateNonUpdated;
    end;
end;

procedure TCustomECScheme.SetOptions(AValue: TSchemeOptions);
var bIdentBlocksChanged, bInv: Boolean;
    i: Integer;
const cInvOptions = [esoIdenticalBlocks, esoDescriptionOnBlock, esoRectangularConnect, esoShowGrid];
begin
  if FOptions=AValue then exit;
  bIdentBlocksChanged:= (([esoIdenticalBlocks]*FOptions)<>([esoIdenticalBlocks]*AValue));
  bInv:= ((cInvOptions*FOptions)<>(cInvOptions*AValue));
  FOptions:=AValue;
  if bIdentBlocksChanged and (esoIdenticalBlocks in AValue) then
    begin
      inc(UpdateCount);
      for i:=0 to Devices.Count-1 do
        begin
          Devices[i].Height:=BlockHeight;
          Devices[i].Width:=BlockWidth;
          exclude(Devices[i].FOptions, edoAutosized);  { esoIdentical cannot be autosized }
        end;
      dec(UpdateCount);
      bInv:=True;
    end;
  if bIdentBlocksChanged then FFlags:=FFlags+[esfNeedRecalc, esfNeedRedraw];
  if bInv then InvalidateNonUpdated;
end;

end.


