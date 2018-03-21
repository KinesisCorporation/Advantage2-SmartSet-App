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

unit ECLink;
{$mode objfpc}{$H+}

//{$DEFINE DBGLINK}  {don't remove, just comment}

interface

uses
  Classes, SysUtils, Controls, Forms, Graphics, LCLIntf, LCLType,
  LMessages, Themes, Types;

type
  {$PACKENUM 2}
  TLinkType = (eltClick,  //expected implementation in OnClick event OR assigned Action
               eltFile,   //opens file in associated application; Link is expected to be valid path to file
               eltMail,   //adds "mailto:" to Link; Link is expected to be valid email adress
               eltWWW);   //opens URL with defult browser; URL should begin with http://...

  { TCustomECLink }
  TCustomECLink = class(TGraphicControl)
  private
    FAlignment: TAlignment;
    FColorHovered: TColor;
    FColorVisited: TColor;
    FHoveredUnderlined: Boolean;
    FLayout: TTextLayout;
    FLink: string;
    FLinkType: TLinkType;
    FShowAccelChar: Boolean;
    FTransparent: Boolean;
    FVisited: Boolean;
    function IsLinkStored: Boolean;
    procedure SetAlignment(AValue: TAlignment);
    procedure SetColorVisited(AValue: TColor);
    procedure SetLayout(AValue: TTextLayout);
    procedure SetShowAccelChar(AValue: Boolean);
    procedure SetTransparent(AValue: Boolean);
    procedure SetVisited(AValue: Boolean);
  protected
    procedure AutosizeInvalidate;
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: Integer;
                                     {%H-}WithThemeSpace: Boolean); override;
    function DialogChar(var Message: TLMKey): boolean; override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure Paint; override;
    procedure SetAction(Value: TBasicAction); override;
    procedure TextChanged; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property ColorHovered: TColor read FColorHovered write FColorHovered default clDefault;
    property ColorVisited: TColor read FColorVisited write SetColorVisited default clDefault;
    property HoveredUnderlined: Boolean read FHoveredUnderlined write FHoveredUnderlined default True;
    property Layout: TTextLayout read FLayout write SetLayout default tlTop;
    property Link: string read FLink write FLink stored IsLinkStored;
    property LinkType: TLinkType read FLinkType write FLinkType default eltClick;
    property ShowAccelChar: Boolean read FShowAccelChar write SetShowAccelChar default True;
    property Transparent: Boolean read FTransparent write SetTransparent default True;
    property Visited: Boolean read FVisited write SetVisited default False;
  end;

  { TECLink }
  TECLink = class(TCustomECLink)
  published
    property Action;
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize default True;
    property BiDiMode;
    property BorderSpacing;
    property Caption;
    property Color;
    property ColorHovered;
    property ColorVisited;
    property Constraints;
    property Cursor default crHandPoint;
    property Enabled;
    property Font;
    property HoveredUnderlined;
    property Layout;
    property Link;
    property LinkType;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property Transparent;
    property Visible;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDrag;
  end;

implementation

{ TCustomECLink }

constructor TCustomECLink.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AutoSize:=True;
  Cursor:=crHandPoint;
  FColorHovered:=clDefault;
  FColorVisited:=clDefault;
  FHoveredUnderlined:=True;
  FShowAccelChar:=True;
  FTransparent:=True;
  AccessibleRole:=larButton;
end;

procedure TCustomECLink.AutosizeInvalidate;
begin
  if Autosize then
    begin
      InvalidatePreferredSize;
      AdjustSize;
    end;
  Invalidate;
end;

procedure TCustomECLink.CalculatePreferredSize(var PreferredWidth,
            PreferredHeight: Integer; WithThemeSpace: Boolean);
var aExtent: TSize;
begin
  aExtent:=Canvas.TextExtent(Caption);
  PreferredWidth:=aExtent.cx;
  PreferredHeight:=aExtent.cy;
end;

procedure TCustomECLink.Click;
begin
  case LinkType of
    eltFile: OpenDocument(Link);
    eltMail: OpenURL('mailto:'+Link);
    eltWWW: OpenURL(Link);
  end;
  inherited Click;
  FVisited:=True;
end;

function TCustomECLink.DialogChar(var Message: TLMKey): boolean;
begin
  Result:=False;
  if ShowAccelChar then
    begin
      if IsAccel(Message.CharCode, Caption) then
        begin
          Result:=True;
          Click;
        end else
        Result:=inherited DialogChar(Message);
    end;
end;

function TCustomECLink.IsLinkStored: Boolean;
begin
  Result:= (LinkType>eltClick);
end;

procedure TCustomECLink.MouseEnter;
begin
  inherited MouseEnter;
  Invalidate;
end;

procedure TCustomECLink.MouseLeave;
begin
  inherited MouseLeave;
  Invalidate;
end;

procedure TCustomECLink.Paint;
var aColor: TColor;
    aDetails: TThemedElementDetails;
    aFlags: Cardinal;
    aStyles: TFontStyles;
const caElementDetails: array [Boolean] of TThemedButton
        = (tbPushButtonDisabled, tbPushButtonNormal);
      caAlignment: array [Boolean, TAlignment] of Cardinal
        = ((DT_LEFT, DT_RIGHT, DT_CENTER),
           (DT_RIGHT or DT_RTLREADING, DT_LEFT or DT_RTLREADING, DT_CENTER or DT_RTLREADING));
begin
  inherited Paint;
  if not Transparent and (Color<>clDefault) then
    begin
      Canvas.Brush.Color:=Color;
      Canvas.FillRect(ClientRect);
    end;
  aColor:=Font.Color;
  aStyles:=Font.Style;
  if MouseEntered then
    begin
      if HoveredUnderlined then
        begin
          aStyles:=aStyles+[fsUnderline];
        end;
      if ColorHovered=clDefault
        then aColor:=clBlue
        else aColor:=ColorHovered;
    end else
    begin
      if Visited then
        begin
          if ColorVisited<>clDefault then aColor:=ColorVisited;
        end;
    end;
  Canvas.Font.Color:=aColor;
  Canvas.Font.Style:=aStyles;
  aDetails:=ThemeServices.GetElementDetails(caElementDetails[IsEnabled]);
  aFlags:=DT_SINGLELINE;
  case Layout of
    tlCenter: aFlags:=aFlags or DT_VCENTER;
    tlBottom: aFlags:=aFlags or DT_BOTTOM;
  end;
  aFlags:=aFlags or caAlignment[IsRightToLeft, Alignment];
  if not ShowAccelChar then aFlags:=aFlags or DT_NOPREFIX;
  ThemeServices.DrawText(Canvas, aDetails, Caption, ClientRect, aFlags, 0);
end;

procedure TCustomECLink.SetAction(Value: TBasicAction);
begin
  inherited SetAction(Value);
  if Value=nil then
    begin
      if Link<>''
        then Caption:=Link
        else if Name<>'' then Caption:=Name;
    end;
  AutosizeInvalidate;
end;

procedure TCustomECLink.TextChanged;
begin
  inherited TextChanged;
  AutosizeInvalidate;
end;

{ Setters }

procedure TCustomECLink.SetAlignment(AValue: TAlignment);
begin
  if FAlignment=AValue then exit;
  FAlignment:=AValue;
  if not AutoSize then Invalidate;
end;

procedure TCustomECLink.SetColorVisited(AValue: TColor);
begin
  if FColorVisited=AValue then exit;
  FColorVisited:=AValue;
  if FVisited then Invalidate;;
end;

procedure TCustomECLink.SetLayout(AValue: TTextLayout);
begin
  if FLayout=AValue then exit;
  FLayout:=AValue;
  if not Autosize then Invalidate;
end;

procedure TCustomECLink.SetShowAccelChar(AValue: Boolean);
begin
  if FShowAccelChar=AValue then exit;
  FShowAccelChar:=AValue;
  AutosizeInvalidate;
end;

procedure TCustomECLink.SetTransparent(AValue: Boolean);
begin
  if FTransparent=AValue then exit;
  FTransparent:=AValue;
  if Color<>clDefault then Invalidate;
end;

procedure TCustomECLink.SetVisited(AValue: Boolean);
begin
  if FVisited=AValue then exit;
  FVisited:=AValue;
  Invalidate;
end;

end.


