{-------------------------------------------------------------------------------------
  TuETilePanel v1.0  2015-05-20
  Author: Miguel A. Risco-Castillo
  http://ue.accesus.com/uecontrols

  THE COPYRIGHT NOTICES IN THE SOURCE CODE MAY NOT BE REMOVED OR MODIFIED.
  IF YOU MODIFY AND/OR DISTRIBUTE THE CODE TO ANY THIRD PARTY THEN YOU MUST NOT
  VEIL THE ORIGINAL AUTHOR. IT MUST ALWAYS BE CLEARLY IDENTIFIABLE.

  The contents of this file are subject in priority to the License in this header,
  in the license.txt file and the Mozilla Public License Version 1.1 (MPL);
  you may not use this file except in compliance with these licenses. You may obtain
  a copy of the MPL License at http://www.mozilla.org/MPL/MPL-1.1.html

  Software distributed under the Licenses is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the Licenses for
  the specific language governing rights and limitations under the Licenses.
-------------------------------------------------------------------------------------}
unit uETilePanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Controls, ExtCtrls, LCLProc, Graphics, uETileImage;

type

  { TCustomuETilePanel }

  TCustomuETilePanel = class(TPanel)
  private
    FImage: TBitmap;
    FTile: TuETileImage;
    FAbout: String;
    procedure SetImage(AValue: TBitmap);
    procedure SetTile(AValue: TuETileImage);
  protected
    property About:string read FAbout;
    property Tile: TuETileImage read FTile write SetTile;
    property Image: TBitmap read FImage write SetImage;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TuETilePanel }

  TuETilePanel = class(TCustomuETilePanel)
  published
    property Image;
    property Tile;
  end;

implementation

{ TuETilePanel }

procedure TCustomuETilePanel.SetTile(AValue: TuETileImage);
begin
  if FTile=AValue then Exit;
  FTile:=AValue;
end;

procedure TCustomuETilePanel.SetImage(AValue: TBitmap);
begin
  if FImage=AValue then Exit;
  FTile.Image.Assign(AValue);
end;

constructor TCustomuETilePanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTile:=TuETileImage.Create(Self);
  FTile.Parent:=Self;
  FTile.Align:=alClient;
  FAbout:=FTile.About;
  FImage:=FTile.Image;
end;

destructor TCustomuETilePanel.Destroy;
begin
  FreeThenNil(FTile);
  inherited Destroy;
end;

end.

