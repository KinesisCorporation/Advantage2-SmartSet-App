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

unit ECScale;
{$mode objfpc}{$H+}

//{$DEFINE DBGSCALE}  {don't remove, just comment}

interface

uses
  Classes, SysUtils, Controls, ECTypes, Graphics, 
  {$IFDEF DBGSCALE} LCLProc, {$ENDIF} LCLType, Math, Themes;

type
  {$PACKENUM 2}
  TScaleType = (estLinear, estLogarithmic);
  TTickAlign = (etaOuter, etaInner, etaCenter);
  TTickSize = (etsShort, etsMiddle, etsLong);
  TTickMetrics = record
    Pos: Integer;
    Size: TTickSize;
  end;
  TTicksVisible = (etvNone, etvLongOnly, etvLongShort, etvAll);
  TValueDisplay = (evdNormal, evdTopLeft, evdBottomRight, evdTopLeftInside,
                   evdBttmRightInside, evdCompactTopLeft, evdCompactBttmRight);
  TScaleValueFormat = (esvfAutoRound, esvfFixedRound, esvfSmartRound, esvfExponential, esvfHexadecimal,
                       esvfMarkHexadec, esvfOctal, esvfMarkOctal, esvfBinary, esvfDate, esvfTime,
                       esvfText);
									                       
const
  cDefTickAlign = etaOuter;
  cDefTickIndent = 5;
  cDefTickLength = 20;
  cDefTickVisible = etvAll;
  cDefValFormat = esvfAutoRound;
  cDefValIndent = 5;
  cDefValVisible = evvAll;
  
type  
  { TCustomECScale }
  TCustomECScale = class(TPersistent)
  private
    FMax: Double;
    FMin: Double;  
    FOnPrepareValue: TOnPrepareValue;
    FText: TStrings;
    procedure SetCorrection(AValue: Double);
    procedure SetDateTimeFormat(const AValue: string);
    procedure SetDigits(const AValue: SmallInt);
    procedure SetFontOrientation(const AValue: Integer);
    procedure SetLogarithmBase(const AValue: Double);
    procedure SetMax(const AValue: Double);
    procedure SetMin(const AValue: Double);
    procedure SetScaleType(const AValue: TScaleType);
    procedure SetText(const AValue: TStrings);
    procedure SetTickAlign(const AValue: TTickAlign);
    procedure SetTickColor(const AValue: TColor);
    procedure SetTickDesign(const AValue: TTickDesign);
    procedure SetTickIndent(const AValue: SmallInt);
    procedure SetTickLength(const AValue: SmallInt);
    procedure SetTickLongValue(const AValue: Double);
    procedure SetTickMiddleValue(const AValue: Double);
    procedure SetTickShortValue(const AValue: Double);
    procedure SetTickVisible(const AValue: TTicksVisible);
    procedure SetValueDisplay(const AValue: TValueDisplay);
    procedure SetValueFormat(const AValue: TScaleValueFormat);
    procedure SetValueIndent(const AValue: SmallInt);
    procedure SetValueShift(const AValue: SmallInt);
    procedure SetValueVisible(const AValue: TValuesVisibility);
  protected
    FCorrection: Double;
    FDateTimeFormat: string;
    FDigits: SmallInt;
    FFontOrientation: Integer;
    FLogarithmBase: Double;   
    FScaleType: TScaleType;
    FTickAlign: TTickAlign;
    FTickColor: TColor;
    FTickDesign: TTickDesign;
    FTickIndent: SmallInt;
    FTickLength: SmallInt;
    FTickLongValue: Double;
    FTickMiddleValue: Double;
    FTickShortValue: Double;
    FTickVisible: TTicksVisible;
    FValueDisplay: TValueDisplay;
    FValueFormat: TScaleValueFormat;
    FValueIndent: SmallInt;
    FValueShift: SmallInt;
    FValueVisible: TValuesVisibility;
  protected
    LengthPx, MaxL, MinL: Integer;
    Reversed: Boolean;
    Parent: TControl;
    procedure CalcTicksValuesLinear(ALength: Integer; AReversed: Boolean);
    procedure CalcTicksValuesLogarithmic(ALength: Integer; AReversed: Boolean);
    function GetStringValue(AIndex: Integer): string; overload;
    procedure Redraw; virtual;
    procedure RecalcRedraw; virtual;
  public
    DTFormat: TFormatSettings;
    OnRecalcRedraw: TObjectMethod;
    OnRedraw: TObjectMethod;
    TickArray: array of TTickMetrics;
    UpdateCount: SmallInt;
    ValueArray: array of Double;
    constructor Create(AParent: TControl);
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure CalcTickPosAndValues(ALength: Integer; AReversed: Boolean);
    procedure Draw(ACanvas: TCanvas; ATicks, AValues: Boolean; AScalePos: TObjectPos;
                ATickStart: TPoint; AExtraValues: array of Double); overload;
    procedure Draw(ACanvas: TCanvas; ATicks, AValues: Boolean;
                AScalePos: TObjectPos; AColor3DDark, AColor3DLight: TColor;
                ATickStart: TPoint; AExtraValues: array of Double);
    procedure EndUpdate;
    function GetPreferredSize(ACanvas: TCanvas; AHorizontal: Boolean;
               ATicks: Boolean = True; AValues: Boolean = True): Integer;
    function GetPreferredValuesHeight(ACanvas: TCanvas): Integer;
    function GetPreferredValuesWidth(ACanvas: TCanvas): Integer;
    function GetStringMax: string;
    function GetStringMin: string;
    function GetStringPosition(APosition: Double): string; overload;
    function GetStringPosition(APosition: Double; ARound: SmallInt): string; overload;
    function GetStringValue(AValue: Double; ARound: Integer): string; overload;
    function GetTextSize(ACanvas: TCanvas; const AStrVal: string; AHorizontal: Boolean): Integer;
    property Correction: Double read FCorrection write SetCorrection;
    property DateTimeFormat: string read FDateTimeFormat write SetDateTimeFormat;
    property Digits: SmallInt read FDigits write SetDigits default 0;
    property FontOrientation: Integer read FFontOrientation write SetFontOrientation default 0;
    property LogarithmBase: Double read FLogarithmBase write SetLogarithmBase;
    property Max: Double read FMax write SetMax;
    property Min: Double read FMin write SetMin;
    property ScaleType: TScaleType read FScaleType write SetScaleType default estLinear;
    property Text: TStrings read FText write SetText;
    property TickAlign: TTickAlign read FTickAlign write SetTickAlign default cDefTickAlign;
    property TickColor: TColor read FTickColor write SetTickColor default clDefault;
    property TickDesign: TTickDesign read FTickDesign write SetTickDesign default etdSimple;
    property TickIndent: SmallInt read FTickIndent write SetTickIndent default cDefTickIndent;
    property TickLength: SmallInt read FTickLength write SetTickLength default cDefTickLength;
    property TickLongValue: Double read FTickLongValue write SetTickLongValue;
    property TickMiddleValue: Double read FTickMiddleValue write SetTickMiddleValue;
    property TickShortValue: Double read FTickShortValue write SetTickShortValue;
    property TickVisible: TTicksVisible read FTickVisible write SetTickVisible default cDefTickVisible;
    property ValueDisplay: TValueDisplay read FValueDisplay write SetValueDisplay default evdNormal;
    property ValueFormat: TScaleValueFormat read FValueFormat write SetValueFormat default cDefValFormat;
    property ValueIndent: SmallInt read FValueIndent write SetValueIndent default cDefValIndent;
    property ValueShift: SmallInt read FValueShift write SetValueShift default 0;
    property ValueVisible: TValuesVisibility read FValueVisible write SetValueVisible default cDefValVisible;
    property OnPrepareValue: TOnPrepareValue read FOnPrepareValue write FOnPrepareValue;
  end;
  
  { TECScale }
  TECScale = class(TCustomECScale)
  published
    property DateTimeFormat;
    property Digits;
   { property FontOrientation; }
    property LogarithmBase;
    property Max;
    property Min;
    property ScaleType;
    property Text;
    property TickAlign;
    property TickColor;
    property TickDesign;
    property TickIndent;
    property TickLength;
    property TickLongValue;
    property TickMiddleValue;
    property TickShortValue;
    property TickVisible;
    property ValueDisplay;
    property ValueFormat;
    property ValueIndent;
    property ValueShift;
    property ValueVisible;
    property OnPrepareValue;
  end;            

implementation

{ TCustomECScale }

constructor TCustomECScale.Create(AParent: TControl);
begin
  inherited Create;
  Parent:=AParent;
  DTFormat:=DefaultFormatSettings;
  FCorrection:=0;
  FDateTimeFormat:='hh:nn:ss';
  DTFormat.LongTimeFormat:=FDateTimeFormat;
  FLogarithmBase:=10;
  FMin:=0;
  FMax:=100;
  FText:=TStringList.Create;
  FTickAlign:=cDefTickAlign;
  FTickColor:=clDefault;
  FTickIndent:=cDefTickIndent;
  FTickLength:=cDefTickLength;
  FTickShortValue:=1;
  FTickMiddleValue:=5;
  FTickLongValue:=10;
  FTickVisible:=cDefTickVisible;
  FValueFormat:=cDefValFormat;
  FValueIndent:=cDefValIndent;
  FValueVisible:=cDefValVisible;
end;

destructor TCustomECScale.Destroy;
begin
  FreeAndNil(FText);
  inherited Destroy;
end;

procedure TCustomECScale.BeginUpdate;
begin
  inc(UpdateCount);
end;

procedure TCustomECScale.CalcTickPosAndValues(ALength: Integer; AReversed: Boolean);
begin
  LengthPx:=ALength;
  Reversed:=AReversed;
  SetLength(TickArray, 0);
  if (TickVisible>etvNone) or (ValueVisible>evvNone) then
    case ScaleType of
      estLinear: CalcTicksValuesLinear(ALength, AReversed);
      estLogarithmic: CalcTicksValuesLogarithmic(ALength, AReversed);
    end;
end;

procedure TCustomECScale.CalcTicksValuesLinear(ALength: Integer; AReversed: Boolean);
var aDistPx, aShift, pxlsPerUnit: Double;
    aMinL, aMaxL, i, j, aPos, myPos, lLim, hLim, tMin, tMax: Integer;
    bAddedTick, bTickInserted: Boolean;

  function GetValueMax(ATickValue: Double): Integer;
  var aCorrection, aFrac, aHighest: Double;
  begin
    aHighest:=Max/ATickValue;
    aFrac:=frac(aHighest);
    aCorrection:=Correction/ATickValue;
    Result:=trunc(aHighest);
    if aFrac>(1-aCorrection) then inc(Result)
      else if aFrac<-aCorrection then dec(Result);
  end;

  function GetValueMin(ATickValue: Double): Integer;
  var aCorrection, aFrac, aLowest: Double;
  begin
    aLowest:=Min/ATickValue;
    aFrac:=frac(aLowest);
    aCorrection:=Correction/ATickValue;
    Result:=trunc(aLowest);
    if aFrac>aCorrection then
      begin
        inc(Result);
        aShift:=1-abs(aFrac);
      end else
      begin
        if aFrac<(-1+aCorrection) then
          begin
            dec(Result);
            aShift:=0;
          end else
          aShift:=abs(aFrac);
      end;
  end;

begin
  pxlsPerUnit:=(ALength-0.99)/(Max-Min);
  aDistPx:=TickShortValue*pxlsPerUnit;  { distance between Short Ticks }
  if (TickVisible>etvLongOnly) and (aDistPx>0.5) then
    begin  { Calc. Small Ticks }
      tMin:=GetValueMin(TickShortValue);
      tMax:=GetValueMax(TickShortValue);
      SetLength(TickArray, tMax-tMin+1);
      for i:=0 to tMax-tMin do
        begin
          TickArray[i].Pos:=round((aShift+i)*aDistPx);
          TickArray[i].Size:=etsShort;
        end;
    end;
  bAddedTick:=False;
  aDistPx:=TickMiddleValue*pxlsPerUnit;  { distance between Middle Ticks }
  if (TickVisible=etvAll) and (aDistPx>0.5) then
    begin  { Calc. Middle Ticks }
      lLim:=0;
      hLim:=trunc(TickMiddleValue/TickShortValue);
      tMin:=GetValueMin(TickMiddleValue);
      tMax:=GetValueMax(TickMiddleValue);
      for i:=0 to tMax-tMin do
        begin
          aPos:=trunc((aShift+i)*aDistPx);
          bTickInserted:=False;
          for j:=lLim to high(TickArray) do
            begin
              if (TickArray[j].Pos=aPos) or (TickArray[j].Pos=(aPos+1)) then
                begin
                  TickArray[j].Size:=etsMiddle;
                  bTickInserted:=True;
                  lLim:=j+hLim;
                  break;
                end;
            end;
          if not bTickInserted then
            begin
              SetLength(TickArray, high(TickArray)+2);
              TickArray[high(TickArray)].Pos:=aPos;
              TickArray[high(TickArray)].Size:=etsMiddle;
              bAddedTick:=True;
            end;
        end;
    end;
  { Calc. Long Ticks }
  aDistPx:=TickLongValue*pxlsPerUnit;  { distance between Long Ticks }
  if aDistPx>0.5 then
    begin
      aMinL:=high(Integer);
      aMaxL:=low(Integer);
      lLim:=0;
      hLim:=trunc(TickLongValue/TickShortValue);
      tMin:=GetValueMin(TickLongValue);
      tMax:=GetValueMax(TickLongValue);
      if (tMax-tMin)>=0 then
        for i:=0 to tMax-tMin do
          begin
            aPos:=trunc((aShift+i)*aDistPx);
            bTickInserted:=False;
            for j:=lLim to high(TickArray) do
              begin
                if (TickArray[j].Pos=aPos) or (TickArray[j].Pos=(aPos+1)) then
                 begin
                   myPos:=TickArray[j].Pos;
                   TickArray[j].Size:=etsLong;
                   bTickInserted:=True;
                   if not bAddedTick then lLim:=j+hLim;  { ensure to search whole array when at least one tick was added }
                   break;
                 end;
              end;
            if not bTickInserted then
              begin
                SetLength(TickArray, high(TickArray)+2);
                TickArray[high(TickArray)].Pos:=aPos;
                TickArray[high(TickArray)].Size:=etsLong;
                myPos:=aPos;
              end;
            if myPos<aMinL then aMinL:=myPos;
            if myPos>aMaxL then aMaxL:=myPos;
          end
        else
        begin
          aMinL:=0;
          aMaxL:=0;
        end;
    end else
    begin
      tMin:=0;
      tMax:=0;
    end;
  if not AReversed then
    begin
      MaxL:=aMaxL;
      MinL:=aMinL;
    end else
    begin
      MaxL:=LengthPx-aMinL;
      MinL:=LengthPx-aMaxL;
    end;
  if ValueVisible>evvNone then
    begin
      SetLength(ValueArray, tMax-tMin+1);
      for i:=0 to tMax-tMin do
        ValueArray[i]:=TickLongValue*(i+tMin);
    end;
end;

procedure TCustomECScale.CalcTicksValuesLogarithmic(ALength: Integer; AReversed: Boolean);
var aHelp, aMin, aMax, fracMin, pxlsPerUnit: Double;
    aMinHasFrac: SmallInt;
    aHiTick, aMiddlePos, i, j, k, aLogBaseMinus1, lowTicks, hiTicks, truncMin, truncMax: Integer;
    bMiddleMark: Boolean;
const caTick: array[False..True] of TTickSize = (etsShort, etsMiddle);
begin
  aLogBaseMinus1:=trunc(LogarithmBase)-1;
  aMin:=Min;
  aMax:=Max;
  if aMin>0 
    then aMin:=logn(LogarithmBase, aMin) 
    else aMin:=0;
  truncMin:=trunc(AMin);
  fracMin:=frac(AMin);
  if fracMin<0.0 then dec(truncMin);
  if aMax>0 
    then aMax:=logn(LogarithmBase, aMax) 
    else aMax:=0;
  truncMax:=trunc(AMax);
  if frac(aMax)<0.0 then dec(truncMax);
  if aMin<aMax then
    begin
      pxlsPerUnit:=(ALength-0.99)/(aMax-aMin);
      if abs(fracMin)>0.0 
        then aMinHasFrac:=1 
        else aMinHasFrac:=0; 
      if TickVisible>etvLongOnly then
        begin  { All Ticks }
          hiTicks:=trunc(Max/power(LogarithmBase, truncMax));
          aHelp:=Min/power(LogarithmBase, truncMin);
          lowTicks:=trunc(LogarithmBase)-trunc(aHelp);
          if frac(aHelp)>0.0 then dec(lowTicks);
          if lowTicks>=aLogBaseMinus1 then lowTicks:=0;
          j:=truncMax-truncMin;
          if (lowTicks>0) or (frac(aHelp)>0.0) then dec(j);
          if j>=0 then
            begin
              SetLength(TickArray, aLogBaseMinus1*j+lowTicks+hiTicks);
              aHiTick:=aLogBaseMinus1;
            end else
            begin
              if frac(AHelp)=0.0 then j:=1 else j:=0;
              SetLength(TickArray, hiTicks+j-trunc(aHelp));
              aHiTick:=hiTicks;
            end;
          SetLength(ValueArray, truncMax-truncMin+1-aMinHasFrac);
          aHelp:=fracMin;
          if aHelp<0 then aHelp:=1+aHelp;
          bMiddleMark:=(TickVisible=etvAll) and (frac(LogarithmBase)=0.0) and (trunc(LogarithmBase) mod 2 =0);
          aMiddlePos:=trunc(LogarithmBase) div 2;
          if aMinHasFrac=1 then
            begin  { Small ticks in front of the very first long tick }
              lowTicks:=trunc(LogarithmBase)-lowTicks;
              for j:=lowTicks to aHiTick do
                begin
                  k:=j-lowTicks;
                  TickArray[k].Pos:=round((-aHelp+logn(LogarithmBase, j))*pxlsPerUnit);
                  TickArray[k].Size:=caTick[bMiddleMark and (j=aMiddlePos)];
                end;
             end;
          i:=0;
          if lowTicks>0 then lowTicks:=trunc(LogarithmBase)-lowTicks;   
          while i<(truncMax-truncMin-aMinHasFrac) do
            begin
              TickArray[lowTicks+i*aLogBaseMinus1].Pos:=round((abs(aMinHasFrac-aHelp)+i)*pxlsPerUnit);
              TickArray[lowTicks+i*aLogBaseMinus1].Size:=etsLong;
              for j:=2 to aLogBaseMinus1 do  { in the middle }
                begin
                  k:=lowTicks+i*aLogBaseMinus1+j-1;
                  TickArray[k].Pos:=TickArray[lowTicks+i*aLogBaseMinus1].Pos+round(logn(LogarithmBase, j)*pxlsPerUnit);
                  TickArray[k].Size:=caTick[bMiddleMark and (j=aMiddlePos)];
                end;
              inc(i);
            end;
          if length(ValueArray)>0 then
            begin
              TickArray[lowTicks+i*aLogBaseMinus1].Pos:=round((abs(aMinHasFrac-aHelp)+i)*pxlsPerUnit);
              TickArray[lowTicks+i*aLogBaseMinus1].Size:=etsLong;
              for j:=2 to hiTicks do  { in the end }
                begin
                  k:=lowTicks+i*aLogBaseMinus1+j-1;
                  TickArray[k].Pos:=TickArray[lowTicks+i*aLogBaseMinus1].Pos+round(logn(LogarithmBase, j)*pxlsPerUnit);
                  TickArray[k].Size:=caTick[bMiddleMark and (j=aMiddlePos)];
                end;
              MinL:=TickArray[lowTicks].Pos;
              MaxL:=TickArray[lowTicks+i*aLogBaseMinus1].Pos;
            end;
        end else
        begin  { LongTicks only }
          aHelp:=Min/power(LogarithmBase, truncMin);
          j:=truncMax-truncMin;
          SetLength(ValueArray, j+1-aMinHasFrac);
          if j>=0 
            then SetLength(TickArray, j+1-aMinHasFrac)
            else
            begin
              if frac(AHelp)=0.0 
                then j:=1 
                else j:=0;
              SetLength(TickArray, j-trunc(aHelp));
            end;
          aHelp:=fracMin;
          if aHelp<0 then aHelp:=1+aHelp;
          i:=0;  
          k:=truncMax-truncMin-aMinHasFrac;
          while i<=k do
            begin
              TickArray[i].Pos:=round((abs(aMinHasFrac-aHelp)+i)*pxlsPerUnit);
              TickArray[i].Size:=etsLong;
              inc(i);
            end;
          MinL:=TickArray[0].Pos;
          dec(i);
          MaxL:=TickArray[i].Pos;
        end;
      if not AReversed then
        for i:=0 to high(ValueArray) do
          ValueArray[i]:=power(LogarithmBase, truncMin+i+aMinHasFrac)
        else
        begin
          j:=high(ValueArray);
          for i:=0 to j do
            ValueArray[i]:=power(LogarithmBase, truncMin+i+aMinHasFrac);
        end;
    end;
end;

procedure TCustomECScale.Draw(ACanvas: TCanvas; ATicks, AValues: Boolean; 
  AScalePos: TObjectPos; ATickStart: TPoint; AExtraValues: array of Double);
begin
  Draw(ACanvas, ATicks, AValues, AScalePos, clDefault, clDefault, ATickStart, AExtraValues);  
end; 

procedure TCustomECScale.Draw(ACanvas: TCanvas; ATicks, AValues: Boolean; AScalePos: TObjectPos; 
  AColor3DDark, AColor3DLight: TColor; ATickStart: TPoint; AExtraValues: array of Double);
var aTLStart, aTLEnd, aTMStart, aTMEnd, aTSStart, aTSEnd: Integer; AStart: Integer;
    {$IFDEF DBGSCALE} td: TDateTime; {$ENDIF}

  procedure CalcTickPoints(AEdge, ARight: Integer);
  var aTLSize, aTMSize, aTSSize: Integer;
  begin  { Calculates Start end End points of Ticks }
    aTLSize:=TickLength;
    if FValueDisplay<=evdBottomRight then
      begin  { evdNormal, evdTopLeft, evdBottomRight }
        aTMSize:=round(aTLSize*0.7);
        aTSSize:=round(aTLSize*0.4);
      end else
      begin  { evdTopLeftInside, evdBttmRightInside, evdCompactTopLeft, evdCompactBttmRight }
        aTMSize:=round(aTLSize*0.5);
        aTSSize:=round(aTLSize*0.3);
      end;
    aTLEnd:=AEdge+ARight*TickIndent;
    aTLStart:=aTLEnd+ARight*aTLSize;
    case TickAlign of
      etaOuter:
        begin
          aTMStart:=aTLStart;
          aTSStart:=aTLStart;
          aTMEnd:=aTMStart-ARight*aTMSize;
          aTSEnd:=aTSStart-ARight*aTSSize;
        end;
      etaInner:
        begin
          aTMEnd:=aTLEnd;
          aTSEnd:=aTLEnd;
          aTMStart:=aTMEnd+ARight*aTMSize;
          aTSStart:=aTSEnd+ARight*aTSSize;
        end;
      etaCenter:
        begin
          aTMEnd:=aTLEnd+ARight*(aTLSize-aTMSize) div 2;
          aTMStart:=aTMEnd+ARight*aTMSize;
          aTSEnd:=aTLEnd+ARight*(aTLSize-aTSSize) div 2;
          aTSStart:=aTSEnd+ARight*aTSSize;
        end;
    end;
  end;

  procedure DrawTicks;
  var aTickColor: TColor;

    procedure nDrawTicks(AOffSet: Integer);
    var aPos, i: Integer;
    begin
      if not Reversed then AOffSet:=AStart+AOffSet
        else AOffSet:=LengthPx+AOffSet+AStart-1;
      if AScalePos in [eopTop, eopBottom] then
        begin
          for i:=0 to high(TickArray) do
            begin
              if not Reversed then aPos:=AOffSet+TickArray[i].Pos
                else aPos:=AOffSet-TickArray[i].Pos;;
              case TickArray[i].Size of
                etsShort: ACanvas.Line(aPos, aTSStart, aPos, aTSEnd);
                etsMiddle: ACanvas.Line(aPos, aTMStart, aPos, aTMEnd);
                etsLong: ACanvas.Line(aPos, aTLStart, aPos, aTLEnd);
              end;
            end;
        end else
        begin
          for i:=0 to high(TickArray) do
            begin
              if not Reversed then aPos:=AOffSet+TickArray[i].Pos
                else aPos:=AOffSet-TickArray[i].Pos;
              case TickArray[i].Size of
                etsShort: ACanvas.Line(aTSStart, aPos, aTSEnd, aPos);
                etsMiddle: ACanvas.Line(aTMStart, aPos, aTMEnd, aPos);
                etsLong: ACanvas.Line(aTLStart, aPos, aTLEnd, aPos);
              end;
            end;
        end;
    end;

  begin
    with ACanvas do
      begin
        Pen.Style:=psSolid;
        aTickColor:=GetColorResolvingDefault(TickColor, clBtnText);
        if not Parent.IsEnabled then
          if TickDesign<etd3DLowered then
            begin
              aTickColor:=GetMergedColor(aTickColor, Pixels[Width div 2, Height div 2], 0.45);
            end else
            begin
              AColor3DDark:=GetMergedColor(AColor3DDark, Pixels[Width div 2, Height div 2], 0.67);
              AColor3DLight:=GetMergedColor(AColor3DLight, Pixels[Width div 2, Height div 2], 0.67);
            end;
        case TickDesign of
          etdSimple:
            begin
              Pen.Color:=aTickColor;
              nDrawTicks(0);
            end;
          etdThick:
            begin
              Pen.Color:=aTickColor;
              nDrawTicks(-1);
              nDrawTicks(0);
            end;
          etd3DLowered:
            begin
              Pen.Color:=AColor3DLight;
              nDrawTicks(1);
              Pen.Color:=AColor3DDark;
              nDrawTicks(0);
            end;
          etd3DRaised:
            begin
              Pen.Color:=AColor3DLight;
              nDrawTicks(-1);
              Pen.Color:=AColor3DDark;
              nDrawTicks(0);
            end;
        end;
      end;
  end;

  procedure DrawValues;  { ARight: -1..TopLeft, 1.. BottomRight }
  var i, lV, valueTL, valueBR: Integer;
      aDetails: TThemedElementDetails;
      aFontColor: TColor;
      cosPi1800, sinPi1800, lHelp: Double;
      strVal: string;
      aRect: TRect;
      aValueVisible: TValuesVisibility;

    function GetTopLeftPos: Integer;
    begin
      with ACanvas do
        if AScalePos in [eopLeft, eopRight] 
          then Result:=round(cosPi1800*TextWidth(strVal)+sinPi1800*TextHeight(strVal))
          else Result:=round(sinPi1800*TextWidth(strVal)+cosPi1800*TextHeight(strVal));
    end;

    function GetValuePos: Integer;
    var c: Char;
        h: SmallInt;
    begin
      with ACanvas do
        if AScalePos in [eopLeft, eopRight] then
          case ValueDisplay of  { Vertical }
            evdNormal: Result:=-round(cosPi1800*(TextHeight(strVal) div 2)+sinPi1800*(TextWidth(strVal) div 2));
            evdTopLeft, evdTopLeftInside: Result:=-round(cosPi1800*TextHeight(strVal)+sinPi1800*TextWidth(strVal)); 
            evdCompactTopLeft: 
              begin
                Result:=0;
                h:=TextHeight('9');
                for c in strVal do
                  if not (c=FormatSettings.DecimalSeparator)
                    then dec(Result, h)
                    else dec(Result, h div 2);
                inc(Result, length(strVal));
              end;
            otherwise Result:=1;
          end else
          case ValueDisplay of  { Horizontal }
            evdNormal: Result:=-round(sinPi1800*(TextHeight(strVal) div 2)+cosPi1800*(TextWidth(strVal) div 2));
            evdTopLeft, evdTopLeftInside, evdCompactTopLeft: Result:=-round(sinPi1800*TextHeight(strVal)+cosPi1800*TextWidth(strVal)); 
            otherwise Result:=1;
          end;
    end;

    procedure DrawValue;
    var aFlags: Cardinal;
        aValueShift: Integer;
    
      procedure nDrawValue(AValue: Integer);
      begin
        if AScalePos in [eopLeft, eopRight] then aRect.Left:=AValue else aRect.Top:=AValue;
        ThemeServices.DrawText(ACanvas, aDetails, strVal, aRect, aFlags, 0);
      end;

      procedure nDrawValueSplitted(AValue: Integer);  { for evdCompactTopLeft & evdCompactBttmRight }
      var c: Char;
          h: Integer;
      begin
        aRect.Left:=AValue;
        h:=ACanvas.TextHeight('9')-1;
        for c in strVal do
          begin
            if c=FormatSettings.DecimalSeparator then dec(aRect.Top, h div 2);    
            ThemeServices.DrawText(ACanvas, aDetails, c, aRect, aFlags, 0);
            inc(aRect.Top, h);
          end;
      end;

    begin
      if ValueDisplay in [evdTopLeft, evdTopLeftInside, evdCompactTopLeft] 
        then aValueShift:=-ValueShift
        else aValueShift:=ValueShift;
      aFlags:=DT_NOPREFIX or DT_SINGLELINE;
      case AScalePos of
        eopTop:
          begin
            aRect.Left:=lV+aValueShift;
            nDrawValue(valueTL-GetTopLeftPos);
          end;
        eopRight:
          begin
            aRect.Top:=lV+aValueShift;
            if ValueDisplay<evdCompactTopLeft 
              then nDrawValue(valueBR)
              else nDrawValueSplitted(valueBR);
          end;
        eopBottom:
          begin
            aRect.Left:=lV+aValueShift;
            nDrawValue(valueBR)
          end;
        eopLeft:
          begin
            aRect.Top:=lV+aValueShift;
            if ValueDisplay<evdCompactTopLeft 
              then nDrawValue(valueTL-GetTopLeftPos)
              else nDrawValueSplitted(valueTL);
          end;
      end;
    end;

  begin
    aDetails:=ThemeServices.GetElementDetails(caThemedContent[caItemState[Parent.IsEnabled]]);    
    aRect.Right:=ACanvas.Width;
    aRect.Bottom:=ACanvas.Height;
    aValueVisible:=ValueVisible;
    case AScalePos of
      eopTop:
        if ATicks then
          case ValueDisplay of
            evdNormal, evdTopLeft, evdBottomRight: valueTL:=aTLStart-ValueIndent+2;
            otherwise
              if TickAlign=etaInner 
                then valueTL:=aTLStart-ValueIndent+ACanvas.TextHeight('1,9')-1
                else valueTL:=aTLStart-ValueIndent+2;
          end else 
          valueTL:=ATickStart.Y-ValueIndent;
      eopRight:
        if ATicks then
          case ValueDisplay of
            evdNormal, evdTopLeft, evdBottomRight: valueBR:=aTLStart+ValueIndent+1;
            otherwise
              case TickAlign of
                etaOuter: valueBR:=aTLStart+ValueIndent+1;  
                etaInner: valueBR:=aTLStart+ValueIndent-ACanvas.TextWidth('9')+1;
                etaCenter: valueBR:=aTLStart+ValueIndent-ACanvas.TextWidth('9') div 2 +1;
              end;
          end else 
          valueBR:=ATickStart.X+ValueIndent;
      eopBottom:
        if ATicks then
          case ValueDisplay of
            evdNormal, evdTopLeft, evdBottomRight: valueBR:=aTLStart+ValueIndent;      
            otherwise
              if TickAlign=etaInner 
                then valueBR:=aTLStart+ValueIndent-ACanvas.TextHeight('1,9')+3
                else valueBR:=aTLStart+ValueIndent;  
          end else 
          valueBR:=ATickStart.Y+ValueIndent;
      eopLeft:
        if ATicks then
          case ValueDisplay of
            evdNormal, evdTopLeft, evdBottomRight: valueTL:=aTLStart-ValueIndent;
            evdTopLeftInside, evdBttmRightInside:
              case TickAlign of
                etaOuter: valueTL:=aTLStart-ValueIndent;  
                etaInner: valueTL:=aTLStart-ValueIndent+ACanvas.TextWidth('9');
                etaCenter: valueTL:=aTLStart-ValueIndent+ACanvas.TextWidth('9') div 2;
              end; 
            otherwise
              case TickAlign of
                etaOuter: valueTL:=aTLStart-ValueIndent-ACanvas.TextWidth('9');  
                etaInner: valueTL:=aTLStart-ValueIndent;
                etaCenter: valueTL:=aTLStart-ValueIndent-ACanvas.TextWidth('9') div 2;
              end; 
          end else 
          valueTL:=ATickStart.X-ValueIndent;
    end;
    with ACanvas do
      begin
        Font.Orientation:=FontOrientation;
        aFontColor:=GetColorResolvingDefault(Font.Color, clBtnText);
        if TickDesign>=etd3DLowered then 
          Font.Color:=GetMergedColor(aFontColor, Pixels[Width div 2, height div 2], 0.75);
        Brush.Style:=bsClear;
        cosPi1800:=abs(cos(pi_1800*FFontOrientation));
        sinPi1800:=abs(sin(pi_1800*FFontOrientation));
        if aValueVisible>evvBounds then
          begin
            i:=MaxL-MinL;
            if i>0
              then lHelp:=i/high(ValueArray)
              else lHelp:=0;
            for i:=0 to high(ValueArray) do
              begin
                strVal:=GetStringValue(i);
                if not Reversed 
                  then lV:=MinL+round(i*lHelp)
                  else lV:=MaxL-round(i*lHelp);
                lV:=AStart+lV+GetValuePos;
                DrawValue;
              end;
          end;
        if aValueVisible in [evvBounds, evvAll] then
          begin
            if (aValueVisible=evvBounds) or (ValueArray=nil) or (Min<>ValueArray[0]) then
              begin
                strVal:=GetStringMin;
                lV:=AStart+GetValuePos;
                if Reversed then lV:=lV+LengthPx;
                DrawValue;
              end;
            if (aValueVisible=evvBounds) or (ValueArray=nil) or (Max<>ValueArray[high(ValueArray)]) then
              begin
                strVal:=GetStringMax;
                lV:=AStart+GetValuePos;
                if not Reversed then lV:=lV+LengthPx;
                DrawValue;
              end;
            for i:=0 to high(AExtraValues) do
              if (aValueVisible=evvBounds) and (AExtraValues[i]>Min) and (AExtraValues[i]<Max) then
                begin
                  strVal:=GetStringValue(AExtraValues[i], FDigits);
                  lV:=round((AExtraValues[i]-Min)*LengthPx/(Max-Min));
                  if not Reversed 
                    then lV:=AStart+lV+GetValuePos
                    else lV:=AStart+LengthPx-lV+GetValuePos;                    
                  DrawValue;
                end;
          end;
      end;
  end;

begin
  {$IFDEF DBGSCALE} td:=Now; {$ENDIF}
  case AScalePos of
    eopTop:
      begin
        CalcTickPoints(ATickStart.Y, -1);
        AStart:=ATickStart.X;
      end;
    eopRight:
      begin
        CalcTickPoints(ATickStart.X, 1);
        AStart:=ATickStart.Y;
      end;
    eopBottom:
      begin
        CalcTickPoints(ATickStart.Y, 1);
        AStart:=ATickStart.X;
      end;
    eopLeft:
      begin
        CalcTickPoints(ATickStart.X, -1);
        AStart:=ATickStart.Y;
      end;
  end;
  ATicks:=ATicks and (TickVisible>etvNone);
  if ATicks then 
    begin
      if TickDesign>=etd3DLowered then
        begin
          AColor3DDark:=GetColorResolvingDefault(AColor3DDark, clBtnShadow);
          AColor3DLight:=GetColorResolvingDefault(AColor3DLight, clBtnHilight);
        end;
      DrawTicks;
    end;
  if AValues and (ValueVisible>evvNone) then DrawValues;
  {$IFDEF DBGSCALE} DebugLn('Scale.Draw '+floattostr((now-td)*24*60*60*1000)); {$ENDIF}
end;

procedure TCustomECScale.EndUpdate;
begin
  dec(UpdateCount);
  if UpdateCount=0 then RecalcRedraw;
end;  

function TCustomECScale.GetPreferredSize(ACanvas: TCanvas; AHorizontal: Boolean; 
  ATicks: Boolean = True; AValues: Boolean = True): Integer;
var bValues: Boolean;
begin
  Result:=0;
  if ATicks or AValues then
    begin
      if ATicks and (TickVisible>etvNone) then inc(Result,TickIndent+TickLength);
      bValues:= (AValues and (ValueVisible>evvNone));
      if AHorizontal then 
        begin  { Horizontal } 
          if bValues then inc(Result, GetPreferredValuesHeight(ACanvas))
        end else
        begin  { Vertical }
          if bValues then inc(Result, GetPreferredValuesWidth(ACanvas))
        end;
    end;
end;

function TCustomECScale.GetPreferredValuesHeight(ACanvas: TCanvas): Integer;
begin  
  Result:=ValueIndent; 
  if ValueDisplay<=evdBottomRight 
    then inc(Result, Math.max(GetTextSize(ACanvas, GetStringMax, True), 
                              GetTextSize(ACanvas, GetStringMin, True)))
    else if TickAlign<>etaInner then inc(Result, ACanvas.TextHeight(',9')-2);
end;  
 
function TCustomECScale.GetPreferredValuesWidth(ACanvas: TCanvas): Integer;
var aWidth: Integer;
begin
  Result:=ValueIndent+2;
  if ValueDisplay<=evdBttmRightInside 
    then aWidth:=(Math.max(GetTextSize(ACanvas, GetStringMin, False),
                           GetTextSize(ACanvas, GetStringMax, False))-2);  { -2 is correction }
  case ValueDisplay of
    evdNormal, evdTopLeft, evdBottomRight: inc(Result, aWidth);                           
    evdTopLeftInside, evdBttmRightInside:
      begin
       inc(Result, aWidth);
        case TickAlign of
          etaInner: dec(Result, ACanvas.TextWidth('9'));
          etaCenter: dec(Result, ACanvas.TextWidth('9') div 2);
        end;           
      end;
    evdCompactTopLeft, evdCompactBttmRight:
      begin
        case TickAlign of
          etaOuter: inc(Result, ACanvas.TextWidth('9'));
          etaCenter: inc(Result, ACanvas.TextWidth('9') div 2);
        end;
      end;
  end;   
end;

function TCustomECScale.GetStringMax: string;
begin
  Result:='';
  if ValueFormat<>esvfText 
    then Result:=GetStringValue(Max, Digits)
    else if FText.Count>0 then Result:=FText[FText.Count-1];
end;

function TCustomECScale.GetStringMin: string;
begin
  Result:='';
  if ValueFormat<>esvfText 
    then Result:=GetStringValue(Min, Digits)
    else if FText.Count>0 then Result:=FText[0];
end;

function TCustomECScale.GetStringPosition(APosition: Double): string; 
begin
  Result:=GetStringPosition(APosition, Digits);
end;

function TCustomECScale.GetStringPosition(APosition: Double; ARound: SmallInt): string;
var i: Integer;
begin
  if ValueFormat<>esvfText then
    begin
      if ScaleType=estLogarithmic 
        then APosition:=LinearToLogarithmic(APosition, Min, Max, LogarithmBase);
      Result:=GetStringValue(APosition, ARound);
    end else
    begin
      i:=round((APosition-Min)/TickLongValue);
      if (i>=0) and (i<Text.Count)    
        then Result:=FText.Strings[i]
        else Result:='';
    end;         
end;   

function TCustomECScale.GetStringValue(AValue: Double; ARound: Integer): string;
begin
  if assigned(FOnPrepareValue) then FOnPrepareValue(self, AValue);
  case ValueFormat of
    esvfAutoRound: if ARound<0 
                     then Result:=floattostr(AValue)
                     else Result:=floattostrF(AValue, ffFixed, 1, ARound);
    esvfFixedRound: Result:=floattostrF(AValue, ffFixed, 1, ARound); 
    esvfSmartRound: 
      begin
        if abs(AValue)>=10 then dec(ARound, trunc(log10(abs(AValue))));
        if (ARound<0) or (frac(AValue)=0) then ARound:=0;
        Result:=floattostrF(AValue, ffFixed, 1, ARound);        
      end;
    esvfExponential: Result:=floattostrF(AValue, ffExponent, ARound, ARound);
    esvfHexadecimal: Result:=hexStr(round(AValue), ARound);
    esvfMarkHexadec: Result:='$'+hexStr(round(AValue), ARound);
    esvfOctal: Result:=octStr(round(AValue), ARound);
    esvfMarkOctal: Result:='&'+octStr(round(AValue), ARound);
    esvfBinary: Result:=binStr(round(AValue), ARound);
    esvfDate: Result:=datetostr(AValue, DTFormat);
    esvfTime: Result:=timetostr(AValue, DTFormat);
  end;
end;

function TCustomECScale.GetStringValue(AIndex: Integer): string;
begin
  Result:='';
  if ValueFormat<>esvfText 
    then Result:=GetStringValue(ValueArray[AIndex], Digits)
    else if (AIndex>=0) and (AIndex<FText.Count) then Result:=FText.Strings[AIndex];
end;

function TCustomECScale.GetTextSize(ACanvas: TCanvas; const AStrVal: string;
  AHorizontal: Boolean): Integer;
var cosPi1800, sinPi1800, aHelp: Double;
begin
  aHelp:=pi_1800*FontOrientation;
  cosPi1800:=abs(cos(aHelp));
  sinPi1800:=abs(sin(aHelp));
  with ACanvas do
    begin
      if AHorizontal  
        then Result:=round(sinPi1800*TextWidth(AStrVal)+cosPi1800*(TextHeight(AStrVal)-2))
        else Result:=round(cosPi1800*TextWidth(AStrVal)+sinPi1800*TextHeight(AStrVal));           
    end;
end;       

procedure TCustomECScale.Redraw;
begin
  if (UpdateCount=0) and assigned(OnRedraw) then OnRedraw;
end;

procedure TCustomECScale.RecalcRedraw;
begin
  if (UpdateCount=0) and assigned(OnRecalcRedraw) then OnRecalcRedraw;
end;  

{ TCustomECScale.Setters }

procedure TCustomECScale.SetCorrection(AValue: Double);
begin
  if FCorrection = AValue then exit;
  FCorrection := AValue;
  if ((TickVisible>etvNone) or (ValueVisible>evvNone)) and (ScaleType=estLinear) then RecalcRedraw;         
end; 

procedure TCustomECScale.SetDateTimeFormat(const AValue: string);
begin
  if FDateTimeFormat=AValue then exit;
  FDateTimeFormat:=AValue;
  DTFormat.LongDateFormat:=AValue;
  DTFormat.LongTimeFormat:=AValue;
  if (ValueVisible>evvNone) and (ValueFormat in [esvfDate, esvfTime]) then RecalcRedraw;
end;

procedure TCustomECScale.SetDigits(const AValue: SmallInt);
begin
  if FDigits=AValue then exit;
  FDigits:=AValue;
  if ValueVisible>evvNone then RecalcRedraw;
end;

procedure TCustomECScale.SetFontOrientation(const AValue: Integer);
begin
  if FFontOrientation=AValue then exit;
  {FFontOrientation:=AValue;}  //seems Qt/GTK2/Win32 works differently now
  if ValueVisible>evvNone then RecalcRedraw;
end;

procedure TCustomECScale.SetLogarithmBase(const AValue: Double);
begin
  if FLogarithmBase=AValue then exit;
  FLogarithmBase:=AValue;
  if ((TickVisible>etvNone) or (ValueVisible>evvNone))
    and (ScaleType=estLogarithmic) then RecalcRedraw;
end;

procedure TCustomECScale.SetMax(const AValue: Double);
var bUpdating: Boolean;
begin
  if FMax=AValue then exit;
  bUpdating:= UpdateCount>0;
  if (Min<AValue) or bUpdating or (csLoading in Parent.ComponentState) then
    begin
      FMax:=AValue;
      if not bUpdating then RecalcRedraw;
    end;
end;

procedure TCustomECScale.SetMin(const AValue: Double);
var bUpdating: Boolean;
begin
  if FMin=AValue then exit;
  bUpdating:= UpdateCount>0;
  if (AValue<Max) or bUpdating or (csLoading in Parent.ComponentState) then
    begin
      FMin:=AValue;
      if not bUpdating then RecalcRedraw;
    end;
end;

procedure TCustomECScale.SetScaleType(const AValue: TScaleType);
begin
  if FScaleType=AValue then exit;
  FScaleType:=AValue;
  if (TickVisible>etvNone) or (ValueVisible>evvNone) then RecalcRedraw;
end;

procedure TCustomECScale.SetText(const AValue: TStrings);
begin
  if FText=AValue then exit;
  FText.Assign(AValue);
end;

procedure TCustomECScale.SetTickAlign(const AValue: TTickAlign);
begin
  if FTickAlign=AValue then exit;
  FTickAlign:=AValue;
  if TickVisible>etvNone then RecalcRedraw;
end;

procedure TCustomECScale.SetTickColor(const AValue: TColor);
begin
  if FTickColor=AValue then exit;
  FTickColor:=AValue;
  if TickVisible>etvNone then Redraw;
end;

procedure TCustomECScale.SetTickDesign(const AValue: TTickDesign);
begin
  if FTickDesign=AValue then exit;
  FTickDesign:=AValue;
  if TickVisible>etvNone then Redraw;
end;

procedure TCustomECScale.SetTickIndent(const AValue: SmallInt);
begin
  if FTickIndent=AValue then exit;
  FTickIndent:=AValue;
  RecalcRedraw;   
end;

procedure TCustomECScale.SetTickLength(const AValue: SmallInt);
begin
  if FTickLength=AValue then exit;
  FTickLength:=AValue;
  if TickVisible>etvNone then RecalcRedraw;
end;

procedure TCustomECScale.SetTickLongValue(const AValue: Double);
begin
  if (AValue<=0) or (FTickLongValue=AValue) then exit;
  FTickLongValue:=AValue;
  if TickVisible>etvNone then RecalcRedraw;
end;

procedure TCustomECScale.SetTickMiddleValue(const AValue: Double);
begin
  if (AValue<=0) or (FTickMiddleValue=AValue) then exit;
  FTickMiddleValue:=AValue;
  if TickVisible=etvAll then RecalcRedraw;
end;

procedure TCustomECScale.SetTickShortValue(const AValue: Double);
begin
  if (AValue<=0) or (FTickShortValue=AValue) then exit;
  FTickShortValue:=AValue;
  if TickVisible>=etvLongShort then RecalcRedraw;
end;

procedure TCustomECScale.SetTickVisible(const AValue: TTicksVisible);
begin
  if FTickVisible=AValue then exit;
  FTickVisible:=AValue;
  RecalcRedraw;
end;

procedure TCustomECScale.SetValueDisplay(const AValue: TValueDisplay);
begin
  if FValueDisplay=AValue then exit;
  FValueDisplay:=AValue;
  if ValueVisible>evvNone then RecalcRedraw;
end;

procedure TCustomECScale.SetValueFormat(const AValue: TScaleValueFormat);
begin
  if FValueFormat=AValue then exit;
  FValueFormat:=AValue;
  if ValueVisible>evvNone 
    then RecalcRedraw
    else Redraw;  
end;

procedure TCustomECScale.SetValueIndent(const AValue: SmallInt);
begin
  if FValueIndent=AValue then exit;
  FValueIndent:=AValue;
  if ValueVisible>evvNone then RecalcRedraw;
end;

procedure TCustomECScale.SetValueShift(const AValue: SmallInt);
begin
  if FValueShift=AValue then exit;
  FValueShift:=AValue;
  if ValueVisible>evvNone then RecalcRedraw;     
end;

procedure TCustomECScale.SetValueVisible(const AValue: TValuesVisibility);
begin
  if FValueVisible=AValue then exit;
  FValueVisible:=AValue;
  RecalcRedraw;
end;

end.


