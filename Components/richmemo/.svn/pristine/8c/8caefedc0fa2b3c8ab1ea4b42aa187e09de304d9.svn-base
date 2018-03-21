{
 win32richmemoproc.pas 
 
 Author: Dmitry 'skalogryz' Boyarintsev 

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

unit Win32RichMemoProc; 

{$mode objfpc}{$H+}

interface

uses
  // windows units
  Windows,richedit, 
  // RTL units  
  Classes, SysUtils, 
  // LCL units
  Graphics,
  // RichMemoUnits
  WSRichMemo, 
  // Win32 widgetset units  
  win32proc; 
  
type
  { TRichEditManager }

  TRichEditManager = class(TObject)
  public
    class function SetSelectedTextStyle(RichEditWnd: Handle; Params: TIntFontParams): Boolean; virtual;
    class function GetSelectedTextStyle(RichEditWnd: Handle; var Params: TIntFontParams): Boolean; virtual;
    class function GetStyleRange(RichEditWnd: Handle; TextStart: Integer; var RangeStart, RangeLen: Integer): Boolean; virtual; 
    class procedure GetSelection(RichEditWnd: Handle; var TextStart, TextLen: Integer); virtual;      
    class procedure SetSelection(RichEditWnd: Handle; TextStart, TextLen: Integer); virtual;      
    class procedure SetHideSelection(RichEditWnd: Handle; AValue: Boolean); virtual;
    class function LoadRichText(RichEditWnd: Handle; ASrc: TStream): Boolean; virtual;
    class function SaveRichText(RichEditWnd: Handle; ADst: TStream): Boolean; virtual;
    class procedure SetText(RichEditWnd: Handle; const Text: WideString; TextStart, ReplaceLength: Integer); virtual;
  end;
  TRichManagerClass = class of TRichEditManager;
                     
var
  RichEditManager : TRichManagerClass = nil;

function InitRichEdit: Boolean;
function GetRichEditClass: AnsiString;
procedure CopyStringToCharArray(const s: String; var Chrs: array of Char; ChrsSize: integer);
function FontStylesToEffects(Styles: TFontStyles): LongWord;
function EffectsToFontStyles(Effects: LongWord): TFontStyles;

implementation

const
  GlobalRichClass : AnsiString = '';
  
const
  TwipsInFontSize = 20; // see MSDN for CHARFORMAT Structure CFM_SIZE
  
function GetRichEditClass: AnsiString;
begin
  Result := GlobalRichClass;
end;  
 
function InitRichEdit: Boolean;
begin
  if GlobalRichClass = '' then begin
    if LoadLibrary('Msftedit.dll') <> 0 then begin 
      GlobalRichClass := 'RichEdit50W';
    end else if LoadLibrary('RICHED20.DLL') <> 0 then begin
      if UnicodeEnabledOS then GlobalRichClass := 'RichEdit20W'
      else GlobalRichClass := 'RichEdit20A'
    end else if LoadLibrary('RICHED32.DLL') <> 0 then begin
      GlobalRichClass := 'RichEdit';
    end;
      
    if not Assigned(RichEditManager) then 
      RichEditManager := TRichEditManager;
      
    Result := GlobalRichClass <> '';
  end;
end;

procedure CopyStringToCharArray(const s: String; var Chrs: array of Char; ChrsSize: integer);
begin
  if length(s) < ChrsSize then ChrsSize := length(s);
  if length(s) > 0 then Move(s[1], Chrs[0], ChrsSize);
end;

function FontStylesToEffects(Styles: TFontStyles): LongWord;
begin
  Result := 0;
  if fsBold in Styles then Result := Result or CFE_BOLD;
  if fsItalic in Styles then Result := Result or CFE_ITALIC;
  if fsStrikeOut in Styles then Result := Result or CFE_STRIKEOUT;
  if fsUnderline in Styles then Result := Result or CFE_UNDERLINE;
end;

function EffectsToFontStyles(Effects: LongWord): TFontStyles;
begin
  Result := [];
  if Effects and CFE_BOLD > 0 then Include(Result, fsBold);
  if Effects and CFE_ITALIC > 0 then Include(Result, fsItalic);
  if Effects and CFE_STRIKEOUT > 0 then Include(Result, fsStrikeOut);
  if Effects and CFE_UNDERLINE > 0 then Include(Result, fsUnderline);
end;

         
procedure CharFormatToFontParams(const fmt: TCHARFORMAT; var Params: TIntFontParams);
begin
  Params.Name := fmt.szFaceName;
  Params.Size := Round(fmt.yHeight/TwipsInFontSize);
  Params.Color := fmt.crTextColor;
  Params.Style := EffectsToFontStyles(fmt.dwEffects);
end;

{ TRichEditManager }

class function TRichEditManager.SetSelectedTextStyle(RichEditWnd: Handle; 
  Params: TIntFontParams): Boolean;
var
  w : WPARAM;
  fmt : TCHARFORMAT;
  
begin
  if RichEditWnd = 0 then begin
    Result := false;
    Exit;
  end;
  
  w := SCF_SELECTION;    
    
  FillChar(fmt, sizeof(fmt), 0);
  fmt.cbSize := sizeof(fmt);
  

  fmt.dwMask := fmt.dwMask or CFM_COLOR;
  fmt.crTextColor := Params.Color;

  fmt.dwMask := fmt.dwMask or CFM_FACE ;
  // keep last char for Null-termination?
  CopyStringToCharArray(Params.Name, fmt.szFaceName, LF_FACESIZE-1); 
  
  fmt.dwMask := fmt.dwMask or CFM_SIZE;
  fmt.yHeight := Params.Size * TwipsInFontSize;
  
  fmt.dwMask := fmt.dwMask or CFM_EFFECTS;
  fmt.dwEffects := FontStylesToEffects(Params.Style);
  
  Result := SendMessage(RichEditWnd, EM_SETCHARFORMAT, w, PtrInt(@fmt))>0;
end;

class function TRichEditManager.GetSelectedTextStyle(RichEditWnd: Handle;  
  var Params: TIntFontParams): Boolean; 
var
  w     : WPARAM;
  fmt   : TCHARFORMAT;
  
begin
  Result := false;
  if RichEditWnd = 0 then Exit;
  
  w := SCF_SELECTION;    
    
  FillChar(fmt, sizeof(fmt), 0);
  fmt.cbSize := sizeof(fmt);
  fmt.dwMask := CFM_COLOR or CFM_FACE or CFM_SIZE or CFM_EFFECTS;
  
  SendMessage(RichEditWnd, EM_GETCHARFORMAT, w, PtrInt(@fmt));
  
  CharFormatToFontParams(fmt, Params);
  Result := true;  
end;

type
  richedit_gettextlengthex = packed record
    flags     : DWORD;
    codepage  : LongWord;
  end;
  Tgettextlengthex = richedit_gettextlengthex;

class function TRichEditManager.GetStyleRange(RichEditWnd: Handle; TextStart: Integer; 
  var RangeStart, RangeLen: Integer): Boolean; 
var
  len     : integer;
  fmt     : TCHARFORMAT;
  textlen : Tgettextlengthex;
  sel     : TCHARRANGE;
  d       : Integer;
  last    : Integer;
  
const
  CP_UNICODE = 1200;  
  ALL_MASK = CFM_BOLD or CFM_ITALIC or CFM_STRIKEOUT or CFM_UNDERLINE or 
             CFM_SIZE or CFM_COLOR or CFM_FACE;
begin
  Result := false;
  if (RichEditWnd = 0) then Exit;
  
  FillChar(textlen, sizeof(textlen), 0);
  textlen.flags := GTL_NUMCHARS or GTL_USECRLF or GTL_PRECISE;
  textlen.codepage := CP_UNICODE;
  len := SendMessage(RichEditWnd, EM_GETTEXTLENGTHEX, WPARAM(@textlen), 0);
  Result := TextStart < len;
  if not Result then Exit;
   
  FillChar(fmt, sizeof(fmt), 0);
  fmt.cbSize := sizeof(fmt);
  
  sel.cpMin := TextStart;
  sel.cpMax := len+1;
  SendMessage(RichEditWnd, EM_EXSETSEL, 0, LPARAM(@sel));
  SendMessage(RichEditWnd, EM_GETCHARFORMAT, SCF_SELECTION, PtrInt(@fmt));
  if (fmt.dwMask and ALL_MASK) <> ALL_MASK then begin
    d := (len - sel.cpMin);
    while d > 1 do begin
      d := d div 2;
      if (fmt.dwMask and ALL_MASK) = ALL_MASK then
        sel.cpMax := sel.cpMax + d        
      else
        sel.cpMax := sel.cpMax - d;
      SendMessage(RichEditWnd, EM_EXSETSEL, 0, LPARAM(@sel));
      SendMessage(RichEditWnd, EM_GETCHARFORMAT, SCF_SELECTION, PtrInt(@fmt));
    end;
    if (fmt.dwMask and ALL_MASK) = ALL_MASK then begin
      while (sel.cpMax <= len) and ((fmt.dwMask and ALL_MASK) = ALL_MASK) do begin
        inc(sel.cpMax);
        SendMessage(RichEditWnd, EM_EXSETSEL, 0, LPARAM(@sel));
        SendMessage(RichEditWnd, EM_GETCHARFORMAT, SCF_SELECTION, PtrInt(@fmt));
      end;
    end else begin
      while (sel.cpMax > sel.cpMin) and ((fmt.dwMask and ALL_MASK) <> ALL_MASK) do begin
        dec(sel.cpMax);
        SendMessage(RichEditWnd, EM_EXSETSEL, 0, LPARAM(@sel));
        SendMessage(RichEditWnd, EM_GETCHARFORMAT, SCF_SELECTION, PtrInt(@fmt));
      end;
      inc(sel.cpMax);
    end;
  end;
  last := sel.cpMax;  
  
  sel.cpMin := 0;
  sel.cpMax := TextStart+1;
  SendMessage(RichEditWnd, EM_EXSETSEL, 0, LPARAM(@sel));
  SendMessage(RichEditWnd, EM_GETCHARFORMAT, SCF_SELECTION, PtrInt(@fmt));
  if (fmt.dwMask and ALL_MASK) <> ALL_MASK then begin
    d := TextStart;
    while d > 1 do begin
      d := d div 2;
      if (fmt.dwMask and ALL_MASK) = ALL_MASK then
        dec(sel.cpMin,d)
      else
        inc(sel.cpMin,d);
      SendMessage(RichEditWnd, EM_EXSETSEL, 0, LPARAM(@sel));
      SendMessage(RichEditWnd, EM_GETCHARFORMAT, SCF_SELECTION, PtrInt(@fmt));
    end;
    if (fmt.dwMask and ALL_MASK) = ALL_MASK then begin
      while (sel.cpMin > 0) and ((fmt.dwMask and ALL_MASK) = ALL_MASK) do begin
        dec(sel.cpMin);
        SendMessage(RichEditWnd, EM_EXSETSEL, 0, LPARAM(@sel));
        SendMessage(RichEditWnd, EM_GETCHARFORMAT, SCF_SELECTION, PtrInt(@fmt));
      end;
      if (fmt.dwMask and ALL_MASK) <> ALL_MASK then inc(sel.cpMin);
    end else begin
      while (sel.cpMin < TextStart) and ((fmt.dwMask and ALL_MASK) <> ALL_MASK) do begin
        inc(sel.cpMin);
        SendMessage(RichEditWnd, EM_EXSETSEL, 0, LPARAM(@sel));
        SendMessage(RichEditWnd, EM_GETCHARFORMAT, SCF_SELECTION, PtrInt(@fmt));
      end;
    end;
  end;  
 
  RangeStart := sel.cpMin;
  RangeLen := last - sel.cpMin - 1;
  Result := true;  
end;

class procedure TRichEditManager.GetSelection(RichEditWnd: Handle; var TextStart, TextLen: Integer); 
var
  Range  : TCHARRANGE;
begin
  Range.cpMax := 0;
  Range.cpMin := 0;
  SendMessage(RichEditWnd, EM_EXGETSEL, 0, PtrInt(@Range));
  TextStart := Range.cpMin;
  TextLen := Range.cpMax-Range.cpMin;
end;

class procedure TRichEditManager.SetSelection(RichEditWnd: Handle; TextStart, TextLen: Integer); 
var
  Range  : TCHARRANGE;
begin
  Range.cpMin := TextStart;
  Range.cpMax := TextStart + TextLen;
  SendMessage(RichEditWnd, EM_EXSETSEL, 0, PtrInt(@Range));
end;

class procedure TRichEditManager.SetHideSelection(RichEditWnd: Handle; AValue: Boolean);
var
  style  : LResult;
begin
  // res-setting options might RichEdit style. Must restore it, after option is changed
  style := GetWindowLong(RichEditWnd, GWL_STYLE);
  if AValue then
    SendMessage(RichEditWnd, EM_SETOPTIONS, ECOOP_AND, not ECO_NOHIDESEL)
  else
    SendMessage(RichEditWnd, EM_SETOPTIONS, ECOOP_OR, ECO_NOHIDESEL);
  SetWindowLong(RichEditWnd, GWL_STYLE, style);
end;

type
  TEditStream_ = packed record
    dwCookie    : PDWORD;
    dwError     : DWORD;
    pfnCallback : EDITSTREAMCALLBACK;
  end;
  
function RTFLoadCallback(dwCookie:PDWORD; pbBuff:LPBYTE; cb:LONG; var pcb:LONG):DWORD; stdcall;
var
  s : TStream;  
begin
  try
    s := TStream(dwCookie);
    pcb := s.Read(pbBuff^, cb);
    Result := 0;
  except
    Result := 1;
  end;
end;

class function TRichEditManager.LoadRichText(RichEditWnd: Handle; ASrc: TStream): Boolean; 
var
  cbs : TEditStream_;
begin
  cbs.dwCookie := PDWORD(ASrc);
  cbs.dwError := 0;
  cbs.pfnCallback := @RTFLoadCallback;
  SendMessage(RichEditWnd, EM_STREAMIN, SF_RTF, LPARAM(@cbs) );
  Result := cbs.dwError = 0;
end;

function RTFSaveCallback(dwCookie:PDWORD; pbBuff:LPBYTE; cb:LONG; var pcb:LONG):DWORD; stdcall;
var
  s : TStream;  
begin
  try
    s := TStream(dwCookie);
    pcb := s.Write(pbBuff^, cb);
    Result := 0;
  except
    Result := 1;
  end;
end;

class function TRichEditManager.SaveRichText(RichEditWnd: Handle; ADst: TStream): Boolean; 
var
  cbs : TEditStream_;
begin
  cbs.dwCookie := PDWORD(ADst);
  cbs.dwError := 0;
  cbs.pfnCallback := @RTFSaveCallback;
  SendMessage(RichEditWnd, EM_STREAMOUT, SF_RTF, LPARAM(@cbs) );
  Result := cbs.dwError = 0;
end;

class procedure TRichEditManager.SetText(RichEditWnd:Handle;
  const Text: WideString; TextStart, ReplaceLength:Integer);
var
  AnsiText : AnsiString;
  txt      : PChar;
  s, l     : Integer;
begin
  GetSelection(RichEditWnd, s, l);
  SetSelection(RichEditWnd, TextStart, ReplaceLength);

  txt:=nil;
  if UnicodeEnabledOS then begin
    if Text<>'' then txt:=@Text[1];
    SendMessageW(RichEditWnd, EM_REPLACESEL, 0, LPARAM(txt));
  end else begin
    AnsiText:=Text;
    if AnsiText<>'' then txt:=@AnsiText[1];
    SendMessageA(RichEditWnd, EM_REPLACESEL, 0, LPARAM(txt));
  end;

  SetSelection(RichEditWnd, s, l);
end;

end.                                            

