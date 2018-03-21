unit RichMemoRTF;

interface

uses
  Classes, SysUtils, LCLProc, LCLIntf,
  RichMemo, RTFParsPre211, Graphics;

//todo: formatting support!

function MVCParserLoadStream(ARich: TCustomRichMemo; Source: TStream): Boolean;

implementation

type
  { TRTFMemoParser }

  TRTFMemoParser = class(TRTFParser)
  private
    txtbuf   : String; // keep it UTF8 encoded!

    fcolor    : TColor;      // Foreground color
  protected
    procedure classUnk;
    procedure classText;
    procedure classControl;
    procedure classGroup;
    procedure classEof;

    procedure doSpecialChar;
    procedure doChangeCharAttr;

    function GefaultTextColor: TColor;
    procedure PushText;
  public
    Memo  : TCustomRichMemo;
    constructor Create(AMemo: TCustomRichMemo; AStream: TStream);
    procedure StartReading;
  end;

{ TRTFMemoParserr }

procedure TRTFMemoParser.classUnk;
begin
  //writelN('unk: ', rtfMajor, ' ',rtfMinor,' ', rtfParam,' ', GetRtfText);
end;

procedure TRTFMemoParser.classText;
begin
  //writeln('txt:  ', rtfMajor, ' ',rtfMinor,' ', rtfParam);
  case rtfMinor of
    rtfOptDest: {skipping option generator};
  else
    txtbuf:=txtbuf+Self.GetRtfText;
  end;
end;

procedure TRTFMemoParser.classControl;
begin
  //writeln('ctrl: ', rtfClass,' ', rtfMajor, ' ', Self.GetRtfText, ' ',rtfMinor,' ', rtfParam);
  case rtfMajor of
    rtfSpecialChar: doSpecialChar;
    rtfCharAttr: doChangeCharAttr;
  end;
end;

procedure TRTFMemoParser.classGroup;
begin
  //writeln('group:  ', rtfMajor, ' ',rtfMinor,' ', rtfParam, ' ', GetRtfText);
end;

procedure TRTFMemoParser.classEof;
begin
  PushText;
end;

procedure TRTFMemoParser.doSpecialChar;
const
  {$ifdef MSWINDOWS}
  CharPara = #13#10;
  {$else}
  CharPara = #10;
  {$endif}
  CharTab  = #9;
  CharLine = #13;
begin
  case rtfMinor of
    rtfLine: txtbuf:=txtbuf+CharLine;
    rtfPar:  txtbuf:=txtbuf+CharPara;
    rtfTab:  txtbuf:=txtbuf+CharTab;
  end;
end;

procedure TRTFMemoParser.doChangeCharAttr;
var
  p : PRTFColor;
begin
  if txtbuf<>'' then PushText;

  case rtfMinor of
    rtfForeColor: begin
      if rtfParam<>0 then p:=Colors[rtfParam]
      else p:=nil;
      if not Assigned(p) then
        fcolor:=GefaultTextColor
      else
        fcolor:=RGBToColor(p^.rtfCRed, p^.rtfCGreen, p^.rtfCBlue);
    end;
  end;
end;

function TRTFMemoParser.GefaultTextColor:TColor;
begin
  Result:=ColorToRGB(Memo.Font.Color);
end;

procedure TRTFMemoParser.PushText;
var
  len   : Integer;
  ofs   : Integer;
  para  : TFontParams;
begin
  len:=UTF8Length(txtbuf);
  if len=0 then Exit;

  ofs:=Memo.GetTextLen;
  Memo.SelStart:=ofs;
  Memo.SelLength:=0;
  Memo.SelText:=txtbuf;

  txtbuf:='';

  Memo.GetTextAttributes(ofs, para);
  para.Color:=ColorToRGB(fColor);
  Memo.SetTextAttributes(ofs, len, para);
end;

constructor TRTFMemoParser.Create(AMemo:TCustomRichMemo;AStream:TStream);
begin
  inherited Create(AStream);
  Memo:=AMemo;
  ClassCallBacks[rtfText]:=@classText;
  ClassCallBacks[rtfControl]:=@classControl;
  ClassCallBacks[rtfGroup]:=@classGroup;
  ClassCallBacks[rtfUnknown]:=@classUnk;
  ClassCallBacks[rtfEof]:=@classEof;
end;

procedure TRTFMemoParser.StartReading;
begin
  Memo.Lines.BeginUpdate;
  try
    inherited StartReading;
    PushText;
    Memo.SelStart:=0;
    Memo.SelLength:=0;
  finally
    Memo.Lines.EndUpdate;
  end;
end;

function MVCParserLoadStream(ARich: TCustomRichMemo; Source: TStream): Boolean;
var
  p   : TRTFMemoParser;
begin
  Result:=Assigned(ARich) and Assigned(Source);
  if not Result then Exit;

  p:=TRTFMemoParser.Create(ARich, Source);
  try
    p.StartReading;
  finally
    p.Free;
  end;
  Result:=True;
end;

initialization
  RTFLoadStream:=@MVCParserLoadStream;


end.
