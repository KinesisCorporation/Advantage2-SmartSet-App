unit RichMemoFactory; 

{$mode objfpc}{$H+}

interface

{$define NoRichMemo}
{$ifdef LCLWin32}{$undef NoRichMemo}{$endif}
{$ifdef LCLCarbon}{$undef NoRichMemo}{$endif}
{$ifdef LCLGtk2}{$undef NoRichMemo}{$endif}

uses
  WSLCLClasses,
  RichMemo
  {$ifdef NoRichMemo},WSRichMemo{$endif}
  {$ifdef LCLWin32},Win32RichMemo{$endif}
  {$ifdef LCLCarbon},CarbonRichMemo{$endif}
  {$ifdef LCLGtk2},RichMemoRTF, Gtk2RichMemo{$endif}
  ;

function RegisterCustomRichMemo: Boolean;

implementation

function RegisterCustomRichMemo: Boolean; alias : 'WSRegisterCustomRichMemo';
begin
  Result := True;
  {$ifdef LCLWin32}RegisterWSComponent(TCustomRichMemo, TWin32WSCustomRichMemo);{$endif}
  {$ifdef LCLCarbon}RegisterWSComponent(TCustomRichMemo, TCarbonWSCustomRichMemo);{$endif}
  {$ifdef LCLGtk2}RegisterWSComponent(TCustomRichMemo, TGtk2WSCustomRichMemo);{$endif}
  {$ifdef NoRichMemo}RegisterWSComponent(TCustomRichMemo, TWSCustomRichMemo);{$endif}
end;


end.

