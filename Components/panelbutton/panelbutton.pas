{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit panelbutton;

interface

uses
  PanelBtn, LabelBox, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('PanelBtn', @PanelBtn.Register);
  RegisterUnit('LabelBox', @LabelBox.Register);
end;

initialization
  RegisterPackage('panelbutton', @Register);
end.
