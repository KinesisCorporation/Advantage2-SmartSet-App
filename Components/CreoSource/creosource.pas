{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit creosource;

{$warn 5023 off : no warning about unused units}
interface

uses
  LineObj, LabelBox, PanelBtn, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('LineObj', @LineObj.Register);
  RegisterUnit('LabelBox', @LabelBox.Register);
  RegisterUnit('PanelBtn', @PanelBtn.Register);
end;

initialization
  RegisterPackage('creosource', @Register);
end.
