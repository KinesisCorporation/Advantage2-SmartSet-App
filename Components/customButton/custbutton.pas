{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit custButton;

interface

uses
  CustomButton, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('CustomButton', @CustomButton.Register);
end;

initialization
  RegisterPackage('custButton', @Register);
end.
