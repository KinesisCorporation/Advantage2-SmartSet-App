{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit uEControls;

interface

uses
  uecontrolsreg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('uecontrolsreg', @uecontrolsreg.Register);
end;

initialization
  RegisterPackage('uEControls', @Register);
end.
