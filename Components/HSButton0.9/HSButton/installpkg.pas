{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit installpkg;

interface

uses
  HSButton, HSSpeedButton, HSStaticText, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('HSButton', @HSButton.Register);
  RegisterUnit('HSSpeedButton', @HSSpeedButton.Register);
  RegisterUnit('HSStaticText', @HSStaticText.Register);
end;

initialization
  RegisterPackage('installpkg', @Register);
end.
