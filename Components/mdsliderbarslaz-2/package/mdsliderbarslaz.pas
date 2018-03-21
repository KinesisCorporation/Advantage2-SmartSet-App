{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit MdSliderbarsLaz;

interface

uses
  mdsliderbarpalettteimages, SliderBars, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('SliderBars', @SliderBars.Register);
end;

initialization
  RegisterPackage('MdSliderbarsLaz', @Register);
end.
