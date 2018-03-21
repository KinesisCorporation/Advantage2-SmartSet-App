{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit richmemopackage; 

interface

uses
  RichMemoFactory, richmemoregister, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('richmemoregister', @richmemoregister.Register); 
end; 

initialization
  RegisterPackage('richmemopackage', @Register); 
end.
