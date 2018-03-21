{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit eccontrols;

interface

uses
  ECTypes, ECScale, ECBevel, ECLink, ECImageMenu, ECSpinCtrls, ECSwitch, 
  ECEditBtns, ECHeader, ECCheckListBox, ECSlider, ECProgressBar, ECRuler, 
  ECGroupCtrls, ECTabCtrl, ECAccordion, ECTriangle, ECConfCurve, ECScheme, 
  ECDesignTime, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ECDesignTime', @ECDesignTime.Register);
end;

initialization
  RegisterPackage('eccontrols', @Register);
end.
