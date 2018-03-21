{------------------------------------------------------------------------------
  uEControlsReg v1.0  2015-10-24
  Author: Miguel A. Risco-Castillo
  http://ue.accesus.com/uecontrols

  Register unit for uE Controls

  THE COPYRIGHT NOTICES IN THE SOURCE CODE MAY NOT BE REMOVED OR MODIFIED.
  IF YOU MODIFY AND/OR DISTRIBUTE THE CODE TO ANY THIRD PARTY THEN YOU MUST NOT
  VEIL THE ORIGINAL AUTHOR. IT MUST ALWAYS BE CLEARLY IDENTIFIABLE.

  The contents of this file are subject in priority to the License in this header,
  in the license.txt file and the Mozilla Public License Version 1.1 (MPL);
  you may not use this file except in compliance with these licenses. You may obtain
  a copy of the MPL License at http://www.mozilla.org/MPL/MPL-1.1.html

  Software distributed under the Licenses is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the Licenses for
  the specific language governing rights and limitations under the Licenses.
------------------------------------------------------------------------------}

unit uecontrolsreg;

{$mode objfpc}{$H+}

interface

uses
  Classes,LResources,
  uerotimage, ueknob, uemultiturn, ueselector, ueled, uebutton, uegauge, uetileimage, uetilepanel;

procedure Register;

implementation

procedure Register;
begin
  {$i uecontrols.lrs}
  RegisterComponents('uEControls', [TuERotImage,TuEKnob,TuEMultiTurn,TuESelector,TuELED,TuEButton,TuEGauge,
                                    TuETileImage,TuETilePanel]);
end;

end.
