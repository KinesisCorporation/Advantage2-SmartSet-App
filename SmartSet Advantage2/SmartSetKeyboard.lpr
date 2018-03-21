program SmartSetKeyboard;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, runtimetypeinfocontrols, u_form_main, u_form_new, u_key_layer, u_const,
  richmemopackage, uecontrols;

{$R *.res}

begin
  GApplication := APPL_ADV2;
  GApplicationName := 'SmartSet App for Advantage2';
  GApplicationTitle := 'Adv2 SmartSet App';
  Application.Title:='Adv2 SmartSet App';
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

