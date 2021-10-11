// ColorMasks
//
// efg, 3 Sept 2002; 23 Feb 2003
// www.efg2.com/Lab

program ColorMasks;

uses
  QForms,
  ScreenColorMasks in 'ScreenColorMasks.pas' {FormColorMasks};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormColorMasks, FormColorMasks);
  Application.Run;
end.
