program converter;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, sysutils, main
  { you can add units after this };

{$R *.res}

{$ifdef DEBUG}
const
  cHeapTrace = 'heaptrace.trc';
{$endif}

begin
  {$ifdef DEBUG}
  if FileExists(cHeapTrace) then
     DeleteFile(cHeapTrace);
  SetHeapTraceOutput(cHeapTrace);
  {$endif}
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

