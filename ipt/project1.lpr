program project1;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, Foundation, UIKit, MainForm;

var
  pool: NSAutoreleasePool;
  retVal: Integer;

{$R *.res}

begin
  pool := NSAutoreleasePool.alloc.init;
  retVal := UIApplicationMain(argc, argv, nil, NSStringFromClass(TMainForm.classClass));
  pool.release;
  Halt(retVal);
end.

