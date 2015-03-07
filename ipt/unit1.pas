unit Unit1;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

interface

uses
  Classes, SysUtils;

type
  MyProtocol = objcprotocol external;

  MyClass = objcclass
  public
    procedure Anton;
    procedure Andrew;
  end;

  MyClassCat = objccategory external name 'UIViewGeometry' (MyClass)
    procedure Alexey;
  end;

  MyClass2 = objcclass(MyClass, MyProtocol)
  public
    procedure Alexander;
  end;

implementation

var c: MyClass;
begin
  c.a
end.

