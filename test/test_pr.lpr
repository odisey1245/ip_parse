program test_pr;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, hscanner;

function dbgStr(s: string): string;
var i: Integer;
begin
  Result := '';
  for i := 1 to Length(s) do
    if s[i] >= ' ' then
      Result := Result + s[i]
    else
      Result := Result + '#' + IntToStr(Byte(s[i]));
end;

function DoTest(FileName: string): Boolean;
var
  scan: THeaderScanner;
  l: TLex;
  s1, s2: string;
begin
  scan := THeaderScanner.Create('', TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite));
  try
    Write(ExtractFileName(FileName), ' ... ');
    l := scan.GetNextLex;
    s1 := l.AsString;
    s2 := '';
    l := scan.GetNextLex;
    while Assigned(l) do
    begin
      if l.LexType = ltNumber then
        s2 := s2 + l.AsString;
      l := scan.GetNextLex;
    end;
    Result := s1 = s2;
    if not Result then
      WriteLn('FAILED: "', s2, '" [needed: "', s1, '"]')
    else
      WriteLn('OK');
  finally
    scan.Free;
  end;
end;

var
  sr: TSearchRec;
begin
  if FindFirst('tests\s*', faAnyFile and not faDirectory, sr) = 0 then
    repeat
      if not DoTest('tests\' + sr.Name) then
        {WriteLn(sr.Name, ' test FAILED!')};
    until FindNext(sr) <> 0;
  FindClose(sr);
  WriteLn('DONE [press Enter]');
  ReadLn;
end.

