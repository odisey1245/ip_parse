program parser;

{ started: 21.02.2012 }

{$mode objfpc}{$H+}

uses
  Classes, sysutils, htranslator, DeclTypes, bparser, ocparser, DeclIds;

{global params}
var
  sdk_path: string = '';
  dest_path: string = 'src';

function ProcessParams(fs: TStrings): Boolean;

  function OnlyAlpha(s: PChar): Boolean;
  begin
    if s^ = #0 then Exit(True)
    else if upCase(s^) in ['A'..'Z'] then Exit(OnlyAlpha(s+1))
    else Exit(False);
  end;

var i: Integer; p: string;
begin
  Result := True;
  for i := 1 to ParamCount do
  begin
    p := ParamStr(i);
    if p = '' then Continue;
    case p[1] of
    'd':
      if Copy(p, 1, 5)= 'dest=' then
      begin
        dest_path := Copy(p, 6, MaxInt);
        Continue;
      end;
    's':
      if Copy(p, 1, 4)= 'sdk=' then
      begin
        sdk_path := Copy(p, 5, MaxInt);
        Continue;
      end;
    end;
    if OnlyAlpha(PChar(p)) then
    begin
      if fs.IndexOf(p) < 0 then fs.Add(p)
    end else begin
      WriteLn('Objective-C to Pascal headers translator');
      WriteLn('Usage:');
      WriteLn('  ', ExtractFileName(ParamStr(0)), ' [param param ...]');
      WriteLn('where "param" is framework name for translation or the following command:');
      WriteLn('  dest=ddd  "ddd" is a destination path for Pascal units (default "src")');
      WriteLn('  sdk=sss   "sss" is a path to SDK (e.g. /Developer/.../iPhoneSimulator5.0.sdk)');
      Result := False;
      Exit;
    end;
  end;
end;

var
  Frameworks: TStringList;
  frameworks_path: string;
begin
  Frameworks := TStringList.Create;
  try
    if not ProcessParams(Frameworks) then Exit;
    if Frameworks.Count = 0 then
    begin
      WriteLn('Error: no frameworks specified for parsing!');
      Exit;
    end;
    if dest_path <> '' then dest_path := IncludeTrailingPathDelimiter(dest_path);
    { TODO : interactive choosing of SDK path }
    if sdk_path <> '' then sdk_path := IncludeTrailingPathDelimiter(sdk_path);
    frameworks_path := sdk_path + 'System' + PathDelim + 'Library' + PathDelim + 'Frameworks' + PathDelim;
    if not DirectoryExists(frameworks_path) then
    begin
      WriteLn('Frameworks cannot be found: path ', frameworks_path, ' does not exist');
      Exit;
    end;
    with TTranslator.Create(Frameworks) do
    try
      DestPath := dest_path;
      if (dest_path <> '') and not DirectoryExists(dest_path) then
        ForceDirectories(dest_path);
      FrameworksPath := frameworks_path;
      Run;
    finally
      Free;
    end;
  finally
    Frameworks.Free;
  end;
  WriteLn('Done');
  WriteLn('[press Enter]');
  ReadLn;
end.

