unit htranslator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DeclTypes, DeclIds;

type

  { TTranslator }

  TTranslator = class
  private
    FDestPath: string;
    FFrameworksPath: string;
    FFrameworks: TStrings;
    FParsedFrameworks: TStringList;
    FGlobTypes: TDeclaredTypes;
    FGlobDeclIds: TGlobDeclIds;
    FGlobDefines, FGlobImported: TStringList;
    procedure ResetAvailTypes;
    function FrameworkHasLibrary(fd, fn: string): Boolean;
    function ParseFramework(framework: string): Boolean;
    procedure FrameworkReference(Sender: TObject; AFramework: string;
      var Cancel: Boolean);
  public
    constructor Create(FrameworksToTranslate: TStrings);
    destructor Destroy; override;
    procedure Run;
    property FrameworksPath: string read FFrameworksPath write FFrameworksPath;
    property DestPath: string read FDestPath write FDestPath;
  end;

implementation

uses ocparser;

{ TTranslator }

constructor TTranslator.Create(FrameworksToTranslate: TStrings);
begin
  FGlobTypes := TDeclaredTypes.Create;
  FGlobTypes.InitPredefinedTypes;
  FGlobDeclIds := TGlobDeclIds.Create;
  FGlobDeclIds.Add('sin', 'Sin', FGlobTypes.GetTypeByPasName('Double'));
  FGlobDeclIds.Add('cos', 'Cos', FGlobTypes.GetTypeByPasName('Double'));
  FGlobDeclIds.Add('sinf', 'Sin', FGlobTypes.GetTypeByPasName('Double'));
  FGlobDeclIds.Add('cosf', 'Cos', FGlobTypes.GetTypeByPasName('Double'));
  FGlobDeclIds.Add('tanf', 'Tan', FGlobTypes.GetTypeByPasName('Double'), 'Math');
  FGlobDeclIds.Add('pi', 'pi', FGlobTypes.GetTypeByPasName('Double'));
  FGlobDeclIds.Add('IsInfinite', 'IsInfinite', FGlobTypes.GetTypeByPasName('Boolean'), 'Math');
  FGlobDeclIds.Add('GL_TEXTURE_2D', 'GL_TEXTURE_2D', FGlobTypes.GetTypeByCName('int'), 'gles11');
  FGlobDeclIds.CommitNew;
  FGlobImported := TStringList.Create;
  FGlobImported.CaseSensitive := True;
  FGlobImported.Sorted := True;
  FGlobImported.Duplicates := dupIgnore;
  FGlobDefines := TStringList.Create;
  FGlobDefines.CaseSensitive := True;
  FGlobDefines.Sorted := True;
  FFrameworks := FrameworksToTranslate;
  FParsedFrameworks := TStringList.Create;
  FParsedFrameworks.CaseSensitive := False;
  FParsedFrameworks.Sorted := True;
end;

destructor TTranslator.Destroy;
var i: Integer;
begin
  FGlobDeclIds.Free;
  FGlobTypes.Free;
  FGlobImported.Free;
  for i := 0 to FGlobDefines.Count - 1 do
    FGlobDefines.Objects[i].Free;
  FGlobDefines.Free;
  FParsedFrameworks.Free;
  inherited Destroy;
end;

procedure TTranslator.Run;
begin
  try
    while FFrameworks.Count > 0 do
    begin
      if ParseFramework(FFrameworks[0]) then
        FFrameworks.Delete(0);
    end;
  except
    on err: Exception do
      WriteLn('Fatal error: ', err.Message);
  end;
end;

procedure TTranslator.FrameworkReference(Sender: TObject;
  AFramework: string; var Cancel: Boolean);
begin
  Cancel := False;
  if FParsedFrameworks.IndexOf(AFramework) >= 0 then Exit;
  FFrameworks.Insert(0, AFramework);
  Cancel := True;
end;

procedure TTranslator.ResetAvailTypes;
begin
  FGlobTypes.ResetAvailable;
end;

function TTranslator.FrameworkHasLibrary(fd, fn: string): Boolean;
var i: Integer;
begin
  i := Length(fd) - 1;
  while (i > 0) and (fd[i] <> PathDelim) do Dec(i);
  Result := FileExists(Copy(fd, 1, i) + fn);
end;

function TTranslator.ParseFramework(framework: string): Boolean;

  function FindFrameworkDir(out fw_dir: string): Boolean;
  var sr: TSearchRec; str: string;
  begin
    Result := False;
    str := framework;
    if Pos('/', str) > 0 then
      str := Copy(str, 1, Pos('/', str) - 1);
    if FindFirst(FFrameworksPath + AllFilesMask, faDirectory, sr) = 0 then
      repeat
        if SameText(str + '.framework', sr.Name) then
        begin
          fw_dir := sr.Name;
          Result := True;
          Break;
        end;
      until FindNext(sr) <> 0;
    FindClose(sr);
  end;

  function FindMainHeader(path: string; out MainFile: string): Boolean;
  var sr: TSearchRec; str: string;
  begin
    // Main header <framework name>.h
    Result := False;
    str := framework;
    if Pos('/', str) > 0 then
      Delete(str, 1, Pos('/', str))
    else
      str := str + '.h';
    if FindFirst(path + AllFilesMask, faAnyFile and not faDirectory, sr) = 0 then
      repeat
        if SameText(str, sr.Name) then
        begin
          MainFile := path + sr.Name;
          Result := True;
          Break;
        end;
      until FindNext(sr) <> 0;
    FindClose(sr);
  end;

  function ExtractFramework(fname: string): string;
  var i: Integer;
  begin
    Result := '';
    i := Pos('.framework', fname);
    if i > 0 then
    begin
      Result := Copy(fname, 1, i - 1);
      while Pos(PathDelim, Result) > 0 do
        Delete(Result, 1, Pos(PathDelim, Result));
    end else
      raise Exception.Create('Cannot determine framework name!');
  end;

  function ExtractFileNameOnly(fname: string): string;
  var pointPos: Integer;
  begin
    Result := ExtractFileName(fname);
    pointPos := Pos('.', Result);
    if pointPos > 0 then
      Result := Copy(Result, 1, pointPos - 1);
  end;

var
  fwd, mfile, unit_name, fwname, dir: string;
  aparser: TObjCParser;
  newUnit: TStringList;
begin
  Result := False;
  if FParsedFrameworks.IndexOf(framework) >= 0 then
  begin
    WriteLn('Framework "', framework, '" has been already parsed');
    Exit(True);
  end;

  if not FindFrameworkDir(fwd) then
    raise Exception.Create('Could not find directory of "' + framework + '" framework');
  dir := FFrameworksPath + fwd + PathDelim + 'Headers' + PathDelim;
  if not DirectoryExists(dir) then
  begin
    // try Versions/C/Headers
    fwd := FFrameworksPath + fwd + PathDelim + 'Versions' + PathDelim + '%s'
      + PathDelim + 'Headers' + PathDelim;
    dir := Format(fwd, ['A']);
    if not DirectoryExists(dir) then
    begin
      dir := Format(fwd, ['C']);
      if not DirectoryExists(dir) then
        raise Exception.Create('Headers directory "' + fwd + '" not found');
    end;
  end;
  fwd := dir;
  if not FindMainHeader(fwd, mfile) then
    mfile := fwd + '*.h';
    //raise Exception.Create('Main header file for framework "' + framework + '" not found in ' + fwd);

  fwname := ExtractFramework(mfile);
  newUnit := TStringList.Create;
  try
    newUnit.Add('{ translation of ' + mfile + ' }');
    unit_name := fwname;
    newUnit.Add('unit ' + unit_name + ';');
    newUnit.Add('');
    newUnit.Add('{$mode objfpc}'); // it cannot be 'delphi' cause it does not allow float->double typecast
    newUnit.Add('{$modeswitch duplicatelocals}');
    //newUnit.Add('{$modeswitch objectivec1}');
    //newUnit.Add('{$modeswitch cvar}');
    //newUnit.Add('{$packrecords c}');
    //newUnit.Add('{$pointermath on}');
    newUnit.Add('');
    newUnit.Add('interface');
    newUnit.Add('');
    if FrameworkHasLibrary(fwd, fwname) then
    begin
      newUnit.Add('{$linkframework ' + fwname + '}');
      newUnit.Add('');
    end;
    WriteLn(ExtractFileName(mfile));
    aparser := TObjCParser.Create(mfile);
    try
      aparser.InitDefines(FGlobDefines);
      aparser.InitImported(FGlobImported);
      aparser.InitTypes(FGlobTypes);
      aparser.InitDeclIds(FGlobDeclIds);
      aparser.OnFrameworkReference := @FrameworkReference;
      FGlobTypes.SetPackRecords('{packrecords c}');
      if not aparser.DoParse(newUnit) then
      begin
        FGlobTypes.DeleteNew(unit_name);
        FGlobDeclIds.DeleteNew;
        Exit;
      end;
      newUnit.Add('');
      newUnit.Add('implementation');
      newUnit.Add('');
      aparser.AddProcs(newUnit);
      newUnit.Add('end.');
      FGlobTypes.CommitNew;
      FGlobDeclIds.CommitNew;
      aparser.RetrieveDefines(FGlobDefines);
      aparser.RetrieveImported(FGlobImported);
    finally
      aparser.RetrieveTypes;
      aparser.Free;
      ResetAvailTypes;
    end;
    newUnit.SaveToFile(FDestPath + LowerCase(unit_name) + '.pas');
  finally
    newUnit.Free;
  end;
  FParsedFrameworks.Add(framework);
  Result := True;
end;

end.

