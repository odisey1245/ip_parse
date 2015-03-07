{ C header parser }
unit hparser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, hscanner, DeclTypes;

type
  TUnitSection = (usConst, usType, usVar, usProcs);

const
  UnitSectionStr: array [TUnitSection] of string = ('const', 'type', 'var', '');

type
  PTypeDeclaration = ^TTypeDeclaration;

  { THeaderParser }

  THeaderParser = class
  private
    FBeforeIndex: Integer;
    FFileName: string;
    FCurFileName: string;
    FTypesStack: TFPList;
    FOnFrameworkReference: TFrameworkReferenceEvent;
    procedure AddAdditionalDirectives;
    procedure CreateInterfaceUsesClause;
    function GenerateFrameworkMainHeader(AFileMask: string): TStream;

    procedure AddAdditionalTypes;

    procedure PrintCurrentFileName;
    function ReadArrayBounds(td: TTypeDeclaration; CreateNewTypes: Boolean): string;
    procedure ReadModifiers(out is_const, is_inline: Boolean);

    procedure TypedefStruct;
    procedure TypedefUnion;
    procedure TypedefEnum;
    procedure NewAlias(td: TTypeDeclaration);
    procedure ParseStructContent(out is_bitpacked: Boolean);
    procedure ParseUnionContent;
    function ReadFuncParams(Dependencies: TFPList = nil): string;
    function ReadStructDefinition: string;
    function ReadUnionDefinition: string;

    procedure ParseStructUnion(isUnion: Boolean);
    procedure StartParsing;

    procedure FrameworkReference(Sender: TObject; AFramework: string;
      var Cancel: Boolean);
    procedure NestedFrameworkReference(Sender: TObject; AFramework: string;
      var Cancel: Boolean);
    procedure NewPragmaPack(Sender: TObject; directive: string);
  protected
    cur_lex: TLex;
    FRealDest: TStrings;
    FDest: array [TUnitSection] of TStrings;
    FDeclaredTypes: TDeclaredTypes;
    Scanner: THeaderScanner;
    FPredefinedSymbols: TStringList; // classes, types and other to skip (C orig names)
    FTypes: TFPList;
    FAdditionalDirectives,
    FRefFrameworks: TStringList; // units to add to uses clause
    FAdditionalTypes: TFPList;
    function NextLex: TLex; inline;

    procedure DestAdd(unitSec: TUnitSection; const str: string);
    procedure DestAddClearLine(unitSec: TUnitSection);
    procedure ParseVarDeclaration(is_extern: Boolean; td: TTypeDeclaration;
      OrigVarName, postfix: string; is_const: Boolean); virtual;

    procedure PushType(td: TTypeDeclaration); inline;
    procedure PushPointerType(td: TTypeDeclaration); inline;
    procedure PopType; inline;

    procedure ErrorLexExpected(lstr: string); inline;
    procedure ErrorLexExpectedIn(lstr, context: string); inline;
    procedure ParseFuncDeclaration(is_extern: Boolean; td: TTypeDeclaration;
      ProcName, postfix: string); virtual;
    function FixNumber(str: string): string;
    procedure ParseNext; virtual;

    procedure InitPredefinedSymbols; virtual;
    function IsPredefined(name: string): Boolean;

    procedure AddFromDestUS(dest: TStrings; usec: TUnitSection);
    procedure AddConstDeclarations(dest: TStrings); virtual;
    procedure AddTypeDeclarations(dest: TStrings); virtual;
    procedure AddVarDeclarations(dest: TStrings); virtual;
    procedure AddProcDeclarations(dest: TStrings); virtual;
    procedure DoAfterParse; virtual;

    function DeclareAuxType(OrigTypeName, typeDeclaration: string): TTypeDeclaration; inline;
    function DeclareType(OrigTypeName, typeDeclaration: string;
      withPointer: Boolean): TTypeDeclaration; inline;
    function DeclarePointerType(OrigTypeName: string): TTypeDeclaration; inline;
    procedure DefinedConstant(Sender: TObject; ConstName, ConstValue: string); virtual;

    function BuiltInCType(out type_str: string): Boolean;
    function ReadConstExpr: string;
    procedure ReadDefinition(out typ, ident: string; CreateNewTypes: Boolean;
      Dependencies: TFPList = nil);
    function ReadType: TTypeDeclaration; virtual;
    procedure SkipToLex(const lex: string; braket_lev: Integer = 0);

    procedure TypeDef;
    procedure EnumToConsts;
    procedure ParseEnum; virtual;
    procedure VarProcDefinition;
  public
    constructor Create(AFileName: string);
    destructor Destroy; override;
    function DoParse(Dest: TStrings): Boolean;
    procedure RetrieveDefines(dest: TStrings);
    procedure RetrieveImported(dest: TStrings);
    procedure RetrieveTypes;
    procedure InitDefines(src: TStrings);
    procedure InitImported(src: TStrings);
    procedure InitTypes(src: TDeclaredTypes);
    procedure StoreDefines(src: TStrings);
    property OnFrameworkReference: TFrameworkReferenceEvent
      read FOnFrameworkReference write FOnFrameworkReference;
  end;

implementation

procedure AddClearLine_(Dest: TStrings);
begin
  if (Dest.Count = 0) or (Trim(Dest[Dest.Count - 1]) <> '') then
    Dest.Add('');
end;

{ THeaderParser }

procedure THeaderParser.DefinedConstant(Sender: TObject; ConstName,
  ConstValue: string);
begin
  if SameText(ConstName, Scanner.CurFrameworkUnit) then Exit; // IOKit
  if FPredefinedSymbols.IndexOf(ConstName) >= 0 then Exit; // e.g. True, False
  DestAdd(usConst, '  ' + ConstName + ' = ' + ConstValue + ';')
end;

procedure THeaderParser.NewPragmaPack(Sender: TObject; directive: string);
begin
  DestAdd(usType, directive);
  FDeclaredTypes.SetPackRecords(directive);
end;

procedure THeaderParser.FrameworkReference(Sender: TObject; AFramework: string;
  var Cancel: Boolean);
begin
  if FRefFrameworks.IndexOf(AFramework) < 0 then
  begin
    if Assigned(FOnFrameworkReference) then
      FOnFrameworkReference(Self, AFramework, Cancel);
  end;
end;

procedure THeaderParser.ParseFuncDeclaration(is_extern: Boolean;
  td: TTypeDeclaration; ProcName, postfix: string);
var
  decl: string;
  sparams, stype: string;
  need_nl: Boolean;
begin
  // indeed, this metod is overriden in TBodyParser
  need_nl := False;
  stype := td.TypeName;
  NextLex;
  sparams := ReadFuncParams;
  if is_extern then
  begin
    postfix := ' cdecl;' + postfix;
  end;
  if stype = 'void' then
    decl := 'procedure ' + ProcName + sparams + ';' + postfix
  else
    decl := 'function ' + ProcName + sparams + ': ' + stype + ';' + postfix;
  if not is_extern and cur_lex.eq(';') then
  begin
    decl := '//' + decl + ' { forward }';
    NextLex;
  end else
  if not is_extern and cur_lex.eq('{') then
  begin
    NextLex;
    decl := '//' + decl + ' { declaration of helper function }';
    SkipToLex('}', 1);
  end else
  if cur_lex.eq('__asm') then
  begin
    decl := '//' + decl + ' { __asm }';
    SkipToLex(';')
  end else
  if not cur_lex.eq(';') then
  begin
    raise Exception.Create('what is it??');
  end else
    need_nl := True;
  DestAdd(usProcs, decl);
  if need_nl then NextLex;
end;

procedure THeaderParser.ParseNext;
begin
  if cur_lex.eq(';') then
  begin
    NextLex; // empty instruction ";;", e.g. CoreGraphics.h
    Exit;
  end;
  if cur_lex.LexType <> ltSymbol then
    raise Exception.Create('ERROR');
  if cur_lex.eq('typedef') then
  begin
    NextLex;
    TypeDef;
  end else
  if cur_lex.eq('enum') then
  begin
    NextLex;
    EnumToConsts;
  end else
  if cur_lex.eq('struct') then
  begin
    NextLex;
    ParseStructUnion(False);
  end else
  if cur_lex.eq('union') then
  begin
    NextLex;
    ParseStructUnion(True);
  end else
    VarProcDefinition;
end;

procedure THeaderParser.AddFromDestUS(dest: TStrings; usec: TUnitSection);

  procedure FixComments(sl: TStringList);
  begin
    // remove comments from end
    while (sl.Count >= 3) and (sl[sl.Count - 1] = '') and (sl[sl.Count - 3] = '')
    and (Copy(sl[sl.Count - 2], 1, 5) = '(*** ') do
    begin
      sl.Delete(sl.Count - 1);
      sl.Delete(sl.Count - 1);
    end;
    while (sl.Count > 0) and (sl[sl.Count - 1] = '') do
      sl.Delete(sl.Count - 1);
  end;

begin
  FixComments(FDest[usec] as TStringList);
  if FDest[usec].Count > 0 then
  begin
    if usec <> usProcs then
      dest.Add(UnitSectionStr[usec]);
    dest.AddStrings(FDest[usec]);
    AddClearLine_(dest);
  end;
end;

procedure THeaderParser.AddConstDeclarations(dest: TStrings);
begin
  AddFromDestUS(dest, usConst);
end;

procedure THeaderParser.AddTypeDeclarations(dest: TStrings);

  function ExtractIncludeFileName(const fname: string): string;
  var i: Integer;
  begin
    i := Pos(PathDelim + 'include' + PathDelim, fname);
    if i > 0 then
      Result := Copy(fname, i + 9, MaxInt)
    else
      Result := ExtractFileName(fname);
  end;

  function MaxDepIndex(td: TTypeDeclaration): Integer;
  var i, j: Integer;
  begin
    Result := -1;
    if td.IsClass then Exit;
    if (td.FullDecl.Count = 1) and (Pos(' = ^', td.FullDecl[0]) > 0) then Exit;
    for i := 0 to td.DependCount - 1 do
    begin
      if td.Depends[i].IsClass then Continue;
      j := FTypes.IndexOf(td.Depends[i]);
      if Result < j then
        Result := j;
    end;
  end;

var
  i, k: Integer;
  ml: Boolean;
  td: TTypeDeclaration;
  fn, str, cur_pr: string;
begin
  dest.Add('type');
  fn := '';
  ml := False;
  i := 0;
  while i < FTypes.Count do
  begin
    td := TTypeDeclaration(FTypes[i]);
    k := MaxDepIndex(td);
    if k > i then
    begin
      if MaxDepIndex(TTypeDeclaration(FTypes[k])) = i then
        raise Exception.CreateFmt('Circular references for types "%s" <-> "%s"',
          [td.TypeName, TTypeDeclaration(FTypes[k]).TypeName]);
      FTypes.Move(k, i);
      Continue;
    end;
    Inc(i);
  end;
  cur_pr := '';
  for i := 0 to FTypes.Count - 1 do
  begin
    td := TTypeDeclaration(FTypes[i]);
    if not td.Extern and (td.DeclFileName <> fn) then
    begin
      AddClearLine_(dest);
      dest.Add('(*** translation from ' + ExtractFileName(td.DeclFileName) + ' ***)');
      fn := td.DeclFileName;
      AddClearLine_(dest);
    end;
    ml := td.FullDecl.Count > 1;
    if ml and (i > 0) then
      AddClearLine_(dest);
    if td.FullDecl.Count = 0 then td.MakeEmptyRecord;
    if td.Extern and (td.FullDecl.Count > 0) and (Pos('{', td.FullDecl[0]) = 0) then
    begin
      str := td.FullDecl[0];
      str := str + StringOfChar(' ', 50 - Length(str)) + ' { '
        + ExtractIncludeFileName(td.DeclFileName) + ' }';
      td.FullDecl[0] := str;
    end;
    if td.IsRecord and (td.FullDecl.Count > 1) then
      if cur_pr <> td.PackRecords then
      begin
        cur_pr := td.PackRecords;
        dest.Add('  ' + cur_pr);
      end;
    dest.AddStrings(td.FullDecl);
    if ml then dest.Add('');
  end;
  AddClearLine_(dest);
end;

procedure THeaderParser.AddVarDeclarations(dest: TStrings);
begin
  AddFromDestUS(dest, usVar);
  if FDest[usVar].Count > 0 then
    FAdditionalDirectives.Add('{$modeswitch cvar}');
end;

procedure THeaderParser.AddProcDeclarations(dest: TStrings);
begin
  AddFromDestUS(dest, usProcs);
end;

procedure THeaderParser.DestAdd(unitSec: TUnitSection; const str: string);
begin
  if (unitSec = usType) and (FTypesStack.Count > 0) then
  begin
    {$ifdef verbose}
    WriteLn(str);
    {$endif}
    TTypeDeclaration(FTypesStack[0]).FullDecl.Add(str);
    Exit;
  end;
  if not Scanner.CurHeaderBelongsToFramework then
  begin
    {$ifdef verbose}
    WriteLn('-', str);
    {$endif}
    Exit;
  end;
  {$ifdef verbose}
  WriteLn(str);
  {$endif}
  FDest[unitSec].Add(str);
end;

procedure THeaderParser.DestAddClearLine(unitSec: TUnitSection);
begin
  if Scanner.CurHeaderBelongsToFramework then
    AddClearLine_(FDest[unitSec])
end;

procedure THeaderParser.ParseVarDeclaration(is_extern: Boolean;
  td: TTypeDeclaration; OrigVarName, postfix: string; is_const: Boolean);
var
  VarName, typ: string;
begin
  // it is VAR definition
  VarName := FixFPCreserv(OrigVarName);
  if not is_extern or Scanner.CurHeaderIsSystem then
  begin
    SkipToLex(';');
    Exit;
  end;
  repeat
    typ := td.TypeName;
    postfix := ' cvar;' + postfix;
    // external const
    if is_const then postfix := postfix + ' { const }';
    if cur_lex.eq('[') then
      typ := ReadArrayBounds(td, False);
    DestAdd(usVar, '  ' + VarName + ': ' + typ + ';' + postfix);
    postfix := ' external;';

    if cur_lex.eq(',') then
    begin
      NextLex;
      VarName := FixFPCreserv(cur_lex.AsString);
      if is_extern and (VarName <> cur_lex.AsString) then
        postfix := ' external name ''' + cur_lex.AsString + ''';';
      NextLex;
    end else
    if cur_lex.eq(';') then
      Break
    else
      raise Exception.Create('unexpected "' + cur_lex.AsString + '"');
  until False;
  NextLex;
end;

procedure THeaderParser.AddAdditionalTypes;
var
  i, j: Integer;
  td: TTypeDeclaration;
begin
  if FAdditionalTypes.Count = 0 then Exit;

  for i := 0 to FAdditionalTypes.Count - 1 do
  begin
    td := TTypeDeclaration(FAdditionalTypes[i]);
    if td.Avail then Continue;
    if td.NeedUnit = '' then
      for j := 0 to td.DependCount - 1 do
        if not td.Depends[j].Avail then
          raise Exception.Create('type "' + td.Depends[j].TypeName + '" not allowed!');
    if td.NeedUnit = '*' then
      raise Exception.Create('type must be already available!');
    if td.NeedUnit = '' then // type from sys header that is not in parsed frameworks yet
    begin
      if FTypes.IndexOf(td) < 0 then
      begin
        FTypes.Insert(FBeforeIndex, td);
        Inc(FBeforeIndex);
      end;
      td.MakeAvail;
    end else begin
      // td.NeedUnit need to be added to uses clause
      if FRefFrameworks.IndexOf(td.NeedUnit) >= 0 then
        raise Exception.Create('unit "' + td.NeedUnit + '" already in used!');
      if td.NeedUnit <> Scanner.CurFrameworkUnit then
        FRefFrameworks.Add(td.NeedUnit);
      FDeclaredTypes.MakeAvailable(td.NeedUnit);
    end;
  end;

  FAdditionalTypes.Clear;
end;

function THeaderParser.DeclareType(OrigTypeName, typeDeclaration: string;
  withPointer: Boolean): TTypeDeclaration;
begin
  Result := FDeclaredTypes.DeclareType(OrigTypeName, typeDeclaration, withPointer, Scanner);
end;

function THeaderParser.DeclarePointerType(OrigTypeName: string): TTypeDeclaration;
begin
  Result := FDeclaredTypes.DeclarePointerType(OrigTypeName, Scanner);
end;

procedure THeaderParser.PushType(td: TTypeDeclaration);
begin
  FTypesStack.Insert(0, td);
end;

procedure THeaderParser.PushPointerType(td: TTypeDeclaration);
begin
  PushType(FDeclaredTypes.GetPointerTypeFor(td));
end;

procedure THeaderParser.PopType;
var
  td: TTypeDeclaration;
begin
  if Scanner.CurHeaderBelongsToFramework then
  begin
    td := TTypeDeclaration(FTypesStack[0]);
    if FTypes.IndexOf(td) < 0 then
      FTypes.Add(td);
  end;
  FTypesStack.Delete(0);
end;

procedure THeaderParser.ErrorLexExpected(lstr: string);
begin
  raise Exception.CreateFmt('"%s" expected but "%s" found!', [lstr, cur_lex.AsString]);
end;

procedure THeaderParser.ErrorLexExpectedIn(lstr, context: string);
begin
  raise Exception.CreateFmt('"%s" expected but "%s" found in %s!', [lstr, cur_lex.AsString, context]);
end;

function THeaderParser.FixNumber(str: string): string;
begin
  if UpCase(str[Length(str)]) = 'L' then
    str := Copy(str, 1, Length(str) - 1);
  if UpCase(str[Length(str)]) = 'L' then
    str := Copy(str, 1, Length(str) - 1);
  if UpCase(str[Length(str)]) = 'U' then
    str := Copy(str, 1, Length(str) - 1);
  if (Length(str) >= 2) and (str[1] = '0') and (UpCase(str[2]) = 'X') then
  begin
    Delete(str, 1, 1);
    str[1] := '$';
  end;
  Result := str;
end;

procedure THeaderParser.InitPredefinedSymbols;
begin
  FPredefinedSymbols.Sorted := True;
  FPredefinedSymbols.Duplicates := dupError;
  FPredefinedSymbols.CaseSensitive := False;  // TRUE, FALSE
  FPredefinedSymbols.Add('pi');
  FPredefinedSymbols.Add('sqrt');
  FPredefinedSymbols.Add('Byte');
end;

function THeaderParser.IsPredefined(name: string): Boolean;
var
  dt: TTypeDeclaration;
begin
  Result := FPredefinedSymbols.IndexOf(name) >= 0;
  if not Result then
  begin
    dt := FDeclaredTypes.GetTypeByCName(name);
    Result := Assigned(dt) and (ExtractFileExt(dt.DeclFileName) <> '.h');
  end;
end;

function THeaderParser.NextLex: TLex;
begin
  cur_lex := Scanner.GetNextLex;
  Exit(cur_lex);
end;

function THeaderParser.DeclareAuxType(OrigTypeName, typeDeclaration: string): TTypeDeclaration;
begin
  Result := FDeclaredTypes.DeclareAuxType(OrigTypeName, typeDeclaration, Scanner);
end;

procedure THeaderParser.NewAlias(td: TTypeDeclaration);
var
  bPtr: Boolean;
  ntyp: TTypeDeclaration;
  typeStr, aliasStr, bound: string;
  i: Integer;
  ptd: TTypeDeclaration;
begin
  bPtr := False;
  if cur_lex.eq('*') then
  begin
    bPtr := True;
    NextLex;
  end;
  if cur_lex.LexType <> ltSymbol then
    ErrorLexExpected('identifier');
  // new alias for declared struct
  aliasStr := cur_lex.AsString;
  NextLex;
  if not IsPredefined(aliasStr) then
  begin
    typeStr := td.TypeName;
    if cur_lex.eq('[') then
    begin
      NextLex;
      bound := ReadConstExpr;
      if TryStrToInt(bound, i) then
      begin
        Dec(i);
        if i > 0 then bound := IntToStr(i)
        else raise Exception.Create('array of 0 elements');
      end else
        bound := '(' + bound + ') - 1';
      if bPtr then
      begin
        ptd := FDeclaredTypes.GetTypeByCName(td.OrigTypeName + '*');
        if ptd = nil then
        begin
          ptd := DeclarePointerType(td.OrigTypeName);
          ptd.DependOn(td);
          PushType(ptd);
          DestAdd(usType, '  ' + ptd.TypeName + ' = ^' + td.TypeName + ';');
          PopType;
        end;
        typeStr := ptd.TypeName;
      end;
      ntyp := DeclareType(aliasStr, typeStr, False);
      PushType(ntyp);
      DestAdd(usType, '  ' + ntyp.TypeName + ' = array[0..' + bound + '] of ' + typeStr + ';');
      PopType;
      if not cur_lex.eq(']') then
        ErrorLexExpected(']');
      NextLex;
    end else
    if bPtr then
    begin
      ntyp := DeclareType(aliasStr, '^' + typeStr, False);
      PushType(ntyp);
      DestAdd(usType, '  ' + ntyp.TypeName + ' = ^' + typeStr + ';');
      PopType;
    end else begin
      ntyp := DeclareType(aliasStr, typeStr, True);
      PushType(ntyp);
      DestAdd(usType, '  ' + ntyp.TypeName + ' = ' + typeStr + ';');
      PopType;
      PushPointerType(ntyp);
      DestAdd(usType, '  ' + FDeclaredTypes.GetPointerTypeFor(ntyp).TypeName + ' = ^' + ntyp.TypeName + ';');
      PopType;
    end;
    ntyp.DependOn(td);
  end;
  while not cur_lex.eq(';') do
  begin
    if not cur_lex.eq(',') then ErrorLexExpected(',');
    NextLex;
    bPtr := False;
    // typedef <type1> t1, *t2;  { GSS/gssapi.h : struct gss_OID_desc_struct }
    if cur_lex.eq('*') then
    begin
      bPtr := True;
      NextLex;
    end;
    if cur_lex.LexType <> ltSymbol then ErrorLexExpected('identifier');
    if bPtr then
    begin
      ntyp := DeclareType(cur_lex.AsString, '^' + td.TypeName, False);
      PushType(ntyp);
      DestAdd(usType, '  ' + ntyp.TypeName + ' = ^' + td.TypeName + ';');
      PopType;
    end else begin
      ntyp := DeclareType(cur_lex.AsString , td.TypeName, False);
      PushType(ntyp);
      DestAdd(usType, '  ' + ntyp.TypeName + ' = ' + td.TypeName + ';');
      PopType;
    end;
    NextLex;
  end;
  if not cur_lex.eq(';') then
    ErrorLexExpected(';');
  NextLex;
end;

procedure THeaderParser.TypedefStruct;
var
  bPtr: Boolean;
  typeStr, sTag: string;
  typ, ptyp: TTypeDeclaration;
  bp: Boolean;
  tmp: TTypeDeclaration;
begin
  if cur_lex.LexType = ltSymbol then
  begin
    sTag := cur_lex.AsString;
    NextLex;
    typ := FDeclaredTypes.GetTypeByCName('struct ' + sTag);
    if Assigned(typ) and (typ.FullDecl.Count > 0) then
    begin
      NewAlias(typ);
      Exit;
    end else begin
      // undeclared struct with tag
      // declare TsTag for "struct tag"
      if not Assigned(typ) then
      begin
        typ := DeclareType('struct ' + sTag, 'record...', True);
        PushPointerType(typ);
        DestAdd(usType, '  ' + FDeclaredTypes.GetPointerTypeFor(typ).TypeName + ' = ^' + typ.TypeName + ';');
        PopType;
      end;
      PushType(typ);
      if not cur_lex.eq('*') then
      begin
        DestAdd(usType, '  ' + typ.TypeName + ' = record');
        ParseStructContent(bp);
        if bp then
          typ.FullDecl[0] := '  ' + typ.TypeName + ' = bitpacked record';
      end;
      PopType;
      NewAlias(typ);
      Exit;
    end;
  end;
  // struct without tag
  tmp := TTypeDeclaration.Create('??', '??', '??', '??');

  FTypesStack.Insert(0, tmp);
  DestAdd(usType, '  ??? = record'); // will be corrected later
  ParseStructContent(bp);
  bPtr := False;
  if cur_lex.eq('*') then
  begin
    bPtr := True;
    NextLex;
  end;
  if cur_lex.LexType <> ltSymbol then
    ErrorLexExpected('identifier');
  if bPtr then
  begin
    typeStr := 's' + cur_lex.AsString;
    typ := DeclareType(typeStr, 'record ...', False);
    typ := FDeclaredTypes.ReplaceTypeDef(typ, tmp);
    typeStr := typ.TypeName;
    if bp then
      tmp.FullDecl[0] := '  ' + typeStr + ' = bitpacked record'
    else
      tmp.FullDecl[0] := '  ' + typeStr + ' = record';
    ptyp := DeclareType(cur_lex.AsString, '^' + typeStr, False);
    PushType(ptyp);
    DestAdd(usType, '  ' + ptyp.TypeName + ' = ^' + typeStr + ';');
    PopType;
  end else begin
    typeStr := cur_lex.AsString;
    typ := DeclareType(typeStr, 'record ...', True);
    typ := FDeclaredTypes.ReplaceTypeDef(typ, tmp);
    typeStr := typ.TypeName;
    if bp then
      tmp.FullDecl[0] := '  ' + typeStr + ' = bitpacked record'
    else
      tmp.FullDecl[0] := '  ' + typeStr + ' = record';
    PushPointerType(typ);
    DestAdd(usType, '  ' + FDeclaredTypes.GetPointerTypeFor(typ).TypeName + ' = ^' + typeStr + ';');
    PopType;
  end;
  PopType;

  NextLex;
  if not cur_lex.eq(';') then
    ErrorLexExpected(';');
  NextLex;
  Exit;
end;

procedure THeaderParser.TypedefUnion;
var
  bPtr: Boolean;
  typeStr, sTag: string;
  typ, ptyp: TTypeDeclaration;
  tmp: TTypeDeclaration;
begin
  if cur_lex.LexType = ltSymbol then
  begin
    sTag := cur_lex.AsString;
    NextLex;
    typ := FDeclaredTypes.GetTypeByCName('union ' + sTag);
    if Assigned(typ) then
    begin
      NewAlias(typ);
      Exit;
    end else begin
      // undeclared union with tag
      // declare TuTag for "union tag"
      typ := DeclareType('union ' + sTag, 'record case...', True);
      PushPointerType(typ);
      DestAdd(usType, '  ' + FDeclaredTypes.GetPointerTypeFor(typ).TypeName + ' = ^' + typ.TypeName + ';');
      PopType;
      PushType(typ);
      //if not cur_lex.eq('*') then
      if cur_lex.eq('{') then
      begin
        DestAdd(usType, '  ' + typ.TypeName + ' = record');
        DestAdd(usType, '    case Integer of');
        ParseUnionContent;
      end;
      PopType;
      NewAlias(typ);
      Exit;
    end;
  end;
  // union without tag
  tmp := TTypeDeclaration.Create('??', '??', '??', '??');
  FTypesStack.Insert(0, tmp);
  DestAdd(usType, '  ??? = record'); // will be corrected later
  DestAdd(usType, '    case Integer of');
  ParseUnionContent;
  bPtr := False;
  if cur_lex.eq('*') then
  begin
    bPtr := True;
    NextLex;
  end;
  if cur_lex.LexType <> ltSymbol then
    ErrorLexExpected('identifier');
  if bPtr then
  begin
    typeStr := 's' + cur_lex.AsString;
    typ := DeclareType(typeStr, 'record ...', False);
    typ := FDeclaredTypes.ReplaceTypeDef(typ, tmp);
    typeStr := typ.TypeName;
    tmp.FullDecl[0] := '  ' + typeStr + ' = record';
    ptyp := DeclareType(cur_lex.AsString, '^' + typeStr, False);
    PushType(ptyp);
    DestAdd(usType, '  ' + ptyp.TypeName + ' = ^' + typeStr + ';');
    PopType;
  end else begin
    typeStr := cur_lex.AsString;
    typ := DeclareType(typeStr, 'record ...', True);
    typ := FDeclaredTypes.ReplaceTypeDef(typ, tmp);
    typeStr := typ.TypeName;
    tmp.FullDecl[0] := '  ' + typeStr + ' = record';
    PushPointerType(typ);
    DestAdd(usType, '  ' + FDeclaredTypes.GetPointerTypeFor(typ).TypeName + ' = ^' + typeStr + ';');
    PopType;
  end;
  PopType;

  NextLex;
  if not cur_lex.eq(';') then
    ErrorLexExpected(';');
  NextLex;
  Exit;
end;

procedure THeaderParser.TypedefEnum;
var s: string; typ, int_type: TTypeDeclaration;
begin
  int_type := FDeclaredTypes.GetTypeByCName('int');
  int_type.Use(FAdditionalTypes);
  if cur_lex.LexType = ltSymbol then
  begin
    s := 'enum ' + cur_lex.AsString;
    typ := DeclareType(s, 'cint', False);
    typ.DependOn(int_type);
    PushType(typ);
    DestAdd(usType, '  ' + typ.TypeName + ' = cint;');
    PopType;
    NextLex;
  end;
  if not cur_lex.eq('{') then
  begin
    if cur_lex.LexType <> ltSymbol then
      ErrorLexExpected('identifier');
    typ := DeclareType(cur_lex.AsString, 'cint', True);
    typ.DependOn(int_type);
    PushType(typ);
    DestAdd(usType, '  ' + typ.TypeName + ' = cint;');
    PushPointerType(typ);
    DestAdd(usType, '  ' + FDeclaredTypes.GetPointerTypeFor(typ).TypeName + ' = ^' + typ.TypeName + ';');
    PopType;
    PopType;
    typ.Use(FAdditionalTypes);
    NextLex;
    if not cur_lex.eq(';') then
      ErrorLexExpected(';');
    NextLex;
    Exit;
  end;
  NextLex;
  ParseEnum;
  if cur_lex.LexType <> ltSymbol then
    ErrorLexExpectedIn('identifier', 'typedef declaration');
  s := cur_lex.AsString;
  NextLex;
  typ := DeclareType(s, 'cint', False);
  typ.DependOn(int_type);
  PushType(typ);
  DestAdd(usType, '  ' + typ.TypeName + ' = cint;');
  PopType;
  if not cur_lex.eq(';') then
    ErrorLexExpected(';');
  NextLex;
end;

{$ifdef VERBOSE}
var totFiles: Integer = 0;
{$endif}
procedure THeaderParser.PrintCurrentFileName;
var
  i: TUnitSection;
begin
  if FCurFileName <> Scanner.CurFileName then
  begin
    FCurFileName := Scanner.CurFileName;
    for i := Low(FDest) to High(FDest) do
    begin
      if i = usType then Continue;
      DestAddClearLine(i);
      with FDest[i] do
      if (Count > 2) and (Strings[Count - 1] = '')
      and (Copy(Strings[Count - 2], 1, 5) = '(*** ') then
        Strings[Count - 2] := '(*** translation from ' + ExtractFileName(FCurFileName) + ' ***)'
      else begin
        DestAdd(i, '(*** translation from ' + ExtractFileName(FCurFileName) + ' ***)');
        DestAddClearLine(i);
      end;
    end;
    {$ifdef VERBOSE}
    Inc(totFiles);
    WriteLn(totFiles);
    {$endif}
  end;
end;

function THeaderParser.ReadArrayBounds(td: TTypeDeclaration;
  CreateNewTypes: Boolean): string;
var
  new_typ, ctyp, ptyp: string;
  i: Integer;
  params: string;
begin
  ctyp := td.OrigTypeName;
  ptyp := td.TypeName;
  while cur_lex.eq('[') do
  begin
    NextLex;
    if cur_lex.eq(']') then
    begin
      NextLex;
      // just Pointer
      if (FDeclaredTypes.GetTypeByCName(ctyp + '*') = nil) and CreateNewTypes then
        DeclarePointerType(ctyp);
      ctyp := ctyp + '*';
    end else begin
      params := cur_lex.AsString;
      NextLex;
      while not cur_lex.eq(']') do
      begin
        if cur_lex.eq('<<') then
          params := params + ' shl'
        else
        if cur_lex.eq('>>') then
          params := params + ' shr'
        else
          params := params + ' ' + cur_lex.AsString;
        NextLex;
      end;
      while (Length(params) > 1) and (params[1] = '(') and (params[Length(params)] = ')') do
        params := Trim(Copy(params, 2, Length(params) - 2));
      if TryStrToInt(params, i) then
      begin
        if i > 0 then
          ptyp := 'array [0..' + IntToStr(i - 1) + '] of ' + ptyp
        else
          ptyp := 'record end'; // array of 0 elems
        ctyp := ctyp + '[' + IntToStr(i) + ']';
      end else begin
        i := -1;
        ptyp := 'array [0..(' + params + ') - 1] of ' + ptyp;
        ctyp := ctyp + '[' + params + ']';
      end;
      if CreateNewTypes then
      begin
        if i > 0 then
        begin
          td := FDeclaredTypes.GetTypeByCName(ctyp);
          if Assigned(td) then
          begin
            td.Use(FAdditionalTypes);
          end else begin
            td := DeclareAuxType(ctyp, ptyp);
            PushType(td);
            DestAdd(usType, '  ' + td.TypeName + ' = ' + ptyp + ';');
            PopType;
          end;
        end else begin
          new_typ := FDeclaredTypes.GenNewName('__arr__');
          td := DeclareType(new_typ, ptyp, False);
          PushType(td);
          DestAdd(usType, '  ' + td.TypeName + ' = ' + ptyp + ';');
          PopType;
        end;
        ctyp := td.OrigTypeName;
      end;

      if not cur_lex.eq(']') then
        ErrorLexExpected(']');
      NextLex;
    end;
  end;
  if CreateNewTypes then
    Result := ctyp
  else
    Result := ptyp;
end;

procedure THeaderParser.ReadModifiers(out is_const, is_inline: Boolean);
begin
  is_const := False;
  is_inline := False;
  repeat
    case cur_lex.AsString of
    'static': NextLex;
    '__inline', '__inline__', 'inline':
      begin
        is_inline := True;
        NextLex;
      end;
    'const':
      begin
        is_const := True;
        NextLex;
      end;
    else Break;
    end;
  until False;
end;

procedure THeaderParser.TypeDef;
var
  typeStr, aliasStr, str: string;
  td: TTypeDeclaration;
  makePointer: Boolean;
  deps: TFPList;
  i: Integer;
begin
  if cur_lex.eq('struct') then
  begin
    NextLex;
    TypedefStruct;
    Exit;
  end else
  if cur_lex.eq('union') then
  begin
    NextLex;
    TypedefUnion;
    Exit;
  end else
  if cur_lex.eq('enum') then
  begin
    NextLex;
    TypedefEnum;
    Exit;
  end;

  deps := TFPList.Create;
  try
    ReadDefinition(typeStr, aliasStr, False, deps);
    if aliasStr = '' then
      raise Exception.Create('aliasStr = ""');
    if not IsPredefined(aliasStr) then
    begin
      makePointer := (Copy(typeStr, 1, 10) <> 'procedure ')
        and (Copy(typeStr, 1, 9) <> 'function ');
      td := DeclareType(aliasStr, typeStr, makePointer);
      for i := 0 to deps.Count - 1 do
        td.DependOn(TTypeDeclaration(deps[i]));
      PushType(td);
      str := '  ' + td.TypeName + ' = ' + typeStr + ';';
      if (deps.Count = 1) and (TTypeDeclaration(deps[0]).OrigTypeName = 'void') then
      begin
        td.IsVoid := True;
        str := '//' + str;
      end;
      DestAdd(usType, str);
      PopType;
      if makePointer then
      begin
        PushPointerType(td);
        if td.IsVoid then
          DestAdd(usType, '  ' + FDeclaredTypes.GetPointerTypeFor(td).TypeName + ' = Pointer;')
        else
          DestAdd(usType, '  ' + FDeclaredTypes.GetPointerTypeFor(td).TypeName + ' = ^' + td.TypeName + ';');
        PopType;
      end;
    end;
  finally
    deps.Free;
  end;

  if not cur_lex.eq(';') then
    ErrorLexExpectedIn(';', 'typedef');
  NextLex;
end;

procedure THeaderParser.ParseStructContent(out is_bitpacked: Boolean);
var
  typ, ident: string;
  fldCnt, i: Integer;
begin
  is_bitpacked := False;
  if not cur_lex.eq('{') then
  begin
    if cur_lex.LexType <> ltSymbol then
      ErrorLexExpected('{');
    DestAdd(usType, '  end;');
    Exit;
  end;
  NextLex;
  fldCnt := 1;
  while not cur_lex.eq('}') do
  begin
    ReadDefinition(typ, ident, False);
    if cur_lex.eq(':') then
    begin
      if not is_bitpacked then
      begin
        is_bitpacked := True;
        // "record" => "bitpacked record" in caller side
      end;
      NextLex;
      if (cur_lex.LexType <> ltNumber) or not TryStrToInt(cur_lex.AsString, i) then
        ErrorLexExpected('number');
      typ := '0..' + IntToStr(1 shl i - 1);
      DestAdd(usType, '    ' + FixFPCreserv(ident) + ': ' + typ + ';');
      NextLex;
      while cur_lex.eq(',') do
      begin
        NextLex;
        if cur_lex.LexType = ltSymbol then
        begin
          ident := FixFPCreserv(cur_lex.AsString);
          NextLex;
        end else begin
          ident := '__fld__' + IntToStr(fldCnt);
          Inc(fldCnt);
        end;
        if not cur_lex.eq(':') then
          ErrorLexExpected(':');
        NextLex;
        if (cur_lex.LexType <> ltNumber) or not TryStrToInt(cur_lex.AsString, i) then
          ErrorLexExpected('number');
        typ := '0..' + IntToStr(1 shl i - 1);
        DestAdd(usType, '    ' + FixFPCreserv(ident) + ': ' + typ + ';');
        NextLex;
      end;
      if not cur_lex.eq(';') then
        ErrorLexExpected(';');
      NextLex;
      Continue;
    end;
    DestAdd(usType, '    ' + FixFPCreserv(ident) + ': ' + typ + ';');
    while cur_lex.eq(',') do
    begin
      NextLex;
      if cur_lex.LexType <> ltSymbol then
        ErrorLexExpected('identifier');
      ident := cur_lex.AsString;
      NextLex;
      DestAdd(usType, '    ' + FixFPCreserv(ident) + ': ' + typ + ';');
    end;
    if not cur_lex.eq(';') then
      ErrorLexExpected(';');
    NextLex;
  end;
  DestAdd(usType, '  end;');
  NextLex;
end;

procedure THeaderParser.ParseUnionContent;
var typ, ident, s: string; i, b: Integer; td: TTypeDeclaration;
begin
  if not cur_lex.eq('{') then
    ErrorLexExpected('{');
  NextLex;
  i := 1;
  while not cur_lex.eq('}') do
  begin
    ReadDefinition(typ, ident, False);
    if ident = '' then
    begin
      td := FDeclaredTypes.GetTypeByPasName(typ);
      if Pos('record', td.Declaration) > 0 then
      begin
        DestAdd(usType, '    ' + IntToStr(i) + ': (');
        for b := 1 to td.FullDecl.Count - 2 do
          DestAdd(usType, '    ' + td.FullDecl[b]);
        DestAdd(usType, '      ' + ');');
        Inc(i);
        { TODO : remove unneeded T__recXX & P_recXX }
        if not cur_lex.eq(';') then
          ErrorLexExpected(';');
        NextLex;
        Continue;
      end else
      ident := 'fld' + IntToStr(i);
    end;
    if cur_lex.eq('[') then
    begin
      NextLex;
      s := ReadConstExpr;
      if TryStrToInt(s, b) then
      begin
        if b > 0 then
          s := IntToStr(b - 1)
        else
          raise Exception.Create('array bound is ' + IntToStr(b));
      end else s := '(' + s + ') - 1';
      s := 'array [0..' + s + '] of ';
      if not cur_lex.eq(']') then
        ErrorLexExpected(']');
      NextLex;
    end;
    { * beutify that breaks GetExprType *
    if Copy(typ, 1, 5) = 'array' then
    begin
      s := Copy(s, 1, Pos(']', s) - 1) + ', ';
      Delete(typ, 1, Pos('[', typ));
    end; }
    DestAdd(usType, '    ' + IntToStr(i) + ': (' + FixFPCreserv(ident) + ': ' + s + typ + ';);');
    Inc(i);
    if not cur_lex.eq(';') then
      ErrorLexExpected(';');
    NextLex;
  end;
  DestAdd(usType, '  end;');
  NextLex;
end;

function THeaderParser.BuiltInCType(out type_str: string): Boolean;
begin
  Result := False;
  type_str := '';
  while True do
  begin
    case cur_lex.AsString of
    'signed',
    'unsigned',
    'long',
    'short',
    'float',
    'char',
    'double',
    'int': type_str := type_str + ' ' + cur_lex.AsString;
    else Break;
    end;
    NextLex;
  end;
  if type_str <> '' then
  begin
    Delete(type_str, 1, 1);
    Result := True;
  end;
end;

function THeaderParser.ReadType: TTypeDeclaration;
var
  s: string;
  td: TTypeDeclaration;
begin
  if cur_lex.eq('const') then
    NextLex; // ignoring 'const' modifier
  if cur_lex.eq('volatile') then
    NextLex; // ignoring 'volatile' modifier

  if cur_lex.eq('struct') then
  begin
    NextLex;
    s := ReadStructDefinition
  end else
  if cur_lex.eq('union') then
  begin
    NextLex;
    s := ReadUnionDefinition
  end else
  if cur_lex.eq('enum') then
  begin
    NextLex; // enum name
    s := 'int';
    NextLex;
  end else
  if not BuiltInCType(s) then
  begin
    s := cur_lex.AsString;
    NextLex;
  end;

  if cur_lex.eq('const') then
    NextLex; // ignoring 'const' modifier
  if cur_lex.eq('volatile') then
    NextLex; // ignoring 'volatile' modifier

  if cur_lex.eq('*') then
  begin
    td := FDeclaredTypes.GetTypeByCName(s);
    if Assigned(td) and td.IsClass then
      NextLex; // all objcclasses are pointers as well
  end;

  // "s" is C type
  while cur_lex.eq('*') or (cur_lex.eq('[')) do
  begin
    if (FDeclaredTypes.GetTypeByCName(s) <> nil) and (FDeclaredTypes.GetTypeByCName(s + '*') = nil) then
    begin
      td := DeclarePointerType(s);
      PushType(td);
      DestAdd(usType, '  ' + td.TypeName + ' = ^' + FDeclaredTypes.GetTypeByCName(s).TypeName + ';');
      PopType;
      td.DependOn(FDeclaredTypes.GetTypeByCName(s));
    end;
    s := s + '*';
    if cur_lex.eq('[') then
    begin
      // only "[ ]" is allowed here!
      NextLex;
      if cur_lex.LexType = ltNumber then
        NextLex; // [x] has no sens, so skip elements count
      if not cur_lex.eq(']') then
        ErrorLexExpected(']');
    end;
    NextLex;
    if cur_lex.eq('const') then
      NextLex;
    if cur_lex.eq('volatile') then
      NextLex;
  end;

  Result := FDeclaredTypes.GetTypeByCName(s);
  if Result = nil then
  begin
    WriteLn('Type "', s, '" not declared!');
    raise Exception.Create('Type "' + s + '" not declared!');
  end;

  if FTypesStack.Count > 0 then
    TTypeDeclaration(FTypesStack[0]).DependOn(Result);
  if Scanner.CurHeaderBelongsToFramework then Result.Use(FAdditionalTypes);
end;

function THeaderParser.ReadFuncParams(Dependencies: TFPList): string;
var t, paramName: string; paramCounter: Integer; typ: TTypeDeclaration;
begin
  paramCounter := 0;
  Result := '';
  while not cur_lex.eq(')') do
  begin
    Inc(paramCounter);
    if cur_lex.eq('...') then
    begin
      Result := Result + '; args: array of const';
      NextLex;
      Continue;
    end;
    if cur_lex.eq('out') then
      NextLex;
    ReadDefinition(t, paramName, True);
    if (t = 'void') and (paramName = '') then
    begin
      if cur_lex.eq(')') then
      begin
        Result := '';
        Break;
      end;
      raise Exception.Create('"func(void)" ?');
    end;
    if (paramName = '') and (cur_lex.eq(',') or cur_lex.eq(')')) then
      paramName := 'param_' + IntToStr(paramCounter);
    if Result <> '' then
      Result := Result + '; ';
    typ := FDeclaredTypes.GetTypeByCName(t);
    if Assigned(Dependencies) then Dependencies.Add(typ);
    Result := Result + FixFPCreserv(paramName) + ': ' + typ.TypeName;
    if cur_lex.eq(',') then NextLex;
  end;
  NextLex;
  Result := '(' + Result + ')';
end;

function THeaderParser.ReadConstExpr: string;

  function CharsToNumber(s: string): string;
  var i: Integer; c: Cardinal;
  begin
    Delete(s, 1, 1);
    Delete(s, Length(s), 1);
    i := 1;
    while i < Length(s) do
      if s[i] = '\' then
      begin
        Delete(s, i, 1);
        case s[i] of
        'n': s[i] := #10;
        'r': s[i] := #13;
        end;
        Inc(i);
      end else Inc(i);
    if not (Length(s) in [1, 4]) then
      raise Exception.Create('[CharsToNumber] str length = ' + IntToStr(Length(s)));
    c := 0;
    for i := Length(s) downto 1 do
      c := c shl 8 or Byte(s[i]);
    Result := IntToStr(c) + ' { ''' + s + ''' }';
  end;

var
  ll: TLex; str: string; i, k: Integer;
  td: TTypeDeclaration;
begin
  Result := '';
  // from CVPixelBuffer.h (CoreVideo framework):
  //   kCVPixelFormatType_16LE555 = 'L555'
  // must be:
  //   kCVPixelFormatType_16LE555 = Ord('5') << 24 + Ord('5') << 16 + Ord('5') << 8 + Ord('L')
  if (cur_lex.LexType = ltDelim) and (Length(cur_lex.AsString) > 1)
  and (cur_lex.AsString[1] = '''') then
  begin
    Result := CharsToNumber(cur_lex.AsString);
    NextLex;
    Exit;
  end;
  if cur_lex.eq('(') then
  begin
    // type cast?
    NextLex;
    if not BuiltInCType(Result) then
    begin
      Result := cur_lex.AsString;
      td := FDeclaredTypes.GetTypeByCName(cur_lex.AsString);
      if Assigned(td) then
        NextLex;
    end else
      td := FDeclaredTypes.GetTypeByCName(Result);
    if Assigned(td) then
    begin
      td.Use(FAdditionalTypes);
      if not cur_lex.eq(')') then
        ErrorLexExpected(')');
      NextLex;
      Result := td.TypeName + '(' + ReadConstExpr() + ')';
      Exit;
    end else
      Result := '(';
  end;
  if cur_lex.eq('~') then
    Result := Result + 'not '
  else
  if cur_lex.LexType = ltNumber then
    Result := Result + FixNumber(cur_lex.AsString)
  else
    Result := Result + cur_lex.AsString;
  ll := cur_lex;
  NextLex;
  while not (cur_lex.eq(',') or cur_lex.eq('}') or cur_lex.eq(']')) do
  begin
    if (ll.LexType <> ltDelim) and (cur_lex.LexType <> ltDelim) then
      Result := Result + ' ';
    if cur_lex.eq('~') then
      Result := Result + ' not '
    else if cur_lex.eq('<<') then
      Result := Result + ' shl '
    else if cur_lex.eq('>>') then
      Result := Result + ' shr '
    else if cur_lex.eq('|') then
      Result := Result + ' or '
    else begin
      str := cur_lex.AsString;
      if cur_lex.LexType = ltNumber then
        str := FixNumber(str);
      Result := Result + str;
    end;
    ll := cur_lex;
    NextLex;
  end;
  Result := Trim(Result);
  if (Result <> '') and (Result[1] = '(') then
  begin
    k := 1;
    for i := 2 to Length(Result) do
      if Result[i] = '(' then Inc(k) else
      if Result[i] = ')' then
      begin
        Dec(k);
        if (k = 0) and (i = Length(Result)) then
          Result := Copy(Result, 2, Length(Result) - 2)
        else Break;
      end;
  end;
end;

function THeaderParser.ReadStructDefinition: string;
var
  stype: string;
  bp: Boolean;
  td: TTypeDeclaration;
begin
  if cur_lex.LexType = ltSymbol then
  begin
    Result := 'struct ' + cur_lex.AsString;
    NextLex;
    td := FDeclaredTypes.GetTypeByCName(Result);
    if Assigned(td) and (td.FullDecl.Count > 0) then Exit;
    if cur_lex.eq('*') then
    begin
      if not Assigned(td) then
      begin
        td := DeclareType(Result, 'record', True);
        PushType(td);
        PushPointerType(td);
        DestAdd(usType, '  ' + FDeclaredTypes.GetPointerTypeFor(td).TypeName + ' = ^' + td.TypeName + ';');
        PopType;
      end else PushType(td);
      PopType;
      Exit;
    end else begin
      if not Assigned(td) then
        raise Exception.Create('not yet implemented: undeclared struct with tag');
    end;
  end;
  td := DeclareType(FDeclaredTypes.GenNewName('__rec'), 'record...', True);
  Result := td.OrigTypeName;
  stype := td.TypeName;
  PushPointerType(td);
  DestAdd(usType, '  ' + FDeclaredTypes.GetPointerTypeFor(td).TypeName + ' = ^' + stype + ';');
  PopType;
  PushType(td);
  DestAdd(usType, '  ' + stype + ' = record');
  ParseStructContent(bp);
  if bp then
    td.FullDecl[0] := '  ' + stype + ' = bitpacked record'
  else
    td.FullDecl[0] :=  '  ' + stype + ' = record';
  PopType;
end;

function THeaderParser.ReadUnionDefinition: string;
var
  td: TTypeDeclaration;
  typ, ident: string;
  c: Integer;
begin
  if cur_lex.LexType = ltSymbol then
  begin
    Result := 'union ' + cur_lex.AsString;
    NextLex;
    if FDeclaredTypes.GetTypeByCName(Result) <> nil then Exit;
    raise Exception.Create('not yet implemented: undeclared union with tag');
  end;
  if cur_lex.eq('{') then
  begin
    NextLex;
    td := FDeclaredTypes.DeclareType(FDeclaredTypes.GenNewName('union __union'), 'record case...', False, Scanner);
    Result := td.OrigTypeName;
    PushType(td);
    DestAdd(usType, '  ' + td.TypeName + ' = record');
    DestAdd(usType, '    case Integer of');
    c := 0;
    while not cur_lex.eq('}') do
    begin
      ReadDefinition(typ, ident, True);
      DestAdd(usType, '    ' + IntToStr(c) + ': (' + ident + ': '
        + FDeclaredTypes.GetTypeByCName(typ).TypeName + ';);');
      Inc(c);
      if not cur_lex.eq(';') then
        ErrorLexExpected(';');
      NextLex;
    end;
    DestAdd(usType, '  end;');
    PopType;
    NextLex;
  end else
    raise Exception.Create('"{" expected in union declaration');
end;

procedure THeaderParser.ReadDefinition(out typ, ident: string;
  CreateNewTypes: Boolean; Dependencies: TFPList);
var
  params, s: string;
  i: Integer;
  td: TTypeDeclaration;
  blocks: Boolean;
begin
  td := ReadType;
  s := td.TypeName;
  if Assigned(Dependencies) then Dependencies.Add(td);
  typ := s;
  if cur_lex.LexType = ltSymbol then
  begin
    ident := cur_lex.AsString;
    NextLex;
    if cur_lex.eq('(') then
    begin
      // typedef void NSUncaughtExceptionHandler(NSException *exception);
      //   NSUncaughtExceptionHandler is function, but only NSUncaughtExceptionHandler* is used
      ident := ident + '*';
      NextLex;
      params := ReadFuncParams(Dependencies);
      if td.OrigTypeName = 'void' then
        s := 'procedure ' + params
      else
        s := 'function ' + params + ': ' + s;
      s := s + '; cdecl';
      typ := s;
    end;
  end else
  if cur_lex.eq('(') then
  begin
    // functional type definition
    // e.g.
    //  void           (|*__routine)(void *);
    NextLex;
    blocks := False;
    if cur_lex.eq('^') then
    begin
      // blocks -- although NS_BLOCKS=0
      blocks := True;
    end else
    if not cur_lex.eq('*') then
      raise Exception.Create('error in functional type: not pointer to function');
    NextLex;
    if cur_lex.LexType = ltSymbol then
    begin
      ident := cur_lex.AsString;
      NextLex;
    end else
      ident := '';
    if not cur_lex.eq(')') then
      ErrorLexExpected(')');
    NextLex;
    if not cur_lex.eq('(') then
      ErrorLexExpected('(');
    NextLex;
    params := ReadFuncParams(Dependencies);
    if td.OrigTypeName = 'void' then
      s := 'procedure ' + params
    else
      s := 'function ' + params + ': ' + s;
    s := s + '; cdecl';
    if blocks then
      s := s + ' { blocks }'; // indeed blocks are not supported (yet?)
    if CreateNewTypes then
    begin
      if ident = '' then
      begin
        ident := FDeclaredTypes.GenNewName('proc', '_proc');
        td := DeclareType(ident, s, False);
        ident := Copy(ident, 1, Length(ident) - 5);
      end else
        td := DeclareType(FDeclaredTypes.GenNewName(ident, '_proc'), s, False);
      if Assigned(Dependencies) then
        for i := 0 to Dependencies.Count - 1 do
          td.DependOn(TTypeDeclaration(Dependencies[i]));
      PushType(td);
      DestAdd(usType, '  ' + td.TypeName + ' = ' + s + ';');
      PopType;
      s := td.TypeName;
    end else typ := s;
  end;
  if CreateNewTypes then
    typ := td.OrigTypeName;
  if cur_lex.eq('[') then
    typ := ReadArrayBounds(td, CreateNewTypes);
end;

procedure THeaderParser.EnumToConsts;
begin
  if cur_lex.LexType = ltSymbol then
    NextLex;
  NextLex; // "{"
  ParseEnum;
  while Assigned(cur_lex) and not cur_lex.eq(';') do NextLex;
  NextLex;
end;

procedure THeaderParser.ParseEnum;
var localVal: Integer; constName, constValue, str: string;
begin
  localVal := 0;
  while not cur_lex.eq('}') do
  begin
    constName := cur_lex.AsString;
    NextLex;
    if cur_lex.eq('=') then
    begin
      NextLex;
      constValue := ReadConstExpr;
      TryStrToInt(constValue, localVal);
    end else constValue := IntToStr(localVal);
    if not cur_lex.eq('}') then NextLex; // "," or "}"
    str := '  ' + constName + ' = ' + constValue + ';';
    if FPredefinedSymbols.IndexOf(constName) >= 0 then
    begin
      str := '//' + str;
      DestAdd(usConst, str);
    end else
      DestAdd(usConst, str);
    Inc(localVal);
  end;
  NextLex;
end;

procedure THeaderParser.VarProcDefinition;
var
  is_extern, is_const, is_inline: Boolean;
  postfix, VarProcName: string;
  td: TTypeDeclaration;
  OrigVarProcName: string;
begin
  postfix := '';
  is_extern := False;
  if cur_lex.eq('extern') then
  begin
    is_extern := True;
    postfix := ' external;';
    NextLex;
  end;
  if cur_lex.eq('@') then
  begin
    // extern @interface in UIKit 3.0 (UILocalizedIndexedCollation.h)
    Exit;
  end;
  ReadModifiers(is_const, is_inline);
  td := ReadType;

  // FOUNDATION_EXPORT NSString * const NSBundleDidLoadNotification;
  if cur_lex.eq('const') then
  begin
    is_const := True;
    NextLex;
  end;

  OrigVarProcName := cur_lex.AsString;
  if cur_lex.LexType <> ltSymbol then
  begin
    // in sys/signal.h:
    // void	(*signal(int, void (*)(int)))(int);
    // TODO : function signal(p1: int; p2: procedure (p3: int)): procedure (p3: int);
    SkipToLex(';');
    Exit;
  end;
  VarProcName := FixFPCreserv(OrigVarProcName);
  if is_extern and (VarProcName <> cur_lex.AsString) then
    postfix := ' external name ''' + cur_lex.AsString + ''';';
  NextLex;
  if not cur_lex.eq('(') then
  begin
    // this is a var declaration
    ParseVarDeclaration(is_extern, td, OrigVarProcName, postfix, is_const);
  end else begin
    // this is a function definition/declaration
    if is_inline and not is_extern then
      postfix := postfix + ' inline;';
    ParseFuncDeclaration(is_extern, td, OrigVarProcName, postfix);
  end;
end;

procedure THeaderParser.DoAfterParse;
begin
  AddConstDeclarations(FRealDest);
  AddTypeDeclarations(FRealDest);
  AddVarDeclarations(FRealDest);
  AddProcDeclarations(FRealDest);

  CreateInterfaceUsesClause;
  AddAdditionalDirectives; // <- this should be after CreateInterfaceUsesClause
end;

function THeaderParser.GenerateFrameworkMainHeader(AFileMask: string): TStream;
var
  s: string;
  sr: TSearchRec;
  fr: string;
  i: Integer;
begin
  fr := '';
  i := Pos('.framework', AFileMask);
  if i > 0 then
  begin
    fr := Copy(AFileMask, 1, i - 1);
    while Pos(PathDelim, fr) > 0 do
      Delete(fr, 1, Pos(PathDelim, fr));
  end;
  if fr <> '' then fr := fr + '/';
  Result := TMemoryStream.Create;
  if FindFirst(AFileMask, faAnyFile and not faDirectory, sr) = 0 then
    repeat
      s := '#import <' + fr + sr.Name + '>' + sLineBreak;
      Result.Write(s[1], Length(s));
    until FindNext(sr) <> 0;
  FindClose(sr);
  Result.Position := 0;
end;

procedure THeaderParser.NestedFrameworkReference(Sender: TObject;
  AFramework: string; var Cancel: Boolean);
var
  s, fn: string;
  i, j: Integer;
begin
  { add $linkframework directive if needed }
  Cancel := False;
  WriteLn(AFramework);
  s := AFramework;
  i := Length(s);
  while (i > 0) and (s[i] <> PathDelim) do Dec(i);
  Dec(i);
  while (i > 0) and (s[i] <> PathDelim) do Dec(i);
  SetLength(s, i);
  Dec(i);
  while (i > 0) and (s[i] <> '.') do Dec(i);
  j := i;
  while (i > 1) and (s[i - 1] <> PathDelim) do Dec(i);
  fn := Copy(s, i, j - i);
  s := s + fn;
  if FileExists(s) then
  begin
    i := 0;
    while (i < FRealDest.Count) and (FRealDest[i] <> 'interface') do Inc(i);
    i := i + 2;
    while (i < FRealDest.Count) and (Copy(FRealDest[i], 1, 2) = '{$') do Inc(i);
    FRealDest.Insert(i, '{$linkframwork ' + fn + '}');
  end;
end;

procedure THeaderParser.AddAdditionalDirectives;
var
  i, j: Integer;
begin
  if FAdditionalDirectives.Count > 0 then
  begin
    i := 2;
    while (i < FRealDest.Count) and (Copy(FRealDest[i], 1, 2) <> '{$') do
      Inc(i);
    if i >= FRealDest.Count then
      raise Exception.Create('cannot find first compiler directive'); // there must be at least "{$mode objfpc}"
    Inc(i);
    // skip all directives
    while (i < FRealDest.Count) and (Copy(FRealDest[i], 1, 2) = '{$') do
      Inc(i);
    for j := 0 to FAdditionalDirectives.Count - 1 do
    begin
      FRealDest.Insert(i, FAdditionalDirectives[j]);
      Inc(i);
    end;
  end;
end;

procedure THeaderParser.CreateInterfaceUsesClause;
var
  u, str: string;
  i, j: Integer;
begin
  if FRefFrameworks.Count > 0 then
  begin
    i := 0;
    while (i < FRealDest.Count) and (FRealDest[i] <> 'interface') do
      Inc(i);
    if i >= FRealDest.Count then
      raise Exception.Create('"interface" not found in unit');
    Inc(i);
    FRealDest.Insert(i, ''); Inc(i);
    FRealDest.Insert(i, 'uses'); Inc(i);
    str := '';

    for j := 0 to FRefFrameworks.Count - 1 do
    begin
      u := FRefFrameworks[j];
      if u[1] = '{' then
      begin
        FAdditionalDirectives.Add(u);
        Continue;
      end;
      if Pos('/', u) > 0 then
      begin
        u := Copy(u, Pos('/', u) + 1, MaxInt);
        if Pos('.', u) > 0 then u := Copy(u, 1, Pos('.', u) - 1);
      end;
      if Length(str) + Length(u) < 80 - 2{indent} - 2{", "} then
      begin
        if str <> '' then str := str + ', ';
        str := str + u;
      end else begin
        str := '  ' + str + ',';
        FRealDest.Insert(i, str); Inc(i);
        str := u;
      end;
    end;
    str := '  ' + str + ';';
    FRealDest.Insert(i, str);
  end;
end;

procedure THeaderParser.ParseStructUnion(isUnion: Boolean);
var
  structName, recName: string;
  is_bitpacked: Boolean;
  td: TTypeDeclaration;
begin
  if cur_lex.LexType <> ltSymbol then
  begin
    // declaration of "struct/union" without tag
    // it's just vars declarations:
    //   stuct {...} x,y,z;
    // this vars have no sense
    SkipToLex(';');
    Exit;
  end;
  structName := cur_lex.AsString;
  if isUnion then
    structName := 'union ' + structName
  else
    structName := 'struct ' + structName;
  td := FDeclaredTypes.GetTypeByCName(structName);
  NextLex;
  if cur_lex.eq(';') then
  begin
    // something like
    //   struct S;
    if td = nil then
    begin
      // S is not declared yet
      td := DeclareType(structName, 'record', True);
      PushType(td);
      PopType; // to add "td" to types list
      PushPointerType(td);
      DestAdd(usType, '  ' + FDeclaredTypes.GetPointerTypeFor(td).TypeName + ' = ^' + td.TypeName + ';');
      PopType;
    end;
    NextLex;
    Exit;
  end;
  if not cur_lex.eq('{') then
  begin
    // "struct/union" declaration without "{...}"
    // it may be declaration like
    //   struct S x,y,z;
    //   struct S func(..);
    // where S is already declared
    SkipToLex(';');
    Exit;
  end;
  td := FDeclaredTypes.GetTypeByCName(structName); // redundant
  if Assigned(td) and td.IsEmptyRecord then
  begin
    td.RefillEmptyRecord;
    recName := td.TypeName;
  end else begin
    td := DeclareType(structName, 'record...', True);
    recName := td.TypeName;
    PushPointerType(td);
    DestAdd(usType, '  ' + FDeclaredTypes.GetPointerTypeFor(td).TypeName + ' = ^' + recName + ';');
    PopType;
  end;
  PushType(td);
  DestAdd(usType, '  ' + recName + ' = record');
  if isUnion then begin
    DestAdd(usType, '    case Integer of');
    ParseUnionContent;
  end else begin
    ParseStructContent(is_bitpacked);
    if is_bitpacked then
      td.FullDecl[0] := '  ' + recName + ' = bitpacked record';
  end;
  PopType;
  while not cur_lex.eq(';') do NextLex; // skip possible vars decls
  NextLex;
end;

procedure THeaderParser.SkipToLex(const lex: string; braket_lev: Integer = 0);
var k: Integer;
begin
  k := braket_lev;
  while Assigned(cur_lex) do
  begin
    if cur_lex.eq('{') then Inc(k) else
    if cur_lex.eq('}') then
    begin
      Dec(k);
      if (k <= 0) and cur_lex.eq(lex) then Break;
    end else
    if cur_lex.eq(lex) and (k = 0) then Break;
    NextLex;
  end;
  NextLex;
end;

procedure THeaderParser.StartParsing;
begin
  while cur_lex <> nil do
  begin
    FAdditionalTypes.Clear;
    PrintCurrentFileName;
    FBeforeIndex := FTypes.Count;
    ParseNext;
    if FAdditionalTypes.Count > 0 then
      AddAdditionalTypes;
  end;
end;

constructor THeaderParser.Create(AFileName: string);
var
  FStream: TStream;
  usec: TUnitSection;
begin
  FTypes := TFPList.Create;
  for usec := Low(TUnitSection) to High(TUnitSection) do
    FDest[usec] := TStringList.Create;
  FAdditionalTypes := TFPList.Create;
  FTypesStack := TFPList.Create;
  FDeclaredTypes := TDeclaredTypes.Create;
  FDeclaredTypes.InitPredefinedTypes;

  FBeforeIndex := 0;
  FCurFileName := '';
  { objc and other objects: classes, types, etc. }
  FPredefinedSymbols := TStringList.Create;
  InitPredefinedSymbols;
  FAdditionalDirectives := TStringList.Create;
  { for adding to uses clause }
  FRefFrameworks := TStringList.Create;
  FRefFrameworks.Sorted := True;
  FFileName := AFileName;
  if Pos('*', AFileName) > 0 then
    FStream := GenerateFrameworkMainHeader(AFileName)
  else
    FStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  Scanner := THeaderScanner.Create(AFileName, FStream);
  with Scanner do
  begin
    IncludeEnabled := True;

    AddDefine('__ENVIRONMENT_IPHONE_OS_VERSION_MIN_REQUIRED__ 30000'); // needed only for iOS 3.0
    //AddDefine('__IPHONE_OS_VERSION_MAX_ALLOWED 50000');
    AddDefine('__GNUC__ 1');
    AddDefine('__OBJC__ 1');
    AddDefine('__STDC__ 1'); // prevent "const" => "__const"
    AddDefine('__APPLE__ 1');
    AddDefine('__APPLE_CC__ 1');
    AddDefine('__LITTLE_ENDIAN__ 1'); // both i386 and arm
    AddDefine('__MACH__ 1');

    AddDefine('M_PI pi');
    AddDefine('isinf IsInfinite'); // to prevent macros
    // workaround for gles20 unavalability
    AddDefine('GL_TEXTURE_CUBE_MAP 0x8513');
    // workaround for ApplicationServices.framework
    AddDefine('kNilOptions 0');

    if DirectoryExists(Copy(AFileName, 1, Pos('Frameworks', AFileName) - 1)
      + '..' + PathDelim + '..' + PathDelim + 'usr' + PathDelim
      + 'include' + PathDelim + 'arm')
    then
      AddDefine('__arm__ 1')
    else
      AddDefine('__i386__ 1'); // simulator

    // GCC extensions:
    AddDefine('__builtin_constant_p(x) 1'); // it's used e.g. for replace
                                            // function call by macros to
                                            // perform the same operation at
                                            // compile time
    AddDefine('_OBJC_OBJC_H_'); // we already have translation of objc/objc.h
    //AddDefine('__BLOCKS__ 1'); // must it be "__BLOCKS__ 0"?
    AddDefine('__strong');

    OnFrameworkReference := @FrameworkReference;
    OnNestedFramework := @NestedFrameworkReference;
    OnDefinedConstant := @DefinedConstant;
    OnPragmaPack := @NewPragmaPack;
  end;
end;

destructor THeaderParser.Destroy;
var
  i: TUnitSection;
begin
  FTypes.Free;
  for i := Low(FDest) to High(FDest) do
    FDest[i].Free;
  FAdditionalTypes.Free;
  FTypesStack.Free;
  FDeclaredTypes.Free;
  FPredefinedSymbols.Free;
  FRefFrameworks.Free;
  FAdditionalDirectives.Free;
  Scanner.Free;
  inherited Destroy;
end;

function THeaderParser.DoParse(Dest: TStrings): Boolean;
begin
  Result := True;
  FRealDest := Dest;
  try
    NextLex;
    StartParsing;
  except
    on EAbort do Exit(False);
    on Exception do raise;
  end;
  DoAfterParse;
end;

procedure THeaderParser.RetrieveDefines(dest: TStrings);
begin
  Scanner.RetrieveDefines(dest);
end;

procedure THeaderParser.RetrieveImported(dest: TStrings);
begin
  Scanner.RetrieveImported(dest);
end;

procedure THeaderParser.RetrieveTypes;
begin
  FDeclaredTypes := nil;
end;

procedure THeaderParser.InitDefines(src: TStrings);
begin
  Scanner.InitDefines(src);
end;

procedure THeaderParser.InitImported(src: TStrings);
begin
  Scanner.InitImported(src);
end;

procedure THeaderParser.InitTypes(src: TDeclaredTypes);
begin
  FDeclaredTypes.Free;
  FDeclaredTypes := src;
end;

procedure THeaderParser.StoreDefines(src: TStrings);
begin
  Scanner.StoreDefines(src);
end;

end.

