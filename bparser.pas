{ extension of C header parser: C proc bodies parser }
unit bparser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, hparser, DeclTypes, DeclIds;

type

  { TExpr }

  TExpr = class
  strict private
    FdeclIds: TGlobDeclIds;
    FforAssignments: TStrings;
    FdeclTypes: TDeclaredTypes;
    Findent: Integer;
    procedure SetDeclIds(AValue: TGlobDeclIds);
    procedure SetDeclTypes(AValue: TDeclaredTypes);
    function IsFloat: Boolean;
  public
    op: string;
    args: array [0..2] of TExpr;
    prior: Integer; // it is Pascal op priority, not C
    (* priority  operations
     *  -1	  ?: ","
     *   0        #func num #id
     *   1        not ^ @ u+ u- . -> [] type_cast
     *   2        and shr shl * / mod
     *   3        or xor + -
     *   4        = <> < > <= >=
     *   5        := (+=,-=, ...)
     *)
    constructor Create(Aop: string; Aprior: Integer = 0);
    destructor Destroy; override;
    function AsCondition: string;
    function AsString(withBrackets: Boolean = False): string;
    function Orig: string;
    function GetType: TTypeDeclaration;
    function WithIf: Boolean;
    procedure SplitIf(out cond, true_part, false_part: TExpr);
    function Copy: TExpr;
    procedure Simplify;
    property declIds: TGlobDeclIds read FdeclIds write SetDeclIds;
    property declTypes: TDeclaredTypes read FdeclTypes write SetDeclTypes;
    property forAssignments: TStrings read FforAssignments write FforAssignments;
    property indent: Integer read Findent write Findent;
  end;

  TNeedUnitProc = procedure (AUnitName: string) of object;

  { TBodyParser }

  TBodyParser = class(THeaderParser)
  private
    FGlobDeclIds: TGlobDeclIds;
    FLastProcParseError: string;
    FCurFunction: string;
    FProcTypes: TStringList; // local proc types: <type_name, type_def>
    FParsedProcs,
    FProcBodies: TStringList;
    FImplUnits: TStringList;
    procedure CheckUndeclaredIdentifiers(expr: TExpr; OnNeedUnit: TNeedUnitProc);
    function ParseProcBody(procHeader: string): Boolean;
    procedure VarDeclaration(ityp: TTypeDeclaration; vars, body: TStrings);
    procedure ParseUnionVars(vars: TStrings);
    procedure ParseStatement(vars, body: TStrings; indent: Integer);
    procedure ParseAssignment(leftHand: string; body: TStrings);
    function ParseExpr: TExpr;
    procedure RecordFieldsAssignement(varName: string; td: TTypeDeclaration;
      expr: TExpr; indent: Integer; body: TStrings);
    procedure ReturnStatement(body: TStrings; indent: Integer);
    procedure SwitchStatement(vars, body: TStrings; indent: Integer);
    procedure IfStatement(vars, body: TStrings; indent: Integer);
    function GetUndeclaredIdentifier(e: TExpr; withLeft: Boolean;
      OnNeedUnit: TNeedUnitProc): TExpr;
    function ReadFuncParams: string;
    procedure Beautify(body: TStrings);
    procedure NewInterfaceUnit(AUnitName: string);
    procedure NewImplementationUnit(AUnitName: string);
  protected
    (*
     * E	-> 	 E11 { "," E}
     * E11      ->       E10 [[ = | += | -= | *= | /= | %= | &= | ^= | "|=" | <<= | >>= ] E]
     * E10      ->       E9 [ ? E : E ]
     * E9       ->       E8 { "||" E8}
     * E8       ->       E7 { && E7}
     * E7       ->       E6 { "|" E6}
     * E6       ->       E5 { ^ E5}
     * E5	->       E4 { & E4}
     * E4	->       E3 {[ == | != ] E3}
     * E3	->       E2 {[ < | <= | > | >= ] E2}
     * E2       ->       E1 {[ << | >> ] E1}
     * E1	-> 	 T {[ + | - ] T}
     * T	-> 	 F {[ * | / | "%" ] F}
     * F	-> 	 "sizeof" F_sizeof | [ ! | ~ | ++ | -- | + | - | * | & ] F0 | "@"<str> // last part => ocparser
     * F_sizeof ->       "(" <type> ")" | "(" E ")"
     * F0       ->       F1 { . <ident> | -> <ident> | "[" E "]" | "(" <params> ")" } | N
     * F1       ->       <ident> | "(" F2 | "[" F3   // part begins with "[" => ocparser
     * F2_type  ->       <type> ")" |
     * F2       ->       F2_type ")" "{" E "}" | F2_type ")" F | E ")"
     * F3       ->       E <ident>[ : E11 ] {<ident>:E11} {"," E11} "]"  // => ocparser
     *)
    function E_: TExpr; virtual;
    function E1: TExpr;
    function E10: TExpr;
    function E11: TExpr;
    function E2: TExpr;
    function E3: TExpr;
    function E4: TExpr;
    function E5: TExpr;
    function E6: TExpr;
    function E7: TExpr;
    function E8: TExpr;
    function E9: TExpr;
    function F_: TExpr; virtual;
    function F_sizeof: TExpr;
    function F0: TExpr;
    function F1: TExpr; virtual;
    function F2: TExpr;
    function F2_type(out expr: TExpr): Boolean;
    function T_: TExpr;

    procedure ParseEnum; override;
    procedure ParseFuncDeclaration(is_extern: Boolean; td: TTypeDeclaration;
      OrigName, postfix: string); override;
    procedure ParseVarDeclaration(is_extern: Boolean; td: TTypeDeclaration;
      OrigVarName, postfix: string; is_const: Boolean); override;
    procedure DefinedConstant(Sender: TObject; ConstName, ConstValue: string); override;
    procedure AddProcDeclarations(dest: TStrings); override;
    procedure DoAfterParse; override;
  public
    constructor Create(AFileName: string);
    destructor Destroy; override;
    procedure AddProcs(Dest: TStrings);
    procedure InitDeclIds(src: TGlobDeclIds);
  end;

implementation

uses hscanner;

type EUndeclaredIdentifier = class(Exception);

{ TBodyParser }

procedure TBodyParser.CheckUndeclaredIdentifiers(expr: TExpr;
  OnNeedUnit: TNeedUnitProc);
var undecl: TExpr; s: string;
begin
  // check all identifiers are declared
  undecl := GetUndeclaredIdentifier(expr, True, OnNeedUnit);
  if Assigned(undecl) then
  begin
    s := undecl.AsString;
    WriteLn('Identifier undeclared: "', s, '"');
    expr.Free;
    raise EUndeclaredIdentifier.Create('undeclared identifier "' + s + '"');
  end;
end;

function TBodyParser.ParseProcBody(procHeader: string): Boolean;
var
  i: Integer;
  vars, body: TStringList;
  td: TTypeDeclaration;
begin
  Result := False;
  { indeed we need orig param names, but func params fixed }
  { only by determinated FixFPCreserv(), so it can be      }
  { applied again to identifier. I hope there won't be     }
  { declarations like:                                     }
  {   int unit, Unit, UNIT;                                }
  //if FCurFunction = 'GLKMathDegreesToRadians' then
  //writeln;
  {$ifdef verbose}
  WriteLn; WriteLn(procHeader);
  {$endif}
  vars := TStringList.Create;
  body := TStringList.Create;
  FProcTypes := TStringList.Create;
  try
    try
      while not cur_lex.eq('}') do
        ParseStatement(vars, body, 1);
      NextLex;
    except
      on e: Exception do
      begin
        FLastProcParseError := e.Message;
        Exit;
      end;
    end;
    // successful parsing:
    Result := True;
    FProcBodies.Add(procHeader);
    if FProcTypes.Count > 0 then
    begin
      FProcBodies.Add('type');
      for i := 0 to FProcTypes.Count - 1 do
      begin
        td := TTypeDeclaration(FProcTypes.Objects[i]);
        if td.Used then
          FProcBodies.AddStrings(td.FullDecl);
      end;
    end;
    if vars.Count > 0 then
    begin
      FProcBodies.Add('var');
      for i := 0 to vars.Count - 1 do
        FProcBodies.Add(vars[i]);
    end;
    Beautify(body);
    FProcBodies.Add('begin');
    FProcBodies.AddStrings(body);
    FProcBodies.Add('end;');
    FProcBodies.Add('');

    {$ifdef verbose}
    if FProcTypes.Count > 0 then
    begin
      WriteLn('type');
      for i := 0 to FProcTypes.Count - 1 do
      begin
        td := TTypeDeclaration(FProcTypes.Objects[i]);
        if td.Used then
          WriteLn(td.FullDecl.Text);
      end;
    end;
    if vars.Count > 0 then
    begin
      WriteLn('var');
      WriteLn(vars.Text);
    end;
    WriteLn('begin');
    WriteLn(body.Text);
    WriteLn('end;');
    WriteLn;
    {$endif}
  finally
    body.Free;
    vars.Free;
    for i := 0 to FProcTypes.Count - 1 do
      FDeclaredTypes.DeleteTypeByOrigName(FProcTypes[i]);
    FProcTypes.Free;
  end;
end;

procedure TBodyParser.VarDeclaration(ityp: TTypeDeclaration; vars,
  body: TStrings);
var
  sptr, var_name, var_type: string;
  typ: TTypeDeclaration;
  arr_bound: Integer;
begin
  while Assigned(cur_lex) do
  begin
    sptr := '';
    while cur_lex.eq('*') do
    begin
      sptr := sptr + '*';
      NextLex;
    end;
    if cur_lex.LexType <> ltSymbol then
      ErrorLexExpectedIn('identifier', 'var declaration');
    var_name := FixFPCreserv(cur_lex.AsString);
    while FGlobDeclIds.IsDeclared(var_name) do
      var_name := var_name + '_';
    typ := FDeclaredTypes.GetTypeByCName(ityp.OrigTypeName + sptr);
    FGlobDeclIds.AddLocal(cur_lex.AsString, var_name, typ);
    if not Assigned(typ) then
      raise Exception.Create('type "' + ityp.OrigTypeName + sptr + '" undeclared');
    var_type := typ.TypeName;
    NextLex;
    if cur_lex.eq('[') then
    begin
      // array decl
      NextLex;
      if (cur_lex.LexType = ltNumber) and TryStrToInt(cur_lex.AsString, arr_bound) then
      begin
        if arr_bound <= 0 then
          raise Exception.Create('array upper bound = ' + IntToStr(arr_bound));
        Dec(arr_bound);
        var_type := 'array [0..' + IntToStr(arr_bound) + '] of ' + var_type;
        NextLex;
      end else begin
        // expression as upper bound
      end;
      if not cur_lex.eq(']') then
        ErrorLexExpected(']');
      NextLex;
    end else
    if cur_lex.eq('=') then
    begin
      // var decl with initialization
      NextLex;
      ParseAssignment(var_name, body);
    end;
    vars.Add('  ' + var_name + ': ' + var_type + ';');
    if cur_lex.eq(',') then
    begin
      NextLex;
      Continue;
    end;
    Break;
  end;
  if not cur_lex.eq(';') then
    ErrorLexExpected(';');
  NextLex;
end;

procedure TBodyParser.ParseUnionVars(vars: TStrings);
var
  s, s2, typ, ident: string;
  i: Integer; td, ptd: TTypeDeclaration;
begin
  td := nil;
  if cur_lex.LexType = ltSymbol then
  begin
    td := DeclareType('union ' + cur_lex.AsString, 'record case...', False);
    FProcTypes.AddObject(td.OrigTypeName, td);
    // foundation:NSConvertHostFloatToSwapped: Pufconv
    ptd := DeclarePointerType('union ' + cur_lex.AsString);
    FProcTypes.AddObject(ptd.OrigTypeName, ptd);
    ptd.FullDecl.Add('  ' + ptd.TypeName + ' = ^' + td.TypeName + ';');
    td.FullDecl.Add('  ' + td.TypeName + ' = record');
    td.FullDecl.Add('    case Integer of');
    NextLex; // union tag
  end;
  if not cur_lex.eq('{') then
    raise Exception.Create('"{" expected');
  NextLex;
  s := ''; i := 0;
  while not cur_lex.eq('}') do
  begin
    ReadDefinition(typ, ident, False);
    if Assigned(td) then
      td.FullDecl.Add('    ' + IntToStr(i) + ': (' + FixFPCreserv(ident) + ': ' + typ + ';);');
    s := s + sLineBreak + '    ' + IntToStr(i) + ': (' + FixFPCreserv(ident) + ': ' + typ + ';);';
    Inc(i);
    if not cur_lex.eq(';') then
      raise Exception.Create('";" expected');
    NextLex;
  end;
  if Assigned(td) then
    td.FullDecl.Add('  end;');
  s := s + sLineBreak + '  end;';
  NextLex;
  if not Assigned(td) and (cur_lex.LexType <> ltSymbol) then
    raise Exception.Create('identifier expected');
  if cur_lex.eq(';') then
  begin
    NextLex;
    Exit;
  end;
  s2 := '';
  while not cur_lex.eq(';') do
  begin
    // '*' not allowed here yet
    if s2 <> '' then s2 := s2 + ', ';
    ident := FixFPCreserv(cur_lex.AsString);
    s2 := s2 + ident;
    FGlobDeclIds.AddLocal(cur_lex.AsString, ident, td); { TODO : type constructor in func declaration; ex? }
    td.Use;
    NextLex;
    if cur_lex.eq(',') then NextLex;
  end;
  NextLex;
  if Assigned(td) then
    s := '  ' + s2 + ': ' + td.TypeName + ';'
  else
    // union without tag
    s := '  ' + s2 + ': record' + sLineBreak + '    case Integer of' + s;
  vars.Add(s);
end;

procedure TBodyParser.ParseStatement(vars, body: TStrings; indent: Integer);
var
  stype: string;
  typ: TTypeDeclaration;
  e: TExpr;
begin
  if cur_lex.eq('{') then
  begin
    NextLex;
    body.Add(StringOfChar(' ', indent * 2) + 'begin');
    while not cur_lex.eq('}') do
      ParseStatement(vars, body, indent + 1);
    NextLex;
    body.Add(StringOfChar(' ', indent * 2) + 'end;');
  end else
  if cur_lex.eq('struct') then
    raise Exception.Create('not implemented')
  else
  if cur_lex.eq('union') then
  begin
    NextLex;
    ParseUnionVars(vars);
  end else
  if cur_lex.eq('while') then
  begin
    raise Exception.Create('to be implemented');
  end else
  if cur_lex.eq('for') then
  begin
    NextLex;
    if not cur_lex.eq('(') then ErrorLexExpectedIn('(', '"for" statement');
    NextLex;
    e := ParseExpr;
    body.Add(StringOfChar(' ', indent * 2) + e.AsString + ';');
    e.Free;
    if not cur_lex.eq(';') then ErrorLexExpectedIn(';', '"for" statement');
    NextLex;
    e := ParseExpr;
    body.Add(StringOfChar(' ', indent * 2) + 'while ' + e.AsCondition + ' do');
    e.Free;
    if not cur_lex.eq(';') then ErrorLexExpectedIn(';', '"for" statement');
    NextLex;
    e := ParseExpr;
    if not cur_lex.eq(')') then ErrorLexExpectedIn(')', '"for" statement');
    NextLex;
    body.Add(StringOfChar(' ', indent * 2) + 'begin');
    if cur_lex.eq('{') then
    begin
      NextLex;
      while not cur_lex.eq('}') do
        ParseStatement(vars, body, indent + 1);
      NextLex;
    end else
      ParseStatement(vars, body, indent + 1);
    body.Add(StringOfChar(' ', (indent + 1) * 2) + e.AsString + ';');
    e.Free;
    body.Add(StringOfChar(' ', indent * 2) + 'end;');
  end else
  if cur_lex.eq('switch') then
  begin
    NextLex;
    SwitchStatement(vars, body, indent);
  end else
  if cur_lex.eq('if') then
  begin
    NextLex;
    IfStatement(vars, body, indent);
  end else
  if cur_lex.eq('return') then
  begin
    NextLex;
    ReturnStatement(body, indent);
    if not cur_lex.eq(';') then
      raise Exception.Create('";" expected');
    NextLex;
  end else
  if BuiltInCType(stype) then
  begin
    VarDeclaration(FDeclaredTypes.GetTypeByCName(stype), vars, body);
  end else
  if FDeclaredTypes.GetTypeByCName(cur_lex.AsString) <> nil then
  begin
    // var declaration
    typ := FDeclaredTypes.GetTypeByCName(cur_lex.AsString);
    NextLex;
    VarDeclaration(typ, vars, body);
  end else begin
    e := ParseExpr;
    { TODO : ?: => if }
    body.Add(StringOfChar(' ', indent * 2) + e.AsString + ';');
    e.Free;
    if not cur_lex.eq(';') then
      raise Exception.Create('";" expected');
    NextLex;
  end;
end;

procedure TBodyParser.ParseAssignment(leftHand: string; body: TStrings);
var
  expr, cond, true_part, false_part: TExpr;
  td: TTypeDeclaration;
begin
  if cur_lex.eq('{') then
  begin
    // struct initialization
    NextLex;
    td := FGlobDeclIds.VarType(leftHand);
    if td = nil then
      raise Exception.Create('Cannot determine type of "' + leftHand + '", it must be struct!');
    expr := ParseExpr; // x,y,z,...
    try
      RecordFieldsAssignement(leftHand, td, expr, 1, body);
    finally
      expr.Free;
    end;
    if not cur_lex.eq('}') then
      raise Exception.Create('"}" expected!');
    NextLex;
    Exit;
  end;
  expr := ParseExpr;
  try
    { TODO : expr := <id(leftHand) '=' expr>; general expr.AsString that handle "?:" }
    if not expr.WithIf then
      body.Add('  ' + leftHand + ' := ' +  expr.AsString + ';')
    else begin
      expr.SplitIf(cond, true_part, false_part);
      try
        body.Add('  if ' + cond.AsCondition + ' then');
        body.Add('    ' + leftHand + ' := ' + true_part.AsString);
        body.Add('  else');
        body.Add('    ' + leftHand + ' := ' + false_part.AsString + ';');
      finally
        cond.Free;
        true_part.Free;
        false_part.Free;
      end;
    end;
  finally
    expr.Free;
  end;
end;

procedure TBodyParser.ParseEnum;
  procedure FixNumConsts(e: TExpr);
  var str: string;
  begin
    if e.op = '#num' then
    begin
      str := e.args[0].op;
      if UpCase(str[Length(str)]) = 'L' then
        str := Copy(str, 1, Length(str) - 1);
      if UpCase(str[Length(str)]) = 'L' then
        str := Copy(str, 1, Length(str) - 1);
      if UpCase(str[Length(str)]) = 'U' then
        str := Copy(str, 1, Length(str) - 1);
      e.args[0].op := str;
    end else begin
      if Assigned(e.args[0]) then FixNumConsts(e.args[0]);
      if Assigned(e.args[1]) then FixNumConsts(e.args[1]);
      if Assigned(e.args[2]) then FixNumConsts(e.args[2]);
    end;
  end;
var
  localVal: Integer;
  constName, constValue, str: string;
  expr: TExpr;
begin
  if not Scanner.CurHeaderBelongsToFramework then
  begin
    inherited;
    Exit;
  end;
  localVal := 0;
  while not cur_lex.eq('}') do
  begin
    expr := nil;
    constName := cur_lex.AsString;
    NextLex;
    if cur_lex.eq('=') then
    begin
      NextLex;
      expr := E11;
      expr.Simplify;
      FixNumConsts(expr); // remove 'L' and 'U' postfixes
      expr.declIds := FGlobDeclIds;
      expr.declTypes := FDeclaredTypes;
      CheckUndeclaredIdentifiers(expr, @NewInterfaceUnit);
    end else begin
      expr := TExpr.Create('#num');
      expr.args[0] := TExpr.Create(IntToStr(localVal));
      expr.declIds := FGlobDeclIds;
      expr.declTypes := FDeclaredTypes;
    end;
    try
      constValue := expr.AsString;
      TryStrToInt(constValue, localVal);
      if not cur_lex.eq('}') then NextLex; // "," or "}"
      str := '  ' + constName + ' = ' + constValue + ';';
      if (expr.op = '#num') and (expr.args[0].op[1] = '''') then
        str := str + ' { ' + expr.args[0].op + ' }';
      if FPredefinedSymbols.IndexOf(constName) >= 0 then
      begin
        str := '//' + str;
        DestAdd(usConst, str);
      end else begin
        DestAdd(usConst, str);
        FGlobDeclIds.Add(constName, constName, expr.GetType, Scanner.CurFrameworkUnit);
      end;
    finally
      expr.Free;
    end;
    Inc(localVal);
  end;
  NextLex;
end;

//*F2_type  ->  <type> ")" |
function TBodyParser.F2_type(out expr: TExpr): Boolean;
var
  typ: TTypeDeclaration;
  stype: string;
begin
  Result := True;
  if cur_lex.eq('union') or cur_lex.eq('struct') then
  begin
    expr := TExpr.Create('#type', 1);
    typ := ReadType; // with all "*"
    expr.args[0] := TExpr.Create(typ.TypeName);
    if Assigned(FProcTypes) and (FProcTypes.IndexOfObject(typ) >= 0) then
      typ.Use;
  end else
  if (cur_lex.LexType = ltSymbol)
  and (FDeclaredTypes.GetTypeByCName(cur_lex.AsString) <> nil)
  then
  begin
    // type conversion
    expr := TExpr.Create('#type', 1);
    typ := ReadType; // with all "*"
    expr.args[0] := TExpr.Create(typ.TypeName);
    if Assigned(FProcTypes) and (FProcTypes.IndexOfObject(typ) >= 0) then
      typ.Use;
  end else
  if (cur_lex.LexType = ltSymbol) and BuiltInCType(stype) then
  begin
    // type conversion
    expr := TExpr.Create('#type', 1);
    while cur_lex.eq('*') do
    begin
      stype := stype + '*';
      NextLex;
    end;
    typ := FDeclaredTypes.GetTypeByCName(stype);
    expr.args[0] := TExpr.Create(typ.TypeName);
  end else
    Result := False;
  if Result then
  begin
    if not cur_lex.eq(')') then
      ErrorLexExpected(')');
    NextLex;
  end;
end;

//* F2   ->       F2_type "{" E "}" | F2_type F | E ")"
function TBodyParser.F2: TExpr;
begin
  if F2_type(Result) then
  begin
    if cur_lex.eq('{') then
    begin
      NextLex;
      Result.args[1] := TExpr.Create('{}');
      Result.args[1].args[0] := E_();
      if not cur_lex.eq('}') then
        ErrorLexExpected('}');
      NextLex;
    end else
      Result.args[1] := F_();
  end else begin
    Result := E_();
    if not cur_lex.eq(')') then
      ErrorLexExpected(')');
    NextLex;
  end;
end;

//* F1   ->       <ident> | N | "(" F2 | "[" F3
function TBodyParser.F1: TExpr;
begin
  if cur_lex.eq('(') then
  begin
    NextLex;
    Result := F2();
  end else begin
    if cur_lex.LexType <> ltSymbol then
      raise Exception.Create('identifier expected');
    Result := TExpr.Create('#id');
    Result.args[0] := TExpr.Create(cur_lex.AsString);
    NextLex;
  end;
end;

//**F0   ->       F1 { . F0 | -> F0 | "[" E "]" | "(" <params> ")" } | N
function TBodyParser.F0: TExpr;
var tmp: TExpr;
begin
  if cur_lex.LexType = ltNumber then
  begin
    Result := TExpr.Create('#num');
    Result.args[0] := TExpr.Create(cur_lex.AsString);
    NextLex;
  end else begin
    Result := F1();
    repeat
      tmp := Result;
      if cur_lex.eq('.') then
      begin
        NextLex;
        Result := TExpr.Create('.', 1);
        Result.args[0] := tmp;
        Result.args[1] := F0();
      end else
      if cur_lex.eq('->') then
      begin
        NextLex;
        Result := TExpr.Create('->', 1);
        Result.args[0] := tmp;
        Result.args[1] := F0();
      end else
      if cur_lex.eq('[') then
      begin
        NextLex;
        Result := TExpr.Create('[]', 1);
        Result.args[0] := tmp;
        Result.args[1] := E_();
        if not cur_lex.eq(']') then
          raise Exception.Create('"]" expected');
        NextLex;
      end else
      if cur_lex.eq('(') then
      begin
        // function call
        NextLex;
        tmp := TExpr.Create('#func');
        tmp.args[0] := Result;
        Result := tmp;
        if not cur_lex.eq(')') then
        begin
          Result.args[1] := TExpr.Create('#param');
          tmp := Result.args[1];
          tmp.args[0] := E11();
          while cur_lex.eq(',') do
          begin
            NextLex;
            tmp.args[1] := TExpr.Create('#param');
            tmp := tmp.args[1];
            tmp.args[0] := E11();
          end;
          if not cur_lex.eq(')') then
            raise Exception.Create('")" expected!');
          NextLex;
        end else NextLex;
      end else Break;
    until False;
  end;
end;

//* F_sizeof ->       "(" <type> ")" | "(" E ")" | F0
function TBodyParser.F_sizeof: TExpr;
var tmp: TExpr;
begin
  if cur_lex.eq('(') then
  begin
    NextLex;
    if F2_type(Result) then
    begin
      tmp := Result;
      Result := Result.args[0];
      tmp.args[0] := nil;
      tmp.Free;
    end else begin
      Result := E_();
      if not cur_lex.eq(')') then
        ErrorLexExpected(')');
      NextLex;
    end;
  end else
    Result := F0();
end;

//* F	-> 	 "sizeof" F_sizeof | [ ! | ~ | ++ | -- | + | - | * | & ] F0 | "@"<str> // last part => ocparser
function TBodyParser.F_: TExpr;
var op: string; tmp: TExpr;
begin
  if cur_lex.eq('sizeof') then
  begin
    Result := TExpr.Create(cur_lex.AsString, 1);
    NextLex;
    Result.args[0] := F_sizeof();
  end else
  if cur_lex.eq('!') or cur_lex.eq('~') or cur_lex.eq('++')
  or cur_lex.eq('--') or cur_lex.eq('+') or cur_lex.eq('-')
  or cur_lex.eq('*') or cur_lex.eq('&') then
  begin
    case cur_lex.AsString of
    '*': op := 'deref';
    '&': op := 'addr';
    '+': op := 'u+';
    '-': op := 'u-';
    else op := cur_lex.AsString;
    end;
    Result := TExpr.Create(op, 1);
    NextLex;
    Result.args[0] := F_();
  end else
    Result := F0();
  if cur_lex.eq('++') or cur_lex.eq('--') then
  begin
    tmp := Result;
    Result := TExpr.Create('p' + cur_lex.AsString, 1);
    Result.args[0] := tmp;
    NextLex;
  end;
end;

//* T	-> 	 F {[ * | / | "%" ] F}
function TBodyParser.T_: TExpr;
var tmp: TExpr;
begin
  Result := F_();
  while cur_lex.eq('*') or cur_lex.eq('/') or cur_lex.eq('%') do
  begin
    tmp := Result;
    Result := TExpr.Create(cur_lex.AsString, 2);
    NextLex;
    Result.args[0] := tmp;
    Result.args[1] := F_();
  end;
end;

//* E1	-> 	 T {[ + | - ] T}
function TBodyParser.E1: TExpr;
var tmp: TExpr;
begin
  Result := T_();
  while cur_lex.eq('+') or cur_lex.eq('-') do
  begin
    tmp := Result;
    Result := TExpr.Create(cur_lex.AsString, 3);
    NextLex;
    Result.args[0] := tmp;
    Result.args[1] := T_();
  end;
end;

//* E2   ->       E1 {[ << | >> ] E1}
function TBodyParser.E2: TExpr;
var tmp: TExpr;
begin
  Result := E1();
  while cur_lex.eq('<<') or cur_lex.eq('>>') do
  begin
    tmp := Result;
    Result := TExpr.Create(cur_lex.AsString, 2);
    NextLex;
    Result.args[0] := tmp;
    Result.args[1] := E1();
  end;
end;

//* E3	->       E2 {[ < | <= | > | >= ] E2}
function TBodyParser.E3: TExpr;
var tmp: TExpr;
begin
  Result := E2();
  while cur_lex.eq('<') or cur_lex.eq('<=')
  or cur_lex.eq('>') or cur_lex.eq('>=') do
  begin
    tmp  := Result;
    Result := TExpr.Create(cur_lex.AsString, 4);
    NextLex;
    Result.args[0] := tmp;
    Result.args[1] := E2();
  end;
end;

//* E4	->       E3 {[ == | != ] E3}
function TBodyParser.E4: TExpr;
var tmp: TExpr;
begin
  Result := E3;
  while cur_lex.eq('==') or cur_lex.eq('!=') do
  begin
    tmp  := Result;
    Result := TExpr.Create(cur_lex.AsString, 4);
    NextLex;
    Result.args[0] := tmp;
    Result.args[1] := E3;
  end;
end;

//* E5	->       E4 { & E4}
function TBodyParser.E5: TExpr;
var tmp: TExpr;
begin
  Result := E4();
  while cur_lex.eq('&') do
  begin
    tmp := Result;
    Result := TExpr.Create(cur_lex.AsString, 2);
    NextLex;
    Result.args[0] := tmp;
    Result.args[1] := E4();
  end;
end;

//* E6   ->       E5 { ^ E5}
function TBodyParser.E6: TExpr;
var tmp: TExpr;
begin
  Result := E5();
  while cur_lex.eq('^') do
  begin
    tmp := Result;
    Result := TExpr.Create(cur_lex.AsString, 3);
    NextLex;
    Result.args[0] := tmp;
    Result.args[1] := E5();
  end;
end;

//* E7   ->       E6 { "|" E6}
function TBodyParser.E7: TExpr;
var tmp: TExpr;
begin
  Result := E6();
  while cur_lex.eq('|') do
  begin
    tmp := Result;
    Result := TExpr.Create(cur_lex.AsString, 3);
    NextLex;
    Result.args[0] := tmp;
    Result.args[1] := E6();
  end;
end;

//* E8   ->       E7 { && E7}
function TBodyParser.E8: TExpr;
var tmp: TExpr;
begin
  Result := E7();
  while cur_lex.eq('&&') do
  begin
    tmp := Result;
    Result := TExpr.Create(cur_lex.AsString, 2);
    NextLex;
    Result.args[0] := tmp;
    Result.args[1] := E7();
  end;
end;

//* E9   ->       E8 { "||" E8}
function TBodyParser.E9: TExpr;
var tmp: TExpr;
begin
  Result := E8();
  while cur_lex.eq('||') do
  begin
    tmp := Result;
    Result := TExpr.Create(cur_lex.AsString, 3);
    NextLex;
    Result.args[0] := tmp;
    Result.args[1] := E8();
  end;
end;

//* E10  ->       E9 [ ? E : E ]
function TBodyParser.E10: TExpr;
var tmp: TExpr;
begin
  Result := E9();
  if cur_lex.eq('?') then
  begin
    NextLex;
    tmp := Result;
    Result := TExpr.Create('?:', -1);
    Result.args[0] := tmp;
    Result.args[1] := E_();
    if not cur_lex.eq(':') then
      raise Exception.Create('":" expected');
    NextLex;
    Result.args[2] := E_();
  end;
end;

//* E11  ->       E10 [[ = | += | -= | *= | /= | %= | &= | ^= | "|=" | <<= | >>= ] E ]
function TBodyParser.E11: TExpr;
var tmp: TExpr;
begin
  Result := E10();
  if cur_lex.eq('=') or cur_lex.eq('+=') or cur_lex.eq('-=')
  or cur_lex.eq('*=') or cur_lex.eq('/=') or cur_lex.eq('%=')
  or cur_lex.eq('&=') or cur_lex.eq('^=') or cur_lex.eq('|=')
  or cur_lex.eq('<<=') or cur_lex.eq('>>=') then
  begin
    tmp := Result;
    Result := TExpr.Create(cur_lex.AsString, 5);
    NextLex;
    Result.args[0] := tmp;
    Result.args[1] := E_();
  end;
end;

//* E	-> 	 E11 { "," E11}
function TBodyParser.E_: TExpr;
var tmp: TExpr;
begin
  Result := E11();
  while cur_lex.eq(',') do
  begin
    tmp := Result;
    Result := TExpr.Create(cur_lex.AsString, -1);
    NextLex;
    Result.args[0] := tmp;
    Result.args[1] := E11();
  end;
end;

function TBodyParser.ParseExpr: TExpr;
begin
  Result := E_;
  Result.Simplify;
  Result.declIds := FGlobDeclIds;
  Result.declTypes := FDeclaredTypes;
  CheckUndeclaredIdentifiers(Result, @NewImplementationUnit);
end;

procedure TBodyParser.RecordFieldsAssignement(varName: string;
  td: TTypeDeclaration; expr: TExpr; indent: Integer; body: TStrings);

  procedure AssignFields(e: TExpr; rec: TTypeDeclaration; var f_num: Integer);
  var field_name: string;
  begin
    if e.op = ',' then
    begin
      AssignFields(e.args[0], rec, f_num);
      e := e.args[1];
    end ;
    field_name := TrimLeft(rec.FullDecl[f_num]);
    field_name := Copy(field_name, 1, Pos(':', field_name) - 1);
    body.Add(StringOfChar(' ', indent * 2) + varName + '.' + field_name + ' := ' + e.AsString + ';');
    Inc(f_num);
  end;

  function FindAppropriateCase(var f_ind: Integer; expr: TExpr; td: TTypeDeclaration): Boolean;
  var fld_count, i, j: Integer;
  begin
    Result := False;
    fld_count := 1;
    while expr.op = ',' do
    begin
      Inc(fld_count);
      expr := expr.args[0];
    end;
    i := f_ind;
    if Pos(': (', td.FullDecl[i]) > 0 then Inc(i);
    while i < td.FullDecl.Count do
    begin
      f_ind := i;
      j := 0;
      while (i < td.FullDecl.Count) do
      begin
        if Pos(');', td.FullDecl[i]) > 0 then Break
        else if Pos(': ', td.FullDecl[i]) > 0 then Inc(j);
        Inc(i);
      end;
      if j = fld_count then
      begin
        Result := True;
        Exit;
      end;
      while (i < td.FullDecl.Count) do
      begin
        if Pos(': (', td.FullDecl[i]) > 0 then
        begin
          Inc(i);
          Break;
        end;
        Inc(i);
      end;
    end;
  end;

var
  i: Integer;
begin
  td := td.ResolveAlias;
  if Pos('record', td.Declaration) = 0 then
    raise Exception.Create('struct variable expected!');
  i := 1;
  if Copy(Trim(td.FullDecl[i]), 1, 5) = 'case ' then
  begin
    Inc(i);
    if not FindAppropriateCase(i, expr, td) then
      raise Exception.Create('Cannot find appropriate record case '{for "' + expr.AsString + '"}+' in ' + td.TypeName);
  end;
  AssignFields(expr, td, i);
end;

procedure TBodyParser.ReturnStatement(body: TStrings; indent: Integer);

  function WithFixType(e: TExpr): string;
  var td, et: TTypeDeclaration;
  begin
    td := FGlobDeclIds.ReturnType(FCurFunction);
    Result := e.AsString;
    if e.op = '#type' then Exit;
    if (td.TypeName = 'Boolean') then
    begin
      if Result = '0' then Exit('False');
      if Result = '1' then Exit('True');
    end;
    et := e.GetType;
    if td <> et then
      Result := td.TypeName + '(' + Result + ')';
  end;

var
  e, cond, e_f, e_t: TExpr;
  td: TTypeDeclaration;
begin
  if cur_lex.eq(';') then
    body.Add(StringOfChar(' ', indent * 2) + 'Exit;')
  else
  begin
    e := ParseExpr;
    try
      if e.WithIf then
      begin
        e.SplitIf(cond, e_t, e_f);
        try
          body.Add(StringOfChar(' ', indent * 2) + 'if ' + cond.AsCondition + ' then');
          body.Add(StringOfChar(' ', (indent + 1) * 2) + 'Exit(' + WithFixType(e_t) + ')');
          body.Add(StringOfChar(' ', indent * 2) + 'else');
          body.Add(StringOfChar(' ', (indent + 1) * 2) + 'Exit(' + WithFixType(e_f) + ');');
        finally
          cond.Free;
          e_t.Free;
          e_f.Free;
        end;
      end else begin
        if (e.op = '#type') and (e.args[1].op = '{}') then
        begin
          td := FGlobDeclIds.ReturnType(FCurFunction);
          if td = nil then
            raise Exception.Create('Cannot determine function "' + FCurFunction + '" result type!');
          if FDeclaredTypes.GetTypeByPasName(e.args[0].op) <> td then
            raise Exception.Create('Incorrect type: got "'
              + FDeclaredTypes.GetTypeByCName(e.args[0].op).TypeName + '", expected "'
              + td.TypeName + '"');
          RecordFieldsAssignement('Result', td, e.args[1].args[0], 1, body);
        end else
          body.Add(StringOfChar(' ', indent * 2) + 'Exit(' + WithFixType(e) + ');');
      end;
    finally
      e.Free;
    end;
  end;
end;

procedure TBodyParser.SwitchStatement(vars, body: TStrings; indent: Integer);
var e: TExpr; case_str: string;

  procedure ParseCase;
  var
    cbreak: Boolean;
  begin
    cbreak := False;
    while not cbreak do
    begin
      if cur_lex.eq('return') then
      begin
        cbreak := True;
        ParseStatement(vars, body, indent + 2);
      end else
      if cur_lex.eq('break') then
      begin
        cbreak := True;
        NextLex;
        if not cur_lex.eq(';') then
          raise Exception.Create('";" expected');
        NextLex;
      end else
        while not cur_lex.eq('case') and not cur_lex.eq('}') do
          ParseStatement(vars, body, indent + 2);
    end;
  end;

var i: Integer;
begin
  if not cur_lex.eq('(') then
    raise Exception.Create('"(" expected');
  NextLex;
  e := ParseExpr;
  try
    body.Add(StringOfChar(' ', indent * 2) + 'case ' + e.AsString + ' of');
  finally
    e.Free;
  end;
  if not cur_lex.eq(')') then
    raise Exception.Create('")" expected');
  NextLex;
  if not cur_lex.eq('{') then
    raise Exception.Create('"{" expected');
  NextLex;
  while not cur_lex.eq('}') do
  begin
    if cur_lex.eq('case') then
    begin
      NextLex;
      with ParseExpr do
      try
        case_str := AsString;
      finally
        Free;
      end;
      if not cur_lex.eq(':') then
        raise Exception.Create('":" expectede');
      NextLex;
      while cur_lex.eq('case') do
      begin
        NextLex;
        with ParseExpr do
        try
          case_str := case_str + ', ' + AsString;
        finally
          Free;
        end;
        if not cur_lex.eq(':') then
          raise Exception.Create('":" expectede');
        NextLex;
      end;
      body.Add(StringOfChar(' ', indent * 2) + case_str + ':');
      body.Add(StringOfChar(' ', (indent + 1) * 2) + 'begin');
      ParseCase;
      body.Add(StringOfChar(' ', (indent + 1) * 2) + 'end;');
    end else
    if cur_lex.eq('default') then
    begin
      NextLex;
      if not cur_lex.eq(':') then
        raise Exception.Create('":" expected');
      NextLex;
      body.Add(StringOfChar(' ', indent * 2) + 'else');
      i := body.Count;
      ParseCase;
      if body.Count = i then // empty case-else section
        body.Delete(i - 1);
    end else
      raise Exception.Create('unexpected ');
  end;
  NextLex;
  body.Add(StringOfChar(' ', indent * 2) + 'end;');
end;

procedure TBodyParser.IfStatement(vars, body: TStrings; indent: Integer);
var e: TExpr;

  procedure ProcessStatements;
  begin
    if cur_lex.eq('{') then
    begin
      NextLex;
      while not cur_lex.eq('}') do
        ParseStatement(vars, body, indent + 1);
      NextLex;
    end else
      ParseStatement(vars, body, indent + 1);
  end;

begin
  if not cur_lex.eq('(') then
    ErrorLexExpected('(');
  NextLex;
  e := ParseExpr;
  e.forAssignments := TStringList(body);
  e.indent := indent;
  body.Add(StringOfChar(' ', indent * 2) + 'if '
    + e.AsCondition + ' then');
  e.Free;
  if not cur_lex.eq(')') then
    ErrorLexExpected(')');
  NextLex;
  body.Add(StringOfChar(' ', indent * 2) + 'begin');
  ProcessStatements;
  if not cur_lex.eq('else') then
    body.Add(StringOfChar(' ', indent * 2) + 'end;')
  else begin
    body.Add(StringOfChar(' ', indent * 2) + 'end else begin');
    NextLex;
    ProcessStatements;
    body.Add(StringOfChar(' ', indent * 2) + 'end;')
  end;
end;

function TBodyParser.GetUndeclaredIdentifier(e: TExpr; withLeft: Boolean;
  OnNeedUnit: TNeedUnitProc): TExpr;
var
  i: Integer;
  td: TTypeDeclaration;
  id: TDeclId;
begin
  Result := nil;
  if withLeft then
  begin
    if e.op = '@str' then
    begin
      td := FDeclaredTypes.GetTypeByCName('NSString');
      if not Assigned(td) then Exit(e);
      if not td.Avail and (td.NeedUnit <> '') then
        OnNeedUnit(td.NeedUnit);
    end else
    if e.op = '#meth' then
    begin
      td := FDeclaredTypes.GetTypeByCName(e.args[0].args[0].op);
      if Assigned(td) then
      begin
        if not td.Avail and (td.NeedUnit <> '') then
          OnNeedUnit(td.NeedUnit);
      end else
        Result := GetUndeclaredIdentifier(e.args[0], True, OnNeedUnit);
      if Assigned(Result) then Exit;
      if Assigned(e.args[2]) then
        Result := GetUndeclaredIdentifier(e.args[2], True, OnNeedUnit);
    end else
    if e.op = '#id' then
    begin
      if FGlobDeclIds.IsDeclaredLocal(e.args[0].op) then Exit;
      id := FGlobDeclIds.FindGlobalId(e.args[0].op);
      if Assigned(id) then
      begin
        if (id.NeedUnit <> '*') and (id.NeedUnit <> Scanner.CurFrameworkUnit) then
          OnNeedUnit(id.NeedUnit);
        Exit;
      end;
      if not IsPredefined(FixFPCreserv(e.args[0].op)) then
        Result := e;
      Exit;
    end else
    if (e.op = '.') or (e.op = '->') then
    begin
      Result := GetUndeclaredIdentifier(e.args[0], True, OnNeedUnit);
      if Assigned(Result) then Exit;
      Result := GetUndeclaredIdentifier(e.args[1], False, OnNeedUnit);
    end else begin
      for i := 0 to 2 do
        if Assigned(e.args[i]) then
        begin
          Result := GetUndeclaredIdentifier(e.args[i], True, OnNeedUnit);
          if Assigned(Result) then Exit;
        end else Break;
    end;
  end else begin
    if e.op = '[]' then
      Result := GetUndeclaredIdentifier(e.args[1], True, OnNeedUnit)
    else begin
      for i := 0 to 2 do
        if Assigned(e.args[i]) then
        begin
          Result := GetUndeclaredIdentifier(e.args[i], withLeft, OnNeedUnit);
          if Assigned(Result) then Exit;
        end else Break;
    end;
  end;
end;

function TBodyParser.ReadFuncParams: string;
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
    Result := Result + FixFPCreserv(paramName) + ': ' + typ.TypeName;
    FGlobDeclIds.AddLocal(paramName, FixFPCreserv(paramName), typ);
    if cur_lex.eq(',') then NextLex;
  end;
  NextLex;
  { TODO: here can be __atribute__ modifier: unavailable, deprecated, etc. }
  Result := '(' + Result + ')';
end;

procedure TBodyParser.Beautify(body: TStrings);

  function CountOfChar(ch: Char; const str: string): Integer;
  var
    i: Integer;
  begin
    Result := 0;
    for i := 1 to Length(str) do
      if str[i] = ch then Inc(Result);
  end;

  procedure Exit2Result(index: Integer);
  var s: string; i: Integer;
  begin
    s := body[index];
    i := 1;
    while (i <= Length(s)) and (s[i] = ' ') do Inc(i);
    Delete(s, i, 5);
    Insert('Result := ', s, i);
    i := Length(s);
    while (i > 0) and (s[i] <> ')') do Dec(i);
    Delete(s, i, 1);
    body[index] := s;
  end;

var
  s: string;
  i: Integer;
begin
  if body.Count = 0 then Exit;
  for i := body.Count - 3 downto 2 do
  begin
    if (Trim(body[i]) = 'begin') and (Trim(body[i + 2]) = 'end;')
    and (CountOfChar(';', body[i + 1]) = 1) then
    begin
      body.Delete(i + 2);
      body.Delete(i);
    end;
  end;
  i := body.Count - 1;
  if Copy(body[i], 3, 5) = 'Exit(' then
    Exit2Result(i)
  else
  if (i > 1) and (Trim(body[i]) = 'Exit(False);')
  and (Trim(body[i - 1]) = 'else') and (Trim(body[i - 2]) = 'Exit(True)') then
  begin
    body.Delete(i);      // Exit(False)
    body.Delete(i - 1);  // else
    body.Delete(i - 2);  // Exit(True)
    s := body[i - 3];
    Delete(s, 3, 2);
    Insert('Result :=', s, 3);
    s := Copy(s, 1, Length(s) - 5);
    body[i - 3] := s;
  end else
  if (i > 1) and (Pos('Exit(', body[i]) > 0) and (Trim(body[i - 1]) = 'else')
  and (Pos('Exit(', body[i - 2]) > 0) then
  begin
    Exit2Result(i);
    Exit2Result(i - 2);
  end;
  // TODO : remove unused pointer type for locally declared struct type
end;

procedure TBodyParser.NewInterfaceUnit(AUnitName: string);
begin
  if (FRefFrameworks.IndexOf(AUnitName) < 0)
  and (AUnitName <> Scanner.CurFrameworkUnit) then
  begin
    FDeclaredTypes.MakeAvailable(AUnitName);
    FRefFrameworks.Add(AUnitName);
  end;
end;

procedure TBodyParser.NewImplementationUnit(AUnitName: string);
begin
  FImplUnits.Add(AUnitName);
end;

procedure TBodyParser.ParseFuncDeclaration(is_extern: Boolean;
  td: TTypeDeclaration; OrigName, postfix: string);
var
  i: Integer;
  decl: string;
  sparams, stype, ProcName: string;
  need_nl, need_add: Boolean;
  isForward, commented: Boolean;
begin
  need_add := True;
  need_nl := False;
  ProcName := FixFPCreserv(OrigName);
  isForward := False;
  stype := td.TypeName;
  FGlobDeclIds.Add(OrigName, ProcName, td, Scanner.CurFrameworkUnit);
  NextLex;
  sparams := ReadFuncParams; // obtain params to GlobDeclIds.Locals
  if is_extern then
  begin
    postfix := ' cdecl;' + postfix;
    FGlobDeclIds.ClearLocals;
  end;
  if Scanner.LastAttribute = 'deprecated' then
  writeln;
  if stype = 'void' then
    decl := 'procedure ' + ProcName + sparams + ';' + postfix
  else
    decl := 'function ' + ProcName + sparams + ': ' + stype + ';' + postfix;
  if not is_extern and cur_lex.eq(';') then
  begin
    decl := '//' + decl + ' { forward }';
    isForward := True;
    NextLex;
  end else
  if not is_extern and cur_lex.eq('{') then
  begin
    NextLex;
    need_add := False;
    FCurFunction := OrigName;
    if Scanner.CurHeaderBelongsToFramework then
    begin
      if not ParseProcBody(decl) then
      begin
        decl := '//' + decl + ' { declaration of helper function, unsuccessful '
          + 'parsing: ' + FLastProcParseError + ' }';
        SkipToLex('}', 1);
        FGlobDeclIds.Delete(OrigName);
      end;
      FParsedProcs.Add(decl);
    end else begin
      need_add := False;
      decl := '//' + decl + ' { helper function of sys header }';
      SkipToLex('}', 1); // after skip it becomes belonging to framework
      FGlobDeclIds.Delete(OrigName); // from sys header
    end;
    FGlobDeclIds.ClearLocals;
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
  commented := Copy(decl, 1, 2) = '//';
  if need_add then
  begin
    DestAdd(usProcs, decl);
    if commented then
    begin
      for i := 0 to FAdditionalTypes.Count - 1 do
        TTypeDeclaration(FAdditionalTypes[i]).Reset;
      FAdditionalTypes.Clear;
    end;
  end;
  if not Scanner.CurHeaderBelongsToFramework or commented then
  begin
    if not isForward then FGlobDeclIds.Delete(OrigName);
    FGlobDeclIds.ClearLocals;
  end;
  if need_nl then NextLex;
end;

procedure TBodyParser.ParseVarDeclaration(is_extern: Boolean;
  td: TTypeDeclaration; OrigVarName, postfix: string; is_const: Boolean);
var declId: TDeclId;
begin
  declId := FGlobDeclIds.FindGlobalId(OrigVarName);
  if not Assigned(declId) then
  begin
    inherited;
    FGlobDeclIds.Add(OrigVarName, FixFPCreserv(OrigVarName), td, Scanner.CurFrameworkUnit);
  end else
    SkipToLex(';');
end;

procedure TBodyParser.DefinedConstant(Sender: TObject;
  ConstName, ConstValue: string);
begin
  if FGlobDeclIds.IsDeclared(ConstName) then Exit;
  inherited DefinedConstant(Sender, ConstName, ConstValue);
end;

procedure TBodyParser.AddProcDeclarations(dest: TStrings);
begin
  inherited AddProcDeclarations(dest);
  if FParsedProcs.Count > 0 then
  begin
    dest.Add('{ helper functions }');
    dest.AddStrings(FParsedProcs);
  end;
end;

procedure TBodyParser.DoAfterParse;
var
  i, j: Integer;
begin
  if FImplUnits.Count > 0 then
    for i := 0 to FRefFrameworks.Count - 1 do
    begin
      j := FImplUnits.IndexOf(FRefFrameworks[i]);
      if j >= 0 then
        FImplUnits.Delete(j);
    end;
  if FProcBodies.Count > 0 then
    FAdditionalDirectives.Add('{$pointermath on}');
  inherited DoAfterParse;
end;

constructor TBodyParser.Create(AFileName: string);
begin
  inherited;
  FProcBodies := TStringList.Create;
  FParsedProcs := TStringList.Create;
  FImplUnits := TStringList.Create;
  FImplUnits.Sorted := True;
  FImplUnits.Duplicates := dupIgnore;
end;

destructor TBodyParser.Destroy;
begin
  FImplUnits.Free;
  FParsedProcs.Free;
  FProcBodies.Free;
  inherited Destroy;
end;

procedure TBodyParser.AddProcs(Dest: TStrings);
var i: Integer; s: string;
begin
  if FImplUnits.Count > 0 then
  begin
    s := 'uses ';
    for i := 0 to FImplUnits.Count - 1 do
    begin
      if i > 0 then s := s + ', ';
      s := s + FImplUnits[i];
    end;
    s := s + ';';
    FProcBodies.Insert(0, s);
    FProcBodies.Insert(1, '');
  end;
  Dest.AddStrings(FProcBodies);
end;

procedure TBodyParser.InitDeclIds(src: TGlobDeclIds);
begin
  FGlobDeclIds := src;
end;

{ TExpr }

procedure TExpr.SetDeclIds(AValue: TGlobDeclIds);
begin
  if FdeclIds = AValue then Exit;
  FdeclIds := AValue;
  if Assigned(args[0]) then args[0].declIds := AValue;
  if Assigned(args[1]) then args[1].declIds := AValue;
  if Assigned(args[2]) then args[2].declIds := AValue;
end;

procedure TExpr.SetDeclTypes(AValue: TDeclaredTypes);
begin
  if FdeclTypes = AValue then Exit;
  FdeclTypes := AValue;
  if Assigned(args[0]) then args[0].declTypes := AValue;
  if Assigned(args[1]) then args[1].declTypes := AValue;
  if Assigned(args[2]) then args[2].declTypes := AValue;
end;

function TExpr.IsFloat: Boolean;
var td: TTypeDeclaration;
begin
  td := GetType;
  Result := Assigned(td) and ((td.TypeName = 'Single')
    or (td.TypeName = 'Double') or (td.TypeName = 'clongdouble'));
end;

constructor TExpr.Create(Aop: string; Aprior: Integer = 0);
begin
  op := Aop;
  prior := Aprior;
  args[0] := nil;
  args[1] := nil;
  args[2] := nil;
end;

destructor TExpr.Destroy;
begin
  args[0].Free;
  args[1].Free;
  args[2].Free;
  inherited Destroy;
end;

function TExpr.AsString(withBrackets: Boolean): string;

  function FixNum(num: string): string;
  var i: Integer; p: string; card: Cardinal; pc: set of Char;
  begin
    if num[1] = '''' then
    begin
      card := 0;
      Delete(num, 1, 1);
      while (num <> '') and (num[1] <> '''') do
      begin
        if (num[1] = '\') and (Length(num) > 1) then
          Delete(num, 1, 1);
        card := card shl 8 + Byte(num[1]);
        Delete(num, 1, 1);
      end;
      Delete(num, 1, 1);
      num := IntToStr(card) + num;
    end;
    if (num[1] = '0') and (Upcase(num[2]) = 'X') then
    begin
      Delete(num, 1, 1);
      num[1] := '$';
      pc := ['U', 'L'];
    end else
      pc := ['U', 'L', 'F', 'D'];
    i := Length(num);
    p := '';
    while UpCase(num[i]) in pc do
    begin
      p := upcase(num[i]) + p;
      Dec(i);
    end;
    SetLength(num, i);
    case p of
    '': { do nothing };
    'U': num := 'cuint(' + num + ')';
    'UL': num := 'culong(' + num + ')';
    'ULL': num := 'culonglong(' + num + ')';
    'L': num := 'clong(' + num + ')';
    'LL': num := 'clonglong(' + num + ')';
    'F': num := 'Single(' + num + ')';
    'D': num := 'Double(' + num + ')';
    'LD': num := 'clongdouble(' + num + ')';
    else
      raise Exception.Create('unknown posfix');
    end;
    { TODO : Need CTypes ! }
    Result := num;
  end;

var
  tmp: TExpr;
  i, j: Integer;
  s: string;
  td: TTypeDeclaration;
  isClass: Boolean;
begin
  case op of
  '@str': Result := Format('NSString.classStringWithCString(%s)', [args[0].op]);
  'p++': Result := 'Inc(' + args[0].AsString + ')';
  'u-': Result := '-' + args[0].AsString(args[0].prior >= prior);
  'u+': Result := '+' + args[0].AsString(args[0].prior >= prior);
  '~': Result := 'not ' + args[0].AsString(args[0].prior >= prior);
  '-=':
    Result := args[0].AsString + ' := ' + args[0].AsString + ' - ' + args[1].AsString(args[1].prior >= 3);
  '+=':
    Result := args[0].AsString + ' := ' + args[0].AsString + ' + ' + args[1].AsString(args[1].prior >= 3);
  '/':
    begin
      if args[0].IsFloat or args[1].IsFloat then
        s := ' / '
      else
        s := ' div ';
      Result := args[0].AsString(args[0].prior > prior) + s + args[1].AsString(args[1].prior >= prior);
    end;
  '+', '-', '<', '*':
    Result := args[0].AsString(args[0].prior > prior) + ' ' + op + ' ' + args[1].AsString(args[1].prior >= prior);
  '[]':
    Result := args[0].AsString(args[0].prior > prior) + '[' + args[1].AsString + ']';
  '->': // TODO : if left hand expr is class then "." must be used instead of "^."
    Result := args[0].AsString(args[0].prior > prior) + '^.' + args[1].AsString;
  '=':
    begin
      Result := args[1].AsString;
      i := Pos(' := ', Result);
      if i > 0 then
      begin
        s := System.Copy(Result, i + 4, MaxInt);
        j := Pos(';', s);
        if j > 0 then s := System.Copy(s, 1, j - 1);
        if not TryStrToInt(s, j) then
          s := System.Copy(Result, 1, i - 1);
        Result := Result + '; ' + args[0].AsString + ' := ' + s;
      end else
        Result := args[0].AsString + ' := ' + Result;
    end;
  '.': Result := args[0].AsString(args[0].prior > prior) + '.' + args[1].AsString;
  '|': Result := args[0].AsString(args[0].prior > prior) + ' or ' + args[1].AsString(args[1].prior >= prior);
  '&': Result := args[0].AsString(args[0].prior > prior) + ' and ' + args[1].AsString(args[1].prior >= prior);
  '&&', '||': Result := AsCondition;
  '>>': Result := args[0].AsString(args[0].prior > prior) + ' shr ' + args[1].AsString(args[1].prior >= prior);
  '<<': Result := args[0].AsString(args[0].prior > prior) + ' shl ' + args[1].AsString(args[1].prior >= prior);
  '==': Result := args[0].AsString(args[0].prior > prior) + ' = ' + args[1].AsString(args[1].prior >= prior);
  '#id':
    if not FdeclIds.GetPasName(args[0].op, Result) then
      Result := FixFPCreserv(args[0].op);
  '#type':
    begin
      Result := args[0].op + '(' + args[1].AsString + ')';
      if Result = 'Pointer(0)' then Result := 'nil';
    end;
  '#num': Result := FixNum(args[0].op);
  'deref': Result := args[0].AsString(args[0].prior > prior) + '^';
  'addr': Result := '@' + args[0].AsString(args[0].prior >= prior);
  '#param': Result := args[0].AsString;
  '#func':
    begin
      Result := args[0].AsString + '('; // #id -> func_name
      tmp := args[1];
      if Assigned(tmp) then
      begin
        Result := Result + tmp.AsString;
        tmp := tmp.args[1];
        while Assigned(tmp) do
        begin
          Result := Result + ', ' + tmp.AsString;
          tmp := tmp.args[1];
        end;
      end;
      Result := Result + ')';
    end;
  '#meth':
    begin
      isClass := False;
      if args[0].op = '#id' then
      begin
        // class method?
        td := FdeclTypes.GetTypeByCName(args[0].args[0].op);
        if Assigned(td) then
        begin
          Result := td.TypeName;
          isClass := True;
        end else begin
          Result := args[0].AsString;
          td := args[0].GetType;
        end;
      end else begin
        Result := args[0].AsString;
        td := args[0].GetType;
      end;
      if td.TypeName = 'id' then td := FdeclTypes.GetTypeByCName('NSObject');
      if not (td is TDeclarationWithMethods) then
        raise Exception.Create('cannot determine type to call method');
      Result := Result + '.'
        + TDeclarationWithMethods(td).FindMethodName(args[1].args[0].op, isClass) + '(';
      tmp := args[2];
      if Assigned(tmp) then
      begin
        Result := Result + tmp.AsString;
        tmp := tmp.args[1];
        while Assigned(tmp) do
        begin
          Result := Result + ', ' + tmp.AsString;
          tmp := tmp.args[1];
        end;
      end;
      Result := Result + ')';
    end;
  'sizeof':
    begin
      td := FdeclTypes.GetTypeByPasName(args[0].op);
      if not td.Avail then td := td.ResolveAlias;
      if Assigned(td) then
        Result := 'SizeOf(' + td.TypeName + ')'
      else
        Result := 'SizeOf' + args[0].AsString(True);
    end;
  else
    raise Exception.Create('Expr.AsString: "' + op + '" not implemented');
  end;
  if withBrackets then Result := '(' + Result + ')';
end;

function TExpr.Orig: string;
begin
  case op of
  '#id': Result := args[0].op;
  '#func': Result := args[0].Orig;
  else
    raise Exception.Create('not implemented: TExpr.Orig for op="' + op + '"');
  end;
end;

function TExpr.AsCondition: string;

  function AsCondWithFix(ex: TExpr; needFix: Boolean; cur_type: TTypeDeclaration;
    parent: TExpr; isVar: Boolean): string;
  var
    i: Integer;
    td: TTypeDeclaration;
    s: string;
    needBrackets: Boolean;
  begin
    if Assigned(parent) then
      needBrackets := (ex = parent.args[0]) and (ex.prior > parent.prior)
        or (ex = parent.args[1]) and (ex.prior >= parent.prior)
    else
      needBrackets := False;
    case ex.op of
    '!':
      Result := 'not ' + AsCondWithFix(ex.args[0], needFix, nil, ex, isVar);
    '&&':
      Result := AsCondWithFix(ex.args[0], needFix, nil, ex, isVar) + ' and '
        + AsCondWithFix(ex.args[1], needFix, nil, ex, isVar);
    '||':
      Result := AsCondWithFix(ex.args[0], needFix, nil, ex, isVar) + ' or '
        + AsCondWithFix(ex.args[1], needFix, nil, ex, isVar);
    '==':
      Result := AsCondWithFix(ex.args[0], False, nil, ex, isVar) + ' = '
        + AsCondWithFix(ex.args[1], False, nil, ex, isVar);
    '>=', '<=', '>', '<':
      Result := AsCondWithFix(ex.args[0], False, nil, ex, isVar) + ' ' + ex.op + ' '
        + AsCondWithFix(ex.args[1], False, nil, ex, isVar);
    '!=':
      Result := AsCondWithFix(ex.args[0], False, nil, ex, isVar) + ' <> '
        + AsCondWithFix(ex.args[1], False, nil, ex, isVar);
    '=':
      begin
        forAssignments.Add(StringOfChar(' ', indent * 2) + ex.AsString + ';');
        if needFix then
          Result := ex.args[0].AsString + ' <> 0'
        else
          Result := ex.args[0].AsString;
      end;
    else
      if needFix then
      begin
        needBrackets := parent <> nil;
        case ex.op of
        '#num': Result := ex.AsString + ' <> 0';
        '->':
          begin
            td := ex.args[0].GetType;
            if not Assigned(td) then
              // undeclared identifier => "id<>0"
              // or identifier without type
              Result := Result + '^.' + ex.args[1].AsString + ' <> 0'
            else
              if td.IsPointer and not td.IsClass then
              begin
                td := td.Deref;
                Result := ex.args[0].AsString + '^.'
                  + AsCondWithFix(ex.args[1], True, td, nil, False);
              end else
                // probably it is a class
                Result := ex.args[0].AsString + '.' + AsCondWithFix(ex.args[1],
                  True, td, nil, False);
          end;
        '#func':
          begin
            td := declIds.ReturnType(ex.args[0].args[0].op); // function result type
            if not Assigned(td) then
              // undeclared identifier => "id<>0"
              Result := ex.AsString + ' <> 0'
            else begin
              td := td.ResolveAlias;
              if td.TypeName = 'Boolean' then
              begin
                Result := ex.AsString;
                needBrackets := False;
              end else
              if td.IsPointer then
                Result := ex.AsString + ' <> nil'
              else
                Result := ex.AsString + ' <> 0'
            end;
          end;
        '#id':
          if cur_type = nil then
          begin
            td := declIds.VarType(ex.args[0].op);
            if not Assigned(td) then
              // undeclared identifier => "id<>0"
              Result := ex.AsString + ' <> 0'
            else begin
              td := td.ResolveAlias;
              if td.IsPointer then
                Result := ex.AsString + ' <> nil'
              else
                Result := ex.AsString + ' <> 0'
            end;
          end else begin
            // "#id" is in cur_type
            s := '';
            for i := 0 to cur_type.FullDecl.Count - 1 do
              if Pos(ex.args[0].op + ': ', cur_type.FullDecl[i]) > 0 then
              begin
                s := cur_type.FullDecl[i];
                Delete(s, 1, Pos(': ', s) + 1);
                s := System.Copy(s, 1, Pos(';', s) - 1);
                Break;
              end;
            if s = '' then
              raise Exception.Create('field "' + ex.args[0].op + '" not found in type "' + cur_type.TypeName + '"');
            if Pos('..', s) > 0 then
              s := 'cint';
            td := FdeclTypes.GetTypeByPasName(s);
            if not Assigned(td) then
              raise Exception.Create('type "' + s + '" not found');
            if td.IsPointer then
              Result := ex.AsString + ' <> nil'
            else
              Result := ex.AsString + ' <> 0'
          end;
        else
          raise Exception.Create('unknown!');
        end;
      end else
        Result := ex.AsString;
    end;
    if needBrackets then Result := '(' + Result + ')'
  end;

begin
  Result := AsCondWithFix(Self, True, nil, nil, True);
end;

function TExpr.GetType: TTypeDeclaration;

  function GetFieldType(expr: TExpr; typ: TTypeDeclaration): TTypeDeclaration;
  var s, fn: string; i, j: Integer; td: TTypeDeclaration;
  begin
    if typ = nil then Exit(nil);
    case expr.op of
    '[]':
      begin
        td := GetFieldType(expr.args[0], typ);
        s := td.OrigTypeName;
        if s[Length(s)] = '*' then
        begin
          s := System.Copy(s, 1, Length(s) - 1);
          Result := FdeclTypes.GetTypeByCName(s);
          Exit;
        end;
        if s[1] = 'P' then
        begin
          td := FdeclTypes.GetTypeByPasName(s);
          if Assigned(td) then Result := td.Depends[0]
          else Result := nil;
        end else
        if Pos('] of ', s) > 0 then
        begin
          Delete(s, 1, Pos('] of ', s) + 4);
          td := FdeclTypes.GetTypeByPasName(s);
          if Assigned(td) then Result := td
          else Result := nil;
        end else
          raise Exception.Create('array?');
      end;
    '#id':
      begin
        typ := typ.ResolveAlias;
        s := '';
        fn := FixFPCreserv(expr.args[0].op);
        for i := 1 to typ.FullDecl.Count - 1 do
        begin
          j := Pos(fn + ': ', typ.FullDecl[i]);
          if (j > 0) and (typ.FullDecl[i][j - 1] in [' ', '(']) then
          begin
            s := typ.FullDecl[i];
            Delete(s, 1, Pos(': ', s) + 1);
            if Pos(': ', s) > 0 then
              Delete(s, 1, Pos(': ', s) + 1);
            s := System.Copy(s, 1, Pos(';', s) - 1);
            Break;
          end;
        end;
        if System.Copy(s, 1, 5) = 'array' then
        begin
          Delete(s, 1, Pos(' of ', s) + 3);
          td := FdeclTypes.GetTypeByPasName(s);
          if Assigned(td) then
          begin
            Result := FdeclTypes.GetTypeByCName(td.OrigTypeName + '*');
            Exit;
          end else
            if s[1] = 'T' then s[1] := 'P' else s := 'P' + s;
        end;
        Result := FdeclTypes.GetTypeByPasName(s);
      end;
    '.': Result := GetFieldType(expr.args[1], GetFieldType(expr.args[0], typ));
    else
      raise Exception.Create('cannot determine expr field type for "' + expr.op + '"');
    end;
  end;

  function GetExprType(ex: TExpr): TTypeDeclaration;
  var
    i: Integer;
    d: Double;
    td: TTypeDeclaration;
    classMet: Boolean;
    str: string;
    postfix: set of Char;
    fs: TFormatSettings;
  begin
    Result := nil;
    with ex do
      case op of
      'u+': Result := GetExprType(args[0]);
      '<', '&&', '||', '==': Result := declTypes.GetTypeByPasName('Boolean');
      'sizeof': Result := declTypes.GetTypeByCName('int');
      '#meth':
        begin
          classMet := False;
          if args[0].op = '#meth' then
            td := GetExprType(args[0])
          else begin
            td := FdeclTypes.GetTypeByPasName(args[0].args[0].op); // for '#type' and '#id'
            if args[0].op <> '#type' then // return [(id)CFMakeCollectable(X) autorelease] (from NSObject.h)
            begin
              if not Assigned(td) then
                td := FdeclIds.VarType(args[0].op)
              else
                classMet := True;
            end;
          end;
          if td.OrigTypeName = 'id' then
            td := FdeclTypes.GetTypeByCName('NSObject');
          if not (td is TClassDeclaration) then
            raise Exception.Create('Method of non-class!');
          if classMet then
          begin
            with TClassDeclaration(td) do
              Result := MethodReturnType(FindMethodName(args[1].args[0].op, True));
            if Assigned(Result) and (Result.OrigTypeName = 'id') then
              Result := td; // class method returns 'id' => class
          end else
            with TClassDeclaration(td) do
              Result := MethodReturnType(FindMethodName(args[1].args[0].op, False))
        end;
      '#type':
        begin
          // args[0].op is a Pascal type
          Result := declTypes.GetTypeByPasName(args[0].op);
        end;
      '&', '|', '<<', '>>', 'u-', '~': Result := GetExprType(args[0]);
      '+', '-', '*', '/':
        if args[0].IsFloat then
          Result := args[0].GetType
        else
          Result := args[1].GetType;
      '.': Result := GetFieldType(args[1], FdeclIds.GetType(args[0].args[0].op));
      '->':
        begin
          td := GetExprType(args[0]);
          if Assigned(td) then
          begin
            if not td.IsClass and (td.DependCount > 0) and (td.FullDecl.Count = 1) then
              td := td.Depends[0];
            Result := GetFieldType(args[1], td);
            td := nil;
          end;;
        end;
      '#id':
        begin
          Result := FdeclIds.VarType(args[0].op);
          if Result = nil then
            Result := FdeclIds.ReturnType(args[0].op); // it may be global func/const, e.g. "pi"
        end;
      '#func': Result := FdeclIds.ReturnType(args[0].args[0].op);
      '#num':
        begin
          str := args[0].op;
          fs.DecimalSeparator := '.';
          fs.ThousandSeparator := #0;
          i := Length(str);
          if (i > 1) and (str[1] = '0') and (str[2] in ['x', 'X'])  then
          begin
            Delete(str, 1, 1);
            str[1] := '$';
            Dec(i);
            postfix := ['u','U','l','L'];
          end else
            postfix := ['u','U','l','L','f','F','d','D'];
          while (i > 0)
          and (str[i] in postfix) do
            Dec(i);
          str := System.Copy(str, 1, i);
          if str[1] = '''' then
            Result := FdeclTypes.GetTypeByCName('unsigned int')
          else
          if TryStrToInt(str, i) then
            Result := FdeclTypes.GetTypeByCName('int')
          else
          if TryStrToFloat(str, d, fs) then
            Result := FdeclTypes.GetTypeByCName('double')
          else
            raise Exception.Create('unknown "num" type');
          Exit;
        end;
      else
        raise Exception.Create('cannot determine expr type for "' + op + '"');
      end;
  end;

begin
  Result := GetExprType(Self);
end;

function TExpr.WithIf: Boolean;
begin
  if op = '?:' then
    Result := True
  else
    Result :=
      Assigned(args[0]) and args[0].WithIf or
      Assigned(args[1]) and args[1].WithIf or
      Assigned(args[2]) and args[2].WithIf;
end;

procedure TExpr.SplitIf(out cond, true_part, false_part: TExpr);

  function ChangeIf(var e: TExpr; out c: TExpr; toTrue: Boolean): Boolean;
  var t: TExpr;
  begin
    if e.op = '?:' then
    begin
      if toTrue then
      begin
        t := e.args[1];
        e.args[2].Free;
      end else begin
        t := e.args[2];
        e.args[1].Free;
      end;
      c := e.args[0];
      e.args[0] := nil;
      e.args[1] := nil;
      e.args[2] := nil;
      e.Free;
      e := t;
      Result := True;
    end else begin
      Result := Assigned(e.args[0]) and ChangeIf(e.args[0], c, toTrue);
      if Result then Exit;
      Result := Assigned(e.args[1]) and ChangeIf(e.args[1], c, toTrue);
      if Result then Exit;
      Result := Assigned(e.args[2]) and ChangeIf(e.args[2], c, toTrue);
    end;
  end;

begin
  true_part := Copy;
  ChangeIf(true_part, cond, True);
  cond.Free;
  false_part := Copy;
  ChangeIf(false_part, cond, False);
end;

function TExpr.Copy: TExpr;
begin
  Result := TExpr.Create(op, prior);
  if Assigned(args[0]) then
    Result.args[0] := args[0].Copy;
  if Assigned(args[1]) then
    Result.args[1] := args[1].Copy;
  if Assigned(args[2]) then
    Result.args[2] := args[2].Copy;
  Result.FdeclIds := FdeclIds;
  Result.FdeclTypes := FdeclTypes;
end;

procedure TExpr.Simplify;
var t: TExpr;
begin
  // simplify "?:"
  if (op = '?:') and (args[0].op = '#num') then
  begin
    if (args[0].args[0].op <> '0') then
    begin
      op := args[1].op;
      FreeAndNil(args[2]);
      FreeAndNil(args[0]);
      t := args[1];
      args[1] := nil;
      args[0] := t.args[0];
      t.args[0] := nil;
      args[1] := t.args[1];
      t.args[1] := nil;
      args[2] := t.args[2];
      t.args[2] := nil;
      t.Free;
    end else begin
      op := args[2].op;
      FreeAndNil(args[1]);
      FreeAndNil(args[0]);
      t := args[2];
      args[2] := nil;
      args[0] := t.args[0];
      t.args[0] := nil;
      args[1] := t.args[1];
      t.args[1] := nil;
      args[2] := t.args[2];
      t.args[2] := nil;
      t.Free;
    end;
  end;
  if Assigned(args[0]) then args[0].Simplify;
  if Assigned(args[1]) then args[1].Simplify;
  if Assigned(args[2]) then args[2].Simplify;
end;

end.

