{ extension of C proc bodies parser: Obj-C parser }
unit ocparser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, bparser, DeclTypes;

type

  TParsProc = procedure of object;

  { TObjCParser }

  TObjCParser = class(TBodyParser)
  private
    FCurMethodParams, FAllMethodParams: TStringList;
    FClassForward, FProtocolForward: TFPList;
    FHasClasses: Boolean;
    FProperties: TFPList;
    procedure ClearProperties;

    //function FixMethodName(const met_name: string): string; deprecated;
    function FixMethodParamName(td: TDeclarationWithMethods; const parm_name: string): string;

    procedure AddProtocolMethods(td: TDeclarationWithMethods; pd: TProtocolDeclaration);
    procedure AddProtocolsMethods(td: TDeclarationWithMethods; protocols: TStrings);
    procedure AddPropertyAccessors(td: TDeclarationWithMethods);
    procedure ObjC_Directive;
    procedure ClassPredefinitions;
    procedure ParseInterface;
    procedure ParseCategory(catName, clsName: string);
    procedure ParseMethod(td: TDeclarationWithMethods);
    procedure ParseMethods(td: TDeclarationWithMethods);
    procedure ParseProperty;
    procedure ParseProtocol;
    procedure ReadTillAtEnd;

    function DeclareClassType(OrigTypeName: string; InheritFrom: TClassDeclaration): TClassDeclaration; inline;
    function DeclareProtocolType(OrigTypeName: string): TProtocolDeclaration; inline;
    function DeclareCategoryType(CatName: string; extClass: TClassDeclaration): TCategoryDeclaration; inline;
  protected
    function F_: TExpr; override;
    function F1: TExpr; override;
    function F3: TExpr;
    function E_: TExpr; override;

    procedure InitPredefinedSymbols; override;
    function ReadType: TTypeDeclaration; override;

    procedure ParseNext; override;
    procedure AddTypeDeclarations(dest: TStrings); override;
    procedure DoAfterParse; override;
  public
    constructor Create(AFileName: string);
    destructor Destroy; override;
  end;

implementation

uses hparser, hscanner;

type

  { TProperty }

  TProperty = class
  public
    prop_type: TTypeDeclaration;
    getter, setter: string;
    prop_name: string;
    ReadOnly: Boolean;
    constructor Create;
    function Copy: TProperty;
  end;

{ TProperty }

constructor TProperty.Create;
begin
  ReadOnly := False;
end;

function TProperty.Copy: TProperty;
begin
  Result := TProperty.Create;
  with Result do
  begin
    prop_type := Self.prop_type;
    ReadOnly := Self.ReadOnly;
  end;
  // getter, setter, prop_name should not be copied!
end;

{ TObjCParser }

procedure TObjCParser.ClearProperties;
var i: Integer;
begin
  for i := 0 to FProperties.Count - 1 do
    TObject(FProperties[i]).Free;
  FProperties.Clear;
end;

function TObjCParser.FixMethodParamName(td: TDeclarationWithMethods;
  const parm_name: string): string;

  procedure ChangeParamName(var ParamName: string);
  begin
    if ParamName[1] = 'A' then
      ParamName := ParamName + '_'
    else
      ParamName := 'A' + ParamName;
  end;

begin
  Result := parm_name;
  if Result <> FixFPCreserv(Result) then
    ChangeParamName(Result);
  while td.HasMethod(Result) do
    ChangeParamName(Result);
  while FCurMethodParams.IndexOf(Result) >= 0 do
    ChangeParamName(Result);
  FCurMethodParams.Add(Result);
  FAllMethodParams.Add(Result);
end;

procedure TObjCParser.AddProtocolMethods(td: TDeclarationWithMethods;
  pd: TProtocolDeclaration);
var
  i: Integer;
  req: Boolean;
  //j: Integer; s: string;
begin
  DestAdd(usType, '    { from ' + pd.TypeName + ' }');
  req := True;
  for i := 2 to pd.FullDecl.Count - 2 do // skip first and last lines
  begin
    if Trim(pd.FullDecl[i]) = 'required' then
    begin
      req := True;
      Continue;
    end else
    if Trim(pd.FullDecl[i]) = 'optional' then
    begin
      req := False;
      Continue;
    end;
    {s := TrimLeft(pd.FullDecl[i]);
    if Copy(s, 1, 6) = 'class ' then Delete(s, 1, 6);
    if Copy(s, 1, 9) = 'function ' then
      Delete(s, 1, 9)
    else if Copy(s, 1, 10) = 'procedure ' then
      Delete(s, 1, 10);
    j := 1;
    while (j < Length(s)) and not (s[j] in [';', ' ', ':', '(']) do Inc(j);
    s := Copy(s, 1, j - 1);}

    if td.FullDecl.IndexOf(pd.FullDecl[i]) >= 0 then Continue;
    if req then
      DestAdd(usType, pd.FullDecl[i])
    else
      DestAdd(usType, '//' + pd.FullDecl[i] + ' { optional }')
  end;
end;

procedure TObjCParser.AddProtocolsMethods(td: TDeclarationWithMethods;
  protocols: TStrings);
var
  pd: TProtocolDeclaration;
  protName: string;
  protDone: TStringList;
  i: Integer;
  s: string;
begin
  protDone := TStringList.Create;
  try
    while protocols.Count > 0 do
    begin
      protName := protocols[0];
      protocols.Delete(0);
      if protDone.IndexOf(protName) >= 0 then Continue;
      protDone.Add(protName);
      pd := FDeclaredTypes.GetProtocol(protName);
      if not Assigned(pd) then
      begin
        DestAdd(usType, '    { protocol ' + protName + ' not found }');
        Continue;
      end;
      if not (td is TClassDeclaration)
      or not Assigned(TClassDeclaration(td).InheritFrom)
      or not TClassDeclaration(td).InheritFrom.ImplementsProtocol(pd) then
        AddProtocolMethods(td, pd);
      for i := pd.InheritProtCount - 1 downto 0 do
      begin
        s := pd.InheritProts[i].OrigTypeName;
        Delete(s, 1, 9); // deletes 'protocol ' from begining
        if protDone.IndexOf(s) < 0 then
          protocols.Insert(0, s);
      end;
    end;
  finally
    protDone.Free;
  end;
end;

procedure TObjCParser.AddPropertyAccessors(td: TDeclarationWithMethods);
var
  i: Integer;
  p: TProperty;
  mes, met_name: string;
begin
  DestAdd(usType, '    { property accessors }');
  for i := 0 to FProperties.Count - 1 do
  begin
    p := TProperty(FProperties[i]);
    // getter
    if p.getter <> '' then
    begin
      mes := p.getter;
      // this method can be already exist
      if td.MessageExists(mes) then
        mes := '';
    end else
      mes := p.prop_name;
    if mes <> '' then
    begin
      met_name := td.FixMethodName(mes, mes);
      DestAdd(usType, '    function ' + met_name + ': ' + p.prop_type.TypeName
        + '; message ''' + mes + ''';');
      td.AddMethodName(met_name, mes, p.prop_type);
    end;
    // setter
    if p.ReadOnly then Continue;
    if p.setter <> '' then
    begin
      mes := p.setter + ':'; // << check it
      // the method with such message can be already exist
      if td.MessageExists(mes) then
        mes := '';
    end else begin
      mes := p.prop_name;
      mes[1] := UpCase(mes[1]);
      mes := 'set' + mes;
    end;
    if mes <> '' then
    begin
      met_name := td.FixMethodName(mes, mes); // mes = message without last ":"
      DestAdd(usType, '    procedure ' + met_name + '(newValue: '
        + p.prop_type.TypeName + '); message ''' + mes + ':'';');
      td.AddMethodName(met_name, mes, nil);
    end;
  end;
end;

procedure TObjCParser.ObjC_Directive;
begin
  if cur_lex.eq('class') then
  begin
    NextLex;
    ClassPredefinitions;
  end else
  if cur_lex.eq('protocol') then
  begin
    NextLex;
    ParseProtocol;
  end else
  if cur_lex.eq('interface') then
  begin
    NextLex;
    ParseInterface;
  end else begin
    raise Exception.Create('unknown obj-c directive: ' + cur_lex.AsString);
  end;
end;

procedure TObjCParser.ClassPredefinitions;
var
  td: TTypeDeclaration;
begin
  FHasClasses := True;
  while not cur_lex.eq(';') do
  begin
    if not IsPredefined(cur_lex.AsString) then
    begin
      if FDeclaredTypes.GetTypeByCName(cur_lex.AsString) = nil then
      begin
        td := DeclareClassType(cur_lex.AsString, nil);
        FClassForward.Add(td);
        DestAdd(usType, '  ' + cur_lex.AsString + ' = objcclass;');
      end;
    end;
    NextLex; // ","
    if cur_lex.eq(',') then NextLex;
  end;
  NextLex;
end;

procedure TObjCParser.ParseInterface;
var
  clsName, sInheritance, str: string;
  protocols: TStringList;
  i: Integer;
  td: TClassDeclaration;
  p: TProtocolDeclaration;
begin
  FHasClasses := True;
  clsName := cur_lex.AsString;
  NextLex;
  if cur_lex.eq('(') then
  begin
    // it is objccategory (class helper for objcclass)
    //  @interface NSObject (NSCoderMethods)
    //    NSCoderMethods = objccategory external (NSObject)
    NextLex;
    ParseCategory(cur_lex.AsString, clsName);
    Exit;
  end;
  if IsPredefined(clsName) then
  begin
    ReadTillAtEnd;
    Exit;
  end;
//  @interface NSObject <NSObject>
//    NSObject = objcclass external (NSObjectProtocol)
//  @interface NSNotificationCenter : NSObject
  sInheritance := '';
  if cur_lex.eq(':') then
  begin
    NextLex;
    sInheritance := cur_lex.AsString;
    NextLex;
  end;
  td := FDeclaredTypes.GetTypeByCName(clsName) as TClassDeclaration;
  if td = nil then
    td := DeclareClassType(clsName, FDeclaredTypes.GetClass(sInheritance))
  else begin
    td.UpdateDeclFilename(Scanner.CurFileName, Scanner.CurFrameworkUnit);
    td.InheritFrom := FDeclaredTypes.GetClass(sInheritance);
  end;
  if Assigned(td.InheritFrom) then
    td.InheritFrom.Use(FAdditionalTypes);
  td.FullDecl.Clear;
  protocols := TStringList.Create;
  try
    if cur_lex.eq('<') then
    begin
      // protocols
      repeat
        NextLex;
        if sInheritance <> '' then
          sInheritance := sInheritance + ', ';
        sInheritance := sInheritance + cur_lex.AsString + 'Protocol';
        p := FDeclaredTypes.GetProtocol(cur_lex.AsString);
        TClassDeclaration(td).AddInheritProt(p);
        p.Use(FAdditionalTypes);
        protocols.Add(cur_lex.AsString);
        NextLex
      until cur_lex.eq('>');
      NextLex;
    end;
    PushType(td);
    DestAdd(usType, '  { ' + clsName + ' }');
    str := '  ' + clsName + ' = objcclass external';
    if td.OrigTypeName <> td.TypeName then
      str := str + ' name ''' + td.OrigTypeName + ''''; // there is no such classes (yet?)
    if sInheritance <> '' then
      str := str + ' (' + sInheritance + ')';
    DestAdd(usType, str);
    // fields
    if cur_lex.eq('{') then
    begin
      // TODO : parse objcclass fields (strict protected) (if it needed at all)
      // for now skip fields
      NextLex;
      i := 1;
      while i > 0 do
      begin
        if cur_lex.eq('{') then Inc(i) else
        if cur_lex.eq('}') then Dec(i);
        NextLex;
      end;
    end;
    DestAdd(usType, '  public');
    FCurMethodParams.Clear;
    FAllMethodParams.Clear;
    ParseMethods(td);
    AddProtocolsMethods(td, protocols);
    DestAdd(usType, '  end;');
    PopType;
  finally
    protocols.Free;
  end;
end;

procedure TObjCParser.ParseCategory(catName, clsName: string);
var
  ext_name, newCatName: string;
  td: TCategoryDeclaration;
  //cls_td: TClassDeclaration;
  protocols: TStringList;
begin
  FHasClasses := True;
  ext_name := '';
  while not cur_lex.eq(')') do NextLex;
  if IsPredefined(catName) then
  begin
    ReadTillAtEnd;
    Exit;
  end;
  newCatName := FDeclaredTypes.GenNewName(catName, '_' + clsName);
  td := DeclareCategoryType(newCatName, FDeclaredTypes.GetClass(clsName));
  td.ExtendedClass.Use(FAdditionalTypes);
  PushType(td);
  if newCatName <> catName then
    ext_name := 'name ''' + catName + ''' ';
  NextLex;
  protocols := TStringList.Create;
  try
    if cur_lex.eq('<') then
    begin
      NextLex;
      while not cur_lex.eq('>') do
      begin
        if cur_lex.LexType <> ltSymbol then
          raise Exception.Create('identifier expected!');
        protocols.Add(cur_lex.AsString);
        NextLex;
        if cur_lex.eq(',') then NextLex;
      end;
      NextLex;
    end;
    DestAdd(usType, '  { ' + catName + ' category for class ' + clsName + ' }');
    DestAdd(usType, '  ' + newCatName + ' = objccategory external ' + ext_name + '(' + clsName + ')');
    FCurMethodParams.Clear;
    FAllMethodParams.Clear;
    ParseMethods(td);
    AddProtocolsMethods(td, protocols);
    DestAdd(usType, '  end;');
    PopType;
  finally
    protocols.Free;
  end;
end;

procedure TObjCParser.ParseMethod(td: TDeclarationWithMethods);
var
  str, stype, ptype, mes, params, param_name: string;
  _varargs: Boolean;
  postfix, newMethodName: string;
  isClassMethod: Boolean;
  retType: TTypeDeclaration;
begin
  // TODO : [beauty] consequence of params with the same type: p1:t1; p2:t1 => p1,p2:t1
  FCurMethodParams.Clear;
  _varargs := False;
  postfix := '';
  str := '    ';
  isClassMethod := cur_lex.eq('+');
  if isClassMethod then
    str := str + 'class '
  else
    if not cur_lex.eq('-') then Exit;
  NextLex;
  if not cur_lex.eq('(') then
  begin
    // without return type => use "id"
    // e.g. in NSURL:
    // - initWithString:(NSString *)URLString;
    stype := 'id';
    retType := FDeclaredTypes.GetTypeByPasName('id');
    retType.Use(FAdditionalTypes);
  end else begin
    NextLex;
    if cur_lex.eq('oneway') then
    begin
      postfix := ' { oneway }';
      NextLex;
    end;
    retType := ReadType;
    stype := retType.TypeName;
    if not cur_lex.eq(')') then
    begin
      str := '//' + str;
      postfix := ' // result is a pointer to function'; // TODO : result is a pointer to function
      stype := 'FuncPtr';
      while True do
        if cur_lex.eq(')') then
        begin
          NextLex;
          if cur_lex.LexType = ltSymbol then Break;
        end else
          NextLex;
    end else
      NextLex;
  end;
  if stype = 'void' then
    str := str + 'procedure '
  else
    str := str + 'function ';
  mes := cur_lex.AsString;
  newMethodName := mes;
  NextLex;
  params := '';
  while cur_lex.eq(':') do
  begin
    mes := mes + ':';
    NextLex;
    if not cur_lex.eq('(') then
    begin
      str := '//' + str;
      postfix := ' // param type must be in breakets' ;
      Break;
    end;
    NextLex;
    if cur_lex.eq('out') then
      NextLex  // ignore "out" for method parameters
    else if cur_lex.eq('inout') then
      NextLex; // ignore "inout" too, and what about "in"?
    ptype := ReadType.TypeName;
    if cur_lex.eq('(') then
    begin
      str := '//' + str;
      postfix := ' // param is a pointer to function'; // TODO : param is a pointer to function
      ptype := 'FuncPtr';
      while True do
        if cur_lex.eq(')') then
        begin
          NextLex;
          if cur_lex.LexType = ltSymbol then Break
        end else
          NextLex;
    end else
    if not cur_lex.eq(')') then
    begin
      str := '//' + str;
      postfix := ' // ")" of param type not found';
      Break;
    end else
      NextLex;
    if cur_lex.LexType <> ltSymbol then
    begin
      str := '//' + str;
      postfix := ' // param name is not identifier ("' + cur_lex.AsString + '")';
      Break;
    end;
    if params <> '' then
      params := params + '; ';
    param_name := FixMethodParamName(td, cur_lex.AsString);
    td.AddParamName(param_name);
    params := params + param_name + ': ' + ptype;
    NextLex;
    if cur_lex.LexType = ltSymbol then
    begin
      mes := mes + cur_lex.AsString;
      newMethodName := newMethodName + '_' + cur_lex.AsString;
      NextLex;
    end else
    if cur_lex.eq(',') then
    begin
      NextLex;
      if cur_lex.eq('...') then
      begin
        _varargs := True;
        NextLex;
      end else begin
        str := '//' + str;
        postfix := ' // after ",": "..." expected but "' + cur_lex.AsString + '" found';
      end;
      Break;
    end;
  end;
  if isClassMethod then
  begin
    // method names collision with class method names:
    // NSProxy: contains class method "respondsToSelector"
    // and implements NSProjectProtocol that contains method "respondsToSelector"
    newMethodName[1] := upCase(newMethodName[1]);
    newMethodName := 'class' + newMethodName;
  end;
  newMethodName := td.FixMethodName(newMethodName, mes);
  td.AddMethodName(newMethodName, mes, retType);
  str := str + newMethodName;
  if params <> '' then
    str := str + '(' + params + ')';
  if stype <> 'void' then
    str := str + ': ' + stype;
  if _varargs then
    str := str + '; varargs';
  str := str + '; message ''' + mes + ''';';
  { resolve compiler hint: }
{ compiler issues hint: add "override", but after this adding cannot compile
  if (td is TClassDeclaration) then
    str := str + TClassDeclaration(td).NeededModifier(newMethodName, str); }
  if not cur_lex.eq(';') then
    str := '//' + str + ' // deprecated?';
  str := str + postfix;
  DestAdd(usType, str);
  // read till ";"
  while not cur_lex.eq(';') do NextLex;
  NextLex;
end;

procedure TObjCParser.ParseMethods(td: TDeclarationWithMethods);
begin
  ClearProperties;
  while Assigned(cur_lex) do
  begin
    if cur_lex.eq('-') or cur_lex.eq('+') then
      ParseMethod(td)
    else
    if cur_lex.eq('@') then
    begin
      NextLex;
      if cur_lex.eq('end') then
      begin
        if FProperties.Count > 0 then
          AddPropertyAccessors(td as TDeclarationWithMethods);
        NextLex;
        Break;
      end else
      if cur_lex.eq('property') then
      begin
        NextLex;
        ParseProperty;
      end else
      if (td is TProtocolDeclaration)
      and (cur_lex.eq('required') or cur_lex.eq('optional')) then
      begin
        DestAdd(usType, '  ' + cur_lex.AsString);
        NextLex
      end else begin
        raise Exception.Create('unhandled directive: ' + cur_lex.AsString);
      end;
    end else begin
      // outside code
      if cur_lex.eq(';') then
      begin
        NextLex;
        // interface AVVideoCompositionLayerInstruction (AVFoundation/AVVideoComposition.h)
        // after instance variable part { } ";"
      end else
      if cur_lex.eq('typedef') then
      begin
        NextLex;
        TypeDef
      end else
      if cur_lex.eq('enum') then
      begin
        NextLex;
        EnumToConsts
      end else
      if cur_lex.eq('extern') then
      begin
        NextLex;
        VarProcDefinition
      end else
        raise Exception.Create('unknown outside code: ' + cur_lex.AsString);
    end;
  end;
end;

procedure TObjCParser.ParseProperty;
var
  p: TProperty;
begin
  p := TProperty.Create;
  FProperties.Add(p);
  if cur_lex.eq('(') then
  begin
    // property attributes
    // not all properties have attributes (e.g. CAMediaTiming.h from QuartzCore)
    NextLex;
    while not cur_lex.eq(')') do
    begin
      if cur_lex.eq('getter') then
      begin
        NextLex;
        if not cur_lex.eq('=') then
          raise Exception.Create('"=" expected');
        NextLex;
        p.getter := cur_lex.AsString;
        NextLex;
      end else
      if cur_lex.eq('setter') then
      begin
        NextLex;
        if not cur_lex.eq('=') then
          raise Exception.Create('"=" expected');
        NextLex;
        p.setter := cur_lex.AsString;
        NextLex;
      end else
      if cur_lex.eq('readonly') then
      begin
        NextLex;
        p.ReadOnly := True;
      end else
        NextLex; // non interested attribute
      if cur_lex.eq(',') then NextLex;
    end;
    NextLex;
  end;
  p.prop_type := ReadType;
  if cur_lex.LexType <> ltSymbol then
  begin
    if cur_lex.eq('(') then
    begin
      NextLex;
      if cur_lex.eq('^') then
      begin
        // C BLOCK - skip this property, in any case it cannot be used now
        FProperties.Remove(p);
        p.Free;
        while not cur_lex.eq(';') do NextLex;
        NextLex;
        Exit;
      end;
    end;
    raise Exception.Create('identifier (property name) expected!');
  end;
  p.prop_name := cur_lex.AsString;
  NextLex;
  // @property(copy) NSString *minificationFilter, *magnificationFilter;
  while cur_lex.eq(',') do
  begin
    if (p.getter <> '') or (p.setter <> '') then
      raise Exception.Create('several properties with one getter/setter');
    NextLex;
    p := p.Copy;
    FProperties.Add(p);
    if cur_lex.eq('*') and p.prop_type.IsClass then NextLex;
    if cur_lex.LexType <> ltSymbol then
      raise Exception.Create('identifier (property name) expected!');
    p.prop_name := cur_lex.AsString;
    NextLex;
  end;
  if not cur_lex.eq(';') then
    raise Exception.Create('";" expected!');
  NextLex;
end;

procedure TObjCParser.ParseProtocol;
var
  protoName, str, sInheritance: string;
  p, pd: TProtocolDeclaration;
begin
  FHasClasses := True;
  protoName := cur_lex.AsString;
  NextLex;
  if cur_lex.eq(',') or cur_lex.eq(';') then
  begin
    // predeclarations
    repeat
      if not IsPredefined('protocol ' + protoName) then
      begin
        if FDeclaredTypes.GetTypeByCName('protocol ' + protoName) = nil then
        begin
          pd := DeclareProtocolType(protoName); // predeclaration
          if FProtocolForward.IndexOf(pd) < 0 then
            FProtocolForward.Add(pd);
        end;
        DestAdd(usType, '  ' + protoName + 'Protocol = objcprotocol;');
      end;
      if not cur_lex.eq(';') then
      begin
        NextLex;
        if cur_lex.LexType <> ltSymbol then
          raise Exception.Create('identifier expected');
        protoName := cur_lex.AsString;
        NextLex;
      end else Break;
    until False;
    NextLex;
    Exit;
  end;
  if IsPredefined('protocol ' + protoName) then
  begin
    // predefined
    ReadTillAtEnd;
    Exit;
  end;
  // new protocol => parse it!
  sInheritance := '';
  pd := FDeclaredTypes.GetTypeByCName('protocol ' + protoName) as TProtocolDeclaration;
  if pd = nil then
    pd := DeclareProtocolType(protoName)
  else
    pd.UpdateDeclFilename(Scanner.CurFileName, Scanner.CurFrameworkUnit);
  if cur_lex.eq('<') then
  begin
    // read inheritance
    repeat
      NextLex;
      if sInheritance <> '' then
        sInheritance := sInheritance + ', ';
      sInheritance := sInheritance + cur_lex.AsString + 'Protocol';
      p := FDeclaredTypes.GetProtocol(cur_lex.AsString);
      if not Assigned(p) then
        raise Exception.Create('Protocol "' + cur_lex.AsString + '" not found!');
      pd.AddInheritProt(p);
      p.Use(FAdditionalTypes);
      NextLex;
    until cur_lex.eq('>');
    NextLex;
  end;
  PushType(pd);
  DestAdd(usType, '  { ' + protoName + ' Protocol }');
  str := '  ' + protoName + 'Protocol = objcprotocol external name ''' + protoName + '''';
  if sInheritance <> '' then
    str := str + ' (' + sInheritance + ')';
  DestAdd(usType, str);
  FCurMethodParams.Clear;
  FAllMethodParams.Clear;
  ParseMethods(pd);
  DestAdd(usType, '  end;');
  PopType;
end;

procedure TObjCParser.ReadTillAtEnd;
begin
  repeat
    while Assigned(cur_lex) and not cur_lex.eq('@') do NextLex;
    NextLex;
  until not Assigned(cur_lex) or cur_lex.eq('end');
  NextLex;
end;

function TObjCParser.DeclareClassType(OrigTypeName: string;
  InheritFrom: TClassDeclaration): TClassDeclaration;
begin
  Result := FDeclaredTypes.DeclareClassType(OrigTypeName, InheritFrom, Scanner);
end;

function TObjCParser.DeclareProtocolType(OrigTypeName: string): TProtocolDeclaration;
begin
  Result := FDeclaredTypes.DeclareProtocolType(OrigTypeName, Scanner);
end;

function TObjCParser.DeclareCategoryType(CatName: string;
  extClass: TClassDeclaration): TCategoryDeclaration;
begin
  Result := FDeclaredTypes.DeclareCategoryType(CatName, extClass, Scanner);
end;

//* F	-> 	 "@"<str> | old.F
function TObjCParser.F_: TExpr;
begin
  if cur_lex.eq('@') then
  begin
    NextLex;
    Result := TExpr.Create('@str');
    if cur_lex.LexType <> ltString then
      ErrorLexExpected('string');
    Result.args[0] := TExpr.Create(cur_lex.AsPasString);
    NextLex;
  end else
    Result := inherited F_;
end;

//* F3   ->       E <ident>[ : E11 ] {<ident>:E11} {"," E11} "]"
function TObjCParser.F3: TExpr; // ObjC method call
var MetName, msg: string; tmp: TExpr;
begin
  Result := TExpr.Create('#meth'); {0-class 1-MetName 2-#param}
  Result.args[0] := E_();
  if cur_lex.LexType <> ltSymbol then
    raise Exception.Create('identifier expected');
  MetName := cur_lex.AsString;
  msg := cur_lex.AsString;
  NextLex;
  if cur_lex.eq(':') then
  begin
    NextLex;
    msg := msg + ':';
    Result.args[2] := TExpr.Create('#param');
    Result.args[2].args[0] := E11();
    tmp := Result.args[2];
  end;
  while cur_lex.LexType = ltSymbol do
  begin
    MetName := MetName + '_' + cur_lex.AsString;
    msg := msg + cur_lex.AsString + ':';
    NextLex;
    if not cur_lex.eq(':') then
      raise Exception.Create('":" expected');
    NextLex;
    tmp.args[1] := TExpr.Create('#param');
    tmp := tmp.args[1];
    tmp.args[0] := E11();
  end;
  Result.args[1] := TExpr.Create(MetName);
  Result.args[1].args[0] := TExpr.Create(msg);
  { varargs }
  while cur_lex.eq(',') do
  begin
    NextLex;
    tmp.args[1] := TExpr.Create('#param');
    tmp := tmp.args[1];
    tmp.args[0] := E11();
  end;
  if not cur_lex.eq(']') then
    raise Exception.Create('"]" expected');
  NextLex;
end;

//* F1   ->   "[" F3 | old.F1
function TObjCParser.F1: TExpr;
begin
  if cur_lex.eq('[') then
  begin
    NextLex;
    Result := F3();
  end else
    Result := inherited F1;
end;

//* E    ->   "@" <str> | old.E
function TObjCParser.E_: TExpr;
begin
  if cur_lex.eq('@') then
  begin
    NextLex;
    if cur_lex.LexType <> ltString then
      raise Exception.Create('Constant string expected after "@"!');
    Result := TExpr.Create('@');
    Result.args[0] := TExpr.Create(cur_lex.AsPasString);
    NextLex;
  end else
    Result := inherited E_;
end;

procedure TObjCParser.InitPredefinedSymbols;
begin
  inherited InitPredefinedSymbols;
  with FPredefinedSymbols do
  begin
    // from rtl/inc/objcbase.pp
    //Add('NSString'); // will be parsed from NSString.h
    Add('True');
    Add('False');
    Add('Boolean');
    Add('PBoolean'); // ?
    //Add('NSInvocation');
    //Add('NSMethodSignature');
    //Add('NSCoder');
    Add('NSInteger');
    Add('NSUInteger');
    Add('struct _NSZone');
    Add('NSZone'); // ?
    Add('Protocol');
    Add('protocol NSObject');
    Add('NSObject');
    //Add('NSCoderMethods');
    //Add('NSCopyingProtocol');
    //Add('NSMutableCopyingProtocol');
    //Add('NSCodingProtocol');
    //Add('NSDiscardableContentProtocol');
  end;
end;

function TObjCParser.ReadType: TTypeDeclaration;
begin
  Result := inherited ReadType;
  if cur_lex.eq('<') then
  begin
    if (Result.OrigTypeName = 'id') or (Result.OrigTypeName = 'Class') then
    begin
      // e.g. (id <SomeProtocolName>)paramName
      // it means that paramName is of type SomeProtocolName
      NextLex;
      Result := FDeclaredTypes.GetTypeByCName('protocol ' + cur_lex.AsString);
      if Result = nil then
      begin
        WriteLn('Type "protocol ', cur_lex.AsString, '" not declared!');
        raise Exception.Create('Type "protocol ' + cur_lex.AsString + '" not declared!');
      end;
      NextLex;
      // it is possible id <SomeProtocolName1,SomeProtocolName2> (e.g. property of UIImagePickerController)
      // in this case first protocol is more interesting than others
      while not cur_lex.eq('>') do NextLex;
      NextLex;
    end else begin
      // UITextInputStringTokenizer:
      // - (id)initWithTextInput:(UIResponder <UITextInput> *)textInput;
      // just skip protocol
      while not cur_lex.eq('>') do NextLex;
      NextLex;
      // here must be "*"
      if cur_lex.eq('*') then
        NextLex; // Result.TypeName is likely a class, so "*" need be consumed
    end;
  end;
end;

procedure TObjCParser.ParseNext;
begin
  if cur_lex.eq('@') then
  begin
    NextLex;
    ObjC_Directive;
  end else
    inherited ParseNext;
end;

procedure TObjCParser.AddTypeDeclarations(dest: TStrings);
var
  i: Integer;
  td: TTypeDeclaration;
  str: string;
begin
  if FTypes.Count = 0 then Exit;
  dest.Add('type');

  if FClassForward.Count > 0 then
    dest.Add('  { classes forward declarations }');
  for i := 0 to FClassForward.Count - 1 do
  begin
    td := TTypeDeclaration(FClassForward[i]);
    if td.FullDecl.Count = 0 then
    begin
      str := '  ' + td.TypeName + ' = objcclass external';
      if td.OrigTypeName <> td.TypeName then
        str := str + ' name ''' + td.OrigTypeName + '''';
      str := str + ';';
      td.FullDecl.Add(str);
      dest.Add(str); // anonym
    end else
      dest.Add('  ' + td.TypeName + ' = objcclass;');
  end;
  if FClassForward.Count > 0 then
    dest.Add('');

  if FProtocolForward.Count > 0 then
    dest.Add('  { protocols forward declarations }');
  for i := 0 to FProtocolForward.Count - 1 do
  begin
    td := TTypeDeclaration(FProtocolForward[i]);
    if td.FullDecl.Count = 0 then
    begin
      str := '  ' + td.TypeName + ' = objcprotocol external';
      if td.OrigTypeName <> td.TypeName then
        str := str + ' name ''' + td.OrigTypeName + '''';
      str := str + ';';
      td.FullDecl.Add(str);
      dest.Add(str); // anonym
    end else
      dest.Add('  ' + td.TypeName + ' = objcprotocol;');
  end;
  if FProtocolForward.Count > 0 then
    dest.Add('');

  i := dest.Count;
  inherited AddTypeDeclarations(dest);
  repeat
    // due to removing '$packrecords' it can run out
    if dest[i] = 'type' then
    begin
      dest.Delete(i); // delete second 'type'
      Break;
    end;
    Dec(i);
  until i < 0;
end;

procedure TObjCParser.DoAfterParse;
begin
  if FHasClasses and not FDeclaredTypes.GetTypeByCName('id').Avail then
    FRefFrameworks.Add(FDeclaredTypes.GetTypeByCName('id').NeedUnit);
  inherited DoAfterParse;
end;

constructor TObjCParser.Create(AFileName: string);
begin
  inherited;
  { to fix identifiers: method names and method params }
  FCurMethodParams := TStringList.Create;
  FCurMethodParams.Sorted := True;
  FAllMethodParams := TStringList.Create;
  FAllMethodParams.Sorted := True;
  FClassForward := TFPList.Create;
  FProtocolForward := TFPList.Create;
  FProperties := TFPList.Create;
end;

destructor TObjCParser.Destroy;
begin
  ClearProperties;
  FProperties.Free;
  FProtocolForward.Free;
  FClassForward.Free;
  FCurMethodParams.Free;
  FAllMethodParams.Free;
  inherited Destroy;
end;

end.

