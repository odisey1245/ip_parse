unit DeclTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, hscanner;

type

  TDeclaredTypes = class;

  { TTypeDeclaration }

  TTypeDeclaration = class
  private
    FOwner: TDeclaredTypes;
    FOrigTypeName: string; // C type name
    FTypeName: string;     // Pas type name
    FDeclaration: string;  // it's only hint
    FDeclFileName: string;
    FAvail: Boolean;
    FDepends: TFPList;
    FFullDecl: TStringList;
    FUsed: Boolean;
    FExtern: Boolean;
    FIsClass: Boolean;
    FIsVoid: Boolean;
    FNeedUnit: string;
    FPackRecords: string;
    function GetDepends(Index: Integer): TTypeDeclaration; inline;
  public
    constructor Create(origName, typName, decl, declFileName: string; AIsClass: Boolean = False);
    constructor CreateBuiltIn(origName, typName, declFileName: string; AIsClass: Boolean = False; ANeedUnit: string = '*');
    destructor Destroy; override;
    function ResolveAlias: TTypeDeclaration;
    procedure DependOn(typ: TTypeDeclaration);
    procedure Use(AUsedList: TFPList);
    procedure Use;
    procedure SetNeedUnit(ANeedUnit: string); inline;
    procedure Reset; inline;
    procedure MakeAvail; inline;
    function IsPointer: Boolean; inline;
    function DependCount: Integer; inline;
    function IsEmptyRecord: Boolean; inline;
    function IsRecord: Boolean;
    procedure UpdateDeclFilename(declFileName, NewNeedUnit: string); inline;
    function Deref: TTypedeclaration;
    procedure MakeEmptyRecord;
    procedure RefillEmptyRecord;

    property Avail: Boolean read FAvail;
    property Used: Boolean read FUsed;
    property Extern: Boolean read FExtern;
    property IsClass: Boolean read FIsClass;
    property IsVoid: Boolean read FIsVoid write FIsVoid;
    property NeedUnit: string read FNeedUnit;
    property TypeName: string read FTypeName;
    property OrigTypeName: string read FOrigTypeName;
    property Declaration: string read FDeclaration;
    property FullDecl: TStringList read FFullDecl;
    property DeclFileName: string read FDeclFileName;
    property Depends[Index: Integer]: TTypeDeclaration read GetDepends;
    property PackRecords: string read FPackRecords;
  end;

  { TDeclarationWithMethods }

  TDeclarationWithMethods = class(TTypeDeclaration)
  private
    FMethodNames, FMethodParams: TStringList;
    FInheritProts: TFPList;
  public
    constructor Create(origName, typName, AdeclFileName: string);
    destructor Destroy; override;
    function MessageExists(mes: string): Boolean;
    procedure AddMethodName(metName, messageName: string; retType: TTypeDeclaration);
    procedure AddMethodsAndParams(src: TDeclarationWithMethods);
    procedure AddParamName(paramName: string);
    procedure GetAllMethodParams(dst: TStrings);
    function FindMethodName(AMessage: string; IsClassMethod: Boolean): string; virtual;
    function FixMethodName(AMethodName, AMessageName: string): string; virtual; abstract;
    function HasMethod(AMethodName: string): Boolean; virtual;
    function MethodReturnType(PasName: string): TTypeDeclaration; virtual;
  end;

  { TProtocolDeclaration }

  TProtocolDeclaration = class(TDeclarationWithMethods)
  private
    function GetInheritProts(Index: Integer): TProtocolDeclaration; inline;
  public
    constructor Create(origName, typName, AdeclFileName: string);
    destructor Destroy; override;
    procedure AddInheritProt(pd: TProtocolDeclaration);
    function FixMethodName(AMethodName, AMessageName: string): string; override;
    function InheritProtCount: Integer; inline;
    property InheritProts[Index: Integer]: TProtocolDeclaration read GetInheritProts;
  end;

  TClassDeclaration = class;

  { TCategoryDeclaration }

  TCategoryDeclaration = class(TDeclarationWithMethods)
  private
    FExtendedClass: TClassDeclaration;
  public
    constructor Create(origName, typName, AdeclFileName: string;
      extClass: TClassDeclaration);
    destructor Destroy; override;
    function FixMethodName(AMethodName, AMessageName: string): string; override;
    property ExtendedClass: TClassDeclaration read FExtendedClass;
  end;

  { TClassDeclaration }

  TClassDeclaration = class(TProtocolDeclaration)
  private
    FInheritFrom: TClassDeclaration;
    FCategories: TFPList;
  public
    constructor Create(origName, typName, AdeclFileName: string;
      inheritFrom: TClassDeclaration);
    constructor CreateBuiltIn(origName, AdeclFileName: string;
      ANeedUnit: string = '*'; inheritFrom: TClassDeclaration = nil);
    destructor Destroy; override;
    function FindMethodName(AMessage: string; IsClassMethod: Boolean): string; override;
    function FixMethodName(AMethodName, AMessageName: string): string; override;
    function HasMethod(AMethodName: string): Boolean; override;
    function ImplementsProtocol(pd: TProtocolDeclaration): Boolean;
    function MethodReturnType(PasName: string): TTypeDeclaration; override;
    function NeededModifier(AMethodName, AMethodDeclaration: string): string;
    property InheritFrom: TClassDeclaration read FInheritFrom write FInheritFrom;
  end;

  { TDeclaredTypes }

  TDeclaredTypes = class
  private
    FCTypes, FPasTypes: TStringList; // objs are TTypeDecl
    FNewTypes, FEmptyRecords: TFPList;
    FPackRecords: string;
    function CreateTypeDeclaration(origName, typName, decl, declFileName: string;
      AIsClass: Boolean = False): TTypeDeclaration;
    function AddBuiltInProtocol(OrigProtoName: string): TProtocolDeclaration;
    function AddBuiltInClass(OrigClassName: string): TClassDeclaration;
    function AddBuiltInType(OrigName, PasName, aDecl, ADeclInFile, ANeedUnit: string): TTypeDeclaration;
  public
    constructor Create;
    destructor Destroy; override;
    // main functionality
    function DeclareAuxType(OrigTypeName, typeDeclaration: string;
      Scanner: THeaderScanner): TTypeDeclaration;
    function DeclareType(OrigTypeName, typeDeclaration: string;
      withPointer: Boolean; Scanner: THeaderScanner): TTypeDeclaration;
    function DeclarePointerType(OrigTypeName: string; Scanner: THeaderScanner): TTypeDeclaration;
    function DeclareClassType(OrigTypeName: string; InheritFrom: TClassDeclaration; Scanner: THeaderScanner): TClassDeclaration;
    function DeclareProtocolType(OrigTypeName: string; Scanner: THeaderScanner): TProtocolDeclaration;
    function DeclareCategoryType(OrigTypeName: string; extClass: TClassDeclaration; Scanner: THeaderScanner): TCategoryDeclaration;
    function ReplaceTypeDef(old_typ, td: TTypeDeclaration): TTypeDeclaration;
    procedure DeleteTypeByOrigName(c_name: string);
    // maintain
    procedure DeleteNew(AUnitName: string);
    procedure CommitNew;
    procedure MakeAvailable(AUnitName: string);
    procedure ResetAvailable;
    //
    procedure InitPredefinedTypes;
    procedure AddPredefinedObjCTypes;
    function GetTypeByPasName(p_name: string): TTypeDeclaration; inline;
    function TypeByPasName(p_name: string): TTypeDeclaration; inline;
    function GetTypeByCName(c_name: string): TTypeDeclaration; inline;
    function GetPointerTypeFor(td: TTypeDeclaration): TTypeDeclaration;
    function GetProtocol(protocolName: string): TProtocolDeclaration;
    function GetClass(clsName: string): TClassDeclaration;
    function GenNewName(const Prefix: string; Postfix: string = ''): string;
    procedure SetPackRecords(directive: string);
  end;

function FixFPCreserv(const sname: string): string;

implementation

var
  FPC_Reserved: TStringList;

function FixFPCreserv(const sname: string): string;
begin
  Result := sname;
  if FPC_Reserved.IndexOf(Result) >= 0 then
    Result := Result + '_';
end;

{ TCategoryDeclaration }

constructor TCategoryDeclaration.Create(origName, typName,
  AdeclFileName: string; extClass: TClassDeclaration);
begin
  inherited Create(origName, typName, AdeclFileName);
  FDeclaration := 'objccategory';
  FExtendedClass := extClass;
  FExtendedClass.FCategories.Add(Self);
end;

destructor TCategoryDeclaration.Destroy;
var i: Integer;
begin
  i := FExtendedClass.FCategories.IndexOf(Self);
  if i >= 0 then
    FExtendedClass.FCategories.Delete(i);
  inherited Destroy;
end;

function TCategoryDeclaration.FixMethodName(AMethodName, AMessageName: string): string;
var i: Integer;
begin
  Result := FixFPCreserv(AMethodName);
  i := 0;
  while i < FMethodNames.Count do
  begin
    if (FMethodNames.ValueFromIndex[i] <> AMessageName)
    and (FMethodNames.Names[i] = Result) then
    begin
      Result := Result + '_';
      i := 0;
      Continue;
    end;
    Inc(i);
  end;
  Result := FExtendedClass.FixMethodName(Result, AMessageName);
end;

{ TDeclarationWithMethods }

function TDeclarationWithMethods.MessageExists(mes: string): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 1 to FFullDecl.Count - 1 do
    if Pos(' message ''' + mes + '''', FFullDecl[i]) > 0 then Exit;
  Result := False;
end;

constructor TDeclarationWithMethods.Create(origName, typName,
  AdeclFileName: string);
begin
  inherited Create(origName, typName, '', AdeclFileName, True);
  FMethodNames := TStringList.Create;
  FMethodParams := TStringList.Create;
  FInheritProts := TFPList.Create;
end;

destructor TDeclarationWithMethods.Destroy;
begin
  FInheritProts.Free;
  FMethodNames.Free;
  FMethodParams.Free;
  inherited Destroy;
end;

procedure TDeclarationWithMethods.AddMethodName(metName, messageName: string;
  retType: TTypeDeclaration);
begin
  FMethodNames.AddObject(metName + '=' + messageName, retType);
end;

procedure TDeclarationWithMethods.AddParamName(paramName: string);
begin
  FMethodParams.Add(paramName);
end;

function TDeclarationWithMethods.HasMethod(AMethodName: string): Boolean;
var
  i: Integer;
begin
  Result := FMethodNames.IndexOfName(AMethodName) >= 0;
  if not Result then
  begin
    Result := True;
    for i := 0 to FInheritProts.Count - 1 do
      if TProtocolDeclaration(FInheritProts[i]).HasMethod(AMethodName) then
        Exit;
    Result := False;
  end;
end;

function TDeclarationWithMethods.MethodReturnType(PasName: string): TTypeDeclaration;
var i: Integer;
begin
  i := FMethodNames.IndexOfName(PasName);
  if i < 0 then
    Result := nil
  else
    Result := TTypeDeclaration(FMethodNames.Objects[i]);
end;

procedure TDeclarationWithMethods.GetAllMethodParams(dst: TStrings);
begin
  dst.AddStrings(FMethodParams);
end;

function TDeclarationWithMethods.FindMethodName(AMessage: string;
  IsClassMethod: Boolean): string;
var
  i, j, k: Integer;
  metName: string;
begin
  Result := '';
  for i := 0 to FMethodNames.Count - 1 do
    if FMethodNames.ValueFromIndex[i] = AMessage then
    begin
      metName := FMethodNames.Names[i];
      for j := 1 to FFullDecl.Count - 1 do
      begin
        k := Pos(' ' + metName, FFullDecl[j]);
        if (k > 0) and (FFullDecl[j][k + Length(metName) + 1] in [' ', '(', ':']) then
          if (Copy(FFullDecl[j], 5, 5) = 'class') = IsClassMethod then
          begin
            Result := metName;
            Exit;
          end;
      end;
    end;
end;

procedure TDeclarationWithMethods.AddMethodsAndParams(
  src: TDeclarationWithMethods);
begin
  FMethodNames.AddStrings(src.FMethodNames);
  FMethodParams.AddStrings(src.FMethodParams);
end;

{ TClassDeclaration }

constructor TClassDeclaration.Create(origName, typName, AdeclFileName: string;
  inheritFrom: TClassDeclaration);
begin
  inherited Create(origName, typName, AdeclFileName);
  FDeclaration := 'objcclass';
  FinheritFrom := inheritFrom;
  FCategories := TFPList.Create;
end;

constructor TClassDeclaration.CreateBuiltIn(origName, AdeclFileName: string;
  ANeedUnit: string; inheritFrom: TClassDeclaration);
begin
  Create(origName, origName, AdeclFileName, inheritFrom);
  FAvail := ANeedUnit = '*';
  FExtern := False;
  FNeedUnit := ANeedUnit;
end;

destructor TClassDeclaration.Destroy;
var i: Integer;
begin
  for i := FCategories.Count - 1 downto 0 do
    FOwner.DeleteTypeByOrigName(TCategoryDeclaration(FCategories[i]).FOrigTypeName);
  FCategories.Free;
  inherited Destroy;
end;

function TClassDeclaration.FindMethodName(AMessage: string;
  IsClassMethod: Boolean): string;
var
  i: Integer;
begin
  Result := inherited;
  if Result = '' then
    for i := 0 to FInheritProts.Count - 1 do
    begin
      Result := TProtocolDeclaration(FInheritProts[i]).FindMethodName(AMessage, IsClassMethod);
      if Result <> '' then Exit;
    end;
  for i := 0 to FCategories.Count - 1 do
  begin
    Result := TCategoryDeclaration(FCategories[i]).FindMethodName(AMessage, IsClassMethod);
    if Result <> '' then Exit;
  end;
  if Assigned(FInheritFrom) then
    Result := FInheritFrom.FindMethodName(AMessage, IsClassMethod);
end;

function TClassDeclaration.FixMethodName(AMethodName, AMessageName: string): string;
begin
  Result := inherited FixMethodName(AMethodName, AMessageName); // current class + protocols
  if Assigned(FInheritFrom) then
    Result := FInheritFrom.FixMethodName(Result, AMessageName);
end;

function TClassDeclaration.HasMethod(AMethodName: string): Boolean;
var
  i: Integer;
begin
  Result := inherited HasMethod(AMethodName);
  if not Result and Assigned(FInheritFrom) then
    Result := FInheritFrom.HasMethod(AMethodName);
  if not Result then
  begin
    Result := True;
    for i := 0 to FCategories.Count - 1 do
      if TCategoryDeclaration(FCategories[i]).HasMethod(AMethodName) then
        Exit;
    Result := False;
  end;
end;

function TClassDeclaration.MethodReturnType(PasName: string): TTypeDeclaration;
var
  i: Integer;
begin
  Result := inherited MethodReturnType(PasName);
  if Assigned(Result) then Exit;
  for i := 0 to FCategories.Count - 1 do
  begin
    Result := TCategoryDeclaration(FCategories[i]).MethodReturnType(PasName);
    if Assigned(Result) then Exit;
  end;
  for i := 0 to InheritProtCount - 1 do
  begin
    Result := InheritProts[i].MethodReturnType(PasName);
    if Assigned(Result) then Exit;
  end;
  if Assigned(FInheritFrom) then
    Result := FInheritFrom.MethodReturnType(PasName);
end;

{ returns modifier to silent compiler hint: override/reintroduce }
function TClassDeclaration.NeededModifier(AMethodName, AMethodDeclaration: string): string;
var
  newParams: string;

  function ExtractParamList(declStr: string): string;
  var
    i, p: Integer;
    s, t: string;
  begin
    Result := '';
    p := Pos('(', declStr);
    if p = 0 then Exit;
    Delete(declStr, 1, p);
    declStr := Copy(declStr, 1, Pos(')', declStr) - 1) + ';';
    while declStr <> '' do
    begin
      p := Pos(';', declStr);
      s := Copy(declStr, 1, p - 1);
      Delete(declStr, 1, p);
      if Pos(':', s) > 0 then
        t := Trim(Copy(s, Pos(':', s) + 1, MaxInt))
      else
        t := Trim(s);
      p := 1;
      i := Pos(',', s);
      while i > 0 do
      begin
        Inc(p);
        Delete(s, 1, i);
        i := Pos(',', s);
      end;
      for i := 1 to p do
        Result := Result + UpperCase(t) + ';';
    end;
  end;

  function FindMethod(cls: TClassDeclaration): string;
  var
    i: Integer;
    s: string;
  begin
    Result := '';
    if cls = nil then Exit;
    for i := 1 to cls.FullDecl.Count - 2 do
    begin
      s := Trim(cls.FullDecl[i]);
      if (s = '') or (s[1] in ['{', '/'] {comments})
      or not (UpCase(s[1]) in ['C'{lass}, 'F'{unction}, 'P'{rocedure}]) then
        Continue;
      if (s[1] in ['c', 'C']) then
        if SameText(Copy(s, 1, 6), 'class ') then
        begin
          Delete(s, 1, 6);
          s := TrimLeft(s);
          if (s = '') or not (s[1] in ['f', 'F', 'p', 'P']) then Continue;
        end else Continue;
      if (s[1] in ['f', 'F']) and SameText(Copy(s, 1, 9), 'function ') then
      begin
        Delete(s, 1, 9);
        s := TrimLeft(s);
      end else
      if SameText(Copy(s, 1, 10), 'procedure ') then
      begin
        Delete(s, 1, 10);
        s := TrimLeft(s);
      end else Continue;
      if not SameText(Copy(s, 1, Length(AMethodName)), AMethodName) then
        Continue;
      Delete(s, 1, Length(AMethodName));
      if (s = '') or (s[1] in ['a'..'z', 'A'..'Z', '0'..'9', '_']) then
        Continue;
      { it is our client -> compare param lists }
      if SameText(ExtractParamList(s), newParams) then
        Result := ' override;'
      else
        Result := ' reintroduce;';
      Exit;
    end;
    Result := FindMethod(cls.InheritFrom);
  end;

begin
  { maybe in future we will need additional parametrs: IsClassMethod, RetType }
  Result := '';
  if Assigned(InheritFrom) then
  begin
    newParams := ExtractParamList(AMethodDeclaration);
    Result := FindMethod(FInheritFrom);
  end;
end;

function TClassDeclaration.ImplementsProtocol(pd: TProtocolDeclaration): Boolean;
begin
  Result := FInheritProts.IndexOf(pd) >= 0;
  if not Result and Assigned(FInheritFrom) then
    Result := FInheritFrom.ImplementsProtocol(pd);
end;

{ TProtocolDeclaration }

function TProtocolDeclaration.GetInheritProts(Index: Integer): TProtocolDeclaration;
begin
  Result := TProtocolDeclaration(FInheritProts[Index]);
end;

constructor TProtocolDeclaration.Create(origName, typName, AdeclFileName: string);
begin
  inherited Create(origName, typName, AdeclFileName);
  FDeclaration := 'objcprotocol';
end;

destructor TProtocolDeclaration.Destroy;
begin
  inherited Destroy;
end;

procedure TProtocolDeclaration.AddInheritProt(pd: TProtocolDeclaration);
begin
  if not Assigned(pd) then
    raise Exception.Create('inherited protocol not assigned!');
  if FInheritProts.IndexOf(pd) < 0 then
    FInheritProts.Add(pd);
end;

function TProtocolDeclaration.FixMethodName(AMethodName, AMessageName: string): string;
var i: Integer;
begin
  Result := FixFPCreserv(AMethodName);
  i := 0;
  while i < FMethodNames.Count do
  begin
    if (FMethodNames.ValueFromIndex[i] <> AMessageName)
    and (FMethodNames.Names[i] = Result) then
    begin
      Result := Result + '_';
      i := 0;
      Continue;
    end;
    Inc(i);
  end;
  for i := 0 to FInheritProts.Count - 1 do
    Result := TProtocolDeclaration(FInheritProts[i]).FixMethodName(Result, AMessageName);
end;

function TProtocolDeclaration.InheritProtCount: Integer;
begin
  Result := FInheritProts.Count;
end;

{ TTypeDeclaration }

function TTypeDeclaration.GetDepends(Index: Integer): TTypeDeclaration;
begin
  Result := TTypeDeclaration(FDepends[Index]);
end;

constructor TTypeDeclaration.Create(origName, typName, decl,
  declFileName: string; AIsClass: Boolean);
begin
  FOrigTypeName := origName;
  FUsed := False;
  FTypeName := typName;
  FDeclaration := decl;
  FDeclFileName := declFileName;
  FDepends := TFPList.Create;
  FFullDecl := TStringList.Create;
  FExtern := True;
  FAvail := False;
  FIsClass := AIsClass;
end;

constructor TTypeDeclaration.CreateBuiltIn(origName, typName,
  declFileName: string; AIsClass: Boolean; ANeedUnit: string);
begin
  Create(origName, typName, '<built-in>', declFileName, AIsClass);
  FAvail := ANeedUnit = '*';
  FExtern := False;
  FNeedUnit := ANeedUnit;
end;

destructor TTypeDeclaration.Destroy;
begin
  FFullDecl.Free;
  FDepends.Free;
  inherited Destroy;
end;

function TTypeDeclaration.ResolveAlias: TTypeDeclaration;
var s: string; typ: TTypeDeclaration;
begin
  Result := nil;
  typ := Self;
  while (Result <> typ) and (typ.FFullDecl.Count = 1)
  and (Pos(';', typ.FFullDecl[0]) > 0) do
  begin
    Result := typ;
    // it is only alias => go to orig
    s := typ.FFullDecl[0];
    Delete(s, 1, Pos(' = ', s) + 2);
    s := System.Copy(s, 1, Pos(';', s) - 1);
    if s[1] = '^' then Exit; // it is not alias but new type declaration
    typ := FOwner.GetTypeByPasName(s);
  end;
  Result := typ;
end;

procedure TTypeDeclaration.DependOn(typ: TTypeDeclaration);
begin
  if not Assigned(typ) then
    raise exception.Create('[DependOn] type expected!');
  if typ = Self then Exit; // ignore circular referencing
  if FDepends.IndexOf(typ) < 0 then
    FDepends.Add(typ);
end;

procedure TTypeDeclaration.Use(AUsedList: TFPList);
var
  i: Integer;
begin
  if FUsed or FAvail then Exit;
  FUsed := True;
  if NeedUnit = '' then
    for i := 0 to FDepends.Count - 1 do
      TTypeDeclaration(FDepends[i]).Use(AUsedList);
  if AUsedList.IndexOf(Self) < 0 then
    AUsedList.Add(Self);
end;

procedure TTypeDeclaration.Use;
begin
  FUsed := True;
  if FOrigTypeName[Length(FOrigTypeName)] = '*' then
    Deref.Use;
  // use depends?
end;

procedure TTypeDeclaration.SetNeedUnit(ANeedUnit: string);
begin
  FNeedUnit := ANeedUnit;
end;

procedure TTypeDeclaration.Reset;
begin
  FUsed := False;
  if FAvail and (FNeedUnit <> '*') then
    FAvail := False;
end;

procedure TTypeDeclaration.MakeAvail;
begin
  FAvail := True;
end;

function TTypeDeclaration.IsPointer: Boolean;
begin
  Result := (FOrigTypeName[Length(FOrigTypeName)] = '*') or FIsClass
    or (FOrigTypeName = 'id')
    or (FFullDecl.Count > 0) and (Pos(' = ^', FFullDecl[0]) > 0);
end;

function TTypeDeclaration.DependCount: Integer;
begin
  Result := FDepends.Count;
end;

function TTypeDeclaration.IsEmptyRecord: Boolean;
begin
  Result := (FFullDecl.Count = 0)
    or (Pos(' = record', FFullDecl[0]) > 0)
    and ((Pos(' end;', FFullDecl[0]) > 0) or (FFullDecl.Count >= 2) and (Pos(' end;', FFullDecl[1]) > 0));
end;

function TTypeDeclaration.IsRecord: Boolean;
begin
  Result := (FFullDecl.Count > 0) and (Pos('record', FFullDecl[0]) > 0)
end;

procedure TTypeDeclaration.UpdateDeclFilename(declFileName, NewNeedUnit: string);
begin
  FDeclFileName := declFileName;
  FNeedUnit := NewNeedUnit;
end;

function TTypeDeclaration.Deref: TTypedeclaration;
var s: string;
begin
  s := FOrigTypeName;
  if (s = '') or (s[Length(s)] <> '*') then
    raise Exception.CreateFmt('cannot deref "%s" type', [s]);
  s := Copy(s, 1, Length(s) - 1);
  Result := FOwner.GetTypeByCName(s);
end;

procedure TTypeDeclaration.MakeEmptyRecord;
begin
  if (Copy(FOrigTypeName, 1, 7) <> 'struct ')
  and (Copy(FOrigTypeName, 1, 6) <> 'union ') then
    raise Exception.Create('OrigTypeName is not "struct" or "union"');
  FFullDecl.Text := '  ' + FTypeName + ' = record end;';
  FDepends.Clear;
end;

procedure TTypeDeclaration.RefillEmptyRecord;
begin
  FFullDecl.Clear;
  if FOwner.FEmptyRecords.IndexOf(Self) < 0 then
    FOwner.FEmptyRecords.Add(Self);
end;

{ TDeclaredTypes }

function TDeclaredTypes.CreateTypeDeclaration(origName, typName, decl,
  declFileName: string; AIsClass: Boolean): TTypeDeclaration;
begin
  Result := TTypeDeclaration.Create(origName, typName, decl, declFileName, AIsClass);
  FNewTypes.Add(Result);
  Result.FPackRecords := FPackRecords;
  Result.FOwner := Self;
end;

constructor TDeclaredTypes.Create;
begin
  FPackRecords := '{$packrecords c}';
  FCTypes := TStringList.Create;
  FCTypes.CaseSensitive := True;
  FCTypes.Sorted := True;
  FPasTypes := TStringList.Create;
  FPasTypes.CaseSensitive := False;
  FPasTypes.Sorted := True;
  FPasTypes.Duplicates := dupAccept;
  FNewTypes := TFPList.Create;
  FEmptyRecords := TFPList.Create;
end;

destructor TDeclaredTypes.Destroy;
begin
  while FCTypes.Count > 0 do
    DeleteTypeByOrigName(TTypeDeclaration(FCTypes.Objects[0]).FOrigTypeName);
  FCTypes.Free;
  FPasTypes.Free;
  FNewTypes.Free;
  FEmptyRecords.Free;
  inherited Destroy;
end;

{ inline arrays }
function TDeclaredTypes.DeclareAuxType(OrigTypeName, typeDeclaration: string;
  Scanner: THeaderScanner): TTypeDeclaration;
var
  i: Integer;
  base_type, els, pp: string;
  td: TTypeDeclaration;
begin
  i := FCTypes.IndexOf(OrigTypeName);
  if i < 0 then
  begin
    i := FCTypes.Add(OrigTypeName);
    if Pos('[', OrigTypeName) = 0 then
      raise Exception.Create('[DeclareAuxType] not array!');
    base_type := Copy(OrigTypeName, 1, Pos('[', OrigTypeName) - 1);
    els := Copy(OrigTypeName, Pos('[', OrigTypeName) + 1, MaxInt);
    els := Copy(els, 1, Pos(']', els) - 1);
    td := GetTypeByCName(base_type);
    if not Assigned(td) then
      raise Exception.Create('[DeclareAuxType] cannot find base type of array!');
    pp := td.TypeName + els + 'Array';
    if pp[1] <> 'T' then pp := 'T' + pp;
    while FPasTypes.IndexOf(pp) >= 0 do pp := pp + '_'; // e.g. UniChar, unichar
    td := CreateTypeDeclaration(OrigTypeName, pp, typeDeclaration, Scanner.CurFileName);
    FCTypes.Objects[i] := td;
    FPasTypes.AddObject(pp, td);
    Result := td;
    td.FAvail := True;
    td.SetNeedUnit('');
    td.FExtern := False;
  end else begin
    td := TTypeDeclaration(FCTypes.Objects[i]);
    WriteLn('type "', OrigTypeName, '" already declared in "',
      td.FDeclFileName, '" as');
    WriteLn(td.FTypeName, ' = ', td.FDeclaration);
    raise Exception.Create('duplicate type declaration!');
  end;
end;

function TDeclaredTypes.DeclareType(OrigTypeName, typeDeclaration: string;
  withPointer: Boolean; Scanner: THeaderScanner): TTypeDeclaration;
var
  i: Integer; s, p, pp: string;
  td1, td2: TTypeDeclaration;
begin
  // type => Ttype; type* => Ptype
  i := FCTypes.IndexOf(OrigTypeName);
  if i < 0 then
  begin
    i := FCTypes.Add(OrigTypeName);
    pp := OrigTypeName;
    if pp[Length(pp)] = '*' then
      pp := Copy(pp, 1, Length(pp) - 1);
    if Pos(' ', pp) > 0 then
      pp := pp[1] + Trim(Copy(pp, Pos(' ', pp) + 1, MaxInt));
    s := FixFPCreserv('T' + pp);
    while FPasTypes.IndexOf(s) >= 0 do s := s + '_'; // e.g. UniChar, unichar
    td1 := CreateTypeDeclaration(OrigTypeName, s, typeDeclaration, Scanner.CurFileName);
    FCTypes.Objects[i] := td1;
    FPasTypes.AddObject(s, td1);
    Result := td1;
    if Scanner.CurHeaderBelongsToFramework then
    begin
      td1.FAvail := True;
      td1.SetNeedUnit(Scanner.CurFrameworkUnit);
      td1.FExtern := False;
    end;
    if withPointer then
    begin
      i := FCTypes.Add(OrigTypeName + '*');
      p := FixFPCreserv('P' + pp);
      while FPasTypes.IndexOf(p) >= 0 do p := p + '_';
      td2 := CreateTypeDeclaration(OrigTypeName + '*', p, '^' + s, Scanner.CurFileName);
      FCTypes.Objects[i] := td2;
      FPasTypes.AddObject(p, td2);
      td2.DependOn(td1);
      if Scanner.CurHeaderBelongsToFramework then
      begin
        td2.FAvail := True;
        td2.SetNeedUnit(Scanner.CurFrameworkUnit);
        td2.FExtern := False;
      end;
    end;
  end else begin
    td1 := TTypeDeclaration(FCTypes.Objects[i]);
    WriteLn('type "', OrigTypeName, '" already declared in "',
      td1.FDeclFileName, '" as');
    WriteLn(td1.FTypeName, ' = ', td1.FDeclaration);
    raise Exception.Create('duplicate type declaration!');
  end;
end;

function TDeclaredTypes.DeclarePointerType(OrigTypeName: string;
  Scanner: THeaderScanner): TTypeDeclaration;
var i: Integer; s: string; td_o, td_n: TTypeDeclaration;
begin
  if FCTypes.IndexOf(OrigTypeName + '*') >= 0 then
    raise Exception.Create('type "' + OrigTypeName + '*" already declared!');
  td_o := GetTypeByCName(OrigTypeName);
  s := td_o.FTypeName;
  if s[1] = 'T' then s[1] := 'P' else s := 'P' + s;
  s := FixFPCreserv(s);
  i := FCTypes.Add(OrigTypeName + '*');
  td_n := CreateTypeDeclaration(OrigTypeName + '*', s, '^' + td_o.FTypeName, Scanner.CurFileName);
  FCTypes.Objects[i] := td_n;
  FPasTypes.AddObject(s, td_n);
  td_n.DependOn(td_o);
  Result := td_n;
  if Scanner.CurHeaderBelongsToFramework then
  begin
    td_n.FAvail := td_o.Avail;
    td_n.SetNeedUnit(Scanner.CurFrameworkUnit);
    td_n.FExtern := False;
  end;
end;

function TDeclaredTypes.DeclareClassType(OrigTypeName: string;
  InheritFrom: TClassDeclaration; Scanner: THeaderScanner): TClassDeclaration;
var
  i: Integer; s: string;
  cd: TClassDeclaration;
  td: TTypeDeclaration;
begin
  i := FCTypes.IndexOf(OrigTypeName);
  if i < 0 then
  begin
    i := FCTypes.Add(OrigTypeName);
    s := FixFPCreserv(OrigTypeName); // if it is not FPC keyword, otherwise 'extern name' needed
    while FPasTypes.IndexOf(s) >= 0 do
      s := s + '_';  // 'GKMatchMakerViewController' vs. 'GKMatchmakerViewController' from GLKit
    cd := TClassDeclaration.Create(OrigTypeName, s, Scanner.CurFileName, InheritFrom);
    FNewTypes.Add(cd);
    cd.FOwner := Self;
    FCTypes.Objects[i] := cd;
    FPasTypes.AddObject(s, cd);
    Result := cd;
    if Scanner.CurHeaderBelongsToFramework then
    begin
      cd.FAvail := True;
      cd.SetNeedUnit(Scanner.CurFrameworkUnit);
      cd.FExtern := False;
    end;
  end else begin
    td := TTypeDeclaration(FCTypes.Objects[i]);
    WriteLn('type "', OrigTypeName, '" already declared in "',
      td.FDeclFileName, '" as');
    WriteLn(td.FTypeName, ' = ', td.FDeclaration);
    raise Exception.Create('duplicate type declaration!');
  end;
end;

function TDeclaredTypes.DeclareProtocolType(OrigTypeName: string;
  Scanner: THeaderScanner): TProtocolDeclaration;
var
  i: Integer; s: string;
  pd: TProtocolDeclaration;
  td: TTypeDeclaration;
begin
  // protocol type => typeProtocol
  i := FCTypes.IndexOf('protocol ' + OrigTypeName);
  if i < 0 then
  begin
    i := FCTypes.Add('protocol ' + OrigTypeName);
    s := FixFPCreserv(OrigTypeName + 'Protocol');
    pd := TProtocolDeclaration.Create('protocol ' + OrigTypeName, s, Scanner.CurFileName);
    FNewTypes.Add(pd);
    pd.FOwner := Self;
    FCTypes.Objects[i] := pd;
    FPasTypes.AddObject(s, pd);
    Result := pd;
    if Scanner.CurHeaderBelongsToFramework then
    begin
      pd.FAvail := True;
      pd.SetNeedUnit(Scanner.CurFrameworkUnit);
      pd.FExtern := False;
    end;
  end else begin
    td := TTypeDeclaration(FCTypes.Objects[i]);
    WriteLn('Protocol "', OrigTypeName, '" already declared in "',
      td.FDeclFileName, '" as');
    WriteLn(td.FTypeName, ' = ', td.FDeclaration);
    raise Exception.Create('duplicate type declaration!');
  end;
end;

function TDeclaredTypes.DeclareCategoryType(OrigTypeName: string;
  extClass: TClassDeclaration; Scanner: THeaderScanner): TCategoryDeclaration;
var
  i: Integer; s: string;
  td: TTypeDeclaration;
  cad: TCategoryDeclaration;
begin
  i := FCTypes.IndexOf(OrigTypeName);
  if i < 0 then
  begin
    i := FCTypes.Add(OrigTypeName);
    s := FixFPCreserv(OrigTypeName);
    cad := TCategoryDeclaration.Create(OrigTypeName, s, Scanner.CurFileName, extClass);
    FNewTypes.Add(cad);
    cad.FOwner := Self;
    FCTypes.Objects[i] := cad;
    FPasTypes.AddObject(s, cad);
    Result := cad;
    if Scanner.CurHeaderBelongsToFramework then
    begin
      cad.FAvail := True;
      cad.SetNeedUnit(Scanner.CurFrameworkUnit);
      cad.FExtern := False;
    end;
  end else begin
    td := TTypeDeclaration(FCTypes.Objects[i]);
    WriteLn('type "', OrigTypeName, '" already declared in "',
      td.FDeclFileName, '" as');
    WriteLn(td.FTypeName, ' = ', td.FDeclaration);
    raise Exception.Create('duplicate type declaration!');
  end;
end;

function TDeclaredTypes.ReplaceTypeDef(old_typ,
  td: TTypeDeclaration): TTypeDeclaration;
var i: Integer; ptd: TTypeDeclaration;
begin
  // it's better to remove it
  i := FCTypes.IndexOf(old_typ.FOrigTypeName);
  FCTypes.Objects[i] := td;
  i := FPasTypes.IndexOf(old_typ.FTypeName);
  FPasTypes.Delete(i);

  td.FOrigTypeName := old_typ.FOrigTypeName;
  td.FTypeName := old_typ.FTypeName;
  td.FDeclaration := old_typ.FDeclaration;
  td.FDeclFileName := old_typ.FDeclFileName;
  td.FAvail := old_typ.FAvail;
  td.FUsed := old_typ.FUsed;
  td.FExtern := old_typ.FExtern;
  td.FNeedUnit := old_typ.FNeedUnit;
  td.FIsClass := old_typ.FIsClass;
  td.FIsVoid := old_typ.FIsVoid;
  td.FPackRecords := old_typ.FPackRecords;

  FPasTypes.AddObject(td.FTypeName, td);
  { now all references need to be updated ... }
  { for now check only Type* }
  ptd := GetTypeByCName(td.FOrigTypeName + '*');
  if Assigned(ptd) then
    with ptd do
      for i := 0 to FDepends.Count - 1 do
        if FDepends[i] = Pointer(old_typ) then
          FDepends[i] := td;

  i := FNewTypes.IndexOf(old_typ);
  if i >= 0 then FNewTypes[i] := td;
  old_typ.Free;
  Result := td;
end;

procedure TDeclaredTypes.DeleteTypeByOrigName(c_name: string);
var
  i: Integer;
  td: TTypeDeclaration;
begin
  with FCTypes do
  begin
    i := IndexOf(c_name);
    if i < 0 then
      raise Exception.Create('Type "' + c_name + '" not found');
    td := TTypeDeclaration(Objects[i]);
    Delete(i);
  end;
  with FPasTypes do
  begin
    i := IndexOf(td.FTypeName);
    if i < 0 then
      raise Exception.Create('Type "' + c_name + '" not found in Pas types');
    // assumption: IndexOf returns first position => forward search
    repeat
      if Objects[i] <> td then Inc(i);
      if (i >= Count) or (Strings[i] <> td.FTypeName) then
        raise Exception.Create('Type "' + c_name + '" not found in Pas types');
    until Objects[i] = td;
    Delete(i);
  end;
  FNewTypes.Remove(td);
  td.Free; // TODO : handle dependencies
end;

function TDeclaredTypes.AddBuiltInProtocol(OrigProtoName: string): TProtocolDeclaration;
begin
  if FCTypes.IndexOf('protocol ' + OrigProtoName) >= 0 then
    raise Exception.Create('built-in protocol redeclaration!');
  Result := TProtocolDeclaration.Create('protocol ' + OrigProtoName, OrigProtoName + 'Protocol', 'objcbase.pp');
  FCTypes.AddObject('protocol ' + OrigProtoName, Result);
  FPasTypes.AddObject(OrigProtoName + 'Protocol', Result);
  Result.FExtern := False;
  Result.FNeedUnit := '{$modeswitch objectivec1}';
end;

function TDeclaredTypes.AddBuiltInClass(OrigClassName: string): TClassDeclaration;
begin
  if FCTypes.IndexOf(OrigClassName) >= 0 then
    raise Exception.Create('built-in class redeclaration!');
  Result := TClassDeclaration.Create(OrigClassName, OrigClassName, 'objcbase.pp', nil);
  Result.FOwner := Self;
  FCTypes.AddObject(OrigClassName, Result);
  FPasTypes.AddObject(OrigClassName, Result);
  Result.FExtern := False;
  Result.FNeedUnit := '{$modeswitch objectivec1}';
end;

function TDeclaredTypes.AddBuiltInType(OrigName, PasName, aDecl, ADeclInFile, ANeedUnit: string): TTypeDeclaration;
begin
  if FCTypes.IndexOf(OrigName) >= 0 then
    raise Exception.Create('built-in type redeclaration!');
  Result := TTypeDeclaration.Create(OrigName, PasName, aDecl, ADeclInFile, False);
  Result.FOwner := Self;
  FCTypes.AddObject(OrigName, Result);
  FPasTypes.AddObject(PasName, Result);
  Result.FAvail := ANeedUnit = '*';
  Result.FExtern := False;
  Result.FNeedUnit := ANeedUnit;
end;

procedure TDeclaredTypes.DeleteNew(AUnitName: string);
var
  i: Integer;
  td: TTypeDeclaration;
begin
  for i := 0 to FEmptyRecords.Count - 1 do
    TTypeDeclaration(FEmptyRecords[i]).MakeEmptyRecord;
  FEmptyRecords.Clear;
  for i := FNewTypes.Count - 1 downto 0 do
  begin
    td := TTypeDeclaration(FNewTypes[i]);
    with FCTypes do
      Delete(IndexOf(td.FOrigTypeName));
    with FPasTypes do
      Delete(IndexOf(td.FTypeName));
    td.Free;
  end;
  FNewTypes.Clear;
  for i := 0 to FPasTypes.Count - 1 do
  begin
    td := TTypeDeclaration(FPasTypes.Objects[i]);
    if td.NeedUnit = AUnitName then
      td.SetNeedUnit('');
  end;
end;

procedure TDeclaredTypes.CommitNew;
begin
  FNewTypes.Clear;
  FEmptyRecords.Clear;
end;

procedure TDeclaredTypes.MakeAvailable(AUnitName: string);
var i: Integer;
begin
  for i := 0 to FCTypes.Count - 1 do
    with TTypeDeclaration(FCTypes.Objects[i]) do
      if not Avail and (NeedUnit = AUnitName) then
        MakeAvail;
end;

procedure TDeclaredTypes.ResetAvailable;
var
  i: Integer;
begin
  for i := 0 to FCTypes.Count - 1 do
    TTypeDeclaration(FCTypes.Objects[i]).Reset;
end;

procedure TDeclaredTypes.InitPredefinedTypes;

  procedure AddWithPointer(C_typename, Pas_typename, PtrPas_typename: string; NeedUnit: string;
    DeclaredInFileName: string = '<no file>');
  var i: Integer; ptd, td: TTypeDeclaration;
  begin
    i := FCTypes.Add(C_typename);
    td := TTypeDeclaration.CreateBuiltIn(C_typename, Pas_typename, DeclaredInFileName, False, NeedUnit);
    FCTypes.Objects[i] := td;
    FPasTypes.AddObject(Pas_typename, FCTypes.Objects[i]);
    i := FCTypes.Add(C_typename + '*');
    ptd := TTypeDeclaration.CreateBuiltIn(C_typename + '*', PtrPas_typename, DeclaredInFileName, False, NeedUnit);
    FCTypes.Objects[i] := ptd;
    FPasTypes.AddObject(PtrPas_typename, FCTypes.Objects[i]);
    ptd.DependOn(td);
  end;

  procedure Add(C_typename, Pas_typename: string; NeedUnit: string;
    DeclaredInFileName: string = '<no file>');
  var i: Integer; td: TTypeDeclaration;
  begin
    i := FCTypes.Add(C_typename);
    td := TTypeDeclaration.CreateBuiltIn(C_typename, Pas_typename, DeclaredInFileName, False, NeedUnit);
    FCTypes.Objects[i] := td;
    FPasTypes.AddObject(Pas_typename, FCTypes.Objects[i]);
    if C_typename[Length(C_typename)] = '*' then
    begin
      { add dependance for pointer type }
      C_typename := Copy(C_typename, 1, Length(C_typename) - 1);
      i := FCTypes.IndexOf(C_typename);
      if i >= 0 then
        td.DependOn(TTypeDeclaration(FCTypes.Objects[i]));
    end;
  end;

  procedure Add(C_typename, Pas_typename, NeedUnit: string; IsClass: Boolean;
    DeclaredInFileName: string = '<no file>');
  var i: Integer;
  begin
    i := FCTypes.Add(C_typename);
    FCTypes.Objects[i] :=
      TTypeDeclaration.CreateBuiltIn(C_typename, Pas_typename, DeclaredInFileName, IsClass, NeedUnit);
    FPasTypes.AddObject(Pas_typename, FCTypes.Objects[i]);
  end;

  procedure AddClass(ClsName, NeedUnit: string; DeclaredInFileName: string = '<no file>');
  var i: Integer;
  begin
    i := FCTypes.Add(ClsName);
    FCTypes.Objects[i] :=
      TClassDeclaration.CreateBuiltIn(ClsName, DeclaredInFileName, NeedUnit);
    FPasTypes.AddObject(ClsName, FCTypes.Objects[i]);
  end;

begin
  // system
  Add('HRESULT', 'HRESULT', '*', False, 'systemh.inc');
  // Objective-C types
  AddWithPointer('Boolean', 'Boolean', 'PBoolean', '*');
  AddWithPointer('BOOL', 'Boolean', 'PBoolean', '*');
  Add('NSInteger', 'NSInteger', '{$modeswitch objectivec1}', 'objcbase.pp');
  //Add('NSInteger*', 'PNSInteger'); // indeed it is not declared
  Add('NSUInteger', 'NSUInteger', '{$modeswitch objectivec1}', 'objcbase.pp');
  Add('Class', '_Class', '{$modeswitch objectivec1}', 'objc1.inc');
  Add('SEL', 'SEL', '{$modeswitch objectivec1}', 'objc1.inc');
  Add('IMP', 'IMP', '{$modeswitch objectivec1}', 'objcnf.inc');
  Add('IMP*', 'pIMP', '{$modeswitch objectivec1}', 'objcnf.inc');
  Add('struct objc_class', 'tobjc_class', '{$modeswitch objectivec1}', 'objcnf.inc');
  Add('struct objc_class*', 'pobjc_class', '{$modeswitch objectivec1}', 'objcnf.inc');
  //Add('NSObject', 'NSObject', '*', True, 'objcbase.pp');
  Add('Protocol', 'Protocol', '{$modeswitch objectivec1}', True, 'objcbase.pp');
  Add('id', 'id', '{$modeswitch objectivec1}', 'objc1.inc');
  // C types
  Add('void', 'void', '*'); // to distinguish procedure/function
  AddWithPointer('char', 'Char', 'PChar', '*');
  Add('char**', 'PPChar', '*');
  AddWithPointer('double', 'Double', 'PDouble', '*');
  AddWithPointer('void*', 'Pointer', 'PPointer', '*');
  AddWithPointer('float', 'Single', 'PSingle', '*');
  // ctypes.pp
  AddWithPointer('int', 'cint', 'Pcint', 'ctypes', 'ctypes.pp');
  AddWithPointer('long', 'clong', 'Pclong', 'ctypes', 'ctypes.pp');
  AddWithPointer('long double', 'clongdouble', 'Pclongdouble', 'ctypes', 'ctypes.pp');
  AddWithPointer('long int', 'clong', 'Pclong', 'ctypes', 'ctypes.pp');
  AddWithPointer('long long', 'clonglong', 'Pclonglong', 'ctypes', 'ctypes.pp');
  AddWithPointer('long long int', 'clonglong', 'Pclonglong', 'ctypes', 'ctypes.pp');
  AddWithPointer('short', 'cshort', 'Pcshort', 'ctypes', 'ctypes.pp');
  AddWithPointer('short int', 'cshort', 'Pcshort', 'ctypes', 'ctypes.pp');
  AddWithPointer('signed char', 'cschar', 'Pcschar', 'ctypes', 'ctypes.pp');
  AddWithPointer('signed int', 'csint', 'Pcsint', 'ctypes', 'ctypes.pp');
  AddWithPointer('signed long', 'cslong', 'Pcslong', 'ctypes', 'ctypes.pp');
  AddWithPointer('signed long long', 'cslonglong', 'Pcslonglong', 'ctypes', 'ctypes.pp');
  AddWithPointer('signed short', 'csshort', 'Pcsshort', 'ctypes', 'ctypes.pp');
  AddWithPointer('unsigned', 'cuint', 'Pcuint', 'ctypes', 'ctypes.pp');
  AddWithPointer('unsigned char', 'cuchar', 'Pcuchar', 'ctypes', 'ctypes.pp');
  AddWithPointer('unsigned int', 'cuint', 'Pcuint', 'ctypes', 'ctypes.pp');
  AddWithPointer('unsigned long', 'culong', 'Pculong', 'ctypes', 'ctypes.pp');
  AddWithPointer('unsigned long int', 'culong', 'Pculong', 'ctypes', 'ctypes.pp');
  AddWithPointer('unsigned long long', 'culonglong', 'Pculonglong', 'ctypes', 'ctypes.pp');
  AddWithPointer('unsigned short', 'cushort', 'Pcushort', 'ctypes', 'ctypes.pp');
  // will use gles11 instead of gles20.pas because it is not available for iphonesim (yet)
  AddWithPointer('GLboolean', 'GLboolean', 'PGLboolean', 'gles11', 'gles11.pp');
  AddWithPointer('GLfloat', 'GLfloat', 'PGLfloat', 'gles11', 'gles11.pp');
  AddWithPointer('GLint', 'GLint', 'PGLint', 'gles11', 'gles11.pp');
  AddWithPointer('GLuint', 'GLuint', 'PGLuint', 'gles11', 'gles11.pp');
  AddWithPointer('GLenum', 'GLenum', 'PGLenum', 'gles11', 'gles11.pp');
  AddWithPointer('GLsizei', 'GLsizei', 'PGLsizei', 'gles11', 'gles11.pp');
  // sockets.pp, socklen_t also is availble from MacOSXPosix
  Add('socklen_t', 'TSockLen', 'sockets', False, 'sockets.pp');
  // MacTypes.pas
  Add('Byte', 'Byte', '*', 'system.h');
  // PByte is more native for Pascal but we are in context of MacTypes!
  // so we will use BytePtr that is declared in MacTypes
  Add('Byte*', 'BytePtr', 'MacTypes', 'MacTypes.pas');
  Add('ByteCount', 'ByteCount', 'MacTypes', 'MacTypes.pas');
  Add('ConstStr255Param', 'ConstStr255Param', 'MacTypes', 'MacTypes.pas');
  Add('ConstStringPtr', 'ConstStringPtr', 'MacTypes', 'MacTypes.pas');
  Add('Fixed', 'Fixed', 'MacTypes', 'MacTypes.pas');
  AddWithPointer('Float32', 'Float32', 'Float32Ptr', 'MacTypes', 'MacTypes.pas');
  AddWithPointer('Float64', 'Float64', 'Float64Ptr', 'MacTypes', 'MacTypes.pas');
  Add('FourCharCode', 'FourCharCode', 'MacTypes', 'MacTypes.pas');
  Add('ItemCount', 'ItemCount', 'MacTypes', 'MacTypes.pas');
  Add('LangCode', 'LangCode', 'MacTypes', 'MacTypes.pas');
  Add('OSStatus', 'OSStatus', 'MacTypes', 'MacTypes.pas');
  Add('OSType', 'OSType', 'MacTypes', 'MacTypes.pas');
  Add('RegionCode', 'RegionCode', 'MacTypes', 'MacTypes.pas');
  Add('SignedByte', 'SignedByte', 'MacTypes', 'MacTypes.pas');
  Add('SInt16', 'SInt16', 'MacTypes', 'MacTypes.pas');
  AddWithPointer('SInt32', 'SInt32', 'SInt32Ptr', 'MacTypes', 'MacTypes.pas');
  AddWithPointer('SInt64', 'SInt64', 'SInt64Ptr', 'MacTypes', 'MacTypes.pas');
  Add('SInt8', 'SInt8', 'MacTypes', 'MacTypes.pas');
  Add('StringPtr', 'StringPtr', 'MacTypes', 'MacTypes.pas');
  Add('UInt16', 'UInt16', 'MacTypes', 'MacTypes.pas');
  AddWithPointer('UInt32', 'UInt32', 'UInt32Ptr', 'MacTypes', 'MacTypes.pas');
  AddWithPointer('UInt64', 'UInt64', 'UInt64Ptr', 'MacTypes', 'MacTypes.pas');
  AddWithPointer('UInt8', 'UInt8', 'UInt8Ptr', 'MacTypes', 'MacTypes.pas');
  AddWithPointer('UniChar', 'UniChar', 'UniCharPtr', 'MacTypes', 'MacTypes.pas');
  Add('UTF32Char', 'UTF32Char', 'MacTypes', 'MacTypes.pas');

  AddPredefinedObjCTypes;
end;

procedure TDeclaredTypes.AddPredefinedObjCTypes;
var
  td, ptd: TTypeDeclaration;
  pd: TProtocolDeclaration;
  cd: TClassDeclaration;
begin
  // NSZone : typedef struct _NSZone NSZone;
  AddBuiltInType('struct _NSZone', 'NSZone', 'record end', 'objcbase.pp', '{$modeswitch objectivec1}');
  td := AddBuiltInType('NSZone', 'NSZone', 'record end', 'objcbase.pp', '{$modeswitch objectivec1}');
  // PNSZone : = ^NSZone
  ptd := AddBuiltInType('NSZone*', 'PNSZone', '^NSZone', 'objcbase.pp', '{$modeswitch objectivec1}');
  ptd.DependOn(td);

  // NSObjectProtocol
  pd := AddBuiltInProtocol('NSObject');
  pd.FullDecl.Add('  { NSObject protocol }');
  // declaration from objcbase.pp:
  pd.FullDecl.Add('  NSObjectProtocol = objcprotocol external name ''NSObject''');
  pd.FullDecl.Add('    function isEqual(obj: id): boolean; message ''isEqual:'';');
  pd.AddMethodName('isEqual', 'isEqual:', TypeByPasName('Boolean')); pd.AddParamName('obj');
  pd.FullDecl.Add('    function hash: NSUInteger; message ''hash'';');
  pd.AddMethodName('hash', 'hash', TypeByPasName('NSUInteger'));
  pd.FullDecl.Add('    function superclass: pobjc_class; message ''superclass'';');
  pd.AddMethodName('superclass', 'superclass', TypeByPasName('pobjc_class'));
  pd.FullDecl.Add('    function _class: pobjc_class; message ''class'';');
  pd.AddMethodName('_class', 'class', TypeByPasName('pobjc_class'));
  pd.FullDecl.Add('    function self: id;  message ''self'';');
  pd.AddMethodName('self', 'self', TypeByPasName('id'));
  pd.FullDecl.Add('    function zone: PNSZone;  message ''zone'';');
  pd.AddMethodName('zone', 'zone', TypeByPasName('PNSZone'));
  pd.FullDecl.Add('    function performSelector(aSelector: SEL): id; message ''performSelector:'';');
  pd.AddMethodName('performSelector', 'performSelector:', TypeByPasName('id')); pd.AddParamName('aSelector');
  pd.FullDecl.Add('    function performSelector_withObject(aSelector: SEL; obj: id): id; message ''performSelector:withObject:'';');
  pd.AddMethodName('performSelector_withObject', 'performSelector:withObject:', TypeByPasName('id'));
  pd.FullDecl.Add('    function performSelector_withObject_withObject(aSelector: SEL; obj1, obj2: id): id; message ''performSelector:withObject:withObject:'';');
  pd.AddMethodName('performSelector_withObject_withObject', 'performSelector:withObject:withObject:', TypeByPasName('id')); pd.AddParamName('obj1'); pd.AddParamName('obj2');
  pd.FullDecl.Add('    function isProxy: boolean; message ''isProxy'';');
  pd.AddMethodName('isProxy', 'isProxy', TypeByPasName('Boolean'));
  pd.FullDecl.Add('    function isKindOfClass(aClass: pobjc_class): boolean; message ''isKindOfClass:'';');
  pd.AddMethodName('isKindOfClass', 'isKindOfClass:', TypeByPasName('Boolean')); pd.AddParamName('aClass');
  pd.FullDecl.Add('    function isMemberOfClass(aClass: pobjc_class): boolean; message ''isMemberOfClass:'';');
  pd.AddMethodName('isMemberOfClass', 'isMemberOfClass:', TypeByPasName('Boolean'));
  pd.FullDecl.Add('    function conformsToProtocol(aProtocol: Protocol): boolean; message ''conformsToProtocol:'';');
  pd.AddMethodName('conformsToProtocol', 'conformsToProtocol:', TypeByPasName('Boolean')); pd.AddParamName('aProtocol');
  pd.FullDecl.Add('    function respondsToSelector(aSelector: SEL): boolean; message ''respondsToSelector:'';');
  pd.AddMethodName('respondsToSelector', 'respondsToSelector:', TypeByPasName('Boolean'));
  pd.FullDecl.Add('    function retain: id; message ''retain'';');
  pd.AddMethodName('retain', 'retain', TypeByPasName('id'));
  pd.FullDecl.Add('    procedure release;  message ''release''; { oneway }');
  pd.AddMethodName('release', 'release', nil);
  pd.FullDecl.Add('    function autorelease: id; message ''autorelease'';');
  pd.AddMethodName('autorelease', 'autorelease', TypeByPasName('id'));
  pd.FullDecl.Add('    function retainCount: NSUInteger; message ''retainCount'';');
  pd.AddMethodName('retainCount', 'retainCount', TypeByPasName('NSUInteger'));
  pd.FullDecl.Add('    function description: NSString; message ''description'';');
  pd.AddMethodName('description', 'description', nil{TypeByPasName('NSString')});
  pd.FullDecl.Add('  end;');

  // NSObject
  cd := AddBuiltInClass('NSObject');
  cd.AddInheritProt(pd);
  cd.FullDecl.Add('  { NSObject }');
  // declaration from objcbase.pp:
  cd.FullDecl.Add('  NSObject = objcclass external (NSObjectProtocol)');
  cd.FullDecl.Add('  public');
  cd.FullDecl.Add('    function isEqual(obj: id): boolean; message ''isEqual:'';');
  //cd.AddMethodName('isEqual', TypeByPasName('Boolean')); cd.AddParamName('obj');
  cd.FullDecl.Add('    function hash: NSUInteger; message ''hash'';');
  //cd.AddMethodName('hash', TypeByPasName('NSUInteger'));
  cd.FullDecl.Add('    function superclass: pobjc_class; message ''superclass'';');
  //cd.AddMethodName('superclass', TypeByPasName('pobjc_class'));
  cd.FullDecl.Add('    function _class: pobjc_class; message ''class'';');
  //cd.AddMethodName('_class', TypeByPasName('pobjc_class'));
  cd.FullDecl.Add('    function self: id;  message ''self'';');
  //cd.AddMethodName('self', TypeByPasName('id'));
  cd.FullDecl.Add('    function zone: PNSZone;  message ''zone'';');
  //cd.AddMethodName('zone', TypeByPasName('PNSZone'));
  cd.FullDecl.Add('    function performSelector(aSelector: SEL): id; message ''performSelector:'';');
  //cd.AddMethodName('performSelector', TypeByPasName('id')); cd.AddParamName('aSelector');
  cd.FullDecl.Add('    function performSelector_withObject(aSelector: SEL; obj: id): id; message ''performSelector:withObject:'';');
  //cd.AddMethodName('performSelector_withObject', TypeByPasName('id'));
  cd.FullDecl.Add('    function performSelector_withObject_withObject(aSelector: SEL; obj1, obj2: id): id; message ''performSelector:withObject:withObject:'';');
  //cd.AddMethodName('performSelector_withObject_withObject', TypeByPasName('id')); cd.AddParamName('obj1'); cd.AddParamName('obj2');
  cd.FullDecl.Add('    function isProxy: boolean; message ''isProxy'';');
  //cd.AddMethodName('isProxy', TypeByPasName('boolean'));
  cd.FullDecl.Add('    function isKindOfClass(aClass: pobjc_class): boolean; message ''isKindOfClass:'';');
  //cd.AddMethodName('isKindOfClass', TypeByPasName('boolean')); cd.AddParamName('aClass');
  cd.FullDecl.Add('    function isMemberOfClass(aClass: pobjc_class): boolean; message ''isMemberOfClass:'';');
  //cd.AddMethodName('isMemberOfClass', TypeByPasName('boolean'));
  cd.FullDecl.Add('    function conformsToProtocol(aProtocol: Protocol): boolean; message ''conformsToProtocol:'';');
  //cd.AddMethodName('conformsToProtocol', TypeByPasName('boolean')); cd.AddParamName('aProtocol');
  cd.FullDecl.Add('    function respondsToSelector(aSelector: SEL): boolean; message ''respondsToSelector:'';');
  //cd.AddMethodName('respondsToSelector', TypeByPasName('boolean'));
  cd.FullDecl.Add('    function retain: id; message ''retain'';');
  //cd.AddMethodName('retain', TypeByPasName('id'));
  cd.FullDecl.Add('    procedure release;  message ''release''; { oneway }');
  //cd.AddMethodName('release', nil);
  cd.FullDecl.Add('    function autorelease: id; message ''autorelease'';');
  //cd.AddMethodName('autorelease', TypeByPasName('id'));
  cd.FullDecl.Add('    function retainCount: NSUInteger; message ''retainCount'';');
  //cd.AddMethodName('retainCount', TypeByPasName('NSUInteger'));
  cd.FullDecl.Add('    function description: NSString; message ''description'';');
  //cd.AddMethodName('description', nil{TypeByPasName('NSString')});
  cd.FullDecl.Add('    class function classIsEqual(obj: id): boolean; message ''isEqual:'';');
  cd.AddMethodName('classIsEqual', 'isEqual:', TypeByPasName('boolean'));
  cd.FullDecl.Add('    class function classHash: cuint; message ''hash'';');
  cd.AddMethodName('classHash', 'hash', TypeByPasName('cuint'));
  cd.FullDecl.Add('    class procedure load; message ''load'';');
  cd.AddMethodName('load', 'load', nil);
  cd.FullDecl.Add('    class procedure initialize; message ''initialize'';');
  cd.AddMethodName('initialize', 'initialize', nil);
  cd.FullDecl.Add('    function init: id; message ''init'';');
  cd.AddMethodName('init', 'init', TypeByPasName('id'));
  cd.FullDecl.Add('    class function new: id; message ''new'';');
  cd.AddMethodName('new', 'new', TypeByPasName('id'));
  cd.FullDecl.Add('    class function allocWithZone(_zone: PNSZone): id; message ''allocWithZone:'';');
  cd.AddMethodName('allocWithZone', 'allocWithZone:', TypeByPasName('id'));
  cd.FullDecl.Add('    class function alloc: id; message ''alloc'';');
  cd.AddMethodName('alloc', 'alloc', TypeByPasName('id'));
  cd.FullDecl.Add('    procedure dealloc; message ''dealloc'';');
  cd.AddMethodName('dealloc', 'dealloc', nil);
  cd.FullDecl.Add('    procedure finalize; message ''finalize'';');
  cd.AddMethodName('finalize', 'finalize', nil);
  cd.FullDecl.Add('    function copy: id; message ''copy'';');
  cd.AddMethodName('copy', 'copy', TypeByPasName('id'));
  cd.FullDecl.Add('    function mutableCopy: id; message ''mutableCopy'';');
  cd.AddMethodName('mutableCopy', 'mutableCopy', TypeByPasName('id'));
  cd.FullDecl.Add('    class function classCopyWithZone(_zone: NSZonePtr): id; message ''copyWithZone:'';');
  cd.AddMethodName('classCopyWithZone', 'copyWithZone:', TypeByPasName('id'));
  cd.FullDecl.Add('    class function classMutableCopyWithZone(_zone: NSZonePtr): id; message ''mutableCopyWithZone:'';');
  cd.AddMethodName('classMutableCopyWithZone', 'mutableCopyWithZone:', TypeByPasName('id'));
  cd.FullDecl.Add('    class function classSuperclass: pobjc_class; message ''superclass'';');
  cd.AddMethodName('classSuperclass', 'superclass', TypeByPasName('pobjc_class'));
  cd.FullDecl.Add('    class function classClass: pobjc_class; message ''class'';');
  cd.AddMethodName('classClass', 'class', TypeByPasName('pobjc_class'));
  cd.FullDecl.Add('    class procedure poseAsClass(aClass: pobjc_class); message ''poseAsClass:'';');
  cd.AddMethodName('poseAsClass', 'poseAsClass:', nil);
  cd.FullDecl.Add('    class function instancesRespondToSelector(aSelector: SEL): boolean; message ''instancesRespondToSelector:'';');
  cd.AddMethodName('instancesRespondToSelector', 'instancesRespondToSelector:', TypeByPasName('boolean'));
  cd.FullDecl.Add('    class function classConformsToProtocol(aProtocol: Protocol): boolean; message ''conformsToProtocol:'';');
  cd.AddMethodName('classConformsToProtocol', 'conformsToProtocol:', TypeByPasName('boolean'));
  cd.FullDecl.Add('    function methodForSelector(aSelector: SEL): IMP; message ''methodForSelector:'';');
  cd.AddMethodName('methodForSelector', 'methodForSelector:', TypeByPasName('IMP'));
  cd.FullDecl.Add('    class function instanceMethodForSelector(aSelector: SEL): IMP; message ''instanceMethodForSelector:'';');
  cd.AddMethodName('instanceMethodForSelector', 'instanceMethodForSelector:', TypeByPasName('IMP'));
  cd.FullDecl.Add('    procedure doesNotRecognizeSelector(aSelector: SEL); message ''doesNotRecognizeSelector:'';');
  cd.AddMethodName('doesNotRecognizeSelector', 'doesNotRecognizeSelector:', nil);
  cd.FullDecl.Add('    procedure forwardInvocation(anInvocation: NSInvocation); message ''forwardInvocation:'';');
  cd.AddMethodName('forwardInvocation', 'forwardInvocation:', nil{TypeByPasName('NSInvocation')});
  cd.FullDecl.Add('    function methodSignatureForSelector(aSelector: SEL): NSMethodSignature; message ''methodSignatureForSelector:'';');
  cd.AddMethodName('methodSignatureForSelector', 'methodSignatureForSelector:', nil{TypeByPasName('NSMethodSignature')});
  cd.FullDecl.Add('    class function _classDescription: NSString; message ''description'';');
  cd.AddMethodName('_classDescription', 'description', nil{TypeByPasName('NSString')});
  cd.FullDecl.Add('  end;');
end;

function TDeclaredTypes.GetTypeByPasName(p_name: string): TTypeDeclaration;
var i: Integer;
begin
  i := FPasTypes.IndexOf(p_name);
  if i < 0 then Exit(nil);
  Result := TTypeDeclaration(FPasTypes.Objects[i]);
end;

function TDeclaredTypes.TypeByPasName(p_name: string): TTypeDeclaration;
var i: Integer;
begin
  i := FPasTypes.IndexOf(p_name);
  if i < 0 then
    raise Exception.CreateFmt('PasType "%s" not found!', [p_name]);
  Result := TTypeDeclaration(FPasTypes.Objects[i]);
end;

function TDeclaredTypes.GetTypeByCName(c_name: string): TTypeDeclaration;
var i: Integer;
begin
  i := FCTypes.IndexOf(c_name);
  if i < 0 then Exit(nil);
  Result := TTypeDeclaration(FCTypes.Objects[i]);
  if Result.FIsVoid then
    Result := GetTypeByCName('void');
end;

function TDeclaredTypes.GetPointerTypeFor(td: TTypeDeclaration): TTypeDeclaration;
begin
  Result := GetTypeByCName(td.FOrigTypeName + '*');
  if not Assigned(Result) then
    raise Exception.Create('Pointer type for "' + td.FOrigTypeName + '" not found');
end;

function TDeclaredTypes.GetProtocol(protocolName: string): TProtocolDeclaration;
var i: Integer;
begin
  i := FCTypes.IndexOf('protocol ' + protocolName);
  if i < 0 then Exit(nil);
  Result := FCTypes.Objects[i] as TProtocolDeclaration;
end;

function TDeclaredTypes.GetClass(clsName: string): TClassDeclaration;
var
  i: Integer;
begin
  i := FCTypes.IndexOf(clsName);
  if i < 0 then Exit(nil);
  Result := FCTypes.Objects[i] as TClassDeclaration;
end;

function TDeclaredTypes.GenNewName(const Prefix: string;
  Postfix: string): string;
var i: Integer;
begin
  Result := Prefix + Postfix;
  i := 0;
  repeat
    if FCTypes.IndexOf(Result) < 0 then Exit;
    Inc(i);
    Result := Prefix + IntToStr(i) + Postfix;
  until i < 0;
  raise Exception.Create('cannot gen new name');
end;

procedure TDeclaredTypes.SetPackRecords(directive: string);
begin
  FPackRecords := directive;
end;

initialization
  FPC_Reserved := TStringList.Create;
  FPC_Reserved.Sorted := True;
  with FPC_Reserved do
  begin
    FPC_Reserved.Add('result');
    // Turbo Pascal reserved words
    FPC_Reserved.Add('absolute');
    FPC_Reserved.Add('and');
    FPC_Reserved.Add('array');
    FPC_Reserved.Add('asm');
    FPC_Reserved.Add('begin');
    FPC_Reserved.Add('case');
    FPC_Reserved.Add('const');
    FPC_Reserved.Add('constructor');
    FPC_Reserved.Add('destructor');
    FPC_Reserved.Add('div');
    FPC_Reserved.Add('do');
    FPC_Reserved.Add('downto');
    FPC_Reserved.Add('else');
    FPC_Reserved.Add('end');
    FPC_Reserved.Add('file');
    FPC_Reserved.Add('for');
    FPC_Reserved.Add('function');
    FPC_Reserved.Add('goto');
    FPC_Reserved.Add('if');
    FPC_Reserved.Add('implementation');
    FPC_Reserved.Add('in');
    FPC_Reserved.Add('inherited');
    FPC_Reserved.Add('inline');
    FPC_Reserved.Add('interface');
    FPC_Reserved.Add('label');
    FPC_Reserved.Add('mod');
    FPC_Reserved.Add('nil');
    FPC_Reserved.Add('not');
    FPC_Reserved.Add('object');
    FPC_Reserved.Add('of');
    FPC_Reserved.Add('on');
    FPC_Reserved.Add('operator');
    FPC_Reserved.Add('or');
    FPC_Reserved.Add('packed');
    FPC_Reserved.Add('procedure');
    FPC_Reserved.Add('program');
    FPC_Reserved.Add('record');
    FPC_Reserved.Add('reintroduce');
    FPC_Reserved.Add('repeat');
    FPC_Reserved.Add('self');
    FPC_Reserved.Add('set');
    FPC_Reserved.Add('shl');
    FPC_Reserved.Add('shr');
    FPC_Reserved.Add('string');
    FPC_Reserved.Add('then');
    FPC_Reserved.Add('to');
    FPC_Reserved.Add('type');
    FPC_Reserved.Add('unit');
    FPC_Reserved.Add('until');
    FPC_Reserved.Add('uses');
    FPC_Reserved.Add('var');
    FPC_Reserved.Add('while');
    FPC_Reserved.Add('with');
    FPC_Reserved.Add('xor');
// Free Pascal reserved words
    FPC_Reserved.Add('dispose');
    FPC_Reserved.Add('exit');
    FPC_Reserved.Add('false');
    FPC_Reserved.Add('new');
    FPC_Reserved.Add('true');
// Object Pascal reserved words
    FPC_Reserved.Add('as');
    FPC_Reserved.Add('class');
    FPC_Reserved.Add('dispinterface');
    FPC_Reserved.Add('except');
    FPC_Reserved.Add('exports');
    FPC_Reserved.Add('finalization');
    FPC_Reserved.Add('finally');
    FPC_Reserved.Add('initialization');
    FPC_Reserved.Add('inline');
    FPC_Reserved.Add('is');
    FPC_Reserved.Add('library');
    FPC_Reserved.Add('on');
    FPC_Reserved.Add('out');
    FPC_Reserved.Add('packed');
    FPC_Reserved.Add('property');
    FPC_Reserved.Add('raise');
    FPC_Reserved.Add('resourcestring');
    FPC_Reserved.Add('threadvar');
    FPC_Reserved.Add('try');
  end;

finalization
  FPC_Reserved.Free;
end.

