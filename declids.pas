unit DeclIds;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DeclTypes;

type

  { TDeclId }

  TDeclId = class
  private
    FPasName: string;
    FNeedUnit: string;
    FReturnType: TTypeDeclaration;
    FFixed: Boolean;
  public
    constructor Create(PasName: string; RetType: TTypeDeclaration; ANeedUnit: string = '*');
    property NeedUnit: string read FNeedUnit;
    property PasName: string read FPasName;
    property ReturnType: TTypeDeclaration read FReturnType;
    property Fixed: Boolean read FFixed;
  end;

  { TGlobDeclIds }

  TGlobDeclIds = class // global functions
  private
    FDeclIds,
    FDeclPasIds: TStringList;
    FLocalDeclIds: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(OrigName, PasName: string; RetType: TTypeDeclaration;
      ANeedUnit: string = '*');
    procedure Delete(OrigName: string);
    procedure AddLocal(OrigName, PasName: string; VarType: TTypeDeclaration);
    procedure ClearLocals;
    function FindGlobalId(OrigName: string): TDeclId;
    function GetPasName(OrigName: string; out PasName: string): Boolean;
    function IsDeclared(PasName: string): Boolean;
    function IsDeclaredLocal(OrigName: string): Boolean;
    function LocalPasName(OrigName: string): string;
    function ReturnType(OrigName: string): TTypeDeclaration;
    function GetType(OrigName: string): TTypeDeclaration;
    function VarType(OrigName: string): TTypeDeclaration;
    procedure DeleteNew;
    procedure CommitNew;
  end;

implementation

{ TGlobDeclIds }

constructor TGlobDeclIds.Create;
begin
  FDeclIds := TStringList.Create;
  FDeclIds.CaseSensitive := True;
  FDeclIds.Sorted := True;
  FDeclPasIds := TStringList.Create;
  FDeclPasIds.CaseSensitive := False;
  FDeclPasIds.Sorted := True;
  FLocalDeclIds := TStringList.Create;
  FLocalDeclIds.CaseSensitive := True;
  FLocalDeclIds.Sorted := True;
end;

destructor TGlobDeclIds.Destroy;
var
  i: Integer;
begin
  FDeclPasIds.Free;
  ClearLocals;
  FLocalDeclIds.Free;
  for i := 0 to FDeclIds.Count - 1 do
    FDeclIds.Objects[i].Free;
  FDeclIds.Free;
  inherited Destroy;
end;

procedure TGlobDeclIds.Add(OrigName, PasName: string;
  RetType: TTypeDeclaration; ANeedUnit: string);
var
  i: Integer;
  td: TTypeDeclaration;
begin
  i := FDeclIds.IndexOf(OrigName);
  if i < 0 then
  begin
    i := FDeclIds.Add(OrigName);
    FDeclIds.Objects[i] := TDeclId.Create(PasName, RetType, ANeedUnit);
    FDeclPasIds.Add(PasName);
  end else begin
    // already declared
    with TDeclId(FDeclIds.Objects[i]) do
      if not Fixed then
      begin
        td := FReturnType;
        if td.ResolveAlias <> RetType.ResolveAlias then
          raise Exception.CreateFmt('"%s" redeclared with different return type', [OrigName]);
      end;
  end;
end;

procedure TGlobDeclIds.Delete(OrigName: string);
var i, j: Integer;
begin
  i := FDeclIds.IndexOf(OrigName);
  if i >= 0 then
  begin
    if not TDeclId(FDeclIds.Objects[i]).Fixed then
    begin
      j := FDeclPasIds.IndexOf(TDeclId(FDeclIds.Objects[i]).PasName);
      FDeclIds.Objects[i].Free;
      FDeclIds.Delete(i);
      if j >= 0 then FDeclPasIds.Delete(j);
    end;
  end;
end;

procedure TGlobDeclIds.AddLocal(OrigName, PasName: string;
  VarType: TTypeDeclaration);
begin
  FLocalDeclIds.AddObject(OrigName, TDeclId.Create(PasName, VarType));
  if PasName <> OrigName then
    FLocalDeclIds.AddObject(PasName, TDeclId.Create(PasName, VarType));
end;

procedure TGlobDeclIds.ClearLocals;
var i: Integer;
begin
  for i := 0 to FLocalDeclIds.Count - 1 do
    FLocalDeclIds.Objects[i].Free;
  FLocalDeclIds.Clear;
end;

function TGlobDeclIds.FindGlobalId(OrigName: string): TDeclId;
var i: Integer;
begin
  i := FDeclIds.IndexOf(OrigName);
  if i >= 0 then
    Result := TDeclId(FDeclIds.Objects[i])
  else
    Result := nil;
end;

function TGlobDeclIds.GetPasName(OrigName: string; out PasName: string): Boolean;
var i: Integer;
begin
  i := FLocalDeclIds.IndexOf(OrigName);
  if i >= 0 then
  begin
    PasName := TDeclId(FLocalDeclIds.Objects[i]).PasName;
    Result := True;
  end else begin
    i := FDeclIds.IndexOf(OrigName);
    Result := i >= 0;
    if Result then
      PasName := TDeclId(FDeclIds.Objects[i]).PasName;
  end;
end;

function TGlobDeclIds.IsDeclared(PasName: string): Boolean;
var i: Integer;
begin
  Result := True;
  for i := 0 to FLocalDeclIds.Count - 1 do
    if TDeclId(FLocalDeclIds.Objects[i]).PasName = PasName then Exit;
  Result := FDeclPasIds.IndexOf(PasName) >= 0;
end;

function TGlobDeclIds.IsDeclaredLocal(OrigName: string): Boolean;
begin
  Result := FLocalDeclIds.IndexOf(OrigName) >= 0;
end;

function TGlobDeclIds.LocalPasName(OrigName: string): string;
var i: Integer;
begin
  i := FLocalDeclIds.IndexOf(OrigName);
  if i < 0 then
    raise Exception.Create('undeclared local var: ' + OrigName);
  Result := TDeclId(FLocalDeclIds.Objects[i]).PasName;
end;

function TGlobDeclIds.ReturnType(OrigName: string): TTypeDeclaration;
var i: Integer;
begin
  i := FDeclIds.IndexOf(OrigName);
  if i < 0 then
    Result := nil
  else
    Result := TDeclId(FDeclIds.Objects[i]).FReturnType;
end;

function TGlobDeclIds.GetType(OrigName: string): TTypeDeclaration;
begin
  Result := VarType(OrigName);
  if not Assigned(Result) then
    Result := ReturnType(OrigName);
end;

function TGlobDeclIds.VarType(OrigName: string): TTypeDeclaration;
var
  i: Integer;
begin
  i := FLocalDeclIds.IndexOf(OrigName);
  if i < 0 then
    Result := nil
  else
    Result := TDeclId(FLocalDeclIds.Objects[i]).ReturnType;
end;

procedure TGlobDeclIds.DeleteNew;
var i, j: Integer; d: TDeclId;
begin
  for i := FDeclIds.Count - 1 downto 0 do
  begin
    d := TDeclId(FDeclIds.Objects[i]);
    if not d.FFixed then
    begin
      j := FDeclPasIds.IndexOf(d.PasName);
      d.Free;
      FDeclIds.Delete(i);
      if j >= 0 then FDeclPasIds.Delete(j);
    end;
  end;
end;

procedure TGlobDeclIds.CommitNew;
var i: Integer;
begin
  for i := 0 to FDeclIds.Count - 1 do
    TDeclId(FDeclIds.Objects[i]).FFixed := True;
end;

{ TDeclId }

constructor TDeclId.Create(PasName: string; RetType: TTypeDeclaration;
  ANeedUnit: string);
begin
  FPasName := PasName;
  FReturnType := RetType;
  FNeedUnit := ANeedUnit;
  FFixed := ANeedUnit = '*';
end;

end.

