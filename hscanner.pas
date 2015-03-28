unit hscanner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TLexType = (ltSymbol, ltNumber, ltDelim, ltString);

  { TLex }

  TLex = class
  private
    rawData: string;
    FLexType: TLexType;
  public
    constructor Create(buf: string; typ: TLexType);
    function eq(comp: string): Boolean; inline;
    function eq(comp: TLex): Boolean; inline;
    function AsString: string; inline;
    function AsPasString: string; inline;
    property LexType: TLexType read FLexType;
  end;

  { TDefineMacro }

  TDefineMacro = class
  private
    Params: array of string;
    Substitute: string;
    FDefinedInFile: string;
    FFramework: string;
  public
    constructor Create(Framework, FileName: string);
    function Enabled(CurFrameworkUnit: string; RefFrameworks: TStrings): Boolean;
    function Copy: TDefineMacro;
    property Framework: string read FFramework;
    property FileName: string read FDefinedInFile;
  end;

  TCommentReadEvent = procedure (Sender: TObject; AComment: string) of object;
  TFrameworkReferenceEvent = procedure (Sender: TObject; AFramework: string; var Cancel: Boolean) of object;
  TPragmaEvent = procedure (Sender: TObject; directive: string) of object;
  TDefinedConstantEvent = procedure (Sender: TObject; ConstName, ConstValue: string) of object;

  { THeaderScanner }

  THeaderScanner = class
  private
    FComment: string;
    FLastAttribute: string;
    FOnNestedFramework: TFrameworkReferenceEvent;
    FStream: TStream; // current working stream
    FStreams: TFPList;
    FLine, FChar: Integer;
    FLexems: TStringList;
    FCurFramework, FCurFrameworkUnit: string;
    FCurFilename: string;
    FIncludeEnabled: Boolean; // 'false' by default
    FCurHeaderIsSystem,
    FCurHeaderBelongsToFramework: Boolean;
    prev_cc, cc: Char;
    FHeadersToSkip,
    FDefines, FImported: TStringList;
    FOnCommentRead: TCommentReadEvent;
    FOnFrameworkReference: TFrameworkReferenceEvent;
    FOnDefinedConstant: TDefinedConstantEvent;
    FOnPragma: TPragmaEvent;
    FRefFrameworks: TStringList;
    function next_char: Char; {inline;}
    function CheckComment: Integer;
    procedure ApplyMacro(dm: TDefineMacro);
    function IsMacro(buf: PChar; len: Integer): Boolean;
    procedure HandleAttribute;
    function MakeLex(buf: PChar; len: Integer; typ: TLexType): TLex;
    procedure NewLine;
    procedure ParseMacro(dname: string; dm: TDefineMacro);
    procedure ParseNewFile(const incFile: string; sys, bcf: Boolean);
    procedure ParseNewString(const newBuf: string);

    procedure PreprocessorDirective;
    procedure ProcessDefineDirective;
    procedure ProcessIfDefDirective;
    procedure ProcessIfDirective;
    procedure ProcessIfNdefDirective;
    procedure ProcessIncludeDirective(import: Boolean);
    procedure ProcessPragmaDirective;
    procedure ProcessAfterIfParse(CondVal: Boolean);
    procedure ProcessUndefDirective;
    procedure SkipPreprocessorDirective;
  protected
    procedure DoOnCommentRead;
    procedure DoFrameworkReference(fr: string);
    function DoNestedFrameworkReference(fr: string): Boolean;
    procedure DoDefinedConst(ConstName, ConstValue: string);
    procedure DoPragma(directive: string);
  public
    constructor Create(CurFramework: string; AStream: TStream);
    destructor Destroy; override;
    // like in C but without leading "#define"
    procedure AddDefine(def: string);
    function GetNextLex(handleMacros: Boolean = True): TLex;
    procedure RetrieveDefines(dest: TStrings);
    procedure RetrieveImported(dest: TStrings);
    procedure InitDefines(src: TStrings);
    procedure InitImported(src: TStrings);
    procedure StoreDefines(src: TStrings);

    property CurFileName: string read FCurFilename;
    property CurFrameworkUnit: string read FCurFrameworkUnit;
    property IncludeEnabled: Boolean
      read FIncludeEnabled write FIncludeEnabled default False;
    property CurHeaderIsSystem: Boolean read FCurHeaderIsSystem;
    property CurHeaderBelongsToFramework: Boolean
      read FCurHeaderBelongsToFramework;
    property OnCommentRead: TCommentReadEvent
      read FOnCommentRead write FOnCommentRead;
    property OnFrameworkReference: TFrameworkReferenceEvent
      read FOnFrameworkReference write FOnFrameworkReference;
    property OnDefinedConstant: TDefinedConstantEvent
      read FOnDefinedConstant write FOnDefinedConstant;
    property OnPragmaPack: TPragmaEvent read FOnPragma write FOnPragma;
    property OnNestedFramework: TFrameworkReferenceEvent read FOnNestedFramework write FOnNestedFramework;
    property LastAttribute: string read FLastAttribute;
  end;

implementation

uses bufstream, strutils;

const
  Alpha = ['a'..'z', 'A'..'Z', '_'];
  Digits = ['0'..'9'];
  AlphaDig = Alpha + Digits;

type

  { TStreamsStackElement }

  TStreamsStackElement = class
  public
    cc: Char;
    Stream: TStream;
    FileName: string;
    HeaderIsSystem,
    HeaderBelongsToFramework: Boolean;
    constructor Create(Scanner: THeaderScanner);
  end;

{ TDefineMacro }

constructor TDefineMacro.Create(Framework, FileName: string);
begin
  SetLength(Params, 0);
  Substitute := '';
  FFramework := Framework;
  FDefinedInFile := FileName;
end;

function TDefineMacro.Enabled(CurFrameworkUnit: string;
  RefFrameworks: TStrings): Boolean;
begin
  Result := true or (FFramework = CurFrameworkUnit)
    or (FDefinedInFile = '*')
    or (RefFrameworks.IndexOf(FFramework) > 0) {and (Pos('include', FDefinedInFile) = 0)};
end;

function TDefineMacro.Copy: TDefineMacro;
var
  i: Integer;
begin
  Result := TDefineMacro.Create(FFramework, FDefinedInFile);
  SetLength(Result.Params, Length(Self.Params));
  for i := 0 to High(Self.Params) do
    Result.Params[i] := self.Params[i];
  Result.Substitute := Self.Substitute;
end;

{ TStreamsStackElement }

constructor TStreamsStackElement.Create(Scanner: THeaderScanner);
begin
  cc := Scanner.cc;
  Stream := Scanner.FStream;
  FileName := Scanner.FCurFilename;
  HeaderIsSystem := Scanner.CurHeaderIsSystem;
  HeaderBelongsToFramework := Scanner.CurHeaderBelongsToFramework;
end;

{ TLex }

constructor TLex.Create(buf: string; typ: TLexType);
begin
  rawData := buf;
  FLexType := typ;
end;

function TLex.eq(comp: string): Boolean;
begin
  Result := rawData = comp;
end;

function TLex.eq(comp: TLex): Boolean;
begin
  Result := rawData = comp.rawData;
end;

function TLex.AsString: string;
begin
  Result := rawData;
end;

function TLex.AsPasString: string;
begin
  if FLexType <> ltString then
    raise Exception.Create('LexType is not ltString!');
  Result := StringReplace(AsString, '''', '''''', [rfReplaceAll]);
  Result[1] := '''';
  Result[Length(Result)] := '''';
  { ESC seq: }
  Result := StringsReplace(Result,
    ['\"', '\n',      '\r',      '\\'],
    ['"',  '''#10''', '''#13''', '\' ],
    [rfReplaceAll]);
end;

{ THeaderScanner }

procedure THeaderScanner.DoOnCommentRead;
begin
  if Assigned(FOnCommentRead) then
    FOnCommentRead(Self, FComment);
end;

procedure THeaderScanner.DoFrameworkReference(fr: string);
var
  needCancel: Boolean;
begin
  if Assigned(FOnFrameworkReference) then
  begin
    needCancel := False;
    FOnFrameworkReference(Self, fr, needCancel);
    if needCancel then
    begin
      WriteLn('  depends on "', fr, '"');
      Abort;
    end else
      if FRefFrameworks.IndexOf(fr) < 0 then
        FRefFrameworks.Add(fr);
  end;
end;

function THeaderScanner.DoNestedFrameworkReference(fr: string): Boolean;
begin
  Result := False;
  if Assigned(FOnNestedFramework) then
    FOnNestedFramework(Self, fr, Result);
  Result := not Result;
end;

procedure THeaderScanner.DoDefinedConst(ConstName, ConstValue: string);
begin
  if Assigned(FOnDefinedConstant) then
    FOnDefinedConstant(Self, ConstName, ConstValue);
end;

procedure THeaderScanner.DoPragma(directive: string);
begin
  if Assigned(FOnPragma) then
    FOnPragma(Self, directive);
end;

function THeaderScanner.next_char: Char;
var sse: TStreamsStackElement;
begin
  prev_cc := cc;
  if not Assigned(FStream) or (FStream.Read(cc, 1) < 1) then
  begin
    FreeAndNil(FStream);
    if FStreams.Count > 0 then
    begin
      sse := TStreamsStackElement(FStreams[0]);
      FStreams.Delete(0);
      FStream := sse.Stream;
      cc := sse.cc;
      FCurFilename := sse.FileName;
      FCurHeaderIsSystem := sse.HeaderIsSystem;
      FCurHeaderBelongsToFramework := sse.HeaderBelongsToFramework;
      sse.Free;
    end else cc := #0
  end else Inc(FChar);
  Result := cc;
end;

function THeaderScanner.CheckComment: Integer;
begin
  if cc = '/' then
  begin
    // one-line comment
    Result := 1;
    while not (cc in [#0,#10,#13]) do next_char;
    if cc = #13 then next_char;
    if cc = #10 then next_char;
  end else
  if cc = '*' then
  begin
    // this multi-line comment must be of the one line!
    Result := 2;
    next_char;
    repeat
      if cc in [#10, #13] then
        raise Exception.Create('Multi-line comment of several lines is not expected here (in preprocessor directive)!')
      else
      if cc = '*' then
      begin
        next_char;
        if cc = '/' then
        begin
          next_char;
          Break;
        end;
      end else
        next_char;
    until cc = #0;
  end else
    Result := 0;
end;

procedure THeaderScanner.ApplyMacro(dm: TDefineMacro);
var
  newBuf, aparam: string;
  i, k, pl: Integer;
begin
  newBuf := dm.Substitute;
  if Length(dm.Params) > 0 then
  begin
    GetNextLex; // "("
    for i := 0 to High(dm.Params) do
    begin
      aparam := '';
      k := 0;
      repeat
        if cc = '(' then Inc(k) else
        if cc = ')' then Dec(k);
        aparam := aparam + cc;
        next_char;
      until (k = 0) and (cc in [',', ')']);
      next_char;
      k := 1;
      pl := Length(dm.Params[i]);
      while k <= Length(newBuf) - pl + 1 do
      begin
        if (Copy(newBuf, k, pl) = dm.Params[i])
        and ((k >= 1) or not (newBuf[k - 1] in Alpha))
        and ((k + pl - 1 = Length(newBuf)) or not (newBuf[k + pl] in AlphaDig)) then
        begin
          newBuf := Copy(newBuf, 1, k - 1) + aparam + Copy(newBuf, k + Length(dm.Params[i]), MaxInt);
          k := k + Length(aparam);
        end else
          Inc(k);
      end
    end;
    i := Pos('##', newBuf) - 1;
    while i >= 0 do
    begin
      k := i + 3; // for the future cause "i" value will be changed
      while (i > 0) and (newBuf[i] in [#9, #10, #13, ' ']) do Dec(i);
      pl := Length(newBuf);
      while (k <= pl) and (newBuf[k] in [#9, #10, #13, ' ']) do Inc(k);
      Delete(newBuf, i + 1, k - i - 1);
      i := Pos('##', newBuf) - 1;
    end;
    if Pos('#', newBuf) > 0 then
    begin
      { handling of single '#' }
      i := Pos('#', newBuf);
      while i > 0 do
      begin
        newBuf[i] := '"';
        { HERE IS A BUG : '# close' => '" close"' }
        { concat with prior string constant }
        for k := i - 1 downto 1 do
          if newBuf[k] in [' ', #9] then Continue else
          if newBuf[k] = '"' then
          begin
            Delete(newBuf, k, i - k + 1);
            i := k;
            Break;
          end else Break;
        k := i + 1;
        repeat
          if k > Length(newBuf) then
          begin
            newBuf := newBuf + '"';
            Break;
          end else
          if newBuf[k] in [' ', #9] then
          begin
            Insert('"', newBuf, k);
            { concat with follow string constant }
            for i := k + 1 to Length(newBuf) do
              if newBuf[i] in [' ', #9] then Continue else
              if newBuf[i] = '"' then
              begin
                Delete(newBuf, k, i - k + 1);
                Break;
              end else Break;
            Break;
          end else
            Inc(k);
        until False;
        i := Pos('#', newBuf);
      end;
    end;
  end;
  ParseNewString(newBuf);
end;

function THeaderScanner.IsMacro(buf: PChar; len: Integer): Boolean;
var
  s: string; i: Integer;
  dm: TDefineMacro;
begin
  Result := False;
  SetLength(s, len);
  Move(buf^, s[1], len);
  if s = '__attribute__' then
  begin
    Result := True;
    HandleAttribute;
    Exit;
  end;
  i := FDefines.IndexOf(s);
  if i >= 0 then
  begin
    dm := TDefineMacro(FDefines.Objects[i]);
    if Length(dm.Params) > 0 then // sys/types.h contains macro major(x) and minor(x)
    begin                         // OpenGLES/EAGL.h contains function EAGLGetVersion(major,minor)
      while cc in [' ', #9] do next_char;
      Result := cc = '(';
    end else
      Result := True;
    if Result then
      ApplyMacro(dm);
  end;
end;

procedure THeaderScanner.HandleAttribute;
var
  l: TLex;
  k: Integer;
begin
  FLastAttribute := '';
  l := GetNextLex(False);
  if not l.eq('(') then
    raise Exception.Create('"(" expected after "__attribute__"');
  k := 1;
  while k > 0 do
  begin
    l := GetNextLex(False);
    if l.eq('(') then Inc(k) else
    if l.eq(')') then Dec(k);
    if k > 0 then
      FLastAttribute := FLastAttribute + l.rawData;
  end;
  FLastAttribute := Trim(FLastAttribute);
  while (FLastAttribute <> '') and (FLastAttribute[1] = '(')
  and (FLastAttribute[Length(FLastAttribute)] = ')') do
    FLastAttribute := Copy(FLastAttribute, 2, Length(FLastAttribute) - 2);
  {$ifdef verbose}
  WriteLn('attribute: "', FLastAttribute, '"');
  {$endif}
end;

function THeaderScanner.MakeLex(buf: PChar; len: Integer; typ: TLexType): TLex;
var
  i: Integer;
  s: string;
begin
  SetLength(s, len);
  Move(buf^, s[1], len);
  i := FLexems.IndexOf(s);
  if (i < 0) or (TLex(FLexems.Objects[i]).LexType <> typ) then
    i := FLexems.AddObject(s, TLex.Create(s, typ));
  Result := TLex(FLexems.Objects[i]);
end;

procedure THeaderScanner.NewLine;
begin
  Inc(FLine);
  FChar := 0;
  if cc = #13 then
  begin
    next_char;
    if cc = #10 then next_char; // Win-like line end
  end else next_char;
end;

procedure THeaderScanner.ParseMacro(dname: string; dm: TDefineMacro);
type
  TState = (sDef, sCom, sComLine, sComMLine);

  function ReadBody: string;
  var
    curState: TState;
    buf: array [0..1023] of Char;
    bufLen: Integer;
  begin
    bufLen := 0;
    Result := '';
    curState := sDef;
    while (cc <> #0) and (not (cc in [#0, #10, #13]) or (curState <> sDef)) do
    begin
      if bufLen > 1000 then
      begin
        buf[bufLen] := #0;
        Result := Result + PChar(@buf[0]);
        bufLen := 0;
      end;
      case curState of
      sDef:
        if cc = '/' then
          curState := sCom
        else
        if cc = '\' then
        begin
          next_char;
          case cc of
          #13:
            begin
              buf[bufLen] := ' ';
              Inc(bufLen);
              next_char;
              if cc <> #10 then Continue;
            end;
          #10:
            begin
              buf[bufLen] := ' ';
              Inc(bufLen);
            end;
          else
            buf[bufLen] := '\';
            Inc(bufLen);
            Continue;
          end;
        end else begin
          buf[bufLen] := cc;
          Inc(bufLen);
        end;
      sCom:
        case cc of
        '*': curState := sComMLine;
        '/': curState := sComLine;
        else
          curState := sDef;
          buf[bufLen] := '/';
          Inc(bufLen);
          buf[bufLen] := cc;
          Inc(bufLen);
        end;
      sComLine:
        case cc of
        #10: curState := sDef;
        #13:
          begin
            curState := sDef;
            next_char;
            if cc <> #10 then Continue;
          end;
        end;
      sComMLine:
        if cc = '*' then
        begin
          next_char;
          if cc = '/' then
            curState := sDef
          else
            Continue;
        end;
      end;
      next_char;
    end;
    next_char;
    if bufLen > 0 then
    begin
      buf[bufLen] := #0;
      Result := Result + PChar(@buf[0]);
    end;
    Result := TrimLeft(Result);
  end;

var
  l: TLex;
  i: Integer;
  fs: TFormatSettings;
  s: string;
  x: Double;
begin
  if cc = '(' then
  begin
    { parametrized macro }
    next_char;
    l := GetNextLex(False);
    while Assigned(l) and (l.rawData = '\') and (cc in [#10,#13]) do
      l := GetNextLex(False);
    while Assigned(l) and (l.rawData <> ')') do
    begin
      if (l.LexType = ltSymbol) or l.eq('...') then
      begin
        i := Length(dm.Params);
        SetLength(dm.Params, i + 1);
        dm.Params[i] := l.rawData;
        l := GetNextLex;
        if not Assigned(l) then Break;
        while Assigned(l) and (l.rawData = '\') and (cc in [#10,#13]) do
          l := GetNextLex;
        if l.rawData = ',' then
          l := GetNextLex;
      end;
    end;
    while cc in [#9, ' '] do next_char;
    dm.Substitute := ReadBody;
    Exit;
  end;
  if not (cc in [#10, #13]) then next_char;
  dm.Substitute := ReadBody;

  if (dm.Substitute <> '') and not CurHeaderIsSystem then
  begin
    s := Trim(dm.Substitute);
    if upcase(s[Length(s)]) = 'L' then
    begin
      s := Copy(s, 1, Length(s) - 1);
      if upcase(s[Length(s)]) = 'L' then
        s := Copy(s, 1, Length(s) - 1);
      if upcase(s[Length(s)]) = 'U' then
        s := Copy(s, 1, Length(s) - 1);
    end;
    if dname[1] = '_' then Exit;
    if dname[Length(dname)] = '_' then Exit;
    if RightStr(dname, 2) = '_H' then Exit;
    if (Length(s) >= 2) and (s[1] = '0') and (UpCase(s[2]) = 'X') then
    begin
      Delete(s, 1, 1);
      s[1] := '$';
      DoDefinedConst(dname, s);
    end else begin
      fs := DefaultFormatSettings;
      fs.DecimalSeparator := '.';
      if TryStrToFloat(s, x, fs) then
        DoDefinedConst(dname, s);
    end;
  end;
end;

procedure THeaderScanner.ParseNewFile(const incFile: string; sys, bcf: Boolean);
var
  se: TStreamsStackElement;
begin
  WriteLn('  ', {$ifndef VERBOSE}ExtractFileName{$endif}(incFile));
  se := TStreamsStackElement.Create(Self);
  FStreams.Insert(0, se);
  FStream := TReadBufStream.Create(TFileStream.Create(incFile, fmOpenRead or fmShareDenyWrite));
  TReadBufStream(FStream).SourceOwner := True;
  FCurFilename := incFile;
  FCurHeaderIsSystem := sys;
  FCurHeaderBelongsToFramework := bcf;
  cc := #0;
  next_char;
end;

procedure THeaderScanner.ParseNewString(const newBuf: string);
var
  se: TStreamsStackElement;
begin
  se := TStreamsStackElement.Create(Self);
  FStreams.Insert(0, se);
  FStream := TStringStream.Create(newBuf);
  prev_cc := #0;
  next_char;
end;

procedure THeaderScanner.PreprocessorDirective;
var l: TLex; str: string;
begin
  next_char;
  l := GetNextLex; // same line
  // #define #if #ifdef #ifndef #undef #include #import #error #include_next
  if l.eq('define') then
    ProcessDefineDirective
  else
  if l.eq('if') then
    ProcessIfDirective
  else
  if l.eq('ifdef') then
    ProcessIfDefDirective
  else
  if l.eq('ifndef') then
    ProcessIfNdefDirective
  else
  if l.eq('undef') then
    ProcessUndefDirective
  else
  if l.eq('include') then
    ProcessIncludeDirective(False)
  else
  if l.eq('import') then
    ProcessIncludeDirective(True)
  else
  if l.eq('pragma') then
    ProcessPragmaDirective
  else
  if l.eq('error') then
  begin
    next_char; // eat delim char
    str := '';
    while not (cc in [#0,#10,#13]) do
    begin
      str := str + cc;
      next_char;
    end;
    raise Exception.Create('#error(' + ExtractFileName(FCurFilename) + '): ' + str);
  end else
  if l.eq('include_next') then
    SkipPreprocessorDirective
  else begin
    if l.eq('warning') then
    begin
      str := '';
      while not (cc in [#0,#10,#13]) do
      begin
        str := str + cc;
        next_char;
      end;
      raise Exception.CreateFmt('%s: warning %s', [FCurFilename, str]);
    end else
      raise Exception.Create('unrecognized directive #' + l.AsString);
  end;
end;

procedure THeaderScanner.ProcessDefineDirective;

  function IsIntSuffix(s: string): Boolean;
  var i: Integer; chs: set of Char;
  begin
    chs := ['u', 'U', 'l', 'L'];
    for i := 1 to Length(s) do
    begin
      if not (s[i] in chs) then Exit(False);
      if s[i] in ['u', 'U'] then chs := chs - ['u', 'U'];
    end;
    Result := True;
  end;

  function SameSubstitute(dm1, dm2: TDefineMacro): Boolean;
  var s1, s2: string; i: Integer;
  begin
    s1 := Trim(dm1.Substitute);
    s2 := Trim(dm2.Substitute);
    if s1 = s2 then Exit(True);
    i := 1;
    while (i <= Length(s1)) and (i <= Length(s2)) and (s1[i] = s2[i]) do Inc(i);
    Dec(i);
    Delete(s1, 1, i);
    Delete(s2, 1, i);
    // IOKit : NSEC_PER_USEC "1000ull" => "1000   "
    // => check if difference is only in int suffix
    if (s1 = '') and IsIntSuffix(s2) or (s2 = '') and IsIntSuffix(s1) then
      Exit(True);
    Result := False;
  end;

var
  l: TLex; defIndex: Integer;
  dm: TDefineMacro; redecl: Boolean;
begin
  l := GetNextLex(False);
  if l.LexType <> ltSymbol then
  begin
    WriteLn('error in #define');
    SkipPreprocessorDirective;
    Exit;
  end;
  redecl := False;
  if l.rawData='COREGRAPHICS_H_'then
  writeln;
  defIndex := FDefines.IndexOf(l.rawData);
  if defIndex < 0 then
    defIndex := FDefines.Add(l.rawData)
  else begin
    redecl := True;
  end;
  dm := TDefineMacro.Create(FCurFrameworkUnit, FCurFilename);
  ParseMacro(l.rawData, dm);
  if redecl then
  begin
    if not SameSubstitute(TDefineMacro(FDefines.Objects[defIndex]), dm) then
    begin
      WriteLn('"', l.rawData , '" redefined in ', FCurFilename, '!');
      WriteLn('"', TDefineMacro(FDefines.Objects[defIndex]).Substitute, '" => "', dm.Substitute, '"');
      {$ifdef VERBOSE}
      if TDefineMacro(FDefines.Objects[defIndex]).FDefinedInFile <> '*' then
      begin
        WriteLn('[press Enter]');
        ReadLn;
      end;
      {$endif}
    end;
    dm.Free;
  end else
    FDefines.Objects[defIndex] := dm;
end;

procedure THeaderScanner.ProcessIfDefDirective;
var l: TLex;
begin
  l := GetNextLex(False);
  ProcessAfterIfParse(FDefines.IndexOf(l.rawData) >= 0);
end;

procedure THeaderScanner.ProcessIfDirective;
var
  p: THeaderScanner;
  cur_lex: TLex;

  function E(): TLex; forward;

  function F(): TLex;
  var r: TLex;
  begin
    if cur_lex.eq('(') then
    begin
      cur_lex := p.GetNextLex;
      Result := E();
      // here cur_lex must be ")"
      cur_lex := p.GetNextLex;
    end else
    if cur_lex.eq('!') then
    begin
      cur_lex := p.GetNextLex;
      r := F();
      if r.eq('0') then
        Result := MakeLex('1', 1, ltNumber)
      else
        Result := MakeLex('0', 1, ltNumber);
    end else
    if cur_lex.eq('defined') then
    begin
      cur_lex := p.GetNextLex(False);
      if not cur_lex.eq('(') then
      begin
        if FDefines.IndexOf(cur_lex.rawData) >= 0 then
          Result := MakeLex('1', 1, ltNumber)
        else
          Result := MakeLex('0', 1, ltNumber);
      end else begin
        cur_lex := p.GetNextLex(False);
        if FDefines.IndexOf(cur_lex.rawData) >= 0 then
          Result := MakeLex('1', 1, ltNumber)
        else
          Result := MakeLex('0', 1, ltNumber);
        cur_lex := p.GetNextLex; // ")"
      end;
      cur_lex := p.GetNextLex;
    end else begin
      if cur_lex.LexType = ltNumber then
        Result := cur_lex
      else
        Result := MakeLex('0', 1, ltNumber);
      cur_lex := p.GetNextLex;
    end;
  end;

  function T(): TLex;
  var r, op: TLex; s: string;
  begin
    Result := F();
    while Assigned(cur_lex)
    and (cur_lex.eq('*') or cur_lex.eq('/')) do
    begin
      op := cur_lex;
      cur_lex := p.GetNextLex;
      r := F();
      if op.eq('*') then
      begin
        s := IntToStr(StrToIntDef(Result.rawData, 1) * StrToIntDef(r.rawData, 1));
        Result := MakeLex(PChar(s), Length(s), ltNumber)
      end else
//      if op.eq('/') then
      begin
        s := IntToStr(StrToIntDef(Result.rawData, 1) div StrToIntDef(r.rawData, 1));
        Result := MakeLex(PChar(s), Length(s), ltNumber)
      end;
    end;
  end;

  function E2(): TLex;
  var r, op: TLex; s: string;
  begin
    Result := T();
    while Assigned(cur_lex) and (cur_lex.eq('+') or cur_lex.eq('-')) do
    begin
      op := cur_lex;
      cur_lex := p.GetNextLex;
      r := T();
      if op.eq('+') then
      begin
        s := IntToStr(StrToIntDef(Result.rawData, 1) + StrToIntDef(r.rawData, 1));
        Result := MakeLex(PChar(s), Length(s), ltNumber)
      end else
      //if op.eq('-') then
      begin
        s := IntToStr(StrToIntDef(Result.rawData, 1) - StrToIntDef(r.rawData, 1));
        Result := MakeLex(PChar(s), Length(s), ltNumber)
      end;
    end;
  end;

  function E1(): TLex;
  var r, op: TLex;
  begin
    Result := E2();
    if not Assigned(cur_lex) then Exit;
    if cur_lex.eq('==') or cur_lex.eq('!=') or cur_lex.eq('<')
    or cur_lex.eq('<=') or cur_lex.eq('>') or cur_lex.eq('>=') then
    begin
      op := cur_lex;
      cur_lex := p.GetNextLex;
      r := E2();
      if op.eq('==') then
      begin
        if Result.eq(r) then
          Exit(MakeLex('1', 1, ltNumber))
        else
          Exit(MakeLex('0', 1, ltNumber))
      end else
      if op.eq('!=') then
      begin
        if not Result.eq(r) then
          Exit(MakeLex('1', 1, ltNumber))
        else
          Exit(MakeLex('0', 1, ltNumber))
      end else
      if op.eq('<') then
      begin
        if Result.rawData < r.rawData then
          Exit(MakeLex('1', 1, ltNumber))
        else
          Exit(MakeLex('0', 1, ltNumber))
      end else
      if op.eq('<=') then
      begin
        if Result.rawData <= r.rawData then
          Exit(MakeLex('1', 1, ltNumber))
        else
          Exit(MakeLex('0', 1, ltNumber))
      end else
      if op.eq('>') then
      begin
        if Result.rawData > r.rawData then
          Exit(MakeLex('1', 1, ltNumber))
        else
          Exit(MakeLex('0', 1, ltNumber))
      end else
      //if op.eq('>=') then
      begin
        if Result.rawData >= r.rawData then
          Exit(MakeLex('1', 1, ltNumber))
        else
          Exit(MakeLex('0', 1, ltNumber))
      end;
    end;
  end;

  function E0(): TLex;
  var r: TLex;
  begin
    Result := E1();
    while Assigned(cur_lex) and cur_lex.eq('&&') do
    begin
      cur_lex := p.GetNextLex;
      r := E1();
      if Result.eq('0') or r.eq('0') then
        Result := MakeLex('0', 1, ltNumber)
      else
        Result := MakeLex('1', 1, ltNumber);
    end;
  end;

  function E(): TLex;
  var r: TLex;
  begin
    Result := E0();
    while Assigned(cur_lex) and (cur_lex.eq('||') ) do
    begin
      cur_lex := p.GetNextLex;
      r := E0();
      if not Result.eq('0') or not r.eq('0') then
        Result := MakeLex('1', 1, ltNumber)
      else
        Result := MakeLex('0', 1, ltNumber);
    end;
  end;

var
  CondBuf: string;
  res: Boolean;
  l, ll: TLex;
  notInDefined: Boolean;
begin
  CondBuf := '';
  next_char;
  ll := nil;
  notInDefined := True;
  while not (cc in [#0, #10, #13]) do
  begin
    if cc = '/' then
    begin
      next_char;
      case CheckComment of
      0: // no comment
        begin
          ll := MakeLex('/', 1, ltDelim);
          CondBuf := CondBuf + '/';
        end;
      1: // one-line comment
        Break;
      else
        // multiline comment
        while cc in [' ', #9] do next_char;
        Continue;
      end;
    end;
    l := GetNextLex(notInDefined);
    while cc in [#9, ' '] do next_char;
    if notInDefined and l.eq('defined') then
      notInDefined := False;
    if not notInDefined and l.eq(')') then
      notInDefined := True;
    while (cc in [#10, #13]) and l.eq('\') do l := GetNextLex;
    if Assigned(ll) and (ll.LexType <> ltDelim) and (l.LexType <> ltDelim) then
      CondBuf := CondBuf + ' ';
    CondBuf := CondBuf + l.rawData;
    ll := l;
  end;
  if cc <> #0 then next_char;
  p := THeaderScanner.Create('', TStringStream.Create(CondBuf));
  try
    cur_lex := p.GetNextLex;
    res := not E().eq('0');
  finally
    p.Free;
  end;
  ProcessAfterIfParse(res);
end;

procedure THeaderScanner.ProcessIfNdefDirective;
var l: TLex;
begin
  l := GetNextLex(False);
  ProcessAfterIfParse(FDefines.IndexOf(l.rawData) < 0);
end;

procedure THeaderScanner.ProcessAfterIfParse(CondVal: Boolean);
var
  k: Integer;
  newBuf: TMemoryStream;
  directive: string;
  se: TStreamsStackElement;
  elif, else_: Boolean;
  com_line, com_mline: Boolean;
begin
  k := 1; elif := False; else_ := False;
  com_line := False; com_mline := False;
  newBuf := TMemoryStream.Create;
  while k > 0 do
  begin
    if not com_line and not com_mline and (cc = '*') and (prev_cc = '/') then
      com_mline := True else
    if not com_line and not com_mline and (cc = '/') and (prev_cc = '/') then
      com_line := True else
    if com_line and (cc in [#10, #13]) then
      com_line := False else
    if com_mline and (cc = '/') and (prev_cc = '*') then
      com_mline := False;
    if (cc = '#') and not com_line and not com_mline then
    begin
      next_char;
      directive := '';
      while cc in [#9, ' '] do next_char;
      while not (cc in [#0, #9, #10, #13, ' ']) do
      begin
        directive := directive + cc;
        next_char;
      end;
      if directive = 'if' then Inc(k) else
      if directive = 'ifdef' then Inc(k) else
      if directive = 'ifndef' then Inc(k) else
      if directive = 'endif' then
      begin
        Dec(k);
        if (k = 0) then
        begin
          if not else_ and not elif then Continue;
          if else_ and CondVal then Continue;
        end;
      end else
      if (directive = 'else') and (k = 1) and not elif then
      begin
        else_ := True;
        CondVal := not CondVal;
        Continue;
      end else
      if (directive = 'elif') and (k = 1) and not elif and not else_ then
      begin
        elif := True;
        CondVal := not CondVal;
        directive := 'if';
      end;
      if CondVal then
      begin
        directive := '#' + directive + cc;
        newBuf.Write(directive[1], Length(directive));
      end;
      if k > 0 then next_char;
      Continue;
    end;
    if CondVal then
      newBuf.Write(cc, SizeOf(cc));
    if cc = #0 then
      raise Exception.Create('something wrong!');
    next_char;
  end;
  if newBuf.Size > 0 then
  begin
    {// for debug:  }
    newBuf.Position := 0;
    SetLength(directive, newBuf.Size);
    newBuf.Read(directive[1], Length(directive));
    // "directive" here is a new input buffer
    {}
    newBuf.Position := 0;
    se := TStreamsStackElement.Create(Self);
    FStreams.Insert(0, se);
    FStream := newBuf;
    cc := #0;
    next_char;
  end else newBuf.Free;
end;

procedure THeaderScanner.ProcessIncludeDirective(import: Boolean);

  function FixPath(const path: string): string;
  var i, j: Integer;
  begin
    Result := path;
    i := Pos('..', Result);
    while i > 0 do
    begin
      j := i - 2;
      while (j > 0) and (Result[j] <> PathDelim) do Dec(j);
      if j <= 0 then Exit;
      Delete(Result, j, i + 2 - j);
      i := Pos('..', Result);
    end;
  end;

  function CharCount(c: Char; const str: string): Integer;
  var i: Integer;
  begin
    Result := 0;
    for i := 1 to Length(str) do
      if str[i] = c then Inc(Result);
  end;

var
  l: TLex;
  incFile, fr, s: string;
  sysInc: Boolean;
begin
  sysInc := False;
  l := GetNextLex;
  incFile := '';
  if l.eq('<') then
  begin
    sysInc := True;
    while not (cc in [#0, '>']) do
    begin
      incFile := incFile + cc;
      next_char;
    end;
    if cc = #0 then
    begin
      WriteLn('unexpected end of stream');
      Exit;
    end;
    next_char;
  end else
  if l.LexType = ltString then
  begin
    incFile := l.AsString;
    incFile := Copy(incFile, 2, Length(incFile) - 2); // remove <">
  end else begin
    WriteLn('Error in include/import directive: " or < expected but ', l.rawData, ' found');
    SkipPreprocessorDirective;
  end;
  if FHeadersToSkip.IndexOf(incFile) >= 0 then
    WriteLn('  ', incFile, ' -- skip')
  else
  if sysInc then
  begin
    // there are 3 posibilities:
    //  0) nested framework
    //  1) part of current framework
    //  2) other framework dependency
    //  3) system header (e.g. <stdio.h>)
    if Pos('/', incFile) > 0 then
    begin
      fr := Copy(incFile, 1, Pos('/', incFile) - 1); // framework
      s := FixPath(ExtractFilePath(FCurFramework) + '..' + PathDelim
        + 'Frameworks' + PathDelim + fr + '.framework');
      if DirectoryExists(s) then
      begin
        // nested framework - should be checked first
        WriteLn('Nested framework ', FCurFrameworkUnit, '::', fr);
        s := s + PathDelim + 'Headers' + PathDelim
          + Copy(incFile, Pos('/', incFile) + 1, MaxInt);
        if FileExists(s) then
        begin
          if (not import or (FImported.IndexOf(incFile) < 0))
          and DoNestedFrameworkReference(s) then
          begin
            if import then FImported.Add(incFile);
            ParseNewFile(s, False, True);
          end;
        end else
          WriteLn('include file "', s, '" not found');
      end else
      if Pos(PathDelim + fr + '.framework', FCurFramework) > 0 then
      begin
        // part of current framework => parse include file
        fr := Copy(FCurFramework, 1, LastDelimiter(PathDelim, FCurFramework));
        fr := fr + Copy(incFile, Pos('/', incFile) + 1, MaxInt);
        if not FileExists(fr) then
        begin
          WriteLn('Include file "', fr, '" could not be found -- skipped');
        end else begin
          if not import or (FImported.IndexOf(incFile) < 0) then
          begin
            if import then FImported.Add(incFile);
            ParseNewFile(fr, False, True);
          end;
        end;
      end else begin
        // different framework reference => it need to be included to uses clause
        // check for Framework
        s := Copy(FCurFramework, 1, Pos('Frameworks', FCurFramework) + 9)
          + PathDelim + fr + '.framework';
        if DirectoryExists(s) then
        begin
          DoFrameworkReference(fr);
          fr := s + PathDelim + 'Headers' + PathDelim + Copy(incFile, Pos('/', incFile) + 1, MaxInt);
          if not FileExists(fr) then
          begin
            WriteLn('Include file "', fr, '" could not be found -- skipped');
          end else begin
            if not import or (FImported.IndexOf(incFile) < 0) then
            begin
              if import then FImported.Add(incFile);
              ParseNewFile(fr, False, True);
            end;
          end;
        end else begin
          // system header, e.g. <objc/objc.h>
          s := Copy(FCurFramework, 1, Pos('Frameworks', FCurFramework) - 1)
            + '..' + PathDelim + '..' + PathDelim + 'usr' + PathDelim
            + 'include' + PathDelim + incFile;
          s := FixPath(s);
          if FileExists(s) then
          begin
            if FIncludeEnabled
            or (Pos(PathDelim + 'include' + PathDelim, FCurFilename) > 0)
            then begin
              if not import or (FImported.IndexOf(incFile) < 0) then
              begin
                if import then FImported.Add(incFile);
                ParseNewFile(s, True, False);
              end;
            end;
          end else
            WriteLn('include file "', s, '" not found');
        end;
      end;
    end else begin
      // it is 3-rd posibility -- system header
      s := Copy(FCurFramework, 1, Pos('Frameworks', FCurFramework) - 1)
        + '..' + PathDelim + '..' + PathDelim + 'usr' + PathDelim
        + 'include' + PathDelim + incFile;
      s := FixPath(s);
      if FileExists(s) then
      begin
        if FIncludeEnabled
        or (Pos(PathDelim + 'include' + PathDelim, FCurFilename) > 0)
        then begin
          if not import or (FImported.IndexOf(incFile) < 0) then
          begin
            if import then FImported.Add(incFile);
            ParseNewFile(s, True, False);
          end;
        end;
      end else
        WriteLn('include file "', s, '" not found');
    end;
  end else begin
    // #include "somefile.h" -- in current dir?
    // FCurFileName = 'D:\mac\System\Library\..\..\usr\include\machine/types.h'
    // incFile = 'i386/types.h'
    s := ExtractFilePath(FCurFramework) + incFile;
    if FileExists(s) then
    begin
      // FCurHeaderIsSystem and FCurHeaderBelongsToFramework are the old
      ParseNewFile(s, FCurHeaderIsSystem, FCurHeaderBelongsToFramework);
      Exit;
    end else begin
      // it seams must be <...>, not "..."
      s := Copy(FCurFramework, 1, Pos('Frameworks', FCurFramework) - 1)
        + '..' + PathDelim + '..' + PathDelim + 'usr' + PathDelim
        + 'include' + PathDelim + incFile;
      s := FixPath(s);
      if FileExists(s) then
      begin
        if FIncludeEnabled
        or (Pos(PathDelim + 'include' + PathDelim, FCurFilename) > 0)
        then begin
          if not import or (FImported.IndexOf(incFile) < 0) then
          begin
            if import then FImported.Add(incFile);
            ParseNewFile(s, True, False);
          end;
        end;
      end else
        WriteLn('include file "', incFile, '" not found');
    end;
  end;
end;

procedure THeaderScanner.ProcessPragmaDirective;
var l: TLex;
begin
  l := GetNextLex;
  if l.eq('pack') then
  begin
    l := GetNextLex;
    if not l.eq('(') then
      raise Exception.Create('"(" expected but "' + l.AsString + '" found');
    l := GetNextLex;
    if l.eq('push') then
    begin
      l := GetNextLex;
      l := GetNextLex;
    end else
    if l.eq('pop') then
      l := GetNextLex;
    if l.LexType = ltNumber then
    begin
      DoPragma('{$packrecords ' + l.AsString + '}');
      l := GetNextLex;
    end else
    if l.eq(')') then
      DoPragma('{$packrecords c}');
    if not l.eq(')') then
      raise Exception.Create('")" expected but "' + l.AsString + '" found');
    while not (cc in [#0, #10, #13]) do next_char;
    next_char;
  end else
  if l.eq('GCC') then
  begin
    l := GetNextLex;
    case l.AsString of
    'poison':
      // skip this directive
      SkipPreprocessorDirective;
    else
      raise Exception.Create('unknown #pragma GCC directive: ' + l.AsString);
    end;
  end else
  if l.eq('mark') then
    SkipPreprocessorDirective // Xcode feature
  else
  if l.eq('options') then
  begin
    l := GetNextLex;
    case l.AsString of
    'align':
      begin
        l := GetNextLex;
        if not l.eq('=') then
          raise Exception.Create('"=" expected in #pragma options');
        l := GetNextLex;
        case l.AsString of
        'power',
        'mac68k',
        'twobyte': DoPragma('{$packrecords 2}');
        'reset':   DoPragma('{$packrecords c}');
        else
          raise Exception.Create('unknown option for "align": ' + l.AsString);
        end;
      end;
    else
      raise Exception.Create('unknown "#pragma options" option: ' + l.AsString);
    end;
  end else
    raise Exception.Create('unknown #pragma directive: ' + l.AsString);
end;

procedure THeaderScanner.ProcessUndefDirective;
var l: TLex; i: Integer;
begin
  l := GetNextLex(False);
  if Assigned(l) then
  begin
    i := FDefines.IndexOf(l.rawData);
    if i >= 0 then
    begin
      FDefines.Objects[i].Free;
      FDefines.Delete(i);
    end;
  end;
end;

procedure THeaderScanner.SkipPreprocessorDirective;
begin
  while next_char <> #0 do
    if cc in [#10, #13] then
      if prev_cc <> '\' then
      begin
        NewLine;
        Break;
      end else NewLine;
end;

constructor THeaderScanner.Create(CurFramework: string; AStream: TStream);
begin
  FHeadersToSkip := TStringList.Create;
  FHeadersToSkip.Sorted := True;
  //FHeadersToSkip.Add('MacTypes.h'); // it's needed for ConditionalMacros.h
  FHeadersToSkip.Add('OpenGLES/ES1/gl.h');
  FHeadersToSkip.Add('OpenGLES/ES1/glext.h');
  FHeadersToSkip.Add('OpenGLES/ES2/gl.h');
  FHeadersToSkip.Add('OpenGLES/ES2/glext.h');
  FHeadersToSkip.Add('sys/socket.h');
  FHeadersToSkip.Add('vecLib/vForce.h'); // it contains declarations I cannot understand : typedef _Complex float __float_complex_t;
  FHeadersToSkip.Add('WebServicesCore/WSMethodInvocation.h'); // undeclared type CFXMLTreeRef
  FHeadersToSkip.Add('WebServicesCore/WSProtocolHandler.h'); // undeclared type CFXMLTreeRef

  FRefFrameworks := TStringList.Create;
  FRefFrameworks.Sorted := True;
  FCurHeaderBelongsToFramework := True;
  FCurHeaderIsSystem := False;
  FIncludeEnabled := False;
  FCurFramework := CurFramework;
  FCurFrameworkUnit := ChangeFileExt(ExtractFileName(FCurFramework), '');
  if FCurFrameworkUnit = '*' then
  begin
    // Frameworks without main header:
    // OpenGLES, CoreAudio, CoreTelephony, IOKit, OpenAL
    FCurFrameworkUnit := Copy(FCurFramework, 1, Pos('.framework', FCurFramework) - 1);
    FCurFrameworkUnit := ExtractFileName(FCurFrameworkUnit);
  end;
  FCurFilename := FCurFramework;
  FDefines := TStringList.Create;
  FDefines.CaseSensitive := True;
  FDefines.Sorted := True;
  FImported := TStringList.Create;
  FImported.CaseSensitive := True;
  FImported.Sorted := True;
  FStreams := TFPList.Create;
  FStream := AStream;
  FLine := 1; FChar := 0;
  FLexems := TStringList.Create;
  FLexems.CaseSensitive := True;
  FLexems.Sorted := True;
  cc := #0;
  next_char;
end;

destructor THeaderScanner.Destroy;
var i: Integer;
begin
  FHeadersToSkip.Free;
  FRefFrameworks.Free;
  FImported.Free;
  for i := 0 to FLexems.Count - 1 do
    FLexems.Objects[i].Free;
  FLexems.Free;
  for i := 0 to FStreams.Count - 1 do
    with TStreamsStackElement(FStreams[i]) do
    begin
      Stream.Free;
      Free;
    end;
  FStreams.Free;
  FStream.Free;
  for i := 0 to FDefines.Count - 1 do
    FDefines.Objects[i].Free;
  FDefines.Free;
  inherited Destroy;
end;

procedure THeaderScanner.AddDefine(def: string);
var lcc: Char; lStream: TStream; lFileName: string;
begin
  lStream := FStream;
  lcc := cc;
  lFileName := FCurFilename;
  FCurFilename := '*';
  FStream := TStringStream.Create(def + sLineBreak + sLineBreak);
  try
    next_char;
    ProcessDefineDirective;
  finally
    FStream.Free;
    FCurFilename := lFileName;
    cc := lcc;
    FStream := lStream;
  end;
end;

function THeaderScanner.GetNextLex(handleMacros: Boolean = True): TLex;
type
  TScannerStates = (ssH, ssID, ssNUM, ssNUMP, ssNUMH, ssEQ, ssCOM, ssCOM_LINE,
    ssCOM_MLINE, ssDELIM, ssEQ_DOUBLE, ssDOUBLE, ssPOINT, ssMIN, ssQUOTE,
    ssDQUOTE);

var
  state: TScannerStates;
  buf: array [0..1024] of Char;
  bufLen: Integer;
begin
  if handleMacros then FLastAttribute := '';
  state := ssH;
  Result := nil;
  if cc = #0 then Exit;
  bufLen := 0;
  while True do
  begin
    case state of
    ssH:
      if cc = #0 then Exit else
      if cc = '#' then
      begin
        PreprocessorDirective;
        Continue;
      end else
      if cc in [#10, #13] then NewLine else
      if cc in [' ', #9] then next_char else
      if cc in Alpha then
      begin
        state := ssID;
        buf[bufLen] := cc; Inc(bufLen);
        next_char;
        Continue;
      end else
      if cc in Digits then
      begin
        state := ssNUM;
        buf[bufLen] := cc; Inc(bufLen);
        next_char;
        Continue;
      end else
      if cc in ['<', '>', '+'] then
      begin
        buf[0] := cc; bufLen := 1;
        state := ssEQ_DOUBLE;
        next_char;
        Continue;
      end else
      if cc in ['=', '!', '*'] then
      begin
        buf[0] := cc; bufLen := 1;
        state := ssEQ;
        next_char;
        Continue;
      end else
      if cc in ['|', '&'] then
      begin
        buf[0] := cc; bufLen := 1;
        state := ssDOUBLE;
        next_char;
        Continue;
      end else
      if cc = '.' then
      begin
        state := ssPOINT;
        buf[0] := cc; bufLen := 1;
        next_char;
        Continue;
      end else
      if cc = '-' then
      begin
        state := ssMIN;
        buf[0] := cc; bufLen := 1;
        next_char;
        Continue;
      end else
      if cc = '/' then
      begin
        state := ssCOM;
        FComment := '';
        next_char;
        Continue;
      end else
      if cc = '''' then
      begin
        buf[0] := cc; bufLen := 1;
        state := ssQUOTE;
        next_char;
        Continue;
      end else
      if cc = '"' then
      begin
        buf[0] := cc; bufLen := 1;
        state := ssDQUOTE;
        next_char;
        Continue;
      end else
      if cc = '\' then
      begin
        next_char;
        if cc in [#10, #13] then
          Continue
        else begin
          buf[0] := cc; bufLen := 1;
          state := ssDELIM;
        end;
        Continue;
      end else begin
        buf[0] := cc; bufLen := 1;
        state := ssDELIM;
        next_char;
        Continue;
      end;
    ssID:
      if cc in AlphaDig then
      begin
        buf[bufLen] := cc; Inc(bufLen);
        next_char;
        Continue;
      end else begin
        if handleMacros and IsMacro(@buf[0], bufLen) then
        begin
          state := ssH;
          bufLen := 0;
          Continue;
        end;
        Exit(MakeLex(@buf[0], bufLen, ltSymbol));
      end;
    ssNUM:
      if cc = 'x' then
      begin
        buf[bufLen] := cc; Inc(bufLen);
        next_char;
        state := ssNUMH;
        Continue;
      end else
      if cc = '.' then
      begin
        buf[bufLen] := cc; Inc(bufLen);
        next_char;
        state := ssNUMP;
        Continue;
      end else
      if cc in Digits then
      begin
        buf[bufLen] := cc; Inc(bufLen);
        next_char;
        Continue;
      end else
      if cc in ['e','E'] then
      begin
        buf[bufLen] := cc; Inc(bufLen);
        next_char;
        if cc in ['+', '-'] then
        begin
          buf[bufLen] := cc; Inc(bufLen);
          next_char;
        end;
        state := ssNUMP;
        Continue;
      end else
      if cc in ['u', 'U'] then
      begin
        buf[bufLen] := cc; Inc(bufLen);
        next_char;
        if cc in ['l', 'L'] then
        begin
          buf[bufLen] := cc; Inc(bufLen);
          next_char;
          if cc in ['l', 'L'] then
          begin
            buf[bufLen] := cc; Inc(bufLen);
            next_char;
          end;
          Exit(MakeLex(@buf[0], bufLen, ltNumber));
        end else
          Exit(MakeLex(@buf[0], bufLen, ltNumber));
      end else
      if cc in ['l','L','f','F','d','D'] then
      begin
        buf[bufLen] := cc; Inc(bufLen);
        next_char;
        Exit(MakeLex(@buf[0], bufLen, ltNumber));
      end else Exit(MakeLex(@buf[0], bufLen, ltNumber));
    ssNUMH:
      if cc in Digits + ['a'..'f', 'A'..'F'] then
      begin
        buf[bufLen] := cc; Inc(bufLen);
        next_char;
        Continue;
      end else
      if cc in ['u', 'U'] then
      begin
        buf[bufLen] := cc; Inc(bufLen);
        next_char;
        if cc in ['l', 'L'] then
        begin
          buf[bufLen] := cc; Inc(bufLen);
          next_char;
          if cc in ['l', 'L'] then
          begin
            buf[bufLen] := cc; Inc(bufLen);
            next_char;
          end;
          Exit(MakeLex(@buf[0], bufLen, ltNumber));
        end;
        Exit(MakeLex(@buf[0], bufLen, ltNumber));
      end else
      if cc in ['l', 'L'] then
      begin
        buf[bufLen] := cc; Inc(bufLen);
        next_char;
        if cc in ['l', 'L'] then
        begin
          buf[bufLen] := cc; Inc(bufLen);
          next_char;
        end;
        Exit(MakeLex(@buf[0], bufLen, ltNumber));
      end else
        Exit(MakeLex(@buf[0], bufLen, ltNumber));
    ssNUMP:
      if cc in Digits then
      begin
        buf[bufLen] := cc; Inc(bufLen);
        next_char;
        Continue;
      end else
      if cc in ['l', 'L'] then
      begin
        buf[bufLen] := cc; Inc(bufLen);
        next_char;
        if cc in ['d', 'D'] then
          Continue
        else
          Exit(MakeLex(@buf[0], bufLen, ltNumber));
      end else
      if cc in ['f','F','d','D'] then
      begin
        buf[bufLen] := cc; Inc(bufLen);
        next_char;
        Exit(MakeLex(@buf[0], bufLen, ltNumber));
      end else Exit(MakeLex(@buf[0], bufLen, ltNumber));
    ssEQ:
      if cc = '=' then
      begin
        buf[bufLen] := cc; Inc(bufLen);
        next_char;
        Exit(MakeLex(@buf[0], bufLen, ltDelim));
      end else begin
        state := ssDELIM;
        Continue;
      end;
    ssCOM:
      if cc = '/' then state := ssCOM_LINE else
      if cc = '*' then
      begin
        state := ssCOM_MLINE;
        next_char;
        Continue;
      end else begin
        buf[0] := '/';
        bufLen := 1;
        state := ssEQ; // maybe "/="
      end;
    ssCOM_LINE:
      if cc in [#10, #13] then
      begin
        DoOnCommentRead;
        next_char;
        state := ssH;
        Continue;
      end else begin
        FComment := FComment + cc;
        next_char;
      end;
    ssCOM_MLINE:
      begin
        if cc in [#10, #13] then NewLine;
        if cc = '*' then
        begin
          next_char;
          if cc = '/' then
          begin
            DoOnCommentRead;
            state := ssH;
            next_char;
          end else
            FComment := FComment + '*' + cc;
        end else begin
          FComment := FComment + cc;
          next_char;
        end;
      end;
    ssDELIM:
      begin
        // check buf is valid delimetr
        Exit(MakeLex(@buf[0], bufLen, ltDelim));
      end;
    ssEQ_DOUBLE: // "?="
      if cc in [buf[0], '='] then
      begin
        buf[1] := cc;
        next_char;
        Exit(MakeLex(@buf[0], 2, ltDelim));
      end else state := ssDELIM;
    ssDOUBLE:
      if cc = buf[0] then
      begin
        buf[1] := cc;
        next_char;
        Exit(MakeLex(@buf[0], 2, ltDelim));
      end else state := ssDELIM;
    ssPOINT: // "." or "..."
      if cc = '.' then
      begin
        buf[bufLen] := cc; Inc(bufLen);
        next_char;
        if bufLen = 3 then Exit(MakeLex(@buf[0], 3, ltDelim));
      end else Exit(MakeLex(@buf[0], bufLen, ltDelim));
    ssMIN: // "-" or "->"
      if cc = '>' then
      begin
        buf[bufLen] := cc; Inc(bufLen);
        next_char;
        Exit(MakeLex(@buf[0], bufLen, ltDelim));
      end else
        state := ssEQ;
    ssQUOTE: // 'xxx' is a number!
      if (cc = '''') and (prev_cc <> '\') then
      begin
        buf[bufLen] := cc; Inc(bufLen);
        next_char;
        Exit(MakeLex(@buf[0], bufLen, ltNumber));
      end else begin
        buf[bufLen] := cc; Inc(bufLen);
        next_char;
      end;
    ssDQUOTE:
      if cc = '"' then
      begin
        next_char;
        while cc in [' ', #9] do next_char;
        if cc = '"' then
        begin
          // concatenation: "aaa" "bbb" => "aaabbb"
          next_char;
          Continue;
        end else begin
          buf[bufLen] := '"'; Inc(bufLen);
          Exit(MakeLex(@buf[0], bufLen, ltString));
        end;
      end else
      if cc = '\' then
      begin
        // save ESC seq
        buf[bufLen] := cc; Inc(bufLen);
        next_char;
        // ignore \"
        buf[bufLen] := cc; Inc(bufLen);
        next_char;
      end else begin
        buf[bufLen] := cc;
        Inc(bufLen);
        next_char;
      end;
    end;
    if cc = #0 then Exit;
  end;
end;

procedure THeaderScanner.RetrieveDefines(dest: TStrings);
var i: Integer;
begin
  for i := 0 to dest.Count - 1 do
    dest.Objects[i].Free;
  dest.Clear;
  dest.AddStrings(FDefines);
  FDefines.Clear;
end;

procedure THeaderScanner.RetrieveImported(dest: TStrings);
begin
  dest.AddStrings(FImported);
end;

procedure THeaderScanner.InitDefines(src: TStrings);
var i: Integer;
begin
  if src.Count = 0 then Exit;
  for i := 0 to FDefines.Count - 1 do
    FDefines.Objects[i].Free;
  FDefines.Clear;
  FDefines.AddStrings(src);
  for i := 0 to FDefines.Count - 1 do
    FDefines.Objects[i] := TDefineMacro(FDefines.Objects[i]).Copy;
end;

procedure THeaderScanner.InitImported(src: TStrings);
begin
  FImported.Clear;
  FImported.AddStrings(src);
end;

procedure THeaderScanner.StoreDefines(src: TStrings);
var i, j: Integer; s1: string;
begin
  i := 0;
  j := 0;
  while i < src.Count do
  begin
    s1 := src[i];
    while (j < FDefines.Count) and (FDefines[j] <> s1) do Inc(j);
    if j >= FDefines.Count then Exit;
    if s1 = FDefines[j] then
    begin
      FDefines.Objects[j] := nil;
      Inc(j);
    end;
    Inc(i);
  end;
end;

end.

