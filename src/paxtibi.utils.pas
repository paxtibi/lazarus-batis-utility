unit paxtibi.utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, fgl, SysUtils;

const
  ISO_EXTENDED_FORMAT_PATTERN: string = 'PyyyyYMMdDTHHmMs.SS';

const
  MILLIS_PER_SECOND = 1000;
  MILLIS_PER_MINUTE = 60 * MILLIS_PER_SECOND;
  MILLIS_PER_HOUR   = 60 * MILLIS_PER_MINUTE;
  MILLIS_PER_DAY    = 24 * MILLIS_PER_HOUR;

type
  { TToken }
  TToken = class;
  TTokenList = specialize TFPGList<TToken>;

  TToken = class
  protected
    class function containsTokenWithValue(tokens: TTokenList; Value: string): boolean;
  private
    fValue: string;
    fCount: int64;
  public
    constructor Create(Value: string); overload;
  public
    constructor Create(Value: string; aCount: int64); overload;
    procedure increment();
    function getCount(): int64;
    function getValue(): string;
    function Equals(Obj: TObject): boolean; override;
    function GetHashCode: PtrInt; override;
    function ToString: ansistring; override;
  end;

  { TDurationFormatUtils }

  TDurationFormatUtils = class
  protected
    class function lexx(format: string): TTokenList;
  protected
    class function format(tokens: TTokenList; years: Int32; months: Int32; days: Int32; hours: Int32; minutes: Int32; seconds: Int32; milliseconds: Int32; padWithZeros: boolean): string;
  public
    class function formatDuration(durationMillis: int64; tl: TTokenList; padWithZeros: boolean = True): string; overload;
    class function formatDuration(durationMillis: int64; aFormatString: string = 'HH:mm:ss.SSSS'; padWithZeros: boolean = True): string; overload;
    constructor Create;
  end;


  { TCronometro }

  TCronometro = class
  protected
    FStartTime: TDateTime;
  public
    function start: TDateTime;
    function stop: TDateTime;
    function getElapsed: int64;
    constructor Create;
    class function format(aElapsedTime: int64): string;
  end;

  { TPrintStream }

  TPrintStream = class(TStream)
  private
    fAutoFlush: boolean;
    fTrouble: boolean;
    fClosing: boolean;
    fOwner: boolean;
    FStream: TStream;
    procedure Write(const buf: array of char); reintroduce; overload;
    procedure Write(const s: string); reintroduce; overload;
  private
    procedure ensureOpen(); overload; virtual;
    procedure newLine(); overload; virtual;
  protected
    procedure setError(); overload; virtual;
  public
    constructor Create(aStream: TStream; const autoFlush: boolean = False; const aOwner: boolean = False); overload;
    destructor Destroy; override;
    function checkError(): boolean; overload; virtual;
    procedure Close(); overload;
    procedure flush(); overload;
    function print(const Value: boolean): TPrintStream; overload; virtual;
    function print(const Value: char): TPrintStream; overload; virtual;
    function print(const Value: double): TPrintStream; overload; virtual;
    function print(const Value: single): TPrintStream; overload; virtual;
    function print(const Value: integer): TPrintStream; overload; virtual;
    function print(const Value: int64): TPrintStream; overload; virtual;
    function print(const Value: Pointer): TPrintStream; overload; virtual;
    function print(const Value: array of char): TPrintStream; overload; virtual;
    function print(const Value: PChar): TPrintStream; overload; virtual;
    function print(const Value: string): TPrintStream; overload; virtual;
    function print(const Value: WideString): TPrintStream; overload; virtual;
    function println(): TPrintStream; overload; virtual;
  end;

function getField(var c: PChar; const sepChar: char = ';'): string;
function getField(const c: string; const sepChar: char = ';'): string;
function Readln(Stream: TStream; var aString: string): boolean;

type
  { TParser }

  TParser = class(TObject)
  private
    fStream: TStream;
    fBuf: PChar;
    fBufLen: integer;
    fPos: integer;
    fDeltaPos: integer;
    fFloatType: char;
    fSourceLine: integer;
    fToken: char;
    fEofReached: boolean;
    fLastTokenStr: string;
    fLastTokenWStr: string;
    function GetTokenName(aTok: char): string;
    procedure LoadBuffer;
    procedure CheckLoadBuffer; inline;
    procedure ProcessChar; inline;
    function IsNumber: boolean; inline;
    function IsHexNum: boolean; inline;
    function IsAlpha: boolean; inline;
    function IsAlphaNum: boolean; inline;
    function GetHexValue(c: char): byte; inline;
    function GetAlphaNum: string;
    procedure HandleNewLine;
    procedure SkipBOM;
    procedure SkipSpaces;
    procedure SkipWhitespace;
  protected
    procedure HandleNumber; virtual;
    procedure HandleSymbol; virtual;
    procedure HandleToken; virtual;
  public
    constructor Create(Stream: TStream);
    destructor Destroy; override;
    function NextToken: char;
    function SourcePos: longint;
    property SourceLine: integer read fSourceLine;
    property Token: char read fToken;
    procedure runToEol;
    function getTokenString: string;
  end;

const
  toEOF     = char(0);
  toSymbol  = char(1);
  toString  = char(2);
  toInteger = char(3);
  toFloat   = char(4);
  toWString = char(5);
  toEOL     = char(6);
  toNumber  = char(7);

type
  TItem = class(TInterfacedObject)
  private
    FNext: TItem;
    FValue: string;
    FKey: string;
    procedure SetKey(const Value: string);
    procedure SetNext(const Value: TItem);
    procedure SetValue(const Value: string);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Next: TItem read FNext write SetNext;
    property Key: string read FKey write SetKey;
    property Value: string read FValue write SetValue;
    procedure add(aKey, aValue: string);
    function getValue(aKey: string): string;
    function getKey(index: integer): string;
    function Count: cardinal;
  end;

  TProperties = class(TinterfacedObject)
  private
    function continueLine(const line: string): boolean; overload; virtual;
    function loadConvert(const theString: string): string; overload; virtual;
    function saveConvert(const theString: string; const escapeSpace: boolean): string; overload; virtual;
    class procedure writeln(const bw: TStream; const s: string); overload; virtual;
    class function toHex(const nibble: longint): char; overload; virtual;
  protected
    FDefaults: TProperties;
    FValues: TItem;
  protected
    procedure print(_out_: TStream; message: string);
    procedure println(_out_: TStream; message: string);
    function readLine(_in_: TStream; var EOF: boolean): string;
  public
    constructor Create; overload; virtual;
    constructor Create(const defaults: TProperties); overload; virtual;
    procedure load(const inStream: TStream); overload; virtual;
    procedure load(filename: string); overload; virtual;
    procedure loadResource(resourceName: string); overload; virtual;
    procedure save(const _out_: TStream; const header: string); overload; virtual;
    procedure save(filename: string; const header: string); overload; virtual;
    procedure store(const _out_: TStream; const header: string); overload; virtual;
    function setProperty(const key: string; const Value: string): string; overload; virtual;
    function setBoolean(const key: string; const Value: boolean): string; overload; virtual;
    function setLongint(const key: string; const Value: longint): string; overload; virtual;
    function getProperty(const key: string; const defaultValue: string = ''): string; overload; virtual;
    function getBoolean(const key: string; const defaultValue: boolean = False): boolean; overload; virtual;
    function getLongint(const key: string; const defaultValue: longint = 0): longint; overload; virtual;
    function propertyNames: TStrings; overload; virtual;
  end;

  { TConfiguration }

  TConfiguration = class
  protected
    FFileName: TFileName;
    FProperties: TProperties;
  public
    constructor Create; overload;
    constructor Create(aFileName: string); overload;
    function get(aKey: string; aDefault: string = ''): string;
  end;

type
  TStringDynArray = array of string;

function substring(const s: string; start: integer; length: integer = 0): string;
function Split(const Source: string; search: char; var outputArray: TStringDynArray): integer;

implementation

uses
  {$IFDEF WINDOWS}
  Windows,
{$ENDIF}
  dateutils, strutils;


function getField(var c: PChar; const sepChar: char): string;
var
  S: PChar;
begin
  S := C;
  while not (c^ in [#0, sepChar]) do
  begin
    Inc(C);
  end;
  if (C^ = sepChar) then
  begin
    C^ := #0;
    Inc(C);
  end;
  SetString(Result, S, PtrInt(C - S));
end;

function getField(const c: string; const sepChar: char): string;
var
  s: PChar;
begin
  S      := PChar(C);
  Result := getField(s, sepChar);
end;

function substring(const s: string; start: integer; length: integer): string;
begin
  if (start < 0) then
  begin
    if (Length <= 0) then
    begin
      Length := Abs(start);
    end;
    start := System.Length(S) + start + 1;
  end;
  if (length = 0) then
  begin
    length := System.Length(s);
  end;
  if start = 0 then
  begin
    start := 1;
  end;
  Result := Copy(s, start, length);
end;

function Split(const Source: string; search: char; var outputArray: TStringDynArray): integer;
var
  C, C1: PChar;
  B: string;
begin
  System.SetLength(outputArray, 0);
  C  := PChar(Source);
  C1 := C;
  while C^ <> #0 do
  begin
    if (C^ = search) then
    begin
      SetString(B, C1, C - C1);
      SetLength(outputArray, Length(outputArray) + 1);
      outputArray[High(outputArray)] := B;
      Inc(C);
      C1 := C;
    end;
    Inc(C);
  end;
  if (C <> C1) then
  begin
    SetString(B, C1, C - C1);
    C1 := C;
    SetLength(outputArray, Length(outputArray) + 1);
    outputArray[High(outputArray)] := B;
  end;
  Result := High(outputArray);
end;

function Readln(Stream: TStream; var aString: string): boolean;
var
  RawLine: string;
  ch: char;
  bLength: longint;
begin
  bLength := SizeOf(ch);
  Result  := False;
  ch      := #0;
  RawLine := '';
  while (Stream.Read(ch, bLength) = bLength) and (not (ch in [#13, #10])) do
  begin
    Result  := True;
    RawLine := RawLine + ch;
  end;
  aString := RawLine;
  if (ch in [#13, #10]) then
  begin
    Result := True;
    if (Stream.Read(ch, bLength) = bLength) and (not (ch in [#13, #10])) then
    begin
      Stream.Seek(-1, soCurrent);
    end;
  end;
end;

{ TConfiguration }

constructor TConfiguration.Create;
begin
  Create(ChangeFileExt(ParamStr(0), '.properties'));
end;

constructor TConfiguration.Create(aFileName: string);
begin
  FFileName   := aFileName;
  FProperties := TProperties.Create();
  FProperties.load(aFileName);
end;

function TConfiguration.get(aKey: string; aDefault: string): string;
var
  aProperty: string;
begin
  aProperty := FProperties.getProperty(aKey, aDefault);
  Result    := aProperty;
end;

const
  keyValueSeparators = '=: '#9#13#10#12;
  strictKeyValueSeparators = '=:';
  specialSaveChars = '=: '#9#13#10#12'#!';
  whiteSpaceChars = ' '#9#13#10#12;
  hexDigit: array[0..15] of char = (
    '0', '1', '2', '3', '4', '5', '6', '7',
    '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');

{ TProperties }

constructor TProperties.Create;
begin
  Create(nil);
end;

constructor TProperties.Create(const defaults: TProperties);
begin
  FValues   := nil;
  FDefaults := defaults;
end;

function TProperties.setProperty(const key: string; const Value: string): string;
begin
  Result := key + '=' + Value;
  if FValues = nil then
  begin
    FValues     := TItem.Create;
    FValues.Key := key;
    FValues.Value := Value;
  end
  else
  begin
    FValues.add(key, Value);
  end;
end;

procedure TProperties.load(filename: string);
var
  FS: TFileStream;
  dir: string;
begin
  try
    dir := ExpandFileName(fileName);
    chdir(ExtractFilePath(dir));
    FS := TFileStream.Create(ExtractFileName(dir), fmOpenRead);
    load(fs);
    FS.Free;
  except
  end;
end;

procedure TProperties.load(const inStream: TStream);

  function endWidth(aString: string; aChar: char): boolean;
  begin
    aString := Trim(aString);
    Result  := aString[Length(aString)] = aChar;
  end;

var
  Strings: TStringList;
  idx: integer;
  S: string;
  aKey, aValue: string;
begin
  Strings := TStringList.Create;
  Strings.LoadFromStream(inStream);
  for idx := Strings.Count - 2 downto 0 do
  begin
    S := Trim(Strings[idx]);
    if Length(S) > 0 then
    begin
      if S[1] = '#' then
      begin
        Strings.Delete(idx);
      end;
      if endWidth(S, '\') then
      begin
        S := Trim(S);
        Delete(S, Length(S), 1);
        Strings[idx] := S + Strings[idx + 1];
        Strings.Delete(idx + 1);
        S := Strings[idx];
      end;
    end
    else
    begin
      Strings.Delete(idx);
    end;
  end;
  Strings.SaveToFile('debug.properties');
  Strings.Sort;
  for S in Strings do
  begin
    idx    := Pos('=', S);
    aKey   := Trim(copy(S, 1, idx - 1));
    aValue := loadConvert(copy(S, idx + 1, Length(S)));
    setProperty(trim(aKey), aValue);
  end;
end;

function TProperties.continueLine(const line: string): boolean;
var
  slashCount, index: longint;
begin
  slashCount := 0;
  index      := length(line);
  while ((index >= 0) and (line[index] = '\')) do
  begin
    Inc(slashCount);
    Dec(index);
  end;
  Result := ((slashCount mod 2) = 1);
end;

function TProperties.loadConvert(const theString: string): string;
var
  achar: widechar;
  len, x, i: longint;
  retChar: widechar;
  Value: word absolute retChar;
  outBuffer: string;
begin
  len := length(theString);
  x   := 1;
  while x <= len do
  begin
    aChar := theString[x];
    Inc(X);
    if (aChar = '\') then
    begin
      aChar := theString[x];
      Inc(X);
      if (aChar = 'u') then
      begin
        // Read the xxxx
        Value := 0;
        for i := 0 to 3 do
        begin
          aChar := theString[x];
          Inc(x);
          case (aChar) of
            '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
            begin
              Value := (Value shl 4) + Ord(aChar) - Ord('0');
            end;
            'a', 'b', 'c', 'd', 'e', 'f':
            begin
              Value := (Value shl 4) + 10 + Ord(aChar) - Ord('a');
            end;
            'A', 'B', 'C', 'D', 'E', 'F':
            begin
              Value := (Value shl 4) + 10 + Ord(aChar) - Ord('A');
            end;
            else
            begin
              raise Exception.Create('Malformed \\uxxxx encoding.');
            end;
          end;
        end;
        SetLength(outBuffer, Length(outBuffer) + 1);
        outbuffer += RetChar;
      end
      else
      begin
        if (aChar = 't') then
        begin
          aChar := #8;
        end
        else if (aChar = 'r') then
        begin
          aChar := #13;
        end
        else if (aChar = 'n') then
        begin
          aChar := #10;
        end
        else if (aChar = 'f') then
        begin
          aChar := #12;
        end;
        outBuffer += aChar;
      end;
    end
    else
    begin
      if pos(aChar, strictKeyValueSeparators) = 0 then
      begin
        outBuffer += aChar;
      end;
    end;
  end;
  Result := outBuffer;
end;

function TProperties.saveConvert(const theString: string; const escapeSpace: boolean): string;
var
  len, x: longint;
  outBuffer: string;
  achar: widechar;

begin
  len := length(theString);
  for x := 1 to len do
  begin
    aChar := theString[x];
    case (aChar) of
      ' ':
      begin
        if (x = 0) or (escapeSpace) then
        begin
          outBuffer := outBuffer + ('\');
        end;
        outBuffer := outBuffer + (' ');
      end;
      '\':
      begin
        outBuffer := outBuffer + ('\');
        outBuffer := outBuffer + ('\');
      end;
      #9:
      begin
        outBuffer := outBuffer + ('\');
        outBuffer := outBuffer + ('t');
      end;
      #10:
      begin
        outBuffer := outBuffer + ('\');
        outBuffer := outBuffer + ('n');
      end;
      #13:
      begin
        outBuffer := outBuffer + ('\');
        outBuffer := outBuffer + ('r');
      end;
      #12:
      begin
        outBuffer := outBuffer + ('\');
        outBuffer := outBuffer + ('f');
      end;
      else
      begin
        if ((aChar < char($0020)) or (aChar > char($007E))) then
        begin
          outBuffer := outBuffer + ('\');
          outBuffer := outBuffer + ('u');
          outBuffer := outBuffer + (toHex((Ord(aChar) shr 12) and $F));
          outBuffer := outBuffer + (toHex((Ord(aChar) shr 8) and $F));
          outBuffer := outBuffer + (toHex((Ord(aChar) shr 4) and $F));
          outBuffer := outBuffer + (toHex(Ord(aChar) and $F));
        end
        else
        begin
          if (pos(aChar, specialSaveChars) <> 0) then
          begin
            outBuffer := outBuffer + ('\');
          end;
          outBuffer := outBuffer + (aChar);
        end;
      end;
    end;
  end;
  Result := outBuffer;
end;

procedure TProperties.save(const _out_: TStream; const header: string);
begin
  store(_out_, header);
end;

procedure TProperties.save(filename: string; const header: string);
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(filename, fmCreate or fmOpenWrite);
  store(fs, header);
  FS.Free;
end;

procedure TProperties.store(const _out_: TStream; const header: string);
var
  index: longint;
  e: TStrings;
  key, val: string;
begin
  //FValues.Sort;
  if (header <> '') then
  begin
    writeln(_out_, '#' + header);
  end;
  writeln(_out_, '#' + DateToStr(now));
  e := propertyNames;
  for index := 0 to e.Count - 1 do
  begin
    key := e[index];
    val := getProperty(string(key));
    key := saveConvert(string(key), True);
    val := saveConvert(val, False);
    writeln(_out_, key + '=' + val);
  end;
end;

class procedure TProperties.writeln(const bw: TStream; const s: string);
var
  line: string;
begin
  line := s + #13#10;
  bw.Write(line[1], length(line));
end;

function TProperties.getProperty(const key: string; const defaultValue: string): string;
begin
  Result := FValues.getValue(key);
  if Result = '' then
  begin
    if assigned(FDefaults) then
    begin
      Result := FDefaults.getProperty(key);
    end;
    if Result = '' then
    begin
      Result := defaultValue;
    end;
    setProperty(key, '');
  end;
  if Length(Result) <> 0 then
  begin
    if Result[1] = '@' then
    begin
      Result := getProperty(Result, defaultValue);
    end;
  end;
end;

function TProperties.getBoolean(const key: string; const defaultValue: boolean): boolean;
var
  Value: string;
begin
  Value := getProperty(key, '');
  if Value = '' then
  begin
    Result := defaultValue;
  end
  else
  begin
    Result := SameText(Value, 'true') or SameText(Value, 'yes') or SameText(Value, 'Y');
  end;
end;

function HexToInt(s: string): cardinal;
var
  P: PWideChar;
begin
  P      := PWideChar(S);
  Result := 0;
  while P^ <> #0 do
  begin
    Result := Result * 16;
    case P^ of
      '0'..'9':
      begin
        Inc(Result, Ord(P^) - Ord('0'));
      end;
      'a'..'f':
      begin
        Inc(Result, Ord(P^) - Ord('a') + 10);
      end;
      'A'..'F':
      begin
        Inc(Result, Ord(P^) - Ord('A') + 10);
      end;
    end;
    Inc(P);
  end;
end;

function TProperties.setBoolean(const key: string; const Value: boolean): string;
begin
  if Value then
  begin
    Result := setProperty(key, 'Y');
  end
  else
  begin
    Result := setProperty(key, 'N');
  end;
end;

function TProperties.setLongint(const key: string; const Value: longint): string;
var
  cifre: string;
  _mod_, _valore_: longint;
begin
  cifre    := '';
  _valore_ := Value;
  while _valore_ > 0 do
  begin
    _mod_    := (_valore_ mod 16);
    _valore_ := _valore_ div 16;
    if _mod_ < 10 then
    begin
      cifre := Chr(_mod_ + Ord('0')) + cifre;
    end
    else
    begin
      _mod_ := _mod_ - 10;
      cifre := Chr(_mod_ + Ord('A')) + cifre;
    end;
  end;
  Result := setProperty(key, cifre);
end;

function TProperties.getLongint(const key: string; const defaultValue: longint): longint;
var
  Value: string;
begin
  Value := getProperty(key, '');
  if Value = '' then
  begin
    Result := defaultValue;
  end
  else
  begin
    Result := HexToInt(Value);
  end;
end;

function TProperties.propertyNames: TStrings;
var
  index: longint;
begin
  if assigned(FDefaults) then
  begin
    Result := FDefaults.propertyNames;
  end
  else
  begin
    Result := TStringList.Create;
  end;

  for index := 0 to FValues.Count - 1 do
  begin
    Result.Add(FValues.getKey(index));
  end;
end;

class function TProperties.toHex(const nibble: longint): char;
begin
  Result := hexDigit[nibble];
end;

procedure TProperties.print(_out_: TStream; message: string);
var
  Valore: PWideChar;
begin
  Valore := PWideChar(message);
  while not (Valore^ = #0) do
  begin
    _out_.Write(Valore^, 1);
    Inc(Valore);
  end;
  _out_.Write(Valore^, 1);
end;

procedure TProperties.println(_out_: TStream; message: string);
begin
  print(_out_, message + #13#10);
end;

function TProperties.readLine(_in_: TStream; var EOF: boolean): string;
var
  c: char;
begin
  Result := '';
  if EOF then
  begin
    exit;
  end;
  EOF := _in_.Read(c, 1) <> 1;
  while (not (c in [#0, #13, #10])) and (not EOF) do
  begin
    if EOF then
    begin
      continue;
    end;
    Result := Result + c;
    EOF    := _in_.Read(c, 1) <> 1;
    if c = #13 then
    begin
      EOF := _in_.Read(c, 1) <> 1;
    end;
  end;
end;

procedure TProperties.loadResource(resourceName: string);
var
  R: TResourceStream;
const
  resourceType: PChar = 'properties';
begin
  R := TResourceStream.Create(0, resourceName, resourceType);
  load(r);
  R.Free;
end;

{ TItem }

procedure TItem.add(aKey, aValue: string);
begin
  if FNext = nil then
  begin
    FNext     := TItem.Create;
    FNext.Key := aKey;
    FNext.FValue := aValue;
  end
  else
  begin
    FNext.add(aKey, aValue);
  end;
end;

function TItem.Count: cardinal;
begin
  if FNext <> nil then
  begin
    Result := FNext.Count + 1;
  end
  else
  begin
    Result := 1;
  end;
end;

constructor TItem.Create;
begin
  inherited;
  FNext  := nil;
  FKey   := '';
  FValue := '';
end;

destructor TItem.Destroy;
begin
  if FNext <> nil then
  begin
    FreeAndNil(FNext);
  end;
  inherited;
end;

function TItem.getKey(index: integer): string;
begin
  if index = 0 then
  begin
    Result := FKey;
  end
  else if FNext <> nil then
  begin
    Result := FNext.getKey(index - 1);
  end;
end;

function TItem.getValue(aKey: string): string;
begin
  if FKey = aKey then
  begin
    Result := FValue;
  end
  else if FNext <> nil then
  begin
    Result := FNext.getValue(aKey);
  end
  else
  begin
    Result := '';
  end;
end;

procedure TItem.SetKey(const Value: string);
begin
  FKey := Value;
end;

procedure TItem.SetNext(const Value: TItem);
begin
  FNext := Value;
end;

procedure TItem.SetValue(const Value: string);
begin
  FValue := Value;
end;


{ TToken }

class function TToken.containsTokenWithValue(tokens: TTokenList; Value: string): boolean;
var
  i, sz: integer;

begin
  sz     := tokens.Count;
  Result := False;
  for i := 0 to sz - 1 do
  begin
    if (tokens[i].getValue() = Value) then
    begin
      Result := True;
    end;
  end;
end;

constructor TToken.Create(Value: string);
begin
  fValue := Value;
  fCount := 1;
end;

constructor TToken.Create(Value: string; aCount: int64);
begin
  fValue := Value;
  fCount := aCount;
end;

procedure TToken.increment;
begin
  Inc(fCount);
end;

function TToken.getCount: int64;
begin
  Result := fCount;
end;

function TToken.getValue: string;
begin
  Result := fValue;
end;

function TToken.Equals(Obj: TObject): boolean;
begin
  Result := inherited Equals(Obj);
end;

function TToken.GetHashCode: PtrInt;
begin
  Result := inherited GetHashCode;
end;

function TToken.ToString: ansistring;
begin
  Result := inherited ToString;
end;

{ TDurationFormatUtils }
const
  pattern_y: string = 'y';
  patternM: string = 'M';
  pattern_d: string = 'd';
  patternH: string = 'H';
  pattern_m: string = 'm';
  pattern_s: string = 's';
  patternS: string = 'S';

class function TDurationFormatUtils.lexx(format: string): TTokenList;
var
  ch: char;
  inLiteral: boolean;
  i, sz: integer;
  token, previous: TToken;
  buffer: string;
  Value: string;
begin
  Result := TTokenList.Create;
  inLiteral := False;
  buffer := '';
  previous := nil;
  sz := Length(format);
  for i := 1 to sz do
  begin
    ch := format[i];
    if inLiteral and (ch <> '''') then
    begin
      buffer += ch;
      Continue;
    end;
    Value := '';
    case (ch) of
      '''':
      begin
        if (inLiteral) then
        begin
          buffer    := '';
          inLiteral := False;
        end
        else
        begin
          buffer := '';
          Result.add(TToken.Create(buffer));
          inLiteral := True;
        end;
      end;
      'y':
      begin
        Value := pattern_y;
      end;
      'M':
      begin
        Value := patternM;
      end;
      'd':
      begin
        Value := pattern_d;
      end;
      'H':
      begin
        Value := patternH;
      end;
      'm':
      begin
        Value := pattern_m;
      end;
      's':
      begin
        Value := pattern_s;
      end;
      'S':
      begin
        Value := patternS;
      end;
      else
      begin
        buffer += ch;
      end;
    end;

    if (Value <> '') then
    begin
      if (previous <> nil) and (previous.getValue() = Value) then
      begin
        previous.increment();
      end
      else
      begin
        token := TToken.Create(Value);
        Result.add(token);
        previous := token;
      end;
      buffer := '';
    end;
  end;
end;

class function TDurationFormatUtils.format(tokens: TTokenList; years: Int32; months: Int32; days: Int32; hours: Int32; minutes: Int32; seconds: Int32; milliseconds: Int32; padWithZeros: boolean): string;
var
  i, sz: integer;
  token: TToken;
  Value: string;
  Count: int64;

  str: string;
begin
  Result := '';
  sz     := tokens.Count;
  for i := 0 to sz - 1 do
  begin
    token := tokens[i];
    Value := token.getValue();
    Count := token.getCount();
    if (Value = pattern_y) then
    begin
      str := IntToStr(years);
      if padWithZeros then
      begin
        str := AddChar('0', str, Count);
      end;
      Result += str;
    end
    else if (Value = patternM) then
    begin
      str := IntToStr(months);
      if padWithZeros then
      begin
        str := AddChar('0', str, Count);
      end;
      Result += str;
    end
    else if (Value = pattern_d) then
    begin
      str := IntToStr(days);
      if padWithZeros then
      begin
        str := AddChar('0', str, Count);
      end;
      Result += str;
    end
    else if (Value = patternH) then
    begin
      str := IntToStr(hours);
      if padWithZeros then
      begin
        str := AddChar('0', str, Count);
      end;
      Result += str;
    end
    else if (Value = pattern_m) then
    begin
      str := IntToStr(minutes);
      if padWithZeros then
      begin
        str := AddChar('0', str, Count);
      end;
      Result += str;
    end
    else if (Value = pattern_s) then
    begin
      str := IntToStr(seconds);
      if padWithZeros then
      begin
        str := AddChar('0', str, Count);
      end;
      Result += str;
    end
    else if (Value = patternS) then
    begin
      str := copy(IntToStr(milliseconds), 1);
      if padWithZeros then
      begin
        str := AddChar('0', str, Count);
      end;
      Result += str;
    end
    else
    begin
      Result += Value;
    end;
  end;
end;

class function TDurationFormatUtils.formatDuration(durationMillis: int64; tl: TTokenList; padWithZeros: boolean): string;
var
  days: int32;
  hours: int32;
  minutes: int32;
  seconds: int32;
  milliseconds: int32;

begin
  days    := 0;
  hours   := 0;
  minutes := 0;
  seconds := 0;
  milliseconds := 0;
  if (TToken.containsTokenWithValue(tl, pattern_d)) then
  begin
    days := (durationMillis div MILLIS_PER_DAY);
    durationMillis := durationMillis - (int64(days) * MILLIS_PER_DAY);
  end;
  if (TToken.containsTokenWithValue(tl, patternH)) then
  begin
    hours := (durationMillis div MILLIS_PER_HOUR);
    durationMillis := durationMillis - (int64(hours) * MILLIS_PER_HOUR);
  end;
  if (TToken.containsTokenWithValue(tl, pattern_m)) then
  begin
    minutes := (durationMillis div MILLIS_PER_MINUTE);
    durationMillis := durationMillis - (int64(minutes) * MILLIS_PER_MINUTE);
  end;
  if (TToken.containsTokenWithValue(tl, pattern_s)) then
  begin
    seconds := (durationMillis div MILLIS_PER_SECOND);
    durationMillis := durationMillis - (int64(seconds) * MILLIS_PER_SECOND);
  end;
  if (TToken.containsTokenWithValue(tl, patternS)) then
  begin
    milliseconds := durationMillis;
  end;
  Result := format(tl, 0, 0, days, hours, minutes, seconds, milliseconds, padWithZeros);
end;



class function TDurationFormatUtils.formatDuration(durationMillis: int64; aFormatString: string; padWithZeros: boolean): string;
var
  tokens: TTokenList;
begin
  tokens := lexx(aFormatString);
  Result := formatDuration(durationMillis, tokens, padWithZeros);
  tokens.Free;
end;

constructor TDurationFormatUtils.Create;
begin

end;

{ TPrintStream }

function TPrintStream.checkError: boolean;
begin
  Result := fTrouble;
end;

procedure TPrintStream.Close;
begin
  if not fClosing then
  begin
    fClosing := True;
    if (fOwner) then
    begin
      FStream.Free;
    end;
  end;
end;

constructor TPrintStream.Create(aStream: TStream; const autoFlush: boolean; const aOwner: boolean);
begin
  inherited Create;
  FStream    := aStream;
  fAutoFlush := autoFlush;
  fOwner     := aOwner;
  FStream.Seek(0, soEnd);
end;

destructor TPrintStream.Destroy;
begin
  Close();
  inherited Destroy;
end;

procedure TPrintStream.ensureOpen;
begin
  assert(Assigned(FStream));
end;

procedure TPrintStream.flush;
begin
  if FStream is THandleStream then
  begin
    {$IFDEF WINDOWS}
    Windows.FlushFileBuffers((FStream as THandleStream).Handle);
    {$ENDIF}
  end;
end;

procedure TPrintStream.newLine;
begin
  Write(system.LineEnding);
  if fAutoFlush then
  begin
    flush;
  end;
end;

function TPrintStream.print(const Value: integer): TPrintStream;
begin
  Write(SysUtils.Format('%d', [Value]));
  Result := self;
end;

function TPrintStream.print(const Value: int64): TPrintStream;
begin
  Write(SysUtils.Format('%d', [Value]));
  Result := self;
end;

function TPrintStream.print(const Value: single): TPrintStream;
begin
  Write(SysUtils.Format('%g', [Value]));
  Result := self;
end;

function TPrintStream.print(const Value: boolean): TPrintStream;
begin
  Write(BoolToStr(Value, True));
  Result := self;
end;

function TPrintStream.print(const Value: char): TPrintStream;
begin
  Write(Value);
  Result := self;
end;

function TPrintStream.print(const Value: double): TPrintStream;
begin
  Result := print(SysUtils.Format('%g', [Value]));
end;

function TPrintStream.print(const Value: Pointer): TPrintStream;
begin
  Write(SysUtils.Format('%f', [Value]));
  Result := self;
end;

function TPrintStream.print(const Value: string): TPrintStream;
begin
  Write(Value);
  Result := self;
end;

function TPrintStream.print(const Value: array of char): TPrintStream;
begin
  Result := print(string(Value));
end;

function TPrintStream.print(const Value: PChar): TPrintStream;
begin
  Result := print(string(Value));
end;

procedure TPrintStream.setError;
begin
  fTrouble := True;
end;

procedure TPrintStream.Write(const buf: array of char);
begin
  ensureOpen;
  FStream.Write(buf[0], length(buf));
end;

procedure TPrintStream.Write(const s: string);
begin
  ensureOpen;
  FStream.Write(s[1], Length(S));
end;

function TPrintStream.print(const Value: WideString): TPrintStream;
begin
  Result := print(WideCharToString(PWideChar(Value)));
end;

function TPrintStream.println: TPrintStream;
begin
  newLine;
  Result := self;
end;


{ TCronometro }

function TCronometro.start: TDateTime;
begin
  Result     := now;
  FStartTime := Result;
end;

function TCronometro.stop: TDateTime;
begin
  Result     := now;
  FStartTime := EncodeTime(0, 0, 0, 0);
end;

function TCronometro.getElapsed: int64;
begin
  Result := MilliSecondsBetween(Now, FStartTime);
end;

constructor TCronometro.Create;
begin
  start;
end;

class function TCronometro.format(aElapsedTime: int64): string;
var
  TL: TTokenList;
begin
  TL := TTokenList.Create;
  TL.Add(TToken.Create('H', 2));
  TL.Add(TToken.Create(':', 1));
  TL.Add(TToken.Create('m', 2));
  TL.Add(TToken.Create(':', 1));
  TL.Add(TToken.Create('s', 2));
  TL.Add(TToken.Create('.', 1));
  TL.Add(TToken.Create('S', 4));

  Result := TDurationFormatUtils.formatDuration(aElapsedTime, TL, True);
  TL.Free;
end;


const
  ParseBufSize = 4096;
  LastSpecialToken = 5;

  TokNames: array[0..LastSpecialToken] of string =
    (
    'EOF',
    'Symbol',
    'String',
    'Integer',
    'Float',
    'WideString'
    );

function TParser.GetTokenName(aTok: char): string;
begin
  if Ord(aTok) <= LastSpecialToken then
  begin
    Result := TokNames[Ord(aTok)];
  end
  else
  begin
    Result := aTok;
  end;
end;

procedure TParser.LoadBuffer;
var
  BytesRead: integer;
begin
  BytesRead := FStream.Read(FBuf^, ParseBufSize);
  FBuf[BytesRead] := #0;
  Inc(FDeltaPos, BytesRead);
  FPos    := 0;
  FBufLen := BytesRead;
  FEofReached := BytesRead = 0;
end;

procedure TParser.CheckLoadBuffer; inline;
begin
  if fBuf[fPos] = #0 then
  begin
    LoadBuffer;
  end;
end;

procedure TParser.ProcessChar; inline;
begin
  fLastTokenStr := fLastTokenStr + fBuf[fPos];
  Inc(fPos);
  CheckLoadBuffer;
end;

function TParser.IsNumber: boolean; inline;

begin
  Result := fBuf[fPos] in ['0'..'9'];
end;

function TParser.IsHexNum: boolean; inline;

begin
  Result := fBuf[fPos] in ['0'..'9', 'A'..'F', 'a'..'f'];
end;

function TParser.IsAlpha: boolean; inline;

begin
  Result := fBuf[fPos] in ['_', 'A'..'Z', 'a'..'z'];
end;

function TParser.IsAlphaNum: boolean; inline;

begin
  Result := IsAlpha or IsNumber;
end;

function TParser.GetHexValue(c: char): byte; inline;

begin
  case c of
    '0'..'9':
    begin
      Result := Ord(c) - $30;
    end;
    'A'..'F':
    begin
      Result := Ord(c) - $37;
    end; //-$41+$0A
    'a'..'f':
    begin
      Result := Ord(c) - $57;
    end; //-$61+$0A
  end;
end;

function TParser.GetAlphaNum: string;
begin
  Result := '';
  while IsAlphaNum do
  begin
    Result := Result + fBuf[fPos];
    Inc(fPos);
    CheckLoadBuffer;
  end;
end;

procedure TParser.HandleNewLine;
begin
  if fBuf[fPos] = #13 then //CR
  begin
    Inc(fPos);
    CheckLoadBuffer;
  end;
  if fBuf[fPos] = #10 then
  begin
    Inc(fPos); //CR LF or LF
    CheckLoadBuffer;
  end;
  Inc(fSourceLine);
  fDeltaPos := -(fPos - 1);
end;

procedure TParser.SkipBOM;
var
  i: integer;
  bom: string[3];

  backup: integer;
begin
  i      := 1;
  bom    := '   ';
  backup := fPos;
  while (fBuf[fPos] in [#$BB, #$BF, #$EF]) and (i <= 3) do
  begin
    bom[i] := fBuf[fPos];
    Inc(fPos);
    CheckLoadBuffer;
    Inc(i);
  end;
  if (bom <> (#$EF + #$BB + #$BF)) then
  begin
    fPos := backup;
  end;
end;

procedure TParser.SkipSpaces;
begin
  while fBuf[fPos] in [' ', #9] do
  begin
    Inc(fPos);
    CheckLoadBuffer;
  end;
end;

procedure TParser.SkipWhitespace;
begin
  while True do
  begin
    case fBuf[fPos] of
      ' ', #9:
      begin
        SkipSpaces;
      end;
      #10, #13:
      begin
        HandleNewLine;
      end
      else
      begin
        break;
      end;
    end;
  end;
end;

procedure TParser.HandleNumber;
begin
  fToken := toNumber;
  fLastTokenStr := '';
  while fBuf[fPos] in ['0'..'9'] do
  begin
    fLastTokenStr += fBuf[fPos];
    Inc(fPos);
    CheckLoadBuffer;
  end;
end;

procedure TParser.HandleSymbol;
begin
  CheckLoadBuffer;
  fToken := toSymbol;
  fLastTokenStr := fBuf[fPos];
  Inc(fPos);
end;

procedure TParser.HandleToken;
begin
  CheckLoadBuffer;
  fLastTokenStr := '';
  while fBuf[FPos] in ['A'..'Z', 'a'..'z', '0'..'9', '_', '-'] do
  begin
    fLastTokenStr += fBuf[fPos];
    Inc(fPos);
    CheckLoadBuffer;
  end;
end;

constructor TParser.Create(Stream: TStream);
begin
  fStream   := Stream;
  fBuf      := GetMem(ParseBufSize + 1);
  fBufLen   := 0;
  fPos      := 0;
  fDeltaPos := 1;
  fSourceLine := 1;
  fEofReached := False;
  fLastTokenStr := '';
  fLastTokenWStr := '';
  fFloatType := #0;
  fToken    := #0;
  LoadBuffer;
  SkipBom;
end;

destructor TParser.Destroy;
begin
  fStream.Position := SourcePos;
  FreeMem(fBuf);
end;

function TParser.NextToken: char;
begin
  CheckLoadBuffer;
  SkipWhiteSpace;
  if fEofReached then
  begin
    fToken := toEOF;
    fLastTokenStr := '';
  end
  else
  begin
    case fBuf[fPos] of
      '0'..'9':
      begin
        HandleNumber;
      end;
      'A'..'Z':
      begin
        HandleToken;
      end;
      'a'..'z':
      begin
        HandleToken;
      end;
      else
      begin
        HandleSymbol;
      end;
    end;
  end;
  Result := fToken;
end;

function TParser.SourcePos: longint;
begin
  Result := fStream.Position - fBufLen + fPos;
end;

procedure TParser.runToEol;
begin
  CheckLoadBuffer;
  fLastTokenStr := '';
  while not (fBuf[FPos] in [#13, #10, #0]) do
  begin
    fLastTokenStr += fBuf[fPos];
    Inc(fPos);
    CheckLoadBuffer;
  end;
end;

function TParser.getTokenString: string;
begin
  Result := fLastTokenStr;
end;


end.
