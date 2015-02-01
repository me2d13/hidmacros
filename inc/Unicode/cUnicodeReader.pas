{                                                                              }
{                        Unicode Reader class v3.05                            }
{                                                                              }
{             This unit is copyright © 2002-2004 by David J Butler             }
{                                                                              }
{                  This unit is part of Delphi Fundamentals.                   }
{                Its original file name is cUnicodeReader.pas                  }
{       The latest version is available from the Fundamentals home page        }
{                     http://fundementals.sourceforge.net/                     }
{                                                                              }
{                I invite you to use this unit, free of charge.                }
{        I invite you to distibute this unit, but it must be for free.         }
{             I also invite you to contribute to its development,              }
{             but do not distribute a modified copy of this file.              }
{                                                                              }
{          A forum is available on SourceForge for general discussion          }
{             http://sourceforge.net/forum/forum.php?forum_id=2117             }
{                                                                              }
{                                                                              }
{ Description:                                                                 }
{   Unicode reader class.                                                      }
{                                                                              }
{ Revision history:                                                            }
{   19/04/2002  0.01  Initial version                                          }
{   28/10/2002  3.02  Refactored for Fundamentals 3.                           }
{   29/10/2002  3.03  Bug fixes and improvements.                              }
{   05/11/2002  3.04  Improved buffer handling.                                }
{   02/01/2004  3.05  Changed reader's block size to 64K as suggested by Eb.   }
{                                                                              }

{$INCLUDE ..\cDefines.inc}
unit cUnicodeReader;

interface

uses
  { Fundamentals }
  cUtils,
  cReaders,
  cUnicodeChar,
  cUnicodeCodecs,
  cUnicode;



{                                                                              }
{ TUnicodeReader                                                               }
{                                                                              }
type
  TUnicodeReader = class
  protected
    FReader      : AReader;
    FReaderOwner : Boolean;
    FReaderPos   : Int64;
    FCodec       : TCustomUnicodeCodec;
    FCodecOwner  : Boolean;
    FBuffer      : WideString;
    FBufPos      : Integer;
    FBufLen      : Integer;
    FRawBuf      : Pointer;
    FRawSize     : Integer;

    procedure ReadError;
    function  BufferChars(const Count: Integer): Integer;
    function  GetBuffer(const Count: Integer): Boolean;

  public
    constructor Create(const Reader: AReader;
                const ReaderOwner: Boolean = True;
                const Codec: TCustomUnicodeCodec = nil;
                const CodecOwner: Boolean = True);
    destructor Destroy; override;

    property  Codec: TCustomUnicodeCodec read FCodec;
    property  CodecOwner: Boolean read FCodecOwner write FCodecOwner;

    procedure Reset;
    function  EOF: Boolean;

    function  ReadChar: WideChar;
    function  ReadWide(const Buf: PWideChar; const Len: Integer): Integer;
    function  ReadWideStr(const Len: Integer): WideString;
    function  ReadUTF8Str(const Len: Integer): String;

    procedure Skip(const Count: Integer);
    function  SkipAll(const CharMatchFunc: WideCharMatchFunction): Integer;

    function  MatchChar(const CharMatchFunc: WideCharMatchFunction;
              const Skip: Boolean): Boolean;
    function  MatchWideChar(const Ch: WideChar; const Skip: Boolean): Boolean;

    function  MatchAnsiStr(const S: String; const CaseSensitive: Boolean;
              const Skip: Boolean): Boolean;
    function  MatchAnsiStrDelimited(const S: String;
              const CaseSensitive: Boolean;
              const Delimiter: WideCharMatchFunction;
              const Skip: Boolean): Boolean;

    function  MatchChars(const CharMatchFunc: WideCharMatchFunction): Integer;
    function  MatchAnsiChars(const C: CharSet): Integer;

    function  LocateAnsiChar(const C: CharSet;
              const Optional: Boolean = False): Integer;
    function  LocateAnsiStr(const S: String; const CaseSensitive: Boolean;
              const Optional: Boolean = False): Integer;

    function  PeekChar: WideChar;
    function  SkipAndPeek(var Ch: WideChar): Boolean;
    function  GetPeekBuffer(const Len: Integer; var Buffer: PWideChar): Integer;

    function  ReadChars(const CharMatchFunc: WideCharMatchFunction): WideString;
    function  ReadAnsiChars(const C: CharSet): String;

    function  SkipToAnsiChar(const C: CharSet;
              const SkipDelimiter: Boolean): Integer;
    function  ReadToAnsiChar(const C: CharSet;
              const SkipDelimiter: Boolean = False): WideString;
    function  ReadUTF8StrToAnsiChar(const C: CharSet;
              const SkipDelimiter: Boolean = False): String;

    function  ReadToAnsiStr(const S: String;
              const CaseSensitive: Boolean = True;
              const SkipDelimiter: Boolean = False): WideString;
    function  ReadUTF8StrToAnsiStr(const S: String;
              const CaseSensitive: Boolean = True;
              const SkipDelimiter: Boolean = False): WideString;
  end;
  EUnicodeReader = class(EUnicode);
  EUnicodeReaderReadError = class(EUnicodeReader);



{                                                                              }
{ TUnicodeMemoryReader                                                         }
{                                                                              }
type
  TUnicodeMemoryReader = class(TUnicodeReader)
  public
    constructor Create(const Data: Pointer; const Size: Integer;
                const Codec: TCustomUnicodeCodec = nil;
                const CodecOwner: Boolean = True);
  end;



{                                                                              }
{ TUnicodeFileReader                                                           }
{                                                                              }
type
  TUnicodeFileReader = class(TUnicodeReader)
  public
    constructor Create(const FileName: String;
                const Codec: TCustomUnicodeCodec = nil;
                const CodecOwner: Boolean = True);
  end;



implementation

uses
  { Delphi }
  SysUtils;



resourcestring
  RSReadError = 'Read error';



{                                                                              }
{ TUnicodeReader                                                               }
{                                                                              }
const
  ReaderBlockSize = 65536; // 64K

constructor TUnicodeReader.Create(const Reader: AReader;
    const ReaderOwner: Boolean;
    const Codec: TCustomUnicodeCodec;
    const CodecOwner: Boolean);
begin
  inherited Create;
  Assert(Assigned(Reader));
  FReader := Reader;
  FReaderOwner := ReaderOwner;
  FReaderPos := Reader.Position;
  FCodec := Codec;
  FCodecOwner := CodecOwner;
  GetMem(FRawBuf, ReaderBlockSize);
end;

destructor TUnicodeReader.Destroy;
begin
  if Assigned(FRawBuf) then
    FreeMem(FRawBuf);
  if FReaderOwner then
    FreeAndNil(FReader);
  if FCodecOwner then
    FreeAndNil(FCodec);
  inherited Destroy;
end;

procedure TUnicodeReader.ReadError;
begin
  raise EUnicodeReaderReadError.Create(RSReadError);
end;

procedure TUnicodeReader.Reset;
begin
  FReader.Position := FReaderPos;
  FBufPos := 0;
  FBufLen := 0;
  // Free excessively large buffer, keep part of it for re-use
  if Length(FBuffer) > 4 * ReaderBlockSize then
    SetLength(FBuffer, 4 * ReaderBlockSize);
end;

function TUnicodeReader.EOF: Boolean;
begin
  if FBufPos < FBufLen then
    Result := False
  else
    Result := FReader.EOF;
end;

function TUnicodeReader.BufferChars(const Count: Integer): Integer;
var I, J, L, M, N: Integer;
    P: PByte;
    Q: PWideChar;
begin
  // Check available characters
  Result := FBufLen - FBufPos;
  if Result >= Count then
    exit;
  L := Length(FBuffer);
  if L > 0 then
    begin
      // Reorganise buffer
      if Result <= 0 then // buffer empty
        begin
          // move pointer to front
          FBufPos := 0;
          FBufLen := 0;
        end else
      if (Result <= ReaderBlockSize div 16) or // buffer is nearly empty; or
         (FBufPos >= 4 * ReaderBlockSize) then // buffer has too much unused space at front
        begin
          // Move data to front
          Q := Pointer(FBuffer);
          Inc(Q, FBufPos);
          Move(Q^, Pointer(FBuffer)^, Result * Sizeof(WideChar));
          FBufPos := 0;
          FBufLen := Result;
        end;
    end;
  // Fill unicode buffer
  Repeat
    // Fill raw character buffer
    P := FRawBuf;
    Inc(P, FRawSize);
    J := FReader.Read(P^, ReaderBlockSize - FRawSize);
    if J <= 0 then // eof
      exit;
    Inc(FRawSize, J);
    // Decode to unicode buffer
    if Assigned(FCodec) then
      begin
        // Decode raw buffer using codec
        P := FRawBuf;
        J := FRawSize;
        L := Length(FBuffer) - FBufLen;
        Repeat
          if L < ReaderBlockSize then
            begin
              // grow unicode buffer to fit at least one raw buffer
              L := ReaderBlockSize;
              SetLength(FBuffer, FBufLen + L);
            end;
          Q := Pointer(FBuffer);
          Inc(Q, FBufLen);
          FCodec.Decode(P, J, Q, L * Sizeof(WideChar), M, N);
          Inc(P, M);
          Dec(J, M);
          Inc(FBufLen, N);
          Dec(L, N);
        Until (J <= 0) or (L > 0);
        I := FRawSize - J;
      end
    else
      begin
        // read raw 16-bit unicode
        I := FRawSize div Sizeof(WideChar);
        L := Length(FBuffer) - FBufLen;
        if L < I then
          begin
            L := I;
            SetLength(FBuffer, FBufLen + L);
          end;
        Q := Pointer(FBuffer);
        Inc(Q, FBufLen);
        Inc(FBufLen, I);
        I := I * Sizeof(WideChar);
        Move(FRawBuf^, Q^, I);
      end;
    // Move undecoded raw data to front of buffer
    if I < FRawSize then
      begin
        Move(P^, FRawBuf^, FRawSize - I);
        Dec(FRawSize, I);
      end
    else
      FRawSize := 0;
    // Check if enough characters have been buffered
    Result := FBufLen - FBufPos;
  Until Result >= Count;
end;

function TUnicodeReader.GetBuffer(const Count: Integer): Boolean;
begin
  Result := FBufLen - FBufPos >= Count;
  if Result then
    exit;
  Result := BufferChars(Count) >= Count;
end;

function TUnicodeReader.ReadWide(const Buf: PWideChar;
    const Len: Integer): Integer;
var P: PWideChar;
begin
  if Len <= 0 then
    begin
      Result := 0;
      exit;
    end;
  // buffer
  Result := FBufLen - FBufPos;
  if Result < Len then
    Result := BufferChars(Len);
  if Result > Len then
    Result := Len;
  // read
  P := Pointer(FBuffer);
  Inc(P, FBufPos);
  Move(P^, Buf^, Sizeof(WideChar) * Result);
  Inc(FBufPos, Result);
end;

function TUnicodeReader.ReadWideStr(const Len: Integer): WideString;
var L: Integer;
    P: PWideChar;
begin
  if Len <= 0 then
    begin
      Result := '';
      exit;
    end;
  // buffer
  L := FBufLen - FBufPos;
  if L < Len then
    L := BufferChars(Len);
  if L > Len then
    L := Len;
  // read
  P := Pointer(FBuffer);
  Inc(P, FBufPos);
  SetLength(Result, L);
  Move(P^, Pointer(Result)^, Sizeof(WideChar) * L);
  Inc(FBufPos, L);
end;

function TUnicodeReader.ReadUTF8Str(const Len: Integer): String;
var L: Integer;
    P: PWideChar;
begin
  if Len <= 0 then
    begin
      Result := '';
      exit;
    end;
  // buffer
  L := FBufLen - FBufPos;
  if L < Len then
    L := BufferChars(Len);
  if L > Len then
    L := Len;
  // read
  P := Pointer(FBuffer);
  Inc(P, FBufPos);
  Result := WideBufToUTF8String(P, L);
  Inc(FBufPos, L);
end;

procedure TUnicodeReader.Skip(const Count: Integer);
begin
  // buffer
  if Count <= 0 then
    exit;
  if FBufLen - FBufPos < Count then
    if not GetBuffer(Count) then
      ReadError;
  // skip
  Inc(FBufPos, Count);
end;

function TUnicodeReader.SkipAll(const CharMatchFunc: WideCharMatchFunction): Integer;
var P: PWideChar;
    N, I: Integer;
begin
  Result := 0;
  // buffer
  N := FBufLen - FBufPos;
  if N <= 0 then
    N := BufferChars(1);
  Repeat
    if N <= 0 then // eof
      exit;
    // skip
    P := Pointer(FBuffer);
    Inc(P, FBufPos);
    For I := 1 to N do
      if not CharMatchFunc(P^) then
        exit else
        begin
          Inc(Result);
          Inc(FBufPos);
          Inc(P);
        end;
    // buffer more
    N := BufferChars(1);
  Until False;
end;

function TUnicodeReader.MatchChar(const CharMatchFunc: WideCharMatchFunction;
    const Skip: Boolean): Boolean;
var P: PWideChar;
begin
  // buffer
  if FBufPos >= FBufLen then
    if BufferChars(1) <= 0 then // eof
      begin
        Result := False;
        exit;
      end;
  // match
  P := Pointer(FBuffer);
  Inc(P, FBufPos);
  Result := CharMatchFunc(P^);
  // skip
  if Skip and Result then
    Inc(FBufPos);
end;

function TUnicodeReader.MatchWideChar(const Ch: WideChar;
    const Skip: Boolean): Boolean;
var P: PWideChar;
begin
  // buffer
  if FBufPos >= FBufLen then
    if BufferChars(1) <= 0 then // eof
      begin
        Result := False;
        exit;
      end;
  // match
  P := Pointer(FBuffer);
  Inc(P, FBufPos);
  Result := P^ = Ch;
  // skip
  if Skip and Result then
    Inc(FBufPos);
end;

function TUnicodeReader.MatchAnsiStr(const S: String;
    const CaseSensitive: Boolean; const Skip: Boolean): Boolean;
var L: Integer;
    P: PWideChar;
begin
  L := Length(S);
  if L = 0 then
    begin
      Result := False;
      exit;
    end;
  // buffer
  if FBufLen - FBufPos < L then
    if BufferChars(L) < L then // eof
      begin
        Result := False;
        exit;
      end;
  // match
  P := Pointer(FBuffer);
  Inc(P, FBufPos);
  Result := WidePMatchAnsiStr(S, P, CaseSensitive);
  // skip
  if Skip and Result then
    Inc(FBufPos, L);
end;

function TUnicodeReader.MatchAnsiStrDelimited(const S: String;
    const CaseSensitive: Boolean; const Delimiter: WideCharMatchFunction;
    const Skip: Boolean): Boolean;
var L: Integer;
    P: PWideChar;
begin
  L := Length(S);
  // buffer
  if FBufLen - FBufPos < L + 1 then
    if BufferChars(L + 1) < L + 1 then // eof
      begin
        Result := False;
        exit;
      end;
  // match
  P := Pointer(FBuffer);
  Inc(P, FBufPos);
  Result := WidePMatchAnsiStr(S, P, CaseSensitive);
  if not Result then
    exit;
  Inc(P, L);
  Result := Delimiter(P^);
  // skip
  if Skip and Result then
    Inc(FBufPos, L);
end;

function TUnicodeReader.MatchChars(const CharMatchFunc: WideCharMatchFunction): Integer;
var P: PWideChar;
    N, I: Integer;
begin
  Result := 0;
  // buffer
  N := FBufLen - FBufPos;
  if N <= 0 then
    N := BufferChars(1);
  Repeat
    if N < Result + 1 then // eof
      exit;
    // match
    P := Pointer(FBuffer);
    Inc(P, FBufPos + Result);
    For I := Result + 1 to N do
      if not CharMatchFunc(P^) then
        exit else
        begin
          Inc(Result);
          Inc(P);
        end;
    // buffer more
    N := BufferChars(Result + 1);
  Until False;
end;

function TUnicodeReader.MatchAnsiChars(const C: CharSet): Integer;
var P: PWideChar;
    N, I: Integer;
begin
  Result := 0;
  // buffer
  N := FBufLen - FBufPos;
  if N <= 0 then
    N := BufferChars(1);
  Repeat
    if N < Result + 1 then // eof
      exit;
    // match
    P := Pointer(FBuffer);
    Inc(P, FBufPos + Result);
    For I := Result + 1 to N do
      if (Ord(P^) > $FF) or not (Char(Byte(P^)) in C) then
        exit else
        begin
          Inc(Result);
          Inc(P);
        end;
    // buffer more
    N := BufferChars(Result + 1);
  Until False;
end;

function TUnicodeReader.LocateAnsiChar(const C: CharSet;
    const Optional: Boolean): Integer;
var P: PWideChar;
    N, I: Integer;
    V: Word;
begin
  Result := 0;
  // buffer
  N := FBufLen - FBufPos;
  if N <= 0 then
    N := BufferChars(1);
  Repeat
    if N < Result + 1 then
      begin
        // eof
        if Optional then
          Result := N else
          Result := -1;
        exit;
      end;
    // locate
    P := Pointer(FBuffer);
    Inc(P, FBufPos + Result);
    For I := Result + 1 to N do
      begin
        V := Ord(P^);
        if (V <= $FF) and (Char(V) in C) then
          // found
          exit;
        Inc(Result);
        Inc(P);
      end;
    // buffer more
    N := BufferChars(Result + 1);
  Until False;
end;

function TUnicodeReader.LocateAnsiStr(const S: String;
    const CaseSensitive: Boolean;
    const Optional: Boolean): Integer;
var P: PWideChar;
    M, N, I: Integer;
begin
  Result := 0;
  M := Length(S);
  if M = 0 then
    exit;
  // buffer
  N := FBufLen - FBufPos;
  if N < M then
    N := BufferChars(M);
  Repeat
    if N < Result + M then
      begin
        // eof
        if Optional then
          Result := N else
          Result := -1;
        exit;
      end;
    P := Pointer(FBuffer);
    Inc(P, FBufPos + Result);
    For I := Result + 1 to N - M + 1 do
      if WidePMatchAnsiStr(S, P, CaseSensitive) then
        // found
        exit else
        begin
          Inc(Result);
          Inc(P);
        end;
    // buffer more characters
    N := BufferChars(Result + M);
  Until False;
end;

function TUnicodeReader.PeekChar: WideChar;
var P: PWideChar;
begin
  // buffer
  if FBufPos >= FBufLen then
    if not GetBuffer(1) then
      ReadError;
  // peek
  P := Pointer(FBuffer);
  Inc(P, FBufPos);
  Result := P^;
end;

function TUnicodeReader.GetPeekBuffer(const Len: Integer;
    var Buffer: PWideChar): Integer;
var P: PWideChar;
begin
  // Result returns the number of wide characters in Buffer.
  // Buffer points to the actual data. The buffer is only valid until the next
  // call to the reader.
  Result := BufferChars(Len);
  if Result = 0 then
    Buffer := nil else
    begin
      P := Pointer(FBuffer);
      Inc(P, FBufPos);
      Buffer := P;
    end;
end;

function TUnicodeReader.ReadChar: WideChar;
var P: PWideChar;
    O: Integer;
begin
  // buffer
  O := FBufPos;
  if O >= FBufLen then
    if GetBuffer(1) then
      O := FBufPos else
      ReadError;
  // read
  P := Pointer(FBuffer);
  Inc(P, O);
  Result := P^;
  Inc(FBufPos);
end;

function TUnicodeReader.SkipAndPeek(var Ch: WideChar): Boolean;
var P: PWideChar;
    C: Integer;
begin
  // Skip
  C := FBufLen - FBufPos;
  if C >= 2 then
    begin
      Inc(FBufPos);
      Result := True;
    end else
    begin
      Result := GetBuffer(2);
      if FBufPos < FBufLen then
        Inc(FBufPos);
    end;
  if Result then
    begin
      // Peek
      P := Pointer(FBuffer);
      Inc(P, FBufPos);
      Ch := P^;
    end else
    Ch := WideChar(#0);
end;

function TUnicodeReader.ReadChars(const CharMatchFunc: WideCharMatchFunction): WideString;
var P: PWideChar;
    L: Integer;
begin
  // calculate length
  L := MatchChars(CharMatchFunc);
  if L = 0 then
    Result := '' else
    begin
      // read
      SetLength(Result, L);
      P := Pointer(FBuffer);
      Inc(P, FBufPos);
      Move(P^, Pointer(Result)^, Sizeof(WideChar) * L);
      Inc(FBufPos, L);
    end;
end;

function TUnicodeReader.ReadAnsiChars(const C: CharSet): String;
var P : PWideChar;
    L : Integer;
begin
  // calculate length
  L := MatchAnsiChars(C);
  if L = 0 then
    Result := '' else
    begin
      // read
      SetLength(Result, L);
      P := Pointer(FBuffer);
      Inc(P, FBufPos);
      Result := WideToLongString(P, L);
      Inc(FBufPos, L);
    end;
end;

function TUnicodeReader.SkipToAnsiChar(const C: CharSet;
    const SkipDelimiter: Boolean): Integer;
var L: Integer;
begin
  // locate
  L := LocateAnsiChar(C, False);
  if L = 0 then
    Result := 0 else
    begin
      // skip characters
      if L < 0 then
        Result := FBufLen - FBufPos else
        Result := L;
      Inc(FBufPos, Result);
    end;
  // skip delimiter
  if (L >= 0) and SkipDelimiter then
    Inc(FBufPos);
end;

function TUnicodeReader.ReadToAnsiChar(const C: CharSet;
    const SkipDelimiter: Boolean): WideString;
var L, M: Integer;
begin
  // locate
  L := LocateAnsiChar(C, False);
  if L = 0 then
    Result := '' else
    begin
      // read
      if L < 0 then
        M := FBufLen - FBufPos else
        M := L;
      Result := ReadWideStr(M);
    end;
  // skip delimiter
  if (L >= 0) and SkipDelimiter then
    Inc(FBufPos);
end;

function TUnicodeReader.ReadUTF8StrToAnsiChar(const C: CharSet;
    const SkipDelimiter: Boolean): String;
var L, M: Integer;
begin
  // locate
  L := LocateAnsiChar(C, False);
  if L = 0 then
    Result := '' else
    begin
      // read
      if L < 0 then
        M := FBufLen - FBufPos else
        M := L;
      Result := ReadUTF8Str(M);
    end;
  // skip delimiter
  if (L >= 0) and SkipDelimiter then
    Inc(FBufPos);
end;

function TUnicodeReader.ReadToAnsiStr(const S: String;
    const CaseSensitive: Boolean; const SkipDelimiter: Boolean): WideString;
var L, M: Integer;
begin
  // locate
  L := LocateAnsiStr(S, CaseSensitive, False);
  if L = 0 then
    Result := '' else
    begin
      // read
      if L < 0 then
        M := FBufLen - FBufPos else
        M := L;
      Result := ReadWideStr(M);
    end;
  // skip delimiter
  if (L >= 0) and SkipDelimiter then
    Inc(FBufPos, Length(S));
end;

function TUnicodeReader.ReadUTF8StrToAnsiStr(const S: String;
    const CaseSensitive: Boolean; const SkipDelimiter: Boolean): WideString;
var L, M: Integer;
begin
  // locate
  L := LocateAnsiStr(S, CaseSensitive, False);
  if L = 0 then
    Result := '' else
    begin
      // read
      if L < 0 then
        M := FBufLen - FBufPos else
        M := L;
      Result := ReadUTF8Str(M);
    end;
  // skip delimiter
  if (L >= 0) and SkipDelimiter then
    Inc(FBufPos, Length(S));
end;



{                                                                              }
{ TUnicodeMemoryReader                                                         }
{                                                                              }
constructor TUnicodeMemoryReader.Create(const Data: Pointer; const Size: Integer;
    const Codec: TCustomUnicodeCodec; const CodecOwner: Boolean);
begin
  inherited Create(TMemoryReader.Create(Data, Size), True, Codec, CodecOwner);
end;



{                                                                              }
{ TUnicodeFileReader                                                           }
{                                                                              }
constructor TUnicodeFileReader.Create(const FileName: String;
    const Codec: TCustomUnicodeCodec; const CodecOwner: Boolean);
begin
  inherited Create(TFileReader.Create(FileName), True, Codec, CodecOwner);
end;



end.

