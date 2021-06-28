(*
 *  File:     $RCSfile: BitStream.pas,v $
 *  Revision: $Revision: 1.1.1.1 $
 *  Version : $Id: BitStream.pas,v 1.1.1.1 2002/04/21 12:57:16 fobmagog Exp $
 *  Author:   $Author: fobmagog $
 *  Homepage: http://delphimpeg.sourceforge.net/
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *)
unit BitStream;

interface
uses
  SysUtils, Classes;

type
  TSyncMode = (INITIAL_SYNC, STRICT_SYNC);

const
  BUFFERINTSIZE = 433;
  // max. 1730 bytes per frame: 144 * 384kbit/s / 32000 Hz + 2 Bytes CRC

type
  { TBitstreamReader }

  TBitstreamReader = class
    private
      bits: uint32;
      mask: longword;
      cur: pbyte;
      buffer: pbyte;

    public
      constructor Create(const memory_buffer: pbyte);
      procedure Start();
      function  Read(): longword;
      function  Read(count: longword): longword;
  end;


  TBitStream = class
  private
    FStream: TStream;

    FBuffer: array[0..BUFFERINTSIZE-1] of Cardinal;

    FFrameSize: Cardinal;     // number of valid bytes in buffer
    FSyncWord: Cardinal;
    FSingleChMode: Boolean;
    FCurrentFrameNumber: Integer;

    bs: TBitstreamReader;

  public
    constructor Create(input_stream: TStream);
    destructor Destroy; override;

    function GetHeader(HeaderString: PCardinal; SyncMode: TSyncMode): Boolean;
    // get next 32 bits from bitstream in an unsigned int,
    // returned value False => end of stream
    function ReadFrame(ByteSize: Cardinal): Boolean;
    // fill buffer with data from bitstream, returned value False => end of stream
    function GetBits(NumberOfBits: Cardinal): Cardinal;
    // read bits (1 <= number_of_bits <= 16) from buffer into the lower bits
    // of an unsigned int. The LSB contains the latest read bit of the stream.
    procedure SetSyncWord(SyncWord: Cardinal);
    // Set the word we want to sync the header to, in
    // Big-Endian byte order
    function FileSize: Cardinal;
    // Returns the size, in bytes, of the input file.
  end;

implementation

//fpc has SwapEndian, but it doesn't get inlined
function bswap(n: longword): longword; inline;
begin
{$ifdef ENDIAN_LITTLE}
  result := (n shr 24) or
            (n shl 24) or
            ((n shr 8) and $ff00) or
            ((n shl 8) and $ff0000);
{$else}
  result := n;
{$endif}
end;

{ TBitStream }

constructor TBitStream.Create(input_stream: TStream);
begin
  FStream := input_stream;
  FStream.Seek(0, soFromBeginning);
  FCurrentFrameNumber := -1;

  bs := TBitstreamReader.Create(@FBuffer);
end;

destructor TBitStream.Destroy;
begin
  bs.Free;
  inherited Destroy;
end;

function TBitStream.FileSize: Cardinal;
begin
  Result := FStream.Size;
end;

function TBitStream.GetBits(NumberOfBits: Cardinal): Cardinal;
begin
  result := bs.Read(NumberOfBits);
end;

function TBitStream.GetHeader(HeaderString: PCardinal;
  SyncMode: TSyncMode): Boolean;
var Sync: Boolean;
    NumRead: Integer;
begin
  repeat
    // Read 4 bytes from the file, placing the number of bytes actually
    // read in numread
    NumRead := FStream.Read(HeaderString^, 4);
    Result := (NumRead = 4);
    if (not Result) then
      exit;

    if (SyncMode = INITIAL_SYNC) then
      Sync := ((HeaderString^ and $0000F0FF) = $0000F0FF)
    else
      Sync := ((HeaderString^ and $000CF8FF) = FSyncWord) and
              (((HeaderString^ and $C0000000) = $C0000000) = FSingleChMode);

//  if ((HeaderString^ and $0000FFFF) = $0000FFFF) then
//    Sync := false;

    if (not Sync) then
      // rewind 3 bytes in the file so we can try to sync again, if
      // successful set result to TRUE
      FStream.Seek(-3, soFromCurrent);
  until (Sync) or (not Result);

  if (not Result) then
    exit;

  HeaderString^ := bswap(HeaderString^);

  inc(FCurrentFrameNumber);

  Result := True;
end;

function TBitStream.ReadFrame(ByteSize: Cardinal): Boolean;
var NumRead: Integer;
begin
  // read bytesize bytes from the file, placing the number of bytes
  // actually read in numread and setting result to TRUE if
  // successful
  NumRead := FStream.Read(FBuffer, ByteSize);
  bs.Start();
  FFrameSize := ByteSize;
  Result := Cardinal(NumRead) = FFrameSize;
end;


procedure TBitStream.SetSyncWord(SyncWord: Cardinal);
begin
  FSyncWord := bswap(Syncword and $FFFFFF3F);
  FSingleChMode := ((SyncWord and $000000C0) = $000000C0);
end;


{ TBitstreamReader }

constructor TBitstreamReader.Create(const memory_buffer: pbyte);
begin
  buffer := memory_buffer;
  Start;
end;

//called after memory buffer data changes
procedure TBitstreamReader.Start();
begin
  cur  := buffer;
  mask := 32;
  bits := bswap( plongword(cur)^ );
end;

function TBitstreamReader.Read(): longword;
begin
  mask -= 1;
  result := (bits shr mask) and 1;
  if mask = 0 then begin
      cur += 4;
      bits := bswap( plongword(cur)^ );
      mask := 32;
  end;
end;

function TBitstreamReader.Read(count: longword): longword;
var
  bits_left: longword;
begin
  Assert(count < 32, 'count of bits to read is over 31');
  if mask > count then begin
      result := bits shr (mask - count) and ($ffffffff shr (32 - count));
      mask -= count;
  end
  else begin
      bits_left := count - mask;
      result := bits and ($ffffffff shr (32 - mask)) shl bits_left;
      cur += 4;
      bits := bswap( plongword(cur)^ );
      mask := 32 - bits_left;
      if bits_left > 0 then
          result := result or (bits shr mask);
  end;
end;

end.
