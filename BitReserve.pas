{
buffer for frame data: current frame's main data can occur in previous frames (aka bit reservoir)
}
unit BitReserve;

interface

uses
  bitstream;

const
  BITRESERVE_BUFFER_SIZE = 2048;  //approx. max frame data size + max reserve (511 bytes)

type

  { TBitReserve }

  TBitReserve = class
  private
    bs: TBitstreamReader;
    _size: integer;
    FBuf: array[0..BITRESERVE_BUFFER_SIZE-1] of byte;

    procedure FlushBytes(bytecount: integer);


  public
    InvalidDataBegin: boolean;

    constructor Create;
    destructor Destroy; override;

    procedure NewFrame(main_data_begin: integer);
    procedure InsertMainData(src: pbyte; length: integer);
    procedure EndFrame();

    function bitPosition: Cardinal;
    function hgetbits(n: Cardinal): Cardinal;
    function hget1bit: Cardinal;
    procedure RewindBits(n: Cardinal);
    procedure SkipBits(n: cardinal);
  end;

implementation

{ TBitReserve }

constructor TBitReserve.Create;
begin
  inherited create;
  _size := 0;
  bs := TBitstreamReader.Create(@FBuf[0]);
end;

destructor TBitReserve.Destroy;
begin
  bs.Free;
  inherited Destroy;
end;

procedure TBitReserve.NewFrame(main_data_begin: integer);
var
  bytes_to_discard: integer;
begin
  Assert(main_data_begin < 512);
  InvalidDataBegin := false;

  //remove padding - data from previous frame(s) which was unused and is now unreachable
  bytes_to_discard := _size - main_data_begin;
  if bytes_to_discard > 0 then
      FlushBytes(bytes_to_discard);

  //can usually happen in splits, if the frame has main_data in previous frame which does not exist anymore
  if (bytes_to_discard < 0) then
      InvalidDataBegin := true;
end;

procedure TBitReserve.InsertMainData(src: pbyte; length: integer);
begin
  move(src^, FBuf[_size], length);
  //reload bitreader buffer
  if _size <= 4 then
      bs.Start();
  _size += length;
end;

procedure TBitReserve.EndFrame();
var
  out_bits: integer;
begin
  //bytealign bit reader position
  out_bits := bs.GetBitPosition and 7;
  if out_bits > 0 then
      hgetbits(8 - out_bits);

  //remove main_data of this frame
  FlushBytes(0);
end;

//remove bytes that bitreader already processed and optionally some extra padding as well
procedure TBitReserve.FlushBytes(bytecount: integer);
begin
  if bytecount = 0 then
      bytecount := bs.GetBytePosition;
  if bytecount > 0 then begin
      _size -= bytecount;
      move(FBuf[bytecount], FBuf[0], _size);
      bs.Start();
  end;
end;


function TBitReserve.bitPosition: Cardinal;
begin
  result := bs.GetBitPosition;
end;

// read 1 bit from the bit stream
function TBitReserve.hget1bit: Cardinal;
begin
  result := bs.Read();
end;

// read N bits from the bit stream
function TBitReserve.hgetbits(n: Cardinal): Cardinal;
begin
  Assert(n > 0, 'zero bits to read');
  result := bs.Read(n);
end;


procedure TBitReserve.RewindBits(n: Cardinal);
begin
  while n > 31 do begin
      bs.SkipBack(31);
      n -= 31;
  end;
  if n > 0 then
      bs.SkipBack(n);
end;

procedure TBitReserve.SkipBits(n: cardinal);
begin
  while n > 31 do begin
      bs.Read(31);
      n -= 31;
  end;
  if n > 0 then
      bs.Read(n)
end;

end.
